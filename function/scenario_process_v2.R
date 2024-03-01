#county to county and state to county/state
process_scenario <- function(dat_temp_cs,Value_opts_cs,Scenario_opt_cs, curr,col_list, all_flag){
ratio <- read.csv('/ratio_2017.csv')
  # data_temp_cs: the filtered datatable
  # Value_opts_cs: which value/tonnage year is selected
  # curr: the geography selection 
  # col_list: the unique ID combination (origin, desitnation, ID, etc)
  if(all_flag == 0){
    dat_temp_cs = dat_temp_cs %>%
      mutate(dms_imp_exp = ifelse(origin %in% curr, destination, origin),
             GEOID = dms_imp_exp) %>% 
      left_join(ratio, by = c('dms_mode','Grouped_sctg2'))
    }
  else{
    
    dat_temp_cs = dat_temp_cs %>% 
      left_join(ratio, by = c('dms_mode','Grouped_sctg2'))
  }
  
  scen1_comm_list1 <- c('Other Chemicals, Plastics, and Rubber',
                        'Raw and Finished Wood Products',
                        'Textiles and Leather',
                        'Mixed Freight') # change increase by 2.9% annually
  
  scen1_comm_list2 <- c('Agriculture and Fish') # agriculture increase by 1.3% annually. 
  
    year_diff <- 2050 - 2022
    
    year_selected <- as.numeric(gsub("\\D", "", Value_opts_cs))
    parameter_sel <- gsub("[0-9_]", "", Value_opts_cs)
    
  if(Scenario_opt_cs == '_s1'){
    
    # for consumer goods:
    group1_new_rate = 0.029
    group1_base_rate = 0.023
    # for agricultural goods
    group2_new_rate = 0.013
    group2_base_rate = 0.011
    
    dat_temp_cs <- dat_temp_cs %>%
      
      mutate(temp = ifelse(
        origin %in% curr & 
          !(destination %in% curr ) & # to avoid any internal flow in any case
          Grouped_sctg2 %in% scen1_comm_list1 & 
          year_selected == 2050 & 
          parameter_sel == 'tons',
        
        # Condition 1
        tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff,
    
        ifelse(
          origin %in% curr & 
            !(destination %in% curr ) &
            Grouped_sctg2 %in% scen1_comm_list2 & 
            year_selected == 2050 & 
            parameter_sel == 'tons',
          
          # Condition 2
          # tons_2022 * (1+0.013)^year_diff,
          tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^ year_diff,
          
          ifelse(
            origin %in% curr & 
              !(destination %in% curr) &
              Grouped_sctg2 %in% scen1_comm_list1 & 
              year_selected == 2050 & 
              parameter_sel == 'value',
            
            # Condition 3
            tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff * ratio_2017,
          
            ifelse(
              origin %in% curr & 
                !(destination %in% curr ) &
                Grouped_sctg2 %in% scen1_comm_list2 & 
                year_selected == 2050 & 
                parameter_sel == 'value',
              
              # Condition 4
              tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^ year_diff*ratio_2017,
              # Default case
              dat_temp_cs[[Value_opts_cs]]
            )
          )
        )
      )
      )%>%
      mutate(temp = ifelse(temp < 0, 0 , temp)) %>% # avoid negative numbers. 
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2022 = sum(tons_2022),
                tons_2050 = sum(tons_2050),
                value_2022 = sum(value_2022),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_cs,'_s1'))) %>%
      ungroup()
    
  } else if (Scenario_opt_cs == '_s2'){
    scen2_comm_list <- c('Non-coal Energy Products')
   
    # for import non-energy 
    group1_new_rate = 0.029
    group1_base_rate = 0.023
    # for export non-energy
    group2_new_rate = 0.023
    group2_base_rate = 0.019
    # for import energy
    group3_new_rate = -0.014
    group3_base_rate = - 0.011
    # for export energy
    group4_new_rate = 0.0295
    group4_base_rate = 0.024
    
    dat_temp_cs <- dat_temp_cs %>%
      mutate(
        temp = case_when(
          destination %in% curr &
            !(origin %in% curr ) &  # import
            year_selected == 2050 & parameter_sel == 'tons' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff),
          
          origin %in% curr & 
            !(destination %in% curr ) & #export
            year_selected == 2050 & parameter_sel == 'tons' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group4_new_rate/group4_base_rate) ^year_diff),
          
          destination %in% curr &
            !(origin %in% curr) &
            year_selected == 2050 & parameter_sel == 'value' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list), 
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff *ratio_2017,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff *ratio_2017),
          
          origin %in% curr &
            !(destination %in% curr ) &
            year_selected == 2050 & parameter_sel == 'value' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff *ratio_2017,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group4_new_rate/group4_base_rate) ^year_diff *ratio_2017),
          
          TRUE ~ dat_temp_cs[[Value_opts_cs]]  # Default case
        )
      ) %>%
      mutate(temp = ifelse(temp < 0, 0 , temp)) %>% # avoid negative numbers. 
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2022 = sum(tons_2022, na.rm = TRUE),
                tons_2050 = sum(tons_2050, na.rm = TRUE),
                value_2022 = sum(value_2022, na.rm = TRUE),
                value_2050 = sum(value_2050, na.rm = TRUE),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_cs,'_s2'))) %>%
      ungroup()
    
  } else if (Scenario_opt_cs == '_s3'){
    
    scen3_comm_list1 <- c('Coal')
    scen3_comm_list2 <- c('Machinery, Electric, and Precision Instruments','Transportation Equipment')
    
    # for Coal goods:
    group1_new_rate = -0.045
    ## no base rate for coal
    # for High Tech Durable goods
    group2_new_rate = 0.028
    group2_base_rate = 0.022
    
    dat_temp_cs = dat_temp_cs %>%
      mutate(temp = ifelse(
          Grouped_sctg2 %in% scen3_comm_list1 & 
          year_selected == 2050 & 
          parameter_sel == 'tons',
        
        # Condition 1
        tons_2022 * (1 + group1_new_rate)^year_diff,
        
        ifelse(
            Grouped_sctg2 %in% scen3_comm_list2 & 
            year_selected == 2050 & 
            parameter_sel == 'tons',
          
          # Condition 2
          tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff, 
          
          ifelse(
              Grouped_sctg2 %in% scen3_comm_list1 & 
              year_selected == 2050 & 
              parameter_sel == 'value',
            
            # Condition 3
            tons_2022 * (1 + group1_new_rate)^year_diff *ratio_2017,
            
            ifelse(
                Grouped_sctg2 %in% scen3_comm_list2 & 
                year_selected == 2050 & 
                parameter_sel == 'value',
              
              # Condition 4
              tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff*ratio_2017,
              # Default case
              dat_temp_cs[[Value_opts_cs]]
            )
          )
        )
      ))%>%
      mutate(temp = ifelse(temp < 0, 0 , temp)) %>% # avoid negative numbers. 
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2022 = sum(tons_2022),
                tons_2050 = sum(tons_2050),
                value_2022 = sum(value_2022),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_cs,'_s3'))) %>%
      ungroup()
    
  }
}


#check_func <- process_scenario(dat_ss, 'value_2050', "_s1",c('48','56','01'),c('origin','destination'), 0)




# ## international trade: 
# process_scenario_in <- function(dat_temp_in,Value_opts_in,Scenario_opt_in, curr,col_list, all_flag){
#   if(all_flag == 0){
#     dat_temp_in = dat_temp_in %>%
#       mutate(dms_imp_exp = ifelse(origin %in% curr, destination, origin),
#              GEOID = dms_imp_exp)}else{
#                dat_temp_in = dat_temp_in} 
#   
#   if(Scenario_opt_in == '_s1'){
#     ## multiply mode 1 and 3 by 1.1
#     dat_temp_in = dat_temp_in %>%
#       mutate(temp = ifelse(dms_mode == '1' & Grouped_sctg2 == 'Energy Products', dat_temp_in[[Value_opts_in]] * 0.95,
#                            ifelse(dms_mode == '6' & Grouped_sctg2 == 'Energy Products' & substr(origin,1,2) %in% c('22','48'),dat_temp_in[[Value_opts_in]] *0.98,
#                                   dat_temp_in[[Value_opts_in]]))) %>%
#       group_by(across(all_of(col_list)))%>%
#       summarise(tons_2019 = sum(tons_2019), 
#                 tons_2021 = sum(tons_2021),
#                 value_2019 = sum(value_2019),
#                 value_2021 = sum(value_2021),
#                 temp = sum(temp)) %>%
#       rename(setNames('temp',paste0(Value_opts_in,'_s1'))) %>%
#       ungroup()
#     
#   } else if (Scenario_opt_in == '_s2'){
#     dat_temp_in = dat_temp_in %>%
#       mutate(temp = ifelse(Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products') & substr(destination,1,2) %in% c('06','48','36','12','53','17','42','13','37','39') & substr(origin,1,2) != substr(destination,1,2),dat_temp_in[[Value_opts_in]] * 1.12,
#                            ifelse(substr(origin,1,2) == substr(destination,1,2) & Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products'),dat_temp_in[[Value_opts_in]]*1.15 ,dat_temp_in[[Value_opts_in]]))) %>%
#       group_by(across(all_of(col_list)))%>%
#       summarise(tons_2019 = sum(tons_2019), 
#                 tons_2021 = sum(tons_2021),
#                 value_2019 = sum(value_2019),
#                 value_2021 = sum(value_2021),
#                 temp = sum(temp)) %>%
#       rename(setNames('temp',paste0(Value_opts_in,'_s2'))) %>%
#       ungroup()
#     
#   } else if (Scenario_opt_in == '_s3'){
#     dat_temp_in = dat_temp_in %>%
#       mutate(temp = ifelse(Grouped_sctg2 %in% c('Agriculture and Fish','Food, Alcohol and Tobacco'),
#                            dat_temp_in[[Value_opts_in]] * 1.3, dat_temp_in[[Value_opts_in]])) %>%
#       group_by(across(all_of(col_list)))%>%
#       summarise(tons_2019 = sum(tons_2019), 
#                 tons_2021 = sum(tons_2021),
#                 value_2019 = sum(value_2019),
#                 value_2021 = sum(value_2021),
#                 temp = sum(temp)) %>%
#       rename(setNames('temp',paste0(Value_opts_in,'_s3'))) %>%
#       ungroup()
#     
#   } 
#   
# }