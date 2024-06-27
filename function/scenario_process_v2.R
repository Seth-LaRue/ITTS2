#county to county and state to county/state

scen1_comm_list1 <- c('Other Chemicals, Plastics, and Rubber',
                      'Raw and Finished Wood Products',
                      'Textiles and Leather',
                      'Mixed Freight') # change increase by 2.9% annually

scen1_comm_list2 <- c('Agriculture and Fish') # agriculture increase by 1.3% annually. 

scen2_comm_list <- c('Non-coal Energy Products')

scen3_comm_list1 <- c('Coal')
scen3_comm_list2 <- c('Machinery, Electric, and Precision Instruments','Transportation Equipment')


process_scenario <- function(dat_temp_cs,Value_opts_cs,Scenario_opt_cs, curr,col_list, all_flag){
  # data_temp_cs: the filtered datatable
  # Value_opts_cs: which value/tonnage year is selected
  # curr: the geography selection 
  # col_list: the unique ID combination (origin, desitnation, ID, etc)
  if(all_flag == 0){
    dat_temp_cs = dat_temp_cs %>%
      mutate(dms_imp_exp = ifelse(origin %in% curr, destination, origin),
             GEOID = dms_imp_exp) %>% 
      mutate(ratio_2017 = value_2017/tons_2017)
  }
  else{
    
    dat_temp_cs = dat_temp_cs %>% 
      mutate(ratio_2017 = value_2017/tons_2017)
  }
  
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
          destination != curr  & # to avoid any internal flow in any case
          Grouped_sctg2 %in% scen1_comm_list1 & 
          year_selected == 2050 & 
          parameter_sel == 'tons',
        
        # Condition 1
        tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff,
        
        ifelse(
          origin %in% curr & 
            destination != curr  &
            Grouped_sctg2 %in% scen1_comm_list2 & 
            year_selected == 2050 & 
            parameter_sel == 'tons',
          
          # Condition 2
          # tons_2022 * (1+0.013)^year_diff,
          tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^ year_diff,
          
          ifelse(
            origin %in% curr & 
              destination != curr &
              Grouped_sctg2 %in% scen1_comm_list1 & 
              year_selected == 2050 & 
              parameter_sel == 'value',
            
            # Condition 3
            tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff * ratio_2017,
            
            ifelse(
              origin %in% curr & 
                destination != curr  &
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
        # temp = ifelse(
        #   destination %in% curr & !(origin == destination) & !(Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'tons'), # import non-energy
        #   tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group1_new_rate/group1_base_rate) ^ year_diff,
        #   ifelse(
        #     origin %in% curr & !(destination == origin) & !(Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'tons'), # export non-energy
        #     tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group2_new_rate/group2_base_rate) ^ year_diff ,
        #     ifelse(
        #       destination %in% curr & !(origin == destination) & (Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'tons'), # import energy
        #       tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group3_new_rate/group3_base_rate) ^ year_diff,
        #       ifelse(
        #         origin %in% curr & !(destination == origin) & (Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'tons'), # export energy
        #         tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group4_new_rate/group4_base_rate) ^ year_diff,
        #         ifelse(destination %in% curr & !(origin == destination) & !(Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'value'), # import non-energy
        #                tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group1_new_rate/group1_base_rate) ^ year_diff * ratio_2017,
        #                ifelse(
        #                  origin %in% curr & !(destination == origin) & !(Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'value'), # export non-energy
        #                  tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group2_new_rate/group2_base_rate) ^ year_diff *ratio_2017 ,
        #                  ifelse(
        #                    destination %in% curr & !(origin == destination) & (Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'value'), # import energy
        #                    tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group3_new_rate/group3_base_rate) ^ year_diff * ratio_2017,
        #                    ifelse(
        #                      origin %in% curr & !(destination == origin) & (Grouped_sctg2 %in% scen2_comm_list & year_selected == 2050 & parameter_sel == 'value'), # export energy
        #                      tons_2022 * (1 + ((tons_2050/tons_2022)^(1/year_diff) - 1) * group4_new_rate/group4_base_rate) ^ year_diff * ratio_2017,
        #                      dat_temp_cs[[Value_opts_cs]])))))))))
      
        temp =  case_when(
          destination %in% curr &
            origin != curr  &  # import
            year_selected == 2050 & parameter_sel == 'tons' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff),

          origin %in% curr &
            destination != curr  & #export
            year_selected == 2050 & parameter_sel == 'tons' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group4_new_rate/group4_base_rate) ^year_diff),

          destination %in% curr &
            origin != curr &
            year_selected == 2050 & parameter_sel == 'value' ~
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff *ratio_2017,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff *ratio_2017),

          origin %in% curr &
            destination != curr  &
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
    
    # for Coal goods:
    group1_new_rate = -0.045
    ## no base rate for coal
    # for High Tech Durable goods
    group2_new_rate = 0.028
    group2_base_rate = 0.022
    
    dat_temp_cs = dat_temp_cs %>%
      mutate(temp = ifelse(
        Grouped_sctg2 %in% scen3_comm_list2 & 
          year_selected == 2050 & 
          parameter_sel == 'tons',
        
        # Condition 2
        tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff,
        
        ifelse(
          Grouped_sctg2 %in% scen3_comm_list2 & 
            year_selected == 2050 & 
            parameter_sel == 'value',
          
          # Condition 4
          tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff*ratio_2017,
          # Default case
          dat_temp_cs[[Value_opts_cs]] # if not matching any, equals the default
        )
      )
      )
      dat_temp_cs <- dat_temp_cs %>%
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

## process scenario columns
rename_tbl_columns <- function(df) {
  # optional oclumns
  scen_col_list <- list(
    "value_2050_s1" = "Value 2050 Scenario 1 ($Million)",
    "value_2050_s2" = "Value 2050 Scenario 2 ($Million)",
    "value_2050_s3" = "Value 2050 Scenario 3 ($Million)",
    "tons_2050_s1" = "Tons 2050 Scenario 1 (K Tons)",
    "tons_2050_s2" = "Tons 2050 Scenario 2 (K Tons)",
    "tons_2050_s3" = "Tons 2050 Scenario 3 (K Tons)"
  )
  # Default columns
  df <- df %>%
    rename('Tons 2022 (K Tons)' = 'tons_2022',
           'Tons 2050 (K Tons)' = 'tons_2050',
           'Value 2022 ($Million)' = 'value_2022',
           'Value 2050 ($Million)' = 'value_2050')
  
  selected_Scen <- intersect(names(df), names(scen_col_list))
  
  if (length(selected_Scen) > 0) {
    df <- df %>%
      rename_with(~ sapply(.x, function(col) scen_col_list[[col]]), 
                  all_of(selected_Scen))
  }
  
  return(df)
}

