#county to county and state to county/state

ratio <- read.csv('ratio_2017.csv')
year_diff <- 2050 - 2022

scen1_comm_list1 <- c('Other Chemicals, Plastics, and Rubber',
                      'Raw and Finished Wood Products',
                      'Textiles and Leather',
                      'Mixed Freight') # change increase by 2.9% annually

scen1_comm_list2 <- c('Agriculture and Fish') # agriculture increase by 1.3% annually 

scen2_comm_list <- c('Non-coal Energy Products')

scen3_comm_list1 <- c('Coal')
scen3_comm_list2 <- c('Machinery, Electric, and Precision Instruments','Transportation Equipment')

process_scenario_v3 <- function(dat_temp_cs, #the filtered datatable
                                Scenario_opt_cs, #scenario selection
                                curr, #select which are the basis of imports and exports
                                all_flag #not sure which one
                                ){
  
  if(all_flag == 0){
    
    dat_temp_cs = dat_temp_cs %>%
      mutate(dms_imp_exp = ifelse(origin %in% curr, destination, origin),
             GEOID = dms_imp_exp) %>% 
      left_join(ratio, by = c('dms_mode','Grouped_sctg2'))
    
    } else {
    
    dat_temp_cs = dat_temp_cs %>% 
      left_join(ratio, by = c('dms_mode','Grouped_sctg2'))
    
  }
    
  if('_s1' %in% Scenario_opt_cs){
    
    # for consumer goods:
    group1_new_rate = 0.029
    group1_base_rate = 0.023
    # for agricultural goods
    group2_new_rate = 0.013
    group2_base_rate = 0.011
    
    dat_temp_cs <- dat_temp_cs %>%
      mutate(tons_2050_s1 = ifelse(
        #conditions
        origin %in% curr & !(destination == origin) & # to avoid any internal flow in any case
          Grouped_sctg2 %in% scen1_comm_list1,
        # Condition 1
        tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff,
        ifelse(
          origin %in% curr & !(destination == origin) &
            Grouped_sctg2 %in% scen1_comm_list2,
          # Condition 2
          # tons_2022 * (1+0.013)^year_diff,
          tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^ year_diff,
          tons_2050))) %>%
      mutate(value_2050_s1 = ifelse(
            origin %in% curr & !(destination == origin) &
              Grouped_sctg2 %in% scen1_comm_list1,
            
            # Condition 3
            tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^ year_diff * ratio_2017,
          
            ifelse(
              origin %in% curr & !(destination == origin) &
                Grouped_sctg2 %in% scen1_comm_list2,
              # Condition 4
              tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^ year_diff*ratio_2017,
              # Default case
              value_2050))) %>%
      mutate(tons_2050_s1 = ifelse(tons_2050_s1 < 0, 0 , tons_2050_s1),
             value_2050_s1 = ifelse(value_2050_s1 < 0, 0, value_2050_s1)) # avoid negative numbers. 

  }
  if('_s2' %in% Scenario_opt_cs){
    
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
        #This seems wrong the third case would never occur? Therefore it assumes that a commodity is always in a group
        tons_2050_s2 = case_when(
          destination %in% curr & !(origin == destination)  ~ # import
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff),
          
          origin %in% curr & !(destination == origin) ~ #export
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group4_new_rate/group4_base_rate) ^year_diff),
          
          TRUE ~ tons_2050),
          
        value_2050_s2 = case_when( 
          destination %in% curr & !(origin == destination) ~ # import
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list), 
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group1_new_rate/group1_base_rate) ^year_diff *ratio_2017,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group3_new_rate/group3_base_rate) ^year_diff *ratio_2017),
          
          origin %in% curr & !(destination == origin) ~ #export
            ifelse(!(Grouped_sctg2 %in% scen2_comm_list),
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff *ratio_2017,
                   tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group4_new_rate/group4_base_rate) ^year_diff *ratio_2017),
          
          TRUE ~ value_2050)
      ) %>%
      mutate(tons_2050_s2 = ifelse(tons_2050_s2 < 0, 0, tons_2050_s2),
             value_2050_s2 = ifelse(value_2050_s2 < 0, 0, value_2050_s2)) # avoid negative numbers. 
  }
  if('_s3' %in% Scenario_opt_cs){
    
    # for Coal goods:
    group1_new_rate = -0.045
    ## no base rate for coal
    # for High Tech Durable goods
    group2_new_rate = 0.028
    group2_base_rate = 0.022
    
    dat_temp_cs = dat_temp_cs %>%
      mutate(tons_2050_s3 = ifelse(
          Grouped_sctg2 %in% scen3_comm_list1,
          # Condition 1
          tons_2022 * (1 + group1_new_rate)^year_diff,
          
          ifelse(
            Grouped_sctg2 %in% scen3_comm_list2,
            # Condition 2
            tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff, 
            tons_2050)),
          value_2050_s3 = ifelse(
            
            Grouped_sctg2 %in% scen3_comm_list1,
            # Condition 3
            tons_2022 * (1 + group1_new_rate)^year_diff *ratio_2017,
            ifelse(
                Grouped_sctg2 %in% scen3_comm_list2,
                # Condition 4
                tons_2022 *(1 + ((tons_2050/tons_2022)^(1/year_diff)-1)* group2_new_rate/group2_base_rate) ^year_diff*ratio_2017,
                # Default case
                value_2050))) %>%
      mutate(tons_2050_s3 = ifelse(tons_2050_s3 < 0, 0 , tons_2050_s3),
             value_2050_s3 = ifelse(value_2050_s3 < 0, 0 , value_2050_s3)) # avoid negative numbers.
  }
  
  return(dat_temp_cs)
  
}

