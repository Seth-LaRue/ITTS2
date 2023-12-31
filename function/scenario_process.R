#county to county and state to county/state
process_scenario <- function(dat_temp_cs,Value_opts_cs,Scenario_opt_cs, curr,col_list, all_flag){
  if(all_flag == 0){
  dat_temp_cs = dat_temp_cs %>%
    mutate(dms_imp_exp = ifelse(origin == curr, destination, origin),
           GEOID = dms_imp_exp)}else{
             dat_temp_cs = dat_temp_cs} 
  comm_list <- c('23','24','26','27','28','29','30','40','43') # change increase by 2.9% annually
  comm_list2 <- c('01','02','03','04','05') # agriculture increase by 1.3% annually. 
  if(Scenario_opt_cs == '_s1'){

        ## multiply mode 1 and 3 by 1.1
    dat_temp_cs = dat_temp_cs %>%
      mutate(temp = ifelse(dms_mode == '1' & Grouped_sctg2 == 'Energy Products', dat_temp_cs[[Value_opts_cs]] * 0.95,
                           ifelse(dms_mode == '6' & Grouped_sctg2 == 'Energy Products' & substr(origin,1,2) %in% c('22','48'),dat_temp_cs[[Value_opts_cs]] *0.98,
                                  dat_temp_cs[[Value_opts_cs]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_cs,'_s1'))) %>%
      ungroup()

    } else if (Scenario_opt_cs == '_s2'){
    dat_temp_cs = dat_temp_cs %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products') & substr(destination,1,2) %in% c('06','48','36','12','53','17','42','13','37','39') & substr(origin,1,2) != substr(destination,1,2),dat_temp_cs[[Value_opts_cs]] * 1.12,
                           ifelse(substr(origin,1,2) == substr(destination,1,2) & Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products'),dat_temp_cs[[Value_opts_cs]]*1.15 ,dat_temp_cs[[Value_opts_cs]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_cs,'_s2'))) %>%
      ungroup()

    } else if (Scenario_opt_cs == '_s3'){
      dat_temp_cs = dat_temp_cs %>%
        mutate(temp = ifelse(Grouped_sctg2 %in% c('Agriculture and Fish','Food, Alcohol and Tobacco'),
                             dat_temp_cs[[Value_opts_cs]] * 1.3, dat_temp_cs[[Value_opts_cs]])) %>%
        group_by(across(all_of(col_list)))%>%
        summarise(tons_2017 = sum(tons_2017), 
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050),
                  temp = sum(temp)) %>%
        rename(setNames('temp',paste0(Value_opts_cs,'_s3'))) %>%
        ungroup()
      
    }
}


## international trade: 
process_scenario_in <- function(dat_temp_in,Value_opts_in,Scenario_opt_in, curr,col_list, all_flag){
  if(all_flag == 0){
    dat_temp_in = dat_temp_in %>%
      mutate(dms_imp_exp = ifelse(origin == curr, destination, origin),
             GEOID = dms_imp_exp)}else{
               dat_temp_in = dat_temp_in} 
  
  if(Scenario_opt_in == '_s1'){
    ## multiply mode 1 and 3 by 1.1
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(dms_mode == '1' & Grouped_sctg2 == 'Energy Products', dat_temp_in[[Value_opts_in]] * 0.95,
                           ifelse(dms_mode == '6' & Grouped_sctg2 == 'Energy Products' & substr(origin,1,2) %in% c('22','48'),dat_temp_in[[Value_opts_in]] *0.98,
                                  dat_temp_in[[Value_opts_in]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2019 = sum(tons_2019), 
                tons_2021 = sum(tons_2021),
                value_2019 = sum(value_2019),
                value_2021 = sum(value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s1'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s2'){
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products') & substr(destination,1,2) %in% c('06','48','36','12','53','17','42','13','37','39') & substr(origin,1,2) != substr(destination,1,2),dat_temp_in[[Value_opts_in]] * 1.12,
                           ifelse(substr(origin,1,2) == substr(destination,1,2) & Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products'),dat_temp_in[[Value_opts_in]]*1.15 ,dat_temp_in[[Value_opts_in]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2019 = sum(tons_2019), 
                tons_2021 = sum(tons_2021),
                value_2019 = sum(value_2019),
                value_2021 = sum(value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s2'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s3'){
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Agriculture and Fish','Food, Alcohol and Tobacco'),
                           dat_temp_in[[Value_opts_in]] * 1.3, dat_temp_in[[Value_opts_in]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2019 = sum(tons_2019), 
                tons_2021 = sum(tons_2021),
                value_2019 = sum(value_2019),
                value_2021 = sum(value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s3'))) %>%
      ungroup()
    
  } 
  
}