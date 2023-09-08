process_scenario <- function(dat_temp_cs,Value_opts_cs,Scenario_opt_cs, curr,col_list, all_flag){
  if(all_flag == 0){
  dat_temp_cs = dat_temp_cs %>%
    mutate(dms_imp_exp = ifelse(origin == curr, destination, origin),
           GEOID = dms_imp_exp)}else{
             dat_temp_cs = dat_temp_cs} 
  
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
      
    } else if (Scenario_opt_cs == '_s4'){
      dat_temp_cs = dat_temp_cs %>%
        mutate(temp = ifelse(Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                  'Textiles and Leather',
                                                  'Vehicles and Transportation Equipment'),
                             dat_temp_cs[[Value_opts_cs]] * 1.3, dat_temp_cs[[Value_opts_cs]])) %>%
        group_by(across(all_of(col_list)))%>%
        summarise(tons_2017 = sum(tons_2017), 
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050),
                  temp = sum(temp)) %>%
        rename(setNames('temp',paste0(Value_opts_cs,'_s4'))) %>%
        ungroup()
      
    } else if (Scenario_opt_cs == '_s5') {
      dat_temp_cs = dat_temp_cs %>%
        mutate(temp = ifelse(dms_mode == c('2','3'), dat_temp_cs[[Value_opts_cs]] * 1.3, dat_temp_cs[[Value_opts_cs]])) %>%
        group_by(across(all_of(col_list)))%>%
        summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050),
                  temp = sum(temp)) %>%
        rename(setNames('temp',paste0(Value_opts_cs,'_s5'))) %>%
        ungroup()
    } else if (Scenario_opt_cs == '_s6') {
      dat_temp_cs = dat_temp_cs %>%
        mutate(temp = ifelse(dms_mode == c('2','3')|Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                                         'Nonmetallic Mineral and Base Metal Products'),
                             dat_temp_cs[[Value_opts_cs]] * 1.15, dat_temp_cs[[Value_opts_cs]])) %>%
        group_by(across(all_of(col_list)))%>%
        summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                  tons_2020 = sum(tons_2020),
                  tons_2050 = sum(tons_2050),
                  value_2017 = sum(value_2017),
                  value_2020 = sum(value_2020),
                  value_2050 = sum(value_2050),
                  temp = sum(temp)) %>%
        rename(setNames('temp',paste0(Value_opts_cs,'_s6'))) %>%
        ungroup()
      

    }
  
}


process_scenario_cc <- function(dat_temp,Value_opts,Scenario_opt, curr,col_list, all_flag){
  if(all_flag == 0){
    dat_temp = dat_temp %>%
      mutate(dms_imp_exp = ifelse(dms_orig == curr, dms_dest, dms_orig),
             GEOID = dms_imp_exp)}else{
               dat_temp = dat_temp} 
  
  if(Scenario_opt == '_s1'){
    ## multiply mode 1 and 3 by 1.1
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(dms_mode == '1' & Grouped_sctg2 == 'Energy Products', dat_temp[[Value_opts]] * 0.95,
                           ifelse(dms_mode == '6' & Grouped_sctg2 == 'Energy Products' & substr(dms_orig,1,2) %in% c('22','48'),dat_temp[[Value_opts]] *0.98,
                                  dat_temp[[Value_opts]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s1'))) %>%
      ungroup()
    
  } else if (Scenario_opt == '_s2'){
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products') & substr(dms_dest,1,2) %in% c('06','48','36','12','53','17','42','13','37','39') & substr(dms_orig,1,2) != substr(dms_dest,1,2),dat_temp[[Value_opts]] * 1.12,
                           ifelse(substr(dms_orig,1,2) == substr(dms_dest,1,2) & Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products'),dat_temp[[Value_opts]]*1.15 ,dat_temp[[Value_opts]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s2'))) %>%
      ungroup()
    
  } else if (Scenario_opt == '_s3'){
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Agriculture and Fish','Food, Alcohol and Tobacco'),
                           dat_temp[[Value_opts]] * 1.3, dat_temp[[Value_opts]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), 
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s3'))) %>%
      ungroup()
    
  } else if (Scenario_opt == '_s4'){
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                'Textiles and Leather',
                                                'Vehicles and Transportation Equipment'),
                           dat_temp[[Value_opts]] * 1.3, dat_temp[[Value_opts]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), 
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s4'))) %>%
      ungroup()
    
  } else if (Scenario_opt == '_s5') {
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(dms_mode == c('2','3'), dat_temp[[Value_opts]] * 1.3, dat_temp[[Value_opts]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s5'))) %>%
      ungroup()
  } else if (Scenario_opt == '_s6') {
    dat_temp = dat_temp %>%
      mutate(temp = ifelse(dms_mode == c('2','3')|Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                                       'Nonmetallic Mineral and Base Metal Products'),
                           dat_temp[[Value_opts]] * 1.15, dat_temp[[Value_opts]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(tons_2017 = sum(tons_2017), # do we need the tons 2017?
                tons_2020 = sum(tons_2020),
                tons_2050 = sum(tons_2050),
                value_2017 = sum(value_2017),
                value_2020 = sum(value_2020),
                value_2050 = sum(value_2050),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts,'_s6'))) %>%
      ungroup()
    return(dat_temp)
    
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
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s1'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s2'){
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products') & substr(destination,1,2) %in% c('06','48','36','12','53','17','42','13','37','39') & substr(origin,1,2) != substr(destination,1,2),dat_temp_in[[Value_opts_in]] * 1.12,
                           ifelse(substr(origin,1,2) == substr(destination,1,2) & Grouped_sctg2 %in% c('Aggregates','Raw and Finished Wood Products'),dat_temp_in[[Value_opts_in]]*1.15 ,dat_temp_in[[Value_opts_in]]))) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s2'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s3'){
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Agriculture and Fish','Food, Alcohol and Tobacco'),
                           dat_temp_in[[Value_opts_in]] * 1.3, dat_temp_in[[Value_opts_in]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s3'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s4'){
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                'Textiles and Leather',
                                                'Vehicles and Transportation Equipment'),
                           dat_temp_in[[Value_opts_in]] * 1.3, dat_temp_in[[Value_opts_in]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s4'))) %>%
      ungroup()
    
  } else if (Scenario_opt_in == '_s5') {
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(dms_mode == c('2','3'), dat_temp_in[[Value_opts_in]] * 1.3, dat_temp_in[[Value_opts_in]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s5'))) %>%
      ungroup()
  } else if (Scenario_opt_in == '_s6') {
    dat_temp_in = dat_temp_in %>%
      mutate(temp = ifelse(dms_mode == c('2','3')|Grouped_sctg2 %in% c('Chemicals, Pharmaceuticals, Plastics, and Rubber',
                                                                       'Nonmetallic Mineral and Base Metal Products'),
                           dat_temp_in[[Value_opts_in]] * 1.15, dat_temp_in[[Value_opts_in]])) %>%
      group_by(across(all_of(col_list)))%>%
      summarise(Tons_2019 = sum(Tons_2019), 
                Tons_2021 = sum(Tons_2021),
                Value_2019 = sum(Value_2019),
                Value_2021 = sum(Value_2021),
                temp = sum(temp)) %>%
      rename(setNames('temp',paste0(Value_opts_in,'_s6'))) %>%
      ungroup()
    
    
  }
  
}