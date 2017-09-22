agriculture_survey_15_may_2016 <- function(dbcon, xml, test=FALSE){
  assign('codedf', read_xls('migrations/VS_Agriculture_15.05.2016.xls', 
                            sheet = 'choices'),
         envir=.GlobalEnv)
  
  xml <- xmlToList(xml)
  
  ###########
  #agric
  ###########

  ag_enumerator_first_name <- xml$staff_details$enumerator
  ag_enumerator_last_name <- xml$staff_details$enumerator_last_name
  ag_time_interview_start <- xml$staff_details$time_interview_start
  ag_date_of_interview <- xml$staff_details$data_entry_date
  ag_supervisor_first_name <- xml$staff_details$supervisor
  ag_supervisor_last_name <- xml$staff_details$supervisor_last_name
  ag_questionnaire_inspection_date <- xml$staff_details$questionnaire_inspection_date
  ag_clerk_first_name <- xml$staff_details$clerk
  ag_clerk_last_name <- xml$staff_details$clerk_last_name
  ag_data_entry_date <- xml$staff_details$data_entry_date
  ag_fields <- xml$fr_01
  ag_year <- xml$fr2_group$fr2_10
  ag_photo <- xml$fr_5
  ag12b_09a <- xml$fd12_9
  ag12a_10 <- ct(xml$fd12_10, 'microfinance')
  ag12a_10_other <- xml$fd12_10_other
  ag_survey_instrument <- xml$survey_instrument
  ag_end_time <- xml$end_time
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  
  ward <- xml$metadata$ward
  landscape_no <- xml$metadata$landscape_no
  town <- xml$metadata$town
  
  country <- xml$country
  
  district <- ct(xml$district, 'districts', country=country, region=xml$region)
  
  region <- ct(xml$region, 'regions', country=country)
  
  hh_refno <- getHHref(dbcon, xml$country, xml$metadata$landscape_no,
                       xml$metadata$eplot_no, xml$metadata$hh_no)
  
  round <- getRound(xml$country, ag_date_of_interview)
  
  agric <- vs.data.frame(uuid, survey_uuid, country, region,
                      district, ward, landscape_no, town, ag_enumerator_first_name, ag_enumerator_last_name,
                      ag_time_interview_start, ag_date_of_interview, ag_supervisor_first_name, ag_supervisor_last_name, ag_questionnaire_inspection_date,
                      ag_clerk_first_name, ag_clerk_last_name, ag_data_entry_date, ag_fields, ag_year, ag_photo, ag12b_09a, ag12a_10,
                      ag12a_10_other, hh_refno, ag_survey_instrument, ag_end_time, round)
  
  ################
  #piigeo_agric
  ################
  
  piigeo_agric <- data.frame(uuid, xml$metadata[ c('eplot_no', 'hh_no') ])
  
  ################
  #piiname_agric
  ################
  
  ag_head_name <- toupper(paste((xml$metadata[ c('hh_first_name', 'hh_last_name')]), collapse=' '))
                         
  piiname_agric <- data.frame(uuid, ag_head_name)
  
  #################
  #agric_cropbyprod
  #################
  
  if (xml$fd9_1=='1'){
    
    agric_cropbyprod <- data.frame()
    count <- 1
    for (i in xml$fd9_group[names(xml$fd9_group)=='fd9_5a_repeat']){
      uuid <- paste0(survey_uuid, '/', count)
      parent_uuid <- survey_uuid
      survey_uuid <- survey_uuid 
      crop_name <- i$fd9_5a_crop_name
      ag09_03 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_pro %>%
        ct('fd9_3_pro')
      ag09_03_other <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_pro_other
      ag09_03_by <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_by %>%
        ct('fd9_3_by')
      ag09_03_by_other <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_by_other
      ag09_03_product_name <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_product_name
      ag09_04_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_04_1
      ag09_04_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_04_2 %>%
        ct('kg_liter')
      ag09_07 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_7_1
      ag09_07_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_7_2 %>%
        ct('kg_liter')
      ag09_05 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_05
      ag09_06_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_06$fd9_5a_06_1
      ag09_06_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_06$fd9_5a_06_2 %>%
        ct('kg_liter')
      ag09_08 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_3$fd9_5a_08
      ag09_8a <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8a
      ag09_8b_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8b$fd9_5a_8b_1
      ag09_8b_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8b$fd9_5a_8b_2 %>%
        ct('kg_liter')
      ag09_10 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_09
      ag09_11 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_10
      is_processed <- xml$fd9_group$fd9_5a_repeat$fd9_5a_crop_not_none$fd9_5a_02_3_options == '1'
      is_byproduct <- xml$fd9_group$fd9_5a_repeat$fd9_5a_crop_not_none$fd9_5a_02_3_options == '2'

      count <- count + 1
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, crop_name, ag09_03, 
                              ag09_03_other, ag09_03_by, ag09_03_by_other, ag09_03_product_name, 
                              ag09_04_1, ag09_04_2, 
                              ag09_07, ag09_07_2, ag09_05, ag09_06_1, ag09_06_2, ag09_08, 
                              ag09_8a, ag09_8b_1, 
                              ag09_8b_2, ag09_10, ag09_11, is_processed, is_byproduct)
      
      agric_cropbyprod <- bind_rows(agric_cropbyprod, tempdf)
    }
  }else{
    agric_cropbyprod <- NULL
  }
  
  ###################
  #agric_extension
  ###################
  
  if(!is.null(xml$fd12_extension)){
    agric_extension <- data.frame()
    count <- 1
    for(i in xml$fd9_group[names(xml$fd9_group)=='f12_repeat']){
      uuid <- paste0(survey_uuid, '/', count)
      parent_uuid <- survey_uuid
      survey_uuid <- survey_uuid
      source_name <- i$fd12_source_name
      source_name_other <- i$fd12_other
      ag12a_02_1 <- i$fd12_1$fd12_02_a
      ag12a_02_2 <- i$fd12_1$fd12_02_a
      ag12a_02_3 <- i$fd12_1$fd12_02_a
      ag12a_02_4 <- i$fd12_1$fd12_02_a
      ag12a_02_5 <- i$fd12_1$fd12_02_a
      ag12a_02_6 <- i$fd12_1$fd12_02_a
      ag12a_03 <- i$fd12_3$fd12_03 %>%
        ct('fd3_6')
      ag12a_04 <- i$fd12_3$fd12_04
      ag12a_05 <- i$fd12_05
      
      count <- count + 1
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, source_name, source_name_other,
                              ag12a_02_1, ag12a_02_2, ag12a_02_3, ag12a_02_4, ag12a_02_5,
                              ag12a_02_6, ag12a_03, ag12a_04, ag12a_05)
      
      agric_extension <- bind_rows(agric_extension, tempdf)
    }
  }else{
    agric_extension <- NULL
  }
  
  #######################
  #agric_field
  # &
  #piigeo_agric_field
  #######################
  
  agric_field_a <- data.frame()
  piigeo_agric_field <- data.frame()
  count <- 1
  for(i in xml$fr[names(xml$fr)=='fr_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    field_name <- i$fr_field_name
    gps_lat <- makeGps(i$fr_4$location$gps_ns, i$fr_4$location$gps_lat)
    gps_long <- makeGps(i$fr_4$location$gps_ew, i$fr_4$location$gps_long)
    
    field_no <- i$fr_field_id
    ag2a_04 <- i$fr_3
    ag2a_07 <- i$fr2_6
    ag2a_08 <- i$fr2_7 %>%
      ct('fr3_7')
    ag2a_08_other <- i$fr2_7_other
  
    count <- count + 1
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no,
                            ag2a_04, ag2a_07, ag2a_08, ag2a_08_other)
    agric_field_a <- bind_rows(agric_field_a, tempdf)
    
    tempdf <- vs.data.frame(uuid, field_name, gps_lat, gps_long)
    piigeo_agric_field <- bind_rows(piigeo_agric_field, tempdf)
  }
  
  agric_field_b <- data.frame()
  for (i in xml$fr2_group[names(xml$fr2_group)=='fr2_repeat']){
    field_no <- i$fr2_field_id
    ag2a_02_1 <- i$fr2_8_grp$fr2_8_1
    ag2a_02_2 <- i$fr2_8_grp$fr2_8_2
    ag2a_02_2_unit <- i$fr2_8_grp$fr2_8_2_1 %>%
      ct('time')
    ag2a_09 <- i$fr2_9_grp$fr2_9
    ag2a_08a <- i$fr2_9_grp$fr2_9a
    ag2a_8b <- i$fr2_9_grp$fr2_9b
    ag2a_8c <- i$fr2_9_grp$fr2_9c
    
    tempdf <- vs.data.frame(field_no, ag2a_02_1, ag2a_02_2, ag2a_02_2_unit, ag2a_09,
                            ag2a_08a, ag2a_8b, ag2a_8c)
    agric_field_b <- bind_rows(agric_field_b, tempdf)
  }
  
  
  if (length(xml$fd3_group_label[names(xml$fd3_group_label)=='fd3_repeat'])==0){
    agric_field_c <- data.frame(field_no=agric_field_b$field_no)
  } else{
    agric_field_c <- data.frame()
    for (i in xml$fd3_group_label[names(xml$fd3_group_label)=='fd3_repeat']){
      field_no <- i$fd3_field_id
      ag2a_10_1 <- i$fd3_2$fd3_2_1
      ag2a_10_2 <- i$fd3_2$fd3_2_2
      ag2a_10_3 <- i$fd3_2$fd3_2_3
      
      tempdf <- vs.data.frame(field_no, ag2a_10_1, ag2a_10_2, ag2a_10_3)
      
      agric_field_c <- bind_rows(agric_field_c, tempdf)
    }
  }
  
  agric_field_d <- data.frame()
  for (i in xml[names(xml)=="fd10c_repeat"]){
    field_no <- i$fr10c_field_id
    ag10_1 <- i$fr10_1
    ag10_2 <- i$fr10_2
    ag10_2_1 <- i$fr10_2=='1'
    ag10_2_2 <- i$fr10_2=='2'
    ag10_2_3 <- i$fr10_2=='3'
    ag10_2_4 <- i$fr10_2=='4'
    ag10_2_5 <- i$fr10_2=='5'
    ag10_2_6 <- i$fr10_2=='6'
    ag10_2_7 <- i$fr10_2=='7'
    ag10_2_8 <- i$fr10_2=='8'
    ag10_2_9 <- i$fr10_2=='9'
    ag10_2_50 <- i$fr10_2=='50'
    ag10_2_51 <- i$fr10_2=='51'
    ag10_2_other <- i$fr10_2_other
    ag10_3 <- i$fr10_3
    ag10_3_other <- i$fr10_3_other
    
    tempdf <- vs.data.frame(field_no, ag10_1, ag10_2, ag10_2_1, ag10_2_2, ag10_2_3,
                            ag10_2_4, ag10_2_5, ag10_2_6, ag10_2_7, ag10_2_8, ag10_2_9,
                            ag10_2_50, ag10_2_51, ag10_2_other, ag10_3, ag10_3_other)
    
    agric_field_d <- bind_rows(agric_field_d, tempdf)
  }
  
  agric_field <- Reduce(f=function(x,y){merge(x,y,all=T)}, x=list(agric_field_a, agric_field_b, agric_field_c, agric_field_d))
  
  ############################
  #agric_field_permcrop
  ############################
  
  agric_field_permcrop <- data.frame()
  for(i in xml[names(xml)=='fd6aii_repeat']){
    count <- 1
    for(j in i[names(i)=='fd6aii_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6aii_field_id), '/', count, 'aii')
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6aii_field_id))
      survey_uuid <- survey_uuid
      crop_type <- 'fruit'
      field_no <- i$fd6aii_field_id
      crop_name <- j$fd6aii_crop_name
      ag6a_02 <- j$fd6aii$fd6aii_fr1_02
      ag6a_05 <- j$fd6aii$fd6aii_fr1_03
      ag6a_08_1 <- j$fd6aii$fd6aii_fr1_04_1
      ag6a_08_2 <- j$fd6aii$fd6aii_fr1_04_2 %>%
        ct('kg_liter')
      ag6a_09 <- j$fd6aii$fd6aii_fr1_05
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, crop_type, field_no, crop_name,
                              ag6a_02, ag6a_05, ag6a_08_1, ag6a_08_2, ag6a_09)
      
      agric_field_permcrop <- bind_rows(agric_field_permcrop, tempdf)
      
      count <- count + 1
    }
  }
  
  for(i in xml[names(xml)=='fd6bii_repeat']){
    count <- 1
    for(j in i[names(i)=='fd6bii_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6bii_field_id), '/', count, 'bii')
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6bii_field_id))
      survey_uuid <- survey_uuid
      crop_type <- 'permanent'
      field_no <- i$fd6bii_field_id
      crop_name <- j$fd6bii_crop_name
      ag6a_02 <- j$fd6bii$fd6bii_fr1_02
      ag6a_05 <- j$fd6bii$fd6bii_fr1_03
      ag6a_08_1 <- j$fd6bii$fd6bii_fr1_04_1
      ag6a_08_2 <- j$fd6bii$fd6bii_fr1_04_2 %>%
        ct('kg_liter')
      ag6a_09 <- j$fd6bii$fd6bii_fr1_05
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, crop_type, field_no, crop_name,
                              ag6a_02, ag6a_05, ag6a_08_1, ag6a_08_2, ag6a_09)
      
      agric_field_permcrop <- bind_rows(agric_field_permcrop, tempdf)
      
      count <- count + 1
    }
  }
  
  agric_field_permcrop <- agric_field_permcrop %>%
    filter(crop_name != 'None')

  ######################################################
  #agric_field_season & 
  #agric_field_season_individual &
  #piiname_agric_field_season_individual
  ######################################################
  
  agric_field_season_a <- data.frame()
  #long rainy
  for(i in xml$fr[names(xml$fr)=='fr_repeat']){
    uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fr_field_id), '/a')
    parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fr_field_id))
    survey_uuid <- survey_uuid
    field_no <- i$fr_field_id
    season <- 'long_rainy'
    ag3a_03 <- i$fr_2b1 %>%
      ct('landuse')
    ag3a_03_other <- i$fr2b1_other
    ag3a_07_1 <- i$fr_2b2 %>%
      ct('crops')
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, season,
                            ag3a_03, ag3a_03_other, ag3a_07_1)
    agric_field_season_a <- bind_rows(agric_field_season_a, tempdf)
  }
  
  #short rainy
  for(i in xml$fr[names(xml$fr)=='fr_repeat']){
    uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fr_field_id), '/b')
    parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fr_field_id))
    survey_uuid <- survey_uuid
    field_no <- i$fr_field_id
    season <- 'short_rainy'
    ag3a_03 <- i$fr_2c %>%
      ct('landuse')
    ag3a_03_other <- i$fr_2c_other
    ag3a_07_1 <- i$fr_2d %>%
      ct('crops')
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, season,
                            ag3a_03, ag3a_03_other, ag3a_07_1)
    agric_field_season_a <- bind_rows(agric_field_season_a, tempdf)
  }
  
  agric_field_season_individual <- data.frame()
  piiname_agric_field_season_individual <- data.frame()
  agric_field_season_b <- data.frame()
  uuidct <- 1
  #long rainy
  for(i in xml$fd3_group_label[names(xml$fd3_group_label)=='fd3_repeat']){
    season <- 'long_rainy'
    field_no <- i$fd3_field_id
    ag3a_03 <- i$fd3_3 %>%
      ct('landuse')
    ag3a_03_other <- i$fd3_3_other
    ag3a_04 <- i$fd3_4
    ag3a_07_1 <- i$fd3_5 %>%
      ct('crops')
    ag3a_10 <- i$fd3_6 %>%
      ct('fd3_6')
    ag3a_14 <- i$fd3_3_skip$fd32_7
    ag3a_15_1 <- i$fd3_3_skip$fd32_8$fd32_8_1 %>%
      ct('fd32_8')
    ag3a_15_2 <- i$fd3_3_skip$fd32_8$fd32_8_2 %>%
      ct('fd32_8')
    ag3a_17 <- i$fd32_9
    ag3a_18 <- i$fd32_1011$fd32_10 %>%
      ct('fd32_10')
    ag3a_18_other <- i$fd32_1011$fd32_10_other
    ag3a_20 <- i$fd32_1011$fd32_11 %>%
      ct('fd32_11')
    ag3a_20_other <- i$fd32_1011$fd32_10_other
    ag31_13 <- i$fd32_1011$fd32_12
    ag3a_23 <- i$fd32_13
    ag3a_24 <- i$fd33_14 %>%
      ct('fd33_14')
    ag3a_24_other <- i$fd33_14_other
    ag3a_28 <- i$fd33_15 %>%
      ct('fd33_15')
    ag3a_34 <- i$fd33_16 %>%
      ct('fd33_16')
    ag3a_33_17 <- i$fd33_17
    ag3a_39 <- i$fd33_18_1
    ag3a_39a <- i$fd3_organic$fd33_18b %>%
      ct('fd33_18b')
    ag3a_39a_other <- i$fd3_organic$fd33_18b_other
    ag3a_39b <- i$fd3_organic$fd33_18c %>%
      ct('fd33_18c')
    ag3a_39b_other <- i$fd3_organic$fd33_18c_other
    ag3a_40 <- i$fd3_organic$fd34$fd34_19
    ag3a_41 <- i$fd3_organic$fd34$fd34_20
    ag3a_42 <- i$fd3_organic$fd34_2$fd34_21
    ag3a_43 <- i$fd3_organic$fd34_2$fd34_22
    ag3a_45 <- i$fd35_23
    ag3a_46 <- i$fd35_inorg$fd35_24 %>%
      ct('fd35_24')
    ag3a_46_other <- i$fd35_inorg$fd35_24_other
    ag3a_47 <- i$fd35_inorg$fd35_25
    ag3a_48 <- i$fd35_inorg$fd35_26
    ag3a_49 <- i$fd35_inorg$fd35_27
    ag3a_52 <- i$fd36_28
    ag3a_53 <- i$fd36_inorg$fd36_29 %>%
      ct('fd36_29')
    ag3a_53_other <- i$fd36_inorg$fd36_29_other
    ag3a_54 <- i$fd36_inorg$fd36_30
    ag3a_55 <- i$fd36_inorg$fd36_31
    ag3a_56 <- i$fd36_inorg$fd36_32
    ag3a_58 <- i$fd37_33
    ag3a_59 <- i$fd37_pest$fd37_34 %>%
      ct('fd37_34')
    ag3a_59_other <- i$fd37_pest$fd37_34_other
    ag3a_60_1 <- i$fd37_pest$fd37_35_1
    ag3a_60_2 <- i$fd37_pest$fd37_35_2 %>%
      ct('fd37_35')
    ag3a_61 <- i$fd37_pest$fd37_36
    ag3a_72_1a <- i$fd38_38$fd38_38_1$fd38_38_1_1
    ag3a_72_2a <- i$fd38_38$fd38_38_1$fd38_38_1_2
    ag3a_72_3a <- i$fd38_38$fd38_38_1$fd38_38_1_3
    ag3a_72_4a <- i$fd38_38$fd38_38_1$fd38_38_1_4
    ag3a_72_1b <- i$fd38_38$fd38_38_2$fd38_38_1_1
    ag3a_72_2b <- i$fd38_38$fd38_38_2$fd38_38_1_2
    ag3a_72_3b <- i$fd38_38$fd38_38_2$fd38_38_1_3
    ag3a_72_4b <- i$fd38_38$fd38_38_2$fd38_38_1_4
    ag3a_72_1c <- i$fd38_38$fd38_38_3$fd38_38_1_1
    ag3a_72_2c <- i$fd38_38$fd38_38_3$fd38_38_1_2
    ag3a_72_3c <- i$fd38_38$fd38_38_3$fd38_38_1_3
    ag3a_72_4c <- i$fd38_38$fd38_38_3$fd38_38_1_4
    ag3a_72_1d <- i$fd38_38$fd38_38_4$fd38_38_1_1
    ag3a_72_2d <- i$fd38_38$fd38_38_4$fd38_38_1_2
    ag3a_72_3d <- i$fd38_38$fd38_38_4$fd38_38_1_3
    ag3a_72_4d <- i$fd38_38$fd38_38_4$fd38_38_1_4
    ag3a_39_1 <- grepl('1', i$fd3_organic$fd33_18a)
    ag3a_39_2 <- grepl('2', i$fd3_organic$fd33_18a)
    ag3a_39_4 <- grepl('4', i$fd3_organic$fd33_18a)
    ag3a_39_5 <- grepl('5', i$fd3_organic$fd33_18a)
    ag3a_39_6 <- grepl('6', i$fd3_organic$fd33_18a)
    ag3a_39_7 <- grepl('7', i$fd3_organic$fd33_18a)
    ag3a_39_8 <- grepl('8', i$fd3_organic$fd33_18a)
    ag3a_45_dap <- grepl('DAP', i$fd35_inorg$fd35_24a)
    ag3a_45_urea <- grepl('UREA', i$fd35_inorg$fd35_24a)
    ag3a_45_tsp <- grepl('TSP', i$fd35_inorg$fd35_24a)
    ag3a_45_can <- grepl('CAN', i$fd35_inorg$fd35_24a)
    ag3a_45_sa <- grepl('SA', i$fd35_inorg$fd35_24a)
    ag3a_45_npk <- grepl('NPK', i$fd35_inorg$fd35_24a)
    ag3a_45_mrp <- grepl('MRP', i$fd35_inorg$fd35_24a)
  
    tempdf <- vs.data.frame(season, field_no, ag3a_03, ag3a_03_other, ag3a_04, ag3a_07_1, ag3a_10, ag3a_14, ag3a_15_1, ag3a_15_2, ag3a_17, ag3a_18, 
                            ag3a_18_other, ag3a_20, ag3a_20_other, ag31_13, ag3a_23, ag3a_24, ag3a_24_other, ag3a_28, ag3a_34, 
                            ag3a_33_17, ag3a_39, ag3a_39a, ag3a_39a_other, ag3a_39b, ag3a_39b_other, ag3a_40, ag3a_41, ag3a_42, 
                            ag3a_43, ag3a_45, ag3a_46, ag3a_46_other, ag3a_47, ag3a_48, ag3a_49, ag3a_52, ag3a_53, ag3a_53_other, 
                            ag3a_54, ag3a_55, ag3a_56, ag3a_58, ag3a_59, ag3a_59_other, ag3a_60_1, ag3a_60_2, ag3a_61, ag3a_72_1a, 
                            ag3a_72_2a, ag3a_72_3a, ag3a_72_4a, ag3a_72_1b, ag3a_72_2b, ag3a_72_3b, ag3a_72_4b, ag3a_72_1c, ag3a_72_2c, 
                            ag3a_72_3c, ag3a_72_4c, ag3a_72_1d, ag3a_72_2d, ag3a_72_3d, ag3a_72_4d, ag3a_39_1, ag3a_39_2, ag3a_39_4, 
                            ag3a_39_5, ag3a_39_6, ag3a_39_7, ag3a_39_8, ag3a_45_dap, ag3a_45_urea, ag3a_45_tsp, ag3a_45_can, ag3a_45_sa, 
                            ag3a_45_npk, ag3a_45_mrp)
    
    agric_field_season_b <- bind_rows(agric_field_season_b, tempdf)
    
    indct <- 1
    for (j in i[names(i)=='fd37_labor']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd3_field_id), '/a/', uuidct)
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd3_field_id), '/a')
      survey_uuid <- survey_uuid
      ag3a_70_preparing <- j$fd37_questions$fd37_preparing
      ag3a_70_weeding <- j$fd37_questions$fd37_weeding
      ag3a_70_fertilizing <- j$fd37_questions$fd37_fertilizing
      ag3a_70_harvesting <- j$fd37_questions$fd37_harvesting
      ind_refno <- paste0('I', substr(100 + indct, 2, 3))
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ag3a_70_preparing,
                              ag3a_70_weeding, ag3a_70_fertilizing, ag3a_70_harvesting, ind_refno)
      agric_field_season_individual <- bind_rows(agric_field_season_individual, tempdf)
      
      ag_indid_name <- j$fd37_name
      tempdf <- vs.data.frame(uuid, ag_indid_name)
      piiname_agric_field_season_individual <- bind_rows(piiname_agric_field_season_individual, tempdf)
      
      uuidct <- uuidct + 1
      indct <- indct + 1
    }
  }
  
  #short rainy
  uuidct <- 1
  for(i in xml$fd3b[names(xml$fd3b)=='fd3b_repeat']){
    season <- 'short_rainy'
    field_no <- i$fd3b_field_id
    ag3a_03 <- i$fd3b_3 %>%
      ct('landuse')
    ag3a_03_other <- i$fd3b_3_other
    ag3a_04 <- i$fd3b_4
    ag3a_07_1 <- i$fd3b_5 %>%
      ct('crops')
    ag3a_10 <- i$fd3b_6 %>%
      ct('fd3_6')
    ag3a_14 <- i$fd3b_3_skip$fd3b2_7
    ag3a_15_1 <- i$fd3b_3_skip$fd3b2_8$fd3b2_8_1 %>%
      ct('fd32_8')
    ag3a_15_2 <- i$fd3b_3_skip$fd3b2_8$fd3b2_8_2 %>%
      ct('fd32_8')
    ag3a_17 <- i$fd3b2_9
    ag3a_18 <- i$fd3b2_1011$fd3b2_10 %>%
      ct('fd32_10')
    ag3a_18_other <- i$fd3b2_1011$fd3b2_10_other
    ag3a_20 <- i$fd3b2_1011$fd3b2_11 %>%
      ct('fd32_11')
    ag3a_20_other <- i$fd3b2_1011$fd3b2_10_other
    ag31_13 <- i$fd3b2_1011$fd3b2_12
    ag3a_23 <- i$fd3b2_13
    ag3a_24 <- i$fd3b3_14 %>%
      ct('fd33_14')
    ag3a_24_other <- i$fd3b3_14_other
    ag3a_28 <- i$fd3b3_15 %>%
      ct('fd33_15')
    ag3a_34 <- i$fd3b3_16 %>%
      ct('fd33_16')
    ag3a_33_17 <- i$fd3b3_17
    ag3a_39 <- i$fd3b3_18_1
    ag3a_39a <- i$fd3b_organic$fd3b3_18b %>%
      ct('fd33_18b')
    ag3a_39a_other <- i$fd3b_organic$fd3b3_18b_other
    ag3a_39b <- i$fd3b_organic$fd3b3_18c %>%
      ct('fd33_18c')
    ag3a_39b_other <- i$fd3b_organic$fd3b3_18c_other
    ag3a_40 <- i$fd3b_organic$fd3b4$fd3b4_19
    ag3a_41 <- i$fd3b_organic$fd3b4$fd3b4_20
    ag3a_42 <- i$fd3b_organic$fd3b4_2$fd3b4_21
    ag3a_43 <- i$fd3b_organic$fd3b4_2$fd3b4_22
    ag3a_45 <- i$fd3b5_23
    ag3a_46 <- i$fd3b5_inorg$fd3b5_24 %>%
      ct('fd35_24')
    ag3a_46_other <- i$fd3b5_inorg$fd3b5_24_other
    ag3a_47 <- i$fd3b5_inorg$fd3b5_25
    ag3a_48 <- i$fd3b5_inorg$fd3b5_26
    ag3a_49 <- i$fd3b5_inorg$fd3b5_27
    ag3a_52 <- i$fd3b6_28
    ag3a_53 <- i$fd3b6_inorg$fd3b6_29 %>%
      ct('fd36_29')
    ag3a_53_other <- i$fd3b6_inorg$fd3b6_29_other
    ag3a_54 <- i$fd3b6_inorg$fd3b6_30
    ag3a_55 <- i$fd3b6_inorg$fd3b6_31
    ag3a_56 <- i$fd3b6_inorg$fd3b6_32
    ag3a_58 <- i$fd3b7_33
    ag3a_59 <- i$fd3b7_pest$fd3b7_34 %>%
      ct('fd37_34')
    ag3a_59_other <- i$fd3b7_pest$fd3b7_34_other
    ag3a_60_1 <- i$fd3b7_pest$fd3b7_35_1
    ag3a_60_2 <- i$fd3b7_pest$fd3b7_35_2 %>%
      ct('fd37_35')
    ag3a_61 <- i$fd3b7_pest$fd3b7_36
    ag3a_72_1a <- i$fd3b8_38$fd3b8_38_1$fd3b8_38_1_1
    ag3a_72_2a <- i$fd3b8_38$fd3b8_38_1$fd3b8_38_1_2
    ag3a_72_3a <- i$fd3b8_38$fd3b8_38_1$fd3b8_38_1_3
    ag3a_72_4a <- i$fd3b8_38$fd3b8_38_1$fd3b8_38_1_4
    ag3a_72_1b <- i$fd3b8_38$fd3b8_38_2$fd3b8_38_1_1
    ag3a_72_2b <- i$fd3b8_38$fd3b8_38_2$fd3b8_38_1_2
    ag3a_72_3b <- i$fd3b8_38$fd3b8_38_2$fd3b8_38_1_3
    ag3a_72_4b <- i$fd3b8_38$fd3b8_38_2$fd3b8_38_1_4
    ag3a_72_1c <- i$fd3b8_38$fd3b8_38_3$fd3b8_38_1_1
    ag3a_72_2c <- i$fd3b8_38$fd3b8_38_3$fd3b8_38_1_2
    ag3a_72_3c <- i$fd3b8_38$fd3b8_38_3$fd3b8_38_1_3
    ag3a_72_4c <- i$fd3b8_38$fd3b8_38_3$fd3b8_38_1_4
    ag3a_72_1d <- i$fd3b8_38$fd3b8_38_4$fd3b8_38_1_1
    ag3a_72_2d <- i$fd3b8_38$fd3b8_38_4$fd3b8_38_1_2
    ag3a_72_3d <- i$fd3b8_38$fd3b8_38_4$fd3b8_38_1_3
    ag3a_72_4d <- i$fd3b8_38$fd3b8_38_4$fd3b8_38_1_4
    ag3a_39_1 <- grepl('1', i$fd3b_organic$fd3b3_18a)
    ag3a_39_2 <- grepl('2', i$fd3b_organic$fd3b3_18a)
    ag3a_39_4 <- grepl('4', i$fd3b_organic$fd3b3_18a)
    ag3a_39_5 <- grepl('5', i$fd3b_organic$fd3b3_18a)
    ag3a_39_6 <- grepl('6', i$fd3b_organic$fd3b3_18a)
    ag3a_39_7 <- grepl('7', i$fd3b_organic$fd3b3_18a)
    ag3a_39_8 <- grepl('8', i$fd3b_organic$fd3b3_18a)
    ag3a_45_dap <- grepl('DAP', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_urea <- grepl('UREA', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_tsp <- grepl('TSP', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_can <- grepl('CAN', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_sa <- grepl('SA', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_npk <- grepl('NPK', i$fd3b5_inorg$fd3b5_24a)
    ag3a_45_mrp <- grepl('MRP', i$fd3b5_inorg$fd3b5_24a)
    
    tempdf <- vs.data.frame(season, field_no, ag3a_03, ag3a_03_other, ag3a_04, ag3a_07_1, ag3a_10, ag3a_14, ag3a_15_1, ag3a_15_2, ag3a_17, ag3a_18, 
                            ag3a_18_other, ag3a_20, ag3a_20_other, ag31_13, ag3a_23, ag3a_24, ag3a_24_other, ag3a_28, ag3a_34, 
                            ag3a_33_17, ag3a_39, ag3a_39a, ag3a_39a_other, ag3a_39b, ag3a_39b_other, ag3a_40, ag3a_41, ag3a_42, 
                            ag3a_43, ag3a_45, ag3a_46, ag3a_46_other, ag3a_47, ag3a_48, ag3a_49, ag3a_52, ag3a_53, ag3a_53_other, 
                            ag3a_54, ag3a_55, ag3a_56, ag3a_58, ag3a_59, ag3a_59_other, ag3a_60_1, ag3a_60_2, ag3a_61, ag3a_72_1a, 
                            ag3a_72_2a, ag3a_72_3a, ag3a_72_4a, ag3a_72_1b, ag3a_72_2b, ag3a_72_3b, ag3a_72_4b, ag3a_72_1c, ag3a_72_2c, 
                            ag3a_72_3c, ag3a_72_4c, ag3a_72_1d, ag3a_72_2d, ag3a_72_3d, ag3a_72_4d, ag3a_39_1, ag3a_39_2, ag3a_39_4, 
                            ag3a_39_5, ag3a_39_6, ag3a_39_7, ag3a_39_8, ag3a_45_dap, ag3a_45_urea, ag3a_45_tsp, ag3a_45_can, ag3a_45_sa, 
                            ag3a_45_npk, ag3a_45_mrp)
    
    agric_field_season_b <- bind_rows(agric_field_season_b, tempdf)
    
    indct <- 1
    for (j in i[names(i)=='fd3b7_labor']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd3b_field_id), '/b/', uuidct)
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd3b_field_id), '/b')
      survey_uuid <- survey_uuid
      ag3a_70_preparing <- j$fd3b7_questions$fd3b7_preparing
      ag3a_70_weeding <- j$fd3b7_questions$fd3b7_weeding
      ag3a_70_fertilizing <- j$fd3b7_questions$fd3b7_fertilizing
      ag3a_70_harvesting <- j$fd3b7_questions$fd3b7_harvesting
      ind_refno <- paste0('I', substr(100 + indct, 2, 3))
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ag3a_70_preparing,
                              ag3a_70_weeding, ag3a_70_fertilizing, ag3a_70_harvesting, ind_refno)
      agric_field_season_individual <- bind_rows(agric_field_season_individual, tempdf)
      
      ag_indid_name <- j$fd3b7_name
      tempdf <- vs.data.frame(uuid, ag_indid_name)
      piiname_agric_field_season_individual <- bind_rows(piiname_agric_field_season_individual, tempdf)
      
      uuidct <- uuidct + 1
      indct <- indct + 1
    }
  }
  
  agric_field_season <- merge(agric_field_season_a,
                              agric_field_season_b,
                              by=c('field_no', 'season')) %>%
    rowcoalesce(c('ag3a_03', 'ag3a_03_other', 'ag3a_07_1'))

  
  ################################
  #agric_field_season_fieldcrop
  ################################
  
  agric_field_season_fieldcrop <- data.frame()
  
  #long rainy
  for (i in xml$fd4[names(xml$fd4)=='fd4_repeat']){
    count <- 1
    for (j in i[names(i)=='fd4_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd4_field_id), '/a/', count)
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd4_field_id), '/a')
      survey_uuid <- survey_uuid
      field_no <- i$fd4_field_id
      season <- 'long_rainy'
      crop_name <- j$fd4_crop_name
      ag4a_01 <- j$fd4_crop_not_none$fd4_01
      ag4a_02 <- j$fd4_crop_not_none$fd4_02 %>%
        ct('fd4_2')
      ag4a_04 <- j$fd4_crop_not_none$fd4_03
      ag4a_05 <- j$fd4_crop_not_none$fd4_04 %>%
        ct('fd4_4')
      ag4a_05_other <- j$fd4_crop_not_none$fd4_04_other
      ag4a_5a <- j$fd4_crop_not_none$fd4_05
      ag4a_06 <- j$fd4_crop_not_none$fd4_06
      ag4a_08 <- j$fd4_crop_not_none$fd4_2$fd4_07
      ag4a_15 <- j$fd4_crop_not_none$fd4_2$fd4_08_1
      ag4a_15_unit <- j$fd4_crop_not_none$fd4_2$fd4_08_2 %>%
        ct('kg_liter')
      ag4a_16 <- j$fd4_crop_not_none$fd4_2$fd4_09
      ag4a_19 <- j$fd4_crop_not_none$fd4_10
      ag4a_21 <- j$fd4_crop_not_none$fd4_3$fd4_11
      ag4a_23 <- j$fd4_crop_not_none$fd4_3$fd4_12 %>%
        ct('fd4_12')
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, season, crop_name, 
                              ag4a_01, ag4a_02, ag4a_04, ag4a_05, ag4a_05_other, ag4a_5a, ag4a_06, 
                              ag4a_08, ag4a_15, ag4a_15_unit, ag4a_16, ag4a_19, ag4a_21, ag4a_23)

      agric_field_season_fieldcrop <- bind_rows(agric_field_season_fieldcrop, tempdf)
      
      count <- count + 1
    }
  }
  
  
  #short rainy
  for (i in xml$fd4b[names(xml$fd4b)=='fd4b_repeat']){
    count <- 1
    for (j in i[names(i)=='fd4b_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd4b_field_id), '/b/', count)
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd4b_field_id), '/b')
      survey_uuid <- survey_uuid
      field_no <- i$fd4b_field_id
      season <- 'short_rainy'
      crop_name <- j$fd4b_crop_name
      ag4a_01 <- j$fd4b_1$fd4b_01
      ag4a_02 <- j$fd4b_1$fd4b_02 %>%
        ct('fd4_2')
      ag4a_04 <- j$fd4b_1$fd4b_03
      ag4a_05 <- j$fd4b_1$fd4b_04 %>%
        ct('fd4_4')
      ag4a_05_other <- j$fd4b_1$fd4b_04_other
      ag4a_5a <- j$fd4b_1$fd4b_05
      ag4a_06 <- j$fd4b_1$fd4b_06
      ag4a_08 <- j$fd4b_1$fd4b_2$fd4b_07
      ag4a_15 <- j$fd4b_1$fd4b_2$fd4b_08_1
      ag4a_15_unit <- j$fd4b_1$fd4b_2$fd4b_08_2 %>%
        ct('kg_liter')
      ag4a_16 <- j$fd4b_1$fd4b_2$fd4b_09
      ag4a_19 <- j$fd4b_1$fd4b_10
      ag4a_21 <- j$fd4b_1$fd4b_3$fd4b_11
      ag4a_23 <- j$fd4b_1$fd4b_3$fd4b_12 %>%
        ct('fd4_12')
      
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, season, crop_name, 
                              ag4a_01, ag4a_02, ag4a_04, ag4a_05, ag4a_05_other, ag4a_5a, ag4a_06, 
                              ag4a_08, ag4a_15, ag4a_15_unit, ag4a_16, ag4a_19, ag4a_21, ag4a_23)
      
      agric_field_season_fieldcrop <- bind_rows(agric_field_season_fieldcrop, tempdf)
      
      count <- count + 1
    }
  }
  
  ##########################
  #agric_fieldcrop
  ##########################
  
  #long rainy
  agric_fieldcrop <- data.frame()
  count <- 1
  for (i in xml$fd5_group[names(xml$fd5_group)=='fd5_repeat']){
    uuid <- paste0(survey_uuid, '/', count, 'a')
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    season <- 'long_rainy'
    crop_name <- i$fd5_crop_name
    ag5a_01 <- i$fd5$fd5_01
    ag5a_02_1 <- i$fd5$fd5_1$fd5_02_1
    ag5a_02_2 <- i$fd5$fd5_1$fd5_02_2 %>%
      ct('kg_liter')
    ag5a_03 <- i$fd5$fd5_1$fd5_03
    ag5a_12_1 <- i$fd5$fd5_1$fd5_04
    ag5a_20 <- i$fd5$fd5_05
    ag5a_21 <- i$fd5$fd5_2$fd5_06 %>%
      ct('fd5_06')
    ag5a_21_other <- i$fd5$fd5_2$fd5_06_other
    ag5a_22 <- i$fd5$fd5_2$fd5_07
    ag5a_24 <- i$fd5$fd5_08 %>%
      ct('fd5_08')
    ag5a_24_other <- i$fd5$fd5_08_other
    ag5a_25 <- i$fd5$fd5_3$fd5_09
    ag5a_26 <- i$fd5$fd5_3$fd5_10
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, season, crop_name, ag5a_01, 
                            ag5a_02_1, ag5a_02_2, ag5a_03, ag5a_12_1, ag5a_20, ag5a_21, 
                            ag5a_21_other, ag5a_22, ag5a_24, ag5a_24_other, ag5a_25, ag5a_26)
    agric_fieldcrop <- bind_rows(agric_fieldcrop, tempdf)
    
    count <- count + 1
  }
  
  #short rainy
  count <- 1
  for (i in xml$fd5s_group[names(xml$fd5s_group)=='fd5s_repeat']){
    uuid <- paste0(survey_uuid, '/', count, 'b')
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    season <- 'short_rainy'
    crop_name <- i$fd5s_crop_name
    ag5a_01 <- i$fd5s$fd5s_01
    ag5a_02_1 <- i$fd5s$fd5s_1$fd5s_02_1
    ag5a_02_2 <- i$fd5s$fd5s_1$fd5s_02_2 %>%
      ct('kg_liter')
    ag5a_03 <- i$fd5s$fd5s_1$fd5s_03
    ag5a_12_1 <- i$fd5s$fd5s_1$fd5s_04
    ag5a_20 <- i$fd5s$fd5s_05
    ag5a_21 <- i$fd5s$fd5s_2$fd5s_06 %>%
      ct('fd5_06')
    ag5a_21_other <- i$fd5s$fd5s_2$fd5s_06_other
    ag5a_22 <- i$fd5s$fd5s_2$fd5s_07
    ag5a_24 <- i$fd5s$fd5s_08 %>%
      ct('fd5_08')
    ag5a_24_other <- i$fd5s$fd5s_08_other
    ag5a_25 <- i$fd5s$fd5s_3$fd5s_09
    ag5a_26 <- i$fd5s$fd5s_3$fd5s_10
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, season, crop_name, ag5a_01, 
                            ag5a_02_1, ag5a_02_2, ag5a_03, ag5a_12_1, ag5a_20, ag5a_21, 
                            ag5a_21_other, ag5a_22, ag5a_24, ag5a_24_other, ag5a_25, ag5a_26)
    agric_fieldcrop <- bind_rows(agric_fieldcrop, tempdf)
    
    count <- count + 1
  }
  
  ##############################
  #agric_implement
  ##############################
  
  agric_implement <- data.frame()
  count <- 1
  for (i in xml[names(xml)=='fd11_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    toolname <- i$fd11_tool_name
    ag11_01 <- i$fd11_group$fd11_1
    ag11_07 <- i$fd11_group$fd11_6
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, toolname,
                            ag11_01, ag11_07)
    agric_implement <- bind_rows(agric_implement, tempdf)
    count <- count + 1
  }
  
  ##############################
  #agric_individual
  #&
  #piiname_agric_individual
  ##############################
  
  agric_individual <- data.frame()
  piiname_agric_individual <- data.frame()
  count <- 1
  for (i in xml$hh_roster[names(xml$hh_roster)=='hh_roster_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    ag_indid_age <- i$hh_roster_group$hh_indid_age
    ag_indid_gender <- i$hh_roster_group$hh_indid_gender %>%
      ct('mf')
    ag_indid_respondent <- i$hh_roster_group$hh_indid_respondent
    ind_refno <- paste0('I', substr(100+count,2,3))
    ag_indid_name <- i$hh_roster_group$hh_indid_name
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ag_indid_age,
                            ag_indid_gender, ag_indid_respondent, ind_refno)
    agric_individual <- bind_rows(agric_individual, tempdf)
    
    tempdf <- vs.data.frame(uuid, ag_indid_name)
    piiname_agric_individual <- bind_rows(piiname_agric_individual, tempdf)
    
    count <- count + 1
  }
  
  ########################
  #agric_livestock
  ########################
  
  agric_livestock <- data.frame()
  count <- 1
  for (i in xml[names(xml)=='fd10_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    animal_name <- i$fd10_animal_name
    ag10a_05_1 <- i$fd10_own_group$fd10_own_indigenous
    ag10a_05_2 <- i$fd10_own_group$fd10_own_improved_beef
    ag10a_05_3 <- i$fd10_own_group$fd10_own_improved_dairy
    ag10a_19 <- i$fd10_3
    ag10a_20 <- i$fd10_4
    ag10a_21 <- i$fd10_5
    ag10a_24 <- i$fd10_6
    ag10a_25 <- i$fd10_7
    ag10a_26 <- i$fd10_8
    ag10a_27 <- i$fd10_9
    ag10a_33 <- i$fd10_11
    ag10a_34 <- i$fd10_12
    ag10a_01 <- i$fd10_13_group$fd10_13_legume
    ag10a_02 <- i$fd10_13_group$fd10_13_green_grass
    ag10a_03 <- i$fd10_13_group$fd10_13_crop_cutting
    ag10a_04 <- i$fd10_13_group$fd10_13_rough_grazing
    
    tempdf <-  vs.data.frame(uuid, parent_uuid, survey_uuid, animal_name, ag10a_05_1, ag10a_05_2, ag10a_05_3, 
                  ag10a_19, ag10a_20, ag10a_21, ag10a_24, ag10a_25, ag10a_26, ag10a_27, ag10a_33, 
                  ag10a_34, ag10a_01, ag10a_02, ag10a_03, ag10a_04)
  
    agric_livestock <- bind_rows(agric_livestock, tempdf)
    
    count <- count +1 
  }
  
  #######################################
  #agric_livestockbyprod
  ######################################
  
  agric_livestockbyprod <- data.frame()
  count <- 1
  for (i in xml[names(xml)=='fd10b_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    ag10b_01 <- i$fd10b_byproduct_name
    ag10b_1a <- i$fd10a_1a$fd10a_1a_quantity
    ag10b_1a_unit <- i$fd10a_1a$fd10a_1a_unit %>%
      ct('fd10b_unit')
    ag10b_1b <- i$fd10a_1b
    ag10b_05_1 <- i$fd10a_1c$fd10a_1c_quantity
    ag10b_05_2 <- i$fd10a_1c$fd10a_1c_units %>%
      ct('fd10b_unit')
    ag10b_06 <- i$fd10a_2
  
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ag10b_01, ag10b_1a, ag10b_1a_unit, 
                  ag10b_1b, ag10b_05_1, ag10b_05_2, ag10b_06)
    agric_livestockbyprod <- bind_rows(agric_livestockbyprod, tempdf)
    count <- count + 1
  }
  
  for (i in xml[names(xml)=='fd10_other_b_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    ag10b_01 <- i$fd10_other_b_byproduct_name
    ag10b_1a <- i$fd10_other_a_1a$fd10_other_a_1a_quantity
    ag10b_1a_unit <- i$fd10_other_a_1a$fd10_other_a_1a_unit %>%
      ct('fd10b_unit')
    ag10b_1b <- i$fd10_other_a_1b
    ag10b_05_1 <- i$fd10_other_a_1c$fd10_other_a_1c_quantity
    ag10b_05_2 <- i$fd10_other_a_1c$fd10_other_a_1c_units %>%
      ct('fd10b_unit')
    ag10b_06 <- i$fd10_other_a_2
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ag10b_01, ag10b_1a, ag10b_1a_unit, 
                            ag10b_1b, ag10b_05_1, ag10b_05_2, ag10b_06)
    agric_livestockbyprod <- bind_rows(agric_livestockbyprod, tempdf)
    count <- count + 1
  }
  
  #########################################
  #agric_permcrop
  ########################################

  agric_permcrop <- data.frame()
  count <- 1
  for(i in xml[names(xml)=='fd7aii_crops']){
    uuid <- paste0(survey_uuid, '/', count, 'aii')
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    crop_type <- 'permanent'
    crop_name <- i$fd7aii_crop_name
    ag7a_02 <- i$fd7ii_check$fd7ii_fr1_02
    ag7a_03_1 <- i$fd7ii_check$fd7ii_fr1_1$fd7ii_fr1_03_1
    ag7a_03_2 <- i$fd7ii_check$fd7ii_fr1_1$fd7ii_fr1_03_2 %>%
      ct('kg_liter')
    ag7a_04 <- i$fd7ii_check$fd7ii_fr1_1$fd7ii_fr1_04
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, crop_type,
                            crop_name, ag7a_02, ag7a_03_1, ag7a_03_2,
                            ag7a_04)
    agric_permcrop <- bind_rows(agric_permcrop, tempdf)
    count <- count + 1
  }
  
  count <- 1
  for(i in xml[names(xml)=='fd7bii_crops']){
    uuid <- paste0(survey_uuid, '/', count, 'bii')
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    crop_type <- 'fruit'
    crop_name <- i$fd7bii_crop_name
    ag7a_02 <- i$fd7bii_check$fd7bii_fr1_02
    ag7a_03_1 <- i$fd7bii_check$fd7bii_fr1_1$fd7bii_fr1_03_1
    ag7a_03_2 <- i$fd7bii_check$fd7bii_fr1_1$fd7bii_fr1_03_2 %>%
      ct('kg_liter')
    ag7a_04 <- i$fd7bii_check$fd7bii_fr1_1$fd7bii_fr1_04
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, crop_type,
                            crop_name, ag7a_02, ag7a_03_1, ag7a_03_2,
                            ag7a_04)
    agric_permcrop <- bind_rows(agric_permcrop, tempdf)
    count <- count + 1
  }
  
  agric_permcrop <- agric_permcrop %>% filter(!is.na(crop_name), crop_name != 'None')
  
  ##############################
  #agric_priceinfo
  #############################
  
  agric_priceinfo <- data.frame()
  count <- 1
  for(i in xml[names(xml)=='fd12_repeat_family']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    source_name_family <- i$fd12_source_name_family
    ag12b_06 <- i$fd12_6_other
    ag12b_08 <- i$fd12_7
    ag12b_09 <- i$fd12_8
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, source_name_family,
                            ag12b_06, ag12b_08, ag12b_09)
    agric_priceinfo <- bind_rows(agric_priceinfo, tempdf)
    count <- count + 1
  }
  
  ########################
  #Write Info
  #########################
  insertDF(dbcon, agric, 'agric', test)
  insertDF(dbcon, piigeo_agric, 'piigeo_agric', test)
  insertDF(dbcon, piiname_agric, 'piiname_agric', test)
  if (!is.null(agric_cropbyprod)){
    insertDF(dbcon, agric_cropbyprod, 'agric_cropbyprod', test)
  }
  if (!is.null(agric_extension)){
    insertDF(dbcon, agric_extension, 'agric_extension', test)
  }
  insertDF(dbcon, agric_field, 'agric_field', test)
  insertDF(dbcon, piigeo_agric_field, 'piigeo_agric_field', test)
  insertDF(dbcon, agric_field_permcrop, 'agric_field_permcrop', test)
  insertDF(dbcon, agric_field_season, 'agric_field_season', test)
  insertDF(dbcon, agric_field_season_individual, 'agric_field_season_individual', test)
  insertDF(dbcon, piiname_agric_field_season_individual, 'piiname_agric_field_season_individual', test)
  insertDF(dbcon, agric_field_season_fieldcrop, 'agric_field_season_fieldcrop', test)
  insertDF(dbcon, agric_fieldcrop, 'agric_fieldcrop', test)
  insertDF(dbcon, agric_implement, 'agric_implement', test)
  insertDF(dbcon, agric_individual, 'agric_individual', test)
  insertDF(dbcon, piiname_agric_individual, 'piiname_agric_individual', test)
  insertDF(dbcon, agric_livestock, 'agric_livestock', test)
  insertDF(dbcon, agric_livestockbyprod, 'agric_livestockbyprod', test)
  insertDF(dbcon, agric_permcrop, 'agric_permcrop', test)
  insertDF(dbcon, agric_priceinfo, 'agric_priceinfo', test)
  
  if (!test){
    dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                              survey_uuid, "',",
                              "'agriculture_survey_15_may_2016','",
                              xml$today,"',current_date);")) 
  }
}