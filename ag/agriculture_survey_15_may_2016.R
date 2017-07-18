library(XML)
library(readxl)
library(RPostgreSQL)

options(stringsAsFactors = FALSE)

agriculture_survey_15_may_2016 <- function(con, xml, test=FALSE, codedf){
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
  ag12a_10 <- codetext(xml$fd12_10, 'microfinance', codedf)
  ag12a_10_other <- xml$fd12_10_other
  ag_survey_instrument <- xml$survey_instrument
  ag_end_time <- xml$end_time
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  
  ward <- xml$metadata$ward
  landscape_no <- xml$metadata$landscape_no
  town <- xml$metadata$town
  
  country <- xml$country
  
  district <- codetext(xml$district, 'districts', codedf, country=country, region=xml$region)
  
  region <- codetext(xml$region, 'regions', codedf, country=country)
  
  hh_refno <- getHHref(con, xml$country, xml$metadata$landscape_no,
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
    ct <- 1
    for (i in xml$fd9_group[names(xml$fd9_group)=='fd9_5a_repeat']){
      uuid <- paste0(survey_uuid, '/', ct)
      parent_uuid <- survey_uuid
      survey_uuid <- survey_uuid 
      crop_name <- i$fd9_5a_crop_name
      ag09_03 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_pro %>%
        codetext('fd9_3_pro', codedf)
      ag09_03_other <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_pro_other
      ag09_03_by <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_by %>%
        codetext('fd9_3_by', codedf)
      ag09_03_by_other <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_03_by_other
      ag09_03_product_name <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_product_name
      ag09_04_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_04_1
      ag09_04_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_04_2 %>%
        codetext('kg_liter', codedf)
      ag09_07 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_7_1
      ag09_07_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_04$fd9_5a_7_2 %>%
        codetext('kg_liter', codedf)
      ag09_05 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_05
      ag09_06_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_06$fd9_5a_06_1
      ag09_06_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_06$fd9_5a_06_2 %>%
        codetext('kg_liter', codedf)
      ag09_08 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_2$fd9_5a_3$fd9_5a_08
      ag09_8a <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8a
      ag09_8b_1 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8b$fd9_5a_8b_1
      ag09_8b_2 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_8b$fd9_5a_8b_2 %>%
        codetext('kg_liter', codedf)
      ag09_10 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_09 %>%
        codetext('kg_liter', codedf)
      ag09_11 <- i$fd9_5a_crop_not_none$fd9_5a_1$fd9_5a_02_3_repeat$fd9_5a_10
      is_processed <- xml$fd9_group$fd9_5a_repeat$fd9_5a_crop_not_none$fd9_5a_02_3_options == '1'
      is_byproduct <- xml$fd9_group$fd9_5a_repeat$fd9_5a_crop_not_none$fd9_5a_02_3_options == '2'

      ct <- ct + 1
      
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
    ct <- 1
    for(i in xml$fd9_group[names(xml$fd9_group)=='f12_repeat']){
      uuid <- paste0(survey_uuid, '/', ct)
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
        codetext('fd3_6', codedf)
      ag12a_04 <- i$fd12_3$fd12_04
      ag12a_05 <- i$fd12_05
      
      ct <- ct + 1
      
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
  ct <- 1
  for(i in xml$fr[names(xml$fr)=='fr_repeat']){
    uuid <- paste0(survey_uuid, '/', ct)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    field_name <- i$fr_field_name
    gps_lat <- makeGps(i$fr_4$location$gps_ns, i$fr_4$location$gps_lat)
    gps_long <- makeGps(i$fr_4$location$gps_ew, i$fr_4$location$gps_long)
    
    field_no <- i$fr_field_id
    ag2a_04 <- i$fr_3
    ag2a_07 <- i$fr2_6
    ag2a_08 <- i$fr2_7 %>%
      codetext('fr3_7', codedf)
    ag2a_08_other <- i$fr2_7_other
  
    ct <- ct + 1
    
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
      codetext('time', codedf)
    ag2a_09 <- i$fr2_9_grp$fr2_9
    ag2a_08a <- i$fr2_9_grp$fr2_9a
    ag2a_8b <- i$fr2_9_grp$fr2_9b
    ag2a_8c <- i$fr2_9_grp$fr2_9c
    
    tempdf <- vs.data.frame(field_no, ag2a_02_1, ag2a_02_2, ag2a_02_2_unit, ag2a_09,
                            ag2a_08a, ag2a_8b, ag2a_8c)
    agric_field_b <- bind_rows(agric_field_b, tempdf)
  }
  
  agric_field_c <- data.frame()
  for (i in xml$fd3_group_label[names(xml$fd3_group_label)=='fd3_repeat']){
    field_no <- i$fd3_field_id
    ag2a_10_1 <- i$fd3_2$fd3_2_1
    ag2a_10_2 <- i$fd3_2$fd3_2_2
    ag2a_10_3 <- i$fd3_2$fd3_2_3
    
    tempdf <- vs.data.frame(field_no, ag2a_10_1, ag2a_10_2, ag2a_10_3)
    
    agric_field_c <- bind_rows(agric_field_c, tempdf)
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
  
  agric_field <- Reduce(f=merge, x=list(agric_field_a, agric_field_b, agric_field_c, agric_field_d))
  
  ############################
  #agric_field_permcrop
  ############################
  
  agric_field_permcrop <- data.frame()
  for(i in xml[names(xml)=='fd6aii_repeat']){
    ct <- 1
    for(j in i[names(i)=='fd6aii_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6aii_field_id), '/', ct, 'aii')
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6aii_field_id))
      crop_type <- 'fruit'
      field_no <- i$fd6aii_field_id
      crop_name <- j$fd6aii_crop_name
      ag6a_02 <- j$fd6aii$fd6aii_fr1_02
      ag6a_05 <- j$fd6aii$fd6aii_fr1_03
      ag6a_08_1 <- j$fd6aii$fd6aii_fr1_04_1
      ag6a_08_2 <- j$fd6aii$fd6aii_fr1_04_2 %>%
        codetext('kg_liter', codedf)
      ag6a_09 <- j$fd6aii$fd6aii_fr1_05
      
      tempdf <- vs.data.frame(uuid, parent_uuid, crop_type, field_no, crop_name,
                              ag6a_02, ag6a_05, ag6a_08_1, ag6a_08_2, ag6a_09)
      
      agric_field_permcrop <- bind_rows(agric_field_permcrop, tempdf)
      
      ct <- ct + 1
    }
  }
  
  for(i in xml[names(xml)=='fd6bii_repeat']){
    ct <- 1
    for(j in i[names(i)=='fd6bii_crops']){
      uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6bii_field_id), '/', ct, 'bii')
      parent_uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6bii_field_id))
      crop_type <- 'permanent'
      field_no <- i$fd6bii_field_id
      crop_name <- j$fd6bii_crop_name
      ag6a_02 <- j$fd6bii$fd6bii_fr1_02
      ag6a_05 <- j$fd6bii$fd6bii_fr1_03
      ag6a_08_1 <- j$fd6bii$fd6bii_fr1_04_1
      ag6a_08_2 <- j$fd6bii$fd6bii_fr1_04_2 %>%
        codetext('kg_liter', codedf)
      ag6a_09 <- j$fd6bii$fd6bii_fr1_05
      
      tempdf <- vs.data.frame(uuid, parent_uuid, crop_type, field_no, crop_name,
                              ag6a_02, ag6a_05, ag6a_08_1, ag6a_08_2, ag6a_09)
      
      agric_field_permcrop <- bind_rows(agric_field_permcrop, tempdf)
      
      ct <- ct + 1
    }
  }
  
  agric_field_permcrop <- agric_field_permcrop %>%
    filter(crop_name != 'None')

  ############################
  #agric_field_season
  ############################
  
  agric_field_season <- data.frame()
  ct <- 1
  for(i in xml$fr[names(xml$fr)=='fr_repeat']){
    uuid <- paste0(survey_uuid, '/', gsub('M', '', i$fd6bii_field_id))
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    field_no <- i$fr_field_id
    season <- 'long_rainy'
    ag3a_03 <- i$fr_2b1 %>%
      codetext('landuse', codedf)
    ag3a_03_other <- i$fr2b1_other
    
  for(i in xml$
    ag3a_04 <- 
  ag3a_07_1
  ag3a_10
  ag3a_14
  ag3a_15_1
  ag3a_15_2
  ag3a_17
  ag3a_18
  ag3a_18_other
  ag3a_20
  ag3a_20_other
  ag31_13
  ag3a_23
  ag3a_24
  ag3a_24_other
  ag3a_28
  ag3a_34
  ag3a_33_17
  ag3a_39
  ag3a_39a
  ag3a_39a_other
  ag3a_39b
  ag3a_39b_other
  ag3a_40
  ag3a_41
  ag3a_42
  ag3a_43
  ag3a_45
  ag3a_46
  ag3a_46_other
  ag3a_47
  ag3a_48
  ag3a_49
  ag3a_52
  ag3a_53
  ag3a_53_other
  ag3a_54
  ag3a_55
  ag3a_56
  ag3a_58
  ag3a_59
  ag3a_59_other
  ag3a_60_1
  ag3a_60_2
  ag3a_61
  ag3a_72_1a
  ag3a_72_2a
  ag3a_72_3a
  ag3a_72_4a
  ag3a_72_1b
  ag3a_72_2b
  ag3a_72_3b
  ag3a_72_4b
  ag3a_72_1c
  ag3a_72_2c
  ag3a_72_3c
  ag3a_72_4c
  ag3a_72_1d
  ag3a_72_2d
  ag3a_72_3d
  ag3a_72_4d
  ag3a_39_1
  ag3a_39_2
  ag3a_39_4
  ag3a_39_5
  ag3a_39_6
  ag3a_39_7
  ag3a_39_8
  ag3a_45_dap
  ag3a_45_urea
  ag3a_45_tsp
  ag3a_45_can
  ag3a_45_sa
  ag3a_45_npk
  ag3a_45_mrp
  
  
  
  
  
  
  
  
  
  
}