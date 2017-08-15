house_hold_15_may_2016 <- function(dbcon, xml, test=FALSE){
  codedf <- read_xls('hh/VS_Household.15.05.2016.xls', sheet = 'choices')
  
  xml <- xmlToList(xml)
  
  ###############################
  #household
  ##################################
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  country <- xml$metadata$country
  region <- xml$metadata$region %>%
    ct('regions', country=country)
  district <- xml$metadata$district %>%
    ct('districts', country=country, region=xml$metadata$region)
  ward <- xml$metadata$ward
  town <- xml$metadata$town
  landscape_no <- xml$metadata$landscape_no
  hh_refno <- getHHref(dbcon, xml$metadata$country, xml$metadata$landscape_no,
                       xml$metadata$eplot_no, xml$metadata$hh_no)
  hh_interview_date <- xml$interview_event$interview_date
  round <- getRound(country, hh_interview_date)
  hh_interview_time <- xml$interview_event$interview_time
  hh_time_finish <- xml$time_finish
  hh_questionnaire_inspection_date <- xml$staff_details$questionnaire_inspection_date
  if(!is.null(xml$staff_details$clerk)){
    hh_clerk_first_name <- xml$staff_details$clerk
    hh_clerk_last_name <- NULL
  }else{
    hh_clerk_first_name <- xml$staff_details$clerk_first_name
    hh_clerk_last_name <- xml$staff_details$clerk_last_name
  }
  hh_data_entry_date <- xml$staff_details$data_entry_date
  hh_data_entry_date_2 <- xml$staff_details$data_entry_date_2
  if(!is.null(xml$staff_details$supervisor)){
    hh_supervisor_first_name <- xml$staff_details$supervisor
    hh_supervisor_last_name <- NULL
  }else{
    hh_supervisor_first_name <- xml$staff_details$supervisor_first_name
    hh_supervisor_last_name <- xml$staff_details$supervisor_last_name
  }
  hh_observations <- xml$observations
  hh_survey_instrument <- xml$survey_instrument

  hh_hv105b_01 <- xml$HV2$hh_hv105b_01
  hh_hv105c_01 <- xml$HV2$hh_hv105c_01 %>%
    ct('primary_fuelwood')
  hh_hv106_01 <- xml$HV2$hh_hv106_01
  hh_hv109_01 <- xml$HV2$hh_hv109_01 %>%
    ct('hv9')
  hh_hv109a_01 <- xml$HV2$hh_hv109a_01 %>%
    ct('percentage_fuelwood')
  hh_hv109b_01 <- xml$HV2$hh_hv109b_01
  hh_hv109c_01 <- xml$HV2$hh_hv109c_01
  hh_hv109c_01_1 <- (grepl('1', xml$HV2$hh_hv109c_01) & !grepl('10|11|12', xml$HV2$hh_hv109c_01)) | grepl('1 ', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_10 <- grepl('10', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_11 <- grepl('11', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_12 <- grepl('12', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_2 <- (grepl('2', xml$HV2$hh_hv109c_01) & !grepl('12', xml$HV2$hh_hv109c_01)) | grepl('2 | 2', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_3 <- grepl('3', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_4 <- grepl('4', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_5 <- grepl('5', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_6 <- grepl('6', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_7 <- grepl('7', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_8 <- grepl('8', xml$HV2$hh_hv109c_01)
  hh_hv109c_01_9 <- grepl('9', xml$HV2$hh_hv109c_01)
  hh_i01 <- xml$I_foodsecurity$hh_i01
  hh_i02_1 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_1
  hh_i02_2 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_2
  hh_i02_3 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_3
  hh_i02_4 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_4
  hh_i02_5 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_5
  hh_i02_6 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_6
  hh_i02_7 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_7
  hh_i02_8 <- xml$I_foodsecurity$foodsecurity_yes$hh_i02_8
  hh_i031 <- xml$I_foodsecurity$num_meals$hh_i031
  hh_i032 <- xml$I_foodsecurity$num_meals$hh_i032
  hh_i04_1 <- xml$I_foodsecurity$hh_i04$hh_i04_1 %>%
    ct('fdsec4')
  hh_i04_1_other <- xml$I_foodsecurity$hh_i04$hh_i04_1_other
  hh_i04_2 <- xml$I_foodsecurity$hh_i04$hh_i04_2 %>%
    ct('fdsec4')
  hh_i04_2_other <- xml$I_foodsecurity$hh_i04$hh_i04_2_other
  hh_i04_3 <- xml$I_foodsecurity$hh_i04$hh_i04_3 %>%
    ct('fdsec4')
  hh_i04_3_other <- xml$I_foodsecurity$hh_i04$hh_i04_3_other
  hh_i05 <- xml$I_foodsecurity$hh_i05 %>%
    ct('fdsec4')
  hh_i05_2 <- xml$I_foodsecurity$hh_i05_2 %>%
    ct('fdsec4')
  hh_i05_2_other <- xml$I_foodsecurity$hh_i05_2_other
  hh_i05_other <- xml$I_foodsecurity$hh_i05_other
  hh_i06 <- xml$I_foodsecurity$hh_i06
  hh_i07_1 <- xml$I_foodsecurity$fd_diverse$hh_i07_1
  hh_i07_2 <- xml$I_foodsecurity$fd_diverse$hh_i07_2
  hh_i07_3 <- xml$I_foodsecurity$fd_diverse$hh_i07_3
  hh_i08 <- xml$I_foodsecurity$hh_i08
  hh_i09b <- xml$I_foodsecurity$fdsec2$hh_i09_2012
  hh_i09b_1 <- (grepl('1', hh_i09b) & !grepl('10|11|12', hh_i09b)) | grepl('1 ', hh_i09b)
  hh_i09b_2 <- (grepl('2', hh_i09b) & !grepl('12', hh_i09b)) | grepl('2 | 2', hh_i09b)
  hh_i09b_3 <- grepl('3', hh_i09b)
  hh_i09b_4 <- grepl('4', hh_i09b)
  hh_i09b_5 <- grepl('5', hh_i09b)
  hh_i09b_6 <- grepl('6', hh_i09b)
  hh_i09b_7 <- grepl('7', hh_i09b)
  hh_i09b_8 <- grepl('8', hh_i09b)
  hh_i09b_9 <- grepl('9', hh_i09b)
  hh_i09b_10 <- grepl('10', hh_i09b)
  hh_i09b_11 <- grepl('11', hh_i09b)
  hh_i09b_12 <- grepl('12', hh_i09b)
  hh_i09a <- xml$I_foodsecurity$fdsec2$hh_i09_2013
  hh_i09a_1 <- (grepl('1', hh_i09a) & !grepl('10|11|12', hh_i09a)) | grepl('1 ', hh_i09a)
  hh_i09a_2 <- (grepl('2', hh_i09a) & !grepl('12', hh_i09a)) | grepl('2 | 2', hh_i09a)
  hh_i09a_3 <- grepl('3', hh_i09a)
  hh_i09a_4 <- grepl('4', hh_i09a)
  hh_i09a_5 <- grepl('5', hh_i09a)
  hh_i09a_6 <- grepl('6', hh_i09a)
  hh_i09a_7 <- grepl('7', hh_i09a)
  hh_i09a_8 <- grepl('8', hh_i09a)
  hh_i09a_9 <- grepl('9', hh_i09a)
  hh_i09a_10 <- grepl('10', hh_i09a)
  hh_i09a_11 <- grepl('11', hh_i09a)
  hh_i09a_12 <- grepl('12', hh_i09a)
  hh_i10_1 <- xml$I_foodsecurity$fdsec_cause$hh_i10_1 %>%
    ct('fdsec10')
  hh_i10_1oth <- xml$I_foodsecurity$fdsec_cause_oth$hh_i10_1oth
  hh_i10_2 <- xml$I_foodsecurity$fdsec_cause$hh_i10_2 %>%
    ct('fdsec10')
  hh_i10_2oth <- xml$I_foodsecurity$fdsec_cause_oth$hh_i10_2oth
  hh_i10_3 <- xml$I_foodsecurity$fdsec_cause$hh_i10_3 %>%
    ct('fdsec10')
  hh_i10_3oth <- xml$I_foodsecurity$fdsec_cause_oth$hh_i10_3oth
  hh_j01 <- xml$J_Housing$hh_j01 %>%
    ct('j1')
  hh_j02 <- xml$J_Housing$hh_j02 %>%
    ct('j2')
  hh_j02oth <- xml$J_Housing$hh_j02oth
  hh_j03 <- xml$J_Housing$hh_j03
  hh_j04_1 <- xml$J_Housing$rooms$hh_j04_1
  hh_j04_2 <- xml$J_Housing$rooms$hh_j04_2
  hh_j05 <- xml$J_Housing$hh_j05 %>%
    ct('j5')
  hh_j05_other <- xml$J_Housing$hh_j05_other
  hh_j06 <- xml$J_Housing$hh_j06 %>%
    ct('j6')
  hh_j06_other <- xml$J_Housing$hh_j06_other
  hh_j07 <- xml$J_Housing$hh_j07 %>%
    ct('j7')
  hh_j07_other <- xml$J_Housing$hh_j07_other
  hh_j08 <- xml$J2_Water$hh_j08 %>%
    ct('j8')
  hh_j08_other <- xml$J2_Water$hh_j08_other
  hh_j09 <- xml$J_Housing$hh_j09 %>%
    ct('j9')
  hh_j09_other <- xml$J_Housing$hh_j09_other
  hh_j10 <- xml$J_Housing$hh_j10
  hh_j11 <- xml$J2_Water$hh_j11 %>%
    ct('j11')
  hh_j11_other <- xml$J2_Water$hh_j11_other
  hh_j12 <- xml$J_Housing$hh_j12 %>%
    ct('j12')
  hh_j12_other <- xml$J_Housing$hh_j12_other
  hh_j13 <- xml$J_Housing$hh_j13 %>%
    ct('j13')
  hh_j13_other <- xml$J_Housing$hh_j13_other
  hh_j14 <- xml$J2_Water$hh_j14 %>%
    ct('jwater')
  hh_j14_oth <- xml$J2_Water$hh_j14_oth
  hh_j15 <- xml$J2_Water$hh_j15 %>%
    ct('jwater')
  hh_j15_oth <- xml$J2_Water$hh_j15_oth
  hh_j16_1 <- xml$J2_Water$hh_j16$hh_j16_1 %>%
    ct('j16')
  hh_j16_1oth <- xml$J2_Water$hh_j16_oth$hh_j16_1oth
  hh_j16_2 <- xml$J2_Water$hh_j16$hh_j16_2 %>%
    ct('j16')
  hh_j16_2oth <- xml$J2_Water$hh_j16_oth$hh_j16_2oth
  hh_j17_1 <- xml$J2_Water$j17$hh_j17_1 %>%
    ct('jwater')
  hh_j17_1oth <- xml$J2_Water$j17_oth$hh_j17_1oth
  hh_j17_2 <- xml$J2_Water$j17$hh_j17_1 %>%
    ct('jwater')
  hh_j17_2oth <- xml$J2_Water$j17_oth$hh_j17_2oth
  hh_j18_1 <- xml$J2_Water$j18$hh_j18_1 %>%
    ct('jwater')
  hh_j18_1oth <- xml$J2_Water$j18_oth$hh_j18_1oth
  hh_j18_2 <- xml$J2_Water$j18$hh_j18_1 %>%
    ct('jwater')
  hh_j18_2oth <- xml$J2_Water$j18_oth$hh_j18_2oth
  hh_j19_1 <- xml$J2_Water$j19$hh_j19_1 %>%
    ct('jwater')
  hh_j19_1oth <- xml$J2_Water$j19_oth$hh_j19_1oth
  hh_j19_2 <- xml$J2_Water$j19$hh_j19_1 %>%
    ct('jwater')
  hh_j19_2oth <- xml$J2_Water$j19_oth$hh_j19_2oth
  hh_j20 <- xml$J2_Water$hh_j20 %>%
    ct('j20')
  hh_j20b_02 <- xml$J2_Water$j20b_02
  hh_j20c_02 <- xml$J2_Water$j20c_02
  hh_j20c_02_1 <- (grepl('1', hh_j20c_02) & !grepl('10|11|12', hh_j20c_02)) | grepl('1 ', hh_j20c_02)
  hh_j20c_02_10 <- grepl('10', hh_j20c_02)
  hh_j20c_02_11 <- grepl('11', hh_j20c_02)
  hh_j20c_02_12 <- grepl('12', hh_j20c_02)
  hh_j20c_02_2 <- (grepl('2', hh_j20c_02) & !grepl('12', hh_j20c_02)) | grepl('2 | 2', hh_j20c_02)
  hh_j20c_02_3 <- grepl('3', hh_j20c_02)
  hh_j20c_02_4 <- grepl('4', hh_j20c_02)
  hh_j20c_02_5 <- grepl('5', hh_j20c_02)
  hh_j20c_02_6 <- grepl('6', hh_j20c_02)
  hh_j20c_02_7 <- grepl('7', hh_j20c_02)
  hh_j20c_02_8 <- grepl('8', hh_j20c_02)
  hh_j20c_02_9 <- grepl('9', hh_j20c_02)
  hh_k2_10_a <- xml$K2_Food$shared$k2_a$k2_10_a
  hh_k2_10_b <- xml$K2_Food$shared$k2_a$k2_10_b
  hh_k2_10_c <- xml$K2_Food$shared$k2_a$k2_10_c
  hh_k2_10_d <- xml$K2_Food$shared$k2_a$k2_10_d
  hh_k2_11_a <- xml$K2_Food$shared$k2_a$k2_11_a
  hh_k2_11_b <- xml$K2_Food$shared$k2_a$k2_11_b
  hh_k2_11_c <- xml$K2_Food$shared$k2_a$k2_11_c
  hh_k2_11_d <- xml$K2_Food$shared$k2_a$k2_11_d
  hh_k2_8_a <- xml$K2_Food$k2_10_a_grp$k2_8_a
  hh_k2_8_b <- xml$K2_Food$k2_b$k2_8_b
  hh_k2_8_c <- xml$K2_Food$k2_c$k2_8_c
  hh_k2_8_d <- xml$K2_Food$k2_d$k2_8_d
  hh_k2_8_e <- xml$K2_Food$k2_e$k2_8_e
  hh_k2_8_f <- xml$K2_Food$k2_f$k2_8_f
  hh_k2_8_g <- xml$K2_Food$k2_g$k2_8_g
  hh_k2_8_h <- xml$K2_Food$k2_h$k2_8_h
  hh_k2_8_i <- xml$K2_Food$k2_i$k2_8_i
  hh_k2_8_j <- xml$K2_Food$k2_j$k2_8_j
  hh_k2_9 <- xml$K2_Food$k2_9_grp$k2_9
  
  household <- vs.data.frame(uuid, survey_uuid, country, region, district, ward, town, landscape_no, hh_refno, round, 
                             hh_interview_date, hh_interview_time, hh_time_finish, hh_questionnaire_inspection_date, 
                             hh_clerk_first_name, hh_clerk_last_name, hh_data_entry_date, hh_data_entry_date_2, 
                             hh_supervisor_first_name, hh_supervisor_last_name, hh_observations, hh_survey_instrument, 
                             hh_hv105b_01, hh_hv105c_01, hh_hv106_01, hh_hv109_01, hh_hv109a_01, hh_hv109b_01, hh_hv109c_01, 
                             hh_hv109c_01_1, hh_hv109c_01_10, hh_hv109c_01_11, hh_hv109c_01_12, hh_hv109c_01_2, hh_hv109c_01_3, 
                             hh_hv109c_01_4, hh_hv109c_01_5, hh_hv109c_01_6, hh_hv109c_01_7, hh_hv109c_01_8, hh_hv109c_01_9, 
                             hh_i01, hh_i02_1, hh_i02_2, hh_i02_3, hh_i02_4, hh_i02_5, hh_i02_6, hh_i02_7, hh_i02_8, hh_i031, 
                             hh_i032, hh_i04_1, hh_i04_1_other, hh_i04_2, hh_i04_2_other, hh_i04_3, hh_i04_3_other, hh_i05, 
                             hh_i05_2, hh_i05_2_other, hh_i05_other, hh_i06, hh_i07_1, hh_i07_2, hh_i07_3, hh_i08, hh_i09b, 
                             hh_i09b_1, hh_i09b_2, hh_i09b_3, hh_i09b_4, hh_i09b_5, hh_i09b_6, hh_i09b_7, hh_i09b_8, hh_i09b_9, 
                             hh_i09b_10, hh_i09b_11, hh_i09b_12, hh_i09a, hh_i09a_1, hh_i09a_2, hh_i09a_3, hh_i09a_4, hh_i09a_5, 
                             hh_i09a_6, hh_i09a_7, hh_i09a_8, hh_i09a_9, hh_i09a_10, hh_i09a_11, hh_i09a_12, hh_i10_1, 
                             hh_i10_1oth, hh_i10_2, hh_i10_2oth, hh_i10_3, hh_i10_3oth, hh_j01, hh_j02, hh_j02oth, hh_j03, 
                             hh_j04_1, hh_j04_2, hh_j05, hh_j05_other, hh_j06, hh_j06_other, hh_j07, hh_j07_other, hh_j08, 
                             hh_j08_other, hh_j09, hh_j09_other, hh_j10, hh_j11, hh_j11_other, hh_j12, hh_j12_other, hh_j13, 
                             hh_j13_other, hh_j14, hh_j14_oth, hh_j15, hh_j15_oth, hh_j16_1, hh_j16_1oth, hh_j16_2, hh_j16_2oth, 
                             hh_j17_1, hh_j17_1oth, hh_j17_2, hh_j17_2oth, hh_j18_1, hh_j18_1oth, hh_j18_2, hh_j18_2oth, 
                             hh_j19_1, hh_j19_1oth, hh_j19_2, hh_j19_2oth, hh_j20, hh_j20b_02, hh_j20c_02, hh_j20c_02_1, hh_j20c_02_10, 
                             hh_j20c_02_11, hh_j20c_02_12, hh_j20c_02_2, hh_j20c_02_3, hh_j20c_02_4, hh_j20c_02_5, hh_j20c_02_6, 
                             hh_j20c_02_7, hh_j20c_02_8, hh_j20c_02_9, hh_k2_10_a, hh_k2_10_b, hh_k2_10_c, hh_k2_10_d, hh_k2_11_a, 
                             hh_k2_11_b, hh_k2_11_c, hh_k2_11_d, hh_k2_8_a, hh_k2_8_b, hh_k2_8_c, hh_k2_8_d, hh_k2_8_e, hh_k2_8_f, 
                             hh_k2_8_g, hh_k2_8_h, hh_k2_8_i, hh_k2_8_j, hh_k2_9)
  
  ###################
  #piiname_household
  ###################
  
  uuid <- uuid
  hh_head_name <- paste(xml$metadata$hh_first_name, xml$metadata$hh_lastname)
  
  piiname_household <- vs.data.frame(uuid, hh_head_name)
  
  ###################
  #piigeo_household
  ###################
  
  uuid <- uuid
  eplot_no <- xml$metadata$eplot_no
  hh_no <- xml$metadata$hh_no
  
  piigeo_household <- vs.data.frame(uuid, eplot_no, hh_no)
  
  ########################
  #household_expenditure
  ########################
  
  l_101_1 <- xml$L_nonfood$L_week$L_week_1$L_101_1
  l_101_2 <- xml$L_nonfood$L_week$L_week_2$L_101_2
  l_102_1 <- xml$L_nonfood$L_week$L_week_1$L_102_1
  l_102_2 <- xml$L_nonfood$L_week$L_week_2$L_102_2
  l_103_1 <- xml$L_nonfood$L_week$L_week_1$L_103_1
  l_103_2 <- xml$L_nonfood$L_week$L_week_2$L_103_2
  l_199_1 <- xml$L_nonfood$L_week$L_week_1$L_199_1
  l_199_2 <- xml$L_nonfood$L_week$L_week_2$L_199_2
  l_201_1 <- xml$L_nonfood$L_month$L_month_1$L_201_1
  l_201_2 <- xml$L_nonfood$L_month$L_month_2$L_201_2
  l_202_1 <- xml$L_nonfood$L_month$L_month_1$L_202_1
  l_202_2 <- xml$L_nonfood$L_month$L_month_2$L_202_2
  l_203_1 <- xml$L_nonfood$L_month$L_month_1$L_203_1
  l_203_2 <- xml$L_nonfood$L_month$L_month_2$L_203_2
  l_203a_1 <- xml$L_nonfood$L_month$L_month_1$L_203a_1
  l_203a_2 <- xml$L_nonfood$L_month$L_month_2$L_203a_2
  l_203b_1 <- xml$L_nonfood$L_month$L_month_1$L_203b_1
  l_203b_2 <- xml$L_nonfood$L_month$L_month_2$L_203b_2
  l_204_1 <- xml$L_nonfood$L_week$L_week_1$L_204_1
  l_204_2 <- xml$L_nonfood$L_week$L_week_2$L_204_2
  l_205_1 <- xml$L_nonfood$L_month$L_month_1$L_205_1
  l_205_2 <- xml$L_nonfood$L_month$L_month_2$L_205_2
  l_206_1 <- xml$L_nonfood$L_week$L_week_1$L_206_1
  l_206_2 <- xml$L_nonfood$L_week$L_week_2$L_206_2
  l_207_1 <- xml$L_nonfood$L_week$L_week_1$L_207_1
  l_207_2 <- xml$L_nonfood$L_week$L_week_2$L_207_2
  l_207a_1 <- xml$L_nonfood$L_week$L_week_1$L_207a_1
  l_207a_2 <- xml$L_nonfood$L_week$L_week_2$L_207a_2
  l_208_1 <- xml$L_nonfood$L_month$L_month_1$L_208_1
  l_208_2 <- xml$L_nonfood$L_month$L_month_2$L_208_2
  l_209_1 <- xml$L_nonfood$L_month$L_month_1$L_209_1
  l_209_2 <- xml$L_nonfood$L_month$L_month_2$L_209_2
  l_211_1 <- xml$L_nonfood$L_month$L_month_1$L_211_1
  l_211_2 <- xml$L_nonfood$L_month$L_month_2$L_211_2
  l_212_1 <- xml$L_nonfood$L_month$L_month_1$L_212_1
  l_212_2 <- xml$L_nonfood$L_month$L_month_2$L_212_2
  l_213_1 <- xml$L_nonfood$L_month$L_month_1$L_213_1
  l_213_2 <- xml$L_nonfood$L_month$L_month_2$L_213_2
  l_214_1 <- xml$L_nonfood$L_month$L_month_1$L_214_1
  l_214_2 <- xml$L_nonfood$L_month$L_month_2$L_214_2
  l_215_1 <- xml$L_nonfood$L_month$L_month_1$L_215_1
  l_215_2 <- xml$L_nonfood$L_month$L_month_2$L_215_2
  l_216_1 <- xml$L_nonfood$L_month$L_month_1$L_216_1
  l_216_2 <- xml$L_nonfood$L_month$L_month_2$L_216_2
  l_217_1 <- xml$L_nonfood$L_month$L_month_1$L_217_1
  l_217_2 <- xml$L_nonfood$L_month$L_month_2$L_217_2
  l_217a_1 <- xml$L_nonfood$L_month$L_month_1$L_217a_1
  l_217a_2 <- xml$L_nonfood$L_month$L_month_2$L_217a_2
  l_218_1 <- xml$L_nonfood$L_month$L_month_1$L_218_1
  l_218_2 <- xml$L_nonfood$L_month$L_month_2$L_218_2
  l_219_1 <- xml$L_nonfood$L_month$L_month_1$L_219_1
  l_219_2 <- xml$L_nonfood$L_month$L_month_2$L_219_2
  l_220_1 <- xml$L_nonfood$L_month$L_month_1$L_220_1
  l_220_2 <- xml$L_nonfood$L_month$L_month_2$L_220_2
  l_221_1 <- xml$L_nonfood$L_month$L_month_1$L_221_1
  l_221_2 <- xml$L_nonfood$L_month$L_month_2$L_221_2
  l_222_1 <- xml$L_nonfood$L_month$L_month_1$L_222_1
  l_222_2 <- xml$L_nonfood$L_month$L_month_2$L_222_2
  l_223_1 <- xml$L_nonfood$L_month$L_month_1$L_223_1
  l_223_2 <- xml$L_nonfood$L_month$L_month_2$L_223_2
  l_224_1 <- xml$L_nonfood$L_month$L_month_1$L_224_1
  l_224_2 <- xml$L_nonfood$L_month$L_month_2$L_224_2
  l_224a_1 <- xml$L_nonfood$L_month$L_month_1$L_224a_1
  l_224a_2 <- xml$L_nonfood$L_month$L_month_2$L_224a_2
  l_224b_1 <- xml$L_nonfood$L_month$L_month_1$L_224b_1
  l_224b_2 <- xml$L_nonfood$L_month$L_month_2$L_224b_2
  l_301_1 <- xml$L_nonfood$L_year$L_year_1$L_301_1
  l_301_2 <- xml$L_nonfood$L_year$L_year_2$L_301_2
  l_302_1 <- xml$L_nonfood$L_year$L_year_1$L_302_1
  l_302_2 <- xml$L_nonfood$L_year$L_year_2$L_302_2
  l_303_1 <- xml$L_nonfood$L_year$L_year_1$L_303_1
  l_303_2 <- xml$L_nonfood$L_year$L_year_2$L_303_2
  l_304_1 <- xml$L_nonfood$L_year$L_year_1$L_304_1
  l_304_2 <- xml$L_nonfood$L_year$L_year_2$L_304_2
  l_305_1 <- xml$L_nonfood$L_year$L_year_1$L_305_1
  l_305_2 <- xml$L_nonfood$L_year$L_year_2$L_305_2
  l_306_1 <- xml$L_nonfood$L_year$L_year_1$L_306_1
  l_306_2 <- xml$L_nonfood$L_year$L_year_2$L_306_2
  l_307_1 <- xml$L_nonfood$L_year$L_year_1$L_307_1
  l_307_2 <- xml$L_nonfood$L_year$L_year_2$L_307_2
  l_308_1 <- xml$L_nonfood$L_year$L_year_1$L_308_1
  l_308_2 <- xml$L_nonfood$L_year$L_year_2$L_308_2
  l_309_1 <- xml$L_nonfood$L_year$L_year_1$L_309_1
  l_309_2 <- xml$L_nonfood$L_year$L_year_2$L_309_2
  l_310_1 <- xml$L_nonfood$L_year$L_year_1$L_310_1
  l_310_2 <- xml$L_nonfood$L_year$L_year_2$L_310_2
  l_311_1 <- xml$L_nonfood$L_year$L_year_1$L_311_1
  l_311_2 <- xml$L_nonfood$L_year$L_year_2$L_311_2
  l_312_1 <- xml$L_nonfood$L_year$L_year_1$L_312_1
  l_312_2 <- xml$L_nonfood$L_year$L_year_2$L_312_2
  l_313_1 <- xml$L_nonfood$L_year$L_year_1$L_313_1
  l_313_2 <- xml$L_nonfood$L_year$L_year_2$L_313_2
  l_314_1 <- xml$L_nonfood$L_year$L_year_1$L_314_1
  l_314_2 <- xml$L_nonfood$L_year$L_year_2$L_314_2
  l_315_1 <- xml$L_nonfood$L_year$L_year_1$L_315_1
  l_315_2 <- xml$L_nonfood$L_year$L_year_2$L_315_2
  l_316_1 <- xml$L_nonfood$L_year$L_year_1$L_316_1
  l_316_2 <- xml$L_nonfood$L_year$L_year_2$L_316_2
  l_317_1 <- xml$L_nonfood$L_year$L_year_1$L_317_1
  l_317_2 <- xml$L_nonfood$L_year$L_year_2$L_317_2
  l_318_1 <- xml$L_nonfood$L_year$L_year_g$L_318_1
  l_318_2 <- xml$L_nonfood$L_year$L_year_g2$L_318_2
  l_319_1 <- xml$L_nonfood$L_year$L_year_g$L_319_1
  l_319_2 <- xml$L_nonfood$L_year$L_year_g2$L_319_2
  l_320_1 <- xml$L_nonfood$L_year$L_year_1$L_320_1
  l_320_2 <- xml$L_nonfood$L_year$L_year_2$L_320_2
  l_320a_1 <- xml$L_nonfood$L_year$L_year_1$L_320a_1
  l_320a_2 <- xml$L_nonfood$L_year$L_year_2$L_320a_2
  l_320b_1 <- xml$L_nonfood$L_year$L_year_1$L_320b_1
  l_320b_2 <- xml$L_nonfood$L_year$L_year_2$L_320b_2
  
  tempdf <- vs.data.frame(l_101_1, l_101_2, l_102_1, l_102_2, l_103_1, l_103_2, 
                          l_199_1, l_199_2, l_201_1, l_201_2, l_202_1, l_202_2, l_203_1, l_203_2, l_203a_1, 
                          l_203a_2, l_203b_1, l_203b_2, l_204_1, l_204_2, l_205_1, l_205_2, l_206_1, l_206_2, 
                          l_207_1, l_207_2, l_207a_1, l_207a_2, l_208_1, l_208_2, l_209_1, l_209_2, l_211_1, 
                          l_211_2, l_212_1, l_212_2, l_213_1, l_213_2, l_214_1, l_214_2, l_215_1, l_215_2, l_216_1, 
                          l_216_2, l_217_1, l_217_2, l_217a_1, l_217a_2, l_218_1, l_218_2, l_219_1, l_219_2, l_220_1, 
                          l_220_2, l_221_1, l_221_2, l_222_1, l_222_2, l_223_1, l_223_2, l_224_1, l_224_2, l_224a_1, 
                          l_224a_2, l_224b_1, l_224b_2, l_301_1, l_301_2, l_302_1, l_302_2, l_303_1, l_303_2, l_304_1, 
                          l_304_2, l_305_1, l_305_2, l_306_1, l_306_2, l_307_1, l_307_2, l_308_1, l_308_2, l_309_1, 
                          l_309_2, l_310_1, l_310_2, l_311_1, l_311_2, l_312_1, l_312_2, l_313_1, l_313_2, l_314_1, 
                          l_314_2, l_315_1, l_315_2, l_316_1, l_316_2, l_317_1, l_317_2, l_318_1, l_318_2, l_319_1, 
                          l_319_2, l_320_1, l_320_2, l_320a_1, l_320a_2, l_320b_1, l_320b_2)
  
  tempdf <- tempdf %>% gather(itemcode, value)
  
  tempdf$var[grepl('_1$', tempdf$itemcode)] <- 'hh_purchased'
  tempdf$var[grepl('_2$', tempdf$itemcode)] <- 'hh_paid'
  tempdf$itemcode <- gsub('_.$', '', tempdf$itemcode)
  
  lmap <- read.csv('hh/lmap.csv')
  
  household_expenditure <- tempdf %>% spread(var, value) %>%
    merge(lmap, all.x=T, all.y=F)
  
  household_expenditure$parent_uuid <- survey_uuid
  household_expenditure$survey_uuid <- survey_uuid
  household_expenditure$uuid <- paste0(survey_uuid, '/', seq(1, nrow(household_expenditure)))
  
  ####################################
  #household_food
  ####################################
  
  household_food <- data.frame()
  count <- 1
  for(i in xml$K_Food[names(xml$K_Food)=='k_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    hh_k_02_1 <- i$k_02$k_02_1 %>%
      ct('k2')
    hh_k_02_2 <- i$k_02$k_02_2
    hh_k_03_1 <- i$k_03$k_03_1 %>%
      ct('k2')
    hh_k_03_2 <- i$k_03$k_03_2
    hh_k_04 <- i$k_04
    hh_k_05_1 <- i$k_05$k_05_1 %>%
      ct('k2')
    hh_k_05_2 <- i$k_05$k_05_2
    hh_k_05a <- i$k_05a
    hh_k_06_1 <- i$k_06$k_06_1 %>%
      ct('k2')
    hh_k_06_2 <- i$k_06$k_06_2
    hh_k_item <- i$k_item
    hh_k_item_other <- NULL
    if (i$k_item_code == '0112'){
      hh_k_item_other <- xml$K_Food$k1_0112_oth
    }
    if (i$k_item_code == '0207'){
      hh_k_item_other <- xml$K_Food$k1_0207_oth
    }
    if (i$k_item_code == '1004'){
      hh_k_item_other <- xml$K_Food$k1_1004_oth
    }
    if (i$k_item_code == '1103'){
      hh_k_item_other <- xml$K_Food$k1_1103_oth
    }
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, hh_k_02_1, hh_k_02_2, 
                            hh_k_03_1, hh_k_03_2, hh_k_04, hh_k_05_1, hh_k_05_2, 
                            hh_k_05a, hh_k_06_1, hh_k_06_2, hh_k_item, hh_k_item_other)
    
    household_food <- bind_rows(household_food, tempdf)
    
    count <- count + 1
  }
  
  ######################################
  #household_individual 
  #&
  #piiname_household_individual
  #####################################
  
  piiname_household_individual <- data.frame()
  household_individual_a <- data.frame()
  count <- 1
  for (i in xml$b_roster[names(xml$b_roster)=='b_roster_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    ind_refno <- paste0('I', substr(100 + count, 2, 3))
    hh_b02 <- i$demo$hh_b02 %>%
      ct('mf')
    hh_indid <- i$indid
    hh_b05 <- i$demo$hh_b05 %>%
      ct('relhh')
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, ind_refno, hh_b02, hh_b05)
    household_individual_a <- bind_rows(household_individual_a, tempdf)
    
    tempdf <- vs.data.frame(uuid, hh_indid)
    piiname_household_individual <- bind_rows(piiname_household_individual, tempdf)
    
    count <- count + 1
  }
  
  household_individual_b <- data.frame()
  count <- 1
  for (i in xml[names(xml)=='b_roster_repeat2']){
    uuid <- paste0(survey_uuid, '/', count)
    hh_b03 <- i$hh_b03
    hh_b04 <- i$hh_b04
    hh_b07 <- i$h_b_78$hh_b07
    hh_b08 <- i$h_b_78$hh_b08
    
    tempdf <- vs.data.frame(uuid, hh_b03, hh_b04, hh_b07, hh_b08)
    
    household_individual_b <- bind_rows(household_individual_b, tempdf)
    
    count <- count + 1
  }
  
  household_individual_c <- data.frame()
  count <- 1
  for (i in xml[names(xml)=='C_E_HV1_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    hh_c01 <- i$C_group$C01$hh_c01
    hh_c02 <- i$C_group$C01$hh_c02 %>%
      ct('language')
    hh_c03 <- i$C_group$C01$hh_c03
    hh_c07 <- i$C_group$hh_c07 %>%
      ct('grade')
    hh_e04 <- i$E01$hh_e04
    hh_e06 <- i$E01$hh_e06 %>%
      ct('work')
    hh_e08 <- i$E01$hh_e08
    hh_e22_1 <- i$E01$hh_e08_group$hh_e22$hh_e22_1
    hh_e22_2 <- i$E01$hh_e08_group$hh_e22$hh_e22_2 %>%
      ct('time')
    hh_e23 <- i$E01$hh_e08_group$hh_e22$hh_e23
    hh_e24_1 <- i$E01$hh_e08_group$hh_e24$hh_e24_1
    hh_e24_2 <- i$E01$hh_e08_group$hh_e24$hh_e24_2 %>%
      ct('time')
    hh_e25 <- i$E01$hh_etime$hh_e25
    hh_e26 <- i$E01$hh_etime$hh_e26
    hh_e27 <- i$E01$hh_etime$hh_e27
    hh_e28 <- i$E01$hh_etime$hh_e28
    hh_e51 <- i$E01$hh_e51
    hh_e52 <- i$E01$hh_e52
    hh_e64_1 <- i$E01$hh_e_self01$h_einc$hh_e64_1
    hh_e64_2 <- i$E01$hh_e_self01$h_einc$hh_e64_2 %>%
      ct('weekmonth')
    hh_e65_1 <- i$E01$hh_e_self01$h_eprof$hh_e65_1
    hh_e65_2 <- i$E01$hh_e_self01$h_eprof$hh_e65_2 %>%
      ct('weekmonth')
    hh_hv103 <- i$HV1$hh_hv103
    hh_hv104 <- i$HV1$hh_hv103_y$hh_hv104 %>%
      ct('collect_firewood')
    hh_hv105 <- i$HV1$hh_hv103_y$hh_hv105
    hh_hv105_other <- i$HV1$hh_hv103_y$hh_hv105_other
    hh_hv105_unit <- i$HV1$hh_hv103_y$hh_hv105_unit
    hh_hv105a <- i$HV1$hh_hv103_y$hh_hv105a
    
    tempdf <- vs.data.frame(uuid, hh_c01, hh_c02, hh_c03, hh_c07, hh_e04, hh_e06, hh_e08, 
                            hh_e22_1, hh_e22_2, hh_e23, hh_e24_1, hh_e24_2, hh_e25, hh_e26, 
                            hh_e27, hh_e28, hh_e51, hh_e52, hh_e64_1, hh_e64_2, hh_e65_1, 
                            hh_e65_2, hh_hv103, hh_hv104, hh_hv105, hh_hv105_other, hh_hv105_unit, 
                            hh_hv105a)
    household_individual_c <- bind_rows(household_individual_c, tempdf)
    count <- count + 1
  }
  
  household_individual_d <- data.frame()
  count <- 1
  for (i in xml$u_anthro[names(xml$u_anthro)=='u_anthro_repeat']){
    uuid <- paste0(survey_uuid, '/', count)
    hh_u1 <- i$u1_01
    hh_u2 <- i$u1_group_01$u2_01
    hh_u3 <- i$u1_group_01$u3_01
    hh_u4 <- i$u1_group_01$u4_01 %>%
      ct('u5')
    hh_u5 <- i$u1_group_01$u5_01
    hh_u6 <- i$u1_group_01$u6_01
    hh_u7 <- i$u1_group_01$u7_01 %>%
      ct('u2')
    hh_u7_other <- i$u1_group_01$u7_01_other
  
    tempdf <- vs.data.frame(uuid, hh_u1, hh_u2, hh_u3, hh_u4, hh_u5, hh_u6, hh_u7, hh_u7_other)
    household_individual_d <- bind_rows(household_individual_d, tempdf)
    count <- count + 1
  }
  
  household_individual <- Reduce(merge, list(household_individual_a, household_individual_b, 
                                             household_individual_c, household_individual_d))
  
  #######################
  #household_possession
  #######################
  
  household_possession <- data.frame(itemcode = tolower(names(xml$n_assets)[names(xml$n_assets)!='n_note']), count=unlist(xml$n_assets))
  
  household_possession$parent_uuid <- survey_uuid
  household_possession$survey_uuid <- survey_uuid
  household_possession$uuid <- paste0(survey_uuid, '/', 1:nrow(household_possession))
  
  n <- read.csv('hh/nmap.csv')
  
  household_possession <- merge(household_possession, n)
  
  ######################
  #household_resource
  ######################
  
  hv2_10_09_oth <- xml$HV2$hv2_10_09_oth
  
  hv2_10_01 <- xml$HV2$hv2_10$hv2_10_01
  hv2_10_02 <- xml$HV2$hv2_10$hv2_10_02
  hv2_10_03 <- xml$HV2$hv2_10$hv2_10_03
  hv2_10_04 <- xml$HV2$hv2_10$hv2_10_04
  hv2_10_05 <- xml$HV2$hv2_10$hv2_10_05
  hv2_10_06 <- xml$HV2$hv2_10$hv2_10_06
  hv2_10_07 <- xml$HV2$hv2_10$hv2_10_07
  hv2_10_08 <- xml$HV2$hv2_10$hv2_10_08
  hv2_10_09 <- xml$HV2$hv2_10$hv2_10_09
  hv2_11_01 <- xml$HV2$hv2_wm_2$hv2_11_01
  hv2_11_02 <- xml$HV2$hv2_wi_2$hv2_11_02
  hv2_11_03 <- xml$HV2$hv2_fish_2$hv2_11_03
  hv2_11_04 <- xml$HV2$hv2_nuts_2$hv2_11_04
  hv2_11_05 <- xml$HV2$hv2_build_2$hv2_11_05
  hv2_11_06 <- xml$HV2$hv2_mplants_2$hv2_11_06
  hv2_11_07 <- xml$HV2$hv2_isc_2$hv2_11_07
  hv2_11_08 <- xml$HV2$hv2_honey_2$hv2_11_08
  hv2_11_09 <- xml$HV2$hv2_other_2$hv2_11_09
  hv2_12_01 <- xml$HV2$hv2_wm_2$hv2_12_01
  hv2_12_02 <- xml$HV2$hv2_wi_2$hv2_12_02
  hv2_12_03 <- xml$HV2$hv2_fish_2$hv2_12_03
  hv2_12_04 <- xml$HV2$hv2_nuts_2$hv2_12_04
  hv2_12_05 <- xml$HV2$hv2_build_2$hv2_12_05
  hv2_12_06 <- xml$HV2$hv2_mplants_2$hv2_12_06
  hv2_12_07 <- xml$HV2$hv2_isc_2$hv2_12_07
  hv2_12_08 <- xml$HV2$hv2_honey_2$hv2_12_08
  hv2_12_09 <- xml$HV2$hv2_other_2$hv2_12_09
  hv2_13_01 <- xml$HV2$hv2_wm_2$hv2_13_01
  hv2_13_02 <- xml$HV2$hv2_wi_2$hv2_13_02
  hv2_13_03 <- xml$HV2$hv2_fish_2$hv2_13_03
  hv2_13_04 <- xml$HV2$hv2_nuts_2$hv2_13_04
  hv2_13_05 <- xml$HV2$hv2_build_2$hv2_13_05
  hv2_13_06 <- xml$HV2$hv2_mplants_2$hv2_13_06
  hv2_13_07 <- xml$HV2$hv2_isc_2$hv2_13_07
  hv2_13_08 <- xml$HV2$hv2_honey_2$hv2_13_08
  hv2_13_09 <- xml$HV2$hv2_other_2$hv2_13_09
  hv2_14_01 <- xml$HV2$hv2_wm_2$hv2_14_01
  hv2_14_02 <- xml$HV2$hv2_wi_2$hv2_14_02
  hv2_14_03 <- xml$HV2$hv2_fish_2$hv2_14_03
  hv2_14_04 <- xml$HV2$hv2_nuts_2$hv2_14_04
  hv2_14_05 <- xml$HV2$hv2_build_2$hv2_14_05
  hv2_14_06 <- xml$HV2$hv2_mplants_2$hv2_14_06
  hv2_14_07 <- xml$HV2$hv2_isc_2$hv2_14_07
  hv2_14_08 <- xml$HV2$hv2_honey_2$hv2_14_08
  hv2_14_09 <- xml$HV2$hv2_other_2$hv2_14_09
  hv2_15_01 <- xml$HV2$hv2_wm_2$hv2_15_01
  hv2_15_02 <- xml$HV2$hv2_wi_2$hv2_15_02
  hv2_15_03 <- xml$HV2$hv2_fish_2$hv2_15_03
  hv2_15_04 <- xml$HV2$hv2_nuts_2$hv2_15_04
  hv2_15_05 <- xml$HV2$hv2_build_2$hv2_15_05
  hv2_15_06 <- xml$HV2$hv2_mplants_2$hv2_15_06
  hv2_15_07 <- xml$HV2$hv2_isc_2$hv2_15_07
  hv2_15_08 <- xml$HV2$hv2_honey_2$hv2_15_08
  hv2_15_09 <- xml$HV2$hv2_other_2$hv2_15_09
  
  hh_nr <- vs.data.frame(hv2_10_01, hv2_10_02, hv2_10_03, hv2_10_04, hv2_10_05, hv2_10_06, hv2_10_07, 
                         hv2_10_08, hv2_10_09, hv2_11_01, hv2_11_02, hv2_11_03, hv2_11_04, hv2_11_05, 
                         hv2_11_06, hv2_11_07, hv2_11_08, hv2_11_09, hv2_12_01, hv2_12_02, hv2_12_03, 
                         hv2_12_04, hv2_12_05, hv2_12_06, hv2_12_07, hv2_12_08, hv2_12_09, hv2_13_01, 
                         hv2_13_02, hv2_13_03, hv2_13_04, hv2_13_05, hv2_13_06, hv2_13_07, hv2_13_08, 
                         hv2_13_09, hv2_14_01, hv2_14_02, hv2_14_03, hv2_14_04, hv2_14_05, hv2_14_06, 
                         hv2_14_07, hv2_14_08, hv2_14_09, hv2_15_01, hv2_15_02, hv2_15_03, hv2_15_04, 
                         hv2_15_05, hv2_15_06, hv2_15_07, hv2_15_08, hv2_15_09) %>%
    gather(key, value) %>%
    mutate(variable = paste0('hv2_', substr(key, 8,9)),
           key = paste0('hh_', substr(key, 1, 6))) %>%
    spread(key, value)
  
  nrmap <- read.csv('hh/nrmap.csv')
  
  household_resource <- merge(nrmap, hh_nr)
  
  household_resource$uuid <- paste0(survey_uuid, '/', seq(1, nrow(household_resource)))
  household_resource$survey_uuid <- survey_uuid
  household_resource$parent_uuid <- survey_uuid
  household_resource$variable <- NULL
  
  if(!is.null(hv2_10_09_oth)){
    household_resource$resource[household_resource$resource=='Other'] <- hv2_10_09_oth
  }
  
  ####################################
  #household_water
  ###################################
  
  j22_02 <- xml$J2_Water$j21_02$j22_02 %>% ct('j23')
  j23_02_1 <- xml$J2_Water$j21_02$j23_02$j23_02_1
  j23_02_2 <- xml$J2_Water$j21_02$j23_02$j23_02_2 %>% ct('j24')
  j24_02 <- xml$J2_Water$j21_02$j24_02
  j22_03 <- xml$J2_Water$j21_03$j22_03 %>% ct('j23')
  j23_03_1 <- xml$J2_Water$j21_03$j23_03$j23_03_1
  j23_03_2 <- xml$J2_Water$j21_03$j23_03$j23_03_2 %>% ct('j24')
  j24_03 <- xml$J2_Water$j21_03$j24_03
  j22_04 <- xml$J2_Water$j21_04$j22_04 %>% ct('j23')
  j23_04_1 <- xml$J2_Water$j21_04$j23_04$j23_04_1
  j23_04_2 <- xml$J2_Water$j21_04$j23_04$j23_04_2 %>% ct('j24')
  j24_04 <- xml$J2_Water$j21_04$j24_04
  j22_05 <- xml$J2_Water$j21_05$j22_05 %>% ct('j23')
  j23_05_1 <- xml$J2_Water$j21_05$j23_05$j23_05_1
  j23_05_2 <- xml$J2_Water$j21_05$j23_05$j23_05_2 %>% ct('j24')
  j24_05 <- xml$J2_Water$j21_05$j24_05
  j22_06 <- xml$J2_Water$j21_06$j22_06 %>% ct('j23')
  j23_06_1 <- xml$J2_Water$j21_06$j23_06$j23_06_1
  j23_06_2 <- xml$J2_Water$j21_06$j23_06$j23_06_2 %>% ct('j24')
  j24_06 <- xml$J2_Water$j21_06$j24_06
  j22_07 <- xml$J2_Water$j21_07$j22_07 %>% ct('j23')
  j23_07_1 <- xml$J2_Water$j21_07$j23_07$j23_07_1
  j23_07_2 <- xml$J2_Water$j21_07$j23_07$j23_07_2 %>% ct('j24')
  j24_07 <- xml$J2_Water$j21_07$j24_07
  j22_08 <- xml$J2_Water$j21_08$j22_08 %>% ct('j23')
  j23_08_1 <- xml$J2_Water$j21_08$j23_08$j23_08_1
  j23_08_2 <- xml$J2_Water$j21_08$j23_08$j23_08_2 %>% ct('j24')
  j24_08 <- xml$J2_Water$j21_08$j24_08
  j22_09 <- xml$J2_Water$j21_09$j22_09 %>% ct('j23')
  j23_09_1 <- xml$J2_Water$j21_09$j23_09$j23_09_1
  j23_09_2 <- xml$J2_Water$j21_09$j23_09$j23_09_2 %>% ct('j24')
  j24_09 <- xml$J2_Water$j21_09$j24_09
  j22_10 <- xml$J2_Water$j21_10$j22_10 %>% ct('j23')
  j23_10_1 <- xml$J2_Water$j21_10$j23_10$j23_10_1
  j23_10_2 <- xml$J2_Water$j21_10$j23_10$j23_10_2 %>% ct('j24')
  j24_10 <- xml$J2_Water$j21_10$j24_10
  j22_11 <- xml$J2_Water$j21_11$j22_11 %>% ct('j23')
  j23_11_1 <- xml$J2_Water$j21_11$j23_11$j23_11_1
  j23_11_2 <- xml$J2_Water$j21_11$j23_11$j23_11_2 %>% ct('j24')
  j24_11 <- xml$J2_Water$j21_11$j24_11
  j22_12 <- xml$J2_Water$j21_12$j22_12 %>% ct('j23')
  j23_12_1 <- xml$J2_Water$j21_12$j23_12$j23_12_1
  j23_12_2 <- xml$J2_Water$j21_12$j23_12$j23_12_2 %>% ct('j24')
  j24_12 <- xml$J2_Water$j21_12$j24_12
  j22_13 <- xml$J2_Water$j21_13$j22_13 %>% ct('j23')
  j22_14 <- xml$J2_Water$j21_14$j22_14 %>% ct('j23')
  j23_14_1 <- xml$J2_Water$j21_14$j23_14$j23_14_1
  j23_14_2 <- xml$J2_Water$j21_14$j23_14$j23_14_2 %>% ct('j24')
  j24_14 <- xml$J2_Water$j21_14$j24_14
  
  household_water <- vs.data.frame(j22_02, j23_02_1, j23_02_2, j24_02, j22_03, j23_03_1, j23_03_2, 
                                   j24_03, j22_04, j23_04_1, j23_04_2, j24_04, j22_05, j23_05_1, 
                                   j23_05_2, j24_05, j22_06, j23_06_1, j23_06_2, j24_06, j22_07, 
                                   j23_07_1, j23_07_2, j24_07, j22_08, j23_08_1, j23_08_2, j24_08, 
                                   j22_09, j23_09_1, j23_09_2, j24_09, j22_10, j23_10_1, j23_10_2, 
                                   j24_10, j22_11, j23_11_1, j23_11_2, j24_11, j22_12, j23_12_1, 
                                   j23_12_2, j24_12, j22_13, j22_14, j23_14_1, j23_14_2, j24_14) %>%
    gather(var, value) %>%
    mutate(variable=as.numeric(substr(var, 5, 6))) %>%
    mutate(column=gsub('_..', '', var)) %>%
    merge(read.csv('hh/jmap.csv')) %>%
    mutate(uuid=paste0(survey_uuid, '/', variable), var=NULL, variable=NULL, 
           parent_uuid=survey_uuid, survey_uuid=survey_uuid) %>%
    spread(column, value) %>%
    filter(!is.na(j22))
  
  ########################
  #Write Info
  #########################
  insertDF(dbcon, household, 'household', test)
  insertDF(dbcon, piiname_household, 'piiname_household', test)
  insertDF(dbcon, piigeo_household, 'piigeo_household', test)
  insertDF(dbcon, household_expenditure, 'household_expenditure', test)
  insertDF(dbcon, household_food, 'household_food', test)
  insertDF(dbcon, household_individual, 'household_individual', test)
  insertDF(dbcon, piiname_household_individual, 'piiname_household_individual', test)
  insertDF(dbcon, household_possession, 'household_possession', test)
  insertDF(dbcon, household_resource, 'household_resource', test)
  insertDF(dbcon, household_water, 'household_water', test)

  if (!test){
  dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                                survey_uuid, "',",
                                "'household_hold_15_may_2016_v1','",
                                xml$today,"',current_date);")) 
  }
}
