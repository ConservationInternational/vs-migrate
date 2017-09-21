library(XML)
library(readxl)
library(RPostgreSQL)

options(stringsAsFactors = FALSE)

vs_household_secv_15_may_2016 <- function(dbcon, xml, test=FALSE){
  assign('codedf', read_xls('migrations/VS_Household_SecV_15.05.2016.xls', 
                            sheet = 'choices'),
         envir=.GlobalEnv)
  
  xml <- xmlToList(xml)
  
  #############################
  #householdcontact
  #############################
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  country <- xml$country
  landscape_no <- xml$metadata$landscape_no
  eplot_no <- xml$metadata$eplot_no
  hh_no <- xml$metadata$hh_no
  hh_refno <- getHHref(dbcon, country, landscape_no, eplot_no, hh_no)
  end_date <- xml$end_group$end_date
  round <- getRound(country,  end_date)
  end_time <- xml$end_group$end_time
  gpsse_lat <- makeGps(xml$v1_group$location$gps_ns, xml$v1_group$location$gpsse_lat)
  gpsse_long <- makeGps(xml$v1_group$location$gps_ew, xml$v1_group$location$gpsse_long)
  v1_1 <- xml$v1_group$v1_1
  v1_2_a_name <- xml$v1_group$v1_2_group$v1_2_a_name
  v1_2_a_phone <- xml$v1_group$v1_2_group$v1_2_a_phone
  v1_2_b_name <- xml$v1_group$v1_2_group$v1_2_b_name
  v1_2_b_phone <- xml$v1_group$v1_2_group$v1_2_b_phone
  v1_2_c_name <- xml$v1_group$v1_2_group$v1_2_c_name
  v1_2_c_phone <- xml$v1_group$v1_2_group$v1_2_c_phone
  v1_3_a <- xml$v1_group$v1_3_group$v1_3_a
  v1_3_b <- xml$v1_group$v1_3_group$v1_3_b
  v1_3_c <- xml$v1_group$v1_3_group$v1_3_c
  v1_3_d <- xml$v1_group$v1_3_group$v1_3_d
  v1_3_e <- xml$v1_group$v1_3_group$v1_3_e
  v1_3_f <- xml$v1_group$v1_3_group$v1_3_f
  v1_4_a <- xml$v1_group$v1_4_group$v1_4_a
  v1_4_b <- xml$v1_group$v1_4_group$v1_4_b
  v1_4_c <- xml$v1_group$v1_4_group$v1_4_c
  v1_4_d <- xml$v1_group$v1_4_group$v1_4_d
  v1_4_e <- xml$v1_group$v1_4_group$v1_4_e
  v1_4_f <- xml$v1_group$v1_4_group$v1_4_f
  refused <- xml$consent$refused
  consent_method <- xml$consent$consent_method
  consent_img <- xml$consent$consent_img
  consent_audio <- xml$consent$consent_audio
  survey_instrument <- xml$survey_instrument
  
  householdcontact <- vs.data.frame(uuid, survey_uuid, country, hh_refno, landscape_no, eplot_no, hh_no, 
                                    end_date, end_time, gpsse_lat, gpsse_long, v1_1, v1_2_a_name, v1_2_a_phone, 
                                    v1_2_b_name, v1_2_b_phone, v1_2_c_name, v1_2_c_phone, v1_3_a, v1_3_b, v1_3_c, 
                                    v1_3_d, v1_3_e, v1_3_f, v1_4_a, v1_4_b, v1_4_c, v1_4_d, v1_4_e, v1_4_f, 
                                    refused, consent_method, consent_img, consent_audio, survey_instrument, round) 
  
  ########################
  #Write Info
  #########################
  insertDF(dbcon, householdcontact, 'householdcontact', test)
  
  if (!test){
    dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                                  survey_uuid, "',",
                                  "'vs_household_secv_15_may_2016','",
                                  xml$today,"',current_date);")) 
  }
}
  