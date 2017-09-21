library(XML)
library(readxl)
library(RPostgreSQL)

options(stringsAsFactors = FALSE)

ffs_yields_dry_weight_17_sep_2015_v1 <- function(dbcon, xml, test=FALSE){
  assign('codedf', read_xls('migrations/FarmField_Yields_Dry_Weight_17.09.2015.xls', 
                            sheet = 'choices'),
         envir=.GlobalEnv)
  
  xml <- xmlToList(xml)
  
  #############
  #yieldslab
  ##############
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  country <- xml$country
  region <- xml$region %>%
    ct('regions', country=country)
  district <- xml$district %>%
    ct('districts', country=country, region=xml$metadata$region)
  ward <- xml$metadata$ward
  landscape_no <- xml$metadata$landscape_no
  eplot_no <- xml$metadata$eplot_no
  hh_no <- xml$metadata$hh_no
  hh_refno <- getHHref(dbcon, country, landscape_no, eplot_no, hh_no)
  
  yieldlab_data_collection_date <- xml$metadata$date
  round <- getRound(country, yieldlab_data_collection_date)
  
  yieldlab_enumerator_first_name <- xml$metadata$enumerator_first_name
  yieldlab_enumerator_last_name <- xml$metadata$enumerator_last_name
  yieldlab_number_of_fields <- xml$number_of_fields
  
  yieldslab <- vs.data.frame(uuid, survey_uuid, country, region, district, ward, landscape_no, 
                             hh_refno, round, yieldlab_data_collection_date, yieldlab_enumerator_first_name, 
                             yieldlab_enumerator_last_name, yieldlab_number_of_fields)
  
  #################
  #piigeo_yieldslab
  ##################
  
  piigeo_yieldslab <- vs.data.frame(uuid, eplot_no, hh_no)
  
  #################
  #yieldslab_field
  #################
  
  #Field 1
  uuid <- paste0(survey_uuid, '/', 1)
  parent_uuid <- survey_uuid
  survey_uuid <- survey_uuid
  field_no <- xml$selected_fields$selected_first_field
  yieldlab_dry_crop_id <- xml$dry_group$dry_crop_group_a$dry_crop_a$dry_crop_id_a %>%
    ct('dry_crop')
  yieldlab_d_141_a1 <- xml$dry_group$dry_crop_group_a$f1_141_a1
  yieldlab_d_141_a2 <- xml$dry_group$dry_crop_group_a$f1_141_a2
  yieldlab_d_141_b <- xml$dry_group$dry_crop_group_a$f1_141_b %>%
    ct('dry')
  yieldlab_d_141_b_other <- xml$dry_group$dry_crop_group_a$f1_141_b_other
  
  yieldslab_field <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, yieldlab_dry_crop_id,
                                   yieldlab_d_141_a1, yieldlab_d_141_a2, yieldlab_d_141_b, yieldlab_d_141_b_other)
  
  #Field 2
  if(yieldlab_number_of_fields=="2"){
    uuid <- paste0(survey_uuid, '/', 2)
    parent_uuid <- survey_uuid
    survey_uuid <- survey_uuid
    field_no <- xml$selected_fields$selected_second_field
    yieldlab_dry_crop_id <- xml$dry_group$dry_crop_group_b$dry_crop_b$dry_crop_id_b %>%
      ct('dry_crop')
    yieldlab_d_141_a1 <- xml$dry_group$dry_crop_group_b$f2_141_a1
    yieldlab_d_141_a2 <- xml$dry_group$dry_crop_group_b$f2_141_a2
    yieldlab_d_141_b <- xml$dry_group$dry_crop_group_b$f2_141_b %>%
      ct('dry')
    yieldlab_d_141_b_other <- xml$dry_group$dry_crop_group_b$f2_141_b_other
    
    tmp <- vs.data.frame(uuid, parent_uuid, survey_uuid, field_no, yieldlab_dry_crop_id,
                         yieldlab_d_141_a1, yieldlab_d_141_a2, yieldlab_d_141_b, yieldlab_d_141_b_other)
    yieldslab_field <- bind_rows(yieldslab_field, tmp)
  }
 
  ########################
  #Write Info
  #########################
  insertDF(dbcon, yieldslab, 'yieldslab', test)
  insertDF(dbcon, piigeo_yieldslab, 'piigeo_yieldslab', test)
  insertDF(dbcon, yieldslab_field, 'yieldslab_field', test)
  
  if (!test){
    dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                                  survey_uuid, "',",
                                  "'ffs_yields_dry_weight_17_sep_2015_v1','",
                                  xml$today,"',current_date);")) 
  }
}
  
  
  
  