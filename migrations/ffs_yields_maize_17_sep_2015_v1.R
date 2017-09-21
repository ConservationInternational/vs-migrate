library(XML)
library(readxl)
library(RPostgreSQL)

options(stringsAsFactors = FALSE)

ffs_yields_maize_17_sep_2015_v1 <- function(dbcon, xml, test=FALSE){
  xml <- xmlToList(xml)
  
  ###########
  #yields
  ###########
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid

  country <- xml$country
  region <- xml$region
  district <- xml$district
  ward <- xml$metadata$ward
  landscape_no <- xml$metadata$landscape_no
  
  hh_refno <- getHHref(dbcon, country, landscape_no,
                       xml$metadata$eplot_no, xml$metadata$hh_no)  
  
  yield_data_collection_date <- xml$metadata$data_collection_date
  
  round <- getRound(country, yield_data_collection_date)
  
  yield_enumerator_first_name <- xml$metadata$enumerator_first_name
  yield_enumerator_last_name <- xml$metadata$enumerator_last_name
  yield_number_of_fields <- xml$number_of_fields
  yield_selected_first_field <- xml$a_group$selected_fields$selected_first_field
  yield_selected_second_field <- xml$a_group$selected_fields$selected_second_field
  
  yields <- vs.data.frame(uuid, survey_uuid, country, region, district, ward,
                             landscape_no, hh_refno, round, yield_data_collection_date, yield_enumerator_first_name,
                             yield_enumerator_last_name, yield_number_of_fields, yield_selected_first_field, yield_selected_second_field)

  ###################
  #piiname_yields
  ###################
  
  uuid <- survey_uuid
  yield_head_name <- paste0(xml$metadata$farmers_first_name, ' ', xml$metadata$farmers_last_name)
  
  piiname_yields <- vs.data.frame(uuid, yield_head_name)
  
  ###################
  #piigeo_yields
  ###################
  
  uuid <- survey_uuid
  eplot_no <- xml$metadata$eplot_no
  hh_no <- xml$metadata$hh_no
  
  piigeo_yields <- vs.data.frame(uuid, eplot_no, hh_no)
  
  ################
  #yields_field
  ###############

  #seca
  seca <- xml$a_group[grepl('_group', names(xml$a_group))]
  
  secadf <- data.frame()
  for (i in seca){
    f <- cutPrefix(i, 4)
    secadf <- bind_rows(secadf, data.frame(f))
  }

  field_no <- paste0('M', row.names(secadf))
  parent_uuid <- uuid
  survey_uuid <- uuid
  uuid <- paste0(uuid, '/', row.names(secadf))

  secadf <- data.frame(uuid, parent_uuid, survey_uuid, field_no, secadf)
  
  secadf[ , c('a_1', 'a_2', 'a_3', 'a_5', 'a_6')] <- secadf[ , c('a_1', 'a_2', 'a_3', 'a_5', 'a_6')] == '1'

  #secb
  secbdf <- data.frame()
  for(i in xml$b_group){
    i <- cutPrefix(i, 4)
    
    b_meta_ <- grabEnds(i) %>%
      expandSelMulti('b_126_a', c('1', '2', '3', '4', '5', '90'))
    b_meta_$b_12_sum <- NULL
    
    b_11_ <- cutPrefix(i$b_11_group, 4) %>% 
      grabEnds
    
    b_13_ <- cutPrefix(i$b_13_group, 4) %>% 
      grabEnds %>%
      expandSelMulti('b_13_11', c('1', '2', '3', '4', '5', '6', '7', '8'))
    
    b_12_ <- cutPrefix(i$b_12_group, 4) %>%
      grabEnds
    
    if(is.null(b_12_)){
      b_12_ <- cutPrefix(i$b_14_group, 4) %>%
        grabEnds
      names(b_12_) <- gsub('14', '12', names(b_12_))
    }
    
    crop <- cutPrefix(i$y_crop, 4) %>%
      .[['yield_crop_name']]

    b_101 <- cutPrefix(i$`101_grp`, 4) %>%
      .[['101']]

    b_11_group <- cutPrefix(i$b_11_group, 4)
    
    gpsne_ <- cutSuffix(b_11_group$ne_group, 3) %>%
      grabEnds
    
    gpsne_$gpsne_lat <- makeGps(gpsne_$gpsne_ns, gpsne_$gpsne_lat)
    gpsne_$gpsne_long <- makeGps(gpsne_$gpsne_ew, gpsne_$gpsne_long)
    
    #gpsne_$gpsne_ns <- NULL
    #gpsne_$gpsne_ew <- NULL
    
    df <- data.frame(b_meta_, b_11_, b_12_, b_13_, crop, b_101, gpsne_)
                       
    secbdf <- bind_rows(secbdf, df)
  }

  secbdf$survey_uuid <- survey_uuid
  
  fields <- c(yield_selected_first_field, yield_selected_second_field)
  secbdf$field_no <- fields[fields != 'None']
  
  yields_field <- merge(secadf, secbdf, all.x=T)
  
  #####################################
  #piigeo_yields_field
  #####################################
  piigeo_yields_field <- yields_field[ , c('uuid', 'field_name')]
  piigeo_yields_field$gpsne_lat <- mapply(FUN=makeGps, sign=yields_field$gpsne_ns, value=yields_field$gpsne_lat)
  piigeo_yields_field$gpsne_long <- mapply(FUN=makeGps, sign=yields_field$gpsne_ew, value=yields_field$gpsne_long)
  
  yields_field[ , c('uuid', 'field_name', 'gpsne_lat', 'gpsne_long', 'gpsne_ew', 'gpsne_ns')] <- NULL
  
  ##################################
  #Implement Rules across columns
  #################################
  
  yields_field$a_5[which(!yields_field$a_5)] <- FALSE
  yields_field$a_6[which(!yields_field$a_6)] <- FALSE
  
  yields_field$b_110[yields_field$b_107 == '2'] <- 0
  yields_field$b_115[yields_field$b_112 == '2'] <- 0
  yields_field$b_120[yields_field$b_117 == '2'] <- 0
  
  yields_field$b_122_b_1 <- grepl('1', yields_field$b_122_b)
  yields_field$b_122_b_2 <- grepl('2', yields_field$b_122_b)
  
  if(!is.null(yields_field$b_126_a_1)){
    yields_field$b_126_a_1[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '1']
    yields_field$b_126_a_2[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '2']
    yields_field$b_126_a_3[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '3']
    yields_field$b_126_a_4[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '4']
    yields_field$b_126_a_5[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '5']
    yields_field$b_126_a_90[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '90']
  }
  
  ################################
  #Give Appropriate Names
  ################################
  
  names(yields_field)[grepl('a_|b_|crop', names(yields_field))] <- paste0('yield_', names(yields_field)[grepl('a_|b_|crop', names(yields_field))])
  
  
  yields_field[ , c("yield_b_109_name", "yield_b_114_name", "yield_b_119_name", 
                    "yield_b_11_136a_calc")] <- NULL
  
  ##############
  #Insert Data
  ##############
  insertDF(dbcon, yields, 'yields', test)
  insertDF(dbcon, piiname_yields, 'piiname_yields', test)
  insertDF(dbcon, piigeo_yields, 'piigeo_yields', test)
  insertDF(dbcon, yields, 'yields', test)

  if (!test){
    dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                                  survey_uuid, "',",
                                  "'household_hold_15_may_2016_v1','",
                                  xml$today,"',current_date);")) 
  }
                          
}