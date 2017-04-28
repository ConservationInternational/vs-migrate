library(XML)
library(readxl)
library(RPostgreSQL)

options(stringsAsFactors = FALSE)

ffs_yields_maize_17_sep_2015_v1 <- function(con, xml, test=FALSE){
  xml <- xmlToList(xml)
  
  ###########
  #yields_hh
  ###########
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  
  metadata_ <- grabEnds(xml$metadata)
  metadata_$hhid <- NULL
  
  basedata_ <- xml[c('country', 'region', 'district', 'number_of_fields')]

  selected_first_field <- xml$a_group$selected_fields$selected_first_field
  selected_second_field <- xml$a_group$selected_fields$selected_second_field
  
  hh_refno <- getHHref(con, basedata_$country, metadata_$landscape_no,
                       metadata_$eplot_no, metadata_$hh_no)
  
  round <- getRound(basedata_$country, metadata_$data_collection_date)
  
  yields_hh <- data.frame(uuid, survey_uuid, hh_refno, round, metadata_, basedata_,
                          selected_first_field, selected_second_field) %>%
    select(-farmers_first_name, -farmers_last_name)

  ################
  #yields_hh_pii
  ################
  
  yields_hh_pii <- data.frame(uuid, metadata_) %>%
    select(uuid, farmers_first_name, farmers_last_name)

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
  secb <- xml$b_group
  
  secbdf <- data.frame()
  for(i in secb){
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
  
  fields <- c(selected_first_field, selected_second_field)
  secbdf$field_no <- fields[fields != 'None']
  
  yields_field <- merge(secadf, secbdf, all.x=T) %>% 
    select(-gpsne_lat, -gpsne_long, -gpsne_accuracy)
  
  ##################
  #yields_field_pii
  ##################
  yields_field_pii <- merge(secadf, secbdf, all.x=T) %>% 
    select(uuid, gpsne_lat, gpsne_long, gpsne_accuracy)
  
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
  
  yields_field$b_126_a_1[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '1']
  yields_field$b_126_a_2[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '2']
  yields_field$b_126_a_3[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '3']
  yields_field$b_126_a_4[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '4']
  yields_field$b_126_a_5[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '5']
  yields_field$b_126_a_90[is.na(yields_field$b_126_a_1) & yields_field$b_126 == '90']
  
  ##############
  #Insert Data
  ##############
  insertDF(con, yields_hh, 'yields_hh', test)
  insertDF(con, yields_hh_pii, 'yields_hh_pii', test)
  insertDF(con, yields_field, 'yields_field', test)
  insertDF(con, yields_field_pii, 'yields_field_pii', test)
  
  if(test){
    dbSendQuery(con$con, paste0("DELETE FROM migration_audit WHERE uuid = '",
                                gsub('uuid:', '', xml$meta$instanceID), 
                                "';"))
  }
  dbSendQuery(con$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                          gsub('uuid:', '', xml$meta$instanceID), "',",
                          "'ffs_yields_paddy_maize_17_sep_2015_v1','",
                          xml$today,"',current_date);")) 
                          
}