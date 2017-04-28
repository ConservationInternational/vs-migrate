library(dplyr)
library(XML)
library(RPostgreSQL)

setwd('D:/Documents and Settings/mcooper/GitHub/vs-migrate/')

db <- 'local'
#db <- 'prod'

forms <- c('ffs_yields_paddy_maize_17_sep_2015_v1')#, 'ffs_yields_maize_02_jul_2015_v1', 'ffs_yields_paddy_maize_02_july_2015_v1', 'ffs_yields_maize_17_sep_2015_v1')

source('formhub_connection.R')
fhcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

if (db=='prod'){
  source('production_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
  test = FALSE
}
if (db=='local'){
  source('local_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
  test = TRUE
}

xforms <- tbl(fhcon, 'odk_logger_xform') %>%
  select(id, id_string) %>%
  data.frame()

ids <- xforms$id[xforms$id_string %in% forms]

migrations <- tbl(dbcon, 'migration_audit') %>%
  select(uuid) %>% 
  data.frame %>% 
  .$uuid

instance_ids <- tbl(fhcon, 'odk_logger_instance') %>%
  select(id, uuid, xform_id) %>%
  data.frame %>%
  #filter((!uuid %in% migrations)) %>%
  filter(xform_id %in% ids)

instances <- tbl(fhcon, 'odk_logger_instance') %>%
  filter(id %in% instance_ids$id) %>%
  select(xml, xform_id) %>%
  data.frame


for (i in 1:nrow(instances)){
  print(i)
  if (instances$xform_id[i]==34){
    ffs_yields_maize_17_sep_2015_v1(dbcon, instances$xml[i], test=test)
  }
}


