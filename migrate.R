library(dplyr)
library(XML)
library(RPostgreSQL)
library(readxl)
library(tidyr)

setwd('D:/Documents and Settings/mcooper/GitHub/vs-migrate/')

db <- 'local'
#db <- 'prod'

forms <- 'house_hold_15_may_2016_v1'#c('agriculture_survey_15_may_2016', 'ffs_yields_paddy_maize_17_sep_2015_v1')

options(stringsAsFactors = F)

source('utils.R')
source('formhub_connection.R')
fhcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

if (db=='prod'){
  source('production_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
  test <- FALSE
}
if (db=='local'){
  source('local_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
  test <- TRUE
}

xforms <- tbl(fhcon, 'odk_logger_xform') %>%
  select(id, id_string) %>%
  collect

ids <- xforms$id[xforms$id_string %in% forms]

migrations <- tbl(dbcon, 'migration_audit') %>%
  select(uuid) %>% 
  collect %>% 
  .$uuid

instances <- tbl(fhcon, 'odk_logger_instance') %>%
  filter((!uuid %in% migrations)) %>%
  select(xml, uuid, xform_id) %>%
  collect %>%
  filter(xform_id %in% ids)

for (i in 1:nrow(instances)){
  print(i)
  if (instances$xform_id[i]==34){
    ffs_yields_maize_17_sep_2015_v1(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==63){
    agriculture_survey_15_may_2016(dbcon, instances$xml[i], test=test, codedf=read_excel('ag/VS_Agriculture_15.05.2016.xls', sheet='choices'))
  }
  if (instances$xform_id[i]==56){
    house_hold_15_may_2016_v1(dbcon, instances$xml[i], test=test, codedf=read_excel('ag/VS_Agriculture_15.05.2016.xls', sheet='choices'))
  }
}


