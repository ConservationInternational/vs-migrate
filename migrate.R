library(dplyr)
library(dbplyr)
library(XML)
library(RPostgreSQL)
library(readxl)
library(tidyr)

setwd('C://Git/vs-migrate/')

#db <- 'local'
db <- 'prod'

#This variable determines whether 1) the script will overwrite exisiting data and 2) the migration will be added to the migration_audit table.
#test <- FALSE
test <- TRUE

forms <- c('vs_household_secv_15_may_2016', 'ffs_yields_dry_weight_17_sep_2015_v1', 
           'ffs_yields_paddy_maize_17_sep_2015_v1', 'eplot_15_may_2016_v1', 
           'agriculture_survey_15_may_2016', 'house_hold_15_may_2016_v1')

options(stringsAsFactors = F)

source('utils.R')
source('formhub_connection.R')
fhcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

if (db=='prod'){
  source('production_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
}
if (db=='local'){
  source('local_connection.R')
  dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)
}

#Get a list of all xform ids
xforms <- tbl(fhcon, 'odk_logger_xform') %>%
  select(id, id_string) %>%
  collect

ids <- xforms$id[xforms$id_string %in% forms]

#Get a list of all previous migration IDs
migrations <- tbl(dbcon, 'migration_audit') %>%
  select(uuid) %>% 
  collect %>% 
  .$uuid

#Get all forms that do not have an ID in migration_audit
instances <- tbl(fhcon, 'odk_logger_instance') %>%
  filter((!uuid %in% migrations)) %>%
  select(xml, uuid, xform_id) %>%
  collect %>%
  filter(xform_id %in% ids)

scripts <- list.files(path='migrations', pattern='.R$', recursive = T)

for (s in scripts){
  source(paste0('migrations/', s))
}

for (i in nrow(instances):1){
  cat(i, i/nrow(instances), '\n')
  if (instances$xform_id[i]==34){
    ffs_yields_maize_17_sep_2015_v1(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==63){
    agriculture_survey_15_may_2016(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==56){
    house_hold_15_may_2016_v1(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==41){
    eplot_15_may_2016_v1(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==33){
    ffs_yields_dry_weight_17_sep_2015_v1(dbcon, instances$xml[i], test=test)
  }
  if (instances$xform_id[i]==47){
    vs_household_secv_15_may_2016(dbcon, instances$xml[i], test=test)
  }
}


