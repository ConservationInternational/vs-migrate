######################################
#This is the script that was used to write the original migration audit
#It is here for documentation purposes
#####################################

library(dplyr)
library(XML)
library(RPostgreSQL)

setwd('D:/Documents and Settings/mcooper/GitHub/vs-migrate/')

source('formhub_connection.R')
fhcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

source('production_connection.R')
dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

table <- tbl(dbcon, 'rra') %>%
  select(uuid) %>%
  collect

audit <- tbl(dbcon, sql('SELECT * FROM audit.logged_actions')) %>%
  filter(table_name == 'rra' & action == 'D') %>%
  select(record_id) %>%
  collect

# auditold <- tbl(dbcon, sql('SELECT * FROM audit.logged_actions')) %>%
#   filter(table_name == 'rapid_water' & action == 'D') %>%
#   select(record_id) %>%
#   collect

uuids <- c(table$uuid, audit$record_id, auditold$record_id)

formids <- tbl(fhcon, 'odk_logger_xform') %>%
  select(id, id_string)

forms <- tbl(fhcon, 'odk_logger_instance') %>%
  filter(uuid %in% uuids) %>%
  left_join(formids, by=c('xform_id'='id')) %>%
  select(uuid, form=id_string, upload_date=date_created) %>%
  collect

forms$upload_date <- as.Date(forms$upload_date)

forms$migration_date <- '2017-04-26'

#forms <- forms[!forms$form %in% c('eplot_17_sep_2015_v1', 'eplot_v20_draft'), ]

dbWriteTable(dbcon$con, 'migration_audit', forms, append=T, row.names=F)











