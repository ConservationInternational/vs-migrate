library(dplyr)
library(XML)
library(RPostgreSQL)

setwd('D:/Documents and Settings/mcooper/GitHub/vs-migrate/')

source('formhub_connection.R')
fhcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

source('production_connection.R')
dbcon <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)


forms <- tbl(fhcon, 'odk_logger_xform') %>%
  select(id, id_string)

migrations <- tbl(dbcon, 'migration_audit') %>%
  select(uuid) %>% 
  data.frame %>% 
  .$uuid

tbl <- tbl(fhcon, 'odk_logger_instance') %>%
  select(xml, uuid, xform_id) %>%
  data.frame()




