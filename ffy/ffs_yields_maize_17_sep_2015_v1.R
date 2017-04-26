library(XML)
library(readxl)

ffs_yields_maize_17_sep_2015_v1 <- function(xml){
  xml <- xmlToList(xml)
  
  map <- read_xlsx('D:/Documents and Settings/mcooper/Documents/Database Restructuring/Formhub-DB Mapping 2017.xlsx')
  
  map <- filter(map, `September 2015`=='ffs_yields_maize_17_sep_2015_v1')
  
  #yields_hh
  yields_hh <- filter(map, dbtable == 'yields_hh')
  
  metadata <- xml$metadata
  
  uuid <- gsub('uuid:', '', xml$meta$instanceID)
  survey_uuid <- uuid
  country <- xml$country
  region <- xml$region
  district <- xml$district
  ward <- metadata$ward
  landscape_no <- metadata$landscape_no
  eplot_no <- metadata$eplot_no
  hh_no <- metadata$hh_no
  hh_refno <- getHHref(country, landscape_no, eplot_no, hh_no)
  round <- getRound(country, xml$today)
  data_collection_date <- metadata$data_collection_date
  enumerator_first_name <- metadata$enumerator_first_name
  enumerator_last_name <- metadata$enumerator_last_name
  number_of_fields <- xml$number_of_fields
  selected_first_field <- xml$a_group$selected_fields$selected_first_field
  selected_second_field <- xml$a_group$selected_fields$selected_second_field
  
  yields_hh <- data.frame(uuid, survey_uuid, country, region, district, ward,
                          landscape_no, eplot_no, hh_no, hh_refno, round,
                          data_collection_date, enumerator_first_name, enumerator_last_name,
                          selected_first_field, selected_second_field)

  
  #yields_hh_pii
  farmers_first_name <- metadata$farmers_first_name
  farmers_last_name <- metadata$farmers_last_name
  
  yields_pii <- data.frame(uuid, farmers_first_name, farmers_last_name)

  
  #yields_field
  
  seca <- xml$a_group[grepl('_group', names(xml$a_group))]
  
  secadf <- 
  
  uuid
  parent_uuid
  survey_uuid
  field_no
  field_name
  a_1
  a_2
  a_3
  a_5
  a_6
  b_103
  b_103_a
  b_106
  b_107
  b_109
  b_109_name
  b_109_other
  b_110
  b_112
  b_114
  b_114_name
  b_114_other
  b_115
  b_117
  b_119
  b_119_name
  b_119_other
  b_120
  b_122
  b_122_b
  b_122_b_1
  b_122_b_2
  b_123
  b_125
  b_126
  b_126_a_1
  b_126_a_2
  b_126_a_3
  b_126_a_4
  b_126_a_5
  b_126_a_90
  b_126_b
  b_128
  b_128_a
  b_128_b
  b_128_c
  b_11_130_1
  b_11_130_2
  b_11_132
  b_11_104
  b_11_105
  b_11_133
  b_11_134
  b_11_136
  b_11_137
  b_11_134_r
  b_11_136_r
  b_11_137_r
  b_13_8
  b_13_9
  b_13_10
  b_13_11
  b_13_11_1
  b_13_11_2
  b_13_11_3
  b_13_11_4
  b_13_11_5
  b_13_11_6
  b_13_11_7
  b_13_11_8
  b_12_1
  b_12_2
  b_12_3
  b_12_4
  b_12_5
  b_12_6
  b_12_7
  b_12_8
  b_12_9
  b_12_10
  b_12_11
  b_13_13
  crop
  b_101
  
  
}