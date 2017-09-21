getHHref <- function(dbcon, country_, landscape_no_, eplot_no_, hh_no_){
  hhref <- tbl(dbcon, 'ref_household') %>%
    filter(country==country_ & landscape_no==landscape_no_ & eplot_no==eplot_no_ & hh_no==hh_no_) %>%
    data.frame()
  
  if(!is.null(hhref$id)){
    return(hhref$id)
  }
  else{
    ids <- tbl(dbcon, 'ref_household') %>%
      select(id) %>%
      data.frame
    
    new_id <- paste0(country_, '-', landscape_no_, '-H01') 
    
    i <- 1
    while(new_id %in% ids$id){
      new_id <- paste0(country_, '-', landscape_no_, '-H', substr(100 + i, 2,3))
      i <- i + 1
    }
    
    vars <- data.frame(column_name=c("id", "country", "landscape_no", "eplot_no", "hh_no", "id_old"),
                       data_type=c("text", "character varying", "text", "text", "text", "text"))
    
    insertlist <- list(new_id, country_, landscape_no_, eplot_no_, hh_no_)
    names(insertlist) <- c('id', 'country', 'landscape_no', 'eplot_no', 'hh_no')
    
    dbSendQuery(dbcon$con, paste0("INSERT INTO household_ref VALUES (", 
                                  insertCollapse(insertlist, vars),
                                  ");"))
    
    return(new_id)
    
  }
}

getSpecies <- function(dbcon, genus_, species_, subspecies_){
  if (is.null(subspecies_)){
    subspecies_ <- '-'
  }
  
  genus_ <- gsub("[^a-zA-Z]+", "", genus_)
  genus_ <- paste(toupper(substr(genus_, 1, 1)), substr(genus_, 2, nchar(genus_)), sep="")
  species_ <- gsub("[^a-zA-Z]+", "", species_) %>%
    tolower
  
  ps <- tbl(dbcon, 'plant_species') %>%
    filter(genus==genus_, species==species_, subspecies==subspecies_) %>%
    data.frame()
  
  if(!is.null(ps$id)){
    return(ps$id)
  }
  else{
    ids <- tbl(dbcon, 'plant_species') %>%
      select(id) %>%
      data.frame
    
    new_id <- max(ids$id) + 1
    
    vars <- data.frame(column_name=c("id", "plant_type", "family", "genus", "species", "subspecies", 
                                     "common", "approved"),
                       data_type=c("integer", "text", "text", "text", "text", "text", "text", 
                                   "boolean"))
    
    insertlist <- list(new_id, genus_, species_, subspecies_)
    names(insertlist) <- c('id', 'genus', 'species', 'subspecies')
    
    dbSendQuery(dbcon$con, paste0("INSERT INTO plant_species (id, genus, species, subspecies) VALUES (", 
                                  insertCollapse(insertlist, vars),
                                  ");"))
    
    return(new_id)
    
  }
}

getRound <- function(country, date){
  if (country == 'RWA' & date > '2016-11-01'){
    return(2)
  } else return(1)
}

cutPrefix <- function(vect, n){
  #Remove the first n letters from a vector of character names
  if(is.null(vect)){
    return(NULL)
  }
  names(vect) <- substr(names(vect), n, nchar(names(vect)))
  return(vect)
}

cutSuffix <- function(vect, n){
  #Remove the first n letters from a vector of character names
  if(is.null(vect)){
    return(NULL)
  }
  names(vect) <- substr(names(vect), 1, nchar(names(vect))-n)
  return(vect)
}

grabEnds <- function(l){
  #Get all elements that do not have any child nodes (ie, arent a list)
  classes <- lapply(X=l, FUN=class)
  sel <- l[classes != 'list' & classes != 'NULL']
  return(sel)
}

expandSelMulti <- function(l, allvar, testnums){
  if(is.null(l[allvar][[1]])){
    return(l)
  }
  for (i in testnums){
    spl <- strsplit(l[allvar][[1]], ' ')
    l[paste0(allvar, '_', i)] <- i %in% spl
  }
  return(l)
}

makeGps <- function(sign, value){
  if (is.null(sign) & is.null(value)){
    return(NULL)
  }
  value <- as.numeric(value)
  if (sign %in% c('s', 'w')){
    value <- -value
  }
  return(value)
}

insertCollapse <- function(vect, vars){
  #must be a list!
  i <- 0
  vars <- vars[match(names(vect), vars$column_name), ]
  
  for (v in vect){
    i <- i + 1
    
    type <- vars$data_type[i]
    
    if (is.na(v)){
      v <- 'NULL'
    } else if(!is.logical(v) & type=='boolean'){
      v <- ifelse((v=='1' | v=='TRUE'), "'t'", 
                  ifelse((v=='2' | v=='FALSE'), "'f'", 
                         stop(paste0('Column type is bool but value isn\'t. on ', names(vars$column_name[i])))))
    } else if(is.logical(v)){
      v <- ifelse(v, "'t'", "'f'")
    } else if(suppressWarnings(is.na(as.numeric(v)))){
      v <- paste0("'", gsub("'", "''", v), "'")
    } else if(names(vect)[i] %in% c('eplot_no', 'hh_no', 'region', 'district')){
      v <- paste0("'", v, "'")
    }
    vect[i] <- v
  }
  paste0(vect, collapse = ', ') 
}

insertDF <- function(con, df, tablename, test=FALSE, log=TRUE){
  #Filter out data from ODK that does not go into db
  #Format sql string, casting variables as appropriate
  #Check that uuid does not already exist, if it does and is a test run, then delete uuid
  #Insert data
  
  if (nrow(df)==0){
    warnings('Table', tablename, 'is empty!')
    return(NULL)
  }
  
  for (i in 1:ncol(df)){
    if (class(df[, i]) == 'factor'){
      df[, i] <- as.character(df[ ,i])
    }
  }
  
  vars <- tbl(con, sql("SELECT * FROM information_schema.columns")) %>%
    filter(table_name == tablename) %>%
    select(column_name, data_type) %>%
    data.frame
  
  df <- df[names(df) %in% vars$column_name]
  
  vars <- vars[vars$column_name %in% names(df), ]
  
  str <- paste0('INSERT INTO ', tablename,
                '(', paste0(names(df), collapse=','), 
                ') VALUES ')
  rows <- NULL
  for (i in 1:nrow(df)){
    sel <- df[i, ] %>% as.list
    rowstr <- insertCollapse(sel, vars)
    rows <- c(rows, paste0('(', rowstr, ')'))
  }
  
  str <- paste0(str, paste(rows, collapse=','), ';')
  
  if (test){
    for (i in 1:nrow(df)){
      dbSendQuery(con$con, paste0("DELETE FROM ", tablename, " WHERE uuid = '", df$uuid[i], "';"))
    }
  }

  if (!test){
    uuids <- tbl(con, tablename) %>%
      data.frame

    for (u in df$uuid){
      if(u %in% uuids$uuid){
        stop(paste0('uuid ', u, ' already exists in ', tablename))
      }
    }
  }
  
  dbSendQuery(con$con, str)
  
}

ct <- function(code, list.name, country, region){
  
  if(is.null(code)){
    return(NULL)
  }
  
  df <- codedf[which(codedf$`list name` == list.name), ]
  
  if(!missing(country)){
    df <- df[df$country == country, ]
  }
  if(!missing(region)){
    df <- df[df$region == region, ]
  }
  
  df$label[match(code, df$name)]
  
}

vs.data.frame <- function(...){
  dots <- substitute(list(...))[-1]
  L <- list(...)
  names <- sapply(dots, deparse)
  
  for (l in 1:length(L)){
    if (is.null(L[[l]]) | identical(L[[l]], character(0))| identical(L[[l]], logical(0))){
      L[[l]] <- NA
    }
  }
  
  DF <- as.data.frame(matrix(unlist(L), 1, dimnames = list(NULL, names)))
  DF
}

getxmlrandom <- function(){
  xml <- instances$xml[sample(1:nrow(instances), 1)]
  xml <- xmlToList(xml)
  return(xml)
}

getxmlindb <- function(){
  sel <- tbl(fhcon, 'odk_logger_instance') %>%
    filter((uuid %in% migrations)) %>%
    select(xml, uuid, xform_id) %>%
    collect %>%
    filter(xform_id %in% ids)
  
  sel$xml[sample(1:nrow(sel), size = 1)] %>%
    xmlToList
}

getxmlsection <- function(name){
  while(is.null(xml[[name]])){
    xml <- getxmlrandom()
  }
  xml
}

getxmluuid <- function(uuid){
  df <- tbl(fhcon, 'odk_logger_instance') %>%
    filter(uuid == uuid) %>%
    select(xml, uuid, xform_id) %>%
    collect
  
  xml <- df$xml[df$uuid == uuid]
  xml <- xmlToList(xml)
  xml
}

rowcoalesce <- function(df, cols){
  for (col in cols){
    colx <- paste0(col, '.x')
    coly <- paste0(col, '.y')
    
    df[ , col] <- coalesce(df[ , colx], df[ , coly])
    
    df[ , colx] <- NULL
    df[ , coly] <- NULL
  }
  
  df
}
