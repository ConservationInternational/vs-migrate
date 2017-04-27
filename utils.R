getHash <- function(){
  c(letters, LETTERS, seq(0,9)) %>%
    sample(11, replace=T) %>%
    paste0(collapse='')
}

getHHref <- function(dbcon, country_, landscape_no_, eplot_no_, hh_no_){
  hhref <- tbl(dbcon, 'household_ref') %>%
    filter(country==country_ & landscape_no==landscape_no_ & eplot_no==eplot_no_ & hh_no==hh_no_) %>%
    data.frame()
  
  if(!is.null(hhref$id)){
    return(hhref$id)
  }
  else{
    ids <- tbl(dbcon, 'household_ref') %>%
      select(id)
    
    new_id <- getHash()
    
    while(newid %in% ids$id){
      new_id <- getHash()
    }
    
    dbSendQuery(dbcon$con, paste0("INSERT INTO household_ref VALUES (", 
                                  insertCollapse(list(new_id, country_, landscape_no_, eplot_no_, hh_no_)),
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
  value <- as.numeric(value)
  if (sign %in% c('s', 'w')){
    value <- -value
  }
  return(value)
}

insertCollapse <- function(vect){
  #must be a list!
  i <- 0
  for (v in vect){
    i <- i + 1
    
    if(is.logical(v)){
      v <- ifelse(v, 't', 'f')
    }
    if(is.na(as.numeric(v))){
      v <- paste0("'", v, "'")
    }
    if(names(vect)[i] %in% c('eplot_no', 'hh_no')){
      v <- paste0("'", v, "'")
    }
    vect[i] <- v
  }
  paste0(vect, collapse = ', ') 
}

insertDF <- function(con, df, tablename){
  str <- paste0('INSERT INTO ', tablename,
                '(', paste0(names(df), collapse=','), 
                ') VALUES ')
  rows <- NULL
  for (i in 1:nrow(df)){
    sel <- df[i, ] %>% as.list
    rowstr <- insertCollapse(sel)
    rows <- c(rows, paste0('(', rowstr, ')'))
  }
  
  str <- paste0(str, paste(rows, collapse=','), ';')
  
  uuids <- tbl(con, tablename) %>%
    select(uuid) %>% data.frame %>% .$uuid
  
  for (u in df$uuid){
    if(u %in% uuids){
      stop(paste0('uuid ', u, 'already exists in ', tablename))
    }
  }
  
  dbSendQuery(con, str)
  
}








