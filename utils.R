getHash <- function(){
  c(letters, LETTERS, seq(0,9)) %>%
    sample(11, replace=T) %>%
    paste0(collapse='')
}

insertCollapse <- function(vect){
  #must be a list!
  for (v in vect){
    if(is.na(as.numeric(v))){
      vect[vect==v] <- paste0("'", v, "'")
    }
  }
  paste0(vect, collapse = ', ') 
}

getHHref <- function(country_, landscape_no_, eplot_no_, hh_no_){
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