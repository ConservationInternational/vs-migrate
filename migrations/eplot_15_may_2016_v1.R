eplot_15_may_2016_v1 <- function(dbcon, xml, test=FALSE){
  codedf <- read_xls('eplot/VS_Eplot_15.05.2016.xls', sheet = 'choices')
  
  xml <- xmlToList(xml)
  
  survey_uuid <- gsub('uuid:', '', xml$meta$instanceID)
  country <- xml$metadata$meta_display$country
  landscape_no <- xml$metadata$meta_display$landscape_no
  eplot_no <- xml$metadata$meta_display$eplot_no
  field_date <- xml$metadata$meta_display$date
  field_measurer1_first_name <- xml$metadata$measurer1_grp$measurer1
  field_measurer1_last_name <- xml$metadata$measurer1_grp$measurer1_last_name
  field_measurer2_first_name <- xml$metadata$measurer2_grp$measurer2
  field_measurer2_last_name <- xml$metadata$measurer2_grp$measurer2_last_name
  field_scribe_first_name <- xml$metadata$scribe_grp$scribe
  field_scribe_last_name <- xml$metadata$scribe_grp$scribe_last_name
  field_manager_first_name <- xml$metadata$manager_grp$manager
  field_manager_last_name <- xml$metadata$manager_grp$manager_last_name
  field_radius <- xml$metadata$meta_display$radius
  field_observations <- xml$metadata$meta_display$observations
  round <- getRound(country, field_date)
  field_survey_instrument <- xml$survey_instrument
  
  ############################
  #eplot_subplot
  ##############################
  eplot_subplot <- data.frame()
  eplot_subplot_tree <- data.frame()
  eplot_subplot_tree_stem <- data.frame()
  eplot_subplot_vegetation <- data.frame()
  for (i in xml[names(xml)=='subplot_group']){
    mass <- i$subplot_holder$subplot_mass
    subplot <- i$plotnum
    
    if (subplot=='1'){
      crust <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_crust_se
      disturbed <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_disturbed_se
      downed <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_dowsed_se
      dung <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_dung_se
      herbaceous <- i$landscape_se$landscape_herbaceous_se
      herbaceous_erosion <- i$landscape_se$landscape_herbaceous_erosion_se
      litter <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_litter_se
      porous <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_porous_se
      rooted <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_rooted_se
      sealed <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$lanscape_sealed_se
      sodic <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_sodic_se
      stone <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_stose_se
      termite <- i$landscape_se$landscape_surface_condition_se$landscape_group_se$landscape_termite_se
      densiometer_e <- i$landscape_se$densiometer_se_e
      densiometer_n <- i$landscape_se$densiometer_se_n
      densiometer_s <- i$landscape_se$densiometer_se_s
      densiometer_w <- i$landscape_se$densiometer_se_w
    
      gpsse_accuracy <- i$gpsse_group$gpsse_accuracy
      gpsse_start <- i$gpsse_group$gpsse_start
      gpsse_end <- i$gpsse_group$gpsse_end
      gpsse_lat <- makeGps(i$gpsse_group$gpsse_ns, i$gpsse_group$gpsse_lat)
      gpsse_long <- makeGps(i$gpsse_group$gpsse_ew, i$gpsse_group$gpsse_long)
      gpsse_measurement_counts <- i$gpsse_group$gpsse_measurement_counts
    }
    if (subplot=='6'){
      crust <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_crust_sw
      disturbed <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_disturbed_sw
      downed <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_dowsed_sw
      dung <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_dung_sw
      herbaceous <- i$landscape_sw$landscape_herbaceous_sw
      herbaceous_erosion <- i$landscape_sw$landscape_herbaceous_erosion_sw
      litter <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_litter_sw
      porous <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_porous_sw
      rooted <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_rooted_sw
      sealed <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$lanscape_swaled_sw
      sodic <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_sodic_sw
      stone <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_stose_sw
      termite <- i$landscape_sw$landscape_surface_condition_sw$landscape_group_sw$landscape_termite_sw
      densiometer_e <- i$landscape_sw$densiometer_sw_e
      densiometer_n <- i$landscape_sw$densiometer_sw_n
      densiometer_s <- i$landscape_sw$densiometer_sw_s
      densiometer_w <- i$landscape_sw$densiometer_sw_w
      
      gpssw_accuracy <- i$gpssw_group$gpssw_accuracy
      gpssw_lat <- makeGps(i$gpssw_group$gpssw_ns, i$gpssw_group$gpssw_lat)
      gpssw_long <- makeGps(i$gpssw_group$gpssw_ew, i$gpssw_group$gpssw_long)
    }
    if (subplot=='11'){
      crust <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_crust_nw
      disturbed <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_disturbed_nw
      downed <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_dowsed_nw
      dung <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_dung_nw
      herbaceous <- i$landscape_nw$landscape_herbaceous_nw
      herbaceous_erosion <- i$landscape_nw$landscape_herbaceous_erosion_nw
      litter <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_litter_nw
      porous <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_porous_nw
      rooted <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_rooted_nw
      sealed <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$lanscape_nwaled_nw
      sodic <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_sodic_nw
      stone <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_stose_nw
      termite <- i$landscape_nw$landscape_surface_condition_nw$landscape_group_nw$landscape_termite_nw
      densiometer_e <- i$landscape_nw$densiometer_nw_e
      densiometer_n <- i$landscape_nw$densiometer_nw_n
      densiometer_s <- i$landscape_nw$densiometer_nw_s
      densiometer_w <- i$landscape_nw$densiometer_nw_w
      
      gpsnw_accuracy <- i$gpsnw_group$gpsnw_accuracy
      gpsnw_lat <- makeGps(i$gpsnw_group$gpsnw_ns, i$gpsnw_group$gpsnw_lat)
      gpsnw_long <- makeGps(i$gpsnw_group$gpsnw_ew, i$gpsnw_group$gpsnw_long)
    }
    if (subplot=='16'){
      crust <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_crust_ne
      disturbed <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_disturbed_ne
      downed <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_dowsed_ne
      dung <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_dung_ne
      herbaceous <- i$landscape_ne$landscape_herbaceous_ne
      herbaceous_erosion <- i$landscape_ne$landscape_herbaceous_erosion_ne
      litter <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_litter_ne
      porous <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_porous_ne
      rooted <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_rooted_ne
      sealed <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$lanscape_nealed_ne
      sodic <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_sodic_ne
      stone <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_stose_ne
      termite <- i$landscape_ne$landscape_surface_condition_ne$landscape_group_ne$landscape_termite_ne
      densiometer_e <- i$landscape_ne$densiometer_ne_e
      densiometer_n <- i$landscape_ne$densiometer_ne_n
      densiometer_s <- i$landscape_ne$densiometer_ne_s
      densiometer_w <- i$landscape_ne$densiometer_ne_w
      
      gpsne_accuracy <- i$gpsne_group$gpsne_accuracy
      gpsne_lat <- makeGps(i$gpsne_group$gpsne_ns, i$gpsne_group$gpsne_lat)
      gpsne_long <- makeGps(i$gpsne_group$gpsne_ew, i$gpsne_group$gpsne_long)
    }
    if (subplot=='23'){
      crust <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_crust_center
      disturbed <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_disturbed_center
      downed <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_dowsed_center
      dung <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_dung_center
      herbaceous <- i$landscape_center$landscape_herbaceous_center
      herbaceous_erosion <- i$landscape_center$landscape_herbaceous_erosion_center
      litter <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_litter_center
      porous <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_porous_center
      rooted <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_rooted_center
      sealed <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$lanscape_centeraled_center
      sodic <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_sodic_center
      stone <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_stose_center
      termite <- i$landscape_center$landscape_surface_condition_center$landscape_group_center$landscape_termite_center
      densiometer_e <- i$landscape_center$densiometer_center_e
      densiometer_n <- i$landscape_center$densiometer_center_n
      densiometer_s <- i$landscape_center$densiometer_center_s
      densiometer_w <- i$landscape_center$densiometer_center_w
      
      field_photo_e <- i$landscape_center$photos_center$photo_e
      field_photo_n <- i$landscape_center$photos_center$photo_n
      field_photo_s <- i$landscape_center$photos_center$photo_s
      field_photo_w <- i$landscape_center$photos_center$photo_w
    }
    
    uuid <- paste0(survey_uuid, '/', subplot)
    parent_uuid <- survey_uuid
    
    tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, mass, crust, disturbed, downed, 
                            dung, herbaceous, herbaceous_erosion, litter, porous, rooted, 
                            sealed, sodic, stone, subplot, termite, densiometer_e, 
                            densiometer_n, densiometer_s, densiometer_w)
    
    eplot_subplot <- bind_rows(eplot_subplot, tempdf)
    
    ##############################
    #eplot_subplot_vegetation
    #################################
    parent_uuid <- paste0(survey_uuid, '/', subplot)
    
    ranks <- list()
    rank <- 1
    uuid <- paste0(parent_uuid, '/', rank)
    genus <- i$subplot_holder$subplot_rank1_group$subplot_rank1_genus
    species <- i$subplot_holder$subplot_rank1_group$subplot_rank1_species
    subspecies <- i$subplot_holder$subplot_rank1_group$subplot_rank1_subspecies
    common <- i$subplot_holder$subplot_rank1_group$subplot_rank1_common
    tree_photo <- i$subplot_holder$subplot_rank1_group$subplot_rank1_tree_rank1_photo
    stembark_photo <- i$subplot_holder$subplot_rank1_group$subplot_rank1_stambark_rank1_photo
    branch_photo <- i$subplot_holder$subplot_rank1_group$subplot_rank1_branch_rank1_photo
    leaf_photo <- i$subplot_holder$subplot_rank1_group$subplot_rank1_leaf_rank1_photo
    if(!is.null(genus)){
      tax <- getSpecies(dbcon, genus, species, subspecies)
      rank1 <- vs.data.frame(uuid, rank, tax, tree_photo, stembark_photo, branch_photo, leaf_photo)
      ranks[[1]] <- rank1
    }
    
    rank <- 2
    uuid <- paste0(parent_uuid, '/', rank)
    genus <- i$subplot_holder$subplot_rank2_group$subplot_rank2_genus
    species <- i$subplot_holder$subplot_rank2_group$subplot_rank2_species
    subspecies <- i$subplot_holder$subplot_rank2_group$subplot_rank2_subspecies
    common <- i$subplot_holder$subplot_rank2_group$subplot_rank2_common
    tree_photo <- i$subplot_holder$subplot_rank2_group$subplot_rank2_tree_rank2_photo
    stembark_photo <- i$subplot_holder$subplot_rank2_group$subplot_rank2_stambark_rank2_photo
    branch_photo <- i$subplot_holder$subplot_rank2_group$subplot_rank2_branch_rank2_photo
    leaf_photo <- i$subplot_holder$subplot_rank2_group$subplot_rank2_leaf_rank2_photo
    if(!is.null(genus)){
      tax <- getSpecies(dbcon, genus, species, subspecies)
      rank2 <- vs.data.frame(uuid, rank, tax, tree_photo, stembark_photo, branch_photo, leaf_photo)
      ranks[[2]] <- rank2
    }
    
    rank <- 3
    uuid <- paste0(parent_uuid, '/', rank)
    genus <- i$subplot_holder$subplot_rank3_group$subplot_rank3_genus
    species <- i$subplot_holder$subplot_rank3_group$subplot_rank3_species
    subspecies <- i$subplot_holder$subplot_rank3_group$subplot_rank3_subspecies
    common <- i$subplot_holder$subplot_rank3_group$subplot_rank3_common
    tree_photo <- i$subplot_holder$subplot_rank3_group$subplot_rank3_tree_rank3_photo
    stembark_photo <- i$subplot_holder$subplot_rank3_group$subplot_rank3_stambark_rank3_photo
    branch_photo <- i$subplot_holder$subplot_rank3_group$subplot_rank3_branch_rank3_photo
    leaf_photo <- i$subplot_holder$subplot_rank3_group$subplot_rank3_leaf_rank3_photo
    if(!is.null(genus)){
      tax <- getSpecies(dbcon, genus, species, subspecies)
      rank3 <- vs.data.frame(uuid, rank, tax, tree_photo, stembark_photo, branch_photo, leaf_photo)
      ranks[[3]] <- rank3
    }
    
    if (length(ranks) > 0){
      tempdf <- Reduce(bind_rows, ranks)
      
      tempdf$parent_uuid <- parent_uuid
      tempdf$survey_uuid <- survey_uuid
      
      eplot_subplot_vegetation <- bind_rows(eplot_subplot_vegetation, tempdf)
    }
    #####################
    #eplot_subplot_tree
    #####################
    
    for (j in i$subplot_holder[names(i$subplot_holder)=='tree']){
      basal_circ_height <- xml$metadata$meta_display$basal_circ_height
      tree_code <- j$subplot_tree_display$subplot_tree_code
      
      genus <- j$subplot_tree_display$subplot_tree_genus
      species <- j$subplot_tree_display$subplot_tree_species
      subspecies <- j$subplot_tree_display$subplot_tree_subspecies
      
      tax <- getSpecies(dbcon, genus, species, subspecies)
      stem_class <- j$subplot_tree_stems
      if (stem_class == '1'){
        canopy_width <- j$subplot_tree_singlestem$subplot_tree_singlestem_canopy_width
        height <- j$subplot_tree_singlestem$subplot_tree_singlestem_height
        circumference <- j$subplot_tree_singlestem$subplot_tree_singlestem_circumference
        numstems <- '1'
      }
      if (stem_class == '2'){
        stemcount <- 1
        for (k in j$subplot_tree_multistem[names(j$subplot_tree_multistem)=='subplot_tree_multistem_stem']){
          ###########################################
          #eplot_subplot_tree_stem
          ###########################################
          uuid <- paste(survey_uuid, subplot, j$tree_position, stemcount, sep='/') 
          parent_uuid <- paste(survey_uuid, subplot, j$tree_position, sep='/') 
          survey_uuid <- survey_uuid
          multistem_stemid <- k$subplot_tree_multistem_stemID
          multistem_height <- k$subplot_tree_multistem_height
          multistem_canopy_width <- k$subplot_tree_multistem_width
          multistem_circumference <- k$subplot_tree_multistem_circumference
          
          stemcount <- stemcount + 1
          tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, multistem_stemid, multistem_height, multistem_canopy_width, multistem_circumference)
          
          eplot_subplot_tree_stem <- bind_rows(eplot_subplot_tree_stem, tempdf)
        }
        
        canopy_width <- NULL
        height <- NULL
        circumference <- NULL
        numstems <- stemcount
        
      }
      if (stem_class == '3'){
        canopy_width <- j$subplot_tree_multistem_samesize$subplot_tree_multistem_samesize_canopy_width
        height <- j$subplot_tree_multistem_samesize$subplot_tree_multistem_samesize_height
        circumference <- j$subplot_tree_multistem_samesize$subplot_tree_multistem_samesize_circumference
        numstems <- j$subplot_tree_multistem_samesize$subplot_tree_multistem_samesize_numstems
      }
      
      tree_photo <- j$subplot_tree_tree_tree_photo
      stembark_photo <- j$subplot_tree_stembark_tree_photo
      branch_photo <- j$subplot_tree_branch_tree_photo
      leaf_photo <- j$subplot_tree_leaf_tree_photo
      subplot <- subplot
      
      uuid <- paste0(survey_uuid, '/', subplot, '/', j$tree_position)
      parent_uuid <- paste0(survey_uuid, '/', subplot)
      survey_uuid <- survey_uuid
    
      tempdf <- vs.data.frame(uuid, parent_uuid, survey_uuid, basal_circ_height, tree_code, tax, 
                            stem_class, canopy_width, height, circumference, numstems, tree_photo,
                            stembark_photo, branch_photo, leaf_photo, subplot)
      
      eplot_subplot_tree <- bind_rows(eplot_subplot_tree, tempdf)
    }
    
    mass <- NULL
    crust <- NULL
    disturbed <- NULL
    downed <- NULL
    dung <- NULL
    herbaceous <- NULL
    herbaceous_erosion <- NULL
    litter <- NULL
    porous <- NULL
    rooted <- NULL
    sealed <- NULL
    sodic <- NULL
    stone <- NULL
    subplot <- NULL
    termite <- NULL
    densiometer_e <- NULL
    densiometer_n <- NULL
    densiometer_s <- NULL
    densiometer_w <- NULL
  }
  
  uuid <- survey_uuid
  
  eplot <- vs.data.frame(uuid, survey_uuid, country, landscape_no, eplot_no, field_date, 
                         field_measurer1_first_name, field_measurer1_last_name, field_measurer2_first_name, 
                         field_measurer2_last_name, field_scribe_first_name, field_scribe_last_name, 
                         field_manager_first_name, field_manager_last_name, field_radius, gpsne_accuracy, 
                         gpsne_lat, gpsne_long, gpsnw_accuracy, gpsnw_lat, gpsnw_long, gpsse_accuracy, 
                         gpsse_start, gpsse_end, gpsse_lat, gpsse_long, gpsse_measurement_counts, 
                         gpssw_accuracy, gpssw_lat, gpssw_long, field_photo_e, field_photo_n, field_photo_s, 
                         field_photo_w, field_observations, round, field_survey_instrument)

  ########################
  #Write Info
  #########################
  insertDF(dbcon, eplot, 'eplot', test)
  insertDF(dbcon, eplot_subplot, 'eplot_subplot', test)
  insertDF(dbcon, eplot_subplot_tree, 'eplot_subplot_tree', test)
  insertDF(dbcon, eplot_subplot_tree_stem, 'eplot_subplot_tree_stem', test)
  insertDF(dbcon, eplot_subplot_vegetation, 'eplot_subplot_vegetation', test)

  if (!test){
    dbSendQuery(dbcon$con, paste0('INSERT INTO migration_audit VALUES (\'', 
                                  survey_uuid, "',",
                                  "'eplot_15_may_2016_v1','",
                                  xml$today,"',current_date);")) 
  }
  
}