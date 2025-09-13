#list of country/ind/model

prevMap.leaflet.alt <- function(res.obj,
                                gadm.shp,
                                by.adm1 = "NAME_1",
                                by.adm2 = "NAME_2",
                                admin1.focus = NULL,
                                color.palette = NULL,
                                value.to.plot = 'mean',
                                value.range = NULL,
                                num_bins=NULL,
                                legend.label = 'Estimates',
                                map.title = NULL,
                                color.reverse = T,
                                no.hatching= F,
                                hatching.density=12,
                                use.basemap= NULL,
                                threshold.p=NULL,
                                legend.color.reverse= T){
  
  ########################################################
  ### check required packages
  ########################################################
  
save(gadm.shp, file = "C:/Users/lucyx/Desktop/prevmap_process/gadm_shp.RData")
  
  if (!requireNamespace("leaflegend", quietly = TRUE)) {
    stop("Package 'leaflegend' is required for this function. Please install it with install.packages('leaflegend').")
  }
  
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required for this function. Please install it with install.packages('viridisLite').")
  }
  
  ########################################################
  ### initialize parameters
  ########################################################
  
  gadm.shp <- sf::st_as_sf(gadm.shp)
  
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  ##
  adm_level <- sub(".*_(\\d+)$", "\\1", by.adm2)
  if (!grepl("^[0-9]+$", adm_level)) {
    adm_level <- "0"
  }
  
  survey.res <- res.obj[[paste0("res.admin", adm_level)]]
  
  if (adm_level == "0") {
    post.samp.mat <- NULL
  } else {
    post.samp.mat <- res.obj[[paste0("admin", adm_level, "_post")]]
  }

  
  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  #res.to.plot <- format_tab_num(survey.res=res.to.plot)
  #res.to.plot$value <- res.to.plot[[value.to.plot]] ### name the variable to plot as value
  
  ### prepare exceedance probabilities
  if(value.to.plot=='exceed_prob'){
    
    post.samp.mat <- res.obj[[paste0('admin',pseudo_level,'_post')]]
    
    if(is.null(post.samp.mat)){stop('No posterior samples provided, cannot produce exceedance probability map.')}
    if(is.null(threshold.p)){stop('No threshold provided, cannot produce exceedance probability map.')}
    
    ### process posterior samples to be in the correct format
    post.samp.mat <- as.matrix(post.samp.mat)
    
    n.samp= 1000
    if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
      post.samp.mat <- t(post.samp.mat)
    }
    
    # threshold.p <- 0.1
    
    res.to.plot$exceed_prob <- apply(post.samp.mat,2,function(x){sum(x>threshold.p)/length(x)})
    
    ### if no valid uncertainty measure, assign NA to exceedance probability
    if(sum(is.na(res.to.plot$var))>0){
      res.to.plot[is.na(res.to.plot$var),]$exceed_prob <- NA
    }
    
  }
  
  
  ########################################################
  ### merge results with spatial dataset
  ########################################################
  
  if (is.null(by.adm1) && is.null(by.adm2)) {
    # National level
    gadm.shp$full_name <- "National"
    res.to.plot$region.name <- "National"
    
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    #adm0 not work
  } else {
    # Subnational levels
    if (by.adm1 == by.adm2) {
      gadm.shp$full_name <- gadm.shp[[by.adm2]]
    } else {
      gadm.shp$full_name <- paste0(gadm.shp[[by.adm1]], "_", gadm.shp[[by.adm2]])
    }
    
    if (!is.null(admin1.focus)) {
      gadm.shp <- gadm.shp[gadm.shp[[by.adm1]] == admin1.focus, ]
      res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus, ]
    }
    
    gadm.with.res <- gadm.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
    
    gadm.with.res$region.name <- gadm.with.res[[by.adm2]]
    gadm.with.res$upper.adm.name <- gadm.with.res[[by.adm1]]
  }
  
  
  
  
  ### modify the format of numeric values and add warning messages
  gadm.with.res$warnings <- NA
  
  ### problematic sd, warnings and set uncertainty related measure to NA
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(!is.na(mean) &is.na(sd),
                                            "Data in this region are insufficient for <br/> reliable estimates with the current method.",
                                            NA)
    )
  
  ### no data warning
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(is.na(mean),
                                            "No data in this region",
                                            warnings))
  
  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  
  ### cv to %
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(cv = sprintf("%.1f%%", cv * 100))
  
  
  if(value.to.plot=='exceed_prob'){
    gadm.with.res <- gadm.with.res %>%
      dplyr::mutate(exceed_prob = sprintf("%.1f%%", exceed_prob * 100))
  }
  
  ### formatting numeric variables to 2 decimal places
  
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(across(c(mean, var, lower, upper,CI.width), ~sprintf("%.2f", .)))
  
  
  ########################################################
  ### hatching for problematic sd
  ########################################################
  
  hatching.ind <- T
  
  hatching.gadm <- gadm.with.res %>%
    subset( is.na(sd) & (!is.na(value)))
  
  ### no hatching if all regions have reasonable sd or manually set
  if(dim(hatching.gadm)[1]==0 | (no.hatching)){
    hatching.ind <- F
  }else{
    
    ### setup hatching polygons
    hatching.regions <- hatched.SpatialPolygons(hatching.gadm,
                                                density = c(hatching.density), angle = c(45))
    
    ### setup hatching legend
    warning.icon <- leaflet::awesomeIconList(
      'Sparse Data' =leaflet::makeAwesomeIcon(icon = "align-justify", library = "glyphicon",
                                              iconColor = 'gray',
                                              markerColor = 'white',
                                              squareMarker = TRUE, iconRotate = 135)
    )
  }
  
  #############################################
  ### parameters for color scale and breaks
  #############################################
  
  ### determine color palette for statistics, if not pre-specified
  
  if(is.null(color.palette)){
    color.palette='viridis'
    if(value.to.plot==c('mean')){
      color.palette = 'viridis'
    }
    if(value.to.plot==c('cv')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::mako(10)[2:10]
    }
    if(value.to.plot==c('CI.width')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::plasma(10)[3:10]
    }
    if(value.to.plot==c('exceed_prob')){
      #color.palette = 'cividis'
      #color.palette = viridisLite::cividis(10)
      color.palette = viridisLite::rocket(10)[3:10]
    }
  }
  
  ### determine value range if not specified, also create legend data
  if(is.null(value.range)){
    
    value.range <-  gadm.with.res$value
    
    if(value.to.plot=='exceed_prob'){
      value.range <- c(-0.001,1.001)
    }
    
    ### if no range specified, use data to determine limits for color schemes
    legend.dat <- gadm.with.res
    
    if(max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T)<0.005){
      new.max <- min(1, max(gadm.with.res$value,na.rm=T)+0.005)
      new.min <- max(0, min(gadm.with.res$value,na.rm=T)-0.005)
      
      legend.dat <- data.frame(value=seq(new.min,new.max,length.out	=10),ID=c(1:10))
      
    }
    
  }else{
    ### if range specified, use range determine limits for color schemes
    legend.dat <- data.frame(value=seq(value.range[1],value.range[2],length.out	=10),
                             ID=c(1:10))
    
  }
  
  ### number of ticks on the legend
  if(is.null(num_bins)){
    if(value.to.plot=='exceed_prob'){
      num_bins <- 6
      
    }else{
      num_bins <- min(round( (max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T))/0.1),6)
      num_bins <- max(4,num_bins)
    }
  }
  
  ### color palette
  if(value.to.plot=='exceed_prob'){
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 #na.color = '#9370DB',
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
    
  }else{
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
  }
  
  numberFormat = function(x) {
    prettyNum(x, format = "f", big.mark = ",", digits =
                3, scientific = FALSE)
  }
  
  if(value.to.plot %in% c('cv','exceed_prob')){
    
    numberFormat = function(x) {
      paste0(formatC(100 * x, format = 'f', digits = 1), "%")
    }
    
  }
  
  ###############################################
  ### hovering effect, information to display
  ###############################################
  
  hover_labels <- gadm.with.res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover_label = {
      label <- paste0('Region: ', region.name, '<br/>')
      
      if (!is.null(by.adm1) && !is.null(by.adm2) && by.adm1 != by.adm2) {
        label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
      }
      
      if(value.to.plot=='exceed_prob'){
        label <- paste0(label,  'Prob (prevalence > ',threshold.p,') = ', exceed_prob, '<br/>')
      }
      
      label <- paste0(label,
                      'Mean (95% CI): ', mean, ' (', lower, ', ', upper, ')', '<br/>',
                      'Coefficient of Variation: ', cv, '<br/>')
      if (!is.na(warnings) && warnings != "") {
        label <- paste0(label, '<span style="color: red;">Warning: ', warnings, '</span><br/>')
      }
      htmltools::HTML(label)  # Ensure that HTML rendering is applied
    }) %>%
    dplyr::ungroup() %>%
    dplyr::pull(hover_label)
  
  
  
  ###############################################
  ### assemble
  ###############################################
  
  ### base map
  adm.map <- gadm.with.res  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1))
  
  adm.map <- add_basemap(original.map=adm.map,
                         static.ind= F,
                         basemap.type =use.basemap)
  
  #if(use.basemap=='OSM'){ adm.map <- adm.map %>%  leaflet::addTiles()}
  
  
  
  adm.map <- adm.map %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
      weight = 1,
      color = "gray",
      fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "15px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )
  
  ### add legend
  missingLabel <- ifelse(value.to.plot=='mean', 'No Data', 'N/A')
  
  adm.map <- adm.map %>%
    leaflegend::addLegendNumeric(pal = pal.legend, values = ~value, title =  htmltools::HTML(legend.label),
                                 orientation = 'vertical', fillOpacity = .7,
                                 position = 'bottomright', group = 'Symbols',
                                 width=25,height=150,naLabel = missingLabel,
                                 data=legend.dat,
                                 bins = num_bins, # Custom tick positions
                                 numberFormat=numberFormat,
                                 decreasing=T
    )
  
  if(hatching.ind){
    
    adm.map <- adm.map %>% leaflet::addPolylines(
      data = hatching.regions,
      color = c( "gray"),
      weight = 2.0,
      opacity = 0.8
    )
    adm.map <- adm.map %>% leaflegend::addLegendAwesomeIcon(iconSet = warning.icon,
                                                            title = 'Interpret with caution:',
                                                            position = 'bottomright')
    
  }
  
  ### add title
  
  if(!is.null(map.title)){
    
    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.65);
    font-weight: bold;
    font-size: 20px;
    }
    "))
    
    title <- tags$div(
      tag.map.title, HTML(paste0(map.title))
    )
    
    adm.map <- adm.map %>%
      leaflet::addControl(title, position = "topleft", className="map-title")
  }
  
  return(adm.map)
}