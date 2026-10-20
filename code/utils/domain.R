## FUNCTION TO GET THE ADMINISTRATIVE BOUNDARIES OF INTEREST -------------------
getAdmBdy <- function(municipality = "Rome"){
  
  adm_bdy_IT <- st_read("map/adm_bdy_IT/Com01012023/Com01012023_WGS84.shx")
  
  italian_cities_it <- c("Roma", "Milano", "Napoli", "Firenze", "Venezia",
                         "Genova", "Torino", "Padova", "Mantova", "Livorno",
                         "Siracusa")
  italian_cities_en <- c("Rome", "Milan", "Naples", "Florence", "Venice",
                         "Genoa", "Turin", "Padua", "Mantua", "Leghorn",
                         "Syracuse")
  
  if(municipality %in% italian_cities_en){
    idx <- which(italian_cities_en == municipality)
    municipality_it <- italian_cities_it[idx]
  } else {
    municipality_it <- municipality
  }
  
  adm_bdy_ <- adm_bdy_IT$geometry[which(adm_bdy_IT$COMUNE == municipality_it)]
  adm_bdy_ <- st_transform(adm_bdy_, crs = 4326)
  
  return(adm_bdy_)
  
}


## FUNCTION TO GET THE ROAD NETWORK OVER DOMAIN OF INTEREST --------------------
getDomain <- function(municipality = "Rome", bounds = NULL){
  
  foldername <- paste0(getwd(), "/domain/", municipality, "/")
    
  if(!file.exists(paste0(foldername, municipality, "_osm.RData"))){
    
    if(!dir.exists(foldername)){
      dir.create(foldername, showWarnings = FALSE)
    }
    
    adm_bdy_IT <- st_read("map/adm_bdy_IT/Com01012023/Com01012023_WGS84.shx")
    
    italian_cities_it <- c("Roma", "Milano", "Napoli", "Firenze", "Venezia",
                           "Genova", "Torino", "Padova", "Mantova", "Livorno",
                           "Siracusa")
    italian_cities_en <- c("Rome", "Milan", "Naples", "Florence", "Venice",
                           "Genoa", "Turin", "Padua", "Mantua", "Leghorn",
                           "Syracuse")
    
    if(municipality %in% italian_cities_en){
      idx <- which(italian_cities_en == municipality)
      municipality_it <- italian_cities_it[idx]
    } else {
      municipality_it <- municipality
    }
    
    adm_bdy_ <- adm_bdy_IT$geometry[which(adm_bdy_IT$COMUNE == municipality_it)]
    adm_bdy_ <- st_transform(adm_bdy_, crs = 4326)
    
    if(is.null(bounds)){
      bbox <- as.numeric(st_bbox(adm_bdy_))
      bounds <- c(bbox[1], bbox[3], bbox[2], bbox[4])
    }

    xbounds <- c(bounds[1], bounds[3])
    ybounds <- c(bounds[2], bounds[4])
    
    its_bbox <- st_bbox(c(xmin = xbounds[1], ymin = ybounds[1],
                          xmax = xbounds[2], ymax = ybounds[2]), crs = 4326) %>% st_as_sfc()
    
    # Complete Network
    # osm_data <- oe_get(municipality, boundary = its_bbox, download_directory = foldername)
    
    # unique(osm_data[[3]])
    # [1] "motorway_link"  "trunk_link"     "trunk"          "primary"        "secondary"      "tertiary"       "unclassified"   "living_street" 
    # [9] "residential"    "pedestrian"     "tertiary_link"  "cycleway"       "service"        "secondary_link" "primary_link"   "steps"         
    # [17] "footway"        NA               "track"          "path"           "services"       "construction"   "rest_area"      "elevator"      
    # [25] "proposed"
    
    # Simplified Network
    my_vectortranslate <- c(
      
      "-select", "highway",
      
      "-where", "highway IN ('motorway_link', 'trunk_link', 'trunk', 'primary',
                             'secondary', 'tertiary', 'unclassified', 'residential',
                             'tertiary_link', 'secondary_link', 'primary_link',
                             'track')"
      
    )
    
    osm_data <- oe_get(municipality, vectortranslate_options = my_vectortranslate,
                       boundary = its_bbox, download_directory = foldername)
    
    filter <- st_within(osm_data, adm_bdy_)
    osm_data <- osm_data[which(lengths(filter) != 0), ]

    save(osm_data, file = paste0(foldername, municipality, "_osm.RData"))
  
  } else {
    
    load(paste0(foldername, municipality, "_osm.RData"))
    
  }
  
  if(!is.null(bounds)){
    bbox <- st_bbox(c(xmin = bounds[1], ymin = bounds[3],
                      xmax = bounds[2], ymax = bounds[4]),
                    crs = 4326)
    osm_data <- osm_data[st_within(osm_data, st_as_sfc(bbox), sparse = FALSE), ]
  }
  
  return (osm_data)
  
}


## FUNCTION TO GET THE MESH OVER THE DOMAIN OF INTEREST ------------------------
getMesh <- function(municipality = "Rome", bounds = NULL){
  
  foldername <- paste0(getwd(), "/domain/", municipality, "/")
  
  if(is.null(bounds)){
    adm_bdy_ <- getAdmBdy(municipality = municipality)
    bbox <- as.numeric(st_bbox(adm_bdy_))
    bounds <- c(bbox[1], bbox[3], bbox[2], bbox[4])
  }
  
  filename <- paste0(foldername, municipality, "_mesh_[",
                     gsub("\\.", ",", round(bounds[1],3)), ",",
                     gsub("\\.", ",", round(bounds[2],3)), "]x[",
                     gsub("\\.", ",", round(bounds[3],3)), ",",
                     gsub("\\.", ",", round(bounds[4],3)),
                     "]", ".RData")
  
  if(!file.exists(filename)){
    
    osm_data_ <- getDomain(municipality = municipality, bounds = bounds)
    adm_bdy_ <- getAdmBdy(municipality = municipality)
    
    sf_network <- as_sfnetwork(osm_data_, directed = FALSE)
    
    # Simplify the sfnetwork
    sf_network <- sf_network %>%
      activate("edges") %>%
      filter(!edge_is_multiple()) %>%
      filter(!edge_is_loop())
    
    # Clean the sfnetwork
    sf_network <- sf_network %>% 
      convert(to_spatial_subdivision, .clean = TRUE)
    
    # Select a Full Connected Graph
    sf_network <- sf_network %>% 
      convert(to_components, .clean = TRUE, .select = 1L)

    filtered <- sf_network %>%
      activate("edges") %>%
      filter(edge_intersects(st_cast(adm_bdy_, to = "POLYGON"))) %>%
      activate("nodes") %>%
      filter(!node_is_isolated())
    
    mesh <- as.mesh.1.5D(filtered)
    
    save(mesh, file = filename)
  
  } else {
    
    load(filename)
    
  }
  
  return (mesh)
}


