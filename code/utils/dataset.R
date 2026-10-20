## HELPER FUNCTIONS FOR TIME CONVERSION ----------------------------------------
generateHour <- function() {
  
  h <- correctHour(as.character(sample(seq(from = 0, to = 23, by = 1), 1)))
  m <- correctHour(as.character(sample(seq(from = 0, to = 59, by = 1), 1)))
  
  return (paste0(h, ":", m))
}

correctHour <- function(x){
  
  if(nchar(x) == 1){
    x <- paste0("0", x)
  }
  
  return (x)
}

filterHolidays <- function(data, from, to){
  
  from_year <- as.integer(from)
  to_year   <- as.integer(to)
  
  holidayIT <- holiday(year = from_year:to_year, Holiday = "EasterSunday")
  holidayIT <- c(holidayIT, holiday(year = from_year:to_year, Holiday = "EasterMonday"))  
  
  for(y in from_year:to_year){
    holidayIT <- c(holidayIT, as.timeDate(paste0(y, "-01-01")),
                   as.timeDate(paste0(y, "-01-06")),
                   as.timeDate(paste0(y, "-04-25")),
                   as.timeDate(paste0(y, "-05-01")),
                   as.timeDate(paste0(y, "-06-02")),
                   as.timeDate(paste0(y, "-08-15")),
                   as.timeDate(paste0(y, "-11-01")),
                   as.timeDate(paste0(y, "-12-08")),
                   as.timeDate(paste0(y, "-12-25")),
                   as.timeDate(paste0(y, "-12-26")))
  }
  
  idxs <- which(isHoliday(x = as.timeDate(as.Date(data$DateHour)), holidays = holidayIT) & 
                  data$Year %in% from_year:to_year)

  return(idxs)
  
}

filterWorkdays <- function(data, from, to){
  
  holiday_idxs <- filterHolidays(data, from, to)
  
  from_year <- as.integer(from)
  to_year   <- as.integer(to)
  
  rows <- seq_len(nrow(data))
  
  idxs <- rows[!(rows %in% holiday_idxs) &
                 data$Year %in% from_year:to_year]
    
  return(idxs)
}

## DATASET ---------------------------------------------------------------------
readDataset <- function(municipality = "Rome", years = NULL){
  
  foldername <- paste0(getwd(), "/data/", municipality, "/")

  if(!file.exists(paste0(foldername, municipality, "_dataset.RData"))){
    
    variables <- c("ID", "Longitude", "Latitude", "DateHour", "Year", "Month", "Day",
                   "DayType", "Hour", "AccidentType", "Uninjured",
                   "Injuries", "Deaths", "Pedestrians", "Bicycles",
                   "Motorcycles", "HeavyVehicles", "RoadSurface", "Weather",
                   "Traffic", "Visibility")

    dataset <- data.frame(matrix(ncol = length(variables), nrow = 1))
    names(dataset) <- variables
    dataset$DateHour <- as.POSIXct(NA,"")
    
    if(municipality == "Rome"){
      
      if(is.null(years)) years <- c(2006, 2022)
      if(length(years == 1)) years <- rep(years, 2)
      
      pb <- txtProgressBar(min = 1, max = (years[2] - years[1] + 1)*12, style = 3, char = "=")
        
      for(y in seq(from = years[1], to = years[2], by = 1)){
        
        for(m in 1:12){
          
          if(nchar(as.character(m)) == 1){
            m <- paste0("0", m)
          }
          
          filename <- paste0(foldername, y, "/road_accidents_", municipality, "_", y, "_", m, ".csv")
          
          if(file.exists(filename)){
            
            # reading the data
            data <- read.csv(filename, header = TRUE, sep = ";", quote = "")
            
            if(sum(names(data) == "Longitude")){
              
              names(data)[which(names(data) == "Longitude")] <- "Longitudine"
              
            }
            
            if(sum(names(data) == "Latitude")){
              
              names(data)[which(names(data) == "Latitude")] <- "Latitudine"
              
            }
            
            # selecting only the variables of interest
            data <- data %>%
              select("Protocollo", "DataOraIncidente", "Longitudine", "Latitudine",
                     "NaturaIncidente", "NUM_FERITI", "NUM_MORTI", "NUM_ILLESI",
                     "FondoStradale", "CondizioneAtmosferica", "Traffico", "Visibilita",
                     "TipoVeicolo")
            
            # reassigning variable names
            names(data) <- c("ID", "DateHour", "Longitude", "Latitude",
                             "AccidentType", "Injuries", "Deaths", "Uninjured",
                             "RoadSurface", "Weather", "Traffic", "Visibility",
                             "VehicleType")
            
            data$Bicycles <- "false"
            data$Motorcycles <- "false"
            data$HeavyVehicles <- "false"
            data$Pedestrians <- "false"
            
            for(i in unique(data$ID)){
              for(j in data$VehicleType[which(data$ID == i)]){
                if(j == "Micromobilit\xe0 elettrica - Hoverboard" || j == "Bicicletta elettrica" ||
                   j == "Monopattino elettrico" || j == "Velocipede"){
                  data$Bicycles[which(data$ID == i)] <- "true"
                }
                if(j == "Ciclomotore" || j == "Ciclomotore con passeggero" ||
                   j == "Motociclo a solo" || j == "Motociclo con passeggero" ||
                   j == "Motociclo di polizia" || j == "Quadriciclo" ||
                   j == "Quadriciclo leggero"){
                  data$Motorcycles[which(data$ID == i)] <- "true"
                }
                if(j == "Autoarticolato" || j == "Autobus di linea" ||
                   j == "Autobus in extraurbana" || j == "Autobus turistico" ||
                   j == "Autobus urbano" || j == "Autocarro superiore 35 q." ||
                   j == "Autogru" || j == "Autopompa" || j == "Autosnodato" ||
                   j == "Autotreno" || j == "Macchina operatrice" ||
                   j == "Motrice" || j == "Rimorchio" || j == "Trattore stradale" ||
                   j == "Veicolo speciale"){
                  data$HeavyVehicles[which(data$ID == i)] <- "true"
                }
              }
              for(k in data$AccidentType[which(data$ID == i)]){
                if(k == "Investimento di pedone"){
                  data$Pedestrians[which(data$ID == i)] <- "true"
                }
              }
            }
            
            # keeping only one row per road accident
            data <- data %>% distinct(ID, .keep_all = TRUE)
            
            data$Longitude <- ifelse(data$Longitude == "", NA, as.numeric(sub(",", ".", data$Longitude)))
            data$Latitude <- ifelse(data$Latitude == "", NA, as.numeric(sub(",", ".", data$Latitude)))
            
            # discarding NAs (in longitudes and latitudes)
            data <- data %>%
              filter(!is.na(Longitude), !is.na(Latitude))
            
            if(nrow(data) > 0){
              
              # correcting coordinates
              for(i in 1:nrow(data)){
                
                n <- floor(log10(abs(data$Longitude[i]))) + 1
                data$Longitude[i] <- data$Longitude[i] / 10^(n-2)
                
                n <- floor(log10(abs(data$Latitude[i]))) + 1
                data$Latitude[i] <- data$Latitude[i] / 10^(n-2)
                
              }
              
              # converting time
              tmp <- data$DateHour
              data <- data[, -which(names(data) == "DateHour")]
              
              DateHour <- as.POSIXct(NA, "")
              for(i in 1:nrow(data)){
                
                d <- as.POSIXct(tmp[i], format = ifelse(nchar(as.character(tmp[i])) == 16,
                                                        "%d/%m/%Y %H:%M",
                                                        "%d/%m/%Y %H:%M:%S"))
                
                if(!is.na(d)){
                  
                  DateHour[i] <- d
                  
                } else {
                  
                  DateHour[i] <- as.POSIXct(paste0(tmp[i], " ", generateHour()),
                                            format = ifelse(nchar(as.character(paste0(tmp[i], " ", generateHour()))) == 16,
                                                            "%d/%m/%Y %H:%M",
                                                            "%d/%m/%Y %H:%M:%S"))
                  
                }
              }
              
              data$DateHour <- as.POSIXct(NA,"")
              data$DateHour <- DateHour
              
              rm(DateHour, tmp, d)
              
              # saving the current locale
              original_locale <- Sys.getlocale("LC_TIME")
              
              # setting the locale to English
              invisible(capture.output(Sys.setlocale("LC_TIME", "en_US.UTF-8")))
              
              # getting Day, DayType, Month, Year
              data$Year <- as.integer(format(data$DateHour, "%Y"))
              data$Month <- months(data$DateHour, abbreviate = TRUE)
              data$Day <- weekdays(data$DateHour, abbreviate = TRUE)
              data$DayType <- ifelse(data$Day != "Sat" & data$Day != "Sun", "weekday", "weekend")
              data$Hour <- format(data$DateHour, "%H:%M")
              
              # restoring the original locale
              invisible(capture.output(Sys.setlocale("LC_TIME", original_locale)))
              
              data <- data[,variables]
      
              dataset <- rbind(dataset, data)
              
            }
          } else {
            
            m <- as.integer(m)
            setTxtProgressBar(pb, (y-years[1])*12+m)
            next
            
          }
          
          m <- as.integer(m)

          setTxtProgressBar(pb, (y-years[1])*12+m)

        }
      } 
    } else if(municipality == "Bergamo"){
  
      filename <- paste0(foldername, "road_accidents_", municipality, ".csv")
      
      if(file.exists(filename)){
        
        # reading the data
        data <- read.csv(filename, header = TRUE, sep = ",")
        
        # selecting only the variables of interest
        data <- data %>%
          select("Protocollo", "Data", "Ora", "NaturaIncidente", "N_Illesi",
                 "N_Feriti", "N_Morti", "Pedoni", "Velocipedi",
                 "Ciclomotori_Motocicli", "Mezzi_Pesanti", "Localizzazione")
        
        # reassigning variable names
        names(data) <- c("ID", "Date", "Hour", "AccidentType", "Uninjured",
                         "Injuries", "Deaths", "Pedestrians", "Bicycles",
                         "Motorcycles", "HeavyVehicles", "Coordinates")
        
        # keeping only one row per road accident
        data <- data %>% distinct(ID, .keep_all = TRUE)
        
        # discarding NAs (in longitudes and latitudes)
        data <- na.omit(data)
        
        data$Longitude <- unlist(lapply(strsplit(gsub("[(),]", "", data$Coordinates), "\\s* "), '[', 2))
        data$Latitude <- unlist(lapply(strsplit(gsub("[(),]", "", data$Coordinates), "\\s* "), '[', 1))
        data <- data[, -which(names(data) == "Coordinates")]
        
        # converting time
        tmp <- paste(data$Date, data$Hour)
        data$DateHour <- as.POSIXct(NA, "")
        data$DateHour <- as.POSIXct(tmp, format = "%d/%m/%Y %H:%M")
  
        rm(tmp)
  
        # saving the current locale
        original_locale <- Sys.getlocale("LC_TIME")
        
        # setting the locale to English
        invisible(capture.output(Sys.setlocale("LC_TIME", "en_US.UTF-8")))
        
        # getting Day, DayType, Month, Year
        data$Year <- as.integer(format(data$DateHour, "%Y"))
        data$Month <- months(data$DateHour, abbreviate = TRUE)
        data$Day <- weekdays(data$DateHour, abbreviate = TRUE)
        data$DayType <- ifelse(data$Day != "Sat" & data$Day != "Sun", "weekday", "weekend")
        data$Hour <- format(data$DateHour, "%H:%M")
        
        # restoring the original locale
        invisible(capture.output(Sys.setlocale("LC_TIME", original_locale)))
        
        data$RoadSurface <- 0
        data$Weather <- 0
        data$Traffic <- 0
        data$Visibility <- 0
        
        data <- data[,variables]
        
        dataset <- rbind(dataset, data)
        
      }
    }
    
    dataset <- dataset[-1,]
    
    save(dataset, file = paste0(foldername, municipality, "_dataset.RData"))
    
  } else {
    
    load(paste0(foldername, municipality, "_dataset.RData"))
    
  }
  
  return (dataset)
  
}


