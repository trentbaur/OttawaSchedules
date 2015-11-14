







FormatData <- function(master_results) {
      formatted <- master_results
      
      #     Append current year to StartDate...
      formatted$StartDate <- as.Date(paste0(formatted$StartDate, ', ', format(Sys.Date(), '%Y')), format = '%A, %B %d, %Y')
      #     ...then add 1 year for any dates in the past
      formatted[formatted$StartDate < Sys.Date(),]$StartDate <- formatted[formatted$StartDate < Sys.Date(),]$StartDate + years(1)
      
      formatted$EndDate <- as.Date(formatted$EndDate)

      last_sessions <- grepl(pattern = 'Last session', x = formatted$Comments)
      
      if(sum(last_sessions)>0) {
            formatted[last_sessions,]$EndDate <- as.Date(gsub(x = formatted[last_sessions,]$Comments, pattern = '\\n            \\n Last session: ',replacement = ''), format = '%A, %B %d, %Y')
      }
      
      #----------------------------------------------------------------------------------
      #     Manually clean up Arena name for Sawmill. I expect more changes in future
      #     schdays_swim[grepl(x = schdays_swim$Arena, pattern = 'Sawmill Creek Pool & Community Centre'),]$Arena
      #----------------------------------------------------------------------------------
      #formatted[grepl(x = formatted$Arena, pattern = 'Sawmill Creek Pool & Community Centre'),]$Arena <- c('Sawmill Creek Pool & Community Centre')
            
      #----------------------------------------------------------------------------------
      #     Join to Sessions lookup to shorten SessionType and retrieve SessionGroup
      #----------------------------------------------------------------------------------
      formatted <- merge(formatted, sessions, by.x = "SessionType", by.y = "longname", all.x = T)

      formatted$SessionType <- formatted$shortname

      
      #----------------------------------------------------
      #     Join to Arenas lookup to retrieve Locale
      #----------------------------------------------------
      formatted <- merge(formatted, arenas, all.x = T)


      #---------------------------------
      #     Clean up Arena Names
      #---------------------------------
      formatted$Arena <- gsub('( Pools)|( Pool)|( Arena)|( Recreation Complex)', '', formatted$Arena)

      formatted
}


ParseCancellations <- function(x) {
      if (!grepl(x = x["Comments"], pattern = 'Last session')) {
            all_cancels <- data.frame()
            
            for(m in months) {
                  can <- str_extract_all(x["Comments"], paste(m, " [0-9, ]+", sep=""))
                  
                  if(!is.na(can)) {
                        cancel <- str_extract_all(can, "[0-9]+")
                        
                        canceldates <- as.Date(paste('2015-', m, '-', cancel[[1]], sep=''), format = "%Y-%b-%d")
                        canceldates <- as.Date(ifelse (as.Date(canceldates) < Sys.Date(), canceldates + years(1), canceldates), origin = '1970-1-1')
                        
                        #     Join everything together into a single data.frame
                        for(d in canceldates) {                        
                              all_cancels <- rbind(all_cancels,
                                                   data.frame(x["ID"], x["StartDate"], as.Date(d, origin="1970-01-01"), stringsAsFactors = FALSE))
                        }
                  }
            }
            
            if (nrow(all_cancels) > 0) {
                  colnames(all_cancels) <- c('ID', 'Start', 'CancelDate')
                  
                  all_cancels$CancelDate <- as.Date(ifelse(all_cancels$CancelDate < all_cancels$Start, all_cancels$CancelDate + years(1), all_cancels$CancelDate), origin = '1970/1/1')
                  
                  all_cancels
            }
      }
}    


GetDates <- function(to, from) {
      if (missing(from)) {
            from <- Sys.Date()
      }
      
      dates <- seq(from = from, to = to, by = "1 day")
      dates <- as.data.frame(dates)
      colnames(dates) <- c('Date')
      dates$Day <- weekdays(dates[,1])
      
      dates
}


TransformData <- function(data) {
      #     Create set of dates to be used
      #     Recreating every time we run will remove old dates that are no longer relevant
      #     This could act as a natural limiter to the data volume
      dates <- GetDates(max(data$EndDate))
      
      #     Join each schedule row to list of dates to get individual schedules
      #     Keep all ancillary fields
      all <- merge(dates, data, by = "Day")
      
      filtered <- all[all$Date >= all$StartDate & all$Date <= all$EndDate, ]
      
      #     Remove cancellations
      cancellations <<- do.call(rbind, apply(data, 1, ParseCancellations))
      cancellations$ID <- as.integer(cancellations$ID)

      filtered <- merge(filtered, cancellations, by.x = c("ID", "Date"), by.y = c("ID", "CancelDate"), all.x = T)

      #     Join time to dates
      filtered$StartDateTime <- as.POSIXlt(as.character(paste(as.character(filtered$Date),  filtered$StartTime)), "%Y-%m-%d %I:%M %p ", tz="EST")
      filtered$EndDateTime <- as.POSIXlt(as.character(paste(as.character(filtered$Date),  filtered$EndTime)), "%Y-%m-%d %I:%M %p ", tz="EST")

      #     Return filtered dates without cancellations
      filtered[is.na(filtered$Start), ]
}


#-------------------------------------------------------------------------------
#     The drivers could be consolidated somewhat but it's easier keeping
#     the variable names separate for debugging purposes.
#     They will also start to diverge more once non-city schedules are added
#     to the dataset
#-------------------------------------------------------------------------------
SkateDriver <- function() {
      #     Avoid loading the data multiple times from the website
      if (!exists('master_skating')) {
            master_skating <<- LoadDataNew()
      }
            
      format_skate <<- FormatData(master_skating)
      
      schdays_skate <<- TransformData(format_skate)
      
      write.csv(schdays_skate[,c('Date', 'StartDateTime', 'EndDateTime', 'Arena', 'Locale', 'SessionType', 'SessionGroup', 'Latitude', 'Longitude')], file="skating.csv")
}


SwimDriver <- function() { 
      #-----------------------------------------------------------------
      #     Avoid loading the data multiple times from the website
      #     Force a reload by dropping master_swimming
      #-----------------------------------------------------------------
      if (!exists('master_swimming')) {
            master_swimming <<- LoadDataNew('swim')
      }
      

#       #----------------------------------------------
#       #     Load Dovercourt data - Purge data first
#       #----------------------------------------------
#       master_swimming <<- master_swimming[master_swimming$Arena != 'Dovercourt', ]
#       
#       dovercourt <<- as.data.frame(read.csv('Dovercourt.csv', stringsAsFactor=F))
#       
#       master_swimming <<- rbind(master_swimming, dovercourt)
#       
# 
#       #-------------------------------------------
#       #     Load YMCA data - Purge data first
#       #-------------------------------------------
#       master_swimming <<- master_swimming[which (! master_swimming$Arena %in% arenas[grep('Y-', arenas$Arena), c('Arena')]), ]
#       
#       ymca <<- as.data.frame(read.csv('YMCA.csv', stringsAsFactor=F))
#       
#       master_swimming <<- rbind(master_swimming, ymca)
      
      
      #     Process Swim Data
      format_swim <<- FormatData(master_swimming)
      
      schdays_swim <<- TransformData(format_swim)
      
      write.csv(schdays_swim[,c('Date', 'StartDateTime', 'EndDateTime', 'Arena', 'Locale', 'SessionType', 'SessionGroup', 'Latitude', 'Longitude')], file="swimming.csv")
}


SkateDriver()

View(schdays_skate)


master_skating[,-9]
master_skating[master_skating$Arena=='Jim Durrell Recreation Centre' & master_skating$SessionType=='Adult Skating',-9]


SwimDriver()

#rm(master_swimming)
#rm(master_skating)

View(schdays_skate)
View(schdays_swim)




schdays_skate[is.na(schdays_skate$SessionType),]

schdays_skate[is.na(schdays_skate$Arena),]

schdays_skate[is.na(schdays_skate$Longitude),]


schdays_swim[is.na(schdays_swim$SessionType),]

schdays_swim[is.na(schdays_swim$Arena),]

unique(schdays_swim[is.na(schdays_swim$Longitude),]$Arena)
