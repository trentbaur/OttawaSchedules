months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

LoadSkating <- function() {

      library('RCurl')
      library('XML')
      library('lubridate')
      
      fileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_skating_results_en.xsl&sq_event=Skating&sq_lang=en&sort=location+asc%2CdayNo+asc%2Cstart_date+asc%2Cstart_time+asc%2Csession_type+asc&ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268948&notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_skating_en.xsl&sq_location=&sq_session_type=&sq_keywords1=&"
      nextFileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_skating_results_en.xsl;sq_event=Skating;sq_lang=en;sort=location%20asc%2CdayNo%20asc%2Cstart_date%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268948;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_skating_en.xsl;sq_location=;sq_session_type=;sq_keywords1=;start="
      
      doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
#      doc <- xmlTreeParse(paste(nextFileUrl, "125", sep=""), useInternal = TRUE)
      
      rootNode <- xmlRoot(doc)
      
      master_skating <- data.frame()
      
      startwith <- c(0)
      
      #     Determine number of results
      numFound <- as.integer(xpathSApply(rootNode, '//result', xmlGetAttr, "numFound"))
      
      
      while(startwith < numFound) {
      
            result_page <- do.call(rbind, xpathApply(doc, "/response/result/doc", function(node) {
                  
                  xp <- './str[@name="id"]'
                  id <- xpathSApply(node, xp, xmlValue)
                  if (is.null(id)) id <- NA
                  
                  xp <- './arr[@name="location"]'
                  location <- xpathSApply(node, xp, xmlValue)
                  if (is.null(location)) location <- NA
                  
                  xp <- './str[@name="location_map"]'
                  location_map <- xpathSApply(node, xp, xmlValue)
                  if (is.null(location_map)) location_map <- NA
                  
                  xp <- './arr[@name="day"]'
                  day <- xpathSApply(node, xp, xmlValue)
                  if (is.null(day)) day <- NA
                  
                  xp <- './date[@name="start_date"]'
                  start_date <- xpathSApply(node, xp, xmlValue)
                  if (is.null(start_date)) start_date <- NA
                  
                  xp <- './date[@name="end_date"]'
                  end_date <- xpathSApply(node, xp, xmlValue)
                  if (is.null(end_date)) end_date <- NA
                  
                  xp <- './str[@name="start_time"]'
                  starttime <- xpathSApply(node, xp, xmlValue)
                  if (is.null(starttime)) starttime <- NA
                  
                  xp <- './str[@name="end_time"]'
                  endtime <- xpathSApply(node, xp, xmlValue)
                  if (is.null(endtime)) endtime <- NA
                  
                  xp <- './arr[@name="session_type"]'
                  session_type <- xpathSApply(node, xp, xmlValue)
                  if (is.null(session_type)) session_type <- NA
                  
                  #     To handle missing comments, append an 'x' on the end
                  #     of the retrieved value.
                  #     This was the only way I could find that worked
                  xp <- './arr[@name="comments"]'
                  comments <- xpathSApply(node, xp, xmlValue)
                  comments <- paste(comments, 'x')
                  
                  #     Join everything together into a single data.frame
                  data.frame(id, location, day, start_date, end_date, starttime, endtime, session_type, comments, location_map, stringsAsFactors = FALSE)
                  
            }))
            
            #     Copy page of results into master data frame
            master_skating <- rbind(master_skating, result_page)      
            
            
      
            #     Move to the next set of results
            #     This is based on the assumption that the web site returns
            #     exactly 25 records
            startwith <- startwith + 25
            
            nextFileUrl <- paste("http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_skating_results_en.xsl;sq_event=Skating;sq_lang=en;sort=location%20asc%2CdayNo%20asc%2Cstart_date%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268948;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_skating_en.xsl;sq_location=;sq_session_type=;sq_keywords1=;start=", startwith, sep="")
            
            doc <- xmlTreeParse(nextFileUrl, useInternal = TRUE)
            
            rootNode <- xmlRoot(doc)
      
      }
      
      colnames(master_skating) <- c('ID', 'Arena', 'Day', 'StartDate', 'EndDate', 'StartTime', 'EndTime', 'SessionType', 'Comments', 'LocationMap')
      
      master_skating
}


LoadSwimming <- function() {
      
      library('RCurl')
      library('XML')
      library('lubridate')
      
      fileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl&notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl&notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl&sq_event=Swimming&sq_lang=en&sort=location+asc%2Cstart_date+asc%2CdayNo+asc%2Cstart_time+asc%2Csession_type+asc&ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957&sq_location=&sq_session_type=&sq_keywords1=&"
                  #http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl&sq_event=Swimming&sq_lang=en&sort=location+asc%2Cstart_date+asc%2CdayNo+asc%2Cstart_time+asc%2Csession_type+asc&ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957&sq_location=&sq_session_type=&sq_keywords1=&
            
      nextFileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;sq_event=Swimming;sq_lang=en;sort=location%20asc%2Cstart_date%20asc%2CdayNo%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957;sq_location=;sq_session_type=;sq_keywords1=;start="
      
      doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
      #      doc <- xmlTreeParse(paste(nextFileUrl, "125", sep=""), useInternal = TRUE)
      
      rootNode <- xmlRoot(doc)
      
      master_swimming <- data.frame()
      
      startwith <- c(0)
      
      #     Determine number of results
      numFound <- as.integer(xpathSApply(rootNode, '//result', xmlGetAttr, "numFound"))
      
      while(startwith < numFound) {
            
            result_page <- do.call(rbind, xpathApply(doc, "/response/result/doc", function(node) {
                  
                  xp <- './str[@name="id"]'
                  id <- xpathSApply(node, xp, xmlValue)
                  if (is.null(id)) id <- NA
                  
                  xp <- './arr[@name="location"]'
                  location <- xpathSApply(node, xp, xmlValue)
                  if (is.null(location)) location <- NA
                  
                  xp <- './str[@name="location_map"]'
                  location_map <- xpathSApply(node, xp, xmlValue)
                  if (is.null(location_map)) location_map <- NA
                  
                  xp <- './arr[@name="day"]'
                  day <- xpathSApply(node, xp, xmlValue)
                  if (is.null(day)) day <- NA
                  
                  xp <- './date[@name="start_date"]'
                  start_date <- xpathSApply(node, xp, xmlValue)
                  if (is.null(start_date)) start_date <- NA
                  
                  xp <- './date[@name="end_date"]'
                  end_date <- xpathSApply(node, xp, xmlValue)
                  if (is.null(end_date)) end_date <- NA
                  
                  xp <- './str[@name="start_time"]'
                  starttime <- xpathSApply(node, xp, xmlValue)
                  if (is.null(starttime)) starttime <- NA
                  
                  xp <- './str[@name="end_time"]'
                  endtime <- xpathSApply(node, xp, xmlValue)
                  if (is.null(endtime)) endtime <- NA
                  
                  xp <- './arr[@name="session_type"]'
                  session_type <- xpathSApply(node, xp, xmlValue)
                  if (is.null(session_type)) session_type <- NA
                  
                  #     To handle missing comments, append an 'x' on the end
                  #     of the retrieved value.
                  #     This was the only way I could find that worked
                  xp <- './arr[@name="comments"]'
                  comments <- xpathSApply(node, xp, xmlValue)
                  comments <- paste(comments, 'x')
                  
                  xp <- './arr[@name="comments_special"]'
                  comments_special <- xpathSApply(node, xp, xmlValue)
                  comments_special <- paste(comments_special, 'x')
                  
                  #     Join everything together into a single data.frame
                  data.frame(id, location, day, start_date, end_date, starttime, endtime, session_type, comments, comments_special, location_map, stringsAsFactors = FALSE)
                  
            }))
            
            #     Copy page of results into master data frame
            master_swimming <- rbind(master_swimming, result_page)      
            
            
            
            #     Move to the next set of results
            #     This is based on the assumption that the web site returns
            #     exactly 25 records
            startwith <- startwith + 25
            
            nextFileUrl <- paste("http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;sq_event=Swimming;sq_lang=en;sort=location%20asc%2Cstart_date%20asc%2CdayNo%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957;sq_location=;sq_session_type=;sq_keywords1=;start=", startwith, sep="")
            
            doc <- xmlTreeParse(nextFileUrl, useInternal = TRUE)
            
            rootNode <- xmlRoot(doc)
      }
      
      colnames(master_swimming) <- c('ID', 'Arena', 'Day', 'StartDate', 'EndDate', 'StartTime', 'EndTime', 'SessionType', 'Comments', 'Special', 'LocationMap')
      
      master_swimming
}

FormatSkating <- function(master_results) {
      formatted <- master_results     
      
      formatted$StartDate <- as.Date(formatted$StartDate)
      formatted$EndDate <- as.Date(formatted$EndDate)

      #formatted$StartTime <- strptime(formatted$StartTime, format = "%H:%M")
      #formatted$EndTime <- strptime(formatted$EndTime, format = "%H:%M")
      

#       #     Re-orient Start and End date to match day of week
#       formatted[formatted$Day != weekdays(formatted$StartDate),]
      
      formatted$Latitude <- as.numeric(gsub(".*LAT=(.*)&LON.*", "\\1",  formatted$LocationMap))
      formatted$Longitude <- as.numeric(gsub(".*LON=(.*)&feat.*", "\\1",  formatted$LocationMap))

      #     Add Johnny Leroux Longitude and Latitude (Missing from XML)
      formatted[formatted$Arena == 'Johnny Leroux Arena (Stittsville)', ]$Latitude <- 45.260748
      formatted[formatted$Arena == 'Johnny Leroux Arena (Stittsville)', ]$Longitude <- -75.925043

      #     Exclude the LocationMap field from result
      formatted[,-which(names(formatted) %in% c('LocationMap'))]

      # Clean up SessionType names
      formatted[formatted$SessionType == '50+ Skating',c('SessionType')] = '50+'
      formatted[formatted$SessionType == 'Adult Skating',c('SessionType')] = 'Adult'
      formatted[formatted$SessionType == 'Family Skating',c('SessionType')] = 'Family'
      formatted[formatted$SessionType == 'Public Skating',c('SessionType')] = 'Public'
      formatted[formatted$SessionType == 'Unsupervised Skate',c('SessionType')] = 'Unsupervised'

      formatted[formatted$SessionType == 'Pick-Up Hockey - 18+',c('SessionType')] = 'Pickup 18+'
      formatted[formatted$SessionType == 'Pick-Up Hockey - 35+',c('SessionType')] = 'Pickup 35+'
      formatted[formatted$SessionType == 'Pick-Up Hockey - 50+',c('SessionType')] = 'Pickup 50+'

      formatted[formatted$SessionType == 'Shinny Hockey - Children',c('SessionType')] = 'Shinny - Children'
      formatted[formatted$SessionType == 'Shinny Hockey - Youth',c('SessionType')] = 'Shinny - Youth'

      formatted[formatted$SessionType == 'Figure Skate',c('SessionType')] = 'Figure'
      formatted[formatted$SessionType == 'Speed Skating',c('SessionType')] = 'Speed'
      

      # Categorize Session types
      formatted$SessionGroup <- c('')
      formatted[formatted$SessionType %in% c('50+', 'Adult', 'Family', 'Public', 'Unsupervised'), ]$SessionGroup <- 'Skating'
      formatted[formatted$SessionType %in% c('Pickup 18+', 'Pickup 35+', 'Pickup 50+'), ]$SessionGroup <- 'Hockey'
      formatted[formatted$SessionType %in% c('Shinny - Children', 'Shinny - Youth'), ]$SessionGroup <- 'Shinny'
      formatted[formatted$SessionType %in% c('Figure', 'Speed'), ]$SessionGroup <- 'Specialized'

      # Categorize Rinks
      formatted$Locale <- c('')
      
      formatted[formatted$Arena == 'Barbara Ann Scott (Pinecrest Recreation Complex)', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Bell Centennial Arena', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Belltown Dome', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Bernard-Grandmaître Arena', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Blackburn Arena', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Bob MacQuarrie Recreation Complex - Orléans', ]$Locale <- 'East'
#formatted[formatted$Arena == 'Brewer Arena', ]$Locale <- 'Central'
formatted[ ,"Locale"] <- ifelse(formatted[ , "Arena"] == "Brewer Arena", "Central", formatted[ ,"Locale"])
      formatted[formatted$Arena == 'Brian Kilrea Arena', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Earl Armstrong Arena', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Fred Barrett', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Goulbourn Recreation Complex', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Howard Darwin Centennial Arena', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'J. Alph Dulude Arena', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Jack Charron Arena', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Jim Durrell Complex', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'John G. Mlacak Centre', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Johnny Leroux Arena (Stittsville)', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Kanata Recreation Complex', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Larry Robinson Arena', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Manotick Arena', ]$Locale <- 'South'
      formatted[formatted$Arena == 'McNabb Arena', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Minto Recreation Complex', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Navan Memorial Centre', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Nepean Sportsplex', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'R. J. Kennedy Arena', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Ray Friel Recreation Complex', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Richmond Arena', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Sandy Hill Arena', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'St-Laurent Complex', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Stuart Holmes (Osgoode) Arena', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Tom Brown Arena', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'W. Erskine Johnston Arena', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Walter Baker Sports Centre', ]$Locale <- 'West'

      formatted$Arena <- gsub('( Arena)|( Recreation Complex)', '', formatted$Arena)

      formatted
}

FormatSwimming <- function(master_results) {
      formatted <- master_results     

      #     Process dates
      formatted$StartDate <- as.Date(formatted$StartDate)
      formatted$EndDate <- as.Date(formatted$EndDate)
      
      #     Process coordinates
      formatted$Latitude <- as.numeric(gsub(".*LAT=(.*)&LON.*", "\\1",  formatted$LocationMap))
      formatted$Longitude <- as.numeric(gsub(".*LON=(.*)&feat.*", "\\1",  formatted$LocationMap))

      #     Remove pools that are closed for annual maintenance
      formatted <- formatted[grep('closed for annual', formatted$Special, invert = TRUE), ]
      
      
      #     Exclude the LocationMap field from result
      formatted[,-which(names(formatted) %in% c('LocationMap'))]
      
      # Clean up SessionType names
      formatted[formatted$SessionType == 'Lane Swim', c('SessionType')] = 'Lane'
      formatted[formatted$SessionType == '50m Lane Swim', c('SessionType')] = '50m Lane'

      formatted[formatted$SessionType == 'Family Swim', c('SessionType')] = 'Family'
      formatted[formatted$SessionType == 'Public Swim', c('SessionType')] = 'Public'
      formatted[formatted$SessionType == 'Wave Swim', c('SessionType')] = 'Wave'
      formatted[formatted$SessionType == '50+ Vitality', c('SessionType')] = '50+ Vitality'
      formatted[formatted$SessionType == '50+ Swim', c('SessionType')] = '50+'
      formatted[formatted$SessionType == 'Teen Swim', c('SessionType')] = 'Teen'

      formatted[formatted$SessionType == 'Women Only',c('SessionType')] = 'Women Only'
      formatted[formatted$SessionType == 'Women Only Family',c('SessionType')] = 'Women Only Family'
      formatted[formatted$SessionType == 'Swim for Persons with a Disability',c('SessionType')] = 'With Disability'
      formatted[formatted$SessionType == 'Preschool Swim',c('SessionType')] = 'Preschool'
      
      
      # Categorize Session types
      formatted$SessionGroup <- c('')
      formatted[formatted$SessionType %in% c('Lane', '50m Lane'), ]$SessionGroup <- 'Lane'
      formatted[formatted$SessionType %in% c('Family', 'Public', 'Wave'), ]$SessionGroup <- 'General'
      formatted[formatted$SessionType %in% c('50+ Vitality', '50+', 'Teen', 'Preschool'), ]$SessionGroup <- 'Age Restricted'
      formatted[formatted$SessionType %in% c('Women Only', 'Women Only Family'), ]$SessionGroup <- 'Gender Restricted'
      formatted[formatted$SessionType %in% c('With Disability'), ]$SessionGroup <- 'With Disability'


      # Categorize Pools
      formatted$Locale <- c('')
      
      formatted[formatted$Arena == 'Bob MacQuarrie - Orléans Pool', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Brewer Arena', ]$Locale <- 'Central'
      formatted[ ,"Locale"] <- ifelse(formatted[ , "Arena"] == "Brewer Pool", "Central", formatted[ ,"Locale"])
      formatted[formatted$Arena == 'Canterbury Pool', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Champagne Pool', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Deborah Anne Kirwan Pool', ]$Locale <- 'South'
      formatted[formatted$Arena == 'François Dupuis Pool', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Goulbourn Pool', ]$Locale <- 'West'
formatted[ ,"Locale"] <- ifelse(formatted[ , "Arena"] == "Goulbourn Pool", "West", formatted[ ,"Locale"])
      formatted[formatted$Arena == 'Jack Purcell Pool', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Kanata Wave Pool', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Lowertown Pool', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Minto Recreation Complex - Barrhaven', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Nepean Sportsplex Pools', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Pinecrest Pool', ]$Locale <- 'West End'
      formatted[formatted$Arena == 'Plant Pool', ]$Locale <- 'Central'
      formatted[formatted$Arena == 'Ray Friel Wave Pool', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Richcraft Recreation Complex - Kanata', ]$Locale <- 'West'
      formatted[formatted$Arena == 'Sawmill Creek Pool', ]$Locale <- 'South'
      formatted[formatted$Arena == 'Splash Wave Pool', ]$Locale <- 'East'
      formatted[formatted$Arena == 'St-Laurent Pool', ]$Locale <- 'East'
      formatted[formatted$Arena == 'Walter Baker Pool', ]$Locale <- 'West'
      
      #formatted[formatted$Arena == 'Bearbrook (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Genest (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Beaverbrook (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Corkstown (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Crestview (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Entrance (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'General Burns (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Glen Cairn (Outdoor)', ]$Locale <- 'East'
      #formatted[formatted$Arena == 'Katimavik (Outdoor)', ]$Locale <- 'East'

      formatted$Arena <- gsub('( Pools)|( Pool)|( Recreation Complex)', '', formatted$Arena)

      formatted
}

ParseCancellations <- function(x) {
      all_cancels <- data.frame()
      
      for(m in months) {
            can <- str_extract(x["Comments"], paste(m, " [0-9, ]+", sep=""))
            
            if(!is.na(can)) {
                  cancel <- str_extract_all(can, "[0-9]+")
                  
                  canceldates <- as.Date(paste('2014-', m, '-', cancel[[1]], sep=''), format = "%Y-%b-%d")
                  
                  #     Join everything together into a single data.frame
                  for(d in canceldates[[1]]) {                        
                        all_cancels <- rbind(all_cancels, data.frame(x["ID"], x["StartDate"], as.Date(d, origin="1970-01-01"), stringsAsFactors = FALSE))
                  }
                  
            }
      }
      
      if (nrow(all_cancels) > 0) {
            colnames(all_cancels) <- c('ID', 'Start', 'CancelDate')
            
            all_cancels$CancelDate <- as.Date(ifelse(all_cancels$CancelDate < all_cancels$Start, all_cancels$CancelDate + years(1), all_cancels$CancelDate), origin = '1970/1/1')

            all_cancels      
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
      #install.packages("stringr")
      library(stringr)
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

      filtered <- merge(filtered, cancellations, by.x = c("ID", "Date"), by.y = c("ID", "CancelDate"), all.x = TRUE)
            
      #     Join time to dates
      filtered$StartDateTime <- as.POSIXlt(paste(as.character(filtered$Date),  filtered$StartTime))
      filtered$EndDateTime <- as.POSIXlt(paste(as.character(filtered$Date),  filtered$EndTime))
      
      #     Return filtered dates without cancellations
      filtered[is.na(filtered$Start), ]
}


SkateDriver <- function() {
      setwd("D:/Projects/Skating")
      
      #     Avoid loading the data multiple times from the website
      if (!exists('master_skating')) {
            master_skating <<- LoadSkating()            
      }
            
      format_skate <<- FormatSkating(master_skating)
      
      schdays_skate <<- TransformData(format_skate)
      
      write.csv(schdays_skate[,c('Date', 'StartDateTime', 'EndDateTime', 'Arena', 'Locale', 'SessionType', 'SessionGroup', 'Latitude', 'Longitude')], file="skating.csv")
}



SkateDriver()


SwimDriver <- function() {
      setwd("D:/Projects/Skating")
      
      #     Avoid loading the data multiple times from the website
      if (!exists('master_swimming')) {
            master_swimming <<- LoadSwimming()
      }
      
      format_swim <<- FormatSwimming(master_swimming)
      
      schdays_swim <<- TransformData(format_swim)
      
      write.csv(schdays_swim[,c('Date', 'StartDateTime', 'EndDateTime', 'Arena', 'Locale', 'SessionType', 'SessionGroup', 'Latitude', 'Longitude')], file="swimming.csv")
}

SwimDriver()








paste('Dec', str_extract_all("Dec 24, 31 ", "[0-9]+"))

nextFileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_skating_results_en.xsl;sq_event=Skating;sq_lang=en;sort=location%20asc%2CdayNo%20asc%2Cstart_date%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268948;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_skating_en.xsl;sq_location=;sq_session_type=;sq_keywords1=;start=100"

doc <- xmlTreeParse(nextFileUrl, useInternal = TRUE)



