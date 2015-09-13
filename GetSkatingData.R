install.packages('RCurl')
install.packages('XML')
install.packages('lubridate')
install.packages('stringr')

library('RCurl')
library('XML')
library('lubridate')
library('stringr')


setwd("D:/Projects/Skating")

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
daynames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

#---------------------------------
#     Define Session names
#---------------------------------
#rm(sessions)
sessions <- matrix(c('Skating 50+', '50+', 'Skating'
                        ,'Adult Skating', 'Adult', 'Skating'
                        ,'Family Skating', 'Family', 'Skating'
                        ,'Public Skating', 'Public', 'Skating'
                        ,'Unsupervised Skate', 'Unsupervised', 'Skating'
                        ,paste0('Pick-up Hockey ', '\u2013', ' Adults 18+'), 'Pickup 18+', 'Hockey'
                        ,paste0('Pick-up Hockey ', '\u2013', ' Adults 35+'), 'Pickup 35+', 'Hockey'
                        ,'Pick-Up Hockey - 50+', 'Pickup 50+', 'Hockey'
                        ,'Shinny Hockey - Child', 'Shinny - Children', 'Shinny'
                        ,'Shinny Hockey - Youth (13 - 17 yrs)', 'Shinny - Youth', 'Shinny'
                        ,'Figure Skating', 'Figure', 'Specialized'
                        ,'Speed Skating', 'Speed', 'Specialized'), ncol=3, byrow = T)

sessions <- rbind(sessions,
           matrix(c('Lane Swim', 'Lane', 'Lane'
                        ,'50m Lane Swim', '50m Lane', 'Lane'
                        ,'Family Swim', 'Family', 'General'
                        ,'Adult Swim', 'Adult', 'General'
                        ,'Public Swim', 'Public', 'General'
                        ,'Wave Swim', 'Wave', 'General'
                        ,'50+ Vitality', '50+ Vitality', 'Age Restricted'
                        ,'50+ Swim', '50+', 'Age Restricted'
                        ,'Teen Swim', 'Teen', 'Age Restricted'
                        ,'Women Only', 'Women Only', 'Gender Restricted'
                        ,'Women Only Family', 'Women Only Family', 'Gender Restricted'
                    
                        ,'Hot Tub / Sauna Only', 'Hot Tub/Sauna', 'Sauna'
                    
                        ,'Swim for Persons with a Disability', 'With Disability', 'With Disability'
                        ,'Preschool Swim', 'Preschool', 'Age Restricted'), ncol=3, byrow = T))

colnames(sessions) <- c('longname', 'shortname', 'SessionGroup')

sessions <- as.data.frame(sessions, stringsAsFactors = F)



#---------------------------------
#     Define Arena names
#---------------------------------
#rm(arenas)
arenas <- matrix(c('Barbara Ann Scott (Pinecrest Recreation Complex)', 'West End', '45.348021', '-75.773401'
                  , 'Bell Centennial Arena', 'West End', '45.324412', '-75.811532'
                  , 'Belltown Dome', 'West End', '45.360014', '-75.803732'
                  , 'Bernard-Grandmaître Arena', 'East', '45.432813', '-75.655395'
                  , 'Blackburn Arena', 'East', '45.43', '-75.563149'
                  , 'Bob MacQuarrie Recreation Complex - Orléans', 'East', '45.466459', '-75.545228'
                  , 'Brewer Arena', 'Central', '45.389097', '-75.691057'
                  , 'Brian Kilrea Arena', 'Central', '45.390558', '-75.628903'
                  , 'Earl Armstrong Arena', 'East', '45.43588', '-75.602864'
                  , 'Fred Barrett', 'South', '45.331162', '-75.598078'
                  , 'Goulbourn Recreation Complex', 'West', '45.263237', '-75.907526'
                  , 'Howard Darwin Centennial Arena', 'West End', '45.341963', '-75.72629'
                  , 'J. Alph Dulude Arena', 'West End', '45.373757', '-75.743907'
                  , 'Jack Charron Arena', 'West', '45.293619', '-75.883852'
                  , 'Jim Durrell Complex', 'Central', '45.372846', '-75.6597'
                  , 'John G. Mlacak Centre', 'West', '45.320822', '-75.897182'
                  , 'Johnny Leroux Arena (Stittsville)', 'West', '45.260748', '-75.925043'
                  , 'Kanata Recreation Complex', 'West', '45.295047', '-75.903432'
                  , 'Larry Robinson Arena', 'South', '45.230279', '-75.46887'
                  , 'Manotick Arena', 'South', '45.22092', '-75.68733'
                  , 'McNabb Arena', 'Central', '45.409026', '-75.702634'
                  , 'Minto Recreation Complex', 'South', '45.253397', '-75.736022'
                  , 'Navan Memorial Centre', 'East', '45.421381', '-75.421326'
                  , 'Nepean Sportsplex', 'West End', '45.326912', '-75.746002'
                  , 'R. J. Kennedy Arena', 'East', '45.514531', '-75.402976'
                  , 'Ray Friel Recreation Complex', 'East', '45.471382', '-75.49173'
                  , 'Richmond Arena', 'South', '45.195657', '-75.837795'
                  , 'Sandy Hill Arena', 'Central', '45.419286', '-75.673804'
                  , 'St-Laurent Complex', 'East', '45.43638', '-75.647014'
                  , 'Stuart Holmes (Osgoode) Arena', 'South', '45.147472', '-75.601273'
                  , 'Tom Brown Arena', 'Central', '45.408105', '-75.722154'
                  , 'W. Erskine Johnston Arena', 'West', '45.349145', '-76.038851'
                  , 'Walter Baker Sports Centre', 'West', '45.280326', '-75.761861'), ncol=4, byrow = T)


arenas <- rbind(arenas,
                  matrix(c('Bob MacQuarrie - Orléans Pool', 'East', '45.466459', '-75.545228'
                              , 'Brewer Pool', 'Central', '45.389361', '-75.691916'
                              , 'Canterbury Pool', 'South', '45.390558', '-75.628903'
                              , 'Champagne Pool', 'Central', '45.430659', '-75.686719'
                              , 'Dovercourt', 'West End', '45.383344', '-75.752209'
                              , 'Deborah Anne Kirwan Pool', 'South', '45.367799', '-75.656587'
                              , 'François Dupuis Pool', 'East', '45.457012', '-75.449182'
                              , 'Goulbourn Pool', 'West', '45.263237', '-75.907526'
                              , 'Jack Purcell Pool', 'Central', '45.415769', '-75.689533'
                              , 'Kanata Wave Pool', 'West', '45.311241', '-75.898782'
                              , 'Lowertown Pool', 'Central', '45.434277', '-75.681504'
                              , 'Minto Recreation Complex - Barrhaven', 'South', '45.253397', '-75.736022'
                              , 'Nepean Sportsplex Pools', 'West End', '45.326912', '-75.746002'
                              , 'Pinecrest Pool', 'West End', '45.348021', '-75.773401'
                              , 'Plant Pool', 'Central', '45.40799', '-75.714452'
                              , 'Ray Friel Wave Pool', 'East', '45.471382', '-75.49173'
                              , 'Richcraft Recreation Complex - Kanata', 'West', '45.344475', '-75.93036'
                              , 'Sawmill Creek Pool', 'South', '45.350423', '-75.636775'
                              , 'Splash Wave Pool', 'East', '45.437087', '-75.600821'
                              , 'St-Laurent Pool', 'East', '45.43638', '-75.647014'
                              , 'Walter Baker Pool', 'West', '45.280326', '-75.761861'
                              , 'Y-Carlingwood', 'West End', '45.37274', '-75.769143'
                              , 'Y-Ruddy Family', 'East', '45.481451', '-75.508211'
                              , 'Y-Taggart Family', 'Central', '45.411486', '-75.689558'
                              , 'Y-Clarence Rockland', 'East', '45.550633', '-75.286126'), ncol=4, byrow = T))

colnames(arenas) <- c('Arena', 'Locale', 'Latitude', 'Longitude')

arenas <- as.data.frame(arenas)

LoadDataNew <- function(type='skate') {
      master <- data.frame()
      master_day <- data.frame()

      page <- c(0)

      repeat{
            fileUrl <- paste0("http://ottawa.ca/2/en/drop-in-programs?activity-id=", ifelse(type=='swim', 392, 391), "&field_dropin_facility_target_id_selective=all&field_dropin_activity_type_target_id_selective=All&&page=", page)

            doc <- htmlTreeParse(fileUrl, useInternal = TRUE, )
      
            rootNode <- xmlRoot(doc)
            child <- xmlChildren(rootNode)[[3]]
            
            #     Retrieve each of the rows
            #     First row are column names 
            rows <- getNodeSet(child, "//tr")[-1]

            if (length(rows) == 0) break

            result_page <- do.call(rbind, lapply(rows, function(x) {
                  #     Parse out field values for each row and return data in same format as previously
                  id <- NA
                  
                  location <- ifelse(is.null(xmlValue(x[[1]])), NA,
                                     gsub(x = xmlValue(x[[1]], trim=T), pattern = ' \\nMap', replacement = ''))
      
                  day <- ifelse(is.null(xmlValue(x[[7]])), NA, xmlValue(x[[7]], trim=T))
                  
                  start_date <- ifelse(xmlValue(x[[9]], trim=T)=='Ongoing', format(Sys.Date(), '%B %d'), xmlValue(x[[9]], trim=T))
                  
                  end_date <- '2016-03-31'
                  
                  starttime <- ifelse(is.null(str_split(xmlValue(x[[5]]), 'to')[[1]][1]), NA, str_split(xmlValue(x[[5]], trim=T), 'to')[[1]][1])
                  
                  endtime <- ifelse(is.null(str_split(xmlValue(x[[5]]), 'to')[[1]][2]), NA, str_split(xmlValue(x[[5]], trim=T), 'to')[[1]][2])
                  
                  session_type <- ifelse(is.null(xmlValue(x[[3]])), NA, xmlValue(x[[3]], trim=T))
                  
                  comments <- ifelse(is.null(xmlValue(x[[11]])), NA, xmlValue(x[[11]]))
      
                  #     Join everything together into a single data.frame
                  data.frame(id, location, day, start_date, end_date, starttime, endtime, session_type, comments, stringsAsFactors = FALSE)
                  
            }))

            
            #     Copy page of results into master data frame
            master <- rbind(master, result_page)      
      
            #     Move to the next set of results
            page <- page + 1
      }

      master$id <- seq(1, nrow(master))
      
      #     The city feed started to combine multiple Days in to a single record
      #     Cycle through each record, matching on the Day of the Week string, and insert into
      #     separate aggregate table
      for (d in daynames) {
            temp <- master[grepl(d, master$day), ]
            
            if (nrow(temp) > 0) {
                  temp$day <- d
                  master_day <- rbind(master_day, temp)
            }
      }      
      
      colnames(master_day) <- c('ID', 'Arena', 'Day', 'StartDate', 'EndDate', 'StartTime', 'EndTime', 'SessionType', 'Comments')

      master_day
}

#LoadDataNew()
#LoadDataNew("http://ottawa.ca/2/en/drop-in-programs?activity-id=391&field_dropin_facility_target_id_selective=all&field_dropin_activity_type_target_id_selective=All&&page=")

LoadData <- function(fileUrl, nextFileUrl) {
      master <- data.frame()
      master_day <- data.frame()
      
      startwith <- c(0)
      
      doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
      
      rootNode <- xmlRoot(doc)
      
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

                  #     comments_special only exists in swim data
                  xp <- './arr[@name="comments_special"]'
                  comments_special <- xpathSApply(node, xp, xmlValue)
                  comments_special <- paste(comments_special, 'x')
                  
                  #     Join everything together into a single data.frame
                  data.frame(id, location, day, start_date, end_date, starttime, endtime, session_type, comments, comments_special, location_map, stringsAsFactors = FALSE)
                  
            }))
            
            #     Copy page of results into master data frame
            master <- rbind(master, result_page)      
      
            #     Move to the next set of results
            #     This is based on the assumption that the web site returns
            #     exactly 25 records
            startwith <- startwith + 25
            
            nextPage <- paste(nextFileUrl, startwith, sep="")
            
            doc <- xmlTreeParse(nextPage, useInternal = TRUE)
            
            rootNode <- xmlRoot(doc)
      }
      
      #     The city feed started to combine multiple Days in to a single record
      #     Cycle through each record, matching on the Day of the Week string, and insert into
      #     separate aggregate table
      for (d in daynames) {
            temp <- master[grepl(d, master$day), ]
            temp$day <- d

            master_day <- rbind(master_day, temp)
      }
      
      colnames(master_day) <- c('ID', 'Arena', 'Day', 'StartDate', 'EndDate', 'StartTime', 'EndTime', 'SessionType', 'Comments', 'Special', 'LocationMap')
      
      master_day
}

      

FormatData <- function(master_results) {
      formatted <- master_results
      
      #     Append current year to StartDate...
      formatted$StartDate <- as.Date(paste0(formatted$StartDate, ', ', format(Sys.Date(), '%Y')), format = '%B %d, %Y')
      #     ...then add 1 year for any dates in the past
      formatted[formatted$StartDate < Sys.Date(),]$StartDate <- formatted[formatted$StartDate < Sys.Date(),]$StartDate + years(1)
      
      formatted$EndDate <- as.Date(formatted$EndDate)


#       #----------------------------------------------------------
#       #     Manually add missing Longitude and Latitude 
#       #----------------------------------------------------------
#       formatted[formatted$Arena == 'Johnny Leroux Arena (Stittsville)', "Latitude"] <- 45.260748
#       formatted[formatted$Arena == 'Johnny Leroux Arena (Stittsville)', "Longitude"] <- -75.925043

#       formatted[formatted$Arena == 'Dovercourt', "Latitude"] <- 45.383344
#       formatted[formatted$Arena == 'Dovercourt', "Longitude"] <- -75.752209
#       
#       formatted[formatted$Arena == 'Y-Carlingwood', "Latitude"] <- 45.372740
#       formatted[formatted$Arena == 'Y-Carlingwood', "Longitude"] <- -75.769143
#       
#       formatted[formatted$Arena == 'Y-Clarence Rockland', "Latitude"] <- 45.550633
#       formatted[formatted$Arena == 'Y-Clarence Rockland', "Longitude"] <- -75.286126
# 
#       formatted[formatted$Arena == 'Y-Ruddy Family', "Latitude"] <- 45.481451
#       formatted[formatted$Arena == 'Y-Ruddy Family', "Longitude"] <- -75.508211
#       
#       formatted[formatted$Arena == 'Y-Taggart Family', "Latitude"] <- 45.411486
#       formatted[formatted$Arena == 'Y-Taggart Family', "Longitude"] <- -75.689558
      

      #----------------------------------------------------------------------------------
      #     Join to Sessions lookup to shorten SessionType and retrieve SessionGroup
      #----------------------------------------------------------------------------------
      formatted <- merge(formatted, sessions, by.x = "SessionType", by.y = "longname", all.x = T, )

      formatted$SessionType <- formatted$shortname

      
      #----------------------------------------------------
      #     Join to Arenas lookup to retrieve Locale
      #----------------------------------------------------
      formatted <- merge(formatted, arenas, all.x = T)


      #----------------------------------------------------
      #     Clean up Arena Names
      #----------------------------------------------------
      formatted$Arena <- gsub('( Pools)|( Pool)|( Arena)|( Recreation Complex)', '', formatted$Arena)

      formatted
}


ParseCancellations <- function(x) {
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
            fileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl&notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl&notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl&sq_event=Swimming&sq_lang=en&sort=location+asc%2Cstart_date+asc%2CdayNo+asc%2Cstart_time+asc%2Csession_type+asc&ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957&sq_location=&sq_session_type=&sq_keywords1=&"
            nextFileUrl <- "http://app06.ottawa.ca/cgi-bin/schedulesearch/searchschedule.pl?stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fpublic_swimming_results_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;notfound_stylesheet=http%3A%2F%2Fapp06.ottawa.ca%2Ftemplates%2Fxslt%2Fresults_notfound_en.xsl;sq_event=Swimming;sq_lang=en;sort=location%20asc%2Cstart_date%20asc%2CdayNo%20asc%2Cstart_time%20asc%2Csession_type%20asc;ret=http%3A%2F%2Fottawa.ca%2Fen%2Fnode%2F268957;sq_location=;sq_session_type=;sq_keywords1=;start="
            
            master_swimming <<- LoadData(fileUrl, nextFileUrl)
      }
      

      #----------------------------------------------
      #     Load Dovercourt data - Purge data first
      #----------------------------------------------
      master_swimming <<- master_swimming[master_swimming$Arena != 'Dovercourt', ]
      
      dovercourt <<- as.data.frame(read.csv('Dovercourt.csv', stringsAsFactor=F))
      
      master_swimming <<- rbind(master_swimming, dovercourt)
      

      #-------------------------------------------
      #     Load YMCA data - Purge data first
      #-------------------------------------------
      master_swimming <<- master_swimming[which (! master_swimming$Arena %in% arenas[grep('Y-', arenas$Arena), c('Arena')]), ]
      
      ymca <<- as.data.frame(read.csv('YMCA.csv', stringsAsFactor=F))
      
      master_swimming <<- rbind(master_swimming, ymca)
      
      
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

format(Sys.Date(), '%B %d')








