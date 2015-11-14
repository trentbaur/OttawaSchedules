source("general.R")

# install.packages('RCurl')
# install.packages('XML')
# install.packages('stringr')
# install.packages('data.table')
# install.packages('lubridate')


library(lubridate)
library(stringr)
library(RCurl)
library(XML)
library(data.table)

#------------------------------------
#     Web Scraping Functions
#------------------------------------
get_url <- function(type='skate', page=0) {
      paste0("http://ottawa.ca/2/en/drop-in-programs?activity-id=",
                        ifelse(type=='swim', 392, 391),
                        "&field_dropin_facility_target_id_selective=",
                        ifelse(type=='swim',"A","a"),
                        "ll&field_dropin_activity_type_target_id_selective=All",
                        ifelse(page==0, "", paste0("&&page=", page)))
}

retrieve_data <- function(type='skate', test=TRUE, page=0) {
      all_results <- data.frame()
      all_results_by_day <- data.frame()

      #     Repeat until a single record is retrieved       
      repeat{
            fileUrl <- get_url(type, page)
            
            doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
            
            rootNode <- xmlRoot(doc)
            
            child <- xmlChildren(rootNode)[[3]]
            
            #     Retrieve each of the rows
            #     First row only consists of column names so ignore it
            rows <- getNodeSet(child, "//tr")[-1]
            
            if (length(rows) <= 1 & is.na(xmlValue(rows[[1]][[3]]))) break
            
            result_page <- do.call(rbind, lapply(rows, function(x) {
                  #     Parse out field values for each row and return data in same format as previously
                  id <- NA
                  
                  location <- ifelse(is.null(xmlValue(x[[1]])), NA,
                                     str_split(string = xmlValue(x[[1]], trim=T), pattern=' \\n Map')[[1]][1])
                  #gsub(x = xmlValue(x[[1]], trim=T), pattern = ' \\nMap', replacement = ''))
                  
                  day <- ifelse(is.null(xmlValue(x[[7]])), NA, xmlValue(x[[7]], trim=T))
                  
                  start_date <- ifelse(xmlValue(x[[9]], trim=T)=='Ongoing', format(Sys.Date(), '%B %d'), xmlValue(x[[9]], trim=T))
                  
                  #     Fudge end date since the site doesn't provide end date per session
                  end_date <- '2016-03-31'
                  
                  starttime <- ifelse(is.null(str_split(xmlValue(x[[5]]), 'to')[[1]][1]), NA, str_split(xmlValue(x[[5]], trim=T), 'to')[[1]][1])
                  
                  endtime <- ifelse(is.null(str_split(xmlValue(x[[5]]), 'to')[[1]][2]), NA, str_split(xmlValue(x[[5]], trim=T), 'to')[[1]][2])
                  
                  session_type <- ifelse(is.null(xmlValue(x[[3]])), NA,
                                         gsub(pattern = 'Fee$', replacement = '', xmlValue(x[[3]], trim=T)))
                  
                  comments <- gsub(pattern = '\\n', replacement = '', x = ifelse(is.null(xmlValue(x[[11]])), NA, xmlValue(x[[11]])))
                  
                  #     Join everything together into a single data.frame
                  data.frame(id, location, day, start_date, end_date, starttime, endtime, session_type, comments, stringsAsFactors = FALSE)
            }))
            
            
            #     Copy page of results into master data frame
            all_results <- rbind(all_results, result_page)      
            
            #     Move to the next set of results
            page <- page + 1
            
            if(test) break
      }
      
      all_results$id <- seq(1, nrow(all_results))
      
      #     The city feed started to combine multiple weekday schedules into a single record
      #     Cycle through each record, matching on the Day of the Week string, and insert into
      #     separate aggregate table
      #     This will handle normal records with one day and records with multiple days
      for (d in daynames) {
            temp <- all_results[grepl(d, all_results$day), ]
            
            if (nrow(temp) > 0) {
                  temp$day <- d
                  all_results_by_day <- rbind(all_results_by_day, temp)
            }
      }      
      
      colnames(all_results_by_day) <- c('ID', 'Arena', 'Day', 'StartDate', 'EndDate', 'StartTime', 'EndTime', 'SessionType', 'Comments')
      
      if(test) {
            all_results_by_day
      } else {
            write.csv(x = all_results_by_day, file = paste0(folder_raw, 'data_', type, '.csv'), fileEncoding = "latin1")
      }
}
#     retrieve_data(type='swim', test=F)
#     retrieve_data(type='swim', test=F)


#---------------------------
#     Preprocess Data
#---------------------------
load_data_raw <- function(type='skate') {
      #     Use data.table for main data manipulation
      data <- fread(input = paste0(folder_raw, 'data_', type, '.csv'))
      
      data
}


format_data <- function(data) {
      #----------------------------
      #     Format dates
      #----------------------------
      #     Append current year to StartDate...
      data$StartDate <- as.Date(paste0(data$StartDate, ', ', format(Sys.Date(), '%Y')), format = '%A, %B %d, %Y')
      #     ...then add 1 year for any dates in the past
      data[data$StartDate < Sys.Date(),]$StartDate <- data[data$StartDate < Sys.Date(),]$StartDate + years(1)
      
      
      data$EndDate <- as.Date(data$EndDate)

      
      #----------------------------
      #     Clean up comments
      #----------------------------
      last_sessions <- grepl(pattern = 'Last session', x = data$Comments)
      
      if(sum(last_sessions)>0) {
            data[last_sessions,]$EndDate <- as.Date(trim(gsub(x = data[last_sessions,]$Comments, pattern = 'Last session: ',replacement = '')), format = '%A, %B %d, %Y')
      }

      
      #----------------------------------------------------------------------------------
      #     Join to Sessions lookup to shorten SessionType and retrieve SessionGroup
      #----------------------------------------------------------------------------------
      data <- merge(data, sessions, by.x = "SessionType", by.y = "longname", all.x = T)
      
      data$SessionType <- data$shortname
      
      
      #----------------------------------------------------
      #     Join to Arenas lookup to retrieve Locale
      #----------------------------------------------------
      #     Clean up Sawmill Arena entries
      data[grepl(pattern = 'Sawmill Creek Pool', x = data$Arena),]$Arena <- c('Sawmill Creek Pool & Community Centre')

      data <- merge(data, arenas, by="Arena", all.x = T)
      
      
      #---------------------------------
      #     Clean up Arena Names
      #---------------------------------
      data$Arena <- gsub('( Pools)|( Pool)|( Arena)|( Recreation Complex)', '', data$Arena)
      
      data
}


parse_cancellations <- function(x) {
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


normalize_data <- function(data) {
      #     Create set of dates to be used
      #     Recreating every time we run will remove old dates that are no longer relevant
      #     This could act as a natural limiter to the data volume
      dates <- GetDates(max(data$EndDate))
      
      #     Join each schedule row to list of dates to get individual schedules
      #     Keep all ancillary fields
      all <- merge(dates, data, by = "Day")
      
      filtered <- all[all$Date >= all$StartDate & all$Date <= all$EndDate, ]
      
      #     Remove cancellations
      cancellations <<- do.call(rbind, apply(data, 1, parse_cancellations))
      cancellations$ID <- as.integer(cancellations$ID)
      
      filtered <- merge(filtered, cancellations, by.x = c("ID", "Date"), by.y = c("ID", "CancelDate"), all.x = T)
      
      #     Join time to dates
      filtered$StartDateTime <- as.POSIXlt(as.character(paste(as.character(filtered$Date),  filtered$StartTime)), "%Y-%m-%d %I:%M %p ", tz="EST")
      filtered$EndDateTime <- as.POSIXlt(as.character(paste(as.character(filtered$Date),  filtered$EndTime)), "%Y-%m-%d %I:%M %p ", tz="EST")
      
      #     Return filtered dates without cancellations
      filtered[is.na(filtered$Start), ]
}


#     Raw data must be retrieved using the retrieve_data() function at the top
#     To reduce number of times we hit the website, it does minimal processing and stores raw
#     data to be processed by the following driver
#
#     NOTE!! - For some reason, loading the arenas table via the source command messes up the
#                 french characters. Manually reload the table before preprocessing the data
preprocess_data <- function(type='skate', test=TRUE) {
      data <- load_data_raw(type)
      
      data <- format_data(data)
      
      data <- normalize_data(data)
      
      if (test) {
            data
      } else {
            filename <- ifelse(type=='swim', 'swimming.csv', 'skating.csv')
            write.csv(data[,c('Date', 'StartDateTime', 'EndDateTime', 'Arena', 'Locale', 'SessionType', 'SessionGroup', 'Latitude', 'Longitude')], file=filename)
      }
}


#     preprocess_data(type='skate', test=F)
#     preprocess_data(type='swim', test=F)
#     x <- preprocess_data(type='swim', test=T)









