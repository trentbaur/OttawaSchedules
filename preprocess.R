source("general.R")

library(lubridate)
library(stringr)
library(RCurl)
library(XML)
library(data.table)

activity_group <- as.data.frame(
                  matrix(c('Skating', '593',
                           'Swimming', '589',
                           'Aquafit', '608',
                           'Spinning', '617',
                           'Fitness', '616',
                           'Yoga', '607',
                           'Sports', '678'), ncol = 2, byrow = T))

colnames(activity_group) <- c('name' ,'code')

#------------------------------------
#     Web Scraping Functions
#------------------------------------
get_city_url <- function(p_activity = 'Skating',
                         p_page=0) {
      
    activities_url <- paste0('http://ottawa.ca/2/en/residents/parks-and-recreation/drop-in-programs?f[0]=field_diss_activity%3A', activity_group[activity_group==p_activity,c('code')])

    if(p_page>0) {
        activities_url <- paste0(activities_url, '&page=', as.character(p_page))
    }

    activities_url
}
#     get_city_url(p_page=2)


retrieve_data <- function(p_activity = 'Skating', p_test = TRUE) {
    all_results <- data.frame()
    page = 0
    
    #     Repeat until a single record is retrieved       
    repeat {
        city_url <- get_city_url(p_activity, page)

        print(city_url)
        
        #   For some reason, need to use httr::GET
        #   htmlTreeParse retrieved the wrong data as if query string was ignored)
        acts <- httr::GET(city_url)
        
        doc <- htmlTreeParse(file = acts, useInternal = TRUE, isURL = TRUE, encoding = 'UTF-8')

        activities <- readHTMLTable(doc, as.data.frame = TRUE, trim = TRUE)
        
        #   Grab node_url to enable linking to the city's actual Facility/Activity page
        if(length(activities) > 0) {
            result_page <- cbind(activities[[1]], node_url=xpathSApply(doc, '//td[@class="views-field views-field-field-diss-repeating"]//a/@href'))
            result_page[,1] <- trim(xpathSApply(doc, '//td[@class="views-field views-field-field-diss-facility"]//a/text()', xmlValue))
        } else {
            break
        }

        #     Copy page of results into master data frame
        all_results <- rbind(all_results, result_page)      
        
        #     Move to the next set of results
        page <- page + 1
        
        if(p_test & page > 2) break
    }

    names(all_results) <- c('Facility', 'Activity', 'Date', 'Time', 'Details', 'node_url')
    
    if(p_test) {
        all_results
    } else {
        saveRDS(object = all_results, file = paste0(folder_raw, p_activity, '_', gsub('-', '', ymd(Sys.Date())), '.rds'))
    }
}
#   retrieve_data(p_activity='Skating', p_test=FALSE)
#   retrieve_data(p_activity='Swimming', p_test=FALSE)


#---------------------------
#     Tidy Data
#---------------------------
load_data_raw <- function(p_activity='Skating') {
    
    file <- max(list.files(path = folder_raw, pattern = p_activity, full.names = TRUE))

    as.data.table(readRDS(file))
}

format_data <- function(p_activity='Skating') {
    
    data <- load_data_raw(p_activity)
    
    #----------------------------
    #     Format dates
    #----------------------------
    #     Append current year to Date...
    data$Date <- as.Date(paste0(data$Date, ', ', format(Sys.Date(), '%Y')), format = '%A, %B %d, %Y')
    #     ...then add 1 year for any dates in the past
    data[data$Date < Sys.Date(),]$Date <- data[data$Date < Sys.Date(),]$Date + years(1)
    
    data <- cbind(data, trim(str_split(data$Time, '-', simplify = TRUE)))
    
    setnames(data, old=c('V1', 'V2'), new=c('StartTime', 'EndTime'))
    
    
    #----------------------------------------------------------------------------------
    #     Join to Sessions lookup to shorten SessionType and retrieve SessionGroup
    #----------------------------------------------------------------------------------
    data$Activity <- gsub(pattern = paste0(p_activity, ' - '), replacement = '', x = data$Activity)
    
    data <- merge(data, sessions, by = "Activity", all.x = TRUE)
    
    data$SessionType <- data$ShortName

    
    #----------------------------------------------------
    #     Join to Arenas lookup to retrieve Locale
    #----------------------------------------------------
    data <- merge(data, unique(facilities), by="Facility", all.x = TRUE)

    data[,.(Date, StartTime, EndTime, Activity, ShortName, Facility, Locale, Longitude, Latitude, SessionGroup, SessionType)]
}
#  format_data()

write_tableau_data <- function(p_activity='Skating') {

    write.table(x = format_data(p_activity), file = paste0(folder_clean, p_activity, '.csv'), sep = ',', row.names = FALSE)
}

#   write_tableau_data('Skating')
#   write_tableau_data('Swimming')




