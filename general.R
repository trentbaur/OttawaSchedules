folder_clean <- 'data_clean/'
folder_raw <- 'data_raw/'


months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

daynames <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")


get_dates <- function(p_to, p_from=Sys.Date()) {
      dates <- as.data.frame(seq(from = p_from, to = p_to, by = "1 day"))

      colnames(dates) <- c('Date')

      dates$Day <- weekdays(dates[,1])
      
      dates
}

#     get_dates(Sys.Date()+20)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#---------------------------------
#     Define Session names
#---------------------------------
#rm(sessions)
sessions <- matrix(c( 'Skating 50+', '50+', 'Skating'
                     ,'Adult Skating', 'Adult', 'Skating'
                     ,'Family Skating', 'Family', 'Skating'
                     ,'Public Skating', 'Public', 'Skating'
                     ,'Unsupervised Skate', 'Unsupervised', 'Skating'
                     ,'Pick-up Hockey - Adults 18+', 'Pickup 18+', 'Hockey'
                     ,'Pick-up Hockey - Adults 35+', 'Pickup 35+', 'Hockey'
                     ,'Pick-up Hockey - Adults 50+', 'Pickup 50+', 'Hockey'
                     ,'Shinny Hockey - Child', 'Shinny - Children', 'Shinny'
                     ,'Shinny Hockey - Youth (13-17 yrs)', 'Shinny - Youth', 'Shinny'
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
                           
                           ,'Hot Tub/Sauna', 'Hot Tub/Sauna', 'Sauna'
                           
                           ,'Swim for Persons with a Disability', 'With Disability', 'With Disability'
                           ,'Preschool Swim', 'Preschool', 'Age Restricted'), ncol=3, byrow = T))

colnames(sessions) <- c('Activity', 'ShortName', 'SessionGroup')

sessions <- as.data.frame(sessions, stringsAsFactors = F)



#---------------------------------
#     Define Arena names
#---------------------------------
#rm(facilities)
facilities <- matrix(c(  'Bell Centennial Arena', 'West End', '45.324412', '-75.811532'
                   , 'Belltown Dome', 'West End', '45.360014', '-75.803732'
                   , 'Bernard-Grandmaître Arena', 'East', '45.432813', '-75.655395'
                   , 'Blackburn Arena', 'East', '45.43', '-75.563149'
                   , 'Bob MacQuarrie Recreation Complex-Orléans', 'East', '45.466459', '-75.545228'
                   , 'Brewer Pool and Arena', 'Central', '45.389097', '-75.691057'
                   , 'Canterbury Recreation Complex', 'Central', '45.390558', '-75.628903'
                   , 'Champagne Fitness Centre', 'Central', '45.430659', '-75.686719'
                   , 'Deborah Anne Kirwan Pool', 'South', '45.367799', '-75.656587'
                   , 'Dempsey Community Centre', 'Central', '45.401927', '-75.627773'
                   , 'Earl Armstrong Arena', 'East', '45.43588', '-75.602864'
                   , 'Eva James Memorial Community Centre', 'West', '45.290656', '-75.857888'
                   , 'François Dupuis Recreation Centre', 'East', '45.457012', '-75.449182'
                   , 'Fred Barrett Arena', 'South', '45.331162', '-75.598078'
                   , 'Goulbourn Recreation Complex', 'West', '45.263237', '-75.907526'
                   , 'Greenboro Community Centre', 'South', '45.363424', '-75.635499'
                   , 'Hintonburg Community Centre', '', '', ''
                   , 'Howard Darwin Centennial Arena', 'West End', '45.341963', '-75.72629'
                   , 'Hunt Club-Riverside Park Community Centre', 'South', '45.351833', '-75.672689'
                   , 'J.A. Dulude Arena', 'West End', '45.373757', '-75.743907'
                   , 'Jack Charron Arena', 'West', '45.293619', '-75.883852'
                   , 'Jack Purcell Community Centre', 'Central', '45.415769', '-75.689533'
                   , 'Jim Durrell Recreation Centre', 'Central', '45.372846', '-75.6597'
                   , 'John G Mlacak Community Centre', 'West', '45.320822', '-75.897182'
                   , 'Johnny Leroux Stittsville Community Arena', 'West', '45.260748', '-75.925043'
                   , 'Kanata Leisure Centre and Wave Pool', 'West', '45.311241', '-75.898782'
                   , 'Kanata Recreation Complex', 'West', '45.295047', '-75.903432'
                   , 'Larry Robinson Arena', 'South', '45.230279', '-75.46887'
                   , 'Lowertown Pool', 'Central', '45.434277', '-75.681504'
                   , 'Manotick Arena', 'South', '45.22092', '-75.68733'
                   , 'McNabb Recreation Centre', 'Central', '45.409026', '-75.702634'
                   , 'Minto Recreation Complex - Barrhaven', 'South', '45.253397', '-75.736022'
                   , 'Navan Memorial Centre', 'East', '45.421381', '-75.421326'
                   , 'Nepean Sportsplex', 'West End', '45.326912', '-75.746002'
                   , 'Overbrook Community Centre', 'Central', '45.425415', '-75.656782'
                   , 'Pinecrest Recreation Complex', 'West End', '45.348021', '-75.773401'
                   , 'Plant Recreation Centre', 'Central', '45.40799', '-75.714452'
                   , 'R.J. Kennedy Arena', 'East', '45.514531', '-75.402976'
                   , 'Ray Friel Recreation Complex', 'East', '45.471382', '-75.49173'
                   , 'Richcraft Recreation Complex-Kanata', 'West', '45.344475', '-75.93036'
                   , 'Richmond Arena', 'South', '45.195657', '-75.837795'
                   , 'Rideauview Community Centre', 'South', '45.277477', '-75.687125'
                   , 'Sandy Hill Arena', 'Central', '45.419286', '-75.673804'
                   , 'Sawmill Creek Community Centre and Pool', 'South', '45.350423', '-75.636775'
                   , 'Splash Wave Pool', 'East', '45.437087', '-75.600821'
                   , 'St-Laurent Complex', 'East', '45.43638', '-75.647014'
                   , 'Stuart Holmes Arena', 'South', '45.147472', '-75.601273'
                   , 'Tom Brown Arena', 'Central', '45.408105', '-75.722154'
                   , 'W. Erskine Johnston Arena', 'West', '45.349145', '-76.038851'
                   , 'Walter Baker Sports Centre', 'West', '45.280326', '-75.761861'), ncol=4, byrow = T)

colnames(facilities) <- c('Facility', 'Locale', 'Latitude', 'Longitude')

facilities <- as.data.table(facilities, stringsAsFactors = F)


