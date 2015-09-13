gsub('-', x = sessions$longname, '')
gsub('-', x = master_skating$SessionType, '')


#str(sessions)

#master_skating[master_skating$SessionType==sessions[sessions$shortname=='Pickup 18+',]$longname,]

#class(master_skating$SessionType)
#class(sessions[sessions$shortname=='Pickup 18+',]$longname)
asc <- function(x) { strtoi(charToRaw(x),16L) }



sessions[sessions$shortname=='50+',]$longname[1]==
      master_skating$SessionType[1]


end <- 19

asc(substring(sessions[sessions$shortname=='50+',]$longname, 1, end))==
      asc(substring(master_skating$SessionType[1], 1, end))


u_char_info(substring(master_skating$SessionType[3], 16, end))



unicode(substring(master_skating$SessionType[3], 16, end))


master_skating$SessionType[3]==iconv('Pick-up Hockey – Adults 18+', "latin1", "UTF-8")


sessions[sessions$shortname=='Pickup 18+',]$longname <- master_skating$SessionType[3]


Encoding('Pick-up Hockey – Adults 18+')
iconv('Pick-up Hockey – Adults 18+', "latin1", "UTF-8")
master_skating$SessionType[3]

substring(master_skating$SessionType[3], 16, end)
asc(substring(master_skating$SessionType[3], 16, end))


Encoding(sessions$longname)