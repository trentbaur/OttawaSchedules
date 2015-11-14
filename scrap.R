install.packages('XML')
install.packages('RCurl')
install.packages('stringr')

library(XML)
library(RCurl)
library(stringr)
rm(doc)
try(doc <- xmlTreeParse(paste0(fileUrl, page), useInternal = TRUE), silent = T)

doc0 <- htmlTreeParse("http://ottawa.ca/2/en/drop-in-programs?activity-id=391&field_dropin_facility_target_id_selective=all&field_dropin_activity_type_target_id_selective=All&&page=0", useInternal=T)
doc4 <- htmlTreeParse("http://ottawa.ca/2/en/drop-in-programs?activity-id=391&field_dropin_facility_target_id_selective=621&field_dropin_activity_type_target_id_selective=All&&page=4", useInternal=T)
root0 <- xmlRoot(doc0)
root4 <- xmlRoot(doc4)
child0 <- xmlChildren(root0)[[3]]
child4 <- xmlChildren(root4)[[3]]
length(getNodeSet(child, "//tr"))
length(getNodeSet(child, "//tr"))
length(getNodeSet(child4, "//tr"))

root0==root4
class(doc)

#html <- getURL("http://ottawa.ca/2/en/drop-in-programs?activity-id=391&field_dropin_facility_target_id_selective=621&field_dropin_activity_type_target_id_selective=All")
#htmlTreeParse(html)

root <-
root <- xmlRoot(doc)
child <- xmlChildren(root)[[3]]

xmlChildren(child0)

rs0 <- getNodeSet(child0, "//tr")

xmlValue(rs[[2]], trim = T)

str_split(xmlValue(rs[[2]][[5]], trim=T), 'to')[[1]][1]



xmlValue(rs[[2]][[1]], trim=T)
xmlValue(rs[[2]][[3]], trim=T)
xmlValue(rs[[2]][[5]], trim=T)
xmlValue(rs[[2]][[7]], trim=T)
xmlValue(rs0[[4]][[9]], trim=T)=='Ongoing'
xmlValue(rs[[2]][[11]], trim=T)





cols <- getNodeSet(rs[[2]], "//span")
xmlValue(cols)

getNodeSet(child, "//field-name-field-dropin-activity-type")

xp <- 'date'


lapply(cols, xmlValue)

ns <- c(str="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/structure")
str(xpathSApply(cols[[3]], './/str:date-display-start', xmlValue, namespaces = ns))

str(cols[[3]])

xmlAttrs(cols[[10]])


readHTMLTable("http://ottawa.ca/2/en/drop-in-programs?activity-id=391&field_dropin_facility_target_id_selective=All&field_dropin_activity_type_target_id_selective=All")

doc$children[6]$html



#     Encoding


x<-matrix(c('Bob MacQuarrie Recreation Complex - Orléans', 'East', '45.466459', '-75.545228'), ncol = 4)


as.data.frame(x)


Encoding[as.vector(x[,1])]



