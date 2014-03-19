rm(list=ls(all=TRUE))
require(plyr)
require(maps)
require(maptools)

pathDirectoryRaw <- "./PhiFreeData/Raw"
pathDirectoryDerived <- "./PhiFreeData/Derived"
# pathInputCensus2012 <- file.path(pathDirectory, "PopByCounty-2012Aug.csv")
# pathInputCensusYearly <- file.path(pathDirectoryRaw, "2014-03-14 inpatient DM amp per 1000DM byYear.csv")
pathInputCountyLookup <- file.path(pathDirectoryRaw, "CountyLookups.csv")
pathInputSummaryCounty <- file.path(pathDirectoryRaw, "2014-03-14 inpatient DM amp per 1000DM.csv")
pathInputSummaryCountyYear <- file.path(pathDirectoryRaw, "2014-03-14 inpatient DM amp per 1000DM byYear.csv")
pathOutputSummaryCounty <- file.path(pathDirectoryDerived, "CountyFortified.csv")
pathOutputSummaryCountyYear <- file.path(pathDirectoryDerived, "CountyYearFortified.csv")
pathOutputStateYear <- file.path(pathDirectoryDerived, "StateYearFortified.csv")

#Read in the necessary data files
# dsCensus2012 <- read.csv(pathInputCensus2012, stringsAsFactors=FALSE)
# dsCensusYearly <- read.csv(pathInputCensusYearly, stringsAsFactors=FALSE)
dsLookup <- read.csv(pathInputCountyLookup, stringsAsFactor=F)
dsCounty <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsCountyYear <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)

# dsCensus2012 <- dsCensus2012[, c("CountyName", "PopTotal")] #Drop the unnecessary columns
################################################################################################
### Find Label Points for each County
################################################################################################
## Indicate a point to label each county
dsCenterPoint <- maps::map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)

countyNameForLabelPoint <- gsub(pattern="oklahoma,(.+)", replacement="\\1", x=dsCenterPoint$names) #Strip away the "oklahoma," part of the name
countyIDs <- match(x=countyNameForLabelPoint, table=tolower(dsCounty$CountyName)) #Using the 'order' fx accounts for the different alphabetical schemes

spForCenters <- maptools::map2SpatialPolygons(dsCenterPoint, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))

dsCoordinates <- data.frame(
  CountyNameLower = countyNameForLabelPoint, 
  "LabelLongitude" = coordinates(spForCenters)[, 1], 
  "LabelLatitude" = coordinates(spForCenters)[, 2], 
  stringsAsFactors = F)

dsCoordinates[dsCoordinates$CountyNameLower=="pottawatomie", "LabelLatitude"] <- dsCoordinates[dsCoordinates$CountyNameLower=="pottawatomie", "LabelLatitude"] + .1
dsCoordinates[dsCoordinates$CountyNameLower=="love", "LabelLatitude"] <- dsCoordinates[dsCoordinates$CountyNameLower=="love", "LabelLatitude"] + .05
################################################################################################
### Work on dsCounty
################################################################################################
# dsCounty <- merge(x=dsCounty, y=dsCensus2012, by="CountyName")
# dsCounty$CountPerCapitaAnnual <- (dsCounty$Count / dsCounty$PopTotal) / yearCount

dsCounty$CountyNameLower <- tolower(dsCounty$CountyName)
dsCounty <- base::merge(x=dsCounty, y=dsCoordinates, by="CountyNameLower", all=TRUE) #Merge the coordinates with the county dataset
dsCounty$CountyNameLower <- NULL #This column is no longer necessary

dsCounty$RateRank <- rank(dsCounty$Rate)
################################################################################################
### Work on dsCountyYear
################################################################################################
# dsCensusYearly$YearPlusOne <- dsCensusYearly$Year + 1
# dsCensusYearly <- dsCensusYearly[, colnames(dsCensusYearly) != "Year"]
# #dsCountyYearFortified <- merge(x=dsCountyYearFortified, y=dsCensus, by="CountyName")
# dsCountyYearFortified <- merge(x=dsCountyYearFortified, y=dsCensusYearly, 
#                                by.x=c("CountyName", "ReferralYear"), 
#                                by.y=c("CountyName", "YearPlusOne"))
# 
# dsCountyYearFortified$CountPerCapitaAnnual <- dsCountyYearFortified$Count / dsCountyYearFortified$PopTotal

dsCountyYear$CountyNameLower <- tolower(dsCountyYear$CountyName)
dsCountyYearFortified <- base::merge(x=dsCountyYear, y=dsCoordinates, by="CountyNameLower", all=TRUE) #Merge the coordinates with the county dataset
dsCountyYearFortified$CountyNameLower <- NULL #This column is no longer necessary

dsCountyYearFortified <- dsCountyYearFortified[order(dsCountyYearFortified$ReferralYear, dsCountyYearFortified$CountyName), ]

dsCountyYearFortified$RateRank <- rank(dsCountyYearFortified$Rate)
################################################################################################
### Work on dsStateYear
################################################################################################
# StateSummarizing <- function( df ) {
#   return( data.frame(Rate=sum(df$Rate, na.rm=T)) ) #, PopTotal=sum(df$PopTotal)
# }
# dsState <- ddply(dsCountyYearFortified, "ReferralYear", StateSummarizing)
# # dsState$CountPerCapitaAnnual <- dsState$Count / dsState$PopTotal
# 
# write.csv(dsState, pathOutputStateYear, row.names=FALSE)
write.csv(dsCounty, pathOutputSummaryCounty, row.names=FALSE)
write.csv(dsCountyYearFortified, pathOutputSummaryCountyYear, row.names=FALSE)
