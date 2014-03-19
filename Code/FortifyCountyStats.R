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
# years <- sort(unique(dsCountyYear$ReferralYear))
# yearCount <- length(years)
################################################################################################
### Find Label Points for each County
################################################################################################
## Indicate a point to label each county
dsCenterPoint <- maps::map("county", "oklahoma", fill=TRUE, col="transparent", plot=FALSE)

countyNameForLabelPoint <- gsub(pattern="oklahoma,(.+)", replacement="\\1", x=dsCenterPoint$names) #Strip away the "oklahoma," part of the name
countyIDs <- match(x=countyNameForLabelPoint, table=tolower(dsCounty$CountyName)) #Using the 'order' fx accounts for the different alphabetical schemes

spForCenters <- map2SpatialPolygons(dsCenterPoint, IDs=countyIDs,  proj4string=CRS(" +proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0"))

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
dsCounty$CountyNameLower <- tolower(dsCounty$CountyName)

# dsCounty <- merge(x=dsCounty, y=dsCensus2012, by="CountyName")
# dsCounty$CountPerCapitaAnnual <- (dsCounty$Count / dsCounty$PopTotal) / yearCount
# dsCounty$CountRank <- rank(dsCounty$Count)
dsCounty$RateRank <- rank(dsCounty$Rate)


dsCounty <- base::merge(x=dsCounty, y=dsCoordinates, by="CountyNameLower", all=TRUE) #Merge the coordinates with the county dataset
dsCounty$CountyNameLower <- NULL
# rm(dsCoordinates)

# spForCenters <- SpatialPolygonsDataFrame(spForCenters, data=dsCensus2012)
# labelCoordinates <- coordinates(spForCenters)
# labelCoordinates[which(dsCounty$CountyName=="Pottawatomie"), 2] <- coordinates(spForCenters)[which(dsCounty$CountyName=="Pottawatomie"), 2] + .1
# labelCoordinates[which(dsCounty$CountyName=="Love"), 2] <- coordinates(spForCenters)[which(dsCounty$CountyName=="Love"), 2] + .05
# colnames(labelCoordinates) <- c("LabelLongitude", "LabelLatitude")
### TODO: pull out CountyIDs and merge properly
# dsCounty <- cbind(dsCounty, labelCoordinates)
# rm(labelCoordinates, spForCenters, dsCenterPoint)

# spFortified <- fortify(sp, region="ID")

################################################################################################
### Work on dsCountyYear
################################################################################################
dsCountyYear$CountyNameLower <- tolower(dsCountyYear$CountyName)
dsCountyYearFortified <- base::merge(x=dsCountyYear, y=dsCoordinates, by="CountyNameLower", all=TRUE) #Merge the coordinates with the county dataset
dsCountyYearFortified$CountyNameLower <- NULL
dsCountyYearFortified <- dsCountyYearFortified[order(dsCountyYearFortified$ReferralYear, dsCountyYearFortified$CountyName), ]
#TODO: this code was written a few years ago.  Replace it with proper plyr/dply code
# dsCountyYearFortified <- data.frame(CountyID=integer(0), CountyName=character(0), Year=integer(0), Rate=integer(0),
#                                     LabelLongitude=numeric(0), LabelLatitude=numeric(0))
# for( year in years ) {
#   dsSlice <- dsCountyYear[dsCountyYear$ReferralYear==year, ]
#   dsSlice <- merge(x=dsLookup, y=dsSlice, by.x="ID", by.y="CountyID", all.x=TRUE, all.y=FALSE)
# #   dsSlice$Rate <- ifelse(is.na(dsSlice$Rate), 0, dsSlice$Rate)
#   dsSlice$CountyName <- dsSlice$Name
#   dsSlice$ReferralYear <- year #This fills in the NAs that exist in the county's without a report that year.
#   dsSlice <- plyr::rename(dsSlice, replace=c(ID="CountyID")) #Rename the ID column
#   dsSlice <- dsSlice[, colnames(dsSlice) != "Name"] #Drop the redundant (County)Name column.
#   
#   dsSlice <- cbind(dsSlice, labelCoordinates)
#   dsCountyYearFortified <- rbind(dsCountyYearFortified, dsSlice)
# }

# dsCensusYearly$YearPlusOne <- dsCensusYearly$Year + 1
# dsCensusYearly <- dsCensusYearly[, colnames(dsCensusYearly) != "Year"]
# #dsCountyYearFortified <- merge(x=dsCountyYearFortified, y=dsCensus, by="CountyName")
# dsCountyYearFortified <- merge(x=dsCountyYearFortified, y=dsCensusYearly, 
#                                by.x=c("CountyName", "ReferralYear"), 
#                                by.y=c("CountyName", "YearPlusOne"))
# 
# dsCountyYearFortified$CountPerCapitaAnnual <- dsCountyYearFortified$Count / dsCountyYearFortified$PopTotal
# dsCountyYearFortified$CountRank <- rank(dsCountyYearFortified$Count)
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
