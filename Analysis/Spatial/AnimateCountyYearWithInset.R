#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)
require(RColorBrewer)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory

pathInputSummaryStateYear <- file.path("./PhiFreeData/Derived/StateYearFortified.csv")
pathInputSummaryCountyYear <- file.path("./PhiFreeData/Derived/CountyYearFortified.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathDirectoryImages <-  file.path(getwd(), "Analysis/Spatial/AnimationImages") #This needs the qualified path to work correctly with ImageMagick
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")
palette <- RColorBrewer::brewer.pal(n=4, name="YlGn")
colorMissing <- "gray80"  


dsCountyAllYears <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
# dsCountyAllYears <- dsCountyAllYears[dsCountyAllYears$ReferralYear %in% years, ]
years <- sort(unique(dsCountyAllYears$ReferralYear))


# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
# dsCountyAllYears$DVLabel <- scales::comma(dsCountyAllYears$DV)

dvName <- "Rate" #The number of victims per county population; darker counties have more victims, adjusted for pop
dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
dsCountyAllYears$DVLabel <- gsub("^0.", ".",round(dsCountyAllYears$DV,3)) #Remove leading zeros.

# dvName <- "CountPerCapitaRank" #The county's rank for the number of victims per county population; darker counties have more victims, adjusted for pop
# dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
# dsCountyAllYears$DVLabel <- dsCountyAllYears$DV

dvFloor <- min(dsCountyAllYears$DV, na.rm=T)
dvCeiling <- max(dsCountyAllYears$DV, na.rm=T)
source(pathInputMappingCode)

#These six lines are for the line graph
dsCounty <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
dsState <- read.csv(pathInputSummaryStateYear, stringsAsFactors=FALSE)
dsCounty <- dsCounty[dsCounty$ReferralYear %in% years, ]
dsState <- dsState[dsState$ReferralYear %in% years, ]
dsCounty$DV <- dsCounty[, dvName]
dsState$DV <- dsState[, dvName]


intervals <- rep(1, length(years))
intervals[1] <- 1
intervals[length(intervals)] <- 3


GraphLongitudinalTrend(dsCounty=dsCountyAllYears, dsState=dsState, labelThreshold=.017886602, yearBand=2007)


s <- saveGIF({
  for( year in years ) {
    dsSlice <- dsCountyAllYears[dsCountyAllYears$ReferralYear==year, ]
    
    title <- paste(dvName, year)
#     g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
#                      dvFloor=dvFloor, dvCeiling=dvCeiling)
    #     g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
    #                      dvFloor=dvFloor, dvCeiling=dvCeiling)    
#     print(g)
    

    
    MapCountiesWithInset(dsValueCountyOneYear=dsSlice,  deviceWidth=18, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling,
                         dsValueCountyAllYears=dsCountyAllYears, dsValueState=dsState, labelThreshold=6, yearBand=year   )
#     names(dsValueAllVariables)
    
  }
}, movie.name=paste0(dvName, ".gif"), outdir=pathDirectoryImages, interval=intervals,ani.width=1600, ani.height=800)

ss <- strsplit(s, split=" ")
ss[[length(ss)]][length(ss[[1]])]