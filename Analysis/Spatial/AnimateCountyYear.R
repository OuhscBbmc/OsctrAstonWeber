#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory
pathInputSummaryCountyYear <- file.path("./Data/Raw/2014-03-14 inpatient DM amp per 1000DM byYear noborder.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")
pathDirectoryImages <-  file.path("./Analysis/Spatial/AnimationImages")

ds <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)

# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# ds$DV <- ds[, dvName]
# ds$DVLabel <- scales::comma(ds$DV)

dvName <- "Rate" #The number of people with amputations, per 1,000 diabetics
ds$DV <- ds[, dvName]
ds$DVLabel <- gsub("^0.", ".",round(ds$DV,3)) #Remove leading zeros.

dvFloor <- min(ds$DV)
dvCeiling <- max(ds$DV)
source(pathInputMappingCode)

years <- sort(unique(ds$ReferralYear)) #2007:2012
intervals <- rep(1, length(years))
intervals[1] <- 4
intervals[length(intervals)] <- 4
# saveMovie({
s <- saveGIF({
  for( year in years ) {
    dsSlice <- ds[ds$ReferralYear==year, ]
    
    title <- paste(dvName, year)
    g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
                     dvFloor=dvFloor, dvCeiling=dvCeiling)
    print(g)
    # ggsave(filename=file.path(pathDirectoryCode, "Animated.png"), plot=g)
  }
}, movie.name=paste0(dvName, ".gif"), outdir=pathDirectoryImages, interval=intervals,ani.width=1600, ani.height=800)

ss <- strsplit(s, split=" ")
ss[[length(ss)]][length(ss[[1]])]