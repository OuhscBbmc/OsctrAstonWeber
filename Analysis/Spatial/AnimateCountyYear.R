#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory
pathInputSummaryCountyYear <- file.path("./PhiFreeData/Derived/CountyYearFortified.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")
pathDirectoryImages <-  file.path(getwd(), "Analysis/Spatial/AnimationImages") #This needs the qualified path to work correctly with ImageMagick

ds <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)

# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
# ds$DV <- ds[, dvName]
# ds$DVLabel <- scales::comma(ds$DV)

dvName <- "Rate" #The number of people with amputations, per 1,000 diabetics
ds$DV <- ds[, dvName]
ds$DVLabel <- gsub("^0.", ".",round(ds$DV,3)) #Remove leading zeros.

dvFloor <- min(ds$DV, na.rm=T)
dvCeiling <- max(ds$DV, na.rm=T)
source(pathInputMappingCode)

years <- sort(unique(ds$ReferralYear)) #2007:2012
intervals <- rep(1, length(years))
intervals[1] <- 2 #4
intervals[length(intervals)] <- 2 #4
s <- saveGIF({
  for( year in years ) {
    dsSlice <- ds[ds$ReferralYear==year, ]
    
    title <- paste(dvName, year)
    g <- MapCounties(dsValue=dsSlice, deviceWidth=14, showCountyValues=T, mapTitle=title,
                     dvFloor=dvFloor, dvCeiling=dvCeiling)
    print(g)
    ggsave(filename=file.path(pathDirectoryImages, paste0("Static", year, ".png")), plot=g)
  }
}, outdir=pathDirectoryImages, movie.name=paste0("Animated", dvName, ".gif"), interval=intervals,ani.width=1600, ani.height=800)

# ss <- strsplit(s, split=" ")
# ss[[length(ss)]][length(ss[[1]])]
