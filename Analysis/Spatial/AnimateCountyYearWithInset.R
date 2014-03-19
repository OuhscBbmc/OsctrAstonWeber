#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)
require(RColorBrewer)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory

pathInputSummaryStateYear <- file.path("./PhiFreeData/Raw/StateYearRate.csv")
pathInputSummaryCountyYear <- file.path("./PhiFreeData/Derived/CountyYearFortified.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathDirectoryImages <-  file.path(getwd(), "Analysis/Spatial/AnimationImages") #This needs the qualified path to work correctly with ImageMagick
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")

widthInches <- 12
heightInches <- widthInches/2
dpi <- 100


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
dsCounty$DV <- dsCounty[, dvName]
dsState$DV <- dsState[, dvName]

animationIntervals <- rep(1, length(years))
animationIntervals[1] <- 1
animationIntervals[length(animationIntervals)] <- 3

breakPoints <- c(min(dsCountyAllYears$DV, na.rm=T), 3.64, 4.72, 5.79, max(dsCountyAllYears$DV, na.rm=T)) #Requested by Chris for the *yearly* rate
# breakPointsPretty <- paste(breakPoints), collapse=", ")
intervalCount <- length(breakPoints) - 1L
labelThreshold <- sort(breakPoints, decreasing=T)[2]
palette <- RColorBrewer::brewer.pal(n=intervalCount, name="YlGnBu")
# colorMissing <- "gray80"  


# GraphLongitudinalTrend(dsCounty=dsCountyAllYears, dsState=dsState, labelThreshold=.017886602, yearBand=2007)


s <- saveGIF({
  for( year in years ) {
    dsSlice <- dsCountyAllYears[dsCountyAllYears$ReferralYear==year, ]    
    title <- paste(year, "Amputation", dvName)
    fileName <- file.path(pathDirectoryImages, paste0("Static", year, ".png"))
    
    png(filename=fileName, width=widthInches, height=heightInches, units="in",  res=dpi)
    MapCountiesWithInset(dsValueCountyOneYear=dsSlice,  deviceWidth=widthInches, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling,
                         dsValueCountyAllYears=dsCountyAllYears, dsValueState=dsState, labelThreshold=labelThreshold, yearBand=year,   
                         intervalCount=intervalCount, breakPoints=breakPoints, paletteResource=palette)  
    dev.off()
  }
}, movie.name=paste0("Animated", dvName, ".gif"), outdir=pathDirectoryImages, interval=animationIntervals, ani.width=widthInches*dpi, ani.height=heightInches*dpi)

ss <- strsplit(s, split=" ")
ss[[length(ss)]][length(ss[[1]])]