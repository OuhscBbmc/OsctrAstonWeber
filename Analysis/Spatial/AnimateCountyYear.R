#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)
require(RColorBrewer)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory
pathInputSummaryCountyYear <- file.path("./PhiFreeData/Derived/CountyYearFortified.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")
pathDirectoryImages <-  file.path(getwd(), "Analysis/Spatial/AnimationImages") #This needs the qualified path to work correctly with ImageMagick
# heightPixels <- 1600
# weightPixels <- 800
heightInches <- 3
widthInches <- 6
dpi <- 400

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
animationIntervals <- rep(1, length(years))
animationIntervals[1] <- 2 #4
animationIntervals[length(animationIntervals)] <- 2 #4

# breakPoints <- c(-Inf, 3.64, 4.72, 5.79, Inf)
breakPoints <- c(min(ds$DV, na.rm=T), 3.64, 4.72, 5.79, max(ds$DV, na.rm=T)) #Requested by Chris for the *yearly* rate

intervalCount <- length(breakPoints) - 1L
labelThreshold <- sort(breakPoints, decreasing=T)[2]
palette <- RColorBrewer::brewer.pal(n=intervalCount, name="Blues")

# dsInitial <- ds[ds$ReferralYear==2007, ]
# MapCounties(dsValue=dsInitial, deviceWidth=14, showCountyValues=T, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling, paletteResource=palette)
# 
# MapCountiesWithInset(dsValueCountyOneYear=dsInitial,  deviceWidth=18, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling,
#                      dsValueCountyAllYears=dsCountyAllYears, dsValueState=dsState, labelThreshold=.017886602, yearBand=year   )



# s <- saveGIF({
  for( year in years ) {
    dsSlice <- ds[ds$ReferralYear==year, ]
    title <- paste(year, "Amputation", dvName)
    
    g <- MapCounties(dsValue=dsSlice, deviceWidth=widthInches, showCountyValues=T, mapTitle=title,
                     dvFloor=dvFloor, dvCeiling=dvCeiling,
                     intervalCount=intervalCount, breakPoints=breakPoints, paletteResource=palette)
    print(g)
    ggsave(filename=file.path(pathDirectoryImages, paste0("Static", year, ".png")), plot=g, width=widthInches, height=heightInches, units="in",  dpi=dpi)
  }
# }, outdir=pathDirectoryImages, movie.name=paste0("Animated", dvName, ".gif"), interval=animationIntervals, ani.width=1600, ani.height=800)

# ss <- strsplit(s, split=" ")
# ss[[length(ss)]][length(ss[[1]])]

