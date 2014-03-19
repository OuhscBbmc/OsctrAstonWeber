#https://github.com/hadley/ggplot2/wiki/Using-ggplot2-animations-to-demonstrate-several-parallel-numerical-experiments-in-a-single-layout
rm(list=ls(all=TRUE)) #Clear variables
require(ggplot2)
require(animation)
require(RColorBrewer)

# pathDirectory <- "F:/Projects/OuHsc/SafeCare/Spatial/SafeCareSpatial"
# pathDirectoryData <- file.path(pathDirectory, "PhiFreeDatasets")
# pathDirectoryCode <- pathDirectory

pathInputSummaryStateYear <- file.path("./PhiFreeData/Raw/StateYearRate.csv")
pathInputSummaryCounty <- file.path("./PhiFreeData/Derived/CountyFortified.csv")
pathInputSummaryCountyYear <- file.path("./PhiFreeData/Derived/CountyYearFortified.csv")
pathInputMappingCode <- file.path("./Code/MapFunctions.R")
pathDirectoryImages <-  file.path(getwd(), "Analysis/Spatial/AnimationImages") #This needs the qualified path to work correctly with ImageMagick
pathOutputAnimation <- file.path("./Analysis/Spatial/MapAnimation.gif")

widthInches <- 12
heightInches <- widthInches / 2
dpiAnimation <- 100
dpiStatic <- 400

dsCounty <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsCountyAllYears <- read.csv(pathInputSummaryCountyYear, stringsAsFactors=FALSE)
# dsCountyAllYears <- dsCountyAllYears[dsCountyAllYears$ReferralYear %in% years, ]
years <- sort(unique(dsCountyAllYears$ReferralYear))
yearsPretty <- paste0(min(years), " - ", max(years))


# dvName <- "CountPerCapitaRank" #The county's rank for the number of victims per county population; darker counties have more victims, adjusted for pop
# dvName <- "Count" #The county's rank for the number of victims per county population; darker counties have more total victims
dvName <- "Rate" #The number of victims per county population; darker counties have more victims, adjusted for pop
dsCounty$DV <- dsCounty[, dvName]
dsCounty$DVLabel <- gsub("^0.", ".",round(dsCounty$DV,3)) #Remove leading zeros.

dsCountyAllYears$DV <- dsCountyAllYears[, dvName]
dsCountyAllYears$DVLabel <- gsub("^0.", ".",round(dsCountyAllYears$DV,3)) #Remove leading zeros.
# dsCountyAllYears$DVLabel <- scales::comma(dsCountyAllYears$DV)

dvFloor <- min(dsCountyAllYears$DV, na.rm=T)
dvCeiling <- max(dsCountyAllYears$DV, na.rm=T)
source(pathInputMappingCode)

#These six lines are for the line graph
# dsCounty <- read.csv(pathInputSummaryCounty, stringsAsFactors=FALSE)
dsState <- read.csv(pathInputSummaryStateYear, stringsAsFactors=FALSE)
# dsCounty <- dsCounty[dsCounty$ReferralYear %in% years, ]

dsState$DV <- dsState[, dvName]

#########################################################################
### Overall Time Graphs (static)
#########################################################################
breakPoints <- c(min(dsCountyAllYears$DV, na.rm=T), 3.44, 4.33, 4.60, max(dsCountyAllYears$DV, na.rm=T)) #Requested by Chris for the *overall* rate
intervalCount <- length(breakPoints) - 1L
labelThreshold <- sort(breakPoints, decreasing=T)[2]
palette <- RColorBrewer::brewer.pal(n=intervalCount, name="YlGnBu")

title <- paste("Amputation", dvName, "\n", yearsPretty)
fileName <- file.path(pathDirectoryImages, "AmputationAllYears.png")

png(filename=fileName, width=widthInches, height=heightInches, units="in", res=dpiStatic)
MapCounties(dsValue=dsCounty,  deviceWidth=widthInches, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling,
            titleLocationBottom = c(x=-102, y=35.7), subtitleLocationMiddle=c(x=-99, y=33.75),
            intervalCount=intervalCount, breakPoints=breakPoints, paletteResource=palette)  
dev.off()

#########################################################################
### Longitudinal graphs (animated & static)
#########################################################################
animationIntervals <- rep(1, length(years))
animationIntervals[1] <- 1
animationIntervals[length(animationIntervals)] <- 3

breakPointsYearly <- c(min(dsCountyAllYears$DV, na.rm=T), 3.64, 4.72, 5.79, max(dsCountyAllYears$DV, na.rm=T)) #Requested by Chris for the *yearly* rate
intervalCountYearly <- length(breakPointsYearly) - 1L
labelThresholdYearly <- sort(breakPointsYearly, decreasing=T)[2]
palette <- RColorBrewer::brewer.pal(n=intervalCountYearly, name="YlGnBu")

s <- saveGIF({
  for( year in years ) {
    dsSlice <- dsCountyAllYears[dsCountyAllYears$ReferralYear==year, ]    
    title <- paste(year, "Amputation", dvName)
    fileName <- file.path(pathDirectoryImages, paste0("AmputationStatic", year, ".png"))
    
#     png(filename=fileName, width=widthInches, height=heightInches, units="in", res=dpiStatic)
    MapCountiesWithInset(dsValueCountyOneYear=dsSlice,  deviceWidth=widthInches, mapTitle=title, dvFloor=dvFloor, dvCeiling=dvCeiling,
                         dsValueCountyAllYears=dsCountyAllYears, dsValueState=dsState, labelThreshold=labelThresholdYearly, yearBand=year,   
                         intervalCount=intervalCountYearly, breakPoints=breakPointsYearly, paletteResource=palette)  
#     dev.off()
  }
}, movie.name=paste0("AmputationAnimated", dvName, ".gif"), outdir=pathDirectoryImages, interval=animationIntervals, ani.width=widthInches*dpiAnimation, ani.height=heightInches*dpiAnimation)
