#http://blogs.luc.edu/rwill5/2012/01/29/experimenting-with-maps-in-r/

# rm(list=ls(all=TRUE))
require(maps)
require(maptools)
# require(sp)
# require(RColorBrewer)
require(colorspace)
require(classInt)
# require(fields)
require(grid)
require(ggplot2)
require(plyr)

MapCounties <- function( dsValuePlot, deviceWidth=10, colorPower=1, showCountyValues=TRUE, mapTitle="",
                         dvFloor=min(dsValuePlot$DV, na.rm=T), dvCeiling=max(dsValuePlot$DV, na.rm=T),
                         intervalCount=3, breakPoints=seq(from=dvFloor,to=dvCeiling, length.out=intervalCount+1),
                         titleLocationBottom=c(x=-102, y=36.2), subtitleLocationMiddle=c(x=-98.7, y=33.75),
                         paletteResource=rev(sequential_hcl(n=intervalCount, h=340, c.=c(80, 0), l=c(40, 90), power=colorPower))) {
  
#   browser()
#   dsValuePlot <- data.frame(
#     CountyID=dsValue$CountyID, 
#     CountyNameLower=tolower(dsValue$CountyName), 
#     CountyName=dsValue$CountyName, 
#     LabelLongitude=dsValue$LabelLongitude,
#     LabelLatitude=dsValue$LabelLatitude,
#     DV=dsValue$DV, 
#     DVLabel=dsValue$DVLabel,
#     stringsAsFactors=FALSE
#   )
  
  #   intervalCount <- 3
  #   breakPoints <- pretty(dsValuePlot$DV, n=intervalCount)
  #   breakPoints <- seq(from=dvFloor,to=dvCeiling, length.out=intervalCount+1)
  #   print(breakPoints)
  dsValuePlot$CountyNameLower <- tolower(dsValuePlot$CountyName)
  breakPointsPretty <- paste0("Cut points: (", paste(breakPoints, collapse=", "), ")")
  
  # highestFloor <- breakPoints[intervalCount]
  # inHighestCategory <- (dsValuePlot$DV > highestFloor)
  
  
  DvInterval <- function( dv ) {
    return( classIntervals(dv, n=intervalCount, style="fixed", fixedBreaks=breakPoints))  
  }
  ColorsContinuous <- function( dv ) {
    return( findColours(DvInterval(dv), paletteResource) )
  }
  ContrastingColor <-function( color ){
    lightness <- c(0.2, 0.6, 0) %*% col2rgb(color)/255
    return( ifelse( lightness >= 0.4, "#0F0F0F", "#F0F0F0") )
  }
  dsValuePlot$ColorFill <- ColorsContinuous(dsValuePlot$DV)
  dsValuePlot$ColorLabel <-t(ContrastingColor(dsValuePlot$ColorFill))#[!inHighestCategory])) 

  dsBoundary <- map_data(map="county", region="OK")
  dsBoundary$region <- dsBoundary$subregion
  
  g <- ggplot(dsValuePlot, aes_string(map_id="CountyNameLower", color="ColorLabel")) 
  g <- g + geom_map(aes_string(fill="ColorFill"), map=dsBoundary, color="gray60")
  #g <- g + geom_text(aes(label=CountyName, x=long, y=lat)) 
  if( showCountyValues ) {
    g <- g + geom_text(aes_string(label="CountyName", x="LabelLongitude", y="LabelLatitude"), vjust=-.2, size=deviceWidth*.25, na.rm=T)
    g <- g + geom_text(aes_string(label="DVLabel", x="LabelLongitude", y="LabelLatitude"), vjust=1, size=deviceWidth*.35, na.rm=T)
  }
  
  g <- g + expand_limits(x=dsBoundary$long, y=dsBoundary$lat) 
  g <- g + scale_fill_identity(name=dvName)
  g <- g + scale_color_identity()
  g <- g + coord_map()
  # g <- g + theme_bw(base_size=2)
  g <- g + theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks.length=unit(0, "cm"))# + theme(
  g <- g + theme(plot.background=element_blank(), panel.background=element_blank())
  g <- g + theme(legend.position=c(0,0), legend.justification=c("left","bottom"))
  g <- g + theme(plot.margin=unit(c(0, 0, 0, 0), "cm")) #+ theme(panel.margin = unit(c(0, 0, 0, 0), "cm"))
  g <- g + annotate("text", x=titleLocationBottom["x"], y=titleLocationBottom["y"], label=mapTitle, hjust=.5, vjust=0, size=deviceWidth*.7)
  g <- g + annotate("text", x=subtitleLocationMiddle["x"], y=subtitleLocationMiddle["y"], label=paste0("Number of diabetic patients\nreceiving amputations,\namong 1,000 diabetic patients\n", breakPointsPretty), hjust=.5, vjust=.5, size=deviceWidth*.35, lineheight=.85)
  
  return( g )
}

MapCountiesWithInset <- function( 
  dsValueCountyOneYear, 
  deviceWidth=10, colorPower=1, showCountyValues=TRUE, mapTitle="", dvFloor=min(dsValueCountyOneYear$DV), dvCeiling=max(dsValueCountyOneYear$DV), #For the map
  dsValueCountyAllYears, dsValueState, labelThreshold=.01, yearBand=NA, #For Inset
  intervalCount=3, breakPoints=seq(from=dvFloor,to=dvCeiling, length.out=intervalCount+1),
  paletteResource=rev(sequential_hcl(n=intervalCount, h=340, c.=c(80, 0), l=c(40, 90), power=colorPower))) {
  
  
  #Start a new page and define the layout of the panels
  #   grid.newpage()
  #Place the bottom left corner of the inset so it's touchingt the bottom left of the parent panels (with the x, y & just parameters). 
  #   Extend the insert 70% of the way up the parent panel, and 36% across.
  subvp <- viewport(width=.36, height=.7, x=0, y=0, just=c(0,0)) 
  
  big <-  MapCounties(dsValue=dsValueCountyOneYear, deviceWidth=deviceWidth, mapTitle=mapTitle, dvFloor=dvFloor, dvCeiling=dvCeiling,
                      intervalCount=intervalCount, breakPoints=breakPoints, paletteResource=paletteResource
                      )
  
  small <- GraphLongitudinalTrend(dsCounty=dsValueCountyAllYears, dsState=dsValueState, labelThreshold=labelThreshold, yearBand=yearBand)
  #   big
  print( big )
  print( small, vp=subvp ) 
}

GraphLongitudinalTrend <- function( dsCounty, dsState, labelThreshold=.01, yearBand=NA ) {
  g <- ggplot(dsCounty, aes(x=ReferralYear, y=DV, ymin=0, group=CountyID, color=factor(CountyID)))
  if( !is.na(yearBand) )
    g <- g + geom_vline(xintercept = yearBand, alpha=.2, size=20)
  g <- g + geom_line(stat="identity")
  g <- g + geom_line(data=dsState, aes(x=Year, y=DV, group=NA, color=NA), stat="identity", size=1, color="black")
#   g <- g + geom_smooth(data=dsState, aes(x=Year, y=DV, group=NA, color=NA), method="loess", size=3)
  if( !is.na(labelThreshold) )
    g <- g + geom_text(data=dsCounty[dsCounty$DV >labelThreshold, ], aes(x=ReferralYear,label=CountyName), vjust=1, size=4, na.rm=T)
#   
  g <- g + scale_x_continuous(breaks=years)
  g <- g + scale_y_continuous(limits=c(0, max(dsCounty$DV, na.rm=T)), expand=c(0,0))
  g <- g + theme_bw()
  g <- g + theme(legend.position = 'none')
  g <- g + labs(x=NULL, y=NULL)
  return( g )
}
