
#Example of bivariate plot for blsh_trk fish for Sanctuaries Futures
#This script used the function to create bivariate plots and identify
## climate refugia, bright spots, habitat loss and unsuitable habitat
#Based on average habitat suitability for the historical (1985-2015) and future (2070-2100) period
##Written by NLO. July, 2023


#load libraries
rm(list=ls())
require(classInt)
require(data.table)
require(ggplot2)
require(raster)
library(biscale)
library(cowplot)
library(patchwork)
library(maptools)

setwd("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\FINAL_RESULTS\\new\\maps\\habitat/grd_files//")



###############
#1-Comparison between historical period (1985-2015) and future (2070-2100) period
################
species="blsh_trk"
#historical hbhw suitable habitat for 1985-2015
gfdl=raster("blsh_trk_Habitat 1985_2015_GFDL_.grd")
had=raster("blsh_trk_Habitat 1985_2015_HAD_.grd")
ipsl=raster("blsh_trk_Habitat 1985_2015_IPSL_.grd")


present=stack(gfdl,had,ipsl)
present=mean(present)


gfdl=raster("blsh_trk_Habitat 2070_2100_GFDL_.grd")
had=raster("blsh_trk_Habitat 2070_2100_HAD_.grd")
ipsl=raster("blsh_trk_Habitat 2070_2100_IPSL_.grd")

future=stack(gfdl,had,ipsl)
future=mean(future)


#bivariate
prueba=stack(present,future)
names(prueba) <- c("present","future")
dat <- setDT(as.data.frame(prueba, xy = TRUE, na.rm = TRUE))


#number of breaks
nBreaks <- 2


clipExt=c(-134, -115.5, 30, 48)



# Create the colour matrix
col.matrixQ <- colmat(nbreaks = nBreaks, breakstyle = "fisher",
                      xlab = xlab, ylab = ylab, 
                      bottomright = "#BC7C8F", upperright = "#806A8A",
                      bottomleft = "#CABED0", upperleft = "#89A1C8",
                      saveLeg = FALSE, plotLeg = TRUE)

bivmapQ <- bivariate.map(rasterx = future[["layer"]], rastery = present[["layer"]],
                         export.colour.matrix = T,
                         colourmatrix = col.matrixQ)




# Convert to dataframe for plotting with ggplot
bivMapDFQ <- setDT(as.data.frame(bivmapQ, xy = TRUE))
colnames(bivMapDFQ)[3] <- "BivValue"
bivMapDFQ <- melt(bivMapDFQ, id.vars = c("x", "y"),
                  measure.vars = "BivValue",
                  value.name = "bivVal",
                  variable.name = "Variable")


bivMapDFQ=as.data.frame(bivMapDFQ)
head(bivMapDFQ)
str(bivMapDFQ)

#get silhouette for blsh_trkh
library(grid)
library(png)
library(ggplot2)
library(gridExtra)
mypng = readPNG('C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\images\\black/blsh.png') 
g <- rasterGrob(mypng, interpolate=T)

bivMapDFQ=na.omit(bivMapDFQ)



#1- Get 2º quantile break values for mean and sd
#mean

mean <- getValues(present)
tempx <- data.frame(mean, quantile = rep(NA, length(mean)))



colourmatrix = col.matrixQ
brks <- with(tempx, classIntervals(mean,
                                   n = attr(colourmatrix, "nbreaks"),
                                   style = attr(colourmatrix, "breakstyle"))$brks)




#sd
present=stack(gfdl,had,ipsl)
present_sd <- calc(present, fun = sd)

sd <- getValues(present_sd)



tempx <- data.frame(sd, quantile = rep(NA, length(sd)))
brks_sd <- with(tempx, classIntervals(sd,
                                      n = attr(colourmatrix, "nbreaks"),
                                      style = attr(colourmatrix, "breakstyle"))$brks)

brks_sd
brks <- round(brks, digits = 2)
brks_sd <- round(brks_sd, digits = 2)


sd_1=brks[2]+brks_sd[2]
sd_2=brks[2]-brks_sd[2]
sd_1
sd_2
xlab=paste0(brks[2]," +/- SD (",brks_sd[2],")")
ylab=paste0(brks[2]," +/- SD (",brks_sd[2],")")



# Create the colour matrix
col.matrixQ <- colmat(nbreaks = nBreaks, breakstyle = "fisher",
                      xlab = xlab, ylab = ylab, 
                      bottomright = "#BC7C8F", upperright = "#806A8A",
                      bottomleft = "#CABED0", upperleft = "#89A1C8",
                      saveLeg = FALSE, plotLeg = TRUE)



#filter positions that fall within the 2ºquantile break +/- sd
gfdl=raster("blsh_trk_Habitat 1985_2015_GFDL_.grd")
had=raster("blsh_trk_Habitat 1985_2015_HAD_.grd")
ipsl=raster("blsh_trk_Habitat 1985_2015_IPSL_.grd")

present=stack(gfdl,had,ipsl)
present=mean(present)
present=as.data.frame(present, xy=T)
present=na.omit(present)
head(present)
max(present$layer)

future=as.data.frame(future, xy=T)
future=na.omit(future)
head(future)
max(future$layer)


#for both the historical and future period
new_frame2 <- present%>% filter( between(present$layer, sd_2, sd_1) )
new_frames_f <- future%>% filter( between(layer, sd_2, sd_1) )


dfr <- rasterFromXYZ(new_frame2)
dfr2 <- rasterFromXYZ(new_frames_f)



# Make the map using ggplot
map_q <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  #geom_raster(aes(fill = bivVal)) +
  geom_raster(aes(fill = bivVal)) +borders(xlim = c(-130,-115), ylim = c(30,47),fill="grey",colour="grey") +
   geom_point(data=new_frame2, aes(x=x, y=y), size=1, shape = 18,colour="white", fill="white")+
  geom_point(data=new_frames_f, aes(x=x, y=y), size=1, shape = 18,colour="white", fill="white")+
  # scale_y_continuous(breaks = seq(0, 1, by = 0.01), 
  # labels = paste0(seq(0, 1, 0.01), "°")) +
  # scale_x_continuous(breaks = seq(-130,-115,2), 
  #labels = paste0(seq(-130,-115,2), "°")) +
  annotation_custom(g,xmin = -116, xmax = -120,ymin = 45, ymax = 46.7) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.5) +
  coord_quickmap(expand = FALSE, xlim = c(-130,-115), ylim = c(30,47)) +
  
  theme(legend.position = "none",
        plot.background = element_blank(),
        #strip.text = element_text(size = 12, colour = "black"),
        #axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")
map_q

#lead sanctuaries boundaries
Monterey <- readShapeSpatial("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\MPA_megan\\NMS\\mbnms_py2/mbnms_py.shp")
Cordell <- readShapeSpatial("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\MPA_megan\\NMS/cbnms_py2/CBNMS_py.shp")
Farallons <- readShapeSpatial("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\MPA_megan\\NMS/gfnms_py2/GFNMS_py.shp")
Channel <- readShapeSpatial("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\Results_HMS\\MPA_megan\\NMS/cinms_py2/cinms_py.shp")



#create final map
map_q=map_q+ geom_polygon(data = Monterey, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
  geom_polygon(data = Cordell, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
  geom_polygon(data = Farallons, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
  geom_polygon(data = Channel, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
  ggtitle("A) hist_NF",species)




myPlot <- cowplot::ggdraw() +
  cowplot::draw_plot(map_q +geom_polygon(data = Monterey, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
                       geom_polygon(data = Cordell, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
                       geom_polygon(data = Farallons, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)+
                       geom_polygon(data = Channel, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size=1)
                     , 0, 0, 1, 1) +
  cowplot::draw_plot(BivLegend, 0.6, 0.6, 0.2, 0.2)
myPlot
setwd("C:\\Users\\nereo\\Dropbox (Personal)\\Nerea\\NOAA\\PROJECTS & COLLABORATIONS\\PROJECTS\\Sanctuaries\\Paper_Refugia\\2_Species\\bivariate/NEW")
ggsave(paste0("",species,".png"), width = 20, height = 20, units = "cm")

