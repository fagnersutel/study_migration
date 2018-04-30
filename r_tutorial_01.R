

#### R-i & RStudio environment ####
setwd("C:/Users/fsmoura/Desktop/r_tutorial_data/") # define working diretory
study_migration <- read.csv("C:/Users/fsmoura/Desktop/r_tutorial_data/study_migration.csv", sep=";") # import data table
View(study_migration) 
rm(study_migration) # remove object from current project

ls() #show objects in current project
str(study_migration) # structure of the data file
fix(study_migration) # correct datatable
install.packages("ggplot2") # install packages needed in current project
install.packages("ggmap")
install.packages("reshape")
library(ggplot2) # load packages into the project
library(ggmap)
library(reshape2)
library(sp)
library(rgdal)
library(rgeos)

# working with variables
study_migration$Sum <- (study_migration$Tallinn + study_migration$Tartu) # calculate new variable
#study_migration$Sum <- NULL # delete variable
study_migration$Sum_txt <- as.character(study_migration$Tallinn + study_migration$Tartu) # calculate new variable & and store it as text
row.names(study_migration) <- study_migration$ov_short # in some cases you need define row names
study_migration_subset <- subset(study_migration, study_migration$Sum > 100) # subset data 

##### plot results on scatterplot ####
plot(x=study_migration$lon, y=study_migration$lat, col="red") # plot spatial data as xy-scatterplot
symbols(x=study_migration$lon, y=study_migration$lat, circles=study_migration$Sum, bg="red", fg="white") # add number of migrants reflected in circle size

plot(x=study_migration$lon, y=study_migration$lat, col="red")
symbols(x=study_migration$lon, y=study_migration$lat, circles=sqrt(study_migration$Sum), bg="white", fg="red") # circles size is calibrated with SQRT

#### save plot to file (jpg) #####
jpeg("plot1.jpg", res=300, width=8, height=5, unit='in') # defien parameters of the output
plot(x=study_migration$lon, y=study_migration$lat, col="red") # draw plot
symbols(x=study_migration$lon, y=study_migration$lat, circles=sqrt(study_migration$Sum),  fg="red") # add size 
text(x=study_migration$lon, y=study_migration$lat, study_migration$ov_short, cex=0.5) # add municipality names
dev.off() # close jpg writing


#### plot data with ggplot2 package #####
# http://www.statmethods.net/advgraphs/ggplot2.html
# http://inundata.org/2013/04/10/a-quick-introduction-to-ggplot2/
library(ggplot2) # http://docs.ggplot2.org/current/
ggplot()+ geom_point(aes(x=lon, y=lat, size=Sum), data=study_migration)
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=log(Sum)), data=study_migration)

ggplot()+ geom_point(aes(x=lon, y=lat, size=Sum), data=study_migration)
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=Sum), data=study_migration)
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=log(Sum)), data=study_migration)
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=log(Sum)), data=study_migration) + scale_colour_gradient(high="red", low="green") + theme_bw() + labs(size="Number of Migrants")
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=log(Sum)), data=study_migration) + scale_colour_gradient(high="red", low="green") + theme_bw() + labs(size="Number of Migrants")+ guides(colour=FALSE)
ggplot()+ geom_point(aes(x=lon, y=lat, colour=Sum, size=log(Sum)), data=study_migration) + scale_colour_gradient(high="red", low="green") + theme_bw() + labs(size="Number of Migrants", title="Some Migration")+ guides(colour=FALSE)

#### plot spatial data on ggmap ###
library(ggplot2)
library(ggmap)

est_map <- ggmap(get_map(location="estonia", maptype="satellite", zoom = 7)) # create basemap

# dot map (xy-locations as dots on map [ggmap & ggplot2])
est_map + geom_point(aes(x=lon, y=lat, fill=Sum, size=log(Sum)), shape=21, alpha=0.7, data=study_migration) + scale_fill_gradient(high="red", low="green") + labs(fill="Number of \nMigrants", title="Some Migration")+ guides(size=FALSE)


# line map (connect locations with each other)
# connect every municipality with Tallin or Tartu. Connect municipality with stronger destination
study_migration$Destination_1 <- ifelse(study_migration$Tallinn > study_migration$Tartu, "Tallinn", "Tartu") # add new variable. If Tallinn is more important destination "Tallinn", else "Tartu"

Tartu_main <- subset(study_migration, Destination_1 =="Tartu")
Tallinn_main <- subset(study_migration, Destination_1 =="Tallinn")
Tartu_main$lat_tartu <- Tartu_city$lat
Tartu_main$lat_tartu <- Tartu_main$lat
Tartu_main$lon_tartu <- Tartu_city$lon
Tartu_main$lon_tartu <- Tartu_main$lon
Tallinn_main$lat_tallinn <- Tallinn_main$lat
Tallinn_main$lon_tallinn <- Tallinn_main$lon
est_map + geom_segment(aes(x=lon, y=lat, xend=lon_tallinn, yend=lat_tallinn), colour="cyan", data=Tallinn_main)+ geom_segment(aes(x=lon, y=lat, xend=lon_tartu, yend=lat_tartu), colour="orange", data=Tartu_main)

est_map + geom_segment(aes(x=lon, y=lat, xend=lon_tallinn, yend=lat_tallinn), colour="cyan", data=Tallinn_main)+ geom_segment(aes(x=lon, y=lat, xend=lon_tartu, yend=lat_tartu), colour="orange", data=Tartu_main) + geom_text(aes(x=lon, y=lat, label="Tartu"), colour="red", size=7, data=Tartu_city)+ geom_text(aes(x=lon, y=lat, label="Tallinn"), colour="blue", size=7, data=Tallinn_city)


### create polygon map (choropleth) with ggplot2 & ggmap
### based on example from http://mazamascience.com/WorkingWithData/?p=1494
localDir <- "r_polygons"  # name of the subfolder with GIS-layer [shp]
# chropleth map with ggplot2
layerName <- "population_census_2011"
dataProjected <- readOGR(dsn=localDir, layer=layerName)
dataProjected@data$id <- rownames(dataProjected@data)
population_census_2011i <- fortify(dataProjected, region = "id")
population_census_2011iPoints <- fortify(dataProjected, region = "id")
population_census_2011iDF <- merge(population_census_2011iPoints, dataProjected@data, by = "id")
ggpopulation_census_2011iDF <- ggplot(data = population_census_2011iDF, aes(x=long, y=lat, group = group,
                                                                            fill = log(rahvaarv))) +
  geom_polygon()  +
  geom_path(color = "white") +
  coord_equal() +
  theme_bw() + 
  scale_fill_gradient(high="red", low="green")
theme(legend.position = "none", title = element_blank(),
      axis.text = element_blank())
print(ggpopulation_census_2011iDF)

# choropleth map with ggplot2 & ggmap
# reproject the data onto a "longlat" projection
dataTransform <- spTransform(dataProjected, CRS("+proj=longlat"))

dataTransformFortified <- fortify(dataTransform, region = "id")
dataTransformFortified <- merge(dataTransformFortified,
                                dataTransform@data, by.x = "id")
est_map + geom_polygon(data = dataTransformFortified,
                       aes(x = long, y = lat, group = group,
                           fill = log(rahvaarv)), alpha=0.7) +
  scale_x_continuous(limits = c(b[1,1],b[1,2])) +
  scale_y_continuous(limits = c(b[2,1],b[2,2])) +
  theme(legend.position = "none", title = element_blank()) + 
  scale_fill_gradient(high="red", low="green")



