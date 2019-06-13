setwd("~/Projects/Potholes")

library(raster) 
library(plyr)
library(dplyr)
library(ggmap) 
library(rgdal) 

merge <- base::merge
select <- dplyr::select
count <- plyr::count

# Import raw potholes data
raw.data <- read.csv('data.csv')

raw.data$month <- raw.data$CREATION.DATE %>% as.Date("%m/%d/%Y") %>% format('%m') %>% as.numeric
raw.data$year  <- raw.data$CREATION.DATE %>% as.Date("%m/%d/%Y") %>% format('%Y') %>% as.numeric
raw.data$Census.Tracts <- raw.data$Census.Tracts %>% as.numeric

# Import shapefile
sf <- readOGR('geo_export_2f2f5e33-355e-4c85-876d-e9c0807cc9a5.shp')
sf.points <- sf %>% fortify

sf.points$Census.Tracts <- sf.points$id %>% as.numeric + 1

# Count pothole reports in each Tract by month,
potholes <- raw.data %>% count(vars=c('Census.Tracts', 'month', 'year'))

# Calculate size of each Tract to get a normalized pothole count
sizes <- sf %>% area %>% as.data.frame
colnames(sizes)[1] <- 'size'
sizes$Census.Tracts <- sizes %>% rownames

potholes <- merge(x=potholes, y=sizes, 
                  by='Census.Tracts', 
                  all.x=TRUE)

potholes$size.km <- (potholes$size / 1e+6)
potholes$freq.per.km <- (potholes$freq / potholes$size.km)

# Merge columns for pothole count in each month
sf.points <- merge(x=sf.points, y=potholes[potholes$month == 1 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                   by='Census.Tracts', 
                   all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Jan'
sf.points[is.na(sf.points$Jan),]$Jan <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 2 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Feb'
sf.points[is.na(sf.points$Feb),]$Feb <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 3 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Mar'
sf.points[is.na(sf.points$Mar),]$Mar <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 4 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Apr'
sf.points[is.na(sf.points$Apr),]$Apr <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 5 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'May'
sf.points[is.na(sf.points$May),]$May <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 6 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Jun'
sf.points[is.na(sf.points$Jun),]$Jun <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 7 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Jul'
sf.points[is.na(sf.points$Jul),]$Jul <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 8 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Aug'
sf.points[is.na(sf.points$Aug),]$Aug <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 9 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Sep'
sf.points[is.na(sf.points$Sep),]$Sep <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 10 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Oct'
sf.points[is.na(sf.points$Oct),]$Oct <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 11 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Nov'
sf.points[is.na(sf.points$Nov),]$Nov <- 0

sf.points <- merge(x=sf.points, y=potholes[potholes$month == 12 & potholes$year == 2014,] %>% select('Census.Tracts', 'freq.per.km'), 
                       by='Census.Tracts', 
                       all.x=TRUE)
colnames(sf.points)[colnames(sf.points) == 'freq.per.km'] <- 'Dec'
sf.points[is.na(sf.points$Dec),]$Dec <- 0

# Return sf.points to proper order
sf.points <- sf.points[order(sf.points$order),]

# Load Map Background from Google
register_google(token)

mapImage <- get_map(location = c(lon = -87.68, lat = 41.85), #c(-88.0, 41.63, -87.4, 42.1),
                    color = "bw",
                    maptype = "toner-background",
                    source = 'stamen',
                    zoom = 10)

# Set Visualization Formatting
scale_grad <- scale_fill_gradient2(limits = c(0, 50),
                                   low = 'white',
                                   high = 'red4',
                                   midpoint = 10,
                                   na.value = 'red4')

coord_lims <- coord_map(xlim = c(-87.95, -87.4),
                        ylim = c(41.63, 42.05))

themes <- theme(legend.position = c(0.95, 0.28),
                legend.background = element_blank(),
                legend.text = element_text(colour = 'white', size=7),
                legend.title = element_text(color = 'white', size=7.5),
                axis.title = element_blank(), 
                axis.text = element_blank()) 

label <- labs(fill = "freq/km²")

ann_x <- -87.435
ann_y <- 42.035
ann_size <- 7

# Visualize Maps
ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Jan),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'JAN')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Feb),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'FEB')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Mar),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'MAR')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Apr),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'APR')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = May),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'MAY')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Jun),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'JUN')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Jul),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'JUL')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Aug),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'AUG')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Sep),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'SEP')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Oct),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'OCT')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Nov),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'NOV')

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Dec),
               data = sf.points,
               color = 'white',
               alpha = .9) + 
  scale_grad + coord_lims  + themes + label + 
  annotate('text', x= ann_x, y = ann_y, size = ann_size, color = 'white', label = 'DEC')
