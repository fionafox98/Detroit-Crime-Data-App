library(sf)
library(readr)
library(ggplot)
library(ggthemes)
library(gganimate)
library(foreign)
library(dplyr)
library(lubridate)
library(tidyverse)
library(wesanderson)
library(rgdal)
library(RStoolbox)
library(geogrid)
library(osmdata)
library(ggsn)
library(osmdata)
library(ggmap)
install.packages("ggspectra")
library(ggspectra)
library(leaflet)
library(data.table)

# Set up working directory and import data 
detroit <- st_read("crime_clip.shp")
dim(detroit)
#plot(st_geometry(detroit), col = sf.colors(12, categorical = TRUE), border = 'grey', 
#  axes = TRUE)

# Drop geometry
crime <- st_drop_geometry(detroit) 

# Convert to date format
crime$date <- as.POSIXct(crime$INCIDENTDA, origin="2011-01-01")
crime$date <- as.Date(format(crime$date, format="%Y-%m-%d"))
crime$dayofweek <- weekdays(as.Date(crime$date))
#crime$year <- format(crime$date, "%Y")

# Plot daily values
crime_daily <- crime %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  group_by(date) %>%
  summarize(count = n())

plot1 <- ggplot(crime_daily, aes(x = date, y = count)) +
  geom_line(color = "bisque2", size = 0.1) + ##F2CA27
  geom_smooth(color = "chocolate", method = "auto") + theme_classic() +
  labs(x = "Date of Crime", y = "Number of Crimes", title = "Daily Crimes in Detroit from 2011 - 2014")
plot1

# Plot monthly
crime$month_year <- floor_date(crime$date, unit = "month")
crime$year <- year(crime$date)
ts_crime <- crime %>%
  group_by(month_year) %>% 
  summarize(count = n())

plot2 <- ggplot(ts_crime, aes(x = month_year, y = count)) +
  geom_line() + 
  labs (title="Timeseries of Crime in Detroit from 2011 to 2014", 
        y="Total count", x = "Timeline") +
  stat_peaks(colour = "red") + stat_peaks(geom = "text", colour = "red", angle = 20,
                                          vjust = 1, hjust = 1,  x.label.fmt = "%d%B") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 0,
               vjust = 1, hjust = 1,  x.label.fmt = "%d%B") + theme_classic()
plot2

# Data categorization by type
data_category <- sort(table(crime$CATEGORY),decreasing = TRUE)
data_category <- data.frame(data_category)
colnames(data_category) <- c("Category", "Frequency")
data_category$percentage <-round((data_category$Frequency) / sum(data_category$Frequency) * 100, 2)
table <- as.data.table(data_category)
table

plot3<- ggplot(crime, aes(x = CATEGORY)) + 
  geom_bar(fill="darkolivegreen2") + theme_classic()
plot3

# Crime trend by type
filter1 <- crime %>% 
  filter(grepl("LARCENY", CATEGORY)) %>%
  mutate(Date = as.Date(date, "%m/%d/%Y")) %>%
  group_by(Date) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

plot4 <- ggplot(filter1, aes(x = Date, y = count)) +
  geom_line(color = "azure3", size = 0.1) +
  geom_smooth(color = "#1A1A1A") +
  labs(x = "Date of .... ", y = "Counts", title = "Daily ..... in Detroit from 2011 - 2014") + 
  theme_classic()
plot4

# Group by category and hour
week_crime <- crime %>%
  mutate(weekday =  weekdays(as.Date(crime$date))) %>% 
  group_by(weekday, CATEGORY, HOUR) %>% # group by the day column
  summarise(count = n())

hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
week_crime$HOUR <- factor(week_crime$HOUR, level = 0:23, label = hour_format)

week_crime <- week_crime %>%
  group_by(CATEGORY) %>%
  mutate(norm = count/sum(count ))

plot5 <- ggplot(week_crime, aes(x = HOUR, y = weekday, fill = norm)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Crime", y = "Day of Week", title = "Number of Crime in Detroit from 2011 - 2014 by Category and Time") +
  scale_fill_gradient(low = "white", high = "#d95f0e") +
  facet_wrap(~ CATEGORY)
plot5

# Factor by Month
crime_month <- crime %>%
  mutate(month =  format(as.Date(date, "%m/%d/%Y"), "%B")) %>%
  group_by(month, dayofweek, HOUR) %>% 
  summarize(count = n()) %>%
  group_by(month) %>%
  mutate(norm = count/sum(count))

hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
crime_month$HOUR <- factor(crime_month$HOUR, level = 0:23, label = hour_format)

plot6 <- ggplot(crime_month, aes(x = HOUR, y = dayofweek, fill = norm)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Crime by Hour", y = "Day of Week", title = "Crime in Detroit from 2011 - 2014, Normalized by Month") +
  scale_fill_gradient(low = "white", high = "#d95f0e") +
  facet_wrap(~ month, nrow = 4)
plot6

# Plotting crime density map by year
plot7 <- ggmap::qmplot(x=LON,y=LAT, data = crime, 
                       maptype = "toner", extent = "normal", zoom = 10,
                       darken = .2, geom = "blank", 
                       main = "Crime density plot for Detroit from 2011-2014", f = 0.2, 
                       xlab = "", ylab = "")  + theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  stat_density_2d(aes(fill = ..level..),  
                  geom = "polygon") + 
  scico::scale_fill_scico(palette = "lajolla") +
  guides(fill=guide_legend(title="Density"))
plot7

# Hotspot with hexbin
detroit_map <- ggmap(get_map(c(-83.3052,42.238,-82.8723,42.5399), 
                             source = "stamen")) # bounding box from https://boundingbox.klokantech.com/
plot8 <- detroit_map + stat_binhex(aes(x = LON, y = LAT),
                                   bins = 100, alpha = .8, 
                                   data = crime) +
  coord_cartesian() + 
  scale_fill_gradient('Crime', low = "#ffeda0", high = "#f03b20") +
  labs(x= "Longitude", y ="Latitude", main = "Count") +
  facet_wrap(.~ CATEGORY)
plot8

# Neighborhood shapefile
neighborhood <- st_read("Current_City_of_Detroit_Neighborhoods.shp")
plot(neighborhood$geometry)
st_crs(neighborhood) # crs of the shapefile 

# Convert to coordinate reference system 
neighborhood <- st_transform(neighborhood, crs = 2898)
st_crs(neighborhood)
detroit_proj <- st_transform(detroit, crs = st_crs(neighborhood)) # keeping the projection constant

# Spatial join
plot(neighborhood$geometry)
plot(detroit_proj$geometry, add = TRUE, col = "darkseagreen2")

detroit_agg <- st_join(detroit_proj, neighborhood)
detroit_agg <- detroit_agg %>% 
  group_by(nhood_name) %>% # group by neighborhood 
  summarize(count = n())

summary(detroit_agg$count)
detroit_agg$geometry <- NULL # removing geometry column 

detroit_nhood_crime <- left_join(neighborhood, detroit_agg) # performing join
summary(detroit_nhood_crime$count)

plot9<- ggplot(detroit_nhood_crime, aes(fill = count)) + 
  geom_sf() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(fill = "# count",
       title = "Crime in Detroit, by Neighborhood",
       subtitle = "2011 - 2014") + theme_classic() 
plot9

# Interactive map with popup information
crime$popup <- paste("<b> Category: </b>", crime$CATEGORY,
                     "<br>", "<b>Year: </b>", crime$year, 
                     "<br>", "<b>Day of week: </b>", weekdays(as.Date(crime$date)),
                     "<br>", "<b>", "<b>Incident Time (hr): </b>", crime$HOUR,
                     "<br>", "<br>Neighbourhood: </b>", crime$NEIGHBORHO,
                     "<br>", "<br>", "<b>Longitude: </b>", crime$LON,
                     "<br>", "<b>Latitude: </b>", crime$LAT)
leaf <- leaflet(crime, width = "100%") %>% addTiles() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(provider = "CartoDB.Positron",group = "Carto") %>%
  addProviderTiles(provider = "Esri.WorldImagery",group = "ESRI") %>%
  addMarkers(lng = ~LON, lat = ~LAT, popup = crime$popup, clusterOptions = markerClusterOptions()) %>%
  addLayersControl(
    baseGroups = c("OSM (default)","Carto", "ESRI"),
    options = layersControlOptions(collapsed = FALSE))

# Animation by CATEGORY
animate <- crime %>%
  filter(CATEGORY == "LARCENY")
neighborhood <- st_read("Current_City_of_Detroit_Neighborhoods.shp")
map <- ggplot(neighborhood) + geom_sf(aes(), fill = NA, alpha = .1, col = "darkorchid2")  +  
  theme_classic() + theme(legend.position = "none")
map
ps <- map + theme_void() + 
  geom_point(data = animate, aes(LON, LAT), inherit.aes = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
ps + transition_time(month_year) +
  labs(title = 'Date: {format(frame_time, "%b %Y")}') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# Animation of heat map
map <- get_map(c(-83.3052,42.238,-82.8723,42.5399), zoom = 10)
ggmap(map)
map_animate <- ggmap(map, extent = "device") +
  stat_density_2d(aes(x = LON, y = LAT,
                      fill = ..level..),
                  data=crime, geom = "polygon") +
  scale_fill_gradient(low = "#fee0d2", high = "#de2d26") +
  transition_states(month_year) + 
  labs(title = "Crime by Month") + facet_wrap(.~ CATEGORY)
map_animate

# Miscellaneous animation 
filter_cat<- crime %>% 
  filter(CATEGORY == 'LARCENY')

p1 <- ggplot(neighborhood, extent = "device") +
  geom_point(data = filter_cat, aes(x = LON, y = LAT, fill = year),
             size = 2, shape = 10, alpha = 0.7) + 
  labs( title = 'XYZ') +
  transition_states(year, transition_length = 1, state_length = 2) +
  enter_fade() +
  exit_fade() 
p1  

library(gapminder)


p2 <- ggplot(week_crime, aes(x = weekday, y = count, group = CATEGORY, color = factor(CATEGORY))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature")
p2



aes(x = HOUR, y = weekday, fill = norm)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6, size = 4)) +
  labs(x = "Hour of Crime", y = "Day of Week", title = "Number of Crime in Detroit from 2011 - 2014 by Category and Time") +
  scale_fill_gradient(low = "white", high = "#d95f0e") 


hour_format <- c(paste(c(12,1:11),"AM"), paste(c(12,1:11),"PM"))
week_crime$HOUR <- factor(week_crime$HOUR, level = 0:23, label = hour_format)
week_crime$weekday <- factor(week_crime$weekday, levels= c("Sunday", "Monday", 
                                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
week_crime[order(week_crime$weekday), ]
library(shiny)
ui <- fluidPage(titlePanel("Crime Data in Detroit"), 
                sidebarLayout(sidebarPanel(selectInput(inputId = "CATEGORY",
                  label = strong("Category of Crime"),
                  choices = unique(crime$CATEGORY) selected = "LARCENY"))),
                mainPanel(leafletOutput("leaf"))
server <- function(input, output) {
  output$leaf <- renderPlot({
  leaf <- leaflet(crime, width = "100%") %>% addTiles() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(provider = "CartoDB.Positron",group = "Carto") %>%
    addProviderTiles(provider = "Esri.WorldImagery",group = "ESRI") %>%
    addMarkers(lng = ~LON, lat = ~LAT, popup = crime$popup, clusterOptions = markerClusterOptions()) %>%
    addLayersControl(
      baseGroups = c("OSM (default)","Carto", "ESRI"),
      options = layersControlOptions(collapsed = FALSE))
    
  })
  


shinyApp(ui = ui, server = server)
