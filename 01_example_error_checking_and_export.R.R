# Load your data 
pro <- read.csv("data/raw_data/example_data/proj.csv", header=T)
img <- read.csv("data/raw_data/example_data/img.csv", header=T)
dep <- read.csv("data/raw_data/example_data/dep.csv", header=T)
cam <- read.csv("data/raw_data/example_data/cam.csv", header=T)


#Load Packages
list.of.packages <- c(
  "leaflet",       # creates interactive maps
  "plotly",        # creates interactive plots   
  "kableExtra",    # Creates interactive tables 
  "tidyr",         # A package for data manipulation
  "dplyr",         # A package for data manipulation
  "viridis",       # Generates colors for plots  
  "corrplot",      # Plots pairwise correlations
  "lubridate",     # Easy manipulation of date objects
  "taxize",        # Package to check taxonomy 
  "sf")            # Package for spatial data analysis 

# Check you have them in your library
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# load them
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)


## Skill check: lubridate

library(lubridate)
# day-month-year
dmy("24-12-2022")

# year-month-day
ymd("2022-12-24")

x <- c("24-12-2022", "2022-12-24", "12-24-2022") #Three different date formats
parse_date_time(x, c("ymd", "dmy", "mdy"))

# Specify your start and end dates
start <- ymd("2021-10-13")
end <- ymd("2021-12-11")

# Specify the interval, and put it in days
interval(start, end)/ddays(1)

# Interval creates an "interval object" - run that along and see what it looks like
# ddays() converts the native units of date objects in R (seconds) to days - run it on its own to see.

# Specify the interval, and put it in weeks
interval(start, end)/ddays(7)


interval(start, end)/ddays(365)


## Deployment dates


# start dates
dep$start_date <- ymd(dep$start_date)

# end dates
dep$end_date <- ymd(dep$end_date)

dep$days <- interval(dep$start_date, dep$end_date)/ddays(1)


summary(dep$days)

dep[is.na(dep$days)==T,] %>% 
  kbl() %>% 
  kable_styling(full_width = T) %>% 
  kableExtra::scroll_box(width = "100%")


## Image dates
ymd_hms("2015-11-21 03:03:44")

img$timestamp <- ymd_hms(img$timestamp)

range(img$timestamp)

table(is.na(img$timestamp))

# Count the number of camera locations
paste(length(unique(dep$placename)), "locations"); paste(length(unique(dep$deployment_id)), "deployments");paste(nrow(img), "image labels"); paste(nrow(img[img$is_blank == TRUE,]), "blanks")


m <- leaflet() %>%             # call leaflet
  addTiles() %>%         # add the default basemap
  addCircleMarkers(      # Add circles for stations
    lng=dep$longitude, lat=dep$latitude) 
m                              # return the map


m <- leaflet() %>%             
  addTiles() %>%         
  addCircleMarkers(      
    lng=dep$longitude, lat=dep$latitude,
    popup=paste(dep$placename)) # include a popup with the placename!
m                              

m <- leaflet() %>%             
  addProviderTiles(providers$Esri.WorldImagery) %>% #Add Esri Wrold imagery         
  addCircleMarkers(      
    lng=dep$longitude, lat=dep$latitude,
    popup=paste(dep$placename)) # include a popup with the placename!
m                              

dep[dep$placename=="ALG069",c("deployment_id", "placename", "longitude", "latitude")]

dep$longitude[dep$placename=="ALG069"] <- -112.5075


# First, set a single categorical variable of interest from station covariates for summary graphs. If you do not have an appropriate category use "project_id".
category <- "feature_type"

# We first convert this category to a factor with discrete levels
dep[,category] <- factor(dep[,category])
# then use the turbo() function to assign each level a color
col.cat <- turbo(length(levels(dep[,category])))
# then we apply it to the dataframe
dep$colours <- col.cat[dep[,category]]

m <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%  
  addTiles(group="Base") %>%     # Include a basemap option too
  addCircleMarkers(lng=dep$longitude, lat=dep$latitude,
                   # Co lour the markers depending on the 'feature type'
                   color=dep$colours,
                   # Add a popup of the placename and feature_type together 
                   popup=paste(dep$placename, dep[,category])) %>%
  
  # Add a legend explaining what is going on
  addLegend("topleft", colors = col.cat,  labels = levels(dep[,category]),
            title = category,
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  
  # add a layer control box to toggle between the layers
  addLayersControl(
    baseGroups = c("Satellite", "Base"))

m


# create a list of all the non-duplicated placenames
camera_locs <- dep %>% 
  dplyr::select(placename, latitude, longitude) %>% 
  unique() %>% # remove duplicated rows (rows where the placename and coordinates match)
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat") # Convert to `sf` format

# Check that there are no duplicated stations
camera_locs[duplicated(camera_locs$placename)==T,]


# distance matrix for all cameras
camera_dist <- st_distance(camera_locs) %>% 
  as.dist() %>% 
  usedist::dist_setNames(as.character(camera_locs$placename)) %>% 
  as.matrix()

# convert to pairwise list
camera_dist_list <- t(combn(colnames(camera_dist), 2))
camera_dist_list <- data.frame(camera_dist_list, dist = camera_dist[camera_dist_list]) %>% 
  arrange(dist) # sort descending

# Duplicate and flip the stations so each one is represented on the left hand side
camera_dist_list <- rbind(camera_dist_list, camera_dist_list[,c(2,1,3)])

# keep just the closest camera for each location
camera_dist_list <- camera_dist_list %>% 
  group_by(X1) %>% 
  slice(which.min(dist))

summary(camera_dist_list$dist)

# check all check the placenames in images are represented in deployments
# This code returns TRUE if it is and FALSE if it isn't. We can then summarize this with table()
table(unique(img$placename) %in% unique(dep$placename))

# check all the placenames in deployments are represented in the images data
table(unique(dep$placename)  %in% unique(img$placename))


library(plotly)
library(ggplot2)
fig <- plot_ly(data = dep,                    # Specify your data frame
               x = ~longitude, y = ~latitude, # The x and y axis columns
               type="scatter")                # and the type of plot
fig
