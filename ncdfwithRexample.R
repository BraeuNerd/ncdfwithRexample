# For a more detailed explanation of this script you can read the full tutorial here: https://towardsdatascience.com/how-to-crack-open-netcdf-files-in-r-and-extract-data-as-time-series-24107b70dcd

# Load packages -------------------------------------------------

library(ncdf4) #package for netcdf manipulation; unlike ncdf package, ncdf4 supports both NetCDF3 and netCDF4 formats.
library(tidyverse) #because who can live without the tidyverse -- data wrangling & ggplot

# Load data -----------------------------------------------------

our_nc_data <- nc_open("GloboLakes/LAKE00001479-GloboLakes-L3S-LSWT-v4.0-fv01.0.nc")
print(our_nc_data)

# Start exploring and extracting --------------------------------

attributes(our_nc_data$var) #Gives out variable names (e.g. 6 vars from this file)
attributes(our_nc_data$dim)

## Get Lat and Lon

lat <- ncvar_get(our_nc_data,"lat") #get Latitudes into its own vector/object
nlat <- dim(lat)
lat

lon <- ncvar_get(our_nc_data, "lon") #get Longitudes into its own vector/object
nlon <- dim(lon)
lon

print(c(nlon, nlat))

# Now we get the time variable
time <- ncvar_get(our_nc_data,"time") #get observation_times (reminder: GloboLakes time units are in seconds since 01-01-1981 00:00:00)
head(time)
tunits <- ncatt_get(our_nc_data, "time","units") #units, days since string (make readable dates & time later)
nt <- dim(time)

#Get the variable of interest! Today: LSWT. We keep using the get_attributes function ncatt_get().
lswt_array <- ncvar_get(our_nc_data, "lake_surface_water_temperature") #get the variable in matrix slices

fillvalue <- ncatt_get(our_nc_data, "lake_surface_water_temperature", "_FillValue")
dim(lswt_array)

# replace nc Fillvalues with  R NAs
lswt_array[lswt_array==fillvalue$value] <- NA
lswt_array
#each "slice" is a point in time; 23 cols = Lat, 24 rows = Lon
dim(lswt_array)


# Fix to readable time -----------------------------------------

#SO FAR SO GOOD! But we still have unreadable (at least not human-readable) dates.
#Also, we are still dealing with a 4D situation.
#Let's get to it

#our print(tunits) remind us that our time origin is seconds since 1981-01-01 00:00:00
time_obs <- as.POSIXct(time, origin = "1981-01-01", tz="GMT")
dim(time_obs) #2622
range(time_obs) #Range: 28 june 1995 - 31 december 2016

#Looks good

# Play around a bit to understand the array slices -------------

#NOT continuous time series yet, just dates with LSWT data. 

#Trying it out
lswt_slice <- lswt_array[ , , 2123] #ex. [lon, lat, TIME] - leave lon and lat empty to try them all, change LSWT at datetime; try different ones
image(lon, lat, lswt_slice)

# After trying several random slices between 1-2622, I noticed there are a lot of missing values around the center, so I double checked the range of lat and lon in which the lake exists and the range the dataset includes
## Turns out: a lot of space in the grid is over land, not lake, and the lake is just between aprox: lat 14.61-14.75 and lon: -91.3 - -91.10

# Let's build the time series dataset/dataframe ----------------

## To make the final dataframe smaller, let's get rid of the land-coords
## first create the whole matrix so not to lose the matching times and temps

#Create 2D matrix of long, lat and time
lonlattime <- as.matrix(expand.grid(lon,lat,time_obs)) # this might take several seconds
head(lonlattime)
dim(lonlattime)

#reshape whole array
lswt_vec_long <- as.vector(lswt_array)
lswt_vec_long
length(lswt_vec_long) # by now it should be 1447344

#Create data.frame
lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
head(lswt_obs)

#change column names
colnames(lswt_obs) <- c("Long","Lat","Date","LSWT_Kelvin")
head(lswt_obs)

# remove the NAs (full train of thought as to *why I did this* _here: link al post_)
lswt_final <- na.omit(lswt_obs)
head(lswt_final)
dim(lswt_final)


# Clean it up a bit --------------------------------------------

#remove lat lon columns:
lswt_final <- lswt_final[-c(1:2)]

#if you inspect the tibble of lswt_final or use glimpse(lswt_final), LSWT Kelvin is still in character type, so we cannot yet take the mean of its values.
glimpse(lswt_final)

# correct the data types:
lswt_final$Date <- as.Date(lswt_final$Date)
lswt_final$LSWT_Kelvin <- as.double(lswt_final$LSWT_Kelvin)

# take the mean temperature for the same day by first grouping by Date and then mean(LSWT_Kelvin)
lswt_final <- lswt_final %>%
  group_by(Date) %>%
  summarize(Mean_K = mean(LSWT_Kelvin))

# Let's turn the Kelvin into degrees Celsius for convenience
lswt_C <- lswt_final %>%
  mutate(LSWT_C = Mean_K-273.15)

# THAT'S IT!!! You got your "simpler" time series for daily mean temperature from 1995-2016
## There will not be daily values since satellites also have their limitations, like no data for a cloudy day for example, 
## but you can then decide what you need for your analysis (leave it as is, some interpolation, etc etc etc).

# If needed, save as .csv
write.csv(as.data.frame(lswt_C), "GloboLakes_Atitlan_TS_95_16.csv", row.names=T)

# Play around with ggplot --------------------------------------

#Checking something
lswt_C %>%
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarize(mean = mean(LSWT_C, na.rm=T)) %>%
  ggplot() +
  geom_point(aes(x=month, y=mean, color=year)) +
  labs(title = "Useless plot of Average Temperature in °C for the entire GloboLakes dataset for Lake Atitlán",
       x = "Month", y = "Mean Temperature (°C)")

# TS plot
lswt_C %>%
  ggplot(aes(x=Date, y=LSWT_C)) +
  geom_point(size=1) +
  geom_line(size=0.5) +
  labs(title="Time series of daily mean LSWT (°C) in Lake Atitlan between 1995-2016", 
       x = "Year",
      y = "Mean Temperature (°C)",
      caption = "Data: GloboLakes | Viz: @braeuNERD")   


# But make it pretty: -------------------------------------------
library(showtext)
library(ggtext)

font_add_google("Roboto") #I just like this font
showtext_auto()

lswt_C %>%
  ggplot(aes(x=Date, y=LSWT_C)) +
  geom_point(size=0.75, color="#7DB2BE") +
  geom_line(size=0.5, color="#7DB2BE") +
  labs(title = "Time series of daily mean LSWT (°C) in Lake Atitlan between 1995-2016",
       subtitle = "raw data extracted  and averaged from GloboLakes .nc file",
       x = "Year",
       y = "Temperature (°C)",
       caption = "Data: GloboLakes | Viz: @braeuNERD") +
  theme(plot.background = element_rect(fill = "#F8F2E7", color = "#F8F2E7"),
        plot.margin = margin(10, 50, 0, 40), 
      panel.background = element_rect(fill = "#F8F2E7"),
      plot.title = element_markdown(family = "Roboto",color = "#61605D", size = 18, margin = margin(20,0,0,0)),
      plot.subtitle = element_markdown(family = "Roboto", color = "grey70", size = 11, face="italic", margin = margin(5, 0, 10, 10)),
      plot.caption = element_text(family = "Roboto", hjust = 1, color = "grey70", size = 11, margin = margin(10,0,20,0)),
      axis.text.x = element_markdown(family = "Roboto", color = "#61605D", size = 8, margin = margin(10,0,0,0), angle = 35),
      axis.text.y = element_markdown(family = "Roboto", color = "#61605D", size = 10),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_markdown(family = "Roboto"),
      axis.title.y = element_markdown(family = "Roboto"),
      axis.line = element_line(colour = "grey70", size = 1)) +
  scale_x_date(limits = as.Date(c("1995-01-01","2017-01-01")), expand = expansion(0)) +
  scale_y_continuous(limits = c(5,30), expand = expansion(0))
  
