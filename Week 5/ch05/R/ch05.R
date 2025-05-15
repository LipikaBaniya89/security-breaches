# set working directory to chapter location
# (change for where you set up files in ch 2)
setwd("~/Documents/ISA/Week 05/ch05")
# make sure the packages for this chapter
# are installed, install if necessary
pkg <- c("ggplot2", "scales", "maptools",
              "sp", "maps", "grid", "car" )
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}

# Listing 5-1 #########################################################
# Load ggplot2 to create graphics
library(ggplot2)
# read the CSV with headers
za <- read.csv("data/zeroaccess.csv", header=T)

# create a ggplot instance with zeroaccess data
gg <- ggplot(data=za, aes(x=long, y=lat)) 
# add the points, set transparency to 1/40th 
gg <- gg + geom_point(size=1, color="#000099", alpha=1/40) 
# add axes labels
gg <- gg + xlab("Longitude") + ylab("Latitude")
# simplify the theme for aesthetics
gg <- gg + theme_bw() 
# this may take a while, over 800,000 points plotted
print(gg)

# Listing 5-2 #########################################################
# requires package : ggplot2
# requires object: za (5-1)
# the "maps" and "mapproj" packages are used by ggplot
# load map data of the world
world <- map_data("world")
# nothing personal penguins, but strip out Antarctica
world <- subset(world, world$region!="Antarctica")
# load world data into ggplot object
gg <- ggplot(data=world, aes(x=long, y=lat))
# trace along the lat/long coords by group (countries)
gg <- gg + geom_path(aes(group=group), colour="gray70")
# now project using the mercator projection
# try different projections with ?mapproject
gg <- gg + coord_map("mercator", xlim=c(-200, 200))
# load up the ZeroAccess points, overiding the default data set
gg <- gg + geom_point(data=za, aes(long, lat), 
                      colour="#000099", alpha=1/40, size=1)
# remove text, axes ticks, grid lines and do gray border on white
gg <- gg + theme(text=element_blank(), 
                 axis.ticks=element_blank(),
                 panel.grid=element_blank(),
                 panel.background=element_rect(color="gray50",
                                               fill="white"))
print(gg)

# New Code for 5.3 and 5.4
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
install.packages("ggplot2")
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Retrieve the world map data as an sf object
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Verify that world_map is correctly loaded
if (is.null(world_map)) {
  stop("Error: world_map could not be loaded. Check rnaturalearth installation.")
} else {
  print("world_map successfully loaded.")
}

# latlong2map function for mapping coordinates to country names
latlong2map <- function(pointsDF, mapping) {
  # Convert pointsDF to an sf object with longitude (x) and latitude (y)
  points_sf <- st_as_sf(pointsDF, coords = c("x", "y"), crs = 4326)
  
  # Perform a spatial join to get country names corresponding to each point
  joined <- st_join(points_sf, world_map, join = st_within)
  
  # Return the country names
  return(joined$name_long)
}


# 1. Load ZeroAccess Infection Data (replace with your actual file)
za_data <- read.csv("data/zeroaccess_locations.csv")

# Assuming your data has 'longitude' and 'latitude' columns
if (!all(c("longitude", "latitude") %in% colnames(za_data))) {
  stop("Error: Infection data needs 'longitude' and 'latitude' columns.")
}

# 2. Prepare Infection Coordinates
za_coords <- data.frame(x = za_data$longitude, y = za_data$latitude)

# 3. Map Infection Coordinates to Country Names
za_countries <- latlong2map(za_coords, world_map)

# 4. Count Infections per Country
infection_counts <- table(za_countries)
infection_counts_df <- as.data.frame(infection_counts)
colnames(infection_counts_df) <- c("name_long", "infection_count")

# 5. Merge Infection Counts with World Map Data
world_map_merged <- left_join(world_map, infection_counts_df, by = "name_long")
world_map_merged$infection_count[is.na(world_map_merged$infection_count)] <- 0

# 6. Visualize Infection Counts on the World Map
ggplot(world_map_merged) +
  geom_sf(aes(fill = infection_count), color = "lightgray", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "Infection Count") +
  labs(title = "ZeroAccess Infections per Country") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "aliceblue"))

# --- ADDITIONS END HERE ---

# Map za coordinates to country names
zworld <- latlong2map(data.frame(x = za$long, y = za$lat), "world")

# Count occurrences per country and convert to a data frame
wct <- data.frame(table(zworld))
colnames(wct) <- c("region", "count")

# Merge country counts with world map data
za.choro <- merge(world_map, wct, by.x = "name_long", by.y = "region", all.x = TRUE)
za.choro$count[is.na(za.choro$count)] <- 0  # Set missing counts to 0

# Plot the choropleth map
gg <- ggplot(za.choro) +
  geom_sf(aes(fill = count), color = "#666666") +
  scale_fill_gradient2(low = "#FFFFFF", high = "#4086AA", 
                       midpoint = median(za.choro$count, na.rm = TRUE),
                       name = "Infections") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  theme_void() +
  theme(panel.background = element_rect(color = "gray50", fill = "white"))

print(gg)

# Listing 5-5 #########################################################
# requires object: wct (5-4)
head(wct)
##        region count
## 1 Afghanistan    53
## 2     Albania  1166
## 3     Algeria  3014
## 4     Andorra     4
## 5      Angola   160
## 6   Argentina  6016

# for each wct$count, divide by sum, gives us proportion of the whole
perc <- wct$count/sum(wct$count)
# covert to a readable format, round it and create percent
wct$perc <- round(perc, 4)*100
# now order the highest percentages on top
wct <- wct[with(wct, order(perc, decreasing=T)), ]
# look at the top few entries.
head(wct)
##      region  count  perc
## 148     USA 261627 35.23
## 24   Canada  35607  4.79
## 74    Japan  33590  4.52
## 145      UK  31813  4.28
## 50  Germany  27336  3.68
## 71    Italy  25717  3.46

# Listing 5-6 #########################################################
# requires package: ggplot2, maps, maptools
# requires objects: za (5-1), latlong2map (5-3)
#zstate <- latlong2map(data.frame(x=za$long, y=za$lat), "state")
# Map za coordinates to country names (you need this to filter for US)
zstate <- latlong2map(data.frame(x=za$long, y=za$lat), "state")

# Select rows from za where the country is "United States of America"
za.state <- za[which(!is.na(zstate)), ]

# load map data of the U.S.
state <- map_data("state")

gg <- ggplot(data=state, aes(x=long, y=lat))
gg <- gg + geom_path(aes(group=group), colour="gray80")
gg <- gg + coord_map("mercator", xlim=c(-130, -65), ylim=c(20, 50)) # Adjust map limits
gg <- gg + geom_point(data=za.state, aes(long, lat),
                      colour="#000099", alpha=1/40, size=1)
# stripping off the "chart junk"
gg <- gg + theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)

# Listing 5-7 (Revised - Using sf and dplyr) ####################################
# requires package: ggplot2, maps, sf, dplyr
# requires objects: za (5-1)

library(maps)
library(ggplot2)
library(dplyr)  # <- needed for left_join()

# Use map.where to match coordinates to states
zstate <- map.where(database = "state", x = za$long, y = za$lat)

# Filter out NAs and Potwin effect
state.index <- which(!is.na(zstate) & za$lat != 38 & za$long != -97)

# Count infections per state
sct <- data.frame(table(zstate[state.index]))
colnames(sct) <- c("region", "count")

# Clean state names
sct$region <- tolower(sct$region)

# Get state map data
states_map <- map_data("state")

# LEFT JOIN to keep the polygon groupings
za.sct <- left_join(states_map, sct, by = "region")
za.sct$count[is.na(za.sct$count)] <- 0  # Fill missing with 0

# Define colors
colors <- c("#A6611A", "#DFC27D", "#F5F5F5", "#80CDC1", "#018571")

# Draw the fixed choropleth
gg <- ggplot(za.sct, aes(x = long, y = lat, group = group, fill = count)) +
  geom_polygon(color = "black") +
  coord_map("polyconic") +
  scale_fill_gradient2(low = colors[5], mid = colors[3], high = colors[1],
                       midpoint = mean(za.sct$count),
                       name = "Infections") +
  theme_void() +
  theme(panel.background = element_rect(color = "gray50", fill = "white"))

print(gg)

# Listing 5-8 #########################################################
# requires package: ggplot2, maps
# requires objects: sct (from Listing 5-7), colors (from Listing 5-7)

# Assuming 'sct' and 'colors' are already defined from the execution of Listing 5-7
# Read in state population and internet users
# Data scraped from http://www.internetworldstats.com/stats26.htm
# Make sure the 'state-internets.csv' file is in your 'data' directory
users <- read.csv("data/state-internets.csv", header = TRUE)

# Convert state names to lowercase for consistency with map data
users$state <- tolower(users$state)

# Now merge with the sct data from the previous example
# Merge by sct$region and users$state
za.users <- merge(sct, users, by.x = "region", by.y = "state")

# Calculate people per infection
# Change 'population' to 'internet.users' if you want to use internet users
za.users$pop2inf <- round(za.users$population / za.users$count, 0)

# Create a simple data frame for merging with map data
za.norm <- data.frame(region = za.users$region,
                      count = za.users$pop2inf)

# Get state map data
state <- map_data("state")

# Merge the normalized data with the state map data
za.norm.map <- left_join(state, za.norm, by = "region")

# Now create the choropleth
gg <- ggplot(za.norm.map, aes(x = long, y = lat, group = group, fill = count)) +
  geom_polygon(colour = "black") +
  coord_map("polyconic") +
  scale_fill_gradient2(low = colors[5], mid = colors[3],
                       high = colors[1],
                       midpoint = mean(za.norm.map$count, na.rm = TRUE),
                       name = "People per\nInfection",
                       na.value = "lightgray") + # Handle NA values
  theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "gray50"))

print(gg)

# Listing 5-9 #########################################################
# requires objects: za.norm (5-8)
# create a box plot of the count
popbox <- boxplot(za.norm$count)

# Listing 5-10 ########################################################
# requires objects: za.norm (5-8), popbox (5-9)
# the values that are considered outliers
print(popbox$out)
## [1]  777 1536 1525 1550  724

# pull the rows from za.norm that have those values
za.norm[za.norm$count %in% popbox$out, ]
##                  region count
## 8  district of columbia   777
## 43                 utah  1536
## 44              vermont  1525
## 46           washington  1550
## 49              wyoming   724

# Listing 5-11 ########################################################
# requires objects: za.norm (5-8)
# get the standard deviation
za.sd <- sd(za.norm$count)
# get the mean
za.mean <- mean(za.norm$count)
# now calculate the z-score and round to 1 decimal
za.norm$z <- round((za.norm$count-za.mean)/za.sd, 1)
# we can inspect the “z” variable for the specific z-scores
# pull out values where absolute value of z-score is > 2
za.norm[which(abs(za.norm$z)>2), ]
##                  region count    z
## 8  district of columbia   777 -2.4
## 43                 utah  1536  2.2
## 44              vermont  1525  2.1
## 46           washington  1550  2.2
## 49              wyoming   724 -2.7


# Listing 5-12 ########################################################
#setting seed for reproducibility
set.seed(1492)
# run 100 times, getting random values between 98 and 102
mean(runif(100, min=98, max=102))
## [1] 100.0141

# Listing 5-13 ########################################################
#setting seed for reproducibility
set.seed(1492)
# iterate seq(10000) times, generate a set of 100 parts and calc mean
parts <- sapply(seq(10000), function(x) mean(runif(100, min=98, max=102)))
# result is a vector of 10,000 sets
# show the min and max of these parts
range(parts)
## [1]  99.57977 100.47559

# Listing 5-14 ########################################################
# requires package: maps, maptools
# requires objects: za (5-1), latlong2map (5-3)
## now mapping lat/long down to county
county <- map.where(database = "county", x = za$long, y = za$lat)
za.county <- county[which(!is.na(county) & za$lat!=38 & za$long!=-97)]
# count the occurances
county.count <- table(za.county)
# need to convert "county, state" into a data frame
# so we split it out by comma
temp.list <- strsplit(names(county.count), ",")
# convert the list into a vector
temp.list <- unlist(temp.list)
# force the vector into a 2 column matrix, filling row by row
temp.matrix <- matrix(temp.list, ncol=2, byrow=T)
# and now create the data frame with the count of county infections
za.county <- data.frame(temp.matrix, as.vector(county.count))
# finally assign names to the fields
# names match the field names in the county map_data
colnames(za.county) <- c("region", "subregion", "infections")
head(za.county)
##    region subregion infections
## 1 alabama   autauga         44
## 2 alabama   baldwin        184
## 3 alabama   barbour         13
## 4 alabama      bibb         13
## 5 alabama    blount         26
## 6 alabama   bullock         11


# Listing 5-15 ########################################################
# requires objects: za.county (5-14)
# read up census data per county
county.data <- read.csv("data/county-data.csv", header=T)
# notice the all.x option here
za.county <- merge(x=county.data, y=za.county, all.x=T)
# replace all NA's with 0
za.county$infections[is.na(za.county$infections)] <- 0
summary(za.county)
##      subregion         region          pop              income      
## washington:  32   texas   : 254   Min.   :     71   Min.   : 19344  
## jefferson :  26   georgia : 159   1st Qu.:  11215   1st Qu.: 37793  
## franklin  :  25   kentucky: 120   Median :  26047   Median : 43332  
## jackson   :  24   missouri: 115   Mean   : 101009   Mean   : 45075  
## lincoln   :  24   kansas  : 105   3rd Qu.:  67921   3rd Qu.: 50010  
## madison   :  20   illinois: 102   Max.   :9962789   Max.   :120096  
## (Other)   :2921   (Other) :2217                                     
##     ipaddr             ufo2010          infections     
## Min.   :        0   Min.   :  0.000   Min.   :   0.00  
## 1st Qu.:     5367   1st Qu.:  0.000   1st Qu.:   6.00  
## Median :    15289   Median :  2.000   Median :  17.00  
## Mean   :   387973   Mean   :  7.943   Mean   :  83.33  
## 3rd Qu.:    62594   3rd Qu.:  6.000   3rd Qu.:  55.25  
## Max.   :223441040   Max.   :815.000   Max.   :7692.00  

# Listing 5-16 ########################################################
# for reproducability
set.seed(1)
# generate 200 random numbers around 10
input <- rnorm(200, mean=10)
summary(input)
##  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 7.785   9.386   9.951  10.040  10.610  12.400 

# Listing 5-17 ########################################################
# requires objects: input (5-16)
# generate output around a mean of 2 x input
dev.off()
output <- rnorm(200, mean=input*2)
# put into data frame to plot it
our.data <- data.frame(input, output)
gg <- ggplot(our.data, aes(input, output))
gg <- gg + geom_point()
gg <- gg + geom_smooth(method = "lm", se=F, color="red")
gg <- gg + theme_bw()
print(gg)

# Listing 5-18 ########################################################
# requires objects: input (5-16), output (5-17)
model <- lm(output ~ input)
summary(model)
## Call:
## lm(formula = output ~ input)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.93275 -0.54273 -0.02523  0.66833  2.58615 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.27224    0.77896   0.349    0.727    
## input        1.97692    0.07729  25.577   <2e-16 ***
##   ---
##   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 1.013 on 198 degrees of freedom
## Multiple R-squared:  0.7677,  Adjusted R-squared:  0.7665 
## F-statistic: 654.2 on 1 and 198 DF,  p-value: < 2.2e-16

# Listing 5-19 ########################################################
# requires objects: model (5-18)
confint(model)
##                 2.5 %   97.5 %
## (Intercept) -1.263895 1.808368
## input        1.824502 2.129343

# Listing 5-20 ########################################################
# requires objects: za.county (5-14 and 5-15)
summary(lm(infections ~ ufo2010, data=za.county))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 17.97998    2.63775   6.816 1.12e-11 ***
## ufo2010      8.22677    0.08843  93.029  < 2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 140.9 on 3070 degrees of freedom
## Multiple R-squared:  0.7382,  Adjusted R-squared:  0.7381 
## F-statistic:  8654 on 1 and 3070 DF,  p-value: < 2.2e-16


# Listing 5-21 ########################################################
# requires objects: za.county (5-14 and 5-15)
summary(lm(infections ~ pop + income + ipaddr + ufo2010, 
           data=za.county))
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.091e+01  5.543e+00   1.968   0.0492 *  
## pop          7.700e-04  9.072e-06  84.876  < 2e-16 ***
## income      -2.353e-04  1.215e-04  -1.937   0.0528 .  
## ipaddr       2.281e-06  3.027e-07   7.534 6.41e-14 ***
## ufo2010      5.495e-01  9.943e-02   5.526 3.54e-08 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 74.9 on 3067 degrees of freedom
## Multiple R-squared:  0.9261,  Adjusted R-squared:  0.926 
## F-statistic:  9610 on 4 and 3067 DF,  p-value: < 2.2e-16


# Listing 5-22 ########################################################
# requires objects: za.county (5-14 and 5-15)
library(car) # for the vif() function
model <- lm(infections ~ pop + income + ipaddr + ufo2010, 
            data=za.county)
sqrt(vif(model))
##      pop   income   ipaddr  ufo2010 
## 2.165458 1.038467 1.046051 2.115512 

# Listing 5-23 ########################################################
# requires objects: za.county (5-14 and 5-15)
za.county$za.by.pop <- za.county$infections/za.county$pop
za.county$ufo.by.pop <- za.county$ufo2010/za.county$pop
summary(lm(za.by.pop ~ ufo.by.pop, data=za.county))
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7.050e-04  1.213e-05  58.106  < 2e-16 ***
## ufo.by.pop  2.679e-01  6.956e-02   3.852  0.00012 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 0.0005793 on 3070 degrees of freedom
## Multiple R-squared:  0.004809,  Adjusted R-squared:  0.004485 
## F-statistic: 14.84 on 1 and 3070 DF,  p-value: 0.0001197

# Listing 5-24 ########################################################
# requires objects: za.county (5-14 and 5-15)
summary(lm(infections ~ pop, data=za.county))
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 4.545e-01  1.435e+00   0.317    0.752    
## pop         8.204e-04  4.247e-06 193.199   <2e-16 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 75.92 on 3070 degrees of freedom
## Multiple R-squared:  0.924,  Adjusted R-squared:  0.924 
## F-statistic: 3.733e+04 on 1 and 3070 DF,  p-value: < 2.2e-16

# Figure 5-10 #########################################################
# Code not in the book
# requires objects: za.county (5-14 and 5-15), colors (5-7)
library(gridExtra)
base.county <- map_data("county")
map.county <- merge(base.county, za.county, all.x=T)
map.county <- map.county[with(map.county, order(group, order)), ]
# convert to log scale for the fill colors in the map
# wash over unknown counties by calling them average.
map.county$logpop <- ifelse(is.na(map.county$pop) | map.county$pop==0,
                            log10(mean(map.county$pop, na.rm=T)), 
                            log10(map.county$pop))
map.county$logza <- ifelse(is.na(map.county$infections) | map.county$infections==0, 
                           log10(mean(map.county$infections, na.rm=T)), 
                           log10(map.county$infections))

gpop <- ggplot(map.county, aes(x=long, y=lat, group=group, fill=logpop))
gpop <- gpop + geom_polygon(linetype=0)
gpop <- gpop + coord_map("polyconic")
gpop <- gpop + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                    high=colors[1], 
                                    midpoint=mean(map.county$logpop, na.rm=T),
                                    guide=F)
gpop <- gpop + ggtitle("U.S. Population")
gpop <- gpop + theme(axis.title=element_blank(), 
                     axis.text=element_blank(),
                     axis.ticks=element_blank(),
                     panel.grid=element_blank(),
                     panel.background=element_blank())
#print(gpop)
ginf <- ggplot(map.county, aes(x=long, y=lat, group=group, fill=logza))
ginf <- ginf + geom_polygon(linetype=0)
ginf <- ginf + coord_map("polyconic")
ginf <- ginf + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                    high=colors[1], 
                                    midpoint=mean(map.county$logza),
                                    guide=F)
ginf <- ginf + ggtitle("ZeroAccess Infections")
ginf <- ginf + theme(axis.title=element_blank(), 
                     axis.text=element_blank(),
                     axis.ticks=element_blank(),
                     panel.grid=element_blank(),
                     panel.background=element_blank())
#print(ginf)
grid.arrange(ginf,gpop)

  # this prediction is mentioned briefly in Chapter 9:
zapop.lm <- lm(infections ~ pop, data=za.county)
predict(zapop.lm, data.frame(pop=6000000), interval="confidence")
##        fit     lwr      upr
## 1 4923.071 4873.88 4972.262
