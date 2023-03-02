# Who-may-move-and-where-
Code for a paper analysing migration trends and its determinants in Pakistan
---
title: "Who may move and where? Evidence of intrantional migration in Pakistan"
author: "Hassan Aziz"
date: "2023-02-07"
output: html_document
---

## Loading Packages
```{r}
library(tidyverse)
library(dplyr)
library(sf)
library(tmap)
library(haven)
library(foreign)
library(ggplot2)
library(reshape2)
library(readxl)
library(stringr)
library(ggmap)
library(sp)
library(rgdal)
library(writexl)
```

# LFS 2020-21
## Link to the data: 
https://www.pbs.gov.pk/content/lfs-2020-21-microdata

## Descriptive statistics for migration

```{r}
lfs2021 <- read_dta("LFS2020-21.dta")

colnames(lfs2021)
str(lfs2021$HHSNo)
View(lfs2021)
length(unique(lfs2021$SNo))
(table(lfs2021$S4C15)/sum(table(lfs2021$S4C15)))*100
## 94.3% people are non migrants, only 5.7% are migrants; only 0.3% migrated in the last 1 year

table(lfs2021$S4C15 > 1 & lfs2021$S4C15 < 7 ) / sum(table(lfs2021$S4C15))*100
## Only 1.6% people have moved in the last 4 years to their current district of residence
# Migration for search of better agricultural land
## Total
(table(lfs2021$S4C18==3)/sum(table(lfs2021$S4C15)))*100

```

## Create the migrants dataset

```{r}

migrants <- lfs2021[lfs2021$S4C15 != 1, ]
## create a dataset that are migrants only 
head(migrants)
str(migrants$S4C16)
## See the variable showing what places people lived in before moving to the current place
```

## converting city codes character to numeric
```{r}
migrants$city_code <- as.numeric(migrants$S4C16)
## city_code is the name of the numeric codes from the city people *migrated from*
```


## Load city labels and merge them
```{r}
city_names <- read_excel("City codes.xlsx")
## Read the file containing the city codes and names

city_names <- rename(city_names, city_name = `City name`, city_code = `City Code`)
## Renaming the city names and city codes

pk.migrants <- merge(migrants, city_names, by = "city_code")
## Merging the two datasets: migrants dataset and the city codes dataset
```

## Creating the migration matrix
### Creating the migration matrix from the tables
```{r}
migration_matrix <- table(pk.migrants$city_name, pk.migrants$District)
## This creates a matrix telling us the origin and the destination cities 
```

### Creating the migration matrix data frame from vector
```{r}
migration.matrix <- as.data.frame(migration_matrix)
## Changing the dataset from a vector to a data frame
```

## Renaming the variables and city names in the migration matrix data frame
```{r}
migration.matrix <- rename(migration.matrix, origin = Var1, destination = Var2, migrant_value = Freq )
## Change the variable names to suitable names

migration.matrix <- mutate_all(migration.matrix, str_to_title)
## Change the city names fonts from all capital letters to title case

write.csv(migration.matrix, file = "migration.matrix.csv", row.names = FALSE)
## Downloading the migration matrix file for exporting the data to Python for data analysis
```

## Loading spatial datasets
```{r}
pk.districts_poly <- st_read("pakistan_districts.shp")
## Loading the polygon dataset for Pakistan, with incorrectly spelled names
pak_districts_lfs21 <- read.csv("Pakistan Districts (LFS-21).csv")
## Districts with Lat-Long data
pk.districts.poly <- read_excel("pk_districts_poly.xlsx")
## Districts with correct names
```

### Add geomtery to the cleaned dataset (with correct names, i.e., the pk.districts.poly dataset)

```{r}
pk.districts_poly <- select(pk.districts_poly, objectid, districts, geometry)
## Only keeping relevant variables in the dataset
pk.districts.poly <- inner_join(pk.districts_poly, pk.districts.poly, by = "objectid")
## Merge the dataset with correct names with geometry, with object id as common variable
pk.districts.poly <- select(pk.districts.poly, -districts.x, -district_a, -geometry.y)
## Only keeping relevant variables in the dataset
```
 
## Attach lat-long with matrix

```{r}
migration.matrix.point.origin <- merge(migration.matrix, pak_districts_lfs21, by.x = "origin", by.y = "District")
## Creating migration matrix with point data for origin districts
migration.matrix.point.dest <- merge(migration.matrix, pak_districts_lfs21, by.x = "destination", by.y = "District")
## Creating migration matrix with point data for destination districts
```

## Convert point data frames to geomtrical objects
```{r}
migration.matrix.point.origin <- st_as_sf(migration.matrix.point.origin, coords = c("Long", "Lat"), crs = st_crs("+proj=longlat +datum=WGS84"))
## Creating spatial dataset from a dataframe
migration.matrix.point.dest <- st_as_sf(migration.matrix.point.dest, coords = c("Long", "Lat"), crs = st_crs("+proj=longlat +datum=WGS84"))
## Creating spatial dataset from a dataframe

```

## project the spatial data 
```{r}
migration.matrix.point.origin <- st_transform(migration.matrix.point.origin, crs = "+proj=aea +lat_1=20 +lat_2=40 +lat_0=0 +lon_0=66 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

migration.matrix.point.dest <- st_transform(migration.matrix.point.dest, crs = "+proj=aea +lat_1=20 +lat_2=40 +lat_0=0 +lon_0=66 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

pk.districts <- st_transform(pk.districts, crs = "+proj=aea +lat_1=20 +lat_2=40 +lat_0=0 +lon_0=66 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

pk.districts.poly <- st_transform(pk.districts.poly, crs = "+proj=aea +lat_1=20 +lat_2=40 +lat_0=0 +lon_0=66 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

```

## Combine the point and polygon datasets

### Converting the migrant value variable from character to numeric
```{r}
migration.matrix.point.origin$migrant_value <- as.numeric(migration.matrix.point.origin$migrant_value) 
migration.matrix.point.dest$migrant_value <- as.numeric(migration.matrix.point.dest$migrant_value)
```

## Group the matrix dataset to make it ready for map creation

### First iteration
```{r}
origin <- migration.matrix.point.origin %>% 
  group_by(origin) %>% 
  summarise(values = sum(migrant_value)) 

destination <- migration.matrix.point.dest %>% 
  group_by(destination) %>% 
  summarise(values = sum(migrant_value))

```

### Group the Karachi data together

```{r}
# Six districts need to be clubbed together for better analysis: Central Karachi, West Karachi, East Karachi, South Karachi, Malir, Korangi.

# Changing the origin dataset
origin2 <- origin %>%
  mutate(origin = case_when(origin == "Central Karachi" ~ "Karachi",
                            origin == "East Karachi" ~ "Karachi",
                            origin == "West Karachi" ~ "Karachi",
                            origin == "South Karachi" ~ "Karachi",
                            origin == "Korangi" ~ "Karachi",
                            origin == "Malir" ~ "Karachi",
                          TRUE ~ origin))
origin <- origin2

# Changing the destination dataset
destination2 <- destination %>%
  mutate(destination = case_when(destination == "Central Karachi" ~ "Karachi",
                                destination == "East Karachi" ~ "Karachi",
                                destination == "West Karachi" ~ "Karachi",
                                destination == "South Karachi" ~ "Karachi",
                                destination == "Korangi" ~ "Karachi",
                                destination == "Malir" ~ "Karachi",
                                TRUE ~ destination))
destination <- destination2
```

### Second iteration (with Karachi grouped together)

```{r}
origin <- migration.matrix.point.origin %>% 
  group_by(origin) %>% 
  summarise(values = sum(migrant_value))

destination <- migration.matrix.point.dest %>% 
  group_by(destination) %>% 
  summarise(values = sum(migrant_value))
```



## Rename join variables
```{r}
names(origin)[names(origin) == "origin"] <- "district"
names(destination)[names(destination) == "destination"] <- "district"
names(pk.districts.poly)[names(pk.districts.poly) == "districts.y"] <- "district"
```

## Create a origin spatial map

```{r}
origin_map <- st_join(pk.districts.poly, origin, by = "district", left = TRUE)
origin_map <- select(origin_map, -district.y)
origin_map
tmap_mode("view")
tm_shape(origin_map) +
  tm_fill("values") +
  tm_borders(col = "black", lwd = 0.5) 
```
## Create a destination spatial map

```{r}
destination_map <- st_join(pk.districts.poly, destination)
View(destination_map)

pk.districts.admin2
tm_shape(destination_map) +
  tm_fill("values") +
  tm_borders(col = "black", lwd = 0.5)
```


## Get a spatial map
```{r}

ggplot() + 
  geom_sf(data = pk.districts_poly, fill = "gray") +
  # Add the points on top of the polygons
  geom_sf(data = destination, color = "red", size = 0.01)

```


