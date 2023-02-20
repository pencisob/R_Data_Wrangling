### Install packages and libraries
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
install.packages("maps")
install.packages("mapproj")

library (devtools)
library (tidyverse)
library (ggmap)
library (dplyr)
library (maps)

### Import information
url2 ="http://672258.youcanlearnit.net/college.csv"
df <- readr::read_csv(url(url2))
rm (url2)

### Basic data wrangling: Missing Values, Factors and variables names
## variable names
names(df)
df <- df %>% rename("hdegree"="highest_degree", "adrate"="admission_rate", "sat"="sat_avg", "salary"="faculty_salary_avg", "default"="loan_default_rate", "debt"="median_debt")
## Factors
skimr::skim(df)
df <- df %>% mutate(region=as.factor(region), hdegree=as.factor(hdegree), control=as.factor(control), gender=as.factor(gender))
## Missing values
descriptr::ds_screener(df)

### Tibble with the information of the map
## California table
unique(df$state)
cali <-df %>% filter(state=="CA")
## Cali info for map: Cities(), Undergrad, Control and coordinates. 
caltable <- cali %>% select(id, name, city, state, control, undergrads, lat, lon)
caltable %>% group_by(city) %>% summarise(N=n())%>% arrange(desc(N))
length(unique(caltable$city))
caltable <- caltable %>% mutate(city=as.factor(city))
caltable

### Map information of California
## California and california county map : countyMapEnv->United States County Map
states <- map_data("state")
rm (states)
counties <- map_data("county")
head (counties, 15)
tail (counties, 15)
caldata <- counties %>% filter(region=="california")
caldata %>% group_by(subregion) %>% summarise(N=n())
descriptr::ds_screener(caldata)
##plotting the map with no info
ggplot(caldata)+ geom_polygon(aes(x=long, y=lat, group=group, color="white"))+coord_map()
ggplot(caldata)+ geom_polygon(aes(x=long, y=lat, group=group, color="white"))+coord_map()+theme(plot.background = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
ggplot(caldata)+ geom_polygon(aes(x=long, y=lat, group=group, color="white"))+coord_map()+ geom_point(mapping=aes(x=lon, y =lat, fill = control, size = undergrads),alpha = 0.5, data = caltable) +theme(plot.background = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())+ggtitle("Most California Colleges are Located in Large Cities", subtitle = "Source: U.S Department of Education")

### Merging the map and the info
## setting key variables
# state
caltable
head (caldata,15)
caltable <- caltable %>% mutate(state=ifelse(state=="CA", "california", state))
# Add county to caltable
library (usmap)
unique(caldata$subregion)
unique(caltable$city)


## creating the labels
# Geocode them
cal_cities <- geocode(c("Los Angeles", "San Diego", "San Francisco", "San Jose", "Fresno", "Sacramento"))
cal_cities
name_c <- c("Los Angeles", "San Diego", "San Francisco", "San Jose", "Fresno", "Sacramento")
name_c
labels <- as_tibble(cbind(name=name_c, cal_cities))
labels

## My graph

##adjusting the map after the video 
ggplot(caldata)+ geom_polygon(aes(x=long, y=lat, group=group), color="gray", fill="beige")+coord_map()+ geom_point(mapping=aes(x=lon, y =lat, color = control, size = undergrads),alpha = 0.3, data = caltable) +theme(plot.background = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())+ggtitle("Most California Colleges are Located in Large Cities", subtitle = "Source: U.S Department of Education")+ geom_text (mapping=aes(x=lon, y =lat, label = name), color = "black", size = 4, data = labels, nudge_y = 1) 
