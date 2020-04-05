## Plotting the CONFIRMED Cases of COVID-19 in the USA
## County Wise for all Lower48 states of the USA

# I used the latest information on County Wise COVID19 data from Github website 
# Data is there for each county in the USA;
# Downloaded shape file for all counties in the USA from data.gov website
# Used the GEOID tag in the Shape File to differentiate all the Counties in the USA
# GEOID is StateFP + CountyFP
# Written code to store the last 5 days of MAP only, 
# As it take more time for each map generation with 3107 counties in lower48 states

setwd("D:/R/Project/COVID19/USACountyWise")
wd <- "D:/R/Project/COVID19/USACountyWise"

#install.packages("readr")
#install.packages('rgdal')
#install.packages("sf", type="binary")
#install.packages(rgeos)
library(readr)
library(rgdal)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(rgeos)

# Reading the COVID information from Github website
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
CONFIRMED <- read_csv(url(urlfile))

# Storing the data onto local harddisk for reference
downfilename <- paste0("D:\\Project\\COVID19\\GitHub_Download_",Sys.Date(),".csv")
write.csv(CONFIRMED,downfilename,row.names=FALSE)
# downloaded data in Excel format as cab be used
# in case Github website is not reachable or datafile source changes

# rename it
mydata <- CONFIRMED

# Reading the Shape file for the USA based on County

USA_COUNTY <- readOGR(paste0(wd,"/tl_2019_us_county"),
                      "tl_2019_us_county")

# Source of USA county 2019 shape file is
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Counties+%28and+equivalent%29

# Old 2017 county info can be downloaded from below; not much change
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-county-and-equivalent-national-shapefile

#Need to filter the Lower48 states based on StateFP data (2 digit code)

statefips <- read_csv("D:/R/Project/COVID19/USACountyWise/US_states_info2.csv")
statefips2 <- statefips

# Mering the State Name and State Abbreviation of Lower48 states to the Shape File
USA_COUNTY2 <- merge(x=USA_COUNTY,
                     y=statefips2,
                     by.x="STATEFP",
                     by.y="StateFP")

# Filtering the USA Shape file for Lower48 states only
USA_COUNTY2 <- USA_COUNTY2[!is.na(USA_COUNTY2@data$StateName),] 

plot(USA_COUNTY2,col='green',main = "USA Map")

# adding "0" to the FIPS column (numeric) of mydata so it can be matched with GEOID(Factor) of ShapeFile

# Checking the column type
str(mydata$FIPS)

# coverting the column type for Character
mydata$FIPS = as.character(mydata$FIPS)

a <- 1
#Only doing for the first 321 rows which covers the first 9 States
for(a in 1:321){ 
  if(nchar(mydata$FIPS[a])==4){ 
    #Add "0" to the first 9 states who has 4 characters only in FIPS
    mydata$FIPS[a] = paste0("0",mydata$FIPS[a])
  }
}

# merging the COVID info to the State Info
COVID2 <- merge(x=statefips2,
                y=mydata,
                by.x="StateName",
                by.y="Province_State",
                all.x=T)  # only keep the value when State is there; so cruise and other counties will go out

# Deleting info for "Out of State" or "UnAssigned"
COVID2 <- COVID2[COVID2$Lat!=0,]

# deleting the unwanted columns from COVID
COVID2 <- subset(COVID2,select = -c(StateName))
COVID2 <- subset(COVID2,select = -c(StateFP))
COVID2 <- subset(COVID2,select = -c(StateAbr))

# Merging COVID onto the 'USA_COUNTY2' shape file
USA_COUNTY2 <- merge(USA_COUNTY2,
                     COVID2,
                     by.x = "GEOID",
                     by.y = "FIPS")

a <- ncol(USA_COUNTY2) #corresponds to latest date read from Github

# Plotting the Map for latest Date read from Github
map2 <- tm_shape(USA_COUNTY2)+
  tm_fill(names(USA_COUNTY2@data)[a], #manually entered the date
          breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,Inf),
          style="fixed",
          colorNA="black")+
  tm_borders()+
  tm_layout("Confirmed Cases")

print(map2)


#############################################
#############################################
##Storing the map data in folder for the USA based on County

# Using FOR loop to make a map of Cumulative CONFIRMED cases
# Storing in Different Directory
dir.create("CONFIRMED_USA_Cumulative")
tmpFolder <- 'CONFIRMED_USA_Cumulative'

#5th day from the latest day is starting plot
a <- ncol(USA_COUNTY2) - 4 

#Only the last 5 days map was saved for analysis;As it takes time for each map analysis
for(a in (ncol(USA_COUNTY2)-4):ncol(USA_COUNTY2)){
 map<- tm_shape(USA_COUNTY2)+
    tm_fill(names(USA_COUNTY2@data)[a], #manually entered the date
            breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,Inf),
            style="fixed",
            colorNA="black")+
    tm_borders()+
  tm_layout("CONFIRMED Cases")
 
# map 
# dev.off()

# get rid of slashes in the date 1/22/20 and make it 1_22_20 
tmpName <- gsub("/","_",names(USA_COUNTY2)[a])
tmpName <- paste0("USA_Cumulative_as_on_",tmpName)

# Saving the file to the Respective folder
tmap_save(map,
          paste0(wd,"/",tmpFolder,"/",
                       tmpName,".png"))
}


#############################################
##Assignment question on Confirmed Cases per Day 
# Using FOR loop to make a map of confirmed cases for each day

# Storing in Different Folder
dir.create("CONFIRMED_USA_EachDay")
tmpFolder <- 'CONFIRMED_USA_EachDay'

#5th day from the latest day is starting plot
a <- ncol(USA_COUNTY2) - 4 

# Only the last 5 days map was saved for analysis; As it takes time for each map analysis
for(a in (ncol(USA_COUNTY2)-4):ncol(USA_COUNTY2)){
  m <- USA_COUNTY2@data[a] - USA_COUNTY2@data[a-1]
  map<- tm_shape(USA_COUNTY2)+
    tm_fill(names(m), #manually entered the date
            breaks = c(0,100,200,500,1000,2000,5000,10000,20000,Inf),
            style="fixed",
            colorNA="black")+
    tm_borders()+
    tm_layout("CONFIRMED Cases Per Day")
  
#  print(map)
#  dev.off() #close the plot
  
# get rid of slashes in the date 1/22/20 and make it 1_22_20 
  tmpName <- gsub("/","_",names(USA_COUNTY2)[a])
  tmpName <- paste0("USA_Confirmed_Cases_on_",tmpName)
# Saving the file to specificed folder  
  tmap_save(map,
            paste0(wd,"/",tmpFolder,"/",
                   tmpName,".png"))
}


#############################################
##Assignment question on Percentage per Day 
# Doing it for the USA based on Each County

# Using FOR loop to make a map of confirmed cases for each day

dir.create("CONFIRMED_USA_Percent_Change")
tmpFolder <- 'CONFIRMED_USA_Percent_Change'

#5th day from the latest day is starting plot
a <- ncol(USA_COUNTY2) - 4 

# Only the last 5 days map was saved for analysis; As it takes time for each map analysis
for(a in (ncol(USA_COUNTY2)-4):ncol(USA_COUNTY2)){
  m <- (USA_COUNTY2@data[a] - USA_COUNTY2@data[a-1])/(USA_COUNTY2@data[a-1])*100
  m <- as.data.frame(m)
  m[mapply(is.infinite,m)] <- 0
  m[is.na(m)] <- 0
  map<- tm_shape(USA_COUNTY2)+
    tm_fill(names(m), #manually entered the date
            breaks = c(0,10,20,50,100,200,500,Inf),
            style="fixed",
            colorNA="black")+
    tm_borders()+
    tm_layout("CONFIRMED Percent Change Per Day")
  
# print(map)
# dev.off() #close the plot

  # get rid of slashes in the date 1/22/20 and make it 1_22_20 
  tmpName <- gsub("/","_",names(USA_COUNTY2)[a])
  tmpName <- paste0("USA_Percent_Change_on_",tmpName)

  # Saving to specified folder 
  tmap_save(map,
            paste0(wd,"/",tmpFolder,"/",
                   tmpName,".png"))
  
}

