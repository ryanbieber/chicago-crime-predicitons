## the modelling application
#source("~/R/chicago-crime-predicitons/src/parameters.R")


Crimes <- c("Homicide 1 and 2","Criminal Sexual Assualt","Robbery","Aggravated Assualt","Aggravated Battery","Burglary","Larceny","Motor Vehicle Theft","Arson",
            "Involuntary Manslaugherter", "Simple Assualt","Simple Battery","Forgery and Counterfeiting","Fraud","Embezzlement","Stolen Property","Vandalism","Weapons Violation","Prostitution","Criminal Sexual Abuse",
            "Drug Abuse","Gambling","Offenses Against Family","Liquor License","Disorderly Conduct","Misc Non Index Offense")
FBI_Code <- c("01A","02","03","04A","04B","05","06","07","09","01B","08A","08B","10","11","12","13","14","15","16","17","18","19","20","22","24","26")


fbi_merge_code <- cbind.data.frame(Crimes, FBI_Code)

library(dplyr)


crimes <- read.csv("C:/Users/leroy/Documents/R/chicago-crime-predicitons/data/Crimes_-_2001_to_Present.csv")

##look at the data
head(crime)

##do so cleaning of the values to make sure we are cleaning up missing values and binning also merge the df we created earlier
crime <- crimes %>%
  mutate(FBI_Code = ifelse(is.na(FBI.Code),"MISSING",FBI.Code)) %>%
  mutate( Location = ifelse(is.na(Location),"MISSING", Location)) %>%
  mutate(Latitude = ifelse(is.na(Latitude),0, Latitude)) %>%
  mutate(Longitude = ifelse(is.na(Longitude),0, Longitude)) %>%
  mutate(Location = ifelse(is.na(Location), "MISSING", Location)) %>%
  mutate(Location.Description = ifelse(is.na(Location.Description),"MISSING", Location.Description)) %>%
  inner_join(fbi_merge_code, copy = TRUE) %>%
  mutate(Date = lubridate::mdy_hms(Date)) %>%
  mutate(DoW = lubridate::wday(Date)) %>%
  mutate(Month = substr(Date,6,7)) %>%
  mutate(Day = substr(Date,9,10)) %>%
  mutate(Year = substr(Date, 1,4)) %>%
  mutate(Hour = substr(Date, 12,13))
# crime %>%
#   group_by(Location.Description) %>%
#   tally()


crime$Location.Description[grep("airport",crime$Location.Description, ignore.case = TRUE)]<-"AIRPORT"
crime$Location.Description[grep("^cha",crime$Location.Description, ignore.case = TRUE)]<-"CHA"
crime$Location.Description[grep("college",crime$Location.Description, ignore.case = TRUE)]<-"COLLEGE"
crime$Location.Description[grep("^cta",crime$Location.Description, ignore.case = TRUE)]<-"CTA"
crime$Location.Description[grep("residence|residential",crime$Location.Description, ignore.case = TRUE)]<-"RESIDENTIAL"
crime$Location.Description[grep("other|^$",crime$Location.Description, ignore.case = TRUE)]<-"OTHER"
crime$Location.Description[grep("parking lot",crime$Location.Description, ignore.case = TRUE)]<-"PARKING LOT"
crime$Location.Description[grep("school",crime$Location.Description, ignore.case = TRUE)]<-"SCHOOL"

# crime_locations <- crime %>%
#   group_by(Location.Description) %>%
#   tally()


## finding binning level
crime_rename <- crime %>%
  group_by(Location.Description) %>%
  tally()
crime_rename$n <- crime_rename$n/nrow(crime)




crime_rename <- crime_rename %>%
  group_by(Location.Description) %>%
  mutate(Location.Description = ifelse(n <.01, "OTHER", Location.Description))



##removing location descriptions that dont contain at least 1% of the data (i.e. 70kish)
unique_crime <- unique(crime_rename$Location.Description)
unique_crime_grep <- paste(unique_crime[1], unique_crime[2], unique_crime[3], unique_crime[4], unique_crime[5], unique_crime[6], unique_crime[7], unique_crime[8], unique_crime[9], unique_crime[10], unique_crime[11], unique_crime[12], unique_crime[13], unique_crime[14], unique_crime[15], unique_crime[16], sep = "|")
crime$Location.Description[!grepl(unique_crime_grep,crime$Location.Description, ignore.case = TRUE)]<-"OTHER"

#unique(crime$Location.Description)

##doing some fixes
crime$Location.Description[grepl("Bowling alley|currency exchange",crime$Location.Description, ignore.case = TRUE)]<-"OTHER"
crime$Location.Description[grepl("gas station",crime$Location.Description, ignore.case = TRUE)]<-"GAS STATION"

# crime %>%
#   group_by(Location.Description) %>%
#   tally()

## all these values are greater than 1% so that seems fairly decent, some say 5% is a good rule of thumb but I think we loose a lot of value
## so we will keep it at 1% and adjust if need be later on


# crime %>%
#   mutate(Tod = ifelse(Hour=="01"|"02"|"03"|"04"|"05", "NIGHT",ifelse(
#     Hour == "06"|"07"|"08"|"09"|"10"|"11", "MORNING", ifelse(
#       Hour == "12"|"13"|"14"|"15"|"16"|"17","AFTERNOON", "EVENING"
#       )
#     )
#   ))


##selecting data for prediction
crime_prediction <- crime %>%
  select( Arrest, Domestic, Location.Description, Month, Day, Hour, Year, Latitude, Longitude, Beat, District, Ward, DoW, Crimes)

crime_prediction$Arrest <- as.logical(crime_prediction$Arrest)
crime_prediction$Domestic <- as.logical(crime_prediction$Domestic)

# crime_prediction %>%
#   filter(Latitude==0) %>%
#   tally()
##about 1% of data doesnt have a location

# crime_prediction %>%
#   group_by(Beat, District,  Arrest)

# beat_district<- crime_prediction %>%
#   group_by(Beat, District,  Arrest) %>%
#   tally() %>%
#   filter(n>100)

beat_district<- crime_prediction %>%
  group_by( District,  Arrest) %>%
  tally() %>%
  filter(n>50)


# beat_total <- crime_prediction %>%
#   group_by(Beat) %>%
#   tally()
district_total <- crime_prediction %>%
  group_by(District) %>%
  tally()


district_merged <- beat_district %>%
  inner_join(district_total, by ="District") %>%
  filter(Arrest==TRUE) %>%
  mutate(District_Ratio = n.x/n.y)



## beats are unique to each district and some have more than one districts attached to them over 618 observations have less than 100 values
## which indicates to me that they were improperly coded and will have to be coreced
##looks to be around 2k obs are improperly coded and will just drop them as that is less than .1%(right around .02%)


## ran into a snag with merging geojson data on beats is that the beats have changed over time so the data will not be reflective of the areas
## using this I will be switching to wards and districts, with wards being at a more community level and districts are a police geo area

## stating with district

library(geojsonio)
library(leaflet)
beats <- geojsonio::geojson_read("data/beats.geojson", what = "sp")
districts <- geojsonio::geojson_read("data/districts.geojson", what = "sp")
pal <- colorNumeric("viridis", NULL)


leaflet(districts) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(as.numeric(district_merged$District_Ratio))) %>%
  addLegend("bottomright", pal = pal, values = ~as.numeric(district_merged$District_Ratio),
            title = "Arrest Rate",
            opacity = 1
  )


sex_crimes <- crime_prediction %>%
  inner_join(district_merged, by ="District") %>%
  select(!Arrest.y) %>%
  filter(Crimes=="Criminal Sexual Assualt")



sex_crimes %>%
  group_by(Hour) %>%
  count() %>%
  plot()

sex_crimes %>%
  group_by(Day) %>%
  count() %>%
  plot()


sex_crimes %>%
  group_by(Month) %>%
  count() %>%
  plot()

sex_crimes %>%
  group_by(Year) %>%
  count() %>%
  plot()

sex_crimes_by_year_month <- sex_crimes %>%
  group_by(Year, Month) %>%
  count() %>%
  mutate(Time = paste(Year, Month, sep = "-")) %>%
  ungroup() %>%
  select(Time, n)

library(ggplot2)
ggplot(sex_crimes_by_year_month, aes(Time, n)) +
  geom_point() +
  theme(axis.text.x = element_blank())

sex_crimes_count <- sex_crimes %>%
  group_by(District) %>%
  count()

sex_crimes_count_merged <- sex_crimes_count %>%
  inner_join(district_merged, by="District") %>%
  mutate(Sex_ratio=n/n.y)

  leaflet(districts) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(as.numeric(sex_crimes_count_merged$Sex_ratio))) %>%
  addLegend("bottomright", pal = pal, values = ~100*as.numeric(sex_crimes_count_merged$Sex_ratio),
            title = "Percent of Criminal Sexual",
            opacity = 1
  )


## Seeing the geojson data for chicago districts then we can start looking at crime rates for particular crimes in the area


## Lets see the liklihood at being arrested based on some basic characteristics
  library(caret)
  library(mlbench)
  set.seed(1337)

  inTrain <- createDataPartition(
    y = crime_prediction$Arrest,
    p = .75,
    list = FALSE
  )

  training <- crime_prediction[inTrain,]
  testing <- crime_prediction[-inTrain,]

  ##fixing outcome to factor
  testing$Arrest <- as.factor(testing$Arrest)
  training$Arrest <- as.factor(training$Arrest)

  ## fixing the missing values
  training <- missRanger::imputeUnivariate(training)
  testing <- missRanger::imputeUnivariate(testing)

  trControl <- trainControl(method = 'repeatedcv',
                            number = 5,
                            savePredictions = TRUE)

  logitFit <- train(
    Arrest ~ as.factor(Domestic)+as.factor(Location.Description)+as.factor(Month)+as.factor(Day)+as.factor(Year)+as.factor(Hour)+as.factor(Beat)+as.factor(District)+as.factor(Ward)+as.factor(DoW)+as.factor(Crimes),
    data = training,
    method = "glmnet",
    family = "binomial",
    trControl = trControl,
    preProc = c("center", "scale")
  )


### so what happened is that this data is too big for a local R to run on it so that will require us to migrate this to SparklyR to then run our Logit on that


###
