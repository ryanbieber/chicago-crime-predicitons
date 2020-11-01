## the modelling application
source("~/R/chicago-crime-predicitons/src/parameters.R")


library(sparklyr)
library(dplyr)

sc <- spark_connect(master = "local")
fbi_codes <- copy_to(sc, fbi_merge_code)

spark_config_settings()

crime <- spark_read_csv(sc, "C:/Users/leroy/Documents/R/chicago-crime-predicitons/data/Crimes_-_2001_to_Present.csv") %>%
  mutate(FBI_Code = ifelse(is.na(FBI_Code),"MISSING",FBI_Code)) %>%
  mutate( Location = ifelse(is.na(Location),"MISSING", Location)) %>%
  mutate(Latitude = ifelse(is.na(Latitude),0, Latitude)) %>%
  mutate(Longitude = ifelse(is.na(Longitude),0, Longitude)) %>%
  mutate(Location = ifelse(is.na(Location), "MISSING", Location)) %>%
  mutate(Location_Description = ifelse(is.na(Location_Description),"MISSING", Location_Description)) %>%
  inner_join(fbi_merge_code, copy = TRUE) %>%
  mutate(Month = substr(Date,0,1)) %>%
  mutate(Day = substr(Date,3,4))



crime_rename <- crime %>%
  group_by(Location_Description) %>%
  tally() %>%
  mutate(Location_Description = ifelse(n<1000, "LESS THAN 1K", Location_Description))

crime_rename <- collect(crime_rename) %>%
  group_by(Location_Description) %>%
  summarise(total = sum(n))

#looking at locations
# location_unique <- crime %>%
#   group_by(Location_Description) %>%
#   tally()
# location_unique <- collect(location_unique) %>%
#   arrange(desc(n))

crime %>%
  group_by(Arrest) %>%
  tally()
crime %>%
  group_by(Domestic) %>%
  tally()

crime %>%
  group_by(Arrest, Domestic) %>%
  tally()

crimes_over_time <- crime %>%
  group_by(Year) %>%
  tally()

crime_time <- collect(crimes_over_time) %>%
  arrange(desc(Year))

crime_update <- crime %>%
  mutate(Day = lubridate::wday(Date))


##todo
##clean location description
##look at the time to get morning, afternoon, evening, night
##time of day
##day of week
## other features if I can think of em


spark_disconnect(sc)
