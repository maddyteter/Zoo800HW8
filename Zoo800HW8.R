########## Zoo800 Homework 8 10/23/2025

#Pull occurence data for Thalassia testudinum from 2000-current in US 

library(gbifdb)
library(dplyr) 
library(ggplot2)


#Objective 1 - Connect to online database and download data remotely 

gbif_conn <- gbif_remote() #connect remotely to GBIF 

gbif_conn <- gbif_remote(backend = "duckdb", bucket = "gbif-open-data-us-east-1")

colnames(gbif_conn) #See all of the column names in GBIF to determine which to select for 
#Thalassia dataset

tt_table <- gbif_conn %>%
  filter(species == "Thalassia testudinum", #filter for thalassia only
         year > 2000) %>% #records from 2000-current to limit data
  select(species, countrycode, eventdate, occurrencestatus, occurrenceid,
         decimallatitude, decimallongitude, year, individualcount, elevation, depth) %>%
  arrange(desc(eventdate)) %>%
  head(1000)   #only pull first 1000 observations

thalassia_df <- collect(tt_table) #turns data from step above into a dataframe in r to manipulate

#Objective 2 - Run EDM on dataset

#Plot 1 - Visualize how many records occurred over the years from 2000-current
hist(thalassia_df$year, 
     main = "Thalassia Records by Year", 
     xlab = "Year", 
     ylab = "Record Count",
     col = "lightblue", 
     breaks = 15)

#The year 2022 had the highest frequency of records


#Plot 2 - Visualize count of records by country
records_country <- thalassia_df %>%
  count(countrycode, name = "record_count") %>%
  arrange(desc(record_count))

records_country %>%
  ggplot(aes(x = reorder(countrycode, record_count), y = record_count)) + #order from low to high count
         geom_col(fill = "lightblue") +
  labs(
    title = "Record Count by Country",
    x = "Country",
    y = "Record Count") +
  theme_classic()

#US has highest number of thalassia records - very skewed towards a few select countries

#Plot 3 - Plot thalassia records on map to check if they are centered around the countries
#listed or if there are any mistakes in coordinates (outside designated range)

ggplot(thalassia_df, aes(x = decimallongitude, y = decimallatitude)) +
  geom_point(aes(color = countrycode), size = 1.5) +
  borders("world", colour = "gray40", fill = "gray90") + #use 
  labs(
    title = "Spatial Distribution of Records",
    x = "Longitude",
    y = "Latitude",
    color = "Country"
  ) +
  theme_minimal()

#A few datapoints off east coast of US in Bermuda but not outside of thalassia range

