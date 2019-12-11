# loading the libraries

library(readr)
library(stringr)
library(tidyverse)

# reading data
# 1,000,000 observations
# chunk 14 is year 2018 - 2019
# change data source when reading

current_chunk <- read_csv("raw_data/Chunk_14000000.csv")

# remove the bracket in the Address string

current_chunk <- current_chunk %>%
  mutate(Address = substr(Address, 2, nchar(Address)-1)) %>%
  mutate(count = str_count(current_chunk$Address, ","))

# separate according to the colums
# 8 list of different scenarios
# will focus on strings with 6/7 commas for valid data
# 978,352 obs remaining

current_chunk_split <- split(current_chunk, current_chunk$count)

# use only obs with 6 and 7 commas
# 699,152 with 6 commas (no middle name)
# 279,200 with 7 commas (with middle name)

current_chunk_6 <- current_chunk_split$"6"
current_chunk_7 <- current_chunk_split$"7"

####### chunk_6

# splitting

current_chunk_6_split <- current_chunk_6 %>%
  separate(Address, into  = c("last_name", 
                              "first_name", 
                              "address_line1",
                              "address_line2",
                              "city",
                              "state",
                              "zipcode"),
           sep = ",",
           remove = TRUE) 

# cleaning

# getting rid of the '' in strings
# columns aftter 'last_name' also had an additional ' '
# for zipcode, remove \n' at the end of the strings

# 546,861 remaining
current_chunk_6_clean <- current_chunk_6_split%>%
  mutate(last_name = substr(last_name, 2, nchar(last_name) -1),
         first_name = substr(first_name, 3, nchar(first_name) -1),
         address_line1 = substr(address_line1, 3, nchar(address_line1) -1),
         address_line2 = substr(address_line2, 3, nchar(address_line2) -1),
         city = substr(city, 3, nchar(city) -1),
         state = substr(state, 3, nchar(state) -1),
         zipcode = substr(zipcode, 3, nchar(zipcode) - 3)
  ) %>%
  filter (last_name != 'N/A',
          city != 'UNKNOWN',
          nchar(first_name) > 0,
          nchar(zipcode) > 0 )

####### chunk_7

# splitting

current_chunk_7_split <- current_chunk_7 %>%
  separate(Address, into  = c("last_name", 
                              "first_name", 
                              "middle_name", 
                              "address_line1",
                              "address_line2",
                              "city",
                              "state",
                              "zipcode"),
           sep = ",")

# cleaning
# 275,098 remaining

current_chunk_7_clean <- current_chunk_7_split %>%
  mutate(last_name = substr(last_name, 2, nchar(last_name) -1),
         first_name = substr(first_name, 3, nchar(first_name) -1),
         address_line1 = substr(address_line1, 3, nchar(address_line1) -1),
         address_line2 = substr(address_line2, 3, nchar(address_line2) -1),
         city = substr(city, 3, nchar(city) -1),
         state = substr(state, 3, nchar(state) -1),
         zipcode = substr(zipcode, 3, nchar(zipcode) - 3)
  ) %>%
  filter (last_name != 'N/A',
          city != 'UNKNOWN',
          nchar(first_name) > 0,
          nchar(zipcode) > 0 )


# combining data
clean_data_1 <- current_chunk_6_clean %>%
  select(-X1, -count)

clean_data_2 <- current_chunk_7_clean %>%
  select(-X1, -count, -middle_name)

# 821,959 rows
clean_data <- rbind(clean_data_1, clean_data_2)

# write the clean data to file
write.csv(clean_data, file = "Chunk_14_clean.csv")


