
# Packages 
library(tidyverse)
library(stringr)

# Step 1

#Importing Data and Getting overview
Data_Storm <- read_csv("StormEvents_details-ftp_v1.0_d1997_c20220425.csv.gz")
head(Data_Storm)

# Step 2

# Sub-setting data by limiting columns 
col_limit <- c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", 
                  "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")
Data_Storm <- Data_Storm[col_limit]

# Step 3 

#Sorting by State and getting overview
Data_Storm <- Data_Storm %>% arrange(STATE)
head(Data_Storm)

# Step 4

# Changing the case for the State and County variables to title case
Data_Storm$STATE <- str_to_title(Data_Storm$STATE)
Data_Storm$CZ_NAME <- str_to_title(Data_Storm$CZ_NAME)

# Step 5

# Filtering for CZ Type = C, then excluding the CZ Type column
Data_Storm <- Data_Storm %>% filter(CZ_TYPE == "C")
Data_Storm <- Data_Storm %>% select(-CZ_TYPE)

# Step 6

# Padding State FIPS and Country FIPS with 0 and uniting the two columns 
Data_Storm$STATE_FIPS <- str_pad(Data_Storm$STATE_FIPS, 
                                 width = 3, side = "left", pad = "0")
Data_Storm$CZ_FIPS <- str_pad(Data_Storm$CZ_FIPS,
                              width = 4, side = "left", pad = "0")
Data_Storm <- unite(Data_Storm, col = "fips",
                    c("STATE_FIPS", "CZ_FIPS"), sep = "")

# Step 7

# Changing column names case to lower case
Data_Storm <- rename_all(Data_Storm, tolower)

# Step 8

# Loading State data and creating a three variable data frame
data("state")
state_df <- data.frame(state.name, state.area, state.region)

# Step 9

#Computing number of events per state and merging with state data frame
events_state <- data.frame(table(Data_Storm$state))
colnames(events_state) <- c("state", "events")
merge_state <- merge(x = state_df, y = events_state, 
                     by.x = "state.name", by.y = "state")

# Step 10

# Land area and number of events Scatter plot 
merge_state %>% ggplot() + aes(x = state.area, y = events, 
                               colour = state.region) + 
  geom_point() + scale_color_hue(direction = 1) +
  labs(x = "Land area (square miles)", 
       y = "# of strom events in 1997", 
       color = "region") +
  theme_gray()





