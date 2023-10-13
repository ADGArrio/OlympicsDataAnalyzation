
# title: "Olympics Project"
# author: "Arrio Gonsalves + Evan Bruns"

library(dplyr)
library(tidyr)

summer_data <- read.csv("/Users/arriogonsalves/Desktop/UT Dallas/Fall 2023/STAT 3355/Olympics Data/Athletes_summer_games.csv")
winter_data <- read.csv("/Users/arriogonsalves/Desktop/UT Dallas/Fall 2023/STAT 3355/Olympics Data/Athletes_winter_games.csv")

# Combines summer and winter data
olympics_data <- rbind(summer_data, winter_data)

# Subsets data to only include USA and GBR Data and Removes Unnecessary Columns
olympics_data <- olympics_data[olympics_data$NOC %in% c("USA", "GBR"), 
                               c("Sex", "Age", "NOC", "Year", "Season", "City", "Sport", "Medal")]

# Subsets olympics_data to get only USA and GBR athlete Data
usa_df <- olympics_data[olympics_data$NOC %in% c("USA"), 
                        c("Sex", "Age", "NOC", "Year", "Season", "City", "Sport", "Medal")]
gbr_df <- olympics_data[olympics_data$NOC %in% c("GBR"), 
                        c("Sex", "Age", "NOC", "Year", "Season", "City", "Sport", "Medal")]

### Male vs. Female Athletes

# USA Male/Female athletes (ALL athletes, not just medal winners)
female_usa_df <- usa_df[usa_df$Sex %in% c("F"), ]
male_usa_df <- usa_df[usa_df$Sex %in% c("M"), ]

# GBR Male/Female athletes (ALL athletes, not just medal winners)
female_gbr_df <- gbr_df[gbr_df$Sex %in% c("F"), ]
male_gbr_df <- gbr_df[gbr_df$Sex %in% c("M"), ]


# Subsets USA athletes to just USA athletes with medals
gold_usa_df <- usa_df[usa_df$Medal %in% c("Gold"), ]
silver_usa_df <- usa_df[usa_df$Medal %in% c("Silver"), ]
bronze_usa_df <- usa_df[usa_df$Medal %in% c("Bronze"), ]

# Subsets GBR athletes to just GBR athletes with medals
gold_gbr_df <- gbr_df[gbr_df$Medal %in% c("Gold"), ]
silver_gbr_df <- gbr_df[gbr_df$Medal %in% c("Silver"), ]
bronze_gbr_df <- gbr_df[gbr_df$Medal %in% c("Bronze"), ]

# USA total medals
usa_medal_count <- nrow(gold_usa_df) + nrow(silver_usa_df) + nrow(bronze_usa_df)

# GBR total medals
gbr_medal_count <- nrow(gold_gbr_df) + nrow(silver_gbr_df) + nrow(bronze_gbr_df)

# USA ratio of gold to total
cat(nrow(gold_usa_df)/usa_medal_count)

# GBR ratio of gold to total
cat(nrow(gold_gbr_df)/gbr_medal_count)

# Comparing Medal Wins Over the Years
library(ggplot2)

# Filter the data for the USA and medals won
usa_medals <- usa_df %>%
  filter(NOC == "USA" & Medal %in% c("Gold", "Silver", "Bronze"))

# Filter the data for medals won by USA and GBR
usa_gbr_medals <- olympics_data %>%
  filter(Medal %in% c("Gold", "Silver", "Bronze"))

# Create a data frame for the total number of medals per country and year
total_medals_per_country <- usa_gbr_medals %>% group_by(Year, NOC) %>%
  summarize(Total_Medals = n())

# Create a bar plot
ggplot(total_medals_per_country, aes(x = Year, y = Total_Medals, fill = NOC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Medals by Year (USA and GBR)",
       x = "Year", y = "Total Medals") +
  scale_fill_manual(values = c("USA" = "blue", "GBR" = "red")) +
  theme_minimal()


### Sports in which either country consistently does better than the other:

# Calculate the number of medals won by the USA and GBR in each sport
usa_sport_medals <- usa_df %>%
  group_by(Sport, Medal) %>%
  summarize(Medal_Count = n())

gbr_sport_medals <- gbr_df %>%
  group_by(Sport, Medal) %>%
  summarize(Medal_Count = n())

# Calculate the total medal count for each sport
usa_sport_medals <- usa_sport_medals %>%
  group_by(Sport) %>%
  summarize(Total_Medals = sum(Medal_Count))

gbr_sport_medals <- gbr_sport_medals %>%
  group_by(Sport) %>%
  summarize(Total_Medals = sum(Medal_Count))

# Identify sports or events where one country consistently outperforms the other
consistent_outperformance <- usa_sport_medals %>%
  filter(Total_Medals > 0) %>%
  anti_join(gbr_sport_medals, by = "Sport")

consistent_underperformance <- gbr_sport_medals %>%
  filter(Total_Medals > 0) %>%
  anti_join(usa_sport_medals, by = "Sport")

# Display the results
cat("Sports or Events where the USA consistently outperforms GBR:\n", consistent_outperformance$Sport)
cat("Sports or Events where GBR consistently outperforms the USA:\n", consistent_underperformance$Sport)


### In which season (Summer/Winter) have each country fared better relative to the other?

library(ggplot2)

# Filter the data for the USA and medals won in the Summer season
usa_summer_medals <- usa_df %>%
  filter(Season == "Summer" & Medal %in% c("Gold", "Silver", "Bronze"))

# Filter the data for medals won by USA and GBR in the Summer season
usa_gbr_summer_medals <- olympics_data %>%
  filter(Season == "Summer" & Medal %in% c("Gold", "Silver", "Bronze"))

# Create a data frame for the total number of medals per country and year in the Summer season
total_summer_medals_per_country <- usa_gbr_summer_medals %>% group_by(Year, NOC) %>%
  summarize(Total_Summer_Medals = n())

# Create a bar plot for the Summer season
ggplot(total_summer_medals_per_country, aes(x = Year, y = Total_Summer_Medals, fill = NOC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Summer Medals by Year (USA and GBR)",
       x = "Year", y = "Total Medals") +
  scale_fill_manual(values = c("USA" = "blue", "GBR" = "red")) +
  theme_minimal()

# Filter the data for the USA and medals won in the Winter season
usa_winter_medals <- usa_df %>%
  filter(Season == "Winter" & Medal %in% c("Gold", "Silver", "Bronze"))

# Filter the data for medals won by USA and GBR in the Winter season
usa_gbr_winter_medals <- olympics_data %>%
  filter(Season == "Winter" & Medal %in% c("Gold", "Silver", "Bronze"))

# Create a data frame for the total number of medals per country and year in the Winter season
total_winter_medals_per_country <- usa_gbr_winter_medals %>% group_by(Year, NOC) %>%
  summarize(Total_Winter_Medals = n())

# Create a bar plot for the Winter season
ggplot(total_winter_medals_per_country, aes(x = Year, y = Total_Winter_Medals, fill = NOC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Winter Medals by Year (USA and GBR)",
       x = "Year", y = "Total Medals") +
  scale_fill_manual(values = c("USA" = "blue", "GBR" = "red")) +
  theme_minimal()




