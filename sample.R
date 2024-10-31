# Install and load necessary packages
install.packages(c("readxl", "tidyverse", "ggplot2", "dplyr"))
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Load the dataset
file_path <- "C:\\Users\\sathv\\Desktop\\R project\\Stephen Curry Stats dataset.xlsx"
nba_data <- read_excel(file_path)

# Inspect the first few rows to understand the structure
head(nba_data)

# Data Exploration
str(nba_data)
summary(nba_data)
sum(is.na(nba_data))

# Data Cleaning (example: removing NA values)
nba_data <- nba_data %>% drop_na()

# Example column names used: Player, Shot_Area, Shot_Made, Shot_Distance, Defender, X_Shot, Y_Shot
# Ensure these match the actual column names in your dataset

# Best Shooting Position
best_shooting_position <- nba_data %>%
  group_by(Player, Shot_Area) %>%
  summarise(Shots_Made = sum(Shot_Made),
            Shots_Attempted = n()) %>%
  mutate(Shooting_Percentage = Shots_Made / Shots_Attempted * 100) %>%
  arrange(desc(Shooting_Percentage))

print(best_shooting_position)

# Best Range
best_range <- nba_data %>%
  group_by(Player, Shot_Distance) %>%
  summarise(Shots_Made = sum(Shot_Made),
            Shots_Attempted = n()) %>%
  mutate(Shooting_Percentage = Shots_Made / Shots_Attempted * 100) %>%
  arrange(desc(Shooting_Percentage))

print(best_range)

# Best Defender
best_defender <- nba_data %>%
  group_by(Defender) %>%
  summarise(Shots_Made = sum(Shot_Made),
            Shots_Attempted = n()) %>%
  mutate(Defense_Effectiveness = 1 - (Shots_Made / Shots_Attempted)) %>%
  arrange(desc(Defense_Effectiveness))

print(best_defender)

# Correlation Between Shooter Efficiency and Defender Effectiveness
shooter_efficiency <- nba_data %>%
  group_by(Player) %>%
  summarise(Shots_Made = sum(Shot_Made),
            Shots_Attempted = n()) %>%
  mutate(Shooting_Percentage = Shots_Made / Shots_Attempted * 100)

defender_effectiveness <- nba_data %>%
  group_by(Defender) %>%
  summarise(Shots_Made = sum(Shot_Made),
            Shots_Attempted = n()) %>%
  mutate(Defense_Effectiveness = 1 - (Shots_Made / Shots_Attempted))

efficiency_vs_defense <- inner_join(shooter_efficiency, defender_effectiveness, by = c("Player" = "Defender"))

correlation <- cor(efficiency_vs_defense$Shooting_Percentage, efficiency_vs_defense$Defense_Effectiveness)
print(correlation)

# Visualization: Spatial Distribution of Taken and Missed Shots
court <- data.frame(
  x = c(-25, -25, 25, 25, -25),
  y = c(-47.5, 47.5, 47.5, -47.5, -47.5)
)

ggplot() +
  geom_polygon(data = court, aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  geom_point(data = nba_data, aes(x = X_Shot, y = Y_Shot, color = as.factor(Shot_Made))) +
  scale_color_manual(values = c("red", "green")) +
  theme_minimal() +
  labs(title = "Spatial Distribution of Taken and Missed Shots",
       x = "Court Width",
       y = "Court Length")
