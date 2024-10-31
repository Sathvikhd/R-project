# Install and load necessary packages
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
library(ggplot2)

## Load the dataset using readxl
data <- read_excel("C:\\Users\\sathv\\Desktop\\R project\\Stephen Curry Stats dataset.xlsx")

# Check the column names to ensure they match what you expect
colnames(data)

# Convert Dates to Date format
data$Dates <- as.Date(data$Dates)

## VISUALIZATIONS

## 1. Points scored over time 
ggplot(data, aes(x=Dates, y=PTS)) +
  geom_line(color="blue") +
  labs(title="Points Scored Over Time", x="Date", y="Points Scored") +
  theme_minimal()

## 2. Total points scored against different opponents 
total_points_by_opponent <- aggregate(PTS ~ Opponent, data=data, sum)

ggplot(total_points_by_opponent, aes(x=reorder(Opponent, -PTS), y=PTS)) +
  geom_bar(stat="identity", fill="darkgreen") +
  labs(title="Total Points Scored Against Different Opponents", x="Opponent", y="Total Points") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

## 3. Relationship between minutes played and points scored 
ggplot(data, aes(x=Minutes, y=PTS)) +
  geom_point(color="red") +
  labs(title="Minutes Played vs. Points Scored", x="Minutes Played", y="Points Scored") +
  theme_minimal()

## 4. Histogram: Distribution of points scored 
ggplot(data, aes(x=PTS)) +
  geom_histogram(binwidth=5, fill="purple", color="black") +
  labs(title="Distribution of Points Scored", x="Points Scored", y="Frequency") +
  theme_minimal()

## 5. Boxplot for points scored in regular season vs. playoffs
ggplot(data, aes(x=Type, y=PTS, fill=Type)) +
  geom_boxplot() +
  scale_fill_manual(values=c("REGULAR SEASON STATS"="lightblue", "PLAYOFFS STATS"="lightcoral")) +
  labs(title="Points Scored in Regular Season vs. Playoffs", x="Game Type", y="Points Scored") +
  theme_minimal()

## PERFORMANCE ANALYSIS 

# Calculate shooting efficiency
data$Shooting_Efficiency <- data$`Successful Shots` / data$`Total Shots`

# Line plot for shooting efficiency over time
ggplot(data, aes(x=Dates, y=Shooting_Efficiency)) +
  geom_line(color="green") +
  labs(title="Shooting Efficiency Over Time", x="Date", y="Shooting Efficiency") +
  theme_minimal()

# Calculate 3-point shooting efficiency
data$Three_Point_Efficiency <- data$`3 Points Succesful` / data$`Total 3 Points`

# Line plot for 3-point shooting efficiency over time
ggplot(data, aes(x=Dates, y=Three_Point_Efficiency)) +
  geom_line(color="purple") +
  labs(title="3-Point Shooting Efficiency Over Time", x="Date", y="3-Point Shooting Efficiency") +
  theme_minimal()

# Data cleaning
# Calculate free throw efficiency
data$Free_Throw_Efficiency <- data$`Successful FT` / data$`Total FT`

# Remove rows with non-finite values in efficiency columns
clean_data <- data %>%
  filter(is.finite(Shooting_Efficiency), is.finite(Three_Point_Efficiency), is.finite(Free_Throw_Efficiency))

# Line plot for free throw efficiency over time
ggplot(clean_data, aes(x=Dates, y=Free_Throw_Efficiency)) +
  geom_line(color="orange") +
  labs(title="Free Throw Efficiency Over Time", x="Date", y="Free Throw Efficiency") +
  theme_minimal()

# Boxplot for shooting efficiency in wins vs. losses
ggplot(clean_data, aes(x=Result, y=Shooting_Efficiency, fill=Result)) +
  geom_boxplot() +
  labs(title="Shooting Efficiency in Wins vs. Losses", x="Game Result", y="Shooting Efficiency") +
  theme_minimal()

# Boxplot for 3-point shooting efficiency in wins vs. losses
ggplot(clean_data, aes(x=Result, y=Three_Point_Efficiency, fill=Result)) +
  geom_boxplot() +
  labs(title="3-Point Shooting Efficiency in Wins vs. Losses", x="Game Result", y="3-Point Shooting Efficiency") +
  theme_minimal()

# Boxplot for free throw efficiency in wins vs. losses
ggplot(clean_data, aes(x=Result, y=Free_Throw_Efficiency, fill=Result)) +
  geom_boxplot() +
  labs(title="Free Throw Efficiency in Wins vs. Losses", x="Game Result", y="Free Throw Efficiency") +
  theme_minimal()

