# Install and load necessary packages
install.packages(c("readxl", "tidyverse", "ggplot2", "dplyr"))
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)



# Load the dataset
file_path <- "C:\\Users\\sathv\\Desktop\\R project\\Stephen Curry Stats dataset.xlsx"
nba_data <- read_excel(file_path)

str(nba_data)
dim(nba_data)

##2.Total points scored against different opponents 
total_points_by_opponent <- aggregate(PTS ~ Opponent, data=nba_data, sum)

# Plot the data using ggplot2
ggplot(total_points_by_opponent, aes(x=reorder(Opponent, -PTS), y=PTS)) +
  geom_bar(stat="identity", fill="darkgreen") +
  labs(title="Total Points Scored Against Different Opponents", x="Opponent", y="Total Points") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

## 3. Relationship between minutes played and points scored
ggplot(nba_data, aes(x=Minutes, y=PTS)) +
  geom_point(color="red") +
  labs(title="Minutes Played vs. Points Scored", x="Minutes Played", y="Points Scored") +
  theme_minimal()

