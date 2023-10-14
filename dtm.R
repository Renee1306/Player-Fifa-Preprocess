library(readr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(magrittr)
library(tidyverse)
library(plotrix)
library(gridExtra)

#Data Import
#read csv file
playerdata = read.csv("Player_FIFA22.csv",header=TRUE)
View(playerdata)

#1
str(playerdata)

table(playerdata)

ggplot(playerdata, aes(x = weight_kg)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  scale_x_continuous(breaks = seq(50, max(playerdata$weight_kg), by = 10)) +
  labs(x = "Weight (kg)", y = "Frequency", title = "Histogram of Weight") 

#Weight
# Value ranges
min_weight <- min(playerdata$weight_kg)
max_weight <- max(playerdata$weight_kg)
range_weight <- max_weight - min_weight
# Median
median_weight <- median(playerdata$weight_kg)
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode <- unique_values[which.max(frequencies)]
  return(mode)
}
mode_weight <- get_mode(playerdata$weight_kg)
mean_weight <- round(mean(playerdata$weight_kg),2)
# Variance
var_weight <- round(var(playerdata$weight_kg),2)
# Percentiles
percentiles <- quantile(playerdata$weight_kg, probs = c(0.25, 0.5, 0.75))
# Create a table
result_table <- data.frame(
  "Statistic" = c("Weight Range", "Median", "Mean", "Mode","Variance", "25th Percentile", "50th Percentile", "75th Percentile"),
  "Weight" = c(range_weight, median_weight, mean_weight,mode_weight, var_weight, percentiles)
)
result_table

# Calculate count percentage
position_counts <- position_counts %>%
  mutate(count_percentage = n / sum(n) * 100)
# Calculate cumulative frequency and cumulative percent
position_counts <- position_counts %>%
  mutate(cumulative_frequency = cumsum(n),
         cumulative_percent = cumulative_frequency / sum(n) * 100)
# Print the updated position_counts data frame
print(position_counts, Inf)

min_wage <- min(playerdata$wage_eur, na.rm = TRUE)
max_wage <- max(playerdata$wage_eur, na.rm = TRUE)
range_wage <- max_wage - min_wage
median_wage <- median(playerdata$wage_eur, na.rm = TRUE)
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode <- unique_values[which.max(frequencies)]
  return(mode)
}
mode_wage <- get_mode(playerdata$wage_eur)
mean_wage <- round(mean(playerdata$wage_eur, na.rm = TRUE), 2)
var_wage <- round(var(playerdata$wage_eur, na.rm = TRUE), 2)
sd_wage <- round(sd(playerdata$wage_eur, na.rm = TRUE), 2)
percentiles <- quantile(playerdata$wage_eur, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
missing_count <- sum(is.na(playerdata$wage_eur))
# Set the display format to suppress scientific notation
options(scipen = 999)
result_table <- data.frame(
  "Statistic" = c("Wage Range", "Minimum", "Maximum", "Median", "Mode", "Mean", "Variance", "Standard Deviation", "25th Percentile", "50th Percentile", "75th Percentile", "Missing Values"),
  "Wage(EUR)" = c(range_wage, min_wage, max_wage, median_wage, mode_wage, mean_wage, var_wage, sd_wage, percentiles, missing_count)
)
colnames(result_table) <- c("Statistic", "Wage (EUR)")

result_table


#player position
playerdata <- playerdata %>%
  separate_rows(player_positions, sep = ", ")
position_counts <- playerdata %>%
  count(player_positions)
print(position_counts,Inf)
ggplot(position_counts, aes(x = player_positions, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Player Position", y = "Count", title = "Distribution of Player Positions")


# Calculate the count distribution of league_name
league_count <- table(playerdata$league_name)
league_count_df <- data.frame(League = names(league_count), Count = as.numeric(league_count))
league_count_df 
# Create the bar plot
ggplot(league_count_df, aes(x = League, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(x = "League Name", y = "Count", title = "Count Distribution of League Names") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create the line graph
ggplot(league_count_df, aes(x = League, y = Count)) +
  geom_line() +
  geom_point() +
  labs(x = "League Name", y = "Count", title = "Count Distribution of League Names") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Calculate the count distribution of league_level
league_level_count <- table(playerdata$league_level)
league_level_count_df <- data.frame(League_Level = names(league_level_count), Count = as.numeric(league_level_count))

# Calculate count percentage
league_level_count_df$Percentage <- with(league_level_count_df, round((Count / sum(Count)) * 100, 2))

# Print the table
print(league_level_count_df)
summary(playerdata$league_level)
# Create the pie chart
ggplot(league_level_count_df, aes(x = "", y = Count, fill = League_Level)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(fill = "League Level", title = "Distribution of League Levels") +
  theme_void()

# Calculate the count of each number
jersey_counts <- table(playerdata$nation_jersey_number)
# Calculate the percentage of each number
jersey_percentages <- prop.table(jersey_counts) * 100
# Create a table with count and percentage
result_table <- data.frame(
  "National Jersey Number" = names(jersey_counts),
  "Count" = jersey_counts,
  "Percentage" = jersey_percentages
)
# Select columns 1, 3, and 5
selected_columns <- result_table[, c(1, 3, 5)]
colnames(selected_columns) <- c("National Jersey Number", "Count", "Percentage")
# Print the selected columns
print(selected_columns)
summary(playerdata$nation_jersey_number)

# Count missing values
missing_count <- sum(is.na(playerdata$wage_eur))
# Count unique values
unique_count <- length(unique(playerdata$wage_eur))
unique_value <- unique(playerdata$wage_eur)
missing_count
unique_count
unique_value


ggplot(playerdata, aes(x = "", y = mentality_penalties)) +
  geom_boxplot() +
  labs(x = "", y = "Mentality Penalties") +
  ggtitle("Boxplot of Mentality Penalties")+
  theme_minimal()


# Calculate the count of players for each release clause value
release_counts <- table(playerdata$release_clause_eur)

# Create a data frame with release clause values and player counts
release_data <- data.frame(
  release_clause_eur = as.numeric(names(release_counts)),
  count = as.numeric(release_counts)
)

# Sort the data frame by release clause values in ascending order
release_data <- release_data[order(release_data$release_clause_eur), ]

# Plot the line graph
ggplot(release_data, aes(x = release_clause_eur, y = count)) +
  geom_line() +
  labs(x = "Release Clause (EUR)", y = "Number of Players", title = "Player Count by Release Clause") +
  theme_minimal()


# Value ranges
min_age <- min(playerdata$release_clause_eur, na.rm = TRUE)
max_age <- max(playerdata$release_clause_eur, na.rm = TRUE)
min_age
max_age

range_age <- max_age - min_age

# Median
median_age <- median(playerdata$age, na.rm = TRUE)

# Mode
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode <- unique_values[which.max(frequencies)]
  return(mode)
}
mode_age <- get_mode(playerdata$age)

# Mean
mean_age <- round(mean(playerdata$age, na.rm = TRUE), 2)

# Variance
var_age <- round(var(playerdata$age, na.rm = TRUE), 2)

# Standard Deviation
sd_age <- round(sd(playerdata$age, na.rm = TRUE), 2)

# Percentiles
percentiles <- quantile(playerdata$age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Count of missing values
missing_count <- sum(is.na(playerdata$age))

# Create a table
result_table <- data.frame(
  "Statistic" = c("Age Range", "Minimum", "Maximum", "Median", "Mode", "Mean", "Variance", "Standard Deviation", "25th Percentile", "50th Percentile", "75th Percentile", "Missing Values"),
  "Age" = c(range_age, min_age, max_age, median_age, mode_age, mean_age, var_age, sd_age, percentiles, missing_count)
)
result_table


min_release_clause <- as.integer(min(playerdata$attacking_short_passing, na.rm = TRUE))
max_release_clause <- as.integer(max(playerdata$attacking_short_passing, na.rm = TRUE))
median_release_clause <- as.integer(median(playerdata$attacking_short_passing, na.rm = TRUE))
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode <- unique_values[which.max(frequencies)]
  return(as.integer(mode))
}
mode_release_clause <- get_mode(playerdata$attacking_short_passing)
mean_release_clause <- as.integer(round(mean(playerdata$attacking_short_passing, na.rm = TRUE), 2))
var_release_clause <- as.integer(round(var(playerdata$attacking_short_passing, na.rm = TRUE), 2))
sd_release_clause <- as.integer(round(sd(playerdata$attacking_short_passing, na.rm = TRUE), 2))
percentiles <- quantile(playerdata$attacking_short_passing, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
missing_count <- sum(is.na(playerdata$attacking_short_passing))
# Set the display format to suppress scientific notation
options(scipen = 999)
result_table <- data.frame(
  "Statistic" = c( "Minimum", "Maximum", "Median", "Mode", "Mean", "Variance", "Standard Deviation", "25th Percentile", "50th Percentile", "75th Percentile", "Missing Values"),
  "ClubContractValidUntil/Year" = c(min_release_clause, max_release_clause, median_release_clause, mode_release_clause, mean_release_clause, var_release_clause, sd_release_clause, percentiles, missing_count)
)
result_table



# Count the frequency of each unique value in the column
frequency <- table(playerdata$club_contract_valid_until)
# Create a data frame with the values and frequencies
data <- data.frame(Valid_Until = names(frequency), Count = as.numeric(frequency))
# Create a bar chart
ggplot(data = data, aes(x = Valid_Until, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3, color = "black") +  # Add labels with count
  labs(x = "Club Contract Valid Until (Year)", y = "Number of Player") +
  ggtitle("Bar Chart of Club Contract Valid Until") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


sum(is.na(playerdata$mentality_penalties))


min_diving <- min(playerdata$attacking_short_passing, na.rm = TRUE)
max_diving <- max(playerdata$attacking_short_passing, na.rm = TRUE)
range_diving <- max_diving - min_diving
median_diving <- median(playerdata$attacking_short_passing, na.rm = TRUE)
get_mode <- function(x) {
  unique_values <- unique(x)
  frequencies <- tabulate(match(x, unique_values))
  mode <- unique_values[which.max(frequencies)]
  return(mode)
}
mode_diving <- get_mode(playerdata$attacking_short_passing)
mean_diving <- round(mean(playerdata$attacking_short_passing, na.rm = TRUE), 2)
var_diving <- round(var(playerdata$attacking_short_passing, na.rm = TRUE), 2)
sd_diving <- round(sd(playerdata$attacking_short_passing, na.rm = TRUE), 2)
percentiles <- quantile(playerdata$attacking_short_passing, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
missing_count <- sum(is.na(playerdata$attacking_short_passing))

result_table <- data.frame(
  "Statistic" = c("Range", "Minimum", "Maximum", "Median", "Mode", "Mean", "Variance", "Standard Deviation", "25th Percentile", "50th Percentile", "75th Percentile", "Missing Values"),
  "Mentality Penalties" = c(range_diving, min_diving, max_diving, median_diving, mode_diving, mean_diving, var_diving, sd_diving, percentiles, missing_count)
)
colnames(result_table) <- c("Statistic", "Attacking Short Passing")
result_table

# Plotting the boxplot
ggplot(playerdata, aes(y = defending)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot of Attacking Volleys", y = "Attacking Volleys")+
  theme_minimal()


# Plotting histogram
ggplot(playerdata, aes(x = attacking_short_passing)) +
  geom_histogram(binwidth = 1, fill = "lightblue",color="white") +
  labs(x = "Attacking Short Passsing", y = "Number of Players", title = "Histogram of Attacking Short Passing")+
  theme_minimal()

library(ggplot2)

# Plotting the histogram
ggplot(playerdata, aes(x = mentality_penalties)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Histogram of Mentality Penalties", x = "Mentality Penalties", y = "Number of Players")

ggplot(playerdata, aes(x = goalkeeping_diving)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  labs(title = "Histogram of Goalkeeping Diving",
       x = "Goalkeeping Diving",
       y = "Count")



# Count number of players for each preferred_foot
preferred_foot_counts <- table(playerdata$club_name, useNA = "ifany")

# Calculate percentage
preferred_foot_percentage <- prop.table(preferred_foot_counts) * 100

# Create a table
result_table <- data.frame(
  "Preferred Foot" = names(preferred_foot_counts),
  "Count" = preferred_foot_counts,
  "Percentage" = round(preferred_foot_percentage,2)
)
result_table <- result_table[, c("Preferred.Foot", "Count.Freq", "Percentage.Freq")]
colnames(result_table) <- c("Preferred Foot", "Count", "Percentage")
result_table



library(ggplot2)

ggplot(result_table, aes(x = `Preferred Foot`, y = Count)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(x = "Preferred Foot", y = "Number of Players", title = "Preferred Foot") +
  theme_minimal()
unique(playerdata$club_position)

is.na(playerdata$club_position)


# Count number of players for each club_position
club_position_counts <- table(playerdata$club_position, useNA = "ifany")

# Calculate percentage
club_position_percentage <- prop.table(club_position_counts) * 100

# Identify missing values
missing_count <- sum(is.na(playerdata$club_position))
missing_percentage <- missing_count / length(playerdata$club_position) * 100

# Create a table
result_table <- data.frame(
  "Club Position" = c(names(club_position_counts), "Missing"),
  "Count" = c(club_position_counts, missing_count),
  "Percentage" = c(round(club_position_percentage, 2), round(missing_percentage, 2))
)

# Sort the table by count in descending order
result_table <- result_table[order(result_table$Count, decreasing = TRUE), ]

colnames(result_table) <- c("Club Position", "Count", "Percentage")
result_table





cleaned_data <- playerdata[!is.na(playerdata$club_position), ]
install.packages("stringr")
library(stringr)

# Remove special characters from club_name column
your_data_frame$club_name <- str_replace_all(your_data_frame$club_name, "[^[:alnum:]]", "")

# Create a bar chart for club_position
ggplot(data=playerdata, aes(x = club_name)) +
  geom_bar(fill="pink") +
  xlab("Club Name") +
  ylab("Number of Player") +
  ggtitle("Bar Chart of Club Name")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

count(unique(playerdata$club_name))

unique_clubs <- playerdata %>%
  distinct(club_name) %>%
  n_distinct()
unique_clubs
# Count number of players for each club_name
club_name_counts <- table(playerdata$club_name, useNA = "ifany")

# Calculate percentage
club_name_percentage <- prop.table(club_name_counts) * 100

# Create a table
result_table <- data.frame(
  "Club Name" = names(club_name_counts),
  "Count" = club_name_counts,
  "Percentage" = round(club_name_percentage, 2)
)
result_table <- result_table[order(result_table$Count.Freq, decreasing = TRUE), ]
result_table <- result_table[2:100, ]  # Subset from second row onwards
result_table <- result_table[, c("Club.Name", "Count.Freq", "Percentage.Freq")]
colnames(result_table) <- c("Club Name", "Count", "Percentage")
result_table

library(dplyr)
library(ggplot2)
club_counts <- playerdata %>% 
  count(club_name)

ggplot(club_counts, aes(x = "", y = n, fill = club_name)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill = "Club Name") +
  theme_void()
cleaned_data <- playerdata %>%
  filter(club_name)%>%
  mutate(cleaned_club_name = gsub("[\\\\?]", "", club_name))
unique(playerdata$player_traits)



library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming player data is stored in a data frame called player_data

split_traits <- playerdata %>%
  separate_rows(player_traits, sep = ", ")

trait_counts <- split_traits %>%
  count(player_traits)

ggplot(trait_counts, aes(x = player_traits, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Player Trait") +
  ylab("Number of Players") +
  ggtitle("Player Trait Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











# Split traits into separate rows
split_traits <- playerdata %>%
  separate_rows(player_traits, sep = ", ")

# Count the number of players for each trait
trait_counts <- split_traits %>%
  count(player_traits) %>%
  rename(Trait = player_traits, Count = n)

# Calculate the percentage
total_players <- nrow(playerdata)
trait_counts <- trait_counts %>%
  mutate(Percentage = round((Count / total_players) * 100, 2))

# Sort the trait counts from high to low
trait_counts <- trait_counts %>%
  arrange(desc(Count))

# Print the trait counts and percentages
print(trait_counts,n=Inf)



missing_count <- sum(is.na(playerdata$defending) | playerdata$defending == "")
unique_values <- unique(playerdata$defending)

num_unique_values <- length(unique_values)
missing_count

num_unique_values

unique_values
# Replace NA with 0
playerdata$goalkeeping_speed[is.na(playerdata$goalkeeping_speed)] <- 0

missingvalue <-sum(is.na(playerdata$goalkeeping_speed))
uniquevalue<-unique(playerdata$goalkeeping_speed)
missingvalue
uniquevalue

# Convert dob column to Date format
playerdata$dob <- as.Date(playerdata$dob)

# Calculate age based on current date and dob
playerdata$age <- ifelse(is.na(playerdata$dob), NA, floor(difftime(Sys.Date(), playerdata$dob, units = "days") / 365))

# Group by sofifaid and dob, and calculate the number of empty data
age_empty_data <- aggregate(age ~ sofifa_id + dob, data = playerdata, FUN = function(x) sum(is.na(x)))

# Display the result
age_empty_data


sum(is.na(playerdata$age))
library(dplyr)





library(reshape2)
library(ggplot2)

# Select the columns of interest
columns_of_interest <- c("defending", "defending_marking_awareness","defending_standing_tackle","defending_sliding_tackle")

# Subset the data with the selected columns
subset_data <- playerdata[, columns_of_interest]

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data, use = "complete.obs")

# Convert the correlation matrix to a data frame for plotting
cor_df <- as.data.frame(correlation_matrix)
cor_df$row <- rownames(cor_df)

# Reshape the data for plotting
cor_df_long <- reshape2::melt(cor_df, id.vars = "row", variable.name = "column", value.name = "correlation")

# Plot the correlation heatmap
ggplot(cor_df_long, aes(x = column, y = row, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "yellow", mid = "white", high = "coral", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(x = "Columns", y = "Columns", title = "Correlation Heatmap for Defending")+
  geom_text(aes(label = round(correlation, 2)), color = "black", size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5))




library(dplyr)
# Subset the data to include only the relevant columns
subset_data <- playerdata[, c("defending", "defending_sliding_tackle")]

# Create a grouping variable based on the "defending_sliding_tackle" column
grouped_data <- subset_data[!is.na(subset_data$defending), ] %>%
  group_by(defending_sliding_tackle)

# Calculate the mean "wage" value for each group
grouped_means <- summarise(grouped_data, mean_defending = mean(defending))

# Replace the missing values in the "defending" column using the mean values 
# from the corresponding "defending_sliding_tackle" group
playerdata$physic[is.na(playerdata$defending)] <- 
  grouped_means$mean_defending[match(playerdata$defending_sliding_tackle[is.na(playerdata$defending)], 
                                  grouped_means$defending_sliding_tackle)]

# Replace empty spaces in the "defending" column with the mean "defending" value 
# from the entire column
playerdata$defending[is.na(playerdata$defending)] <- mean(playerdata$defending, na.rm = TRUE)

playerdata$defending<-as.integer(playerdata$defending)

sum(is.na(playerdata$power_shot_power))

unique(playerdata$defending)

# Calculate the lower and upper fences
q1 <- quantile(playerdata$mentality_penalties, 0.25)
q3 <- quantile(playerdata$mentality_penalties, 0.75)
iqr <- q3 - q1
lower_fence = q1 - (1.5 * iqr)
upper_fence = q3 + (1.5 * iqr)

# Count the number of lower outliers
num_lower_outliers <- sum(playerdata$mentality_penalties < lower_fence)

# Count the number of upper outliers
num_upper_outliers <- sum(playerdata$mentality_penalties > upper_fence)

# Print the counts of lower and upper outliers
print(lower_fence)
print(upper_fence)
print(num_lower_outliers)
print(num_upper_outliers)


# Plotting the boxplot with adjusted y-axis limits
ggplot(playerdata, aes(y = club_contract_valid_until)) +
  geom_boxplot(color = "black") +
  labs(title = "Boxplot of Mentality Penalties", y = "Mentality Penalties") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 



# Calculate median and IQR
median_value <- median(playerdata$mentality_penalties, na.rm = TRUE)
q1 <- quantile(playerdata$mentality_penalties, 0.25, na.rm = TRUE)
q3 <- quantile(playerdata$mentality_penalties, 0.75, na.rm = TRUE)
iqr <- q3 - q1

# Define lower and upper thresholds
lower_threshold <- q1 - 1.5 * iqr
upper_threshold <- q3 + 1.5 * iqr

# Identify outliers
outliers <- playerdata$mentality_penalties < lower_threshold | playerdata$goalkeeping_diving > upper_threshold

# Replace outliers with median value
playerdata$goalkeeping_diving[outliers] <- median_value


# Subset the dataset to include only the relevant columns
subset_data <- playerdata[, c("physic", "power_strength")]

# Remove rows with missing values in both columns
subset_data <- subset_data[complete.cases(subset_data), ]

# Calculate the correlation coefficient between "physic" and "power_strength"
correlation <- cor(subset_data$physic, subset_data$power_strength)

# Replace the missing values in the "physic" column using the correlation
missing_indices <- which(is.na(playerdata$physic))
playerdata$physic[missing_indices] <- playerdata$power_strength[missing_indices] * correlation
playerdata$physic <-as.integer(playerdata$physic)
unique(playerdata$physic)

# Calculate the lower and upper quartiles
q1 <- quantile(playerdata$mentality_penalties, 0.25)
q3 <- quantile(playerdata$mentality_penalties, 0.75)

# Calculate the interquartile range (IQR)
iqr <- q3 - q1

# Define the lower and upper thresholds for outliers
lower_threshold <- q1 - 1.5 * iqr
upper_threshold <- q3 + 1.5 * iqr

# Remove outliers
playerdata <- playerdata[playerdata$mentality_penalties >= lower_threshold & 
                           playerdata$mentality_penalties <= upper_threshold, ]
unique(playerdata$club_contract_valid_until)


# Subset the data for players with age 17
subset_data <- playerdata[playerdata$age == 17, c("sofifa_id", "dob", "age")]

# Remove rows with NA values
subset_data <- subset_data[complete.cases(subset_data), ]

# Print the subsetted data
print(subset_data)



# Filter rows with preferred_foot as "l" or "r" and group by sofifa_id and preferred_foot
filtered_data <- playerdata %>% 
  filter(preferred_foot %in% c("L", "R")) %>% 
  group_by(sofifa_id, preferred_foot) %>% 
  select(sofifa_id, preferred_foot)

# Print the filtered data
print(playerdata)

# Replace "L" with "Left" and "R" with "Right"
foot_dp <- playerdata %>% 
  mutate(preferred_foot = ifelse(preferred_foot == "L", "Left", 
                                 ifelse(preferred_foot == "R", "Right", preferred_foot)))

print(foot_dp)
print(unique_foot)

empty_count <- sum(playerdata$club_position == "")
print(empty_count)

# Replace NULL values with "Unknown" in club_position column
playerdata$club_position[playerdata$club_position == ""] <- "Unknown"

library(dplyr)

empty_positions <- playerdata[playerdata$club_position == "", c("sofifa_id", "club_position")] %>%
  group_by(sofifa_id, club_position) %>%
  summarise(count = n())
unique(playerdata$club_position)

print(empty_positions)
empty_positions 

# Count occurrences of "JOKE", "MMD", "74" in club_position
count_joke <- sum(playerdata$club_position == "JOKE")
count_mmd <- sum(playerdata$club_position == "MMD")
count_74 <- sum(playerdata$club_position == "74")

# Print the counts
print(count_joke)
print(count_mmd)
print(count_74)


library(dplyr)

# Remove rows with specific club positions
playerdata <- playerdata %>%
  filter(club_position != "JOKE" & club_position != "MMD" & club_position != "74")

new_playerdata <- playerdata[!is.na(playerdata$club_contract_valid_until), ]
playerdata <-new_playerdata
unique(playerdata$club_contract_valid_until)
count_3023 <- sum(playerdata$club_contract_valid_until == 3023, na.rm = TRUE)
count_3023
count_3023 <- table(playerdata$club_contract_valid_until)["3023"]
playerdata$club_contract_valid_until[is.na(playerdata$club_contract_valid_until)] <- 2023

# Replace "3023" with "2023"
playerdata$club_contract_valid_until[playerdata$club_contract_valid_until == 3023] <- 2023
unique()