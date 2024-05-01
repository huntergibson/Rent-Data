# Hunter Gibson
# 10/18/2023
# Final Project

####################### Webscraping File #######################

# Clear Environment
rm(list=ls())

# Load in rent dataset
library(xml2)
library(openxlsx)
library(rvest)
setwd("C:/Users/hunte/OneDrive - University of Iowa/School Notes/First Semester Senior Year/Data Wrangling/Projects")
rent_xlsx <- read.xlsx("apartments_for_rent_classified_10K_2.xlsx", sheet = 1)
write.csv(rent_xlsx, "rent.csv", row.names = FALSE)
rent_df <- read.csv("rent.csv", header = TRUE, na.strings = c(""," ","NA","null"))
rent_df <- na.omit(rent_df)
# Scrape Wise Voter Data
page<-read_html("https://wisevoter.com/state-rankings/average-income-by-state/") 
class(page)
page

# Scrape State Text
state <- page %>% html_nodes(".shdb-on-page-table-body-Geo") %>% html_text()

# Scrape Annual Average Income
Average_Income <- page %>% html_nodes(".shdb-on-page-table-body-Data") %>% html_text()

# Scrape Percent Change
Percent_Change <- page %>% html_nodes("#shdb-on-page-table-body-Change-positive") %>% html_text()

# Create Data Frame
Income_df = data.frame(state, Average_Income, Percent_Change, stringsAsFactors = FALSE)

View(Income_df)

# Change states to the abbreviated versions to match rent df
state_abbreviations <- c(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "District of Columbia" = "DC",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Mississippi" = "MS",
  "Missouri" = "MO",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Vermont" = "VT",
  "Virginia" = "VA",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY"
)

# Replace full state names with abbreviations
Income_df$state <- state_abbreviations[Income_df$state]

# Print the modified dataframe
View(Income_df)

# Merge the two dataframes on the state column
merged_df <- merge(rent_df, Income_df, by = "state")

# Check Differences
differences <- setdiff(rent_df$state, Income_df$state)
print(differences)

print(sort(unique(Income_df$state)))
print(sort(unique(rent_df$state)))

# Write to csv
write.csv(merged_df, file = "clean_data.csv", row.names = FALSE)



################################### Data Analysis ###################################

# Read in clean csv file
clean_df <- read.csv("clean_data.csv", header = TRUE)
View(clean_df)

# Remove dollar sign and convert to numeric for Average Income
clean_df$Average_Income_Num <- as.numeric(gsub("[$,]", "", clean_df$Average_Income))

# Remove percent sign, divide by 100, and convert to numeric for Percent Change
clean_df$Percent_Change_Num <- as.numeric(gsub("%", "", clean_df$Percent_Change)) / 100

# Check structure of the data
str(clean_df)


# Load in libraries
library(dplyr)
library(ggplot2)


# Load necessary libraries
# install.packages("corrplot")
library(corrplot)

# Create correlation matrix 
correlation_matrix <- cor(clean_df[, c("bathrooms", "bedrooms", "price", "square_feet", "Average_Income_Num", "Percent_Change_Num")])

# Create a correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
print(corrplot)

############################# Top average rent ############################# 

# Make sure price column is numeric
clean_df$price <- as.numeric(clean_df$price)

# Aggregate data by state, summing the prices
agg_df <- clean_df %>%
  group_by(state) %>%
  summarise(average_price = round(mean(price), 0)) %>%
  arrange(desc(average_price))

# Selects the top 10 aggregated states
top_10_states <- head(agg_df, 10)

# Creates a bar chart showing states with highest average rent
ggplot(top_10_states, aes(x = reorder(state, -average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Highest Average Rent",
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

############################# Lowest average rent #############################

# Selects the bottom 10 aggregated states
bottom_10_states <- tail(agg_df, 10)

# Creates a bar chart showing states with lowest average rent
ggplot(bottom_10_states, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Bottom 10 States with Lowest Average Rent",  # Adjust title
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")


############################# Highest average income #############################


# Makes sure average income is numeric

clean_df$Average_Income_Num <- as.numeric(clean_df$Average_Income_Num)

unique_states <- clean_df %>%
  distinct(state, .keep_all = TRUE) %>%
  arrange(desc(Average_Income_Num))

# Selects the top 10 unique states
top_10_income_states <- head(unique_states, 10)

# Creates a bar chart showing states with highest average income
ggplot(top_10_income_states, aes(x = reorder(state, -Average_Income_Num), y = Average_Income_Num, fill = Average_Income_Num)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Highest Average Income",
       x = "State",
       y = "Average Income") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(Average_Income_Num)), y = Average_Income_Num), vjust = -1.0, color = "black", size = 3, fontface = "bold")


############################# Lowest average income #############################

# Only pulls unique states
unique_states <- clean_df %>%
  distinct(state, .keep_all = TRUE) %>%
  arrange(Average_Income_Num)  # Change from desc(Average_Income_Num) to Average_Income_Num

# Selects the bottom 10 unique states
bottom_10_income_states <- head(unique_states, 10)

# Creates a bar chart showing states with lowest average income
ggplot(bottom_10_income_states, aes(x = reorder(state, Average_Income_Num), y = Average_Income_Num, fill = Average_Income_Num)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Bottom 10 States with Lowest Average Income",  
       x = "State",
       y = "Average Income") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(Average_Income_Num)), y = Average_Income_Num), vjust = -1.0, color = "black", size = 3, fontface = "bold")

############################# Comparisons #############################

# Compare bottom states based on income and rent price
common_bottom_states <- intersect(bottom_10_income_states$state, bottom_10_states$state)

# Print common states
print(common_bottom_states)

# Compare top states based on income and rent price
common_top_states <- intersect(top_10_income_states$state, top_10_states$state)

# Print common states
print(common_top_states)

#  Compare bottom states based on income and rent price and bathroom number
common_bottom_bathroom <- intersect(bottom_10_states$state, test_df$state)

# Print common states
print(common_bottom_bathroom)

############################# Bathrooms + Bedrooms #############################

# Makes sure price, bathrooms, and bedrooms columns are numeric
clean_df$price <- as.numeric(clean_df$price)
clean_df$bathrooms <- as.numeric(clean_df$bathrooms)
clean_df$bedrooms <- as.numeric(clean_df$bedrooms)

# Aggregate data by the number of bathrooms and bedrooms, calculating the average price
average_price_by_bathrooms <- clean_df %>%
  group_by(bathrooms) %>%
  summarise(average_price = mean(price, na.rm = TRUE))

# Print result
print(average_price_by_bathrooms)

# Aggregate data by the number of bathrooms and bedrooms, calculating the average price
average_price_by_bedrooms <- clean_df %>%
  group_by(bedrooms) %>%
  summarise(average_price = mean(price, na.rm = TRUE))

# Print result
print(average_price_by_bedrooms)


############################# Pets Column #############################

# Check values for column
unique(clean_df$pets_allowed)

# Create a new column pets_status based on the pets allowed column
clean_df <- clean_df %>%
  mutate(pets_status = ifelse(pets_allowed == "None", "No", "Yes"))

# Print results
print(clean_df)


############################# price bedroom #############################

# 1 bedroom df
average_price_by_state_bedrooms_1 <- clean_df %>%
  filter(bedrooms == 1) %>%
  group_by(state, bedrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bedroom_1 <- head(average_price_by_state_bedrooms_1, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bedroom_1, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 1 Bedroom",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

# 2 bedroom df
average_price_by_state_bedrooms_2 <- clean_df %>%
  filter(bedrooms == 2) %>%
  group_by(state, bedrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bedroom_2 <- head(average_price_by_state_bedrooms_2, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bedroom_2, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 2 Bedrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

# 3 bedroom df
average_price_by_state_bedrooms_3 <- clean_df %>%
  filter(bedrooms == 3) %>%
  group_by(state, bedrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bedroom_3 <- head(average_price_by_state_bedrooms_3, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bedroom_3, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 3 Bedrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")


# 4 bedroom df
average_price_by_state_bedrooms_4 <- clean_df %>%
  filter(bedrooms == 4) %>%
  group_by(state, bedrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bedroom_4 <- head(average_price_by_state_bedrooms_4, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bedroom_4, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 4 Bedrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

############################# price bathroom #############################

# 1 bathroom df
average_price_by_state_bathrooms_1 <- clean_df %>%
  filter(bathrooms == 1) %>%
  group_by(state, bathrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bathroom_1 <- head(average_price_by_state_bathrooms_1, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bathroom_1, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 1 Bathroom",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

# 2 bathroom df
average_price_by_state_bathrooms_2 <- clean_df %>%
  filter(bathrooms == 2) %>%
  group_by(state, bathrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bathroom_2 <- head(average_price_by_state_bathrooms_2, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bathroom_2, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 2 Bathrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

# 3 bathroom df
average_price_by_state_bathrooms_3 <- clean_df %>%
  filter(bathrooms == 3) %>%
  group_by(state, bathrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bathroom_3 <- head(average_price_by_state_bathrooms_3, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bathroom_3, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 3 Bathrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

# 4 bathroom df
average_price_by_state_bathrooms_4 <- clean_df %>%
  filter(bathrooms == 4) %>%
  group_by(state, bathrooms) %>%
  summarise(average_price = round(mean(price, na.rm = TRUE), 0)) %>%
  arrange(average_price)

# Find top 10 lowest price
top_10_bathroom_4 <- head(average_price_by_state_bathrooms_4, 10)

# Create bar chart to show price by state with bedroom filter
ggplot(top_10_bathroom_4, aes(x = reorder(state, average_price), y = average_price, fill = average_price)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 10 States with Lowest Rental Price for 4 Bathrooms",  
       x = "State",
       y = "Average Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price), vjust = -1.0, color = "black", size = 3, fontface = "bold")

############################# price pets #############################

# Calculate average rent price for pets_status of "Yes"
average_price_yes <- mean(clean_df$price[clean_df$pets_status == "Yes"], na.rm = TRUE)

# Calculate average rent price for pets_status of "No"
average_price_no <- mean(clean_df$price[clean_df$pets_status == "No"], na.rm = TRUE)

# Prints the results
cat("Average Rent Price for Pets Status 'Yes': $", round(average_price_yes, 2), "\n")
cat("Average Rent Price for Pets Status 'No': $", round(average_price_no, 2), "\n")

# Agg data for avg price by pet status
average_prices_pets <- clean_df %>%
  group_by(pets_status) %>%
  summarise(average_price = mean(price, na.rm = TRUE))

# Print the results
print(average_prices_pets)

# Creates a bar chart for pet status and avg rent price
ggplot(average_prices_pets, aes(x = reorder(pets_status, average_price), y = average_price, fill = pets_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("black", "lightgreen")) +
  labs(title = "Average Rent Price by Pets Status",  
       x = "Pets Status",
       y = "Average Rent Price") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  geom_text(aes(label = paste0("$", scales::comma(average_price)), y = average_price),
            vjust = -1.0, color = "black", size = 3, fontface = "bold")

