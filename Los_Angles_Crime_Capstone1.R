
library(ggplot2)
library(readr)
library(dplyr)


theme_set(theme_minimal())


file_path <- "C:/Users/Zaur/OneDrive/Desktop/Crime_Data_from_2020_to_Present.csv"
crime_data <- read_csv(file_path)

# shape of the data
dim(crime_data) 

#columns
print(colnames(crime_data))
#first a few row
print(head(crime_data))

# Basic summary statistics
print(summary(crime_data))

#check missing data

print(colSums(is.na(crime_data)))

crime_data <- crime_data %>% distinct()



# Count occurrences of each area
value_counts <- table(crime_data$`AREA NAME`)
print(value_counts)

# Compute mean value of area counts
mean_value <- mean(value_counts)
print(paste("Mean value of area counts:", as.integer(mean_value)))





# Convert value_counts to a data frame for ggplot
value_counts_df <- as.data.frame(value_counts)
colnames(value_counts_df) <- c("Area_Name", "Number_of_Crimes")

# Create the bar plot with alternative styling
ggplot(value_counts_df, aes(x = reorder(Area_Name, Number_of_Crimes), y = Number_of_Crimes)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  labs(title = "Crimes Count by Area", x = "Area Name", y = "Number of Crimes") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "darkblue", size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dotted", color = "gray60"),
        plot.margin = margin(2, 2, 2, 2, "cm")) +
  coord_flip()  # Optional: flip coordinates for better readability



# Create a data frame for the pie chart
vict_sex_counts <- as.data.frame(table(crime_data$`Vict Sex`))
colnames(vict_sex_counts) <- c("Vict_Sex", "Count")

# Create the pie chart
ggplot(vict_sex_counts, aes(x = "", y = Count, fill = Vict_Sex)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Victim Sex") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(Count / sum(Count))), position = position_stack(vjust = 0.5), size = 5)



#Age >0
filtered_data <- crime_data %>% filter(`Vict Age` > 0)

# Count occurrences of victim ages
victim_age_counts <- filtered_data %>% count(`Vict Age`)

# Print counts
print(victim_age_counts)

# Plot the data
ggplot(victim_age_counts, aes(x = `Vict Age`, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Victim Ages", x = "Age", y = "Frequency") +
  theme_minimal()
 


# Assuming you have a dataframe `top_10_crimes` with crime types and counts


crime_counts <- crime_data %>%
  count(Crime_Type = `Crm Cd Desc`) %>%
  arrange(desc(n))

# Get the top 10 crime types
top_10_crimes <- head(crime_counts, 10)
print(top_10_crimes)

#status count
crime_by_status <- crime_data %>%
  count(Status) %>%
  arrange(desc(n))

crime_by_status_df <- crime_by_status %>%
  rename(Count = n, Status = Status)

print(crime_by_status_df)

