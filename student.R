# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Set working directory (Modify if needed)
setwd("~/Documents/studentdata project")

# Load the data
student_df <- read_excel("Student.xlsx")
registration_df <- read_excel("Registration.xlsx")
course_df <- read_excel("Course.xlsx")

# Check column names to avoid merge issues
print(colnames(student_df))
print(colnames(registration_df))
print(colnames(course_df))

# Merge the data using left joins
merged_df <- registration_df %>%
  left_join(student_df, by = "Student ID") %>%
  left_join(course_df, by = "Instance ID")

# Ensure merge is successful
print(head(merged_df))

# Extract birth year from Birth Date
merged_df <- merged_df %>%
  mutate(Birth_Year = as.numeric(format(as.Date(`Birth Date`), "%Y")))


# Chart: Number of Students per Major (Title)

title_counts <- merged_df %>%
  count(Title) %>%
  arrange(desc(n))  # Sort for better visualization

ggplot(title_counts, aes(x = reorder(Title, -n), y = n, fill = Title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Number of Students per Major", x = "Major (Title)", y = "Number of Students") +
  theme_classic() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Chart: Distribution of Students by Birth Year

ggplot(merged_df, aes(x = Birth_Year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Students by Birth Year", x = "Birth Year", y = "Number of Students") +
  theme_classic() +
  scale_x_continuous(breaks = seq(min(merged_df$Birth_Year, na.rm = TRUE),
                                  max(merged_df$Birth_Year, na.rm = TRUE), 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


#  Total Cost per Major, Segmented by Payment Plan

total_cost_per_major <- merged_df %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(Total_Cost = sum(`Total Cost`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Cost))

print(total_cost_per_major)


#  Total Balance Due by Major, Segmented by Payment Plan

balance_due_per_major <- merged_df %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(Total_Balance_Due = sum(`Balance Due`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Balance_Due))

print(balance_due_per_major)
