# This R environment comes with many helpful analytics packages installed
# It is defined by the kaggle/rstats Docker image: https://github.com/kaggle/docker-rstats
# For example, here's a helpful package to load

library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session
# Set CRAN mirror to another server
chooseCRANmirror(graphics = FALSE, ind = 1)  # You can also try a specific mirror index (e.g., 1 for USA)
install.packages("tidyverse", repos = "https://cloud.r-project.org/")
install.packages("ggplot2", repos = "https://cloud.r-project.org/")
install.packages("readr", repos = "https://cloud.r-project.org/")
install.packages("corrplot", repos = "https://cloud.r-project.org/")
# Load libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(corrplot)
# Load data (adjust the file path)
df <- read_csv("C:/Users/hp/OneDrive/Desktop/global air pollution dataset.csv")

# Display the first few rows of the dataset
head(df)
# Check for missing values in the dataset
sum(is.na(df))
# Separate numeric and non-numeric columns
numeric_cols <- sapply(df, is.numeric)
non_numeric_cols <- !numeric_cols

# Display the numeric and non-numeric columns
print("Numeric columns:")
print(names(df)[numeric_cols])
print("Non-numeric columns:")
print(names(df)[non_numeric_cols])
# Fill missing values in numeric columns with the mean
df[numeric_cols] <- lapply(df[numeric_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Fill missing values in non-numeric columns with the mode
df[non_numeric_cols] <- lapply(df[non_numeric_cols], function(x) {
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  ifelse(is.na(x), mode_val, x)
})

# Display the first few rows after filling missing values
head(df)
# Check the data types of each column
str(df)

# Example: Convert AQI Value to numeric if it's not already
df$`AQI Value` <- as.numeric(df$`AQI Value`)
# Display summary statistics for numeric columns
summary(df)
# Visualize the distribution of AQI values
ggplot(df, aes(x = `AQI Value`)) +
  geom_histogram(binwidth = 10, color = "black", fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "red") +
  labs(title = "Distribution of AQI Values", x = "AQI Value", y = "Frequency") +
  theme_minimal()
# Plot the relationship between AQI Value and CO AQI Value
ggplot(df, aes(x = `AQI Value`, y = `CO AQI Value`)) +
  geom_point() +
  labs(title = "AQI Value vs. CO AQI Value", x = "AQI Value", y = "CO AQI Value") +
  theme_minimal()
# Calculate and visualize the correlation matrix for numeric columns
correlation_matrix <- cor(df[numeric_cols], use = "complete.obs")

# Create a correlation plot
corrplot(correlation_matrix, method = "circle", type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")