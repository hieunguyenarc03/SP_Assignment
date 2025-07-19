raw_file_path <- "./All_GPUs.csv"
formatted_file_path <- "./All_GPUs_formatted.csv"
cleaned_file_path <- "./All_GPUs_cleaned.csv"

# 1.Read data
gpus_df <- read.csv(raw_file_path, stringsAsFactors = FALSE)

## Format the data (Only 'gpu_formatted_df' is used for further processing)

gpu_formatted_df <- gpus_df

gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  gsub("[\r\n]+", "", column)
})
gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  ifelse(grepl("^\\s*-\\s*$|^\\s*$", column), NA, column)
})
gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  trimws(column)
})

write.csv(gpu_formatted_df, formatted_file_path, row.names = FALSE)

# 2.Initial Data Exploration
## Show the number of rows and columns
dim(gpu_formatted_df)

## Check if there are any duplicated rows
sum(duplicated(gpu_formatted_df))

## View the structure of the dataset: data types, first values, etc.
str(gpu_formatted_df)

## View summary statistics: min, max, mean, median, etc.
summary(gpu_formatted_df)

# 3.Data cleaning
## Print number of missing values in each column
missing_values <- colSums(is.na(gpu_formatted_df))
missing_df <- data.frame(
  Index = seq_along(missing_values),
  Column = names(missing_values),
  Missing_Count = as.vector(missing_values),
  Missing_Percent = round(100 * missing_values / nrow(gpu_formatted_df), 2)
)
missing_df[missing_df$Missing_Count >= 0, ]

## Define selected column names
selected_columns <- c(
  "Core_Speed", "Manufacturer", "Memory",
  "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", 
  "Pixel_Rate", "Process"
)

## Copy selected columns from gpu_formatted_df into a new dataframe
gpu_cleaned_df <- gpu_formatted_df[, selected_columns]

## Clean the data (Only 'gpu_cleaned_df' is used for further processing)

## 'Core_Speed' processing
### Convert 'Core_Speed' column from string (e.g., "738 MHz") to numeric
units_core_speed <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory_Speed)
unique(units_core_speed)
gpu_cleaned_df$Core_Speed <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Core_Speed))

### Visualize distribution of 'Core_Speed'
hist(
  gpu_cleaned_df$Core_Speed,
  breaks = 30,
  col = "skyblue",
  main = "Core_Speed Distribution",
  xlab = "Core Speed (MHz)"
)

### Replace missing values in 'Core_Speed' with the median
median_core_speed <- median(gpu_cleaned_df$Core_Speed, na.rm = TRUE)
gpu_cleaned_df$Core_Speed[is.na(gpu_cleaned_df$Core_Speed)] <- median_core_speed

## 'Memory' processing
### Convert 'Memory' column from string (e.g., "1024 MB") to numeric
units_memory <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory)
unique(units_memory)
gpu_cleaned_df$Memory <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Memory))

### Visualize distribution of 'Memory'
hist(
  gpu_cleaned_df$Memory,
  breaks = 30,
  col = "green",
  main = "Memory Distribution",
  xlab = "Memory (MB)"
)

### Replace missing values in 'Memory' with the median
median_memory <- median(gpu_cleaned_df$Memory, na.rm = TRUE)
gpu_cleaned_df$Memory[is.na(gpu_cleaned_df$Memory)] <- median_memory

## 'Pixel_Rate' processing
### Convert 'Pixel_Rate' column from string (e.g., "12 GPixel/s") to numeric
units_pixel_rate <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Pixel_Rate)
unique(units_pixel_rate)
gpu_cleaned_df$Pixel_Rate <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Pixel_Rate))

### Visualize distribution of 'Pixel_Rate'
hist(
  gpu_cleaned_df$Pixel_Rate,
  breaks = 30,
  col = "skyblue",
  main = "Pixel_Rate Distribution",
  xlab = "Pixel Rate (GPixel/s)"
)

### Replace missing values in 'Pixel_Rate' with the median
median_pixel_rate <- median(gpu_cleaned_df$Pixel_Rate, na.rm = TRUE)
gpu_cleaned_df$Pixel_Rate[is.na(gpu_cleaned_df$Pixel_Rate)] <- median_pixel_rate

## Remove rows with any remaining missing values
gpu_cleaned_df <- gpu_cleaned_df[complete.cases(gpu_cleaned_df), ]

## 'Manufacturer', 'Memory_Bandwidth', 'Memory_Bus', 'Memory_Speed', 'Process' processing
### Convert all columns to character type
gpu_cleaned_df$Manufacturer <- as.factor(gpu_cleaned_df$Manufacturer)

units_memory_bandwidth <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory_Bandwidth)
unique(units_memory_bandwidth)
gpu_cleaned_df$Memory_Bandwidth <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Memory_Bandwidth))

units_memory_bus <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory_Bus)
unique(units_memory_bus)
gpu_cleaned_df$Memory_Bus <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Memory_Bus))

units_memory_speed <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory_Speed)
unique(units_memory_speed)
gpu_cleaned_df$Memory_Speed <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Memory_Speed))

units_process <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Process)
unique(units_process)
gpu_cleaned_df$Process <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Process))

## Save the cleaned data frame to CSV
write.csv(gpu_cleaned_df, cleaned_file_path, row.names = FALSE)

# Define the numeric columns for boxplot visualization
selected_columns <- c(
  "Core_Speed", "Memory", "Pixel_Rate", 
  "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", "Process"
)

## Set up the layout to plot multiple boxplots in one window
par(mfrow = c(2, 4)) # Arrange in 2 rows, 4 columns (adjust as needed)

## Function to find and print outliers from boxplot
find_boxplot_outliers <- function(data, column_name) {
  box_stats <- boxplot.stats(data[[column_name]])$out
  if (length(box_stats) > 0) {
    cat("Outliers in", column_name, ":", box_stats, "\n")
  } else {
    cat("No outliers in", column_name, "\n")
  }
  return(box_stats)
}

## Plot boxplots and identify outliers for each column
for (col in selected_columns) {
### Plot boxplot
  boxplot(gpu_cleaned_df[[col]], 
          main = paste("Boxplot of", col), 
          ylab = col, 
          col = "skyblue", 
          border = "black")
  
### Print outliers
  find_boxplot_outliers(gpu_cleaned_df, col)
}

## Reset to default layout
par(mfrow = c(1, 1))

## Check rows with Core_Speed from 100–400 MHz
low_core_speed <- gpu_cleaned_df[gpu_cleaned_df$Core_Speed %in% c(100, 200, 300, 350, 400), ]

## Print the number of rows and detailed information
cat("Number of rows with Core_Speed from 100–400 MHz:", nrow(low_core_speed), "\n")
print("Details of rows with low Core_Speed:")
print(low_core_speed)

## Check the manufacturer of these GPUs
cat("Distribution by Manufacturer:\n")
table(low_core_speed$Manufacturer)

## Summarize other columns for evaluation
cat("Summary of other columns for GPUs with low Core_Speed:\n")
summary(low_core_speed)

## Check rows with Memory_Bus = 3072, 4096, 8192 bit
suspicious_bus <- gpu_cleaned_df[gpu_cleaned_df$Memory_Bus %in% c(3072, 4096, 8192), ]

## Print the number of rows and detailed information
cat("Number of rows with Memory_Bus = 3072, 4096, 8192 bit:", nrow(suspicious_bus), "\n")
print("Details of rows with unusual Memory_Bus:")
print(suspicious_bus)

## Check the manufacturer
cat("Distribution by Manufacturer:\n")
table(suspicious_bus$Manufacturer)

## Check Memory_Bandwidth for these rows
cat("Summary of Memory_Bandwidth for GPUs with unusual Memory_Bus:\n")
summary(suspicious_bus$Memory_Bandwidth)