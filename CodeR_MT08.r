raw_file_path <- "./All_GPUs.csv"
formatted_file_path <- "./All_GPUs_formatted.csv"
cleaned_file_path <- "./All_GPUs_cleaned.csv"

# Read data
gpus_df <- read.csv(raw_file_path, stringsAsFactors = FALSE)
gpu_formatted_df <- gpus_df

gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  gsub("[\\r\\n]+", "", column)
})
gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  ifelse(grepl("^\\s*$", column), NA, column)
})
gpu_formatted_df[] <- lapply(gpu_formatted_df, function(column) {
  ifelse(grepl("^\\s*-\\s*$|^\\s*$", column), NA, column)
})

write.csv(gpu_formatted_df, formatted_file_path, row.names = FALSE)

# From here, only formated data is used

# Initial Data Exploration
## Show the number of rows and columns
dim(gpu_formatted_df)

## Check if there are any duplicated rows
sum(duplicated(gpu_formatted_df))

## View the structure of the dataset: data types, first values, etc.
str(gpu_formatted_df)

## View summary statistics: min, max, mean, median, etc.
summary(gpu_formatted_df)

# Data cleaning
## Print number of missing values in each column
missing_values <- colSums(is.na(gpu_formatted_df))
missing_df <- data.frame(
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

# From here, only cleaned data is used

## 'Core_Speed' processing
### Convert 'Core_Speed' column from string (e.g., "738 MHz") to numeric
gpu_cleaned_df$Core_Speed <- as.numeric(gsub(" MHz", "", gpu_cleaned_df$Core_Speed))

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
### Convert 'Core_Speed' column from string (e.g., "1024 MB") to numeric
gpu_cleaned_df$Memory <- as.numeric(gsub(" MB", "", gpu_cleaned_df$Memory))

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

# Remove rows with any remaining missing values
gpu_cleaned_df <- gpu_cleaned_df[complete.cases(gpu_cleaned_df), ]

# Save the cleaned data frame to CSV
write.csv(gpu_cleaned_df, cleaned_file_path, row.names = FALSE)