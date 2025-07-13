raw_file_path <- "./All_CPUs.csv"
formatted_file_path <- "./All_CPUs_formatted.csv"
cleaned_file_path <- "./All_CPUs_cleaned.csv"

gpus_df <- read.csv(file_path, stringsAsFactors = FALSE)

gpus_df[] <- lapply(gpus_df, function(column) {
  gsub("[\\r\\n]+", "", column)
})
gpus_df[] <- lapply(gpus_df, function(column) {
  ifelse(grepl("^\\s*$", column), "NA", column)
})
gpus_df[] <- lapply(gpus_df, function(column) {
  ifelse(grepl("^\\s*-\\s*$|^\\s*$", column), "NA", column)
})

write.csv(gpus_df, formatted_file_path, row.names = FALSE)

# From here, only formated data is used

# Read data
gpu_formatted_df <- read.csv(formatted_file_path, stringsAsFactors = FALSE)

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
  Missing_Count = as.vector(missing_values)
)
missing_df[missing_df$Missing_Count >= 0, ]

## Select relevant attributes for GPU analysis (PERFORMACNCE / PRICE)
### Define selected column names
selected_columns <- c("Boost_Clock", "Core_Speed", "Manufacturer",
                      "Memory", "Memory_Bus", "Memory_Speed", 
                      "Pixel_Rate", "Process")

### Extract selected attributes as a data frame
selected_att_df <- gpu_formatted_df[, selected_columns]

### Remove rows with missing values in those columns
all_gpus_cleaned <- gpu_formatted_df[complete.cases(gpu_formatted_df[, selected_columns]), ]

### Filter only selected columns for saving
all_gpus_cleaned_selected <- all_gpus_cleaned[, selected_columns]

### Save cleaned + selected data to CSV
write.csv(all_gpus_cleaned_selected, cleaned_file_path, row.names = FALSE)