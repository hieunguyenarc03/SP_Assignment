# %% Paths to data files
raw_file_path <- "./All_GPUs.csv"
formatted_file_path <- "./All_GPUs_formatted.csv"
cleaned_file_path <- "./All_GPUs_cleaned.csv"

# %% 1. Read and format the data
gpus_df <- read.csv(raw_file_path, stringsAsFactors = FALSE)

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

# %% 2. Initial Data Exploration
dim(gpu_formatted_df)
sum(duplicated(gpu_formatted_df))
str(gpu_formatted_df)
summary(gpu_formatted_df)

# %% 3. Data Cleaning: Missing value count
missing_values <- colSums(is.na(gpu_formatted_df))
missing_df <- data.frame(
  Index = seq_along(missing_values),
  Column = names(missing_values),
  Missing_Count = as.vector(missing_values),
  Missing_Percent = round(100 * missing_values / nrow(gpu_formatted_df), 2)
)
missing_df[missing_df$Missing_Count >= 0, ]

# %% 3.1 Select and clean numeric columns
selected_columns <- c(
  "Core_Speed", "Manufacturer", "Memory",
  "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", 
  "Pixel_Rate", "Process"
)
gpu_cleaned_df <- gpu_formatted_df[, selected_columns]

# %% Clean Core_Speed
units_core_speed <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory_Speed)
unique(units_core_speed)
gpu_cleaned_df$Core_Speed <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Core_Speed))
hist(gpu_cleaned_df$Core_Speed, breaks = 30, col = "skyblue", main = "Core_Speed Distribution", xlab = "Core Speed (MHz)")
median_core_speed <- median(gpu_cleaned_df$Core_Speed, na.rm = TRUE)
gpu_cleaned_df$Core_Speed[is.na(gpu_cleaned_df$Core_Speed)] <- median_core_speed

# %% Clean Memory
units_memory <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Memory)
unique(units_memory)
gpu_cleaned_df$Memory <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Memory))
hist(gpu_cleaned_df$Memory, breaks = 30, col = "green", main = "Memory Distribution", xlab = "Memory (MB)")
median_memory <- median(gpu_cleaned_df$Memory, na.rm = TRUE)
gpu_cleaned_df$Memory[is.na(gpu_cleaned_df$Memory)] <- median_memory

# %% Clean Pixel_Rate
units_pixel_rate <- gsub("^[0-9.]+\\s*", "", gpu_cleaned_df$Pixel_Rate)
unique(units_pixel_rate)
gpu_cleaned_df$Pixel_Rate <- as.numeric(gsub("[^0-9.]", "", gpu_cleaned_df$Pixel_Rate))
hist(gpu_cleaned_df$Pixel_Rate, breaks = 30, col = "skyblue", main = "Pixel_Rate Distribution", xlab = "Pixel Rate (GPixel/s)")
median_pixel_rate <- median(gpu_cleaned_df$Pixel_Rate, na.rm = TRUE)
gpu_cleaned_df$Pixel_Rate[is.na(gpu_cleaned_df$Pixel_Rate)] <- median_pixel_rate

# %% Clean remaining columns and remove rows with missing values
gpu_cleaned_df <- gpu_cleaned_df[complete.cases(gpu_cleaned_df), ]
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

# %% Save cleaned data
write.csv(gpu_cleaned_df, cleaned_file_path, row.names = FALSE)

# %% Boxplot + outlier detection
selected_columns <- c(
  "Core_Speed", "Memory", "Pixel_Rate", 
  "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", "Process"
)

par(mfrow = c(2, 4))

find_boxplot_outliers <- function(data, column_name) {
  col_data <- data[[column_name]]
  outliers <- boxplot.stats(col_data)$out  
  if (length(outliers) > 0) {
    outlier_counts <- table(outliers)  
    cat("Outliers in", column_name, ":\n")
    print(outlier_counts)
  } else {
    cat("No outliers in", column_name, "\n")
  }
}

for (col in selected_columns) {
  boxplot(gpu_cleaned_df[[col]], 
          main = paste("Boxplot of", col), 
          ylab = col, 
          col = "skyblue", 
          border = "black")
  find_boxplot_outliers(gpu_cleaned_df, col)
}

par(mfrow = c(1, 1))

# %% Remove outlier with Memory_Bus == 3072
library(dplyr)
gpu_cleaned_df <- gpu_cleaned_df %>% filter(Memory_Bus != 3072)

# %% 4. Plotting visualizations
hist(gpu_cleaned_df$Core_Speed,
     main = "Histogram of Core_Speed",
     xlab = "Core Speed (MHz)",
     col = "lightblue",
     border = "black")

boxplot(Core_Speed ~ Manufacturer,
        data = gpu_cleaned_df,
        main = "Core_Speed by Manufacturer",
        ylab = "Manufacturer",
        xlab = "Core Speed (MHz)",
        col = "lightgreen",
        border = "black",
        horizontal = TRUE)

# %% Scatter plots
variables <- c("Memory", "Pixel_Rate", "Memory_Bandwidth", "Memory_Bus", "Memory_Speed", "Process")
par(mfrow = c(3, 2))

for (i in seq_along(variables)) {
  var <- variables[i]
  formula <- as.formula(paste("Core_Speed ~", var))
  plot(formula, data = gpu_cleaned_df, type = "p", col = i, pch = 16,
       main = paste("Correlation between Core Speed and", gsub("_", " ", var)),
       cex.main = 1)
}
# %% Test for normality
shapiro.test(gpu_cleaned_df$Core_Speed)

# %% Calculate 99% confidence interval for Core Speed
x <- gpu_cleaned_df$Core_Speed

E <- function(x) {
  qnorm(p = 0.01/2, lower.tail = FALSE) * sd(x) / sqrt(length(x))
}
margin_error <- E(x)
lower_ci <- mean(x) - margin_error
upper_ci <- mean(x) + margin_error
mean_core_speed <- mean(x)

data.frame(
  Lower_CI_99 = lower_ci,
  Mean        = mean_core_speed,
  Upper_CI_99 = upper_ci
)

# %% 5.2.1 - Tách hai mẫu Core_Speed theo Process: 14nm và 28nm
group_14 <- gpu_cleaned_df$Core_Speed[gpu_cleaned_df$Process == 14]
group_28 <- gpu_cleaned_df$Core_Speed[gpu_cleaned_df$Process == 28]

# Kiểm tra số lượng mẫu
length(group_14)
length(group_28)

# %% 5.2.2 - Kiểm định phân phối chuẩn với Shapiro-Wilk
shapiro.test(group_14)
shapiro.test(group_28)

# %% 5.2.3 - Kiểm định trung bình hai mẫu với t.test
test_result <- t.test(group_14, group_28)
print(test_result)

# %% RR khoảng bác bỏ
alpha <- 0.05
Za <- qnorm(p = alpha / 2, lower.tail = FALSE)

RR <- data.frame(
  Lower = -Za,
  Upper = Za
)

print(RR)
# %% 5.2.4 - Tính khoảng tin cậy hai phía
ci_lower <- test_result$conf.int[1]
ci_upper <- test_result$conf.int[2]
mean_diff <- test_result$estimate[1] - test_result$estimate[2]

data.frame(
  Mean_Diff = mean_diff,
  CI_Lower_95 = ci_lower,
  CI_Upper_95 = ci_upper,
  P_Value = test_result$p.value,
  T_Value = test_result$statistic
)



