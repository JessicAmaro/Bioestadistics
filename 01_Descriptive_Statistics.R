
# Descriptive Statistics
# This script performs basic statistical analysis on a numerical dataset:
# frequency calculation, measures of central tendency, dispersion, kurtosis, and skewness,
# as well as generating histograms, polygons, and ogives

# ---- LOAD REQUIRED LIBRARIES ----
install.packages("e1071")  # To calculate skewness
library(e1071)

# install.packages("dplyr") # Commented out since it's usually installed once
library(dplyr)

# install.packages("moments") # Package to calculate kurtosis
library(moments)

library(ggplot2)  # For graphics

# ---- LOAD DATA ----
# Load the data from a text file
file_path <- "D:/Maestría/Materias/bioesta/datos.txt"
data <- scan(file = file_path, what = numeric(), sep = "
")
data

# ---- INITIAL CALCULATIONS ----
# Range, number of observations, number of intervals using Sturges' formula
range_value <- max(data) - min(data)
n <- length(data)
k <- round(1 + 3.32 * log10(n))

# ---- INTERVAL CONSTRUCTION ----
# Determine interval width
standard_deviation <- sd(data)
total_range <- max(data) - min(data)
interval_length <- round(total_range / k, 1)
interval_length <- 4.3  # Manually set

# Interval limits
lower_limits <- seq(min(data), max(data), by = interval_length)
upper_limits <- lower_limits + interval_length
intervals <- paste(lower_limits, upper_limits, sep = "-")
intervals2 <- intervals

# ---- FREQUENCIES ----
hist_result <- hist(data, breaks = c(lower_limits, max(upper_limits)), plot = FALSE)
relative_frequency <- hist_result$counts / sum(hist_result$counts)
table1 <- data.frame(
  Lower_Limit = hist_result$breaks[-length(hist_result$breaks)],
  Upper_Limit = hist_result$breaks[-1],
  Total_Frequency = sum(hist_result$counts),
  Relative_Frequency = relative_frequency,
  Absolute_Frequency = hist_result$counts
)

relative_frequencies <- table1$Relative_Frequency
relative_frequencies_percentage <- relative_frequencies * 100
fa1 <- 0
relative_frequencies2 <- c(fa1, relative_frequencies, fa1)
relative_frequencies_percentage2 <- c(fa1, relative_frequencies_percentage, fa1)

# Cumulative frequencies
cumulative_frequencies <- cumsum(hist_result$counts)
cumulative_frequencies <- c(fa1, cumulative_frequencies)
cumulative_frequencies_percentage <- (cumulative_frequencies / sum(hist_result$counts)) * 100

# Manual absolute frequency
absolute_frequency <- table(cut(data, breaks = c(lower_limits, max(upper_limits))))
absolute_frequency2 <- c(fa1, absolute_frequency, fa1)

# ---- CLASS MARKS AND POLYGONS ----
class_marks <- seq(min(data) + interval_length / 2, max(upper_limits) - interval_length / 2, by = interval_length)
class_marks_polygon <- c(13.598, class_marks, 47.998)
upper_limits2 <- c(15.748, upper_limits)

# ---- OGIVES ----
df <- data.frame(upper_limits2 = upper_limits2, CumulativeFrequency = cumulative_frequencies)
df_percentage <- data.frame(upper_limits2 = upper_limits2, CumulativeFrequency = cumulative_frequencies_percentage)

# Absolute ogive
ggplot(df, aes(x = upper_limits2, y = CumulativeFrequency)) + geom_point() + geom_line() +
  labs(title = "Ogive", x = "Upper Limits", y = "Cumulative Frequency") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = df$upper_limits2)

# Percentage ogive
ggplot(df_percentage, aes(x = upper_limits2, y = CumulativeFrequency)) + geom_point() + geom_line() +
  labs(title = "Ogive", x = "Upper Limits", y = "Cumulative Frequency Percentage") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = df_percentage$upper_limits2)

# ---- HISTOGRAMS ----
# With class marks
barplot(absolute_frequency, names.arg = class_marks, main = "Histogram", xlab = "Class Mark", ylab = "Absolute Frequency", col = "lightblue", border = "white", space = 0)

# Relative
barplot(relative_frequencies, names.arg = class_marks, main = "Histogram", xlab = "Class Mark", ylab = "Relative Frequency", col = "lightblue", border = "white", space = 0)

# Percentage
barplot(relative_frequencies_percentage, names.arg = class_marks, main = "Histogram", xlab = "Class Mark", ylab = "Relative Frequency (%)", col = "lightblue", border = "white", space = 0)

# With intervals
barplot(absolute_frequency, names.arg = intervals2, main = "Histogram", xlab = "Intervals", ylab = "Absolute Frequency", col = "lightblue", border = "white", space = 0)
barplot(relative_frequencies, names.arg = intervals2, main = "Histogram", xlab = "Intervals", ylab = "Relative Frequency", col = "lightblue", border = "white", space = 0)
barplot(relative_frequencies_percentage, names.arg = intervals2, main = "Histogram", xlab = "Intervals", ylab = "Relative Frequency (%)", col = "lightblue", border = "white", space = 0)

# ---- FREQUENCY POLYGONS ----
plot(class_marks_polygon, absolute_frequency2, type = "b", pch = 19, col = "blue", xlab = "Class Marks", ylab = "Absolute Frequency", main = "Frequency Polygon", axes = FALSE)
axis(side = 1, at = class_marks_polygon, labels = round(class_marks_polygon, 2))
axis(side = 2)

plot(class_marks_polygon, relative_frequencies2, type = "b", pch = 19, col = "blue", xlab = "Class Marks", ylab = "Relative Frequency", main = "Frequency Polygon", axes = FALSE)
axis(side = 1, at = class_marks_polygon, labels = round(class_marks_polygon, 2))
axis(side = 2)

plot(class_marks_polygon, relative_frequencies_percentage2, type = "b", pch = 19, col = "blue", xlab = "Class Marks", ylab = "Relative Frequency (%)", main = "Frequency Polygon", axes = FALSE)
axis(side = 1, at = class_marks_polygon, labels = round(class_marks_polygon, 2))
axis(side = 2)

# ---- MEASURES OF CENTRAL TENDENCY AND DISPERSION ----
mean_val <- mean(data)
median_val <- median(data)
mode_val <- as.numeric(names(sort(table(data), decreasing = TRUE)[1]))

standard_deviation <- sd(data)
variance <- var(data)
standard_error <- standard_deviation / sqrt(n)
coefficient_variation <- (standard_deviation / mean_val) * 100

# Quartiles, deciles, percentiles
q1 <- quantile(data, 0.25, type = 6)
q2 <- quantile(data, 0.50)
q3 <- quantile(data, 0.75, type = 6)
d1 <- quantile(data, 0.1)
d9 <- quantile(data, 0.9)
p10 <- quantile(data, 0.10, type = 6)
p90 <- quantile(data, 0.90, type = 6)

# Kurtosis and skewness
kurtosis_result <- kurtosis(data, type = 2)
skewness_val <- skewness(data, type = 2)

# ---- SUMMARY TABLE ----
minimum <- min(data)
maximum <- max(data)

measures <- c("Mean", "Median", "Mode", "Variance", "Standard Deviation", "Coefficient of Variation", "Skewness", "Standard Error", "Range", "Minimum", "Maximum", "Kurtosis", "Quartile 1", "Quartile 3", "Percentile 10", "Percentile 90")
values <- c(mean_val, median_val, mode_val, variance, standard_deviation, coefficient_variation, skewness_val, standard_error, range_value, minimum, maximum, kurtosis_result, q1, q3, p10, p90)
results_table <- data.frame(Measure = measures, Value = values)

View(results_table)
