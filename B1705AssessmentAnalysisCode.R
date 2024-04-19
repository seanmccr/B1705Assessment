# ----- B1705 Assessment | Data Analysis | 20.03.2024 -----
# ----- Data Cleaning, Creation, Renaming and Re-ordering -----
# Loading libraries
library(e1071)
library(ggplot2)
library(tidyverse)

# Combine winner and loser data into one
player_data <- bind_rows(
  select(ATP2023Final, Age = WAge, Rank = WRank, OppRank = LRank, Aces = WAce, DF = WDoubleFault, '1stSvInPerc' = W1stSv_Percentage, '1stSvWonPerc' = W1stSvWon_Percentage, '2ndSvInPerc' = W2ndSvIn_Percentage, '2ndSvWonPerc' = W2ndSvWon_Percentage, BPConvPerc = WBPConv_Percentage, Player = Winner, Height = WHeight, Winner = Winner, Loser = Loser, Surface = Surface),
  select(ATP2023Final, Age = LAge, Rank = LRank, OppRank = WRank, Aces =  LAce, DF = LDoubleFault,  '1stSvInPerc' = L1stSv_Percentage, '1stSvWonPerc' = L1stSvWon_Percentage, '2ndSvInPerc' = L2ndSvIn_Percentage, '2ndSvWonPerc' = L2ndSvWon_Percentage, BPConvPerc = LBPConv_Percentage, Player = Loser, Height = LHeight, Winner = Winner, Loser = Loser, Surface = Surface)
) 

# Assigning a unique numerical identifier to each entry
player_data <- player_data %>%
  mutate(entry_id = row_number())

# Re-ordering variables
player_data <- player_data %>%
  select(entry_id, Player, Age, Rank, Surface, Height, OppRank, Aces, DF, '1stSvInPerc', '1stSvWonPerc', '2ndSvInPerc', '2ndSvWonPerc', BPConvPerc, Winner, Loser) 

player_data <- player_data %>%
  mutate(`Win?` = Player == Winner)

player_data <- player_data %>%
  select(-Winner, -Loser)

library(dplyr)

# Renaming variables
player_data <- player_data %>%
  rename(
    FirstServeInPercent = `1stSvInPerc`,
    SecondServeInPercent = `2ndSvInPerc`,
    FirstServeWonPercent = `1stSvWonPerc`,
    SecondServeWonPercent = `2ndSvWonPerc`,
  )


# ----- 1. Assessing for Errors or Outliers -----
##### 1.1 Basic Statistics -----
library(dplyr)
library(psych)

# selecting variables for stats
statistics_summary <- player_data %>%
  select(Aces, FirstServeInPercent, FirstServeWonPercent, DF, SecondServeInPercent, SecondServeWonPercent) %>%    # You can add more columns as needed
  describe()

# Print the summary table
print(statistics_summary)

# Putting stats in DF
summary_dataframe <- as.data.frame(statistics_summary)

# Selecting only relevant columns for simplicity, you can adjust as needed
summary_cleaned <- summary_dataframe %>%
  select(-c(mad, vars, trimmed)) %>%
  rename(Mean = mean,
         Std.Dev = sd,
         Std.Error = se,
         Median = median,
         Min = min,
         Max = max,
         Range = range,
         Kurtosis = kurtosis,
         Skew = skew, 
         )

# Saving DF to put into paper
library(openxlsx)
write.xlsx(summary_cleaned, file = "/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1705/Assessment 1/R Files/Saved Table/StatisticalSummary.xlsx", rowNames = FALSE)

##### 1.2. Boxplots: Assessing Outliers -----
boxplot(player_data$Aces, main = "Box Plot - Aces Outlier Detection")
boxplot(player_data$FirstServeInPercent, main = "Box Plot - First Sv In Outlier Detection")
boxplot(player_data$FirstServeWonPercent, main = "Box Plot - First Sv Won Outlier Detection")
boxplot(player_data$DF, main = "Box Plot - DF Outlier Detection")
boxplot(player_data$SecondServeInPercent, main = "Box Plot - Second Sv In Outlier Detection")
boxplot(player_data$SecondServeWonPercent, main = "Box Plot - Second Sv Won Outlier Detection")

##### 1.3. Scatterplot: Assessing Outliers -----

library(ggplot2)
# Create theme that tries to meet IEEE formatting 
theme_ieee <- function() {
  theme_minimal(base_size = 12) +  
    theme(
      text = element_text(family = "Times New Roman"),  
      plot.title = element_blank(),  
      axis.title = element_text(size = 12),  
      axis.text = element_text(size = 11),  
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank()
    )
}

# Scatterplot function 
generate_ieee_scatterplots <- function(player_data, Height) {
  dv_list <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", 
               "SecondServeInPercent", "SecondServeWonPercent") 
  
  for (dv in dv_list) {
    p <- ggplot(player_data, aes_string(x = Height, y = dv)) +
      geom_point(color = "black", alpha = 0.5) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = paste("Scatterplot of", dv, "vs", "Height (cm)"), x = "Height (cm)", y = dv) +
      theme_ieee()  # Custom theme
    # Save plots to file
    ggsave(filename = paste0("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1705/Assessment 1/R Files/Plot Visual Saves/scatterplot_", dv, "_vs_", Height, ".png"), plot = p, width = 8, height = 6, dpi = 300)
    
    print(p)
  }
}

generate_ieee_scatterplots(player_data, "Height")

##### 1.4. Z-Scores: Assessing Outliers #####
library(dplyr)
# Calculate & print z-scores
calculate_z_scores <- function(data) {
  variables <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")
  
  # Calculating z-scores
  z_scores <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    scale()
  
  # Adding the z-score columns back in
  for (var in variables) {
    data[[paste0(var, "_z")]] <- z_scores[, var]
  }
  
  return(data)
}

player_data_with_z <- calculate_z_scores(player_data)
head(player_data_with_z)

# Z-score columns selected
z_score_columns <- c("Aces_z", "DF_z", "FirstServeInPercent_z", "FirstServeWonPercent_z", "SecondServeInPercent_z", "SecondServeWonPercent_z")

# Define threshold for outliers
threshold <- 2

# Loop through z-score columns, create seperate DF
for(z_col in z_score_columns) {
  
  outlier_indices <- which(abs(player_data_with_z[[z_col]]) > threshold)
  
  # If outliers, create DFs for them
  if(length(outlier_indices) > 0) {
    df_name <- paste0("outliers_", gsub("_z", "", z_col))
    assign(df_name, player_data_with_z[outlier_indices, ])
  }
}

# Inspect individual tables to determine if values are outliers or likely to be accurate 
# Possible outliers were matched as accurate within base dataset, and represent 
# accurate, real-world performance metrics produced by players and not recording errors

# ----- 2. Testing Assumptions -----
##### 2.1. Normality Testing -----

# Histogram Plots
hist(player_data$Aces, main = "Histogram of Data Showing Normal Distribution", xlab = "Aces Values", border = "blue", col = "green")
hist(player_data$FirstServeInPercent, main = "Histogram of Data Showing Normal Distribution", xlab = "First Serve In Values", border = "blue", col = "green")
hist(player_data$FirstServeWonPercent, main = "Histogram of Data Showing Normal Distribution", xlab = "First Serve Won Values", border = "blue", col = "green")
hist(player_data$DF, main = "Histogram of Data Showing Normal Distribution", xlab = "DF Values", border = "blue", col = "green")
hist(player_data$SecondServeInPercent, main = "Histogram of Data Showing Normal Distribution", xlab = "Second Serve In Values", border = "blue", col = "green")
hist(player_data$SecondServeWonPercent, main = "Histogram of Data Showing Normal Distribution", xlab = "Second Serve Won Values", border = "blue", col = "green")

# Q-Q Plot Normality Distributions
qqnorm(player_data$Aces)
qqline(player_data$Aces, col = "red")
qqnorm(player_data$FirstServeInPercent)
qqline(player_data$FirstServeInPercent, col = "red")
qqnorm(player_data$FirstServeWonPercent)
qqline(player_data$FirstServeWonPercent, col = "red")
qqnorm(player_data$DF)
qqline(player_data$DF, col = "red")
qqnorm(player_data$SecondServeInPercent)
qqline(player_data$SecondServeInPercent, col = "red")
qqnorm(player_data$SecondServeWonPercent)
qqline(player_data$SecondServeWonPercent, col = "red")

# Anderson-Darling testing, 
library(nortest)

variables_to_test <- c("Aces", "DF", "FirstServeInPercent", "FirstServeWonPercent", "SecondServeInPercent", "SecondServeWonPercent")

for (variable in variables_to_test) {
  ad_test_result <- ad.test(player_data[[variable]])
  print(paste("Anderson-Darling Test Result for", variable))
  print(ad_test_result)
}

##### 2.2. Homoscedasticity Testing -----
library(lmtest) 
library(dplyr) 

# Function to run Breusch-Pagan tests
run_breusch_pagan_tests <- function(data, dependent_vars, independent_var) {
  results <- list()
  
  for (dv in dependent_vars) {
   
    formula <- as.formula(paste(dv, "~", independent_var))
    model <- lm(formula, data = data)
    test_result <- bptest(model)
    results[[dv]] <- test_result
  }
  
  return(results)
}

# Metrics to test
metrics <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# View & run test
bp_tests_results <- run_breusch_pagan_tests(player_data, metrics, "Height")
print(bp_tests_results)

##### 2.3. Linearity Testing -----
##### Residual Plots: Code Function -----

make_residual_plots <- function(data, variables) {
  for (var in variables) {
    
    model <- lm(reformulate("Height", response = var), data = data)
    
    plot(model$fitted.values, resid(model),
         xlab = "Fitted Values", ylab = "Residuals",
         main = paste("Residuals vs Fitted for", var))
    abline(h = 0, col = "red")
  }
}

# Specify  variables for residual plots
variables_to_plot <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Call function & variables
make_residual_plots(player_data, variables_to_plot)


##### Scatterplots: Code Function -----
# Scatterplot function
generate_scatterplots <- function(data, variables) {
  for (var in variables) {
    
    p <- ggplot(data, aes(x = Height, y = .data[[var]])) +
      geom_point(aes(color = Height), alpha = 0.6) +  
      geom_smooth(method = "lm", color = "blue", se = FALSE) +  
      labs(title = paste(var, "by Height"), x = "Height (cm)", y = var) +
      theme_minimal()
    
    print(p)
  }
}

# Specifying variables 
variables_to_plot <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF",  "SecondServeInPercent", "SecondServeWonPercent")

# Execute function with variables
generate_scatterplots(player_data, variables_to_plot)


##### 2.4. Independence of Observations -----

# Violated; each data entry has an equivalent data entry from match which may influence 
# and is influenced, and data entries are at risk of being influenced dependent 
# on player form, confidence from match results of their previous match etc. 

##### 2.5. Data Transformation -----

# Variables for transformation 
variables <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Adding small constant to make suitable for log transformation
player_data_adj <- player_data
player_data_adj[variables] <- lapply(player_data_adj[variables], function(x) ifelse(x == 0, x + 1, x))

# Transformations
log_transformed <- lapply(player_data_adj[variables], log)
sqrt_transformed <- lapply(player_data_adj[variables], sqrt)
squared_transformed <- lapply(player_data_adj[variables], function(x) x^2)

# Including transformed values back into 
player_data_logtrans <- cbind(player_data_adj, setNames(log_transformed, paste0(names(log_transformed), "_log")))
player_data_sqrttrans <- cbind(player_data_adj, setNames(sqrt_transformed, paste0(names(sqrt_transformed), "_sqrt")))
player_data_squaredtrans <- cbind(player_data_adj, setNames(squared_transformed, paste0(names(squared_transformed), "_squared")))


# Focus Variables 
metrics <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# List of transformed datasets
datasets <- list(
  logtrans = player_data_logtrans,
  sqrttrans = player_data_sqrttrans,
  squaredtrans = player_data_squaredtrans
)

# Transformation suffixes
suffixes <- c("_log", "_sqrt", "_squared")

# Generate Q-Q and Histogram plots on transformed data to assess assumptions still needed
for (key in names(datasets)) {
  dataset <- datasets[[key]]
  suffix <- suffixes[which(names(datasets) == key)]
  
  for (metric in metrics) {
    transformed_metric <- paste0(metric, suffix)
    
    # QQ plot
    qqnorm(dataset[[transformed_metric]], main = paste("QQ Plot -", transformed_metric))
    qqline(dataset[[transformed_metric]], col = "red")
    
    # Histogram
    hist(dataset[[transformed_metric]], main = paste("Histogram -", transformed_metric), xlab = transformed_metric, breaks = 20, col = 'skyblue', border = 'white')
  }
}

##### 2.6. Heteroscedasticity Testing: Transformed Data -----
library(lmtest)

# Log transformed Breusch-Pagen tests
# Function 
run_breusch_pagan_log <- function(data) {
  results_log <- list()
  metrics <- c("Aces_log", "FirstServeInPercent_log", "FirstServeWonPercent_log", 
               "DF_log", "SecondServeInPercent_log", "SecondServeWonPercent_log")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + Rank + Age + Surface + OppRank"))
    model <- lm(formula, data = data)
    results_log[[metric]] <- bptest(model)
  }
  
  return(results_log)
}

# Running function and printing
results_log <- run_breusch_pagan_log(player_data_logtrans)
print(results_log)

# Square root transformed Breusch-Pagan tests
# Function 
run_breusch_pagan_sqrt <- function(data) {
  results_sqrt <- list()
  metrics <- c("Aces_sqrt", "FirstServeInPercent_sqrt", "FirstServeWonPercent_sqrt", 
               "DF_sqrt", "SecondServeInPercent_sqrt", "SecondServeWonPercent_sqrt")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + Rank + Age + Surface + OppRank"))
    model <- lm(formula, data = data)
    results_sqrt[[metric]] <- bptest(model)
  }
  
  return(results_sqrt)
}

# Running functions and printing
results_sqrt <- run_breusch_pagan_sqrt(player_data_sqrttrans)
print(results_sqrt)

# Square transformed Breusch-Pagen tests
# Function 
run_breusch_pagan_squared <- function(data) {
  results_squared <- list()
  metrics <- c("Aces_squared", "FirstServeInPercent_squared", "FirstServeWonPercent_squared", 
               "DF_squared", "SecondServeInPercent_squared", "SecondServeWonPercent_squared")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + Rank + Age + Surface + OppRank"))
    model <- lm(formula, data = data)
    results_squared[[metric]] <- bptest(model)
  }
  
  return(results_squared)
}

# Running functions and printing
results_squared <- run_breusch_pagan_squared(player_data_squaredtrans)
print(results_squared)

##### 2.6.1. Yeo-Johnson Transformation and Testing -----
# Yeo-Johnson Transformation Code
# Load library
library(bestNormalize)

# Variables 
variables_to_transform <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Initialize the new dataset with existing data
player_data_transformed <- player_data

# Apply Yeo-Johnson transformations, add to new dataset
for (var in variables_to_transform) {
  transformation_result <- bestNormalize(player_data[[var]], silent = TRUE)
  player_data_transformed[[paste0(var, "_YJ")]] <- predict(transformation_result, player_data[[var]])
}

# Check structure for inclusion
str(player_data_transformed)



# Yeo-Johnson transformed Breusch-Pagen tests
# Function 
run_breusch_pagan_YJ <- function(data) {
  results_YJ <- list()
  metrics <- c("Aces_YJ", "FirstServeInPercent_YJ", "FirstServeWonPercent_YJ", 
               "DF_YJ", "SecondServeInPercent_YJ", "SecondServeWonPercent_YJ")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + Rank + Age + Surface + OppRank"))
    model <- lm(formula, data = data)
    results_YJ[[metric]] <- bptest(model)
  }
  
  return(results_YJ)
}

# Running function and printing
results_YJ <- run_breusch_pagan_YJ(player_data_transformed)
print(results_YJ)

##### 2.6.2. Running Q-Q & Histograms for YJ Transformed Data -----
# List of transformed variables 
variables_to_transform_YJ <- c("Aces_YJ", "FirstServeInPercent_YJ", "FirstServeWonPercent_YJ", "DF_YJ", "SecondServeInPercent_YJ", "SecondServeWonPercent_YJ")

# QQ plot and Histograms function for YJ transformations
plot_normality_check <- function(data, variables) {
  par(mfrow = c(2, 2)) 
  for (var in variables) {
    # QQ plot
    qqnorm(data[[var]], main = paste("QQ Plot -", var))
    qqline(data[[var]], col = "red")
    # Histogram
    hist(data[[var]], main = paste("Histogram -", var), xlab = var, breaks = 30, col = 'skyblue', border = 'white')
  }
}

# Running functions
plot_normality_check(player_data_transformed, variables_to_transform_YJ)

# Assumptions were not deemed to be met after transformations; action taken is to run non-parametric tests

# ----- 3. Statistical Analysis -----
# Kendall's Tau 
# Variables
variables <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Function to run tests with Height vs DV
for (var in variables) {
  kendall_test <- cor.test(player_data$Height, player_data[[var]], method = "kendall")
  
  # Print 
  cat("\nKendall's Tau correlation test between Height and", var, ":\n")
  print(kendall_test)
}

# ----- 4. Results -----

# In paper, report results and statistics.

# ----- 5. Libraries Used Citations -----

# Libraries
libraries <- c("ggplot2", "tidyverse", "e1071", "nortest", "lmtest", "bestNormalize", "dplyr", "lubridate", "stringr", "dbplyr", "tidyr")

# Function to print each citation
for (lib in libraries) {
  cat("Citation for", lib, ":\n")
  print(citation(lib))
  cat("\n") 
}

# ----- 6. Tidy Environment ----
all_objects <- ls()
objects_to_keep <- c("PlayersDF", "ATP2023Final", "player_data")
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Leave ATP2023Final, PlayersDF & player_data
rm(list = objects_to_remove)

# Save player_data dataset as .xlsx file
write.xlsx(player_data, "/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1705/Assessment 1/R Files/player_data.xlsx", rowNames = FALSE)





