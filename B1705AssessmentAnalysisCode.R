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

 


# ----- 1. Outliers and Treatment -----
##### 1.1. Boxplots: Detecting Outliers -----

boxplot(player_data$Aces, main = "Box Plot - Aces Outlier Detection")
boxplot(player_data$FirstServeInPercent, main = "Box Plot - First Sv In Outlier Detection")
boxplot(player_data$FirstServeWonPercent, main = "Box Plot - First Sv Won Outlier Detection")
boxplot(player_data$DF, main = "Box Plot - DF Outlier Detection")
boxplot(player_data$SecondServeInPercent, main = "Box Plot - Second Sv In Outlier Detection")
boxplot(player_data$SecondServeWonPercent, main = "Box Plot - Second Sv Won Outlier Detection")

##### 1.2. Scatterplot: Detecting Outliers -----

# Function to generate scatterplots for specified DVs against an IV
generate_scatterplots <- function(player_data, Height) {
  # List of dependent variables to create scatterplots for
  dv_list <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")
  
  # Loop through each DV and generate a scatterplot
  for (dv in dv_list) {
    print(ggplot(player_data, aes_string(x = Height, y = dv)) +
            geom_point() + # Scatterplot
            geom_smooth(method = "lm", color = "blue", se = FALSE) + # Linear regression line
            labs(title = paste("Scatterplot of", dv, "vs", Height), x = Height, y = dv) +
            theme_minimal())
  }
}

generate_scatterplots(player_data, "Height")


##### 1.3. Z-Scores: Detecting Outliers #####
library(dplyr)
# Function to calculate and print z-scores for the specified variables
calculate_z_scores <- function(data) {
  variables <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")
  
  # Calculating z-scores for the specified variables
  z_scores <- data %>%
    dplyr::select(dplyr::all_of(variables)) %>%
    scale()
  
  # Adding the z-score columns back to the original data with a "_z" suffix
  for (var in variables) {
    data[[paste0(var, "_z")]] <- z_scores[, var]
  }
  
  return(data)
}

player_data_with_z <- calculate_z_scores(player_data)
head(player_data_with_z)

# Assuming we have a list of z-score column names
z_score_columns <- c("Aces_z", "DF_z", "FirstServeInPercent_z", "FirstServeWonPercent_z", "SecondServeInPercent_z", "SecondServeWonPercent_z")

# Define threshold for outliers
threshold <- 2

# Loop through z-score columns and create separate dataframes for outliers in each
for(z_col in z_score_columns) {
  # Identify rows where the absolute value of the z-score exceeds the threshold
  outlier_indices <- which(abs(player_data_with_z[[z_col]]) > threshold)
  
  # If there are any outliers, create a new dataframe for them
  if(length(outlier_indices) > 0) {
    # Create a dynamic name for the dataframe based on the z-score column name
    df_name <- paste0("outliers_", gsub("_z", "", z_col))
    # Use assign to create the dataframe in the global environment
    assign(df_name, player_data_with_z[outlier_indices, ])
  }
}

# To verify, you can list the created dataframes for outliers like so:
ls(pattern = "outliers_")

# Inspect individual tables to determine if values are outliers or likely to be accurate 
# No outliers removed as all identified and matched as accurate with base dataset, and they represent 
# accurate, real-world performance metrics produced by players and not recording errors






# ----- 2. Testing Assumptions -----
##### 2.1. Normality Testing -----
# Histogram Normality Distributions
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

# Distribution of data in First Serve In %, First Serve Won % and 2nd Serve Won % are considered
# normal. Aces, DF and 2nd Serve In % are considered of a non-normal distribution 


##### 2.2. Homoscedasticity Testing -----
library(lmtest) # for bptest
library(dplyr) # for dynamic data manipulation

# Function to run Breusch-Pagan tests
run_breusch_pagan_tests <- function(data, dependent_vars, independent_var) {
  results <- list()
  
  for (dv in dependent_vars) {
    # Construct the formula
    formula <- as.formula(paste(dv, "~", independent_var))
    
    # Fit the linear model
    model <- lm(formula, data = data)
    
    # Perform the Breusch-Pagan test
    test_result <- bptest(model)
    
    # Store the result
    results[[dv]] <- test_result
  }
  
  return(results)
}

# Metrics to test
metrics <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Running the tests
bp_tests_results <- run_breusch_pagan_tests(player_data, metrics, "Height")

# Viewing the results
print(bp_tests_results)


##### 2.3. Linearity Testing -----
##### Residual Plots: Code Function -----
make_residual_plots <- function(data, variables) {
  for (var in variables) {
    # Fit a linear model for the variable against Height
    model <- lm(reformulate("Height", response = var), data = data)
    
    # Plot the residuals
    plot(model$fitted.values, resid(model),
         xlab = "Fitted Values", ylab = "Residuals",
         main = paste("Residuals vs Fitted for", var))
    abline(h = 0, col = "red")
  }
}

# Specify the variables for residual plots
variables_to_plot <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Call the function with player_data dataset and the specified variables
make_residual_plots(player_data, variables_to_plot)


##### Scatterplots: Code Function -----
# Function to generate scatterplots for specified variables against Height
generate_scatterplots <- function(data, variables) {
  for (var in variables) {
    # Using ggplot2 to generate scatterplot for each variable against Height
    p <- ggplot(data, aes(x = Height, y = .data[[var]])) +
      geom_point(aes(color = Height), alpha = 0.6) +  # Adds scatter plot points
      geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds linear regression line
      labs(title = paste(var, "by Height"), x = "Height (cm)", y = var) +
      theme_minimal()
    
    # Print the plot
    print(p)
  }
}

# Specifying the variables of interest
variables_to_plot <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF",  "SecondServeInPercent", "SecondServeWonPercent")

# Execute the function with the player_data dataset and the specified variables
generate_scatterplots(player_data, variables_to_plot)




##### 2.4. Independence of Observations -----

# Violated as values are multiple by each player from same tournaments; form and rank can mean one set of results is influenced by the previous

##### 2.5. Data Transformation -----

# Transforming each metric into log, sqrt and squared for transformed variable selection to re-test assumptions
variables <- c("Aces", "DF", "FirstServeInPercent", "FirstServeWonPercent", "SecondServeInPercent", "SecondServeWonPercent")

# Adding a small constant to handle zeros for log transformation
player_data_adj <- player_data
player_data_adj[variables] <- lapply(player_data_adj[variables], function(x) ifelse(x == 0, x + 1, x))

# Transformations
log_transformed <- lapply(player_data_adj[variables], log)
sqrt_transformed <- lapply(player_data_adj[variables], sqrt)
squared_transformed <- lapply(player_data_adj[variables], function(x) x^2)

# Examples of how to include them back in the data frame (shown for log transformation)
# Adjusting from your described approach, make sure you explicitly set the new names
player_data_logtrans <- cbind(player_data_adj, setNames(log_transformed, paste0(names(log_transformed), "_log")))
player_data_sqrttrans <- cbind(player_data_adj, setNames(sqrt_transformed, paste0(names(sqrt_transformed), "_sqrt")))
player_data_squaredtrans <- cbind(player_data_adj, setNames(squared_transformed, paste0(names(squared_transformed), "_squared")))


# Metrics of interest
metrics <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Datasets with transformed metrics
datasets <- list(
  logtrans = player_data_logtrans,
  sqrttrans = player_data_sqrttrans,
  squaredtrans = player_data_squaredtrans
)

# Transformation suffixes
suffixes <- c("_log", "_sqrt", "_squared")

# Loop through each dataset and metric to generate QQ plots and histograms
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




# Log transformed breusch pagen tests
library(lmtest)

# Function for the log-transformed dataset
run_breusch_pagan_log <- function(data) {
  results_log <- list()
  metrics <- c("Aces_log", "FirstServeInPercent_log", "FirstServeWonPercent_log", 
               "DF_log", "SecondServeInPercent_log", "SecondServeWonPercent_log")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + OppRank + Age + Surface"))
    model <- lm(formula, data = data)
    results_log[[metric]] <- bptest(model)
  }
  
  return(results_log)
}

# Running the tests on the log-transformed dataset
results_log <- run_breusch_pagan_log(player_data_logtrans)
print(results_log)




# Sqrt transformed breusch pagan tests
# Function for the square root-transformed dataset
run_breusch_pagan_sqrt <- function(data) {
  results_sqrt <- list()
  metrics <- c("Aces_sqrt", "FirstServeInPercent_sqrt", "FirstServeWonPercent_sqrt", 
               "DF_sqrt", "SecondServeInPercent_sqrt", "SecondServeWonPercent_sqrt")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + OppRank + Age + Surface"))
    model <- lm(formula, data = data)
    results_sqrt[[metric]] <- bptest(model)
  }
  
  return(results_sqrt)
}

# Running the tests on the square root-transformed dataset
results_sqrt <- run_breusch_pagan_sqrt(player_data_sqrttrans)
print(results_sqrt)



# Square transformed breusch pagen tests
# Function for the squared-transformed dataset
run_breusch_pagan_squared <- function(data) {
  results_squared <- list()
  metrics <- c("Aces_squared", "FirstServeInPercent_squared", "FirstServeWonPercent_squared", 
               "DF_squared", "SecondServeInPercent_squared", "SecondServeWonPercent_squared")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + OppRank + Age + Surface"))
    model <- lm(formula, data = data)
    results_squared[[metric]] <- bptest(model)
  }
  
  return(results_squared)
}

# Running the tests on the squared-transformed dataset
results_squared <- run_breusch_pagan_squared(player_data_squaredtrans)
print(results_squared)



# Yeo-Johnson Transformation Code
# Load the required library
library(bestNormalize)

# Variables to transform with Yeo-Johnson
variables_to_transform <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Initialize the new dataset with existing data
player_data_transformed <- player_data

# Apply Yeo-Johnson transformations and add to the new dataset
for (var in variables_to_transform) {
  transformation_result <- bestNormalize(player_data[[var]], silent = TRUE)
  player_data_transformed[[paste0(var, "_YJ")]] <- predict(transformation_result, player_data[[var]])
}

# View the structure of the new dataset to confirm new variables have been added
str(player_data_transformed)


run_breusch_pagan_YJ <- function(data) {
  results_YJ <- list()
  metrics <- c("Aces_YJ", "FirstServeInPercent_YJ", "FirstServeWonPercent_YJ", 
               "DF_YJ", "SecondServeInPercent_YJ", "SecondServeWonPercent_YJ")
  
  for (metric in metrics) {
    formula <- as.formula(paste0(metric, " ~ Height + OppRank + Age + Surface"))
    model <- lm(formula, data = data)
    results_squared[[metric]] <- bptest(model)
  }
  
  return(results_YJ)
}


# Code to plot histograms and QQ plots of YJ transformed data
# List of transformed variables with Yeo-Johnson method to check for normality
variables_to_transform_YJ <- c("Aces_YJ", "FirstServeInPercent_YJ", "FirstServeWonPercent_YJ", "DF_YJ", "SecondServeInPercent_YJ", "SecondServeWonPercent_YJ")

# Function to generate QQ plots and Histograms for the transformed variables
plot_normality_check <- function(data, variables) {
  par(mfrow = c(2, 2)) # Adjusts the plot area to a 2x2 layout
  for (var in variables) {
    # QQ plot for Yeo-Johnson transformed variables
    qqnorm(data[[var]], main = paste("QQ Plot -", var))
    qqline(data[[var]], col = "red")
    
    # Histogram for Yeo-Johnson transformed variables
    hist(data[[var]], main = paste("Histogram -", var), xlab = var, breaks = 30, col = 'skyblue', border = 'white')
  }
}

# Running the function
plot_normality_check(player_data_transformed, variables_to_transform_YJ)














