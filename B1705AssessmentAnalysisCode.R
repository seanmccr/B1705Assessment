# ----- B1705 Assessment | Data Analysis | 20.03.2024 -----


# Combine winner and loser data into one
player_data <- bind_rows(
  select(ATP2023Final, Aces = WAce, DF = WDoubleFault, '1stSvInPerc' = W1stSv_Percentage, '1stSvWonPerc' = W1stSvWon_Percentage, '2ndSvInPerc' = W2ndSvIn_Percentage, '2ndSvWonPerc' = W2ndSvWon_Percentage, BPConvPerc = WBPConv_Percentage, Player = Winner, Height = WHeight, Winner = Winner, Loser = Loser),
  select(ATP2023Final, Aces = LAce, DF = LDoubleFault,  '1stSvInPerc' = L1stSv_Percentage, '1stSvWonPerc' = L1stSvWon_Percentage, '2ndSvInPerc' = L2ndSvIn_Percentage, '2ndSvWonPerc' = L2ndSvWon_Percentage, BPConvPerc = LBPConv_Percentage, Player = Loser, Height = LHeight, Winner = Winner, Loser = Loser)
) 

# Assigning a unique numerical identifier to each entry
player_data <- player_data %>%
  mutate(entry_id = row_number())

# Re-ordering variables
player_data <- player_data %>%
  select(entry_id, Player, Height, Aces, DF, '1stSvInPerc', '1stSvWonPerc', '2ndSvInPerc', '2ndSvWonPerc', BPConvPerc, Winner, Loser) 

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

# View the first few rows to confirm changes
head(player_data)



# ----- Regression Analysis: Linear Regression -----
  # Use map from purrr to iterate over each column name in vars_to_plot
library(ggplot2)
library(purrr)
library(rlang) # Make sure to load the rlang package

##### 1.1. Linear Regression Scatterplot + Regression Lines -----
# Columns you want to plot against 'Height'
vars_to_plot <- names(player_data)[4:10]

# Use map from purrr to iterate over each column name in vars_to_plot
map(vars_to_plot, function(x) {
  ggplot(player_data, aes(x = !!sym("Height"), y = !!sym(x))) + 
    geom_point() +
    geom_smooth(method = "lm", color = "blue", fill = "blue", se = FALSE) + 
    ggtitle(paste("Scatter plot of Height vs", x)) +
    xlab("Height") +
    ylab(x) +
    theme_minimal()
})



##### 1.2. Model Fitting -----
##### 1.2.1. Model for Multiple Single Linear Regression Models -----
response_vars <- names(player_data)[4:10] # Adjust as necessary

# Empty list to store model summaries
model_summaries <- list()

# Loop over each response variable to create and summarize the model
model_summaries <- map(response_vars, function(var) {
  # Create a formula string for the model
  formula_str <- paste(var, "~ Height", sep = "")
  
  # Fit the model
  model <- lm(as.formula(formula_str), data = player_data)
  
  # Summarize the model
  summary(model)
})

# Names the list elements according to the response variable
names(model_summaries) <- response_vars



# ---- Normality Testing -----
library(ggplot2)

# Histogram for 'TotalAces'
aceshisto <- ggplot(player_data, aes(x = Aces)) + 
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") + 
    geom_density(col = "red") + 
    labs(title = "Histogram of Aces", x = "Aces", y = "Density")

# QQ Plot for 'TotalAces'
qqnorm(player_data$Aces)
qqline(player_data$Aces, col = "red")


# Anderson-Darling test for 'Aces'
ad_test <- ad.test(player_data$Aces)

print(ad_test)

# Applying a log transformation assuming all values are positive.
player_data_transformed <- log(player_data$Aces + 1) # +1 to avoid log(0)

# Retest for normality using your preferred method, e.g., the Anderson-Darling test
library(nortest)
ad_test_transformed <- ad.test(player_data_transformed)

# Checking the results
print(ad_test_transformed)

qqnorm(player_data_transformed)
qqline(player_data_transformed, col = "red")






# Assuming 'YourVariable' is your left-skewed variable in 'player_data'

# Step 1: Reflecting the data (assuming positive values and choosing a reflection point)
reflected_data <- max(player_data$Aces) + 1 - player_data$Aces

# Step 2: Applying a log transformation to the reflected data
transformed_data <- log(reflected_data)

# Let's visualize the transformed data to check for improvement
hist(transformed_data, main="Histogram of Transformed Data", xlab="Transformed Variable")

