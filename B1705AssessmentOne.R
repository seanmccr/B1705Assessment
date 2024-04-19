# -----  B1705 | Cleaning | 08.03.2024 -----
# ----- 1. Loading in datasets & Libraries -----
ATP2023Correct <- readxl::read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023CorrectMatches.xlsx")
ATP2023Incorrect <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1703/Assessment 2/ATP2023SeasonIncorrectMatches.csv")

library(dplyr)
library(lubridate)

# ----- 2. Alterations, Cleaning& Pre-processing -----

##### 2.1. Renaming and Removing -----
# Converting date formats
ATP2023Incorrect$tourney_date <- ymd(ATP2023Incorrect$tourney_date)

# Renaming 
ATP2023Incorrect <- ATP2023Incorrect %>%
     rename(
         WRank = winner_rank,
         LRank = loser_rank
      )

ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    LSeed = loser_seed,
    WSeed = winner_seed
  )

# Remove columns
ATP2023Incorrect <- select(ATP2023Incorrect, -c(1))

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
   rename(
    Tournament = tourney_name,
    Surface = surface,
    Date = tourney_date
)

# Remove columns
ATP2023Incorrect <- select(ATP2023Incorrect, -c(3,4,6,7,15))
ATP2023Incorrect <- select(ATP2023Incorrect, -c(5,12))

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    Score = score,
    LAge = loser_age
    )

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    BestOf = best_of,
    Round = round, 
    Minutes = minutes,
    LCountry = loser_ioc,
    WCountry = winner_ioc,
    WAge = winner_age,
    WHeight = winner_ht,
    WHand = winner_hand
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    Winner = winner_name,
    Loser = loser_name,
    LHand = loser_hand,
    LHeight = loser_ht,
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    WRankPts = winner_rank_points,
    LRankPts = loser_rank_points,
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
    WAce = w_ace,
    WBPSave = w_bpSaved,
    WBPFaced = w_bpFaced,
    LAce = l_ace,
    LBPSave = l_bpSaved,
    LBPFaced = l_bpFaced
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
  rename(
     WDoubleFault = w_df,
     LDoubleFault = l_df,
     WTotalSVPts = w_svpt,
     WSVGames = w_SvGms,
     LTotalSVPts = l_svpt,
     LSVGames = l_SvGms
)

# Rename
ATP2023Incorrect <- ATP2023Incorrect %>%
   rename(
     W1stIn = w_1stIn,
     W1stWon = w_1stWon,
     W2ndWon = w_2ndWon,
     L1stIn = l_1stIn,
     L1stWon = l_1stWon,
     L2ndWon = l_2ndWon
)

ATP2023Incorrect$Date <- ymd(ATP2023Incorrect$Date)

# New Dataset as renamed
MASTERATP2023Incorrect <- ATP2023Incorrect

##### 2.2. Splitting Scores
# Separating score into separate variables for better applicability
# Split 'Score' to separate sets
score_splits <- strsplit(ATP2023Incorrect$Score, " ")
View(score_splits)
score_splits <- strsplit(ATP2023Incorrect$Score, " ")
 
# Columns for variables
num_matches <- length(ATP2023Incorrect$Score)
new_columns <- matrix(NA, nrow = num_matches, ncol = 10)
colnames(new_columns) <- c('W1', 'L1', 'W2', 'L2', 'W3', 'L3', 'W4', 'L4', 'W5', 'L5')
 
# 2. Separating set scores
for (i in 1:num_matches) {
   sets <- score_splits[[i]]
   for (j in 1:length(sets)) {
     set_score <- sets[j]
        # Check for any retirements
         if(set_score == "RET") {
             # Put NA in when retirements happen
           new_columns[i, (2*j-1):(2*j)] <- NA
                 next
               }
         # Split set score by '-', sperate winner from losers scores
           games <- unlist(strsplit(set_score, "-"))
           # Tiebreaks 
             games <- gsub("\\((.*?)\\)", "", games)
             # Scores into new variables
               new_columns[i, (2*j-1):(2*j)] <- games[1:2]
             }
 }
 

# Renaming & binding
new_df <- as.data.frame(new_columns)
DRAFTATP2023Incorrect <- cbind(ATP2023Incorrect, new_df)

# Rename
ATP2023Final <- DRAFTATP2023Incorrect
# Remove variables
ATP2023Final <- select(ATP2023Final, -c(16))

# NA Count for each variable
na_count_ATP2023Final <- ATP2023Final %>% 
  summarise_all(~sum(is.na(.)))
print(na_count_ATP2023Final)

# Rename 
ATP2023FinalDraft <- ATP2023Final
# Remove Davis Cup entries
ATP2023Final <- ATP2023Final %>%
  filter(!grepl("Davis Cup", Tournament))

# Dataset's featuring NA's to assess reason
ATP2023Final_NA_WAce <- filter(ATP2023FinalDraft, is.na(WAce))
ATP2023Final_NA_WRank <- filter(ATP2023Final, is.na(WRank))
ATP2023Final_NA_LRank <- filter(ATP2023Final, is.na(LRank))
ATP2023Final_NA_LAge <- filter(ATP2023Final, is.na(LAge))
ATP2023Final_NA_Minutes <- filter(ATP2023Final, is.na(Minutes))
ATP2023Final_NA_WHeight <- filter(ATP2023Final, is.na(WHeight))
ATP2023Final_NA_LHeight <- filter(ATP2023Final, is.na(LHeight))

# Updating age and Height
ATP2023Final <- ATP2023Final %>%
  mutate(LAge = ifelse(Loser == "Liam Krall", 21.7, LAge),
         LHeight = ifelse(Loser == "Liam Krall", 191, LHeight))
# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LAge = ifelse(Loser == "Manas Dhamne", 15.1, LAge),
         LHeight = ifelse(Loser == "Manas Dhamne", 188, LHeight))

# Inputting match length via ATP website
ATP2023Final <- ATP2023FinalDraft %>%
  mutate(Minutes = ifelse(row_number() == 38, 73 , Minutes),
         Minutes = ifelse(row_number() == 35, 91, Minutes),
         Minutes = ifelse(row_number() == 27, 75, Minutes),
         Minutes = ifelse(row_number() == 24, 143, Minutes),
         Minutes = ifelse(row_number() == 15, 130, Minutes),
         Minutes = ifelse(row_number() == 45, 91, Minutes))

# Input heights via ATP website
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Ben Shelton", 193, LHeight),
         LHeight = ifelse(Loser == "Gijs Brouwer", 191, LHeight),
         LHeight = ifelse(Loser == "Alexander Shevchenko", 185, LHeight),
         LHeight = ifelse(Loser == "Luca Van Assche", 178, LHeight),
         LHeight = ifelse(Loser == "Rinky Hijikata", 178, LHeight),
         LHeight = ifelse(Loser == "Stefanos Sakellaridis", 196, LHeight),
         LHeight = ifelse(Loser == "Flavio Cobolli", 183, LHeight),
         LHeight = ifelse(Loser == "Dalibor Svrcina", 178, LHeight),
         LHeight = ifelse(Loser == "Juncheng Shang", 180, LHeight),
         LHeight = ifelse(Loser == "Brandon Holt", 185, LHeight),
         LHeight = ifelse(Loser == "Luciano Darderi", 183, LHeight),
         LHeight = ifelse(Loser == "Matija Pecotic", 185, LHeight),
         LHeight = ifelse(Loser == "Camilo Ugo Carabelli", 185, LHeight),
         LHeight = ifelse(Loser == "Alexander Ritschard", 193, LHeight),
         LHeight = ifelse(Loser == "Jacopo Berrettini", 193, LHeight),
         LHeight = ifelse(Loser == "Riccardo Bonadio", 180, LHeight),
         LHeight = ifelse(Loser == "Aleksandar Kovacevic", 183, LHeight),
         LHeight = ifelse(Loser == "Francesco Passaro", 180, LHeight),
         LHeight = ifelse(Loser == "Ivan Gakhov", 191, LHeight),
         LHeight = ifelse(Loser == "Abedallah Shelbayh", 180, LHeight),
         LHeight = ifelse(Loser == "Pablo Llamas Ruiz", 188, LHeight),
         LHeight = ifelse(Loser == "Genaro Alberto Olivieri", 175, LHeight),
         LHeight = ifelse(Loser == "Ryan Peniston", 180, LHeight),
         LHeight = ifelse(Loser == "Jan Choinski", 196, LHeight),
         LHeight = ifelse(Loser == "Filip Misolic", 180, LHeight),
         LHeight = ifelse(Loser == "Alex Michelsen", 193, LHeight),
         LHeight = ifelse(Loser == "Ethan Quinn", 191, LHeight),
         LHeight = ifelse(Loser == "Dino Prizmic", 188, LHeight),
         LHeight = ifelse(Loser == "Sho Shimabukuro", 180, LHeight),
         LHeight = ifelse(Loser == "Gabriel Diallo", 203, LHeight),
         LHeight = ifelse(Loser == "Omni Kumar", 173, LHeight),
         LHeight = ifelse(Loser == "Yu Hsiou Hsu", 178, LHeight),
         LHeight = ifelse(Loser == "Titouan Droguet", 191, LHeight),
         LHeight = ifelse(Loser == "Jakub Mensik", 193, LHeight),
         LHeight = ifelse(Loser == "Philip Sekulic", 191, LHeight),
         LHeight = ifelse(Loser == "Alibek Kachmazov", 185, LHeight),
         LHeight = ifelse(Loser == "Beibit Zhukayev", 196, LHeight),
         LHeight = ifelse(Loser == "Terence Atmane", 193, LHeight),
         LHeight = ifelse(Loser == "Bu Yunchaokete", 185, LHeight),
         LHeight = ifelse(Loser == "Shintaro Mochizuki", 175, LHeight),
         LHeight = ifelse(Loser == "Giovanni Mpetshi Perricard", 203, LHeight),
         LHeight = ifelse(Loser == "Mark Lajal", 191, LHeight),
         LHeight = ifelse(Loser == "Billy Harris", 193, LHeight)
         )
   
# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Kiranpal Pannu", 185, LHeight),
         LHeight = ifelse(Loser == "Mattia Bellucci", 175, LHeight),
         LHeight = ifelse(Loser == "Oleksii Krutykh", 185, LHeight),
         LHeight = ifelse(Loser == "Alex Rybakov", 185, LHeight),
         LHeight = ifelse(Loser == "Clement Chidekh", 180, LHeight),
         LHeight = ifelse(Loser == "Mateus Alves", 193, LHeight),
         LHeight = ifelse(Loser == "Nick Chappell", 178, LHeight),
         LHeight = ifelse(Loser == "Rodrigo Pacheco Mendez", 188, LHeight),
         LHeight = ifelse(Loser == "Henrique Rocha", 180, LHeight),
         LHeight = ifelse(Loser == "Younes Lalami Laaroussi", 188, LHeight),
         LHeight = ifelse(Loser == "Valentin Vacherot", 193, LHeight),
         LHeight = ifelse(Loser == "Daniel Rincon", 185, LHeight),
         LHeight = ifelse(Loser == "Max Hans Rehberg", 183, LHeight),
         LHeight = ifelse(Loser == "Martin Landaluce", 191, LHeight),
         LHeight = ifelse(Loser == "Alvaro Lopez San Martin", 178, LHeight),
         LHeight = ifelse(Loser == "Eduardo Nava", 180, LHeight),
         LHeight = ifelse(Loser == "George Loffhagen", 188, LHeight),
         LHeight = ifelse(Loser == "Arthur Fery", 175, LHeight),
         LHeight = ifelse(Loser == "Eliot Spizzirri", 183, LHeight),
         LHeight = ifelse(Loser == "Yunseong Chung", 178, LHeight),
         LHeight = ifelse(Loser == "Andres Martin", 183, LHeight),
         LHeight = ifelse(Loser == "Jesper De Jong", 180, LHeight),
         LHeight = ifelse(Loser == "Skander Mansouri", 193, LHeight),
         LHeight = ifelse(Loser == "Alexis Galarneau", 180, LHeight),
         LHeight = ifelse(Loser == "Strong Kirchheimer", 185, LHeight),
         LHeight = ifelse(Loser == "Nicolas Moreno De Alboran", 185, LHeight),
         LHeight = ifelse(Loser == "Learner Tien", 180, LHeight),
         LHeight = ifelse(Loser == "Tao Mu", 180, LHeight),
         LHeight = ifelse(Loser == "Jie Cui", 183, LHeight),
         LHeight = ifelse(Loser == "Ye Cong Mo", 183, LHeight),
         LHeight = ifelse(Loser == "Rigele Te", 188, LHeight),
         LHeight = ifelse(Loser == "Denis Yevseyev", 185, LHeight),
         LHeight = ifelse(Loser == "Karl Friberg", 191, LHeight),
         LHeight = ifelse(Loser == "Alexander Blockx", 191, LHeight),
         LHeight = ifelse(Loser == "Matteo Martineau", 183, LHeight)
  )

# Again
ATP2023Final <- ATP2023Final %>%
  mutate(LHeight = ifelse(Loser == "Kiranpal Pannu", 185, LHeight))

# Replacing NA values in match length with 0 for Walkovers
ATP2023Final <- ATP2023Final %>%
  mutate(Minutes = ifelse(is.na(Minutes), 0, Minutes))

# Load necessary libraries
library(dplyr)
library(lubridate)

##### 2.2. Merging Datasets -----
# Update ATP2023Correct with the tournament start date for each match
ATP2023Correct <- ATP2023Correct %>%
  group_by(Tournament) %>%
  mutate(Date = min(Date)) %>%
  ungroup()

# Summarize the variable types in ATP2023Final
str(ATP2023Final)
str(ATP2023Correct)

# Extract unique Surface values from ATP2023Final dataset
unique_surfaces_final <- unique(ATP2023Final$Surface)

# Print the unique surfaces from ATP2023Final
print(unique_surfaces_final)

# Extract unique Surface values from ATP2023Correct dataset
unique_surfaces_correct <- unique(ATP2023Correct$Surface)

# Print the unique surfaces from ATP2023Correct
print(unique_surfaces_correct)

unique_series <- unique(ATP2023Correct$Series)
print(unique_series)

# Ensure all relevant columns are of the correct type (if not already converted)
ATP2023Correct <- ATP2023Correct %>%
  mutate(across(c(W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank), as.numeric))

# Prepare the data from ATP2023Correct with only the required variables for joining and the 'Series' variable
ATP2023Correct_Series <- ATP2023Correct %>%
  select(Series, W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank)

# Merge 'Series' into ATP2023Final based on the unique combination
# Note: ATP2023Final should also have W1, W2, W3, W4, W5, L1, L2, L3, L4, L5, WRank, LRank correctly formatted as numerical types before this step
ATP2023Final_withSeries <- merge(ATP2023Final, ATP2023Correct_Series, 
                                 by = c("W1", "L1", "W2", "L2", "W3", "L3", "W4", "L4", "W5", "L5", "WRank", "LRank"),
                                 all.x = TRUE)

# Optional: check if merge was performed correctly   
head(ATP2023Final_withSeries)

##### 2.4. Reordering and checking NA -----
# Reordering Dataset
ATP2023Final_reordered <- ATP2023Final_withSeries %>%
  select(Tournament, Series, Surface, Date, Winner, WSeed, WHand, WHeight, WCountry, WAge, Loser, LSeed, LHand, LHeight, 
         LCountry, LAge, BestOf, Round, Minutes, WAce, WDoubleFault, WTotalSVPts, W1stIn, W1stWon, W2ndWon, 
         WSVGames, WBPSave, WBPFaced, LAce, LDoubleFault, LTotalSVPts, L1stIn, L1stWon, L2ndWon, LSVGames, LBPSave, 
         LBPFaced, WRankPts, LRankPts, WRank, LRank, W1, L1, W2, L2, W3, L3, W4, L4, W5, L5) 

# Checking for NA Values
na_count_ATP2023Final_reordered <- ATP2023Final_reordered %>% 
  summarise_all(~sum(is.na(.)))
print(na_count_ATP2023Final_reordered)

# Renaming
ATP2023Finalreordered_NA_Series <- filter(ATP2023Final_reordered, is.na(Series))

# Matching NA values to other tournament names to fill them
ATP2023Final_reordered <- ATP2023Final_reordered %>%
  group_by(Tournament) %>%
  mutate(
    Series = ifelse(is.na(Series),
                    first(Series[!is.na(Series)]), 
                    Series) 
  ) %>%
  ungroup() 


# Putting tournament names in 
ATP2023Final_reordered <- ATP2023Final_reordered %>%
  mutate(Series = case_when(
    Tournament %in% c("Lyon", "Geneva") ~ "ATP250",
    TRUE ~ Series 
  ))

# Remove 'Tournament' entries labelled 'NextGen Finals'
ATP2023Final_reordered <- ATP2023Final_reordered %>%
  filter(!grepl("NextGen Finals", Tournament))

# Rename 
ATP2023PointsDraft <- ATP2023Final_reordered

# Replace NA values in the 'Tournament' column with 'ATP500' for United Cup entries
ATP2023PointsDraft <- ATP2023PointsDraft %>%
  mutate(Series = if_else(Tournament == "United Cup" & is.na(Series), "ATP500", Series))

# Filtering the dataset to include only 'United Cup' entries
ATP2023PointsDraft_UnitedCup <- filter(ATP2023PointsDraft, Tournament == "United Cup")

# Filtering the dataset to exclude 'United Cup' entries
ATP2023PointsDraft_OtherGames <- filter(ATP2023PointsDraft, Tournament != "United Cup")






# ----- 3. Points Function -----
# Define a function for regular tournament point calculation (exclude United Cup)
getTournamentPoints <- function(series, round, winnerFlag = FALSE) {
  
  points <- list(
    'Grand Slam' = c(R128 = 10, R64 = 35, R32 = 45, R16 = 90, QF = 180, SF = 360, F = 480, W = 800),
    'Masters 1000' = c(R128 = 10, R64 = 15, R32 = 20, R16 = 45, QF = 90, SF = 180, F = 240, W = 400),
    'ATP500' = c(R64 = 0, R32 = 20, R16 = 25, QF = 45, SF = 90, F = 120, W = 200),
    'ATP250' = c(R64 = 0, R32 = 10, R16 = 10, QF = 25, SF = 45, F = 60, W = 100),
    'Masters Cup' = c(RR = 200, SF = 400, F = 0, W = 500)
  )
  
  # Adjust points depending on final
  seriesPoints <- points[[series]]
  if (is.null(seriesPoints)) {
    return(NA)  
  }
  
  finalPoints <- ifelse(round == 'F', seriesPoints['F'], 0)
  
  winnerBonus <- ifelse(winnerFlag & round == 'F', seriesPoints['W'], 0) +
    ifelse(winnerFlag & round != 'F', seriesPoints[round], 0)
  
  return(finalPoints + winnerBonus)
}

# Adjust ATP2023PointsDraft DF using function
ATP2023PointsDraft_OtherGames <- ATP2023PointsDraft_OtherGames %>%
  rowwise() %>%
  mutate(
    WPoints = getTournamentPoints(Series, Round, TRUE),
    LPoints = if_else(Round == "F", getTournamentPoints(Series, Round, FALSE), 0)
  ) %>%
  ungroup()


# Points function for United Cup 
calculateUnitedCupPoints <- function(Round, LRank) {
  points <- case_when(
    Round == "RR" & LRank >= 1 & LRank <= 10 ~ 80,
    Round == "RR" & LRank >= 11 & LRank <= 20 ~ 65,
    Round == "RR" & LRank >= 21 & LRank <= 30 ~ 55,
    Round == "RR" & LRank >= 31 & LRank <= 50 ~ 40,
    Round == "RR" & LRank >= 51 & LRank <= 100 ~ 35,
    Round == "RR" & LRank >= 101 & LRank <= 250 ~ 25,
    Round == "RR" & LRank > 250 ~ 20,
    Round == "SF" & LRank >= 1 & LRank <= 10 ~ 130,
    Round == "SF" & LRank >= 11 & LRank <= 20 ~ 105,
    Round == "SF" & LRank >= 21 & LRank <= 30 ~ 90,
    Round == "SF" & LRank >= 31 & LRank <= 50 ~ 60,
    Round == "SF" & LRank >= 51 & LRank <= 100 ~ 40,
    Round == "SF" & LRank >= 101 & LRank <= 250 ~ 35,
    Round == "SF" & LRank > 250 ~ 25,
    Round == "F" & LRank >= 1 & LRank <= 10 ~ 180,
    Round == "F" & LRank >= 11 & LRank <= 20 ~ 140,
    Round == "F" & LRank >= 21 & LRank <= 30 ~ 120,
    Round == "F" & LRank >= 31 & LRank <= 50 ~ 90,
    Round == "F" & LRank >= 51 & LRank <= 100 ~ 60,
    Round == "F" & LRank >= 101 & LRank <= 250 ~ 40,
    Round == "F" & LRank > 250 ~ 35,
    TRUE ~ 0 # Default to 0 points for scenarios not defined above
  )
  
  return(points)
}

# Updating dataset for United Cup points
ATP2023PointsDraft_UnitedCup <- ATP2023PointsDraft_UnitedCup %>%
  mutate(
    WPoints = calculateUnitedCupPoints(Round, LRank),
    LPoints = 0 
  )

# Combining the datasets
ATP2023FinalClean <- bind_rows(ATP2023PointsDraft_OtherGames, ATP2023PointsDraft_UnitedCup)

# Convert the result to a dataframe before printing
ATP2023FinalCleanNAList <- ATP2023FinalClean %>% 
  summarise_all(~sum(is.na(.))) %>%
  as.data.frame()  # This line converts the result to a dataframe

# Now print, and it should show all columns regardless of the number
print(ATP2023FinalCleanNAList)

# Filtering
ATP2023FinalClean_NAWHeight <- filter(ATP2023FinalClean, is.na(WHeight))

# Inputting data
ATP2023FinalClean <- ATP2023FinalClean %>%
  mutate(LHeight = ifelse(Loser == "Dragos Nicolae Madaras", 191, LHeight))

# Inputting data
ATP2023FinalClean <- ATP2023FinalClean %>%
  mutate(WHeight = ifelse(Winner == "Ben Shelton", 193, WHeight),
         WHeight = ifelse(Winner == "Gijs Brouwer", 191, WHeight),
         WHeight = ifelse(Winner == "Alexander Shevchenko", 185, WHeight),
         WHeight = ifelse(Winner == "Luca Van Assche", 178, WHeight),
         WHeight = ifelse(Winner == "Rinky Hijikata", 178, WHeight),
         WHeight = ifelse(Winner == "Stefanos Sakellaridis", 196, WHeight),
         WHeight = ifelse(Winner == "Flavio Cobolli", 183, WHeight),
         WHeight = ifelse(Winner == "Dalibor Svrcina", 178, WHeight),
         WHeight = ifelse(Winner == "Juncheng Shang", 180, WHeight),
         WHeight = ifelse(Winner == "Brandon Holt", 185, WHeight),
         WHeight = ifelse(Winner == "Luciano Darderi", 183, WHeight),
         WHeight = ifelse(Winner == "Matija Pecotic", 185, WHeight),
         WHeight = ifelse(Winner == "Camilo Ugo Carabelli", 185, WHeight),
         WHeight = ifelse(Winner == "Alexander Ritschard", 193, WHeight),
         WHeight = ifelse(Winner == "Jacopo Berrettini", 193, WHeight),
         WHeight = ifelse(Winner == "Riccardo Bonadio", 180, WHeight),
         WHeight = ifelse(Winner == "Aleksandar Kovacevic", 183, WHeight),
         WHeight = ifelse(Winner == "Francesco Passaro", 180, WHeight),
         WHeight = ifelse(Winner == "Ivan Gakhov", 191, WHeight),
         WHeight = ifelse(Winner == "Abedallah Shelbayh", 180, WHeight),
         WHeight = ifelse(Winner == "Pablo Llamas Ruiz", 188, WHeight),
         WHeight = ifelse(Winner == "Genaro Alberto Olivieri", 175, WHeight),
         WHeight = ifelse(Winner == "Ryan Peniston", 180, WHeight),
         WHeight = ifelse(Winner == "Jan Choinski", 196, WHeight),
         WHeight = ifelse(Winner == "Filip Misolic", 180, WHeight),
         WHeight = ifelse(Winner == "Alex Michelsen", 193, WHeight),
         WHeight = ifelse(Winner == "Ethan Quinn", 191, WHeight),
         WHeight = ifelse(Winner == "Dino Prizmic", 188, WHeight),
         WHeight = ifelse(Winner == "Sho Shimabukuro", 180, WHeight),
         WHeight = ifelse(Winner == "Gabriel Diallo", 203, WHeight),
         WHeight = ifelse(Winner == "Omni Kumar", 173, WHeight),
         WHeight = ifelse(Winner== "Yu Hsiou Hsu", 178, WHeight),
         WHeight = ifelse(Winner == "Titouan Droguet", 191, WHeight),
         WHeight = ifelse(Winner == "Jakub Mensik", 193, WHeight),
         WHeight = ifelse(Winner == "Philip Sekulic", 191, WHeight),
         WHeight = ifelse(Winner == "Alibek Kachmazov", 185, WHeight),
         WHeight = ifelse(Winner == "Beibit Zhukayev", 196, WHeight),
         WHeight = ifelse(Winner == "Terence Atmane", 193, WHeight),
         WHeight = ifelse(Winner == "Bu Yunchaokete", 185, WHeight),
         WHeight = ifelse(Winner == "Shintaro Mochizuki", 175, WHeight),
         WHeight = ifelse(Winner == "Giovanni Mpetshi Perricard", 203, WHeight),
         WHeight = ifelse(Winner == "Mark Lajal", 191, WHeight),
         WHeight = ifelse(Winner == "Billy Harris", 193, WHeight)
  )

# Rename
ATP2023FinalClean_B <- ATP2023FinalClean
ATP2023FinalClean_C <- ATP2023FinalClean

# ----- 4. Identifying Byes -----
# Columns with Bye initialised
ATP2023FinalClean_C <- ATP2023FinalClean_C %>%
  mutate(WBye = FALSE, LBye = FALSE)

# Define round orders
round_order <- c("R128", "R64", "R32", "R16", "QF", "SF", "F")

# Function to check for bye
checkForBye <- function(first_round_in_tournament) {
  first_round_index <- match(first_round_in_tournament, round_order)
  if (!is.na(first_round_index) && first_round_index > 1) {
    return(TRUE)
  }
  return(FALSE)
}

# Inputting Byes
ATP2023FinalClean_C <- ATP2023FinalClean_C %>%
  mutate(WBye = FALSE, LBye = FALSE) %>%
  group_by(Tournament, Winner, Loser) %>%
  mutate(WFirstRound = first(Round), LFirstRound = first(Round)) %>%
  ungroup() %>%
  rowwise() %>%  
  mutate(WBye = ifelse(!is.na(WSeed) & checkForBye(WFirstRound), TRUE, WBye),
         LBye = ifelse(!is.na(LSeed) & checkForBye(LFirstRound), TRUE, LBye)) %>%
  ungroup() %>%  #
  select(-WFirstRound, -LFirstRound)


# Code to indicate byes
ATP2023FinalClean <- ATP2023FinalClean_C %>%
  group_by(Tournament, Winner, Loser) %>%
  mutate(EarliestWinnerRound = min(match(Round, c("R128", "R64", "R32", "R16", "QF", "SF", "F"), nomatch = 100)),
         EarliestLoserRound = min(match(Round, c("R128", "R64", "R32", "R16", "QF", "SF", "F"), nomatch = 100))) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(WBye = if_else(Series == "Grand Slam" | Series == "Masters Cup", FALSE,
                        !is.na(WSeed) & EarliestWinnerRound > match("R32", c("R128", "R64", "R32", "R16", "QF", "SF", "F"))),
         LBye = if_else(Series == "Grand Slam" | Series == "Masters Cup", FALSE,
                        !is.na(LSeed) & EarliestLoserRound > match("R32", c("R128", "R64", "R32", "R16", "QF", "SF", "F")))) %>%
  ungroup()

# If bye received, mark all other matches in tournament as Bye received
ATP2023FinalClean <- ATP2023FinalClean %>%
  group_by(Tournament, Winner) %>%
  mutate(WBye = ifelse(any(WBye == TRUE), TRUE, FALSE)) %>%
  ungroup() %>%
  group_by(Tournament, Loser) %>%
  mutate(LBye = ifelse(any(LBye == TRUE), TRUE, FALSE)) %>%
  ungroup()

# Changing data type
library(dplyr)
ATP2023Final <- ATP2023Final %>%
  mutate(across(c(W1, W2, W3, W4, W5, L1, L2, L3, L4, L5), as.numeric))


# ----- 5. Create Player Values ----

# Combine 'Winner' and 'Loser' 
unique_players <- unique(c(ATP2023Final$Winner, ATP2023Final$Loser))

# Create a new dataframe with 'Players' variable
PlayersDF <- data.frame(Players = unique_players)

head(PlayersDF)

library(dplyr)

# Calculate total points winners & losers separately 
winner_points <- ATP2023Final %>%
  group_by(Winner) %>%
  summarize(W_Points = sum(WPoints, na.rm = TRUE)) %>%
  rename(Players = Winner, `2023Pts` = W_Points)

loser_points <- ATP2023Final %>%
  group_by(Loser) %>%
  summarize(L_Points = sum(LPoints, na.rm = TRUE)) %>%
  rename(Players = Loser, `2023Pts` = L_Points)

# Combining points from both Winner and Loser, and aggregating again in case of overlap
all_points <- bind_rows(winner_points, loser_points) %>%
  group_by(Players) %>%
  summarize(`2023Pts` = sum(`2023Pts`, na.rm = TRUE))
 
# Joining with PlayersDF and ordering
PlayersDF <- left_join(PlayersDF, all_points, by = "Players")
  
# Generating the final ranking
PlayersDF <- PlayersDF %>%
  arrange(desc(`2023Pts`)) %>%
  mutate(`2023FinRnk` = row_number())

# Add 'TourFinalStatus' column
PlayersDF <- PlayersDF %>%
  mutate(TourFinalStatus = case_when(
    `2023FinRnk` <= 8 ~ "Qualified",
    `2023FinRnk` %in% 9:10 ~ "Alternates",
    TRUE ~ "Did not Qualify"
  ))

# Create
ATP2023Final <- ATP2023FinalClean


# ----- 6. Creating Values -----
# Aggregate aces for winners and losers 
winner_aces <- ATP2023Final %>%
  group_by(Winner) %>%
  summarize(TotalWAces = sum(WAce, na.rm = TRUE))

loser_aces <- ATP2023Final %>%
  group_by(Loser) %>%
  summarize(TotalLAces = sum(LAce, na.rm = TRUE))

# Combine ace totals for each player
aces_combined <- bind_rows(winner_aces %>% rename(Player=Winner, Aces=TotalWAces), 
                           loser_aces %>% rename(Player=Loser, Aces=TotalLAces)) %>%
  group_by(Player) %>%
  summarize(TotalAces = sum(Aces, na.rm = TRUE))

# Merge total aces in PlayersDF
PlayersDF <- merge(PlayersDF, aces_combined, by.x = "Players", by.y = "Player", all.x = TRUE)

# 0 in for 'NA' 
PlayersDF$TotalAces[is.na(PlayersDF$TotalAces)] <- 0

# Display the updated PlayersDF
head(PlayersDF)

# Load Libraries
library(dplyr)
library(stringr)
library(dbplyr)
library(tidyr)

# Putting match name together for data row
ATP2023Final <- ATP2023Final %>%
  mutate(MatchName = paste(Tournament,",",Round,":", Winner,"vs",Loser))

# Alter value name
ATP2023Final <- ATP2023Final %>%
  mutate(Series = str_replace(Series, "ATP500", "ATP 500"),
         Series = str_replace(Series, "ATP250", "ATP 250"))

# Create 1st Serve In Percentage
ATP2023Final <- ATP2023Final %>%
  mutate(W1stSv_Percentage = W1stIn / WTotalSVPts * 100,
         L1stSv_Percentage = L1stIn / LTotalSVPts * 100)

player_stats <- ATP2023Final %>%
  select(Winner, Loser, W1stSv_Percentage, L1stSv_Percentage) %>%
  pivot_longer(cols = c(Winner, Loser), 
               names_to = "PlayerType", 
               values_to = "Player") %>%
  pivot_longer(cols = c(W1stSv_Percentage, L1stSv_Percentage), 
               names_to = "PercentageType", 
               values_to = "ServicePercentage") %>%
  group_by(Player) %>%
  summarise(Overall_1stSv_Percentage = mean(ServicePercentage, na.rm = TRUE))
# Checking dataframe
head(player_stats)

# Create 1st Serve Won Percentage
ATP2023Final <- ATP2023Final %>%
  mutate(
    W1stSvWon_Percentage = (W1stWon / W1stIn) * 100,
    L1stSvWon_Percentage = (L1stWon / L1stIn) * 100
  )
# Createa BP Conversion Percentage
ATP2023Final <- ATP2023Final %>%
  mutate(
    WBPConv_Percentage = (LBPFaced - LBPSave) / LBPFaced * 100,
    LBPConv_Percentage = (WBPFaced - WBPSave) / WBPFaced * 100
  )
# Total 2nd Serve Points
ATP2023Final <- ATP2023Final %>%
  mutate(
    WTotal2ndSVPts = WTotalSVPts - W1stIn,
    LTotal2ndSVPts = LTotalSVPts - L1stIn
  )

# 2nd ServesIn
ATP2023Final <- ATP2023Final %>%
  mutate(
    W2ndIn = WTotal2ndSVPts - WDoubleFault,
    L2ndIn = LTotal2ndSVPts - LDoubleFault
  )

# 2nd Serve Percentage In
ATP2023Final <- ATP2023Final %>%
  mutate(
    W2ndSvIn_Percentage = (WTotal2ndSVPts - WDoubleFault) / WTotal2ndSVPts * 100,
    L2ndSvIn_Percentage = (LTotal2ndSVPts - LDoubleFault) / LTotal2ndSVPts * 100
  )
# 2nd Serve Won Percentage
ATP2023Final <- ATP2023Final %>%
  mutate(
    W2ndSvWon_Percentage = (W2ndWon / W2ndIn) * 100,
    L2ndSvWon_Percentage = (L2ndWon / L2ndIn) * 100
  )

# Aggregate stats
player_stats_agg <- ATP2023Final %>%
  select(Winner, Loser, W1stSvWon_Percentage, L1stSvWon_Percentage, WBPConv_Percentage, LBPConv_Percentage, W2ndSvWon_Percentage, L2ndSvWon_Percentage, W1stSv_Percentage, L1stSv_Percentage, W2ndSvIn_Percentage, L2ndSvIn_Percentage) %>%
  pivot_longer(cols = c(Winner, Loser), names_to = "Player_Type", values_to = "Players") %>%
  pivot_longer(cols = c(W1stSvWon_Percentage, L1stSvWon_Percentage, WBPConv_Percentage, LBPConv_Percentage, W2ndSvWon_Percentage, L2ndSvWon_Percentage, W1stSv_Percentage, L1stSv_Percentage, W2ndSvIn_Percentage, L2ndSvIn_Percentage),
               names_to = "Statistic_Type", values_to = "Value") %>%
  group_by(Players) %>%
  summarise(
    FirstSvWon_Percentage = mean(Value[Statistic_Type %in% c("W1stSvWon_Percentage", "L1stSvWon_Percentage")], na.rm = TRUE),
    BPConv_Percentage = mean(Value[Statistic_Type %in% c("WBPConv_Percentage", "LBPConv_Percentage")], na.rm = TRUE),
    SecondSvWon_Percentage = mean(Value[Statistic_Type %in% c("W2ndSvWon_Percentage", "L2ndSvWon_Percentage")], na.rm = TRUE),
    FirstSv_Percentage = mean(Value[Statistic_Type %in% c("W1stSv_Percentage", "L1stSv_Percentage")], na.rm = TRUE),
    SecondSv_Percentage = mean(Value[Statistic_Type %in% c("W2ndSvIn_Percentage", "L2ndSvIn_Percentage")], na.rm = TRUE)
  )

# Merge stats back into dataset
PlayersDF <- left_join(PlayersDF, player_stats_agg, by = "Players")

# Create Double Fault variables
winner_DF <- ATP2023Final %>%
  group_by(Winner) %>%
  summarize(TotalWDF = sum(WDoubleFault, na.rm = TRUE))

loser_DF <- ATP2023Final %>%
  group_by(Loser) %>%
  summarize(TotalLDF = sum(LDoubleFault, na.rm = TRUE))

# Combining both
DF_combined <- bind_rows(winner_DF %>% rename(Player=Winner, DF=TotalWDF), 
                           loser_DF %>% rename(Player=Loser, DF=TotalLDF)) %>%
  group_by(Player) %>%
  summarize(TotalDF = sum(DF, na.rm = TRUE))

# Merging datasets
PlayersDF <- merge(PlayersDF, DF_combined, by.x = "Players", by.y = "Player", all.x = TRUE)

# Calculate Matches Played and Wins
player_matches <- ATP2023Final %>%
  select(Winner, Loser) %>%
  pivot_longer(cols = c(Winner, Loser), names_to = "Outcome", values_to = "Player") %>%
  group_by(Player) %>%
  summarise(
    MatchesPlayed = n(),
    Wins = sum(Outcome == "Winner")
  )

# Calculate Win Percentage
player_matches <- player_matches %>%
  mutate(WinPerc = (Wins / MatchesPlayed) * 100)

# Merge with PlayersDF
PlayersDF <- left_join(PlayersDF, player_matches, by = c("Players" = "Player"))


# ----- B1705 | Analysis | 20.03.2024 -----
# ----- 7. Adjustments -----
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


# -----  8. Assessing for Errors or Outliers -----
##### 8.1 Basic Statistics -----
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

##### 8.2. Boxplots: Assessing Outliers -----
boxplot(player_data$Aces, main = "Box Plot - Aces Outlier Detection")
boxplot(player_data$FirstServeInPercent, main = "Box Plot - First Sv In Outlier Detection")
boxplot(player_data$FirstServeWonPercent, main = "Box Plot - First Sv Won Outlier Detection")
boxplot(player_data$DF, main = "Box Plot - DF Outlier Detection")
boxplot(player_data$SecondServeInPercent, main = "Box Plot - Second Sv In Outlier Detection")
boxplot(player_data$SecondServeWonPercent, main = "Box Plot - Second Sv Won Outlier Detection")

##### 8.3. Scatterplot: Assessing Outliers -----

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

##### 8.4. Z-Scores: Assessing Outliers #####
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

# ----- 9. Testing Assumptions -----
##### 9.1. Normality Testing -----
variables_ <- c("Aces", "FirstServeInPercent", "FirstServeWonPercent", "DF", "SecondServeInPercent", "SecondServeWonPercent")

# Q-Q plots
plot_normality_qq <- function(data, variables) {
  par(mfrow = c(3, 2)) 
  for (var in variables_) {
    
    qqnorm(data[[var]], main = paste("QQ Plot -", var))
    qqline(data[[var]], col = "red")
    
    }
}

# Running functions
plot_normality_qq(player_data, variables_)

ggsave(filename = paste0("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1705/Assessment 1/R Files/Plot Visual Saves/scatterplot_", dv, "_vs_", Height, ".png"), plot = p, width = 8, height = 6, dpi = 300)

# Histogram plots
plot_normality_hist <- function(data, variables) {
  par(mfrow = c(3, 2)) 
  for (var in variables_) {
    # Histogram
    hist(data[[var]], main = paste("Histogram -", var), xlab = var, breaks = 30, col = 'black', border = 'white')
    
  }
}

# Running functions
plot_normality_hist(player_data, variables_)

# Anderson-Darling testing, 
library(nortest)

variables_to_test <- c("Aces", "DF", "FirstServeInPercent", "FirstServeWonPercent", "SecondServeInPercent", "SecondServeWonPercent")

for (variable in variables_to_test) {
  ad_test_result <- ad.test(player_data[[variable]])
  print(paste("Anderson-Darling Test Result for", variable))
  print(ad_test_result)
}

##### 9.2. Homoscedasticity Testing -----
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

##### 9.3. Linearity Testing -----
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


##### 9.4. Independence of Observations -----

# Violated; each data entry has an equivalent data entry from match which may influence 
# and is influenced, and data entries are at risk of being influenced dependent 
# on player form, confidence from match results of their previous match etc. 

##### 9.5. Data Transformation -----

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

##### 9.6. Heteroscedasticity Testing: Transformed Data -----
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

##### 9.6.1. Yeo-Johnson Transformation and Testing -----
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

##### 9.6.2. Running Q-Q & Histograms for YJ Transformed Data -----
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

# ----- 10. Statistical Analysis -----
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

# ----- 11. Results -----

# In paper, report results and statistics.

# ----- 12. Libraries Used Citations -----

# Libraries
libraries <- c("ggplot2", "tidyverse", "e1071", "nortest", "lmtest", "bestNormalize", "dplyr", "lubridate", "stringr", "dbplyr", "tidyr")

# Function to print each citation
for (lib in libraries) {
  cat("Citation for", lib, ":\n")
  print(citation(lib))
  cat("\n") 
}

# ----- 13. Tidy Environment ----
all_objects <- ls()
objects_to_keep <- c("PlayersDF", "ATP2023Final", "player_data")
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Leave ATP2023Final, PlayersDF & player_data
rm(list = objects_to_remove)

# Save player_data dataset as .xlsx file
write.xlsx(player_data, "/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1705/Assessment 1/R Files/player_data.xlsx", rowNames = FALSE)













