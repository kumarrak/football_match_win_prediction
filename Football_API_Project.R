rm(list = ls())
#Installing and importing required  libraries
#install.packages("caret")
library(tidyverse)
library(caret) #?
library(httr)
library(glue) #url glue
library(magrittr)
library(purrr)
library(repurrrsive)
library(kableExtra)
library(data.table) #setnames
library(h2o)
library(skimr)
library(recipes)
library(stringr)
library(ggplot2)
library(ggthemes)
library(e1071)
#library(tracerer)
#library(rlist)
#library(patchwork)


#https://api.football-data.org/v2/matches
#https://api.football-data.org//v2/competitions
#ff9d3d9ab03f431aaf64ea00464e31a0 --Ashok
#4f4a4ebaefd9470686e8f917d4a2a3d1 --naveen
#6bbd4288011d4449a6867080c5eb207d --varun

token = "ff9d3d9ab03f431aaf64ea00464e31a0"
football_api = GET("https://api.football-data.org//v2/competitions",
                   add_headers("X-Auth-Token"= token)
)

#Establishing a connection with API
conn = content(football_api)
competitions = conn$competitions

#Storing all competition id and their names in df
ids = map_int(competitions, ~.$id)
names = map_chr(competitions, ~.$name)

all_competitions = data.frame(id = ids, name = names)

#display all competitions
all_competitions

write.csv(all_competitions, file="all_competitions.csv", row.names = FALSE)
#Selecting required competitions by their id's from the API
#SelctCompetitions = c("2000", "2001", "2002", "2003","2004", "2012", "2014", "2015", "2016", "2017", "2018", "2019", "2021","2058","2140","2146","2152","2154")
#SelctCompetitions = subset (all_competitions, select = -c(34, 27, 38, 36,40))
#matches_df <- subset (matches_df, select = -c(1,7,13))
SelctCompetitions = c(all_competitions[1:157,])
# All selected competitions from above
#selected_comps = all_competitions %>%
#  filter(all_competitions$id %in% SelctCompetitions)
selected_comps = SelctCompetitions

#display all available compts
selected_comps

#Retrieving match data from the API
matches_data = list()


for(id in selected_comps$id){
  url = glue("http://api.football-data.org/v2/competitions/{id}/matches?dateFrom=2020-02-01&dateTo=2022-04-01")
  matches_comp = GET(url, add_headers("X-Auth-Token"= token))
  matches_data = append(matches_data, content(matches_comp))
}

all_matches = matches_data[names(matches_data) == "matches"]
summary(all_matches)

#Converting list of matches to a df.
matches_df = data.frame()

for(x in 1:50000){
  tbl <- as_tibble(all_matches[x])
  matches_df <- bind_rows(matches_df, unnest_wider(tbl,matches))
}


matches_df <- subset (matches_df, select = -c(1,7,13))
#write.csv(matches_df, file="matches_api_tt.csv", row.names = FALSE)
#selecting only required columns
#matches_df <- subset (matches_df, select = c(season,utcDate,status,matchday,stage,group,lastUpdated,odds,score,homeTeam,awayTeam))
#matches_df <- subset (matches_df, select = -c(NA,id, season, referees, group))
#names(matches_df)[0]<- "Sl No"

view(matches_df)


######## Data cleaning ##################

#Un-nesting list data for odds column:

matches_df  <- matches_df %>%
  mutate(df = map(odds, ~ .x %>% 
                    map_df(magrittr::extract,)))

view(matches_df)

matches_df  <- matches_df  %>%
  select(-odds) %>%  ##changes -odds to odds
  unnest(df)

#Renaming the columns for enhance understanding 
matches_df <- matches_df %>% 
  dplyr::rename(
    Odds.homeWin = homeWin,
    Odds.draw = draw,
    Odds.awayWin = awayWin
  )
view(matches_df)

#Un-nesting list data for awayTeam column
#converting list data to a column for awayTeam column
matches_df  <- matches_df %>%
  mutate(df = map(awayTeam, ~ .x %>% 
                    map_df(magrittr::extract, 
                    )))
matches_df  <- matches_df  %>%
  select(-awayTeam) %>%
  unnest(df)

matches_df <- subset (matches_df, select = -c(id))

matches_df <- matches_df %>% 
  dplyr::rename(
    awayTeam = name
  )
view(matches_df)
#Un-nesting list data for homeTeam column
#converting list data to a column for homeTeam column

matches_df  <- matches_df %>%
  mutate(df = map(homeTeam, ~ .x %>% 
                    map_df(magrittr::extract, 
                    )))

matches_df  <- matches_df  %>%
  select(-homeTeam) %>%
  unnest(df)

matches_df <- subset (matches_df, select = -c(id))
matches_df <- matches_df %>% 
  dplyr::rename(
    homeTeam = name
  )
view(matches_df)

# Un-nesting list data for scores column.
#This will yield full time and half time goals of the homeTeam and awayTeam

scores = matches_df$score
winner_vector = map_chr(scores,"winner", .default = NA)

fullTimeScores = map(scores,"fullTime")
fTHome = map_int(fullTimeScores,"homeTeam", .default = NA)
fTAway = map_int(fullTimeScores,"awayTeam", .default = NA)

halfTimeScores = map(scores,"halfTime")
hTHome = map_int(halfTimeScores,"homeTeam", .default = NA)
hTAway = map_int(fullTimeScores,"awayTeam", .default = NA)


matches_df$halfTime_score_Home = hTHome
matches_df$halfTime_score_Away = hTAway
matches_df$fullTime_score_Home = fTHome
matches_df$fullTime_score_Away = fTAway
matches_df$winner = winner_vector


view(matches_df) 

football_df <- subset (matches_df, select = -c(score))
#football_df <- football_df[, c("Odds.homeWin","Odds.draw","Odds.awayWin","hTHome", "hTAway","fTHome", "fTAway", "homeTeam", "awayTeam","winner")]
football_df <- football_df[, c("Odds.homeWin","Odds.draw","Odds.awayWin","halfTime_score_Home", "halfTime_score_Away","fullTime_score_Home", "fullTime_score_Away", "homeTeam", "awayTeam","winner")]

football_df
view(football_df)
write.csv(football_df, file="football_api_fully.csv", row.names = FALSE)

#read complete data

dff <- read.csv(file ="https://raw.githubusercontent.com/kumarrak/football/main/football_api_full.csv")
dff1 <- read.csv(file ="https://raw.githubusercontent.com/kumarrak/football/main/football_api_full1..csv")
dff2 <- read.csv(file ="https://raw.githubusercontent.com/kumarrak/football/main/football_api_full2.csv")
df_complete <- do.call('rbind', list(dff1, dff2, dff))

#21353 records
nrow(df_complete)

#drop rows with NA values
colSums(is.na(df_complete))

df_complete <- drop_na(df_complete)
view(df_complete)

#Total No. of records
nrow(df_complete)

# Assessing the winner based on the outcome of the match
#0 = Draw match, 1 = Home team Win, 2 = Away team Win
df_complete$winner <- ifelse(df_complete$winner == "DRAW",0,df_complete$winner)
df_complete$winner <- ifelse(df_complete$winner == "HOME_TEAM",1,df_complete$winner)
df_complete$winner <- ifelse(df_complete$winner == "AWAY_TEAM",2,df_complete$winner)
df_complete$winner <- as.factor(df_complete$winner)

#Final dataset
view(df_complete)


#1) Count of wins(Home team, Away team), Draws

ggplot(df_complete, aes(x=winner, fill=winner )) + 
  geom_bar(colour="black")+
  geom_text( stat='count', aes(label=..count..), vjust= 2, position = position_dodge(.9) ) +
  theme_economist()+
  scale_fill_discrete(name = "Winner", labels = c("Draw", "Home Team", "Away Team"))
  

homewins =subset(df_complete, winner==1) %>% count(homeTeam )
homewins <- homewins[with(homewins,order(-n)),]
homewins <- homewins[1:10,]

awaywins =subset(df_complete, winner==2) %>% count(awayTeam )
awaywins <- awaywins[with(awaywins,order(-n)),]
awaywins <- awaywins[1:10,]

ggplot(data = homewins, aes(y = reorder(homeTeam, n), x = n, fill = homeTeam)) +
  geom_bar(stat = "identity", position = 'dodge')+
  geom_text(aes(label = n), vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6))+
  xlab("wins") +
  ylab("Team Name")+
  theme_gdocs()+
  labs(title = "Top Home Team Winners ")+
  theme(legend.position="none")

ggplot(data = awaywins, aes(y = reorder(awayTeam, n), x = n, fill = awayTeam)) +
  geom_bar(stat = "identity", position = 'dodge')+
  geom_text(aes(label = n), vjust = 1.5, colour = "black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6))+
  xlab("wins") +
  ylab("Team Name")+
  theme_gdocs()+
  labs(title = "Top Away Team Winners ")+
  theme(legend.position="none")

# 3) Top 3 home winning teams: matches draw, won and lost

count_homewins_top3 <- homewins[1:3,]
count_homewins_top3 <- count_homewins_top3[, c('homeTeam')]
three_team_df <- df_complete[, c('homeTeam', 'winner')] %>% 
  filter(grepl('CA Mineiro|CR Flamengo|Fluminense FC', homeTeam))%>% 
  group_by(homeTeam)

ggplot(three_team_df, aes(homeTeam)) + 
  geom_bar(aes(fill = winner), position="dodge")+
  theme_gdocs()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+
  geom_text( stat='count', aes(fill = winner, label=..count..), vjust= 2, position = position_dodge(.9) )+
  scale_fill_manual(values=c("#EC7063","#AF7AC5","#F5D6B1"))
  
  #scale_fill_discrete(name = "Winner", labels = c("Draw", "Home Team", "Away Team"))



##### Split the dataset into train (70%) and test (30%). ######

set.seed(200)
trainIndex <- createDataPartition(df_complete$winner,p = 0.8,list = FALSE)

traindata <- df_complete[trainIndex,]
nrow(traindata)
head(traindata)

testdata <- df_complete[-trainIndex,]
nrow(testdata)
head(testdata)


#Organize the data:
# Training data
x_train_tbl <- traindata %>% select(-winner)
y_train_tbl <- traindata %>% select(winner)

# Testing data
x_test_tbl <-testdata %>% select(-winner)

#Data inspection
x_train_tbl_skim = partition(skim(x_train_tbl))
names(x_train_tbl_skim)

# Data Wrangling

string_2_factor_names <- x_train_tbl_skim$character$skim_variable
string_2_factor_names


unique_numeric_values_tbl <- x_train_tbl %>%
  select(x_train_tbl_skim$numeric$skim_variable) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  mutate(key = as_factor(key))
unique_numeric_values_tbl

factor_limit <- 12
num_2_factor_names <- unique_numeric_values_tbl %>%
  filter(value < factor_limit) %>%
  arrange(desc(value)) %>%
  pull(key) %>% # pull out a single variable as a vector
  as.character()
num_2_factor_names

# Recipes for ML Data Transformation

rec_obj <- recipe(~ ., data = x_train_tbl) %>%
  step_string2factor(all_of(string_2_factor_names)) %>%
  step_num2factor(all_of(num_2_factor_names),
                  levels=str_c(0:1000),
                  transform = function(x) x + 1) %>%
  step_impute_mean(all_numeric()) %>% # missing values in numeric columns
  step_impute_mode(all_nominal()) %>% # missing values in factor columns
  prep(training = x_train_tbl)

rec_obj

#Start baking:

x_train_processed_tbl <- bake(rec_obj, x_train_tbl)
x_test_processed_tbl <- bake(rec_obj, x_test_tbl)

#Transforming the outcome variable (y):

rec_obj_for_y <- recipe(~ ., data = y_train_tbl) %>%
  prep(stringsAsFactors = FALSE)
y_train_processed_tbl <- bake(rec_obj_for_y, y_train_tbl)

################ Use H2o ##########################
#We will use h2o 

h2o.init(nthreads = -1)

#h2o.clusterInfo()

data_h2o <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl),
  destination_frame= "train.hex") 

new_data_h2o <- as.h2o(
  x_test_processed_tbl,
  destination_frame= "test.hex"
)

data_h2o_no_destination <- as.h2o(
  bind_cols(y_train_processed_tbl, x_train_processed_tbl)
)

h2o.ls()

#Splitting the training data into 3 subsets:
# 70/15/15 split
splits <- h2o.splitFrame(data = data_h2o, seed = 555,
                         ratios = c(0.7, 0.15))
train_h2o <- splits[[1]] 
valid_h2o <- splits[[2]]
test_h2o <- splits[[3]] 

# Deep Learning Model 1

y <- "winner" # column name for outcome
x <- colnames(select(df_complete,-c('winner','homeTeam','awayTeam','fullTime_score_Home','fullTime_score_Away')))

m1 <- h2o.deeplearning(
  model_id = "dl_model_first",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o, 
  epochs = 2
)

summary(m1)

# Deep Learning Model 2 with some serious tuning

m2 <- h2o.deeplearning(
  model_id="dl_model_tuned",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  overwrite_with_best_model = F, ## Return the final model after 100 epochs,
  ## even if not the best
  hidden = c(50,50,50), ## more hidden layers -> more complex interactions
  epochs = 2, 
  score_validation_samples = 10000, ## downsample validation set for faster scoring
  score_duty_cycle = 0.025, ## don't score more than 2.5% of the wall time
  adaptive_rate = F, ## manually tuned learning rate
  rate = 0.01,
  rate_annealing = 2e-6,
  momentum_start = 0.2, ## manually tuned momentum
  momentum_stable = 0.4,
  momentum_ramp = 1e7,
  l1 = 1e-5, ## add some L1/L2 regularization
  l2 = 1e-5,
  max_w2 = 10 ## helps stability for Rectifier
)

summary(m2)

# Hyper-parameter tuning w/grid search

hyper_params <- list(
  hidden = list( c(50,50,50), c(60,60) ),
  input_dropout_ratio = c(0, 0.05),
  rate = c(0.01, 0.02),
  rate_annealing = c(1e-8, 1e-7, 1e-6)
)

grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  epochs = 3,
  stopping_metric = "misclassification",
  stopping_tolerance = 1e-2, 
  stopping_rounds = 2,
  score_validation_samples = 10000, ## downsample validation set for faster scoring
  score_duty_cycle = 0.025, ## don't score more than 2.5% of the wall time
  adaptive_rate = F, #manually tuned learning rate
  momentum_start = 0.5, #manually tuned momentum
  momentum_stable = 0.9,
  momentum_ramp = 1e7,
  l1 = 1e-5,
  l2 = 1e-5,
  activation = c("Rectifier"),
  max_w2 = 10, #can help improve stability for Rectifier
  hyper_params = hyper_params
)

grid <- h2o.getGrid("dl_grid", sort_by="logloss", decreasing=FALSE)
dl_grid_summary_table <- grid@summary_table
dl_grid_summary_table


#To find best model in the grid

dl_grid_best_model <- h2o.getModel(dl_grid_summary_table$model_ids[1])
summary(dl_grid_best_model)

#To find parameters used in the best model

dl_grid_best_model_params <- dl_grid_best_model@allparameters
dl_grid_best_model_params


# AutoML in h2o

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 200 
)

automl_leaderboard <- automl_models_h2o@leaderboard
automl_leaderboard

automl_leader <- automl_models_h2o@leader

performance_h2o <- h2o.performance(automl_leader, newdata = test_h2o)
performance_h2o %>%
  h2o.confusionMatrix()

#Close the h2o:

h2o.shutdown(prompt = F)


#######  Naive Bayes Model ###########
# Fitting to training dataset

nvclassifier <- naiveBayes(winner ~ ., data = traindata)
nvclassifier

# Predicting on test data'
y_pred <- predict(nvclassifier, newdata = testdata)

# Confusion Matrix
cm <- table(testdata$winner, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)

### Summary
#After splitting the dataset into 80% of traing data and the rest as testing data.
#We have applied Naive Bayes model to the test data and got an accuracy of 72%. 
#Then we have fitted with train and validation data which is obtained from 80% of the original data, the accuracy of the model seems to fall below with test data compared to train and validation data as the epochs increase.

