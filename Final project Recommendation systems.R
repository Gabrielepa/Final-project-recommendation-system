################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


library(dplyr)
library (stringr)
library(tibble)
library(ggplot2)


#DATABASE STRUCTURE of edx
str(edx)
dim(edx)

sum(is.na.data.frame(edx))    # Any missing value?
#No missing value

pat<-"^[0-9]"                 
sum(!str_detect(edx$rating,pat))  # has the column rating in edx only numeric values?
#Yes, it has

head(edx)

#Genres
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  head()

#The table above shows that several movies are classified in more than one genre. The 
#number of genres in each movie is listed in this table, sorted in descend order.

tibble(count = str_count(edx$genres, fixed("|")), genres = edx$genres) %>% 
  group_by(count, genres) %>%
  summarise(n = n()) %>%
  arrange(-count) %>% 
  head()

#Date
#The rating period was collected over almost 14 years
library(lubridate)
tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))

if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "white") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) + 
  theme_economist()

#The following table lists the days with more ratings. Not surprisingly, the movies are 
#well known blockbusters
edx %>% mutate(date = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(10)

#Ratings
#Users have the option to choose a rating value from 0.5 to 5.0, totaling 10 possible 
#values. This is unusual scale, so most movies get a rounded value rating, as shown in 
#the chart below
#Count the number of each ratings:
edx %>% group_by(rating) %>% summarize(n=n())
#How many ratings are in edx?
edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Rating Distribution", subtitle = "Higher ratings are prevalent.") + 
  xlab("Rating") +
  ylab("Count") +
  theme_economist()

#Movies
#There are 10677 different movies in the edx set. We know from intuition that some of 
#them are rated more than others, since many movies are watched by few users and 
#blockbusters tend to have more ratings.
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "The distribution is almost symetric.") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_economist()
#10.7% of movies have less than 10 ratings

#Users
#There are 69878 different users are in the edx set.
#The majority of users rate few movies, while a few users rate more than a thousand 
#movies.
#5% users rated less than 20 movies.
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head()

#The user distribution is right skewed
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Users", 
          subtitle="The distribution is right skewed.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = comma) + 
  theme_economist()
#% of users rated less than 20 movies
#Show the heatmap of users x movies
#This user-movie matrix is sparse, with the vast majority of empty cells. Notice that 
#four movies have more ratings, and one or two users are more active
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
title("User x Movie Matrix")

#--->DATA CLEANING

#As previously discussed, several features can be used to predict the rating for a given 
#user. However, many predictors increases the model complexity and requires more computer 
#resources, so in this research the estimated rating uses only movie and user information
train_set <- train_set %>% select(userId, movieId, rating, title)
test_set  <- test_set  %>% select(userId, movieId, rating, title)

#----> MODELING

#-Loss function

# Define Mean Absolute Error (MAE)
MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}
# Define Mean Squared Error (MSE)
MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings)^2)
}
# Define Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#-FIRST MODEL- RANDOM PREDICTION

#The first model randomly predicts the ratings using the observed probabilities in the 
#training set.
#Since the training set is a sample of the entire population and we don't know the real 
#distribution of ratings, the Monte Carlo simulation with replacement provides a good 
#approximation of the rating distribution
set.seed(4321, sample.kind = "Rounding")

# Create the probability of each rating
p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

# Estimate the probability of each rating with Monte Carlo simulation
B <- 10^3
M <- replicate(B, {
  s <- sample(train_set$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))

# Predict random ratings
y_hat_random <- sample(rating, size = nrow(test_set), 
                       replace = TRUE, prob = prob)

# Create a table with the error results
result <- tibble(Method = "Project Goal", RMSE = 0.8649, MSE = NA, MAE = NA)
result <- bind_rows(result, 
                    tibble(Method = "Random prediction", 
                           RMSE = RMSE(test_set$rating, y_hat_random),
                           MSE  = MSE(test_set$rating, y_hat_random),
                           MAE  = MAE(test_set$rating, y_hat_random)))
result
#The RMSE of random prediction is very high (1.501245)

#-SECOND MODEL- MODEL PREDICTION WITH JUST THE AVERAGE
# Mean of observed values
mu <- mean(train_set$rating)

# Update the error table  
result <- bind_rows(result, 
                    tibble(Method = "Mean", 
                           RMSE = RMSE(test_set$rating, mu),
                           MSE  = MSE(test_set$rating, mu),
                           MAE  = MAE(test_set$rating, mu)))
result
# Show the RMSE improvement
#1.060054

#-THIRD MODEL- MODEL PREDICTION WITH JUST THE AVERAGE+ MOVIE EFFECT

# Movie effects (bi)
bi <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
head(bi)
#The movie effect is normally left skewed distributed
bi %>% ggplot(aes(x = b_i)) + 
  geom_histogram(bins=10, col = I("black")) +
  ggtitle("Movie Effect Distribution") +
  xlab("Movie effect") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_economist()
# Predict the rating with mean + bi  
y_hat_bi <- mu + test_set %>% 
  left_join(bi, by = "movieId") %>% 
  .$b_i

# Calculate the RMSE  
result <- bind_rows(result, 
                    tibble(Method = "Mean + bi", 
                           RMSE = RMSE(test_set$rating, y_hat_bi),
                           MSE  = MSE(test_set$rating, y_hat_bi),
                           MAE  = MAE(test_set$rating, y_hat_bi)))

result
# Show the RMSE improvement  
#0.9429615

#-FORTH MODEL- MODEL PREDICTION WITH JUST THE AVERAGE+MOVIE EFFECT+USER EFFECT

# User effect (bu)
bu <- train_set %>% 
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Prediction
y_hat_bi_bu <- test_set %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Update the results table
result <- bind_rows(result, 
                    tibble(Method = "Mean + bi + bu", 
                           RMSE = RMSE(test_set$rating, y_hat_bi_bu),
                           MSE  = MSE(test_set$rating, y_hat_bi_bu),
                           MAE  = MAE(test_set$rating, y_hat_bi_bu)))

result
# Show the RMSE improvement  
#0.8646843

#The user effect is normally distributed
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(color = "black") + 
  ggtitle("User Effect Distribution") +
  xlab("User Bias") +
  ylab("Count") +
  scale_y_continuous(labels = comma) + 
  theme_economist()

#-EVALUATING THE MODEL RESULT
#Check the 10 largest residual differences
train_set %>% 
  left_join(bi, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10)
titles <- train_set %>% 
  select(movieId, title) %>% 
  distinct()
#Top 10 best movies (ranked by bi).
#These are unknown movies
bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(-b_i) %>% 
  select(title) %>%
  head()
#Top 10 worst movies (ranked by bi), also unknown movies:
bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(b_i) %>% 
  select(title) %>%
  head() 
#Number of ratings for 10 best movies:
train_set %>% 
  left_join(bi, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  slice(1:10)

#-FIFTH MODEL- REGULARIZATION
#Now, we regularize the user and movie effects adding a penalty factor ??, which is a 
#tuning parameter. We define a number of values for ?? and use the regularization 
#function to pick the best value that minimizes the RMSE
regularization <- function(lambda, trainset, testset){
  
  # Mean
  mu <- mean(trainset$rating)
  
  # Movie effect (bi)
  b_i <- trainset %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  # User effect (bu)  
  b_u <- trainset %>% 
    left_join(b_i, by="movieId") %>%
    filter(!is.na(b_i)) %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
  # Prediction: mu + bi + bu  
  predicted_ratings <- testset %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    filter(!is.na(b_i), !is.na(b_u)) %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, testset$rating))
}

# Define a set of lambdas to tune
lambdas <- seq(0, 10, 0.25)

# Tune lambda
rmses <- sapply(lambdas, 
                regularization, 
                trainset = train_set, 
                testset = test_set)

# Plot the lambda vs RMSE
tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization", 
          subtitle = "Pick the penalization that gives the lowest RMSE.") +
  theme_economist()

#Next, we apply the best ?? to the linear model
# We pick the lambda that returns the lowest RMSE.
lambda <- lambdas[which.min(rmses)]

# Then, we calculate the predicted rating using the best parameters 
# achieved from regularization.  
mu <- mean(train_set$rating)

# Movie effect (bi)
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect (bu)
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Prediction
y_hat_reg <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Update the result table
result <- bind_rows(result, 
                    tibble(Method = "Regularized bi and bu", 
                           RMSE = RMSE(test_set$rating, y_hat_reg),
                           MSE  = MSE(test_set$rating, y_hat_reg),
                           MAE  = MAE(test_set$rating, y_hat_reg)))

result
# Regularization made a small improvement in RMSE
#0.8641362

#--->FINAL VALIDATION

#As we can see from the result table, regularization and matrix factorization achieved 
#the target RMSE. So, finally we train the complete edx set with both models and 
#calculate the RMSE in the validation set. The project goal is achieved if the RMSE stays 
#below the target

#- LINEAR MODEL WITH REGULARIZATION
#During the training and testing phases, the linear model with regularization achieved 
#the target RMSE with a small margin. Here we do the final validation with the validation 
#set
mu_edx <- mean(edx$rating)

# Movie effect (bi)
b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_edx)/(n()+lambda))

# User effect (bu)
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_edx)/(n()+lambda))

# Prediction
y_hat_edx <- validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  pull(pred)

# Update the results table
result <- bind_rows(result, 
                    tibble(Method = "Final Regularization (edx vs validation)", 
                           RMSE = RMSE(validation$rating, y_hat_edx),
                           MSE  = MSE(validation$rating, y_hat_edx),
                           MAE  = MAE(validation$rating, y_hat_edx)))

# Show the RMSE improvement
result 

#As expected, the RMSE calculated on the validation set (0.8648177) is lower than the 
#target of 0.8649 and slightly higher than the RMSE of the test set (0.8641362)

#Top 10 best movies
validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = mu_edx + b_i + b_u) %>% 
  arrange(-pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)
#Top 10 worst movies
validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = mu_edx + b_i + b_u) %>% 
  arrange(pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)
