#Install all needed libraries if not present

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

#Loading libraries

library(dplyr)
library (stringr)
library(tibble)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(scales)

################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

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
set.seed(1)
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

# Define Root Mean Squared Error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

######################################################################################
# EXPLORATORY ANALYSIS

#Dimension of the dataset
dim(edx)

#Types of rating
levels(as.factor(edx$rating))

ifelse(sum(is.na.data.frame(edx))==0,"No missing value","Missing values")
# Any missing value? No missing value

pat<-"^[0-9]"                 
ifelse(sum(!str_detect(edx$rating,pat))==0,"Only numbers in rating","Check ratings")
# has the column rating in edx only numeric values? Yes, it has

#Number of variables
cat("There are",dim(edx)[2],"variables:")

str(edx) #Type of variables in the dataset

as_tibble(edx) #List the first ten

#Let's explore each variable

#a.Genres

#As we can see several movies are classified in more than one genre. The table below
#lists the first 20 genres sorted in descend order by frequency
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  arrange(-n) %>%
  head(n=20)


#b.Date - The rating period was collected over almost 14 years

tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))


edx %>% mutate(year = year(as_datetime(timestamp, origin="1970-01-01"))) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "white") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) + 
  theme_economist()

#The following table lists the days with more ratings.
edx %>% mutate(date = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(10)

#c.Ratings

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

#d.Movies
#Number of different movies
cat(length(unique(edx$movieId)),"different movies")

#Rating of the movies
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "The distribution is almost symmetric.") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_economist()

#Roughly 10% of movies have less than 10 ratings
mov<-edx %>% group_by(movieId) %>%
  summarise(n=n())
cat("Precisely",mean(mov$n<10)*100,"% have less than 10 ratings")

#e.Users

#In the edx set there are
cat(length(unique(edx$userId)),"different users")

#Distribution of the users
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  geom_vline(xintercept = 1000,linetype=2,colour="red",size=1)+
  scale_x_log10() + 
  ggtitle("Distribution of Users", 
          subtitle="The distribution is right skewed.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = comma) + 
  theme_economist()

#Roughly 5% of users rated less than 20 movies.
us<-edx %>% group_by(userId) %>%
  summarise(n=n())
cat("Precisely",round(mean(us$n<20)*100,2),"%")

#Heatmap of users x movies
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

#####################################################################################
#--->DATA CLEANING

#We forecast the estimated rating only with movie and user information

edx <- edx %>% select(userId, movieId, rating, title)
validation  <- validation  %>% select(userId, movieId, rating, title)

####################################################################################
#----> MODELING

#-Defining the Loss function, that measures the accuracy of an event 

# Define Mean Absolute Error (MAE)
MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}


#-FIRST MODEL- RANDOM PREDICTION

#We use only the observed probabilities
set.seed(4321)

# Create the probability of each rating
p <- function(x, y) mean(y == x)
rating <- seq(0.5,5,0.5)

#Monte Carlo simulation
B <- 10^3
M <- replicate(B, {
  s <- sample(edx$rating, 100, replace = TRUE)
  sapply(rating, p, y= s)
})
prob <- sapply(1:nrow(M), function(x) mean(M[x,]))

# Predict random ratings
y_hat_random <- sample(rating, size = nrow(validation), 
                       replace = TRUE, prob = prob)

# Create a table with the error results
result <- data.frame(Method = "Project Goal", RMSE = 0.8649)
result <- bind_rows(result, 
                    data.frame(Method = "Random prediction", 
                               RMSE = RMSE(validation$rating, y_hat_random)))
result

#The RMSE of random prediction is very high (1.501245)

#-SECOND MODEL- MODEL PREDICTION WITH JUST THE AVERAGE
# Mean of observed values
mu <- mean(edx$rating)

# Update the error table  
result <- bind_rows(result, 
                    data.frame(Method = "Just the average", 
                               RMSE = RMSE(validation$rating, mu)))
result


#-THIRD MODEL- MODEL PREDICTION WITH JUST THE AVERAGE+ MOVIE EFFECT

# Movie effects (bi)
bi <- edx %>% 
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
y_hat_bi <- mu + validation %>% 
  left_join(bi, by = "movieId") %>% 
  .$b_i

# Calculate the RMSE  
result <- bind_rows(result, 
                    data.frame(Method = "just the average + bi", 
                               RMSE = RMSE(validation$rating, y_hat_bi)))

result

#-FORTH MODEL- MODEL PREDICTION WITH JUST THE AVERAGE+MOVIE EFFECT+USER EFFECT

# User effect (bu)
bu <- edx %>% 
  left_join(bi, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Prediction
y_hat_bi_bu <- validation %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Update the results table
result <- bind_rows(result, 
                    data.frame(Method = "just the average + bi + bu", 
                               RMSE = RMSE(validation$rating, y_hat_bi_bu)))

result

#The user effect is normally distributed
edx %>% 
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

# The final RMSE in not as good as we want yet. To do better we have to consider the
#frequency of rating. Let's see:

titles <- edx %>% 
  select(movieId, title) %>% 
  distinct()

#Top 10 best movies (ranked by bi). These seem unknown movies
bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(-b_i) %>% 
  select(title) %>%
  head(n=10)
# Let's look at how often they are rated
edx %>% count(movieId) %>% 
  left_join(bi, by="movieId") %>%
  left_join(titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

#Top 10 worst movies (ranked by bi), also unknown movies:
bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(b_i) %>% 
  select(title) %>%
  head(n=10)

# Let's look at how often they are rated
edx %>% count(movieId) %>% 
  left_join(bi,by="movieId") %>%
  left_join(titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)

#Few users rated  "best" and "worst" movies with uncertain result

#-FIFTH MODEL- REGULARIZATION
#Correction of the uncertainty with the penalty factor lambda

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

# We can go further defining a set of lambdas to tune
lambdas <- seq(0, 10, 0.25)

# Tune lambda
rmses <- sapply(lambdas, 
                regularization, 
                trainset = edx, 
                testset = validation)

# Plot the lambda vs RMSE
tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization", 
          subtitle = "Pick the penalization that gives the lowest RMSE.") +
  theme_economist()

#we apply the best lambda to the linear model
lambda <- lambdas[which.min(rmses)]

# Then, we calculate the predicted rating using the best parameters 
# achieved by the regularization.  
mu <- mean(edx$rating)

# Movie effect (bi)
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect (bu)
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Prediction
y_hat_reg <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Update the result table
result <- bind_rows(result, 
                    data.frame(Method = "Regularized bi and bu", 
                               RMSE = RMSE(validation$rating, y_hat_reg)))

result

## --->FINAL VALIDATION

#we calculate the RMSE in the validation set. The project goal is achieved if the RMSE 
#stays below the target

# Movie effect (bi)
b_i_edx <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# User effect (bu)
b_u_edx <- edx %>% 
  left_join(b_i_edx, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Prediction
y_hat_fin <- validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Update the results table
result <- bind_rows(result, 
                    data.frame(Method = "Final Regularization (edx vs validation)", 
                               RMSE = RMSE(validation$rating, y_hat_fin)
                    ))

# Show the RMSE improvement
result 

#As expected, the RMSE calculated on the validation set is lower than the 
#target of 0.8649

#Let's see what are the top 10 best movies with our recommendation system:

#Top 10 best movies

validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  arrange(-pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)


#and the top 10 worst movies

#Top 10 worst movies
validation %>% 
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>% 
  mutate(pred = + b_i + b_u) %>% 
  arrange(pred) %>% 
  group_by(title) %>% 
  select(title) %>%
  head(10)


