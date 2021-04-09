library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#First model
mu_hat <- mean(train_set$rating)
mu_hat
#> [1] 3.54
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
#> [1] 1.05
predictions <- rep(3, nrow(test_set))
RMSE(test_set$rating, predictions)
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#Movie effect
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings, test_set$rating)
#> [1] 0.989

#Movie+user effect
train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
#> [1] 0.905

#Regularization
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)
train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  pull(n)
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(title)
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  pull(title)
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
RMSE(predicted_ratings, test_set$rating)
#> [1] 0.97

lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
            rmses <- sapply(lambdas, function(l){
              predicted_ratings <- test_set %>% 
                left_join(just_the_sum, by='movieId') %>% 
                mutate(b_i = s/(n_i+l)) %>%
                mutate(pred = mu + b_i) %>%
                pull(pred)
              return(RMSE(predicted_ratings, test_set$rating))
            })
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
lambdas <- seq(0, 10, 0.25)
            
rmses <- sapply(lambdas, function(l){
              
              mu <- mean(train_set$rating)
              
              b_i <- train_set %>% 
                group_by(movieId) %>%
                summarize(b_i = sum(rating - mu)/(n()+l))
              
              b_u <- train_set %>% 
                left_join(b_i, by="movieId") %>%
                group_by(userId) %>%
                summarize(b_u = sum(rating - b_i - mu)/(n()+l))
              
              predicted_ratings <- 
                test_set %>% 
                left_join(b_i, by = "movieId") %>%
                left_join(b_u, by = "userId") %>%
                mutate(pred = mu + b_i + b_u) %>%
                pull(pred)
              
              return(RMSE(predicted_ratings, test_set$rating))
            })
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda
#> [1] 3.25

min(rmses)
#Regularized Movie + User Effect Model 	0.881
