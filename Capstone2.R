library(dslabs)
library(tidyverse)
library(lubridate)

# ---- Import_Datas ----
edx <- readRDS("Data/edx.rds")
validation <- readRDS("Data/validation.rds")

dim(edx)
dim(validation)

head(edx)
head(validation)

#str(edx)
# ---- Recommendation_system_RMSE ----


RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}

# ---- Naive_approach ----

mu_hat <- mean(edx$rating)
mu_hat

# ---- Validation_Naive_approach_1 ----

predictions <- rep(mu_hat, nrow(validation))

naive_rmse <- RMSE(validation$rating, predictions)

# ---- Validation_Naive_approach_2 ----

rmse_results <-
  tibble(method = "Just the average", RMSE = naive_rmse)

rmse_results %>% knitr::kable()

# ---- Add_movie_bias ----

mu <- mean(edx$rating)

#Let's compute the average bias for each movies
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))

# ---- Plot_movie_bias ----
head(movie_avgs)

movie_avgs %>% ggplot(aes(x = b_m)) + geom_histogram(binwidth = 0.25, color = "black")


# ---- Validation_Movie_bias_approach_1 ----

#The RHS of the following code is of the form real_number + Matrix, where
#the matrix is given by the code
#validation %>%   left_join(movie_avgs, by='movieId') %>%  .$b_m

predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$b_m


# ---- Validation_Movie_bias_approach_2 ----

model_1_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie Effect Model",
                                 RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

# ---- plot_User_bias ----

edx %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n() >= 100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")


# ---- Add_User_bias ----
user_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))


# ---- Validation_User_bias_approach ----

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_m + b_u) %>%
  .$pred

model_2_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User Effects Model",
                                 RMSE = model_2_rmse))

rmse_results %>% knitr::kable()



# ---- Going_further_genres_bias ----


edx %>% group_by(genres) %>%
  filter(n()>35000) %>% 
  summarize(b_g = mean(rating)) %>%
  mutate(genres = reorder(genres, b_g)) %>% 
  ggplot(aes(x=genres,y=b_g)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Add_Genres_bias ----

genre_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_m - b_u))

# ---- Validation_Genres_bias_approach ----

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_m + b_u + b_g) %>%
  .$pred


model_3_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User + Genres Effects Model",
                                 RMSE = model_3_rmse))

rmse_results %>% knitr::kable()


# ---- Week_bias_add_predictor ----


edx <- edx %>% mutate(date = as_datetime(timestamp),
                      week = round_date(date, "week"))

validation <- validation %>% mutate(date = as_datetime(timestamp),
                                    week = round_date(date, "week"))

# ---- Plot_Week_bias ----

edx %>%
  group_by(week) %>%
  summarize(avg_ratings = mean(rating)) %>%
  ggplot(aes(x = week, y = avg_ratings)) +
  geom_point() +
  geom_smooth()


# ---- Add_Week_bias ----

week_avgs <- edx %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  group_by(week) %>%
  summarize(b_w = mean(rating - mu - b_m - b_u - b_g))

# ---- Validation_Week_bias_approach ----

predicted_ratings <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  left_join(week_avgs, by = 'week') %>%
  mutate(pred = mu + b_m + b_u + b_g + b_w) %>%
  .$pred


model_4_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(
  rmse_results,
  tibble(method = "Movie + User + Genres + Week Effects Model",
         RMSE = model_4_rmse)
)

rmse_results %>% knitr::kable()


# ---- Regularization_Movie_User ----

lambdas <- seq(0, 10, 0.25)


rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  #Regul movie bias
  Rmovie_bias <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu) / (n() + l))
  #Regul user bias
  Ruser_bias <- edx %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu) / (n() + l))
  
  #apply predictions
  predicted_ratings <-
    validation %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    left_join(Ruser_bias, by = "userId") %>%
    mutate(pred = mu + b_m + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)


#the lambda giving the minimal RMSE is 
lambda <- lambdas[which.min(rmses)]

lambda

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Regularized Movie + User Effect Model",
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# ---- Regularization_Movie_User_Genres ----


rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  #Regul movie bias
  Rmovie_bias <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu) / (n() + l))
  #Regul user bias
  Ruser_bias <- edx %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu) / (n() + l))
  #regul genres bias
  Rgenre_bias <- edx %>%
    left_join(Rmovie_bias, by = 'movieId') %>%
    left_join(Ruser_bias, by = 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_m - b_u - mu) / (n() + l))
  
  #apply predictions
  predicted_ratings <-
    validation %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    left_join(Ruser_bias, by = "userId") %>%
    left_join(Rgenre_bias, by = 'genres') %>%
    mutate(pred = mu + b_m + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)


#the lambda giving the minimal RMSE is 
lambda <- lambdas[which.min(rmses)]

lambda

rmse_results <- bind_rows(
  rmse_results,
  tibble(method = "Regularized Movie + User + Genres Effect Model",
             RMSE = min(rmses))
)
rmse_results %>% knitr::kable()


# ---- Regularization_Movie_User_Genres_Week ----

rmses <- sapply(lambdas, function(l) {
  mu <- mean(edx$rating)
  #Regul movie bias
  Rmovie_bias <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu) / (n() + l))
  #Regul user bias
  Ruser_bias <- edx %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu) / (n() + l))
  #regul genres bias
  Rgenre_bias <- edx %>%
    left_join(Rmovie_bias, by = 'movieId') %>%
    left_join(Ruser_bias, by = 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_m - b_u - mu) / (n() + l))
  #regul week bias
  Rweek_bias <- edx %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    left_join(Ruser_bias, by = "userId") %>%
    left_join(Rgenre_bias, by = 'genres') %>%
    group_by(week) %>%
    summarize(b_w = sum(rating - b_g - b_m - b_u - mu) / (n() + l))
  
  #apply predictions
  predicted_ratings <-
    validation %>%
    left_join(Rmovie_bias, by = "movieId") %>%
    left_join(Ruser_bias, by = "userId") %>%
    left_join(Rgenre_bias, by = 'genres') %>%
    left_join(Rweek_bias, by = 'week') %>%
    mutate(pred = mu + b_m + b_u + b_g + b_w) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)


#the lambda giving the minimal RMSE is 
lambda <- lambdas[which.min(rmses)]

lambda

rmse_results <- bind_rows(
  rmse_results,
  tibble(method = "Regularized Movie + User + Genres + Week Effect Model",
             RMSE = min(rmses))
)



# ---- Conclusion ----
rmse_results %>% knitr::kable()


rmse_results %>% filter(RMSE <=0.9) %>% 
  mutate(method=reorder(method,desc(RMSE))) %>% 
  ggplot(aes(x=method,y=RMSE, ymin=0.864, ymax=0.866)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
