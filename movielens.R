
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
if(!exists("edx") ) { dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
}

if(!exists("edx")) edx <- readRDS("D:/R/MovieLens/edx.rds")
if(!exists("validation")) validation <- readRDS("D:/R/MovieLens/validation.rds")



##RMSE using different models
##1 Predict using mean rating
edx_mean <- mean(e1$rating)

RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}


naive_rmse <- RMSE(v1$rating, edx_mean)

predictions <- rep(round(edx_mean,1) , nrow(v1))

rmse_results <- data_frame(method = "Using Mean Rating", RMSE = naive_rmse)

rmse_results%>% knitr::kable()


##2 Predict using Mean rating and movie effect 

movie_avgs <- e1 %>% 
     group_by(movieId) %>%  summarize(b_i = mean(rating - edx_mean))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- edx_mean + v1 %>% 
     left_join(movie_avgs, by='movieId') %>%
     .$b_i

model_1_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results2 <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

##3 Predict using Mean rating , movie effect and User effect 


user_avgs <- e1 %>% select (movieId, userId,rating) %>%
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - edx_mean - b_i))
user_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- v1 %>% select (movieId, userId) %>%
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = edx_mean + b_i + b_u) %>%
     .$pred
model_2_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results3 <- bind_rows(rmse_results2,
      data_frame(method="Movie + User Effects Model",   RMSE = model_2_rmse ))



##4 Predict using Mean rating , movie effect , User effect and Genre effect")

if(!exists("r1")) r1<- e1 %>%select(movieId) %>% left_join(movie_avgs, by='movieId') %>% select (b_i)   #split columns to lower memory usage
if(!exists("r2")) r2<- e1%>%select(userId) %>% left_join(user_avgs, by='userId') %>% select (b_u)
if(!exists("c1a"))  c1a <- e1 %>%   select (movieId, userId) %>% left_join(movie_avgs, by='movieId') %>% 
     left_join(user_avgs, by='userId') 
                      
if(!exists("c1"))   c1 <- e1 %>% select (genres, rating) %>% cbind (c1a$b_i , c1a$b_u) 
genre_avgs <- c1 %>% set_names("genres", "rating","b_i","b_u") %>%     group_by(genres) %>%
     summarize(b_g = mean(rating - edx_mean - b_i-b_u))
genre_avgs %>% qplot(b_g, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- v1 %>%   select (movieId, userId,genres) %>%
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
     mutate(pred = edx_mean + b_i + b_u+b_g) %>%
     .$pred
model_3_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results4 <- bind_rows(rmse_results3,
     data_frame(method="Movie + User + Genre Effects Model",  
        RMSE = model_3_rmse ))

rm("c1", "c1a")

##5 Predict using Mean rating , movie , User, Genre and movie year


if(!exists("r3")) r3<- e1 %>%   select (genres) %>% left_join(genre_avgs, by='genres') %>% select (b_g)

if(!exists("c2")) c2 <- cbind(e1$myear,e1$rating,r1,r2,r3) 
c2 <- as.data.frame(c2)%>%set_names("myear","rating","b_i","b_u","b_g")
myear_avgs <- c2 %>%    group_by(myear) %>%
     summarize(b_y = mean(rating - edx_mean - b_i- b_u - b_g  ))
myear_avgs %>% qplot(b_y, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- v1 %>%   select (movieId, userId,genres,myear) %>%  
   left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(myear_avgs, by='myear') %>%
     mutate(pred = edx_mean + b_i + b_u+b_g +b_y) %>%
     .$pred
model_4_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results5 <- bind_rows(rmse_results4,
     data_frame(method="Movie + User + Genre + Movie year Effects Model",  
        RMSE = model_4_rmse ))



rm("c2")

##6 Predict using Mean rating , movie , User, Genre, movie year and rating year

if(!exists("r4")) r4<- e1 %>%   select (myear) %>%  left_join(myear_avgs, by='myear') %>% select (b_y)
if(!exists("y5")) y5<- cbind(e1$ryear,  e1$rating, r1,r2,r3,r4)
y5 <- as.data.frame(y5) %>% set_names("ryear","rating","b_i","b_u","b_g","b_y")
ryear_avgs  <- y5 %>%  group_by(ryear) %>%
     summarize(b_y2 = mean(rating - edx_mean - b_i- b_u - b_g - b_y ))
ryear_avgs %>% qplot(b_y2, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- v1 %>%   select (movieId, userId,genres, myear,ryear)  %>%
     left_join(movie_avgs, by='movieId') %>%  
     left_join(user_avgs, by='userId') %>% 
  left_join(genre_avgs, by='genres') %>%
  left_join(myear_avgs, by='myear') %>%  
  left_join(ryear_avgs, by='ryear') %>% 
     mutate(pred = edx_mean + b_i + b_u+b_g +b_y +b_y2)%>%
     .$pred
  
model_5_rmse <- RMSE( predicted_ratings , v1$rating)
rmse_results6 <- bind_rows(rmse_results5,
     data_frame(method="Movie + User + Genre + Movie year + Rating year Effects Model",  
        RMSE = model_5_rmse ))
rmse_results6 %>% knitr::kable()

rm(y5)
##7 Predict using Mean rating , movie , User, Genre, movie year, rating Yearlapsed  effect
if(!exists("r1")) r1<- e1 %>%select(movieId) %>% left_join(movie_avgs, by='movieId') %>% select (b_i)
if(!exists("r2")) r2<- e1%>%select(userId) %>% left_join(user_avgs, by='userId') %>% select (b_u)
r5<- e1%>%select(ryear) %>% left_join(ryear_avgs, by='ryear') %>% select(b_y2)
 c7 <- cbind(e1$yearlapsed,e1$rating,r1,r2, r3, r4,r5)
c7 <- as.data.frame(c7)%>%set_names("yearlapsed","rating","b_i","b_u", "b_g", "b_y", "b_y2")
yearlapsed_avgs <- c7  %>%     group_by(yearlapsed) %>%
     summarize(b_yl = mean(rating - edx_mean - b_i-b_u - b_g -b_y - b_y2))
yearlapsed_avgs %>% qplot(b_yl, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <-  v1 %>%   select (movieId, userId,genres, myear,ryear, yearlapsed)  %>%
     left_join(movie_avgs, by='movieId') %>%  
     left_join(user_avgs, by='userId') %>% 
  left_join(genre_avgs, by='genres') %>%
  left_join(myear_avgs, by='myear') %>%  
  left_join(ryear_avgs, by='ryear') %>%
  left_join(yearlapsed_avgs, by='yearlapsed') %>%
     mutate(pred = edx_mean + b_i + b_u+b_g+b_y+b_y2+b_yl) %>%
     .$pred
model_6_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results7 <- bind_rows(rmse_results6,
     data_frame(method="Movie + User + Genres + Movie year + Rating year + Year lapsed Effects Model",  
        RMSE = model_6_rmse ))

rm("c7")
rm(r1,r2,r3,r4,r5)

##8 Predict using Mean rating and Regularized Movie Effect
lambda <- 3
mu <- mean(e1$rating)
movie_reg_avgs <- e1 %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
    ggplot(aes(original, regularlized, size=sqrt(n))) + 
    geom_point(shape=1, alpha=0.5)

e1 %>%
     dplyr::count(movieId) %>% 
     left_join(movie_reg_avgs) %>%
     arrange(desc(b_i)) %>% 
     select( b_i, n) %>% 
     slice(1:10) %>% 
     knitr::kable()
v1 %>%
     dplyr::count(movieId) %>% 
     left_join(movie_reg_avgs) %>%
     arrange(b_i) %>% 
     select( b_i, n) %>% 
     slice(1:10) %>% 
     knitr::kable()
predicted_ratings <- v1 %>% 
     left_join(movie_reg_avgs, by='movieId') %>%
     mutate(pred = mu + b_i) %>%
     .$pred
model_8_rmse <- RMSE(predicted_ratings, v1$rating)
rmse_results8 <- bind_rows(rmse_results,
       data_frame(method="Regularized Movie Effect Model",  
         RMSE = model_8_rmse ))



rm(movie_reg_avgs)

lambdas <- seq(0, 10, 0.25)
mu <- mean(e1$rating)
just_the_sum <- e1 %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
rmses8a <- sapply(lambdas, function(l){
    predicted_ratings <- v1 %>% 
        left_join(just_the_sum, by='movieId') %>% 
        mutate(b_i = s/(n_i+l)) %>%
        mutate(pred = mu + b_i) %>%
        .$pred
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses8a)  
l1 <- lambdas[which.min(rmses8a)]

rmses8 <- v1 %>% 
        left_join(just_the_sum, by='movieId') %>% 
        mutate(b_i = s/(n_i+l1)) %>%
        mutate(pred = mu + b_i) %>%
        .$pred
model_8_rmse <- RMSE(rmses8, v1$rating)
rmse_results8 <- bind_rows(rmse_results,
       data_frame(method="Regularized Movie Effect Model",  
         RMSE = model_8_rmse ))


 
##9 Predict using Mean rating , Regularized Movie + User Effect

lambdas <- seq(0, 10, 0.25)

 movie_avgs2e <- e1 %>% select(movieId) %>%  left_join(just_the_sum, by='movieId') %>%     mutate(b_i = s/(n_i+l1)) %>%
       select (movieId , b_i)
  movie_avgs2v <- v1 %>% select(movieId) %>%  left_join(just_the_sum, by='movieId') %>%     mutate(b_i = s/(n_i+l1)) %>%
       select (movieId , b_i)

u1 <- e1 %>% select ( userId, rating) %>% cbind (movie_avgs2e$b_i) %>% set_names ("userId","rating","b_i") %>% group_by(userId) %>%      summarize(s = sum(rating - mu - b_i ), n_i = n()) %>% select(userId,s,n_i)


rmses9a <- sapply(lambdas, function(l){
    predicted_ratings <- v1 %>% select ( userId)  %>% 
      cbind (rmses8)%>%   set_names ("userId","mu_b_i")%>%
      left_join(u1, by='userId') %>% 
        mutate(b_u = s/(n_i+l)) %>%
        mutate(pred = mu_b_i +b_u) %>%
        .$pred
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses9a)  
l2<- lambdas[which.min(rmses9a)]


rmses9 <- v1 %>% select ( userId)  %>% 
      cbind (rmses8)%>%   set_names ("userId","mu_b_i")%>%
      left_join(u1, by='userId') %>% 
        mutate(b_u = s/(n_i+l2)) %>%
        mutate(pred = mu_b_i +b_u) %>%
        .$pred
model_9_rmse <- RMSE( rmses9 , v1$rating)
rmse_results9 <- bind_rows(rmse_results8,
       data_frame(method="Regularized User Effect Model",  
         RMSE = model_9_rmse ))


##10 Predict using Mean rating , Regularized Movie + User + Genres Effect


lambdas <- seq(0, 10, 0.25)

 user_avgs2 <- e1 %>% select(userId) %>%  left_join(u1, by='userId') %>%     mutate(b_u = s/(n_i+l2)) %>%  select (userId , b_u)

 rm(u1)

g1 <- e1 %>% select ( genres, rating) %>% cbind (movie_avgs2e$b_i,user_avgs2$b_u) %>% set_names ("genres","rating","b_i","b_u") %>% group_by(genres) %>%      summarize(s = sum(rating - mu - b_i - b_u ), n_i = n()) %>% select(genres,s,n_i)


rmses10a <- sapply(lambdas, function(l){
    predicted_ratings <- v1 %>% select ( genres)  %>% 
      cbind (rmses9)%>%   set_names ("genres","mu_b_i_bu")%>%
      left_join(g1, by='genres') %>% 
        mutate(b_g = s/(n_i+l)) %>%
        mutate(pred = mu_b_i_bu +b_g) %>%
        .$pred
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses10a)  
l3<- lambdas[which.min(rmses10a)]


rmses10 <- v1 %>% select (genres)  %>% 
      cbind (rmses9)%>%   set_names ("genres","mu_b_i_bu")%>%
      left_join(g1, by='genres') %>% 
        mutate(b_g = s/(n_i+l3)) %>%
        mutate(pred = mu_b_i_bu +b_g) %>%
        .$pred
model_10_rmse <- RMSE( rmses10 , v1$rating)
rmse_results10 <- bind_rows(rmse_results9,
       data_frame(method="Regularized Genres Effect Model",  
         RMSE = model_10_rmse ))

rmse_results10 %>% knitr::kable()


##11 Predict using Mean rating , Regularized Movie + User + Genres + Movie Year Effect


lambdas <- seq(0, 10, 0.25)

 genres_avgs2 <- e1 %>% select(genres) %>%  left_join(g1, by='genres') %>%     mutate(b_g = s/(n_i+l3)) %>%  select (genres , b_g)

rm(g1)
 
my1 <- e1 %>% select ( myear, rating) %>% cbind (movie_avgs2e$b_i,user_avgs2$b_u, genres_avgs2$b_g) %>% set_names ("myear","rating","b_i","b_u", "b_g") %>% group_by(myear) %>%      summarize(s = sum(rating - mu - b_i - b_u -b_g ), n_i = n()) %>% select(myear,s,n_i)


rmses11a <- sapply(lambdas, function(l){
    predicted_ratings <- v1 %>% select ( myear)  %>% 
      cbind (rmses10)%>%   set_names ("myear","mu_bi_bu_bg")%>%
      left_join(my1, by='myear') %>% 
        mutate(b_y = s/(n_i+l)) %>%
        mutate(pred = mu_bi_bu_bg +b_y) %>%
        .$pred
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses11a)  
l4<- lambdas[which.min(rmses11a)]


rmses11 <- v1 %>% select (myear)  %>% 
      cbind (rmses10)%>%   set_names ("myear","mu_bi_bu_bg")%>%
      left_join(my1, by='myear') %>% 
        mutate(b_y = s/(n_i+l4)) %>%
        mutate(pred = mu_bi_bu_bg +b_y) %>%
        .$pred
model_11_rmse <- RMSE( rmses11 , v1$rating)
rmse_results11 <- bind_rows(rmse_results10,
       data_frame(method="Regularized Movie Year Effect Model",  
         RMSE = model_11_rmse ))

##12 Predict using Mean rating , Regularized Movie + User + Genres + Movie Year + rating year Effect


lambdas <- seq(0, 10, 0.25)

 myear_avgs2 <- e1 %>% select(myear) %>%  left_join(my1, by='myear') %>%     mutate(b_y = s/(n_i+l4)) %>%  select (myear , b_y)


ry1 <- e1 %>% select ( ryear, rating) %>% cbind (movie_avgs2e$b_i,user_avgs2$b_u, genres_avgs2$b_g,  myear_avgs2$b_y) %>% set_names ("ryear","rating","b_i","b_u", "b_g", "b_y") %>% group_by(ryear) %>%      summarize(s = sum(rating - mu - b_i - b_u -b_g -b_y), n_i = n()) %>% select(ryear,s,n_i)


rmses12a <- sapply(lambdas, function(l){
    predicted_ratings <- v1 %>% select ( ryear)  %>% 
      cbind (rmses11)%>%   set_names ("ryear","mu_bi_bu_bg_by")%>%
      left_join(ry1, by='ryear') %>% 
        mutate(b_y2 = s/(n_i+l)) %>%
        mutate(pred = mu_bi_bu_bg_by +b_y2) %>%
        .$pred
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses12a)  
l5<- lambdas[which.min(rmses12a)]


rmses12 <- v1 %>% select (ryear)  %>% 
      cbind (rmses11)%>%   set_names ("ryear","mu_bi_bu_bg_by")%>%
      left_join(ry1, by='ryear') %>% 
        mutate(b_y2 = s/(n_i+l5)) %>%
        mutate(pred = mu_bi_bu_bg_by +b_y2) %>%
        .$pred
model_12_rmse <- RMSE( rmses12 , v1$rating)
rmse_results12 <- bind_rows(rmse_results11,
       data_frame(method="Regularized Rating Year Effect Model",  
         RMSE = model_12_rmse ))

rmse_results12 %>% knitr::kable()


##13 Predict using Mean rating , Regularized Movie + User + Genres + Movie Year + Rating Year + Year Lapsed Effect



lambdas <- seq(0, 10, 0.25)
ry2 <- ry1 %>%  mutate(b_y2 = s/(n_i+l5) ) %>%  select ( ryear, b_y2)
 ryear_avgs2 <- e1 %>% select(ryear) %>%  left_join(ry2, by='ryear') %>%  select ( b_y2)


yl1 <- e1 %>% select ( yearlapsed, rating) %>% cbind (movie_avgs2e$b_i,user_avgs2$b_u, genres_avgs2$b_g,  myear_avgs2$b_y, ryear_avgs2$b_y2) %>% set_names ("yearlapsed","rating","b_i","b_u", "b_g", "b_y", "b_y2") %>% group_by(yearlapsed) %>%      summarize(s = sum(rating - mu - b_i - b_u -b_g -b_y - b_y2), n_i = n()) %>% select(yearlapsed,s,n_i)


rmses13a <- sapply(lambdas, function(l){
    predicted_ratingsa <- v1 %>% select ( yearlapsed)  %>% 
          left_join(yl1, by='yearlapsed') %>% mutate(b_yl = s / (n_i + l) )%>% select(b_yl) 
    predicted_ratings = rmses12 + predicted_ratingsa$b_yl
    return(RMSE(predicted_ratings, v1$rating))
})
qplot(lambdas, rmses13a)  
l6<- lambdas[which.min(rmses13a)]


rmses13 <- v1 %>% select (yearlapsed)  %>% 
      cbind (rmses12)%>%   set_names ("yearlapsed","mu_bi_bu_bg_by_by2")%>%
      left_join(yl1, by='yearlapsed') %>% 
        mutate(b_yl = s/(n_i+l6)) %>%
        mutate(pred = mu_bi_bu_bg_by_by2 +b_yl) %>%
        .$pred
model_13_rmse <- RMSE( rmses13 , v1$rating)
rmse_results13 <- bind_rows(rmse_results12,
       data_frame(method="Regularized Year lapsd Effect Model",  
         RMSE = model_13_rmse ))

rmse_results13 %>% knitr::kable()

## End of report


