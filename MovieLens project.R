## Ben Neely
## PH125_9x MovieLens project

## Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(kableExtra)) install.packages("kableExtra")

## Location of downloaded data
## MovieLens 10M dataset:
## https://grouplens.org/datasets/movielens/10m/
## http://files.grouplens.org/datasets/movielens/ml-10m.zip

## Import data and clean it up a little bit
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

## Create test data set that consists of 10% of ratings
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

## Look at count of each rating for table development later
xtabs(~rating,edx)

## Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

## Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

###############################################################
###############################################################
## Tidy up data to create more predictors
## Extract year from title
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

edx1=edx%>%mutate(year=substrRight(title,5))
edx2=edx1%>%mutate(year=substr(year,1,4),
                   year=as.integer(year))

## Separate genres
edx3=edx2%>%separate_rows(genres,convert=T)

## Convert timestamp to usable data
edx4=edx3%>%
  mutate(date=as_datetime(timestamp))

edx4$rev_year=year(edx4$date)

## Keep userId, movieId, rating, genres, year, and rev_year
edx5=edx4%>%
  select(c("userId","movieId","rating","genres","year","rev_year"))
#################################################################################
## Create RMSE function to evaluate models
RMSE=function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}
#################################################################################
## Look at average of all the ratings from training set
(mu_hat=mean(edx5$rating))

## Look at RMSE by simply predicting each movie to have the mean rating (mu_hat)
(naive_rmse=RMSE(edx5$rating,mu_hat))

## Create table to store results from different modeling approaches
rmse_results=data_frame(method="Just the average",RMSE=naive_rmse)
#################################################################################

## Estimate average rating by movie as b_i (Movie Effects Model)
mu=mean(edx5$rating)
movie_avgs=edx5%>%
  group_by(movieId)%>%
  summarize(b_i=mean(rating-mu))

## Run predictions using movie effect model (b_i) calculated above
predicted_ratings=mu+edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  .$b_i

model_1_rmse=RMSE(predicted_ratings,edx5$rating)
rmse_results=bind_rows(rmse_results,
                       data_frame(method="Movie Effect Model",
                                  RMSE=model_1_rmse))

## Add model and RMSE to table
rmse_results%>%knitr::kable()
#################################################################################

## Estimate average rating by user as b_u (User Effect Model)
user_avgs=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  group_by(userId)%>%
  summarize(b_u=mean(rating-mu-b_i))

## Run predictions using movie effect model (b_i) and user effect model (b_u) calculated above
predicted_ratings=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  mutate(pred=mu+b_i+b_u)%>%
  .$pred

model_2_rmse=RMSE(predicted_ratings,edx5$rating)
rmse_results=bind_rows(rmse_results,
                       data_frame(method="Movie + User Effects Model",
                                  RMSE=model_2_rmse))

## Add model and RMSE to table
rmse_results%>%knitr::kable()
#################################################################################

## Estimate average rating by release year as b_rely (Rel Year Effect Model)
relyear_avgs=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  group_by(year)%>%
  summarize(b_rely=mean(rating-mu-b_i-b_u))

## Run predictions using movie effect model (b_i), user effect model (b_u), and release year (b_rely) calculated above
predicted_ratings=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  mutate(pred=mu+b_i+b_u+b_rely)%>%
  .$pred

model_3_rmse=RMSE(predicted_ratings,edx5$rating)
rmse_results=bind_rows(rmse_results,
                       data_frame(method="Movie + User + rel year Effects Model",
                                  RMSE=model_3_rmse))

## Add model and RMSE to table
rmse_results%>%knitr::kable()
#################################################################################

## Estimate average rating by review year as b_rev (Rev Year Effect Model)
revyear_avgs=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  group_by(rev_year)%>%
  summarize(b_revy=mean(rating-mu-b_i-b_u-b_rely))

## Run predictions using movie effect model (b_i), user effect model (b_u), 
## release year (b_rely), and review year (b_revy) calculated above
predicted_ratings=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  left_join(revyear_avgs,by='rev_year')%>%
  mutate(pred=mu+b_i+b_u+b_rely+b_revy)%>%
  .$pred

model_4_rmse=RMSE(predicted_ratings,edx5$rating)
rmse_results=bind_rows(rmse_results,
                       data_frame(method="Movie + User + rel year + rev year Effects Model",
                                  RMSE=model_4_rmse))

## Add model and RMSE to table
rmse_results%>%knitr::kable()
#################################################################################

## Estimate average rating by genre as b_gen (Genre Effect Model)
genre_avgs=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  left_join(revyear_avgs,by='rev_year')%>%
  group_by(genres)%>%
  summarize(b_gen=mean(rating-mu-b_i-b_u-b_rely-b_revy))

## Run predictions using movie effect model (b_i), user effect model (b_u), 
## release year (b_rely), review year (b_revy), and genre (b_gen) calculated above
predicted_ratings=edx5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  left_join(revyear_avgs,by='rev_year')%>%
  left_join(genre_avgs,by='genres')%>%
  mutate(pred=mu+b_i+b_u+b_rely+b_revy+b_gen)%>%
  .$pred

model_5_rmse=RMSE(predicted_ratings,edx5$rating)
rmse_results=bind_rows(rmse_results,
                       data_frame(method="Movie + User + rel year + rev year + genre Effects Model",
                                  RMSE=model_5_rmse))

## Add model and RMSE to table
rmse_results%>%knitr::kable()%>%kable_styling(full_width=F)

#################################################################################
## Movie + User + Release Year + Review Year + Genre model has lowest RMSE (0.854)
## Apply this model to the validation data set to assess accuracy
temp1=temp%>%mutate(year=substrRight(title,5))
temp2=temp1%>%mutate(year=substr(year,1,4),
                   year=as.integer(year))

## Separate genres
temp3=temp2%>%separate_rows(genres,convert=T)

## Convert timestamp to usable data
temp4=temp3%>%
  mutate(date=as_datetime(timestamp))

temp4$rev_year=year(temp4$date)

## Keep userId, movieId, rating, genres, year, and rev_year
temp5=temp4%>%
  select(c("userId","movieId","rating","genres","year","rev_year"))

##Predict ratings from full data set
predicted_ratings=temp5%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(relyear_avgs,by='year')%>%
  left_join(revyear_avgs,by='rev_year')%>%
  left_join(genre_avgs,by='genres')%>%
  mutate(pred=mu+b_i+b_u+b_rely+b_revy+b_gen)%>%
  .$pred

## Round to nearest integer because ratings are more likely to be integers than 0.5 increments
## See output from line 38
rounded_pred=round(predicted_ratings)

## Observe accuracy
acc=mean(rounded_pred==temp5$rating)