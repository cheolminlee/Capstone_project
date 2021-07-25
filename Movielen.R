# Movielens Project
# Cheol Min Lee
# 7/25/2021


# Create test and validation sets
# Create edx set, validation set, and submission file
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(simtimer)) install.packages("simtimer", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(dplyr)
library(stringr)
library(caret)
library(simtimer)
library(lubridate)
library(ggplot2)
library(tidyr)
library(corrplot)
library(RColorBrewer)
library(hexbin)
library(Metrics)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(2021)
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
head(edx)

# How many distinct movie, users and genres in movielens data
n_distinct(edx$movieId)
n_distinct(edx$genres)
n_distinct(edx$userId)


#**Extract the premier date and calculate the age of the move **
# Change from Timestamp to year
edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))
head(edx)

# Extract the premier date
premier <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()

#Add the premier date
edx_with_title_dates <- edx %>% mutate(premier_date = premier)
head(edx_with_title_dates)

#drop the timestamp
edx_with_title_dates <- edx_with_title_dates %>% select(-timestamp)
head(edx_with_title_dates)

#looking at the dates - are they correct?
edx_with_title_dates %>% filter(premier_date > 2021) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

edx_with_title_dates[edx_with_title_dates$movieId == "671", "premier_date"] <- 1996
edx_with_title_dates[edx_with_title_dates$movieId == "2308", "premier_date"] <- 1973
edx_with_title_dates[edx_with_title_dates$movieId == "4159", "premier_date"] <- 2001
edx_with_title_dates[edx_with_title_dates$movieId == "5310", "premier_date"] <- 1985
edx_with_title_dates[edx_with_title_dates$movieId == "8864", "premier_date"] <- 2004
edx_with_title_dates[edx_with_title_dates$movieId == "27266", "premier_date"] <- 2004

edx_with_title_dates %>% filter(premier_date < 1900) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

edx_with_title_dates[edx_with_title_dates$movieId == "1422", "premier_date"] <- 1997
edx_with_title_dates[edx_with_title_dates$movieId == "4311", "premier_date"] <- 1998
edx_with_title_dates[edx_with_title_dates$movieId == "5472", "premier_date"] <- 1972
edx_with_title_dates[edx_with_title_dates$movieId == "6290", "premier_date"] <- 2003
edx_with_title_dates[edx_with_title_dates$movieId == "6645", "premier_date"] <- 1971
edx_with_title_dates[edx_with_title_dates$movieId == "8198", "premier_date"] <- 1960
edx_with_title_dates[edx_with_title_dates$movieId == "8905", "premier_date"] <- 1992
edx_with_title_dates[edx_with_title_dates$movieId == "53953", "premier_date"] <- 2007

# **Extract the premier date and calculate the age of the move **
# Change from Timestamp to year
edx <- mutate(edx, year_rated = year(as_datetime(timestamp)))
head(edx)

# Extract the premier date
premier <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()

# Add the premier date
edx_with_title_dates <- edx %>% mutate(premier_date = premier)
head(edx_with_title_dates)

# Drop the timestamp
edx_with_title_dates <- edx_with_title_dates %>% select(-timestamp)
head(edx_with_title_dates)

# Check the dates 
edx_with_title_dates %>% filter(premier_date > 2021) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

edx_with_title_dates[edx_with_title_dates$movieId == "671", "premier_date"] <- 1996
edx_with_title_dates[edx_with_title_dates$movieId == "2308", "premier_date"] <- 1973
edx_with_title_dates[edx_with_title_dates$movieId == "4159", "premier_date"] <- 2001
edx_with_title_dates[edx_with_title_dates$movieId == "5310", "premier_date"] <- 1985
edx_with_title_dates[edx_with_title_dates$movieId == "8864", "premier_date"] <- 2004
edx_with_title_dates[edx_with_title_dates$movieId == "27266", "premier_date"] <- 2004

edx_with_title_dates %>% filter(premier_date < 1900) %>% group_by(movieId, title, premier_date) %>% summarize(n = n())

edx_with_title_dates[edx_with_title_dates$movieId == "1422", "premier_date"] <- 1997
edx_with_title_dates[edx_with_title_dates$movieId == "4311", "premier_date"] <- 1998
edx_with_title_dates[edx_with_title_dates$movieId == "5472", "premier_date"] <- 1972
edx_with_title_dates[edx_with_title_dates$movieId == "6290", "premier_date"] <- 2003
edx_with_title_dates[edx_with_title_dates$movieId == "6645", "premier_date"] <- 1971
edx_with_title_dates[edx_with_title_dates$movieId == "8198", "premier_date"] <- 1960
edx_with_title_dates[edx_with_title_dates$movieId == "8905", "premier_date"] <- 1992
edx_with_title_dates[edx_with_title_dates$movieId == "53953", "premier_date"] <- 2007

# Calculate the age of a movie
edx_with_title_dates <- edx_with_title_dates %>% mutate(age_of_movie = 2021 - premier_date, 
                                                        rating_date_range = year_rated - premier_date)
head(edx_with_title_dates)

#**Make graphes based on Movielens data**
# Make graphes based on Movielens data
Plot1<- edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "cadetblue2", color = "grey20", bins = 20) +
  labs(y = "Number of Movie", x = "Rating")+
  scale_x_log10() 

# Make Distribution of Users
Plot2<- edx %>% group_by(userId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "71b7begreen", color = "grey20", bins = 20) +
  labs(y = "Number of User", x = "Rating")+
  scale_x_log10()  

# Make Figure 1 with Plot1 and Plot2
grid.arrange(Plot1, Plot2, nrow=1, ncol=2)
#**Figure 1. Distribution of movie rating and distribution of user rating in Movielens data.**
  
  
#**Calculate movie rating average, user rating average, average rating by age of movie, average rating by year**
movie_avgs <- edx_with_title_dates %>% group_by(movieId) %>% summarize(avg_movie_rating = mean(rating))
user_avgs <- edx_with_title_dates %>% group_by(userId) %>% summarize(avg_user_rating = mean(rating))
year_avgs <- edx_with_title_dates%>% group_by(year_rated) %>% summarize(avg_rating_by_year = mean(rating)) 
age_avgs <- edx_with_title_dates %>% group_by(age_of_movie) %>% summarize(avg_rating_by_age = mean(rating)) 
head(age_avgs)
head(user_avgs)

# Age of movie vs average movie rating
Plot3 <- age_avgs %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(alpha = 1, colour = "#99CC00") +
  ggtitle("a. Age of a Movie vs Average Movie Rating")

# UserId vs average movie rating
Plot4 <- user_avgs %>%
  ggplot(aes(userId, avg_user_rating)) +
  geom_point(alpha = 1/10, colour = "#FFCC33") +
  ggtitle("b. User vs Average User Rating")


# Test the linear models
summary(lm(avg_rating_by_age ~ age_of_movie, data = age_avgs))
summary(lm(avg_user_rating~ userId, data = user_avgs))


# Make Figure 2 with Plot3 and Plot4
grid.arrange(Plot3, Plot4, nrow=1, ncol=2)
#**Figure 2. Relationship between age of movie (a) and average rating by age and user and average user rating (b).**

#**Relationship between age of movie and average rating by age**
# Movies less than 60 years old
age_of_movie_less_than60 <- age_avgs %>% filter(age_of_movie <60)
Plot5 <- age_of_movie_less_than60 %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(alpha = 1, colour = "#FF6633", size=5) +
  ggtitle("a. Less than 60 years old")


# Movies between 30 and 60 years old
age_between30_and_60 <- age_avgs %>% filter((age_of_movie > 30) & (age_of_movie < 60))
Plot6 <- age_between30_and_60 %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(alpha = 1, colour = "#CC0033", size=5) +
  ggtitle("b. Between 30 and 60 years old")


# Movies between 10 and 50 years old
age_between10_and_50 <- age_avgs %>% filter((age_of_movie > 10) & (age_of_movie < 50))
Plot7 <- age_between10_and_50 %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(alpha = 1, colour = "#3399FF", size=5) +
  ggtitle('c. Between 10 and 50 years old')


# Movies movie between 20 and 40 years old
age_between20_and_40 <- age_avgs %>% filter((age_of_movie > 20) & (age_of_movie < 40))
Plot8 <- age_between20_and_40 %>%
  ggplot(aes(age_of_movie, avg_rating_by_age)) +
  geom_point(alpha = 1, colour = "#CC00CC", size=5) +
  ggtitle('d. Between 10 and 40 years old')


summary(lm(avg_rating_by_age ~ age_of_movie, data = age_of_movie_less_than60)) #The R-squared is 0.6812
summary(lm(avg_rating_by_age ~ age_of_movie, data = age_between30_and_60)) #The R-squared is 0.6291
summary(lm(avg_rating_by_age ~ age_of_movie, data = age_between10_and_50)) #The R-squared is 0.5566
summary(lm(avg_rating_by_age ~ age_of_movie, data = age_between20_and_40)) #The R-squared is 0.5188

# Make Figure 3 with Plot5, Plot6, Plot7, and Plot8
grid.arrange(Plot5, Plot6, Plot7, Plot8, nrow=2, ncol=2)
#**Figure 3. Relationship average rating by age and age of movie.**
  
#**Genres's effect**
# Split Genres data into single genres
dat <- edx_with_title_dates %>% separate_rows(genres, sep ="\\|")
head(dat)


# Distribution of Ratings according to genres
temp <- dat %>%
  group_by(genres) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n/sumN) %>%
  arrange(-percentage)

# Make bar graph of genres
Plot9 <- temp %>%
  ggplot(aes(reorder(genres, percentage), percentage, fill= percentage)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Spectral") + labs(y = "Percentage", x = "Genre") +
  ggtitle("a. Distribution of Genres by Percent")

# Make genres Mean rating
temp <- dat %>%
  group_by(genres) %>%
  summarize(mean_rating_by_genre=mean(rating)) %>%
  arrange(-mean_rating_by_genre)

Plot10 <- temp %>%
  ggplot(aes(reorder(genres, mean_rating_by_genre), mean_rating_by_genre, fill= mean_rating_by_genre)) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_distiller(palette = "Spectral") + labs(y = "Mean Rating", x = "Genre") +
  ggtitle("b. Average Rating of Genres")

# Make Figure 4 with Plot9 and Plot10
grid.arrange(Plot9, Plot10, nrow=2, ncol=1)
#**Figure 4. Distribution of genres by percentage (a) and average rating of genres.**
  
  
#**Prepare correlation analysis**
# Make number of movie ratings according to movie
n_movies_ratings <- edx_with_title_dates %>% group_by(movieId) %>% summarize(n = n())

# Make Average Movie Rating for each movie
avg_movie_rat <- edx_with_title_dates %>% group_by(movieId) %>% summarize(avg_m_r = mean(rating))

# Make correlation data
cor_dat <- edx_with_title_dates %>% select(rating, movieId, userId, year_rated, age_of_movie, rating_date_range, premier_date) %>%
  left_join(n_movies_ratings, by = "movieId") %>%
  left_join(avg_movie_rat, by = 'movieId')
head(cor_dat)


# Test pearson correlation analysis 
temp <- cor_dat %>% select(one_of("rating", "movieId", "userId", "year_rated", "age_of_movie", 
                                  "rating_date_range", "premier_date", "n", "avg_m_r")) %>% as.matrix()
M <- cor(temp)
testRes = cor.mtest(temp, conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'square', type = 'lower', insig='blank',tl.col = 'black',
         addCoef.col ='black', number.cex = 0.8, order = 'hclust', diag=FALSE, col = brewer.pal(n = 10, name = "Spectral"))

#**Figure 5. Correlation between ranting, movieID, year rated, age of movie, rating date range, premier data, number of movies ratigs, average movie rate.**
  
  
#**Calculate the RMSE**
# Use RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# lambda is a tuning parameter
lambdas <- seq(0,5,.5)

# For each lambda,find b_i & b_u, followed by rating prediction & testing
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_with_title_dates$rating)
  
  b_i <- edx_with_title_dates %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx_with_title_dates %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
  predicted_ratings <- edx_with_title_dates %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, edx_with_title_dates$rating))
})

# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses, geom=c("point","line"))

lambdas[which.min(rmses)]

# Use the model on the Validation data
mu <- mean(validation$rating)
l <- 0.15

b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_ratings, validation$rating)
