if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
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

#Validation

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in 'validation' set are also in 'edx' set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from 'validation' set back into 'edx' set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# create the train set and test set

# split the 'edx' set in 2 parts: the training set and the test set. 

set.seed(1234, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

# Data exploration
str(edx)


dim(edx)

head(edx)

# edx set structure
# movieId: integer
# userId : integer
# rating: numeric
# timestamp: numeric
# title: character
# genres: character
length(unique(edx$genres))

# View the first 6 genres
edx %>% group_by(genres) %>% 
  summarise(n=n()) %>%
  head()

# Count the number of different genres for each movie
tibble(cnt = str_count(train_set$genres, fixed("|")), 
       genres = train_set$genres) %>% 
  group_by(cnt, genres) %>%
  summarise(n = n()) %>%
  arrange(-cnt) %>% 
  head()

# Create a vector of unique genres 

# First, transform this data to matrix and perform  dimension reduction 
#train <- as.matrix(edx)
#test  <- as.matrix(validation)
#head(edx)

# Separate the predictors and outcomes. 
#foo <- list(x = (edx[,-3]), rating = edx[,3])
#class(foo$x)

#edx %>% group_by(movieId) %>% summarize(n=n()) %>% count()
#edx %>% group_by(title) %>% summarize(n=n()) %>% count()
#edx %>% distinct(movieId) %>% count()
#edx %>% distinct(title) %>% count()
length(unique(edx$rating))
# Date explorationg

# Convert timestamp into date format
library(lubridate)
edx <- mutate(edx, date = as_datetime(timestamp))

# Check the range period of ratings
tibble(`Initial Date` = date(as_datetime(min(edx$timestamp), origin="1970-01-01")),
       `Final Date` = date(as_datetime(max(edx$timestamp), origin="1970-01-01"))) %>%
  mutate(Period = duration(max(edx$timestamp)-min(edx$timestamp)))

# Plot histogram of rating distribution over the years
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

# Dates with more ratings
edx %>% mutate(date = date(as_datetime(timestamp, origin="1970-01-01"))) %>%
  group_by(date, title) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  head(10)


# Ratings exploration

edx %>% group_by(rating) %>% summarize(n=n())

# Chart with distribution of each rating
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

# How many different movies are in the 'edx' set?
length(unique(edx$movieId))

# Distribution of movies
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bin = 30, color = "white") +
  scale_x_log10() + 
  ggtitle("Movies")


# Distribution of users
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  head()

# How many different users are in the 'edx' set?
length(unique(edx$userId))

# Distribution of users rating movies
edx %>% group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(bin = 30, color = "white") +
  scale_x_log10() + 
  ggtitle("Users")

#the heat map of users x movies
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

##  Data Cleaning

train_set <- train_set %>% select(userId, movieId, rating, title)
test_set <- test_set %>% select(userId, movieId, rating, title)


# Define RMSE, MSE and MAE
# (MAE)
MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}
#(MSE)
MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings)^2)
}
# (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Random Prediction

set.seed(4321, sample.kind = "Rounding")

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
                           MSE = MSE(test_set$rating, y_hat_random),
                           MAE = MAE(test_set$rating, y_hat_random)))

result %>% knitr::kable()


# Linear Model

# We're building the linear model based on the formula:
# y_hat = mu + bi + bu + epsilon u,i

# The initial prediction is the mean of the ratings (mu).
# y_hat = mu
mu <- mean(train_set$rating)
# Update the error table
result <- bind_rows(result,
                    tibble(Method = "Mean",
                           RMSE = RMSE(test_set$rating, mu),
                           MSE = MSE(test_set$rating, mu),
                           MAE = MAE(test_set$rating, mu)))
# Show the RMSE improvement
result %>% knitr::kable()

# 2. Include movie effect (bi)
# y_hat = mu + bi
# Movie effects (bi)
bi <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
head(bi)
# Plot the distribution of movie effects
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
                           MSE = MSE(test_set$rating, y_hat_bi),
                           MAE = MAE(test_set$rating, y_hat_bi)))

# Show the RMSE improvement
result %>% knitr::kable()


# Include user effect (bu)
# y_hat = mu + bi + bu
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
                           MSE = MSE(test_set$rating, y_hat_bi_bu),
                           MAE = MAE(test_set$rating, y_hat_bi_bu)))
# Show the RMSE improvement
result %>% knitr::kable()

# Plot the distribution of user effects
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


# Checking the model result

train_set %>% 
  left_join(bi, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10)

titles <- train_set %>% 
  select(movieId, title) %>% 
  distinct()

# Top 10 best movies (ranked by bi).

bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(-b_i) %>% 
  head() %>%
  pull(title)

# Top 10 worst movies (ranked by bi):

bi %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(b_i) %>% 
  head() %>%
  pull(title)

# Number of ratings for 10 best movies:
train_set %>% 
  left_join(bi, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  slice(1:10)

train_set %>% count(movieId) %>% 
  left_join(bi, by="movieId") %>% 
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

# Regularization

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

lambdas <- seq(0, 10, 0.25)
# Update RMSES table
rmses <- sapply(lambdas,
                regularization,
                trainset = train_set,
                testset = test_set)
# Plot the lambda x RMSE
tibble(Lambda = lambdas, RMSE = rmses) %>%
  ggplot(aes(x = Lambda, y = RMSE)) +
  geom_point() +
  ggtitle("Regularization",
          subtitle = "Pick the penalization that gives the lowest RMSE.") +
  theme_economist()


lambda <- lambdas[which.min(rmses)]
lambda


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
                           MSE = MSE(test_set$rating, y_hat_reg),
                           MAE = MAE(test_set$rating, y_hat_reg)))
# Show the RMSE improvement
result %>% knitr::kable()

# Matrix Factorization with recosystem

if(!require(recosystem))
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
set.seed(123, sample.kind = "Rounding") # This is a randomized algorithm
# Convert the train and test sets into recosystem input format
train_data <- with(train_set, data_memory(user_index = userId,
                                          item_index = movieId,
                                          rating = rating))
test_data <- with(test_set, data_memory(user_index = userId,
                                        item_index = movieId,
                                        rating = rating))
# Create the model object
r <- recosystem::Reco()

# Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30),
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1),
                                       costq_l2 = c(0.01, 0.1),
                                       nthread = 4, niter = 10))
# Train the algorithm
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

# Calculate the predicted values
y_hat_reco <- r$predict(test_data, out_memory())
head(y_hat_reco, 10)


# Testing recommenderlab
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
head(as(Jester5k, "data.frame"))

train_s <- with(edx, data.frame(user = userId, item = title, rating = rating))
train_s <- as(train_s, "realRatingMatrix")
train_s

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
r <- Recommender(train_s[1:62890], method = "POPULAR")
names(recommenderlab::getModel(r))
getModel(r)$topN

recom <- recommenderlab::predict(r, train_s[62891:69878], type="ratings")
y_hat_reclab <- as(recom, "data.frame")

# Evaluation of predicted ratings

set.seed(123, sample.kind="Rounding")

e <- evaluationScheme(train_s, method="split", train=0.9,
                      given=-1, goodRating=4)
e

r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2

p1 <- predict(r1, getData(e, "known"), type="ratings")
p1

p2 <- predict(r2, getData(e, "known"), type="ratings")
p2

error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error

# Testing other ML algorithms

# Train KNN

fit <- train(rating ~ .,
             method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = train_set)
# Unable to train KNN. This code returns an error

# Train random forest

fit <- train(rating ~ .,
             method = "rf", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = train_set)
# Unable to train RF. This code returns an error
# random forest
library(randomForest)
train_rf <- randomForest(rating ~ ., data = train_set)

# Unable to use Random forest. This code returns an error

#Use regression trees (rpart)

library(rpart)
fit_rpart <- rpart(rating ~ userId + movieId, data = train_set)
y_hat_rpart = predict(fit_rpart, test_set)
             
result <- bind_rows(result,
                    tibble(Method = "Regression tree - rpart",
                           RMSE = RMSE(test_set$rating, y_hat_rpart),
                           MSE = MSE(test_set$rating, y_hat_rpart),
                           MAE = MAE(test_set$rating, y_hat_rpart)))

# Show the RMSE 
result %>% knitr::kable()

# K nearest neighours - knn

# Calculating the knn runs fast, but the prediction

fit_knn <- knn3(rating ~ userId + movieId, data = train_set)
y_hat_knn <- predict(fit_knn, test_set)

result <- bind_rows(result,
                    tibble(Method = "knn",
                           RMSE = RMSE(test_set$rating, y_hat_knn),
                           MSE = MSE(test_set$rating, y_hat_knn),
                           MAE = MAE(test_set$rating, y_hat_knn)))

# Show the RMSE 
result %>% knitr::kable()

# The RMSE of knn is very high (3.58)
head(y_hat_knn)
class(y_hat_knn)

ratings <- as.numeric(dimnames(y_hat_knn)[[2]])
y_hat_knn_hp <- sapply(1:nrow(y_hat_knn),
                         function(x) ratings[which.max(y_hat_knn[x,])]) 

head(y_hat_knn_hp)

result <- bind_rows(result,
                    tibble(Method = "Knn high prob (hp)",
                           RMSE = RMSE(test_set$rating, y_hat_knn_hp),
                           MSE = MSE(test_set$rating, y_hat_knn_hp),
                           MAE = MAE(test_set$rating, y_hat_knn_hp))) 
result %>% knitr::kable()

# RMSE improved substantially (dropped to 1.35)
y_hat_knn_wa <- sapply(1:length(ratings),
                        function(x) ratings[x]*y_hat_knn[,x]) %>% 
                  rowSums()

head(y_hat_knn_wa)

result <- bind_rows(result,
                    tibble(Method = "Knn weighted average (wa)",
                           RMSE = RMSE(test_set$rating, y_hat_knn_wa),
                           MSE = MSE(test_set$rating, y_hat_knn_wa),
                           MAE = MAE(test_set$rating, y_hat_knn_wa)))

result %>% knitr::kable()

# The RMSE reduced to 1.08, but it's still higher than the mean.

# Dimension reduction (PCA / SVD)

# The dataset is very large, but there's only 5 predictors.
# So, dimension reduction won't be very useful here.

pca <- prcomp(train_set)


# Ensemble

# Ensemble 1: regularization and recosystem
y_reg_reco <- tibble(regularization = y_hat_reg, 
                     recosys = y_hat_recon) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM Reg + recosystem",
                           RMSE = RMSE(test_set$rating, y_reg_reco),
                           MSE = MSE(test_set$rating, y_reg_reco),
                           MAE = MAE(test_set$rating, y_reg_reco)))

# Ensemble 2: regularization, rpart and knn highest probability
y_reg_rpart_knn_hp <- tibble(regularization = y_hat_reg, 
                             rpart = y_hat_rpart, 
                             knn = y_hat_knn_hp) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM Reg + rpart + knn",
                           RMSE = RMSE(test_set$rating, y_reg_rpart_knn_hp),
                           MSE = MSE(test_set$rating, y_reg_rpart_knn_hp),
                           MAE = MAE(test_set$rating, y_reg_rpart_knn_hp)))

# Ensemble 3: regularization and rpart
y_reg_rpart <- tibble(regularization = y_hat_reg, 
                      rpart = y_hat_rpart) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM reg + rpart",
                           RMSE = RMSE(test_set$rating, y_reg_rpart),
                           MSE = MSE(test_set$rating, y_reg_rpart),
                           MAE = MAE(test_set$rating, y_reg_rpart)))

# Ensemble 4: regularization and knn highest probability
y_reg_knn_hp <- tibble(regularization = y_hat_reg, 
                   knn = y_hat_knn_hp) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM reg + knn hp",
                           RMSE = RMSE(test_set$rating, y_reg_knn_hp),
                           MSE = MSE(test_set$rating, y_reg_knn_hp),
                           MAE = MAE(test_set$rating, y_reg_knn_hp)))

# Ensemble 5: regularization, rpart and knn weighted average
y_reg_rpart_knn_wa <- tibble(regularization = y_hat_reg, 
                   rpart = y_hat_rpart, 
                   knn = y_hat_knn_wa) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM reg + rpart + knn wa",
                           RMSE = RMSE(test_set$rating, y_reg_rpart_knn_wa),
                           MSE = MSE(test_set$rating, y_reg_rpart_knn_wa),
                           MAE = MAE(test_set$rating, y_reg_rpart_knn_wa)))

# Ensemble 6: regularization and knn weighted average
y_reg_knn_wa <- tibble(regularization = y_hat_reg, 
                    knn = y_hat_knn_wa) %>% rowMeans()

result <- bind_rows(result,
                    tibble(Method = "LM reg + knn wa",
                           RMSE = RMSE(test_set$rating, y_reg_knn_wa),
                           MSE = MSE(test_set$rating, y_reg_knn_wa),
                           MAE = MAE(test_set$rating, y_reg_knn_wa)))
# Show the RMSE 
result %>% knitr::kable()


# Final validation Linear Model with Regularization.

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
                           MSE = MSE(validation$rating, y_hat_edx),
                           MAE = MAE(validation$rating, y_hat_edx)))

result %>% knitr::kable()

# As expeted, the RMSE calculated on the 'validation' set  is slightly more than the value from the test set.


# Top 10 best movies
validation %>%
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  arrange(-pred) %>%
  group_by(title) %>%
  select(title) %>%
  head(10)

# Top 10 worst movies
validation %>%
  left_join(b_i_edx, by = "movieId") %>%
  left_join(b_u_edx, by = "userId") %>%
  mutate(pred = mu_edx + b_i + b_u) %>%
  arrange(pred) %>%
  group_by(title) %>%
  select(title) %>%
  head(10)

# Final validation with Matrix Factorization - recosystem

set.seed(1234, sample.kind = "Rounding")

edx_reco <- with(edx, data_memory(user_index = userId,
                                  item_index = movieId,
                                  rating = rating))
validation_reco <- with(validation, data_memory(user_index = userId,
                                                item_index = movieId,
                                                rating = rating))

r <- recosystem::Reco()
# Tune the parameters
opts <- r$tune(edx_reco, opts = list(dim = c(10, 20, 30),
                                     lrate = c(0.1, 0.2),
                                     costp_l2 = c(0.01, 0.1),
                                     costq_l2 = c(0.01, 0.1),
                                     nthread = 4, niter = 10))
# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))

# Calculate the prediction
y_hat_final_reco <- r$predict(validation_reco, out_memory())
# Update the result table
result <- bind_rows(result,
                    tibble(Method = "Final Matrix Factorization - recosystem",
                           RMSE = RMSE(validation$rating, y_hat_final_reco),
                           MSE = MSE(validation$rating, y_hat_final_reco),
                           MAE = MAE(validation$rating, y_hat_final_reco)))

# Show the RMSE improvement
result %>% knitr::kable()

# Top 10 best movies:
tibble(title = validation$title, rating = y_hat_final_reco) %>%
  arrange(-rating) %>%
  group_by(title) %>%
  select(title) %>%
  head(10)

# Top 10 worst movies:
tibble(title = validation$title, rating = y_hat_final_reco) %>%
  arrange(rating) %>%
  group_by(title) %>%
  select(title) %>%
  head(10)
