# Movielens-recommendation-system
#This repository contains the report and code of the capstone project of HarvardX’s Data Science Professional Certificate program.

Capstone project from Harvard Professional Data Science Certification course through edX. A recommender system from Netflix challenge to predict user's rating of a movie.

download-data.R : download movielens data from http://files.grouplens.org/datasets/movielens/ml-10m.zip

The aim in this project is to train a machine learning algorithm that predicts user ratings (from 0.5 to 5 stars) using the inputs of a provided subset (edx dataset provided by the staff) to predict movie ratings in a provided validation set.

The value used to evaluate algorithm performance is the Root Mean Square Error, or RMSE. RMSE is one of the most used measure of the differences between values predicted by a model and the values observed. RMSE is a measure of accuracy, to compare forecasting errors of different models for a particular dataset, a lower RMSE is better than a higher one. The effect of each error on RMSE is proportional to the size of the squared error; thus larger errors have a disproportionately large effect on RMSE. Consequently, RMSE is sensitive to outliers.
Four models that will be developed will be compared using their resulting RMSE in order to assess their quality. The evaluation criteria for this algorithm is a RMSE expected to be lower than 0.8775.
The function that computes the RMSE for vectors of ratings and their corresponding predictors will be the following:
$$ RMSE = \sqrt{\frac{1}{N}\displaystyle\sum_{u,i} (\hat{y}_{u,i}-y_{u,i})^{2}} $$
