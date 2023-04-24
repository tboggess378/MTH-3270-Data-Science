# Authors: TObias Boggess, Carl Perry
# Date: May 04, 2022
# Description: Final Project

################################################################################
#                                 Saving Data                                  #
################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(yardstick)
library(kknn)
library(scales)
library(mclust)
# Load in the Racial and Ethnic Representativeness of US Postsecondary 
# Education Institutions data set. 
my.file <- file.choose()
fryr.cllg <-
  read.csv(file = my.file,
           header = TRUE,
           sep = ",",
           stringsAsFactors = FALSE)

# Filter out all other years besides 2017
fryr.cllg <- filter(fryr.cllg, year == 2017)

# Load in supplemental 2017 data set
my.file1 <- file.choose()
sup.2017 <-
  read.csv(
    file = my.file1,
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

# Joint data sets of fryr.cllg and sup.2017
comb.fryr <- left_join(
  x = fryr.cllg,
  y = sup.2017,
  by = c("unitid", "inst_name" = "institution.name", "year")
)
# Splitting data set into train and test data sets
set.seed(54)
temp <- sort(sample(nrow(comb.fryr), nrow(comb.fryr)*.75))
comb.fryr.train <- comb.fryr[temp,]
comb.fryr.test <- comb.fryr[-temp,]

################################################################################
#                                    Task One                                  #
################################################################################


################################# Decision Trees ###############################
# Decision tree with minsplit of 10
set.seed(34)
tree.comb <- rpart(fourcat ~ DRVEF2017_RV.Undergraduate.enrollment +
                     DRVIC2017.Tuition.and.fees..2016.17 +
                     total_enrollment,
                   data = comb.fryr.train,
                   control = rpart.control(minsplit = 10))

# Creates the predictions based on the decision tree above
comb.preds <- predict(tree.comb, newdata = comb.fryr.test, type = "class")
# comb.preds

# Adding predictions to data frame comb.fryr.test
comb.fryr.test1 <- mutate(comb.fryr.test, predType = comb.preds)

# Determines the accuracy of the decision tree above
accuracy(
  data = comb.fryr.test1,
  truth = as.factor(fourcat),
  estimate = as.factor(predType)
)


# Decision tree with minsplit of 100
set.seed(12)
tree.comb1 <- rpart(fourcat ~ DRVEF2017_RV.Undergraduate.enrollment +
                      DRVIC2017.Tuition.and.fees..2016.17 +
                      total_enrollment,
                    data = comb.fryr.train,
                    control = rpart.control(minsplit = 100))

# Creates the predictions based on the decision tree above
comb.preds1 <- predict(tree.comb1, newdata = comb.fryr.test, type = "class")
# comb.preds1

# Adding predictions to data frame comb.fryr.test
comb.fryr.test2 <- mutate(comb.fryr.test, predType = comb.preds1)

# Determines the accuracy of the decision tree above
accuracy(
  data = comb.fryr.test2,
  truth = as.factor(fourcat),
  estimate = as.factor(predType)
)


# Decision tree with minsplit of 300 
set.seed(98)
tree.comb2 <- rpart(fourcat ~ DRVEF2017_RV.Undergraduate.enrollment +
                      DRVIC2017.Tuition.and.fees..2016.17 +
                      total_enrollment,
                    data = comb.fryr.train,
                    control = rpart.control(minsplit = 300))

# Creates the predictions based on the decision tree above
comb.preds2 <- predict(tree.comb2, newdata = comb.fryr.test, type = "class")
# comb.preds1

# Adding predictions to data frame comb.fryr.test
comb.fryr.test3 <- mutate(comb.fryr.test, predType = comb.preds2)

# Determines the accuracy of the decision tree above
accuracy(
  data = comb.fryr.test3,
  truth = as.factor(fourcat),
  estimate = as.factor(predType)
)


############################### K Nearest Neighbors ############################
comb.fryr.knn.train <-
  filter(comb.fryr.train,
         !is.na(comb.fryr.train$DRVEF2017_RV.Undergraduate.enrollment) & 
           !is.na(comb.fryr.train$DRVIC2017.Tuition.and.fees..2016.17))

comb.fryr.knn.test <- 
  filter(comb.fryr.train,
         !is.na(comb.fryr.train$DRVEF2017_RV.Undergraduate.enrollment) &
           !is.na(comb.fryr.train$DRVIC2017.Tuition.and.fees..2016.17))

# K nearest neighbor with tuning parameter k = 15
set.seed(102)
knn.comb <- kknn(as.factor(fourcat) ~ DRVEF2017_RV.Undergraduate.enrollment +
                   DRVIC2017.Tuition.and.fees..2016.17 +
                   total_enrollment,
                 train = comb.fryr.knn.train,
                 test = comb.fryr.knn.test,
                 k = 15)

# Predictions to be added to data frame in test data set
knn.comb.preds <- fitted(knn.comb)
comb.fryr.test4 <- mutate(comb.fryr.knn.test, predFourcat = knn.comb.preds)

# Accuracy of k nearest neighbor model above
accuracy(comb.fryr.test4,
         truth = as.factor(fourcat),
         estimate = as.factor(predFourcat))


# K nearest neighbor with tuning parameter k = 60
set.seed(103)
knn.comb1 <- kknn(as.factor(fourcat) ~ DRVEF2017_RV.Undergraduate.enrollment +
                    DRVIC2017.Tuition.and.fees..2016.17 +
                    total_enrollment,
                  train = comb.fryr.knn.train,
                  test = comb.fryr.knn.test,
                  k = 60)

# Predictions to be added to data frame in test data set
knn.comb.preds <- fitted(knn.comb1)
comb.fryr.test5 <- mutate(comb.fryr.knn.test, predFourcat = knn.comb.preds)

# Accuracy of k nearest neighbor model above
accuracy(comb.fryr.test5,
         truth = as.factor(fourcat),
         estimate = as.factor(predFourcat))


# K nearest neighbor with tuning parameter k = 100
set.seed(104)
knn.comb2 <- kknn(as.factor(fourcat) ~ DRVEF2017_RV.Undergraduate.enrollment +
                    DRVIC2017.Tuition.and.fees..2016.17 +
                    total_enrollment,
                  train = comb.fryr.knn.train,
                  test = comb.fryr.knn.test,
                  k = 100)

# Predictions to be added to data frame in test data set
knn.comb.preds <- fitted(knn.comb2)
comb.fryr.test6 <- mutate(comb.fryr.knn.test, predFourcat = knn.comb.preds)

# Accuracy of k nearest neighbor model above
accuracy(comb.fryr.test6,
         truth = as.factor(fourcat),
         estimate = as.factor(predFourcat))


################################################################################
#                                    Task Two                                  #
################################################################################

# K cluster using DRVEF2017_RV.Undergraduate.enrollment, 
# EF2017D_RV.Student.to.faculty.ratio, total_enrollment, 
# DRVGR2017_RV.Graduation.rate.total.cohort, and
# col_white

# Selecting columns to use in cluster
fryr.clust <-
  select(comb.fryr,
         DRVEF2017_RV.Undergraduate.enrollment,
         EF2017D_RV.Student.to.faculty.ratio,
         total_enrollment,
         DRVGR2017_RV.Graduation.rate..total.cohort,
         col_white)

# Rescaling each column to fit on same scale
fryr.clust$DRVEF2017_RV.Undergraduate.enrollment <-
  rescale(x = fryr.clust$DRVEF2017_RV.Undergraduate.enrollment,
          to = c(0, 1), 
          from = range(fryr.clust$DRVEF2017_RV.Undergraduate.enrollment, 
                       na.rm = TRUE, finite = TRUE))

fryr.clust$EF2017D_RV.Student.to.faculty.ratio <-
  rescale(x = fryr.clust$EF2017D_RV.Student.to.faculty.ratio,
          to = c(0, 1), 
          from = range(fryr.clust$EF2017D_RV.Student.to.faculty.ratio, 
                       na.rm = TRUE, finite = TRUE))

fryr.clust$total_enrollment <-
  rescale(x = fryr.clust$total_enrollment,
          to = c(0, 1), 
          from = range(fryr.clust$total_enrollment, 
                       na.rm = TRUE, finite = TRUE))

fryr.clust$DRVGR2017_RV.Graduation.rate..total.cohort <-
  rescale(x = fryr.clust$DRVGR2017_RV.Graduation.rate..total.cohort,
          to = c(0, 1), 
          from = range(fryr.clust$DRVGR2017_RV.Graduation.rate..total.cohort, 
                       na.rm = TRUE, finite = TRUE))

fryr.clust$col_white <-
  rescale(x = fryr.clust$col_white,
          to = c(0, 1), 
          from = range(fryr.clust$col_white, 
                       na.rm = TRUE, finite = TRUE))


# Clustering variables to try to fit fourcat in normal data set --> comb.fryr
# Using k = 5
set.seed(675)
fryr_kmclust <- kmeans(na.omit(fryr.clust), centers = 5)
fryr_kmclust
