```R
# global graphics options
options(repr.plot.width = 5, repr.plot.height = 4)
```


```R
# import necessary packages
library(ggplot2, warn.conflicts = FALSE)  # graphics
library(class, warn.conflicts = FALSE)  # knn
library(healthcareai, warn.conflicts = FALSE)  # train-test-split
library(randomForest, warn.conflicts = FALSE)  # feature importances
library(corrplot)  # cross-correlations
library(mlbench)  # data
```

# TASK. CLASSIFICATION part2
- Take same dataset as previously
- Build classification with random forest
- Compare results. Which method works better for you?



```R
# load data
data(Ionosphere)
ionosphere <- Ionosphere
ionosphere[is.na(ionosphere)] <- -1  # encode missing values with -1
```


```R
# train-test-split 7:3
set.seed(17)
ionosphere <- split_train_test(ionosphere, Class, percent_train = 0.7)

head(ionosphere$train[26:35], 2) %>%
    knitr::kable()
```


    
    
    |      V26|      V27|      V28|      V29|      V30|      V31|      V32|      V33|      V34|Class |
    |--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|:-----|
    | -0.51171|  0.41078| -0.46168|  0.21266| -0.34090|  0.42267| -0.54487|  0.18641| -0.45300|good  |
    | -0.26569| -0.20468| -0.18401| -0.19040| -0.11593| -0.16626| -0.06288| -0.13738| -0.02447|bad   |



```R
columns_to_use <- c("V5", "V8", "V27", "Class")
```


```R
# predict with Random Forest
rf <- randomForest(Class ~ ., data = ionosphere$train[, columns_to_use], importance = T,
    sampsize = 200, mtry = 2, ntree = 50, do.trace = 10)

pred_rf <- predict(rf, newdata = ionosphere$test[, columns_to_use])
table(pred_rf, Real = ionosphere$test$Class)
round(mean(pred_rf == ionosphere$test$Class) * 100, 2)
```

    ntree      OOB      1      2
       10:   8.10% 11.24%  6.33%
       20:   7.29%  8.99%  6.33%
       30:   8.10% 11.24%  6.33%
       40:   8.50% 11.24%  6.96%
       50:   8.50% 11.24%  6.96%
    


           Real
    pred_rf bad good
       bad   32    6
       good   5   61



89.42


# Result

Earlier, K-nearest neighbors performed better on test data than Logistic regression (~88% acc vs. ~81% acc) using the same characteristics for both models and a balanced, stratified test sample, but Random forest on the same data increased in accuracy to ~90%, which is better than both models above. 
