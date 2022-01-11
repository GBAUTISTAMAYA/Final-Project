# Gildardo Bautista-Maya
# Applied Multivariate Stats
# Professor Ping Ye

# Call the ISLR library and check the head of College (a built-in data frame with ISLR, use data() to check this.) 
# Then reassign College to a dataframe called df.
install.packages('ISLR')
library('ISLR')
head(College)
df <- College

# Check the head, structure and summary of the df.
head(df)
#                              Private Apps Accept Enroll Top10perc Top25perc F.Undergrad P.Undergrad Outstate Room.Board Books Personal PhD Terminal S.F.Ratio perc.alumni Expend Grad.Rate
# Abilene Christian University     Yes 1660   1232    721        23        52        2885         537     7440       3300   450     2200  70       78      18.1          12   7041        60
# Adelphi University               Yes 2186   1924    512        16        29        2683        1227    12280       6450   750     1500  29       30      12.2          16  10527        56
# Adrian College                   Yes 1428   1097    336        22        50        1036          99    11250       3750   400     1165  53       66      12.9          30   8735        54
# Agnes Scott College              Yes  417    349    137        60        89         510          63    12960       5450   450      875  92       97       7.7          37  19016        59
# Alaska Pacific University        Yes  193    146     55        16        44         249         869     7560       4120   800     1500  76       72      11.9           2  10922        15
# Albertson College                Yes  587    479    158        38        62         678          41    13500       3335   500      675  67       73       9.4          11   9727        55
str(df)
# 'data.frame':	777 obs. of  18 variables:
#  $ Private    : Factor w/ 2 levels "No","Yes": 2 2 2 2 2 2 2 2 2 2 ...
#  $ Apps       : num  1660 2186 1428 417 193 ...
#  $ Accept     : num  1232 1924 1097 349 146 ...
#  $ Enroll     : num  721 512 336 137 55 158 103 489 227 172 ...
#  $ Top10perc  : num  23 16 22 60 16 38 17 37 30 21 ...
#  $ Top25perc  : num  52 29 50 89 44 62 45 68 63 44 ...
#  $ F.Undergrad: num  2885 2683 1036 510 249 ...
#  $ P.Undergrad: num  537 1227 99 63 869 ...
#  $ Outstate   : num  7440 12280 11250 12960 7560 ...
#  $ Room.Board : num  3300 6450 3750 5450 4120 ...
#  $ Books      : num  450 750 400 450 800 500 500 450 300 660 ...
#  $ Personal   : num  2200 1500 1165 875 1500 ...
#  $ PhD        : num  70 29 53 92 76 67 90 89 79 40 ...
#  $ Terminal   : num  78 30 66 97 72 73 93 100 84 41 ...
#  $ S.F.Ratio  : num  18.1 12.2 12.9 7.7 11.9 9.4 11.5 13.7 11.3 11.5 ...
#  $ perc.alumni: num  12 16 30 37 2 11 26 37 23 15 ...
#  $ Expend     : num  7041 10527 8735 19016 10922 ...
#  $ Grad.Rate  : num  60 56 54 59 15 55 63 73 80 52 ...
summary(df)
 # Private        Apps           Accept          Enroll       Top10perc       Top25perc      F.Undergrad     P.Undergrad         Outstate       Room.Board       Books           Personal   
 # No :212   Min.   :   81   Min.   :   72   Min.   :  35   Min.   : 1.00   Min.   :  9.0   Min.   :  139   Min.   :    1.0   Min.   : 2340   Min.   :1780   Min.   :  96.0   Min.   : 250  
 # Yes:565   1st Qu.:  776   1st Qu.:  604   1st Qu.: 242   1st Qu.:15.00   1st Qu.: 41.0   1st Qu.:  992   1st Qu.:   95.0   1st Qu.: 7320   1st Qu.:3597   1st Qu.: 470.0   1st Qu.: 850  
 #           Median : 1558   Median : 1110   Median : 434   Median :23.00   Median : 54.0   Median : 1707   Median :  353.0   Median : 9990   Median :4200   Median : 500.0   Median :1200  
 #           Mean   : 3002   Mean   : 2019   Mean   : 780   Mean   :27.56   Mean   : 55.8   Mean   : 3700   Mean   :  855.3   Mean   :10441   Mean   :4358   Mean   : 549.4   Mean   :1341  
 #           3rd Qu.: 3624   3rd Qu.: 2424   3rd Qu.: 902   3rd Qu.:35.00   3rd Qu.: 69.0   3rd Qu.: 4005   3rd Qu.:  967.0   3rd Qu.:12925   3rd Qu.:5050   3rd Qu.: 600.0   3rd Qu.:1700  
 #           Max.   :48094   Max.   :26330   Max.   :6392   Max.   :96.00   Max.   :100.0   Max.   :31643   Max.   :21836.0   Max.   :21700   Max.   :8124   Max.   :2340.0   Max.   :6800  
 #      PhD            Terminal       S.F.Ratio      perc.alumni        Expend        Grad.Rate     
 # Min.   :  8.00   Min.   : 24.0   Min.   : 2.50   Min.   : 0.00   Min.   : 3186   Min.   : 10.00  
 # 1st Qu.: 62.00   1st Qu.: 71.0   1st Qu.:11.50   1st Qu.:13.00   1st Qu.: 6751   1st Qu.: 53.00  
 # Median : 75.00   Median : 82.0   Median :13.60   Median :21.00   Median : 8377   Median : 65.00  
 # Mean   : 72.66   Mean   : 79.7   Mean   :14.09   Mean   :22.74   Mean   : 9660   Mean   : 65.46  
 # 3rd Qu.: 85.00   3rd Qu.: 92.0   3rd Qu.:16.50   3rd Qu.:31.00   3rd Qu.:10830   3rd Qu.: 78.00  
 # Max.   :103.00   Max.   :100.0   Max.   :39.80   Max.   :64.00   Max.   :56233   Max.   :118.00  

# Create a scatterplot of Grad.Rate versus Room.Board, colored by the Private column. Please interpret the scatterplot.
gratevsrb <- ggplot(data = df, aes(x = Grad.Rate, y = Room.Board)) +geom_point(aes(shape = Private, color = Private))
plot(gratevsrb)
# The most noticable pattern is that non-private data-points are concentrated between 30% and 75% graduation rate.
# Meanwhile, private data-points are concentrated between 40% and 85% graduation rate.
# There is an institution with a graduation rate above 100%

# Create a histogram of full time undergrad students, color by Private. Please interpret the histogram.
ftusvsprivate <- ggplot(data = df, aes(x = F.Undergrad)) + geom_histogram(aes(fill = Private))
plot(ftusvsprivate)
# Institutions with a larger full-time undergraduate population are more likely to be non-private.
# Conversely, smaller full-time undergraduation populations are more likely to be private.
# The histogram itself is right skewed.

# Create a histogram of Grad.Rate colored by Private. You should see something odd here.
gratevsprivate <- ggplot(data = df, aes(x = Grad.Rate)) + geom_histogram(aes(fill = Private))
plot(gratevsprivate)
# While there is no noticeable difference between private and non-private, there does exist a gap between 55% and 60%.
# There is an institution with a graduation rate above 100%

# What college had a Graduation Rate of above 100% ? Change that college's grad rate to 100%.
subset(df, df$Grad.Rate > 100)
#                   Private Apps Accept Enroll Top10perc Top25perc F.Undergrad P.Undergrad Outstate Room.Board Books Personal PhD Terminal S.F.Ratio perc.alumni Expend Grad.Rate
# Cazenovia College     Yes 3847   3433    527         9        35        1010          12     9384       4840   600      500  22       47      14.3          20   7697       118

# Split your data into training and testing sets 70/30. Set seed as 101.
set.seed(101)
split = sample.split(df$Private, SplitRatio = 0.7)
training_set = subset(df, split == TRUE)
testing_set = subset(df, split == FALSE)

# Use the rpart library to build a decision tree to predict whether or not a school is Private. 
# Remember to only build your tree off the training data.
dt <- rpart(Private ~., data = training_set)

# Use predict() to predict the Private label on the test data.
DTPredict = predict(dt, type = 'prob', newdata = testing_set[-1])

# Check the Head of the predicted values. You should notice that you actually have two columns (Yes/No) with the probabilities.
head(DTPredict)
#                                                  No       Yes
# Adrian College                          0.003311258 0.9966887
# Alfred University                       0.003311258 0.9966887
# Allegheny College                       0.003311258 0.9966887
# Allentown Coll. of St. Francis de Sales 0.003311258 0.9966887
# Alma College                            0.003311258 0.9966887
# Amherst College                         0.003311258 0.9966887

# Turn these two columns into one column to match the original Yes/No Label for a Private column.
y_pred = ifelse(DTPredict > 0.5, 1, 0)
dtresult$model_result <- y_pred[,2]
dtresult$actual_result <- ifelse(testing_set$Private == 'Yes', 1 ,0)

# Create a confusion matrix of your tree model. What's the model accuracy? 
cm = table(dtresult$actual_result, dtresult$model_result)
cm
  #     0   1
  # 0  57   7
  # 1   9 160
T1Error = cm[1,2]/sum(cm)
T1Error
# 0.03004292
T2Error = cm[2,1]/sum(cm)
T2Error
# 0.03862661
Accuracy = 1 - (T1Error + T2Error)
Accuracy
# 0.9313305

# Use the rpart.plot library and the prp() function to plot out your tree model.
install.packages('rpart.plot')
library('rpart.plot')
prp(dt)

# Call the randomForest package library
library('randomForest')

# Now use randomForest() to build out a model to predict Private class. Use n=500 trees. 
# Add importance=TRUE as a parameter in the model. (Use help(randomForest) to find out what this does.
rfmodel <- randomForest(Private ~., data = training_set, ntree = 500, importance = TRUE)

# What was your model's confusion matrix on its own training set? 
rfmodel
# Confusion matrix:
#      No Yes class.error
# No  127  21  0.14189189
# Yes  11 385  0.02777778

# Now use your random forest model to predict on your test set.
prob_pred = predict(rfmodel, type = 'prob', newdata = testing_set)

# What's the confusion matrix on the test set? What's the model accuracy?
rfpred = ifelse(prob_pred > 0.5, 1, 0)
rfact = ifelse(testing_set$Private == 'Yes', 1, 0)
tcm = table(rfact, rfpred[,2])
tcm
# rfact   0   1
#     0  57   7
#     1   6 163
accuracy <- 1 - sum(tcm[1,2], tcm[2,1])/sum(tcm)
accuracy
# 0.944206

# Use the caTools library to build a logistic regression model to predict whether or not a school is Private. 
# Remember to only build your model off the training data.
lrmodel = glm(formula = Private ~., family = binomial, data = training_set)

# Use predict() to predict the Private label on the test data.
lrpred = predict(lrmodel, type = 'response', newdata = testing_set[,-1])

# Check the Head of the predicted values. You should notice that you actually have two columns (Yes/No) with the probabilities.
head(lrpred)
# Adrian College                       Alfred University                       Allegheny College Allentown Coll. of St. Francis de Sales 
# 0.9991789                               0.9999543                               0.9999899                               0.9948399 
# Alma College                         Amherst College 
# 0.9990147                               0.9999986 
# The prediction function requires logistic regression models to use the "type = 'response'" input, thus only probabilities

# Turn these two columns into one column to match the original Yes/No Label for a Private column.
lrpred = ifelse(lrpred > 0.5, 1, 0)

# Use table() to create a confusion matrix of your logistic regression model. What's the model accuracy?
lrfact = ifelse(testing_set$Private == 'Yes', 1, 0)
lrcm = table(lrfact, lrpred)
lrcm
#       lrpred
# lrfact   0   1
#      0  58   6
#      1   4 165
lrt1error = lrcm[1,2]/sum(lrcm)
lrt2error = lrcm[2,1]/sum(lrcm)
lraccuracy = 1 - (lrt1error + lrt2error)
lraccuracy
# 0.9570815]

# Now compare the model accuracy for the above tree model, random forest model and logistic regression model. 
# Which one is the best for this data set?
# Logistic Regression Model with 95.7% accuracy

# Use the caTools library to build a multiple linear regression model to predict the enrollment.
mlrmodel <- lm(Enroll ~., data = training_set)

# Show the summary information of your model. What's the R-squared value of the training data?
summary(mlrmodel)
# Call:
# lm(formula = Enroll ~ ., data = training_set)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -754.99  -63.17   -7.02   51.86 1419.05 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 114.645457  81.263879   1.411 0.158901    
# PrivateYes   56.058279  27.132868   2.066 0.039311 *  
# Apps         -0.025058   0.006918  -3.622 0.000320 ***
# Accept        0.121127   0.012818   9.450  < 2e-16 ***
# Top10perc     4.034295   1.142023   3.533 0.000448 ***
# Top25perc    -2.693321   0.890548  -3.024 0.002613 ** 
# F.Undergrad   0.156851   0.004531  34.618  < 2e-16 ***
# P.Undergrad  -0.008492   0.006262  -1.356 0.175662    
# Outstate     -0.005003   0.003875  -1.291 0.197271    
# Room.Board   -0.015341   0.009482  -1.618 0.106279    
# Books        -0.021411   0.049412  -0.433 0.664970    
# Personal     -0.003835   0.011764  -0.326 0.744582    
# PhD           0.092938   0.891768   0.104 0.917036    
# Terminal     -0.739053   0.988120  -0.748 0.454832    
# S.F.Ratio    -1.366292   2.528939  -0.540 0.589244    
# perc.alumni   1.656226   0.788795   2.100 0.036232 *  
# Expend        0.004141   0.002837   1.460 0.144991    
# Grad.Rate     0.838340   0.596051   1.406 0.160169    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 169.6 on 526 degrees of freedom
# Multiple R-squared:  0.9653,	Adjusted R-squared:  0.9642 
# F-statistic: 860.1 on 17 and 526 DF,  p-value: < 2.2e-16
# R^2: .9653

# Predict the enrollments on test data. Show the head of the prediction dataframe.
mlrpred = predict(mlrmodel, type = 'response', newdata = testing_set[,-4])
mlrresult = as.data.frame(mlrpred)
head(mlrpred)
# Adrian College                       Alfred University                       Allegheny College Allentown Coll. of St. Francis de Sales 
#        326.3561                                439.5181                                513.5823                                304.8539 
#    Alma College                         Amherst College 
#        383.1358                                510.3873 

# Whats are the MSE and R-squared value of the prediction? Is this a good model?
mlrresult$actual <- testing_set$Enroll
mlrresult$error <- (mlrresult$mlrpred - mlrresult$actual)
mlrresult$error <- (mlrresult$error)^2
MSE <- (sum(mlrresult$error)/nrow(mlrresult))
MSE <- MSE^0.5
MSE
# 268.1595
rsquared <- (nrow(mlrresult)*sum(mlrresult$mlrpred * mlrresult$actual) - (sum(mlrresult$mlrpred)*sum(mlrresult$actual)))/(((nrow(mlrresult)*sum((mlrresult$mlrpred)^2) - (sum(mlrresult$mlrpred))^2)*(nrow(mlrresult)*sum((mlrresult$actual)^2) - (sum(mlrresult$actual))^2))^0.5)
rsquared
# 0.9642209
# With a high r-squared value, this is a good model