###################################libraries###########
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(caret)
library(randomForest)
library(devtools)
library(parallel)
library(doParallel)
library(ggraph)
library(igraph)
library(rpart)
library(rpart.plot)
library(tree)
library(xgboost)
library(ROCR)
library(pROC)
library(xgboost)
library(rJava)
library(Ckmeans.1d.dp)
library(DiagrammeR)

############################### loading the data /data prep ############### 
#Methods 4 Poster
#collect us election data
el_survey <- read.csv("~/polybox/Methods 4 Poster/CCES20_Common_OUTPUT.csv")
names(el_survey)

#select all relevant variables
el_subset <- el_survey %>% select(birthyr, gender, race, educ, comptype, region,
                                  CC20_307, CC20_309e, CC20_356a, 
                     urbancity, CC20_363, CC20_364a, ideo5, employ, 
                     pew_religimp, pew_prayer, religpew, newsint, marstat, dualcit, ownhome, faminc_new,
                     child18, union, investor, phone, internethome, internetwork, presvote16post,
                     sexuality, trans
                     )

#remove NA's in the dependent variable
el_subset %<>% filter(!is.na(CC20_364a))

colSums(is.na(el_subset))
names(el_subset)

#fix CC_307 character issue
train_balanced_OHE$CC20_307 <- as.numeric(train_balanced_OHE$CC20_307)
test_OHE$CC20_307 <- as.numeric(test_OHE$CC20_307)

############################################ processingdata####################################################
#make numerical values to categorial
el_subset$gender <- ifelse(el_subset$gender==1, 1, 0)###

el_subset$CC20_307 <- ifelse(el_subset$CC20_307==1 | el_subset$CC20_307 ==2 , "1", "0")

el_subset$educ <- ifelse(el_subset$educ==1, "no_hs", 
                  ifelse(el_subset$educ==2, "hs", 
                  ifelse(el_subset$educ==3, "some_col", 
                  ifelse(el_subset$educ==4, "2_col", 
                  ifelse(el_subset$educ==5, "4_col", "postgrad")))))

el_subset$race <- ifelse(el_subset$race==1, "white", 
                         ifelse(el_subset$race==2, "black", 
                                ifelse(el_subset$race==3, "hispanic", 
                                       ifelse(el_subset$race==4, "asian", 
                                              ifelse(el_subset$race==5, "native", 
                                                     ifelse(el_subset$race==6, "two_more", 
                                                            ifelse(el_subset$race==7, "other", "mideast")))))))

el_subset$comptype <- ifelse(el_subset$comptype==1, "phone", 
                             ifelse(el_subset$comptype==2, "ipad", "computer"))

el_subset$region <- ifelse(el_subset$region==1, "northeast", 
                         ifelse(el_subset$region==2, "midwest", 
                                ifelse(el_subset$educ==3, "south", "west")))

el_subset$CC20_356a <- ifelse(el_subset$CC20_356a==1, "before", ifelse(el_subset$CC20_356a==2, "after", "not_sure"))

el_subset$CC20_356 <- ifelse(el_subset$CC20_356==1, "support", ifelse(el_subset$CC20_356==2, "oppose", "not_sure"))

el_subset$urbancity <- ifelse(el_subset$urbancity==1, "city", 
                         ifelse(el_subset$urbancity==2, "suburb", 
                                ifelse(el_subset$urbancity==3, "town", 
                                       ifelse(el_subset$urbancity==4, "rural", "other"))))

el_subset$CC20_363 <- ifelse(el_subset$CC20_363==1, "yes", 
                         ifelse(el_subset$CC20_363==2, "probably", 
                                ifelse(el_subset$CC20_363==3, "already", 
                                       ifelse(el_subset$CC20_363==4, "plan", 
                                              ifelse(el_subset$CC20_363==5, "no", "undecided")))))

el_subset$CC20_364a <- ifelse(el_subset$CC20_364a==1, "trump", 
                             ifelse(el_subset$CC20_364a==2, "biden", 
                                    ifelse(el_subset$CC20_364a==3, "other", 
                                           ifelse(el_subset$CC20_364a==4, "not_sure", "no_vote"))))

el_subset$ideo5 <- ifelse(el_subset$ideo5==1, "very_liberal", 
                             ifelse(el_subset$ideo5==2, "liberal", 
                                    ifelse(el_subset$ideo5==3, "moderate", 
                                           ifelse(el_subset$ideo5==4, "conservative", 
                                                  ifelse(el_subset$ideo5==5, "very_conservative", "not_sure")))))

el_subset$employ <- ifelse(el_subset$employ==1, "full_time", 
                         ifelse(el_subset$employ==2, "part_time", 
                                ifelse(el_subset$employ==3, "temp_off", 
                                       ifelse(el_subset$employ==4, "unemployed", 
                                              ifelse(el_subset$employ==5, "retired", 
                                                     ifelse(el_subset$employ==6, "disabled", 
                                                            ifelse(el_subset$employ==7, "take_care_family",
                                                                   ifelse(el_subset$employ==8, "student", "other"))))))))
#omit those that dont know how many times they pray
el_subset %<>% filter(pew_prayer != 8)

el_subset$religpew <- ifelse(el_subset$religpew==1, "protestant", 
                           ifelse(el_subset$religpew==2, "catholic", 
                                  ifelse(el_subset$religpew==3, "mormon", 
                                         ifelse(el_subset$religpew==4, "orthodox", 
                                                ifelse(el_subset$religpew==5, "jewish", 
                                                       ifelse(el_subset$religpew==6, "muslim", 
                                                              ifelse(el_subset$religpew==7, "buddhist",
                                                                     ifelse(el_subset$religpew==8, "hindu",
                                                                            ifelse(el_subset$religpew==9, "atheist", 
                                                                                                  ifelse(el_subset$religpew==10, "agnostic", 
                                                                                                                  ifelse(el_subset$religpew==11, "nothing_particular", "other")))))))))))

el_subset$newsint <- ifelse(el_subset$newsint==1, "most_time", 
                          ifelse(el_subset$newsint==2, "some_time", 
                                 ifelse(el_subset$newsint==3, "now_and_then", 
                                        ifelse(el_subset$newsint==4, "hardly", "not_sure"))))

el_subset$marstat <- ifelse(el_subset$marstat==1, "married", 
                          ifelse(el_subset$marstat==2, "separated", 
                                 ifelse(el_subset$marstat==3, "divorced", 
                                        ifelse(el_subset$marstat==4, "widowed", 
                                               ifelse(el_subset$marstat==5, "never_married", "civil_partnership")))))

el_subset$dualcit <- ifelse(el_subset$dualcit==1, 1, 0)###

el_subset$ownhome <- ifelse(el_subset$ownhome==1, "own", 
                            ifelse(el_subset$ownhome==2, "rent", "other"))

el_subset %<>% filter(faminc_new!=97)

el_subset$child18 <- ifelse(el_subset$child18==1, 1, 0)###

el_subset$union <- ifelse(el_subset$union==1, "currently", 
                          ifelse(el_subset$union==2, "formerly", "never"))

el_subset$investor <- ifelse(el_subset$investor==1, 1, 0)###

el_subset$phone <- ifelse(el_subset$phone==1, "line_and_cell", 
                           ifelse(el_subset$phone==2, "cell", 
                                  ifelse(el_subset$phone==3, "line", "none")))

el_subset$internethome <- ifelse(el_subset$internethome==1, "cable/dsl/broadband", 
                                 ifelse(el_subset$internethome==2, "dial_up", "none"))

el_subset$internetwork <- ifelse(el_subset$internetwork==1, "cable/dsl/broadband", 
                                 ifelse(el_subset$internetwork==2, "dial_up", "none"))

el_subset$presvote16post <- ifelse(el_subset$presvote16post==1, "clinton", 
                         ifelse(el_subset$presvote16post==2, "trump", 
                                ifelse(el_subset$presvote16post==3, "johnson", 
                                       ifelse(el_subset$presvote16post==4, "stein", 
                                              ifelse(el_subset$presvote16post==5, "mcmullin", 
                                                     ifelse(el_subset$presvote16post==6, "other", "didnt_vote"))))))

el_subset$sexuality <- ifelse(el_subset$sexuality==1, "hetero", 
                            ifelse(el_subset$sexuality==2, "gay_woman", 
                                   ifelse(el_subset$sexuality==3, "gay_man", 
                                          ifelse(el_subset$sexuality==4, "bi", 
                                                 ifelse(el_subset$sexuality==5, "other", "prefer_not_say")))))

el_subset$trans <- ifelse(el_subset$trans==1, "yes", 
                          ifelse(el_subset$trans==2, "no", "prefer_not_say"))

#create age variable
el_subset$age <- 2021 - el_subset$birthyr

el_subset %<>% select(-birthyr)

#only use biden and trump outcomes
el_subset %<>% filter(CC20_364a == "biden" | CC20_364a == "trump")

el_subset$policefeelsafe <- as.numeric(el_subset$policefeelsafe)
el_subset$investor <- as.numeric(el_subset$investor)
el_subset$faminc_new <- as.numeric(el_subset$faminc_new)
el_subset$pew_prayer <- as.numeric(el_subset$pew_prayer)
el_subset$pew_religimp <- as.numeric(el_subset$pew_religimp)


#make variables to factor
cols <- c(2:5, 8:13, 16:18, 23, 25:31)
el_subset[cols] <- lapply(el_subset[cols], factor)

el_subset <- el_subset[complete.cases(el_subset)==T,]

###use one hot encoding
dmy <- dummyVars(" ~  race + educ + comptype + region + 
  urbancity + employ + religpew + newsint + marstat +
  ownhome + union + phone + internethome + internetwork + sexuality + trans", data = el_subset)
trsf <- data.frame(predict(dmy, newdata = el_subset))

#columns that are not one hot encoded: trump, male, policefeelsafe, dualcit, child18, investor
dummyfied_data <- cbind(el_subset[, c(11, 1, 6, 7,14,15, 19, 21, 22 , 24, 32)], trsf)
dummyfied_data$CC20_364a <- ifelse(dummyfied_data$CC20_364a == "trump", 1, 0)
dummyfied_data$CC20_364a <- as.factor(dummyfied_data$CC20_364a)

#split the data one hot encoded
trainIndex_OHE <- createDataPartition(dummyfied_data$CC20_364a, 
                                  p = .8, 
                                  list = F, 
                                  times = 1)

#split the data without one hot encoding
trainIndex <- createDataPartition(el_subset$CC20_364a, 
                                  p = .8, 
                                  list = F, 
                                  times = 1)

#formula select columns that I think are not too highly correlated
f <- CC20_364a ~ age + gender + race + educ + comptype + region + CC20_307 + CC20_309e + 
  urbancity + employ + pew_religimp + pew_prayer + religpew + newsint + marstat + dualcit + 
  ownhome + faminc_new + child18 + union + investor + phone + internethome + internetwork + sexuality + trans


######################################################################IMPORTANT STEP: BALANCING TRAIN SET
train_OHE <- dummyfied_data[trainIndex_OHE,]
train <- el_subset[trainIndex,]

library(ROSE)
train_balanced_OHE <- ovun.sample(CC20_364a ~ ., data = train_OHE, method = "over")$data

### without oversampling train data
train


table(train_balanced_OHE$CC20_364a)

#divide into test data that was NOT oversampled
test_OHE <- dummyfied_data[-trainIndex_OHE,]
test <- el_subset[-trainIndex,]


fitControl <- trainControl(method = "cv", number = 5)

train_balanced_OHE$CC20_307 <- as.numeric(train_balanced_OHE$CC20_307)
test_OHE$CC20_307 <- as.numeric(test_OHE$CC20_307)

##################################################1.0 singletrees #############################################
#test_OHE accuracy function


confusion <- function(x){
  df <- predict(x, test_OHE, type = 'class')
  confusionMatrix(df, test_OHE$CC20_364a)
}

control <- rpart.control(minsplit = 20, minbucket = 10, cp = 0.000, 
              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 5,
              surrogatest_OHEyle = 0, maxdepth = 30)


train_balanced_OHE_single_tree <- train_balanced_OHE

#run this to show "biden" and "trump" instead of 1's and 0's
#train_balanced_OHE_single_tree$CC20_364a <- as.factor(ifelse(train_balanced_OHE_single_tree$CC20_364a==1, "Trump", "Biden"))

#default
set.seed(3)
fit_tree <- rpart(CC20_364a~., data = train_balanced_OHE_single_tree, method = "class", control = control)
fit_tree$cptable

confusion(fit_tree)

opt <- which.min(fit_tree$cptable[,"xerror"])

#The xerror column contains of estimates of cross-validated prediction error for different numbers of splits (nsplit). 
#The best tree has eleven splits. Now we can prune back the large initial tree using the ideal pruning parameter

cp <- fit_tree$cptable[opt, "CP"]
fit_tree_pruned <- prune(fit_tree, cp = cp)

plot(fit_tree_pruned, uniform = TRUE, margin = 0.03, branch = 1)
text(fit_tree_pruned, use.n = TRUE, pretty = 1, cex = 1)
rn <- rownames(fit_tree_pruned$frame)
lev <- rn[sort(unique(fit_tree_pruned$where))]
where <- factor(rn[fit_tree_pruned$where], levels = lev)
mosaicplot(table(where, train_balanced_OHE$CC20_364a), main = "Feature Space Partition", xlab = "", ylab = "Trump Voter",las = 1)
rpart.plot(fit_tree_pruned, fallen.leaves = F, cex = 0.8, ycompress = F, space = 0, extra = 101, gap = 0)

#testing the model on unseen data
confusion(fit_tree_pruned)

################################################# 2.0 randomForest #############################################
rf_default <- randomForest(CC20_364a~., train_balanced_OHE, importance = T)
rf_default_imbalanced <- randomForest(CC20_364a ~ ., train, importance = T)

plot(rf_default)
plot(rf_default_imbalanced)

rf_default_imbalanced$confusion

confusion <- function(x){
  df <- predict(x, test_OHE, type = 'class')
  confusionMatrix(df, test_OHE$CC20_364a)
}

confusion(rf_default)

confusion <- function(x){
  df <- predict(x, test, type = 'class')
  confusionMatrix(df, test$CC20_364a)
}

confusion(rf_default_imbalanced)

rf_default$mtry

#utilize parallelization
cluster <- makeCluster(11)
registerDoParallel(cluster)
start_time <- Sys.time()

##################### 2.1 tuning hyper-parameters random forest ############
set.seed(3)
fitControl_caret <- trainControl(method = "cv", 
                                 number = 5, 
                                 classProbs = T, 
                                 allowParallel = T,
                                 savePredictions = "final")

#balanced sample
data_set_for_caret <- train_balanced_OHE
data_set_for_caret$CC20_364a <- as.factor(ifelse(data_set_for_caret$CC20_364a == 1, "Trump", "Biden"))

#imbalancd sample
caret_imbalanced <- train_imbalanced
caret_imbalanced$CC20_364a <- as.factor(ifelse(caret_imbalanced$CC20_364a == 1, "Trump", "Biden"))

#train on balanced dataset
tunegrid <- expand.grid(mtry = c(seq(1, 20, 1)))
rf_caret <- train(CC20_364a ~ . , 
               data=data_set_for_caret, 
               method='rf', 
               metric='ROC',
               ntree = 200,
               tuneGrid=tunegrid, 
               trControl=fitControl_caret)
end_time <- Sys.time()
time_difference = end_time - start_time
print(time_difference)

#train on imbalanced dataset
rf_caret_imbalanced <- train(CC20_364a ~ . , 
                  data=caret_imbalanced, 
                  method='rf', 
                  metric='ROC',
                  ntree = 200,
                  tuneGrid=tunegrid, 
                  trControl=fitControl_caret)

#plot mtry graphs for imbalanced and balanced in one single plot
max(rf_caret$results["Kappa"])
max(rf_caret_imbalanced$results["Kappa"])

rf_caret$

#prepare test validation caret dataset
test_OHE_caret <- test_OHE
test_OHE_caret$CC20_364a <- as.factor(ifelse(test_OHE$CC20_364a == "1", "Trump", "Biden"))

#validate balanced dataset
result.predicted.prob <- predict(rf_caret, test_OHE_caret, type="raw") # Prediction
confusionMatrix(result.predicted.prob, test_OHE_caret$CC20_364a) #confusion matrix

#validate imbalanced dataset
result.predicted.prob_imbalanced <- predict(rf_caret_imbalanced, test_OHE_caret, type="raw") # Prediction
confusionMatrix(result.predicted.prob_imbalanced, test_OHE_caret$CC20_364a) #confusion matrix


####################################################### save workspace
save.image(file = "Met4poster.RData")

kappa <- rf_caret$results$Kappa
kappa_imbalanced <- rf_caret_imbalanced$results$Kappa

caret_plot <- as.data.frame(cbind(kappa, kappa_imbalanced))

#plot kappa comparison
ggplot(caret_plot) +
  geom_line(aes(x = c(1:20), y = kappa, color = "Oversampled"), size = 1.5) +
  geom_line(aes(x = c(1:20), y = kappa_imbalanced, color = "Original"), size = 1.5) + 
  geom_vline(xintercept = 13, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Kappa Comparison Random Forest") +
  xlab("Number of variables at each split")

################################################# 3.0 boosting #################################################
####train data
train_label <- as.matrix(as.numeric(as.character((train_balanced_OHE[,"CC20_364a"]))))
train_balanced_OHE$CC20_307 <- as.numeric(train_balanced_OHE$CC20_307)
train_variables <- as.matrix(train_balanced_OHE[, -1])

####test data
test_label <- as.numeric(as.matrix(test_OHE[,"CC20_364a"]))
test_OHE$CC20_307 <- as.numeric(test_OHE$CC20_307)
test_variables <- as.matrix(test_OHE[, -1])

#train also with imbalanced data
train_imbalanced <- dummyfied_data[trainIndex,]

#xgb matrices
dtrain <- xgb.DMatrix(data = train_variables, label=train_label)

dtest <- xgb.DMatrix(data = test_variables, label=test_label)

#xgb matrices with imbalanced data
train_label_imbalanced <- as.numeric(as.matrix(as.factor(train_imbalanced[,"CC20_364a"])))
train_imbalanced$CC20_307 <- as.numeric(train_imbalanced$CC20_307)
train_variables_imbalanced <- as.matrix(train_imbalanced[, -1])


dtrain_imbalanced <- xgb.DMatrix(data = train_variables_imbalanced, label=train_label_imbalanced)

dtest <- xgb.DMatrix(data = test_variables, label=test_label)

#basic training
bstSparse <- xgboost(data = dtrain, max.depth = 1000, eta = 0.5, nthread = 11, nrounds = 10, objective = "binary:logistic")
bstSparse_imbalanced <- xgboost(data = dtrain_imbalanced, max.depth = 1000, eta = 0.5, nthread = 11, nrounds = 10, objective = "binary:logistic")


####################################################### save workspace
save.image(file = "Met4poster.RData")

#tune boost parameters
bst <- xgboost(data = dtrain, 
               max.depth = 50, 
               eta = 0.2, 
               nthread = 11,
               nfolds = 15,
               nrounds = 500, 
               objective = "binary:logistic", 
               verbose = 2)

summary(bst)
confusion_xgb(bst)

#prediction performance
pred <- predict(bst, dtest, type = "class")
prediction <- as.factor(as.numeric(pred > 0.5))
confusionMatrix(prediction, as.factor(test_label))

#test validation function
confusion_xgb <- function(x) {
  pred <- predict(x, test_variables, type = "class")
  prediction <- as.factor(as.numeric(pred > 0.5))
  confusionMatrix(prediction, as.factor(test_label))
}

################ foolin' around ############
set.seed(3)
df_xgb <- rep(NA, 30)

#optimize max depth tree balanced data
set.seed(3)
watchlist <- list(train=dtrain)
for (i in 1:30) {
asdf_depth <- xgb.train(data=dtrain, 
                      max.depth=i, 
                      eta= 0.2,
                      subsample = 0.5,
                      colsample_bytree = 0.2,
                      gamma = 1,
                      nthread = 11,
                      nfold = 5, #do not change
                      nrounds= 100,
                      watchlist = watchlist,
                      early_stopping_rounds = 5,
                      eval_metric = c("auc","error"),
                      objective = "binary:logistic")
df_xgb[i] <- asdf_depth$best_score
print(i)
}
asdf <- as.data.frame(df_xgb)

############################## optimize max depth imbalanced data
df_xgb_imba <- rep(NA, 30)
set.seed(3)
watchlist <- list(train=dtrain_imbalanced)
for (i in 1:30) {
  asdf_depth <- xgb.train(data=dtrain_imbalanced, 
                          max.depth=i, 
                          eta= 0.2,
                          subsample = 0.5,
                          colsample_bytree = 0.2,
                          gamma = 1,
                          nthread = 11,
                          nfold = 5, #do not change
                          nrounds= 100,
                          watchlist = watchlist,
                          early_stopping_rounds = 5,
                          eval_metric = c("auc","error"),
                          objective = "binary:logistic")
  df_xgb_imba[i] <- asdf_depth$best_score
  print(i)
}

df_xgb_imba <- as.data.frame(df_xgb_imba)

asdf_depth <- cbind(df_xgb, as.data.frame(df_xgb_imba))

#plot max depth
depth_plot <- ggplot(asdf_depth) +
  geom_line(aes(x = c(1:30), y = df_xgb, color = "Oversampled"), size = 1.0) +
  geom_line(aes(x = c(1:30), y = df_xgb_imba, color = "Original"), size = 1.0) +
  geom_vline(xintercept = 13, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Gradient Boosting Maximum Depth") +
  xlab("Maximum number of trees for each iteration") +
  ylab("AUC")


#optimal tree depth balanced: 13

################################ optimize eta parameter balanced
set.seed(3)
watchlist <- list(train=dtrain)
df_eta <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain, 
                        max.depth=13, 
                        eta=i/100,
                        subsample = 0.5,
                        colsample_bytree = 0.2,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        early_stopping_rounds = 5,
                        watchlist = watchlist,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_eta[i] <- bst_asdf$best_score
  print(i)
}

df_eta <- as.data.frame(df_eta)

#optimize eta parameter imbalanced
set.seed(3)
watchlist <- list(train=dtrain_imbalanced)
df_eta_imba <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain_imbalanced, 
                        max.depth=13, 
                        eta=i/100,
                        subsample = 0.5,
                        colsample_bytree = 0.2,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        early_stopping_rounds = 5,
                        watchlist = watchlist,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_eta_imba[i] <- bst_asdf$best_score
  print(i)
}

df_eta_imba <- as.data.frame(df_eta_imba)

asdf_eta <- cbind(df_eta, df_eta_imba)

####################################################### save workspace
save.image(file = "Met4poster.RData")

#plot optimal eta
eta_plot <- ggplot(asdf_eta) +
  geom_line(aes(x = c(1:100), y = df_eta, color = "Oversampled"), size = 1.0) +
  geom_line(aes(x = c(1:100), y = df_eta_imba, color = "Original"), size = 1.0)+
  geom_vline(xintercept = 12, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Gradient Boosting Shrinkage Parameter") +
  xlab("Value of shrinkage parameter (%)") +
  ylab("AUC")

#optimal eta parameter: 0.12

########################################### optimize subsample parameter
set.seed(3)
watchlist <- list(train=dtrain)
df_subsample <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain, 
                        max.depth=13, 
                        eta=0.12,
                        subsample = i/100,
                        colsample_bytree = 0.2,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_subsample[i] <- bst_asdf$best_score
  print(i)
}
df_subsample <- as.data.frame(df_subsample)

set.seed(3)
watchlist <- list(train=dtrain_imbalanced)
df_subsample_imba <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain_imbalanced, 
                        max.depth=13, 
                        eta=0.12,
                        subsample = i/100,
                        colsample_bytree = 0.2,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_subsample_imba[i] <- bst_asdf$best_score
  print(i)
}
df_subsample_imba <- as.data.frame(df_subsample_imba)

df_subsample_both <- cbind(df_subsample, df_subsample_imba)

#plot optimal subsample
subsample_plot <- ggplot(df_subsample_both) +
  geom_line(aes(x = c(1:100), y = df_subsample, color = "Oversampled"), size = 1.0) +
  geom_line(aes(x = c(1:100), y = df_subsample_imba, color = "Original"), size = 1.0)+
  geom_vline(xintercept = 30, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Gradient Boosting Subsample Parameter") +
  xlab("Percentage used for Subsample (%)") +
  ylab("AUC")

#optimal subsample_: 0.3

##################################optimize colsample by tree
set.seed(3)
watchlist <- list(train=dtrain)
df_coltree <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain,
                        max.depth=13,
                        eta=0.12,
                        subsample = 0.3,
                        colsample_bytree = i/100,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_coltree[i] <- bst_asdf$best_score
  print(i)
}

df_coltree <- as.data.frame(df_coltree)

#colsample imbalanced
set.seed(3)
watchlist <- list(train=dtrain_imbalanced)
df_coltree_imba <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain_imbalanced,
                        max.depth=13,
                        eta=0.12,
                        subsample = 0.3,
                        colsample_bytree = i/100,
                        gamma = 1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_coltree_imba[i] <- bst_asdf$best_score
  print(i)
}
df_coltree_imba <- as.data.frame(df_coltree_imba)

df_coltree_both <- cbind(df_coltree, df_coltree_imba)


####################################################### save workspace
save.image(file = "Met4poster.RData")

#plot optimal coltree sample
coltree_plot <- ggplot(df_coltree_both) +
  geom_line(aes(x = c(1:100), y = df_coltree, color = "Oversampled"), size = 1.0) +
  geom_line(aes(x = c(1:100), y = df_coltree_imba, color = "Original"), size = 1.0)+
  geom_vline(xintercept = 25, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Gradient Boosting Tree Column Sample") +
  xlab("Percentage used for Tree Column Sample (%)") +
  ylab("AUC")


#optimal coltree: 0.25

########################################### optimize gamma balanced
set.seed(3)
df_gamma <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain,
                        max.depth=13,
                        eta=0.12,
                        subsample = 0.3,
                        colsample_bytree = 0.25,
                        gamma = i-1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_gamma[i] <- bst_asdf$best_score
  print(i)
}

df_gamma <- as.data.frame(df_gamma)

set.seed(3)
df_gamma_imba <- rep(NA, 100)
for (i in 1:100) {
  bst_asdf <- xgb.train(data=dtrain_imbalanced,
                        max.depth=13,
                        eta=0.12,
                        subsample = 0.3,
                        colsample_bytree = 0.25,
                        gamma = i-1,
                        nthread = 11,
                        nfold = 5, #do not change
                        nrounds= 100,
                        watchlist=watchlist,
                        early_stopping_rounds = 10,
                        eval_metric = c("auc","error"),
                        objective = "binary:logistic")
  df_gamma_imba[i] <- bst_asdf$best_score
  print(i)
}

df_gamma_imba <- as.data.frame(df_gamma_imba)

df_gamma_both <- cbind(df_gamma, df_gamma_imba)

#plot optimal gamma
gamma_plot <- ggplot(df_gamma_both) +
  geom_line(aes(x = c(0:99), y = df_gamma, color = "Oversampled"), size = 1.0) +
  geom_line(aes(x = c(0:99), y = df_gamma_imba, color = "Original"), size = 1.0)+
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "gray90", colour = "black",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "gray"),
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")) +
  ggtitle("Gradient Boosting Gamma Parameter") +
  xlab("Gamma Value") +
  ylab("AUC")


depth_plot
eta_plot
subsample_plot
coltree_plot
gamma_plot

parameter_plot <- gridExtra::grid.arrange(depth_plot, eta_plot, subsample_plot, coltree_plot, gamma_plot, ncol = 1)
parameter_plot


####################################################### save workspace with plots!
save.image(file = "Met4poster.RData")

#optimal gamma: 0

#_________________________________________
############# final model ################
#_________________________________________

watchlist <- list(train=dtrain)
set.seed(3)
bst_final <- xgb.train(data=dtrain,
                      max.depth=13,
                      eta=0.12,
                      subsample = 0.3,
                      colsample_bytree = 0.25,
                      gamma = 0,
                      nthread = 11,
                      nfold = 5, #do not change
                      nrounds= 100,
                      watchlist=watchlist,
                      early_stopping_rounds = 100,
                      eval_metric = c("auc","error"),
                      objective = "binary:logistic")



watchlist <- list(train=dtrain_imbalanced)
set.seed(3)
bst_final_imba <- xgb.train(data=dtrain_imbalanced,
                       max.depth=13,
                       eta=0.12,
                       subsample = 0.3,
                       colsample_bytree = 0.25,
                       gamma = 0,
                       nthread = 11,
                       nfold = 5, #do not change
                       nrounds= 100,
                       watchlist=watchlist,
                       early_stopping_rounds = 100,
                       eval_metric = c("auc","error"),
                       objective = "binary:logistic")

# final test validation
confusion_xgb(bst_final)
confusion_xgb(bst_final_imba)

# visualize feature importance
importance <- xgb.importance(model = bst_final)
xgb.ggplot.importance(importance_matrix = importance,
                      top_n = 1000,
                      measure = "Gain", n_clusters = 4)

#visualize part of the model tree
xgb.plot.tree(model = bst_final_asdf, trees = 2)

####################################################### save workspace with plots!
save.image(file = "Met4poster.RData")