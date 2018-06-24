#change this to your wd when you pull latest
dir <- '~/Documents/R/HomeLoanDetection'

#setwd
getwd()
setwd(dir)

#load data
application_train <- read.csv('Data/application_train.csv')
sum(application_train$TARGET)
bureau <-read.csv('Data/bureau.csv')
#DONT NEED THESE (AS OF NOW)
#bureau_balance <- read.csv('Data/bureau_balance.csv')
#cash_balance <- read.csv('Data/POS_CASH_balance.csv')
#credit_card_balance <- read.csv('Data/credit_card_balance.csv')
#installments_payments <- read.csv('Data/installments_payments.csv')
#IF I HAVE TIME I WANT TO GENERATE A COLUMN FOR PAST DEFAULTED LOANS
#previous_application <- read.csv('Data/previous_application.csv')
application_test <- read.csv('Data/application_test.csv')

#preview
#DONT NEED TO RUN THESE, FEEL FREE TO UNCOMMENT AND VIEW
#head(application_train)
#head(bureau)
#head(bureau_balance)
#head(credit_card_balance)
#head(installments_payments)

#GENERAL METRICS ABOUT FEATURES PROVIDED
#(MEAN, SD, MIN/MAX, MODE), THESE MUST BE
#RAN IF INTERESTED IN METRICS FOR EACH FEATURE
#!NA count 
colSums(!is.na(application_train))

#Means
if(!require('dplyr')) install.packages('dplyr')
library(dplyr)
colMeans(dplyr::select_if(application_train, is.numeric), na.rm = TRUE)

#Standard deviation
sapply(dplyr::select_if(application_train, is.numeric), sd, na.rm = TRUE)

#Min
sapply(dplyr::select_if(application_train, is.numeric), min, na.rm = TRUE)

#Max
sapply(dplyr::select_if(application_train, is.numeric), max, na.rm = TRUE)

#Mode (THIS DOESNT WORK... YET, LET ME KNOW IF MODE IS NECESSARY)
#VISUALIZATIONS WILL PROBABLY COVER THIS BETTER AS WELL
#sapply(dplyr::select_if(application_train, is.factor), mode)
#print(dplyr::select_if(application_train, is.factor))

#plots (ALSO SHIT RIGHT NOW, LEAVING VISUALIZATIONS FOR SEAN TO
#HANDLE FOR NOW...)
#d <- density(application_train$AMT_INCOME_TOTAL)
#plot(d) 

#print(temp[1:300,])

# = "Active"   "Bad debt" "Closed"   "Sold"  
levels(bureau$CREDIT_ACTIVE)

#THIS IS THE DATASET USED FOR FEATURE GENERATION, application -> left join bureau
#THIS WILL TIE EVERY LOAN APPLICATION TO RELEVANT CREDIT REPORTS TO DETERMINE METRICS
#OF LATE PAYMENTS, OVERDUE BALANCES, ETC...
app_bur <- merge(application_train, bureau, by='SK_ID_CURR', all.x = T)
#records without bureau credit reports (SMH) > 44000
sapply(app_bur, function(x) length(unique(x)))
#THIS ISNT USED RN, SAME THING BUT INNER JOINED
#app_bur_inner <- merge(application_train, bureau, by='SK_ID_CURR')

#feature generation
final_data <- data.frame(id=application_train$SK_ID_CURR)
final_data$class <- application_train$TARGET
#final_data$class <- as.factor(application_train$TARGET)
final_data$credit_vs_income <- application_train$AMT_CREDIT/application_train$AMT_INCOME_TOTAL
final_data$annuity_vs_income <- application_train$AMT_ANNUITY/application_train$AMT_INCOME_TOTAL
final_data$price_vs_loan <- application_train$AMT_GOODS_PRICE/application_train$AMT_CREDIT
final_data$flag_own_car <- application_train$FLAG_OWN_CAR
final_data$flag_own_reality <- application_train$FLAG_OWN_REALTY
final_data$cnt_children <- application_train$CNT_CHILDREN
final_data$amt_income_total <- application_train$AMT_INCOME_TOTAL
final_data$days_birth <- application_train$DAYS_BIRTH
final_data$days_emp <- application_train$DAYS_EMPLOYED
final_data$obs_social_30 <- application_train$OBS_30_CNT_SOCIAL_CIRCLE
final_data$def_social_30 <- application_train$DEF_30_CNT_SOCIAL_CIRCLE
final_data$obs_social_60 <- application_train$OBS_60_CNT_SOCIAL_CIRCLE
final_data$def_social_60 <- application_train$DEF_60_CNT_SOCIAL_CIRCLE
final_data$contract_type <- application_train$NAME_CONTRACT_TYPE
#Generate feature for bad debt or sold credit report status
bad_records <- filter(app_bur, app_bur$CREDIT_ACTIVE %in% c('Bad debt', 'Sold'))
#I'm dumb AF
final_data$status_flag <- ifelse(final_data$id %in% bad_records$SK_ID_CURR, 1, 0)
#5243 bad status flags
sum(final_data$status_flag == 1)

#levels(bureau$CREDIT_TYPE)

status_id <- app_bur[!duplicated(app_bur$SK_ID_CURR),c('SK_ID_CURR', 'CREDIT_TYPE')]
#head(status_id, n=100)
final_data <- merge(final_data, status_id, by.x = c('id'), by.y = c('SK_ID_CURR'), all.x = TRUE)

#head(final_data, n=100)

max_overdue <- aggregate(app_bur$AMT_CREDIT_MAX_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

max_overdue <- merge(max_overdue, application_train[,c('SK_ID_CURR', 'AMT_CREDIT')], by.x = c('Group.1'), by.y = c('SK_ID_CURR'))
max_overdue$overdue_ratio <- max_overdue$Group.1/max_overdue$AMT_CREDIT

final_data <- merge(final_data, max_overdue[,c('Group.1', 'overdue_ratio')], by.x = c('id'), by.y = c('Group.1'))
#
max_dpd <- aggregate(app_bur$CREDIT_DAY_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

final_data <- merge(final_data, max_dpd, by.x = 'id', by.y = 'Group.1')

final_data$max_dpd <- as.numeric(final_data$x)

#max_prolong <- aggregate(app_bur$CNT_CREDIT_PROLONG, by = list(app_bur$SK_ID_CURR), max)
#final_data <- merge(final_data, max_prolong, by.x = 'id', by.y = 'Group.1')
#final_data$max_prolong <- final_data$x.y
#max(max_prolong$x)

#THIS COLUMN IS SHIT
#count_overdue <- as.data.frame(table(unique(app_bur[app_bur$AMT_CREDIT_SUM_OVERDUE > 0,])$SK_ID_CURR))
#total overdue balance count payments = 3644
#sum(count_overdue$Freq)
#max overdue balance count = 8
#max(count_overdue$Freq)

#final_data <- merge(final_data, count_overdue, by.x = 'id', by.y = 'Var1', all.x = TRUE)

#final_data$count_overdue <- final_data$Freq

#DROP COLUMNS THAT NEEDED TO BE RENAMED 
#THIS IS A RESULT OF PACKAGES RENAMING BY DEFAULT
drops <- c('x.x', 'x.y', 'Freq', 'max_prolong', 'x')
final_data <- final_data[,!(names(final_data) %in% drops)]
#final_data <- subset(final_data, select = -c('x.x', 'x.y', 'Freq', 'TARGET'))

#FACTORIZE INT DATA WITH LOW MAX VALUES
#max(final_data$max_prolong)
#final_data$max_prolong <- as.factor(final_data$max_prolong)
#final_data$count_overdue <- as.factor(final_data$count_overdue)

#RECREATE THE FEATURES FOR TEST
app_bur <- merge(application_test, bureau, by='SK_ID_CURR', all.x = T)

#records without bureau credit reports (SMH) ~ 6000
sapply(app_bur, function(x) length(unique(x)))

test_data <- data.frame(id=application_test$SK_ID_CURR)
#test_data$class <- application_test$TARGET
test_data$credit_vs_income <- application_test$AMT_CREDIT/application_test$AMT_INCOME_TOTAL
test_data$annuity_vs_income <- application_test$AMT_ANNUITY/application_test$AMT_INCOME_TOTAL
test_data$price_vs_loan <- application_test$AMT_GOODS_PRICE/application_test$AMT_CREDIT
test_data$flag_own_car <- application_test$FLAG_OWN_CAR
test_data$flag_own_reality <- application_test$FLAG_OWN_REALTY
test_data$cnt_children <- application_test$CNT_CHILDREN
test_data$amt_income_total <- application_test$AMT_INCOME_TOTAL
test_data$days_birth <- application_test$DAYS_BIRTH
test_data$days_emp <- application_test$DAYS_EMPLOYED
test_data$obs_social_30 <- application_test$OBS_30_CNT_SOCIAL_CIRCLE
test_data$def_social_30 <- application_test$DEF_30_CNT_SOCIAL_CIRCLE
test_data$obs_social_60 <- application_test$OBS_60_CNT_SOCIAL_CIRCLE
test_data$def_social_60 <- application_test$DEF_60_CNT_SOCIAL_CIRCLE
test_data$contract_type <- application_test$NAME_CONTRACT_TYPE
#Generate feature for bad debt or sold credit report status
bad_records <- filter(app_bur, app_bur$CREDIT_ACTIVE %in% c('Bad debt', 'Sold'))
test_data$status_flag <- ifelse(test_data$id %in% bad_records$SK_ID_CURR, 1, 0)

#796 records esketit
sum(test_data$status_flag == 1)

#levels(bureau$CREDIT_TYPE)

#final_data$CREDIT_TYPE <- 
status_id <- app_bur[!duplicated(app_bur$SK_ID_CURR),c('SK_ID_CURR', 'CREDIT_TYPE')]
#head(status_id, n=100)
test_data <- merge(test_data, status_id, by.x = c('id'), by.y = c('SK_ID_CURR'), all.x = TRUE)

#head(test_data, n=100)

max_overdue <- aggregate(app_bur$AMT_CREDIT_MAX_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

#count(max_overdue[is.na(max_overdue$x),])
#head(max_overdue)
#names(max_overdue)

max_overdue <- merge(max_overdue, application_test[,c('SK_ID_CURR', 'AMT_CREDIT')], by.x = c('Group.1'), by.y = c('SK_ID_CURR'))
max_overdue$overdue_ratio <- max_overdue$Group.1/max_overdue$AMT_CREDIT

test_data <- merge(test_data, max_overdue[,c('Group.1', 'overdue_ratio')], by.x = c('id'), by.y = c('Group.1'))

max_dpd <- aggregate(app_bur$CREDIT_DAY_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

test_data <- merge(test_data, max_dpd, by.x = 'id', by.y = 'Group.1')

test_data$max_dpd <- test_data$x

#max_prolong <- aggregate(app_bur$CNT_CREDIT_PROLONG, by = list(app_bur$SK_ID_CURR), max)
#test_data <- merge(test_data, max_prolong, by.x = 'id', by.y = 'Group.1')
#test_data$max_prolong <- test_data$x.y

#COLUMN IS SHIT
#count_overdue <- as.data.frame(table(unique(app_bur[app_bur$AMT_CREDIT_SUM_OVERDUE > 0,])$SK_ID_CURR))
#total overdue balance count payments = 3644
#sum(count_overdue$Freq)
#max overdue balance count = 8
#max(count_overdue$Freq)
#test_data <- merge(test_data, count_overdue, by.x = 'id', by.y = 'Var1', all.x = TRUE)
#test_data$count_overdue <- test_data$Freq

#DROP COLUMNS THAT NEEDED TO BE RENAMED 
#THIS IS A RESULT OF PACKAGES RENAMING BY DEFAULT
drops <- c('x.x', 'x.y', 'x', 'Freq')
test_data <- test_data[,!(names(test_data) %in% drops)]

#FACTORIZE INT DATA?
#test_data$max_prolong <- as.factor(test_data$max_prolong)

#GONNA HAVE TO CLEAN (TRAIN)
sum(is.na(final_data$credit_vs_income))
sum(is.na(final_data$annuity_vs_income))
sum(is.na(final_data$price_vs_loan))
sum(is.na(final_data$status_flag))
sum(is.na(final_data$CREDIT_TYPE))
sum(is.na(final_data$overdue_ratio))
sum(is.na(final_data$max_dpd))
sum(is.na(final_data$max_prolong))
#sum(is.na(final_data$count_overdue))
sum(is.na(final_data$class))


#GONNA HAVE TO CLEAN (TEST)
sum(is.na(test_data$credit_vs_income))
sum(is.na(test_data$annuity_vs_income))
sum(is.na(test_data$price_vs_loan))
sum(is.na(test_data$status_flag))
sum(is.na(test_data$CREDIT_TYPE))
sum(is.na(test_data$overdue_ratio))
sum(is.na(test_data$max_dpd))
sum(is.na(test_data$max_prolong))
#sum(is.na(test_data$count_overdue))

#COMPUTE VALUES
#final_data$annuity_vs_income <- ifelse(is.na(final_data$annuity_vs_income), mean(final_data$annuity_vs_income, na.rm = TRUE), final_data$annuity_vs_income)
#final_data$price_vs_loan <- ifelse(is.na(final_data$price_vs_loan), mean(final_data$price_vs_loan, na.rm = TRUE), final_data$price_vs_loan)
#final_data$annuity_vs_income <- ifelse(is.na(final_data$annuity_vs_income), mean(final_data$annuity_vs_income, na.rm = TRUE), final_data$annuity_vs_income)
#final_data$annuity_vs_income <- ifelse(is.na(final_data$annuity_vs_income), mean(final_data$annuity_vs_income, na.rm = TRUE), final_data$annuity_vs_income)
#final_data$CREDIT_TYPE <- addNA(final_data$CREDIT_TYPE)
#final_data$CREDIT_TYPE <- ifelse(is.na(final_data$CREDIT_TYPE), as.factor(NA), as.factor(final_data$CREDIT_TYPE))
#levels <- final_data$CREDIT_TYPE
#levels[length(levels) + 1] <- "None"
#final_data$CREDIT_TYPE <- factor(final_data$CREDIT_TYPE, levels = levels)
#final_data$CREDIT_TYPE[is.na(final_data$CREDIT_TYPE)] <- "None"
#levels(final_data$CREDIT_TYPE)
#test_data$credit

#final_data$class <- as.numeric(final_data$class)
#final_data$max_dpd <- as.numeric(final_data$max_dpd)

#THIS WILL HOPEFULLY FIX THE "ALL 0 OUTPUT ISSUE"... NOW THEYRE MAJORITY 1 WTF
pos_cases <- application_train[application_train$TARGET == 1,c('SK_ID_CURR')]
neg_cases <- application_train[application_train$TARGET == 0,c('SK_ID_CURR')]
#GOING TO TRY TO USE AS MUCH DATA AS POSSIBLE
neg_index <- sample(neg_cases, length(pos_cases)*2)
balanced_ids <- c(pos_cases, neg_cases[neg_index])
balanced_data <- data.frame(id=balanced_ids)

balanced_data <- merge(balanced_data, final_data, by = 'id')

#simple nn
#ONLY USING THE GENERATED FEATURES FOR FIRST ITERATION
#FURTHER ANALYSIS WILL BE DONE ON ADDING MORE FEATURES
#PROVIDED IN THE DATASET

library(randomForest)
final_data <- na.roughfix(final_data)
test_data <- na.roughfix(test_data)
balanced_data <- na.roughfix(balanced_data)

if(!require('nnet')) install.packages('nnet')
library(nnet)

n <- names(balanced_data)
f <- as.formula(paste("class ~", paste(n[!n %in% c("class", "id")], collapse = " + ")))
f
final_data$class <- as.factor(final_data$class)
levels(final_data$class)
balanced_data$class <- as.factor(balanced_data$class)
final_scaled <- final_data
final_scaled$flag_own_car <- as.numeric(final_scaled$flag_own_car)
final_scaled$flag_own_reality <- as.numeric(final_scaled$flag_own_reality)
final_scaled$contract_type <- as.numeric(final_scaled$contract_type)
final_scaled$CREDIT_TYPE <- as.numeric(final_scaled$CREDIT_TYPE)
final_scaled[,!names(final_scaled) %in% c('id', 'class')] <- scale(final_scaled[,!names(final_scaled) %in% c('id', 'class')])

balanced_scaled <- balanced_data
balanced_scaled$flag_own_car <- as.numeric(balanced_scaled$flag_own_car)
balanced_scaled$flag_own_reality <- as.numeric(balanced_scaled$flag_own_reality)
balanced_scaled$contract_type <- as.numeric(balanced_scaled$contract_type)
balanced_scaled$CREDIT_TYPE <- as.numeric(balanced_scaled$CREDIT_TYPE)
balanced_scaled[,!names(balanced_scaled) %in% c('id', 'class')] <- scale(balanced_scaled[,!names(balanced_scaled) %in% c('id', 'class')])
#DONT RUN PAST HERE... IDK WHAT WILL HAPPEN
ann <- nnet(f, data=balanced_scaled, size=50, MaxNWts = 10000, maxit = 4000, abstol = .1)
#if(!require('e1071')) install.packages('e1071')
#library(e1071)
#cross_val <- tune.nnet(class~., MaxNWts = 10000, data=final_data, size = 10)

test_scaled <- test_data
test_scaled$flag_own_car <- as.numeric(test_scaled$flag_own_car)
test_scaled$flag_own_reality <- as.numeric(test_scaled$flag_own_reality)
test_scaled$contract_type <- as.numeric(test_scaled$contract_type)
test_scaled$CREDIT_TYPE <- as.numeric(test_scaled$CREDIT_TYPE)
lengths(test_scaled)
test_scaled[,!names(test_scaled) %in% c('id')] <- scale(test_scaled[,!names(test_scaled) %in% c('id')])


ann_pred <- predict(ann, newdata = test_scaled, type='class')
#sum(ann_pred)

head(ann_pred)
sum(as.numeric(ann_pred))
sum(ann_pred[,1])

count(ann_pred, na.rm = T)

#stupid hack that should never be used
#ann_pred[is.na(ann_pred)] = 1

if(!require('caret')) install.packages('caret')
library(caret)

if(!require('neuralnet')) install.packages('neuralnet')
library(neuralnet)

#apparently this is a bug
m <- model.matrix( 
  ~., 
  data = final_data
)

n <- names(final_data)
f <- as.formula(paste("class ~", paste(n[!n %in% c("class", "id", 'max_dpd')], collapse = " + ")))
f
nn <- neuralnet(f, data=final_data, hidden=100, threshold=1)

X <- final_data[ ,!(names(final_data) %in% c('class'))]

if(!require('randomForest')) install.packages('randomForest')
library(randomForest)
rf_data <- final_data
use_cols <- c('class', 'days_birth', 'overdue_ratio', 'annuity_vs_income', 'days_emp', 'credit_vs_income', 'amt_income_total', 'price_vs_loan')
rf_data$class <- as.factor(rf_data$class)
rf_data <- rf_data[,use_cols]
f <- as.formula(paste("class ~", paste(use_cols[!use_cols %in% c("class")], collapse = " + ")))
mtry <- tuneRF(rf_data[-1], rf_data$class, ntreeTry=200,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)

model <- randomForest(f, data=rf_data, ntree=200, mtry=2)

#Feature Importance
varImpPlot(model)
print(model)

rf_pred <- predict(model,newdata = test_data,type = "class")
print(rf_pred)

if(!require('gbm')) install.packages('gbm')
library(gbm)
rf_data <- rf_data[,!names(rf_data) %in% c('max_dpd')]
rf_data$class <- application_train$TARGET
attach(rf_data)
head(rf_data)
print(names(rf_data))
gbm <- gbm(f, rf_data, n.trees = 10, distribution = "bernoulli")
detach(rf_data)
gbm_pred <- predict(gbm, newdata = test_data, n.trees = 10)
print(gbm_pred)

out_data <- data.frame(SK_ID_CURR=application_test$SK_ID_CURR)
out_data$SK_ID_CURR <- application_test$SK_ID_CURR 
out_data$TARGET <- rf_pred


#UHHH WHAaAaAaAaT???? TODO
#out_data <- temp_data[,c("Loan_ID","Loan_Status")]
write.csv(out_data,"to_submit1.csv", row.names = F)
