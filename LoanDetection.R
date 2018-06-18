#change this to your wd when you pull latest
dir <- 'Documents/R/HomeLoanDetection'

#setwd
getwd()
setwd(dir)

#load data
application_train <- read.csv('Data/application_train.csv')
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
app_bur <- merge(application_train, bureau, by='SK_ID_CURR', all.x = TRUE)

#THIS ISNT USED RN, SAME THING BUT INNER JOINED
#app_bur_inner <- merge(application_train, bureau, by='SK_ID_CURR')


#feature generation
final_data <- data.frame(id=application_train$SK_ID_CURR)
final_data$class <- application_train$TARGET
final_data$credit_vs_income <- application_train$AMT_CREDIT/application_train$AMT_INCOME_TOTAL
final_data$annuity_vs_income <- application_train$AMT_ANNUITY/application_train$AMT_INCOME_TOTAL
final_data$price_vs_loan <- application_train$AMT_GOODS_PRICE/application_train$AMT_CREDIT

#Generate feature for bad debt or sold credit report status
bad_records <- filter(app_bur, app_bur$CREDIT_ACTIVE %in% c('Bad debt', 'Sold'))
#I'm dumb AF
#bad_records <- app_bur[app_bur, CREDIT_ACTIVE %in% c('Bad debt', 'Sold')]
#bad_records <- lapply(bad_records, is.list)
final_data$status_flag <- ifelse(final_data$id %in% bad_records$SK_ID_CURR, 1, 0)
#5243 bad status flags
sum(final_data$status_flag == 1)

#Credit Type - Cash, Car, etc.
#THIS IS JUST WHAT TYPE OF LOAN, MAJORITY ARE JUST CONSUMER CREDIT(CREDIT CARDS)
#TWWB DUNNO WHAT CHANGED, GOING TO OMIT AND REVISIT
#final_data$credit_type <- app_bur[match(unique(app_bur$SK_ID_BUREAU), app_bur$SK_ID_BUREAU),]

levels(bureau$CREDIT_TYPE)

#final_data$CREDIT_TYPE <- 
status_id <- app_bur[!duplicated(app_bur$SK_ID_CURR),c('SK_ID_CURR', 'CREDIT_TYPE')]
head(status_id, n=100)
final_data <- merge(final_data, status_id, by.x = c('id'), by.y = c('SK_ID_CURR'), all.x = TRUE)

head(final_data, n=100)

max_overdue <- aggregate(app_bur$AMT_CREDIT_MAX_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

max_overdue <- merge(max_overdue, application_train[,c('SK_ID_CURR', 'AMT_CREDIT')], by.x = c('Group.1'), by.y = c('SK_ID_CURR'))
max_overdue$overdue_ratio <- max_overdue$Group.1/max_overdue$AMT_CREDIT

final_data <- merge(final_data, max_overdue[,c('Group.1', 'overdue_ratio')], by.x = c('id'), by.y = c('Group.1'))

max_dpd <- aggregate(app_bur$CREDIT_DAY_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

final_data <- merge(final_data, max_dpd, by.x = 'id', by.y = 'Group.1')

final_data$max_dpd <- final_data$x

max_prolong <- aggregate(app_bur$CNT_CREDIT_PROLONG, by = list(app_bur$SK_ID_CURR), max)
final_data <- merge(final_data, max_prolong, by.x = 'id', by.y = 'Group.1')
final_data$max_prolong <- final_data$x.y

count_overdue <- as.data.frame(table(unique(app_bur[app_bur$AMT_CREDIT_SUM_OVERDUE > 0,])$SK_ID_CURR))
#total overdue balance count payments = 3644
sum(count_overdue$Freq)
#max overdue balance count = 8
max(count_overdue$Freq)

final_data <- merge(final_data, count_overdue, by.x = 'id', by.y = 'Var1', all.x = TRUE)

final_data$count_overdue <- final_data$Freq

#ADD TARGET AND SHIT
final_data <- merge(final_data, app_bur[,c('SK_ID_CURR', 'TARGET')], by.x = 'id', by.y = 'SK_ID_CURR')

#DROP COLUMNS THAT NEEDED TO BE RENAMED 
#THIS IS A RESULT OF PACKAGES RENAMING BY DEFAULT
drops <- c('x.x', 'x.y', 'Freq')
final_data <- final_data[,!(names(final_data) %in% drops)]

#RECREATE THE FEATURES FOR TEST
test_data <- data.frame(id=application_test$SK_ID_CURR)
test_data$class <- application_test$TARGET
test_data$credit_vs_income <- application_test$AMT_CREDIT/application_test$AMT_INCOME_TOTAL
test_data$annuity_vs_income <- application_test$AMT_ANNUITY/application_test$AMT_INCOME_TOTAL
test_data$price_vs_loan <- application_test$AMT_GOODS_PRICE/application_test$AMT_CREDIT

#Generate feature for bad debt or sold credit report status
bad_records <- filter(app_bur, app_bur$CREDIT_ACTIVE %in% c('Bad debt', 'Sold'))
test_data$status_flag <- ifelse(test_data$id %in% bad_records$SK_ID_CURR, 1, 0)
count(bad_records)

#5243 records esketit
sum(test_data$status_flag == 1)

#Credit Type - Cash, Car, etc.
test_data$credit_type <- app_bur[match(unique(app_bur$SK_ID_BUREAU), app_bur$SK_ID_BUREAU),]

#app_bur %>% group_by(SK_ID_CURR) %>% 

levels(bureau$CREDIT_TYPE)

#final_data$CREDIT_TYPE <- 
status_id <- app_bur[!duplicated(app_bur$SK_ID_CURR),c('SK_ID_CURR', 'CREDIT_TYPE')]
head(status_id, n=100)
test_data <- merge(test_data, status_id, by.x = c('id'), by.y = c('SK_ID_CURR'), all.x = TRUE)

head(test_data, n=100)

max_overdue <- aggregate(app_bur$AMT_CREDIT_MAX_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

max_overdue <- merge(max_overdue, application_test[,c('SK_ID_CURR', 'AMT_CREDIT')], by.x = c('Group.1'), by.y = c('SK_ID_CURR'))
max_overdue$overdue_ratio <- max_overdue$Group.1/max_overdue$AMT_CREDIT

test_data <- merge(test_data, max_overdue[,c('Group.1', 'overdue_ratio')], by.x = c('id'), by.y = c('Group.1'))

max_dpd <- aggregate(app_bur$CREDIT_DAY_OVERDUE, by = list(app_bur$SK_ID_CURR), max)

test_data <- merge(test_data, max_dpd, by.x = 'id', by.y = 'Group.1')

test_data$max_dpd <- test_data$x

max_prolong <- aggregate(app_bur$CNT_CREDIT_PROLONG, by = list(app_bur$SK_ID_CURR), max)
test_data <- merge(test_data, max_prolong, by.x = 'id', by.y = 'Group.1')
test_data$max_prolong <- test_data$x.y

count_overdue <- as.data.frame(table(unique(app_bur[app_bur$AMT_CREDIT_SUM_OVERDUE > 0,])$SK_ID_CURR))
#total overdue balance count payments = 3644
sum(count_overdue$Freq)
#max overdue balance count = 8
max(count_overdue$Freq)

test_data <- merge(test_data, count_overdue, by.x = 'id', by.y = 'Var1', all.x = TRUE)

test_data$count_overdue <- test_data$Freq

#ADD TARGET AND SHIT (NVM I'M DUMB)
test_data <- merge(test_data, app_bur[,c('SK_ID_CURR', 'TARGET')], by.x = 'id', by.y = 'SK_ID_CURR')

#DROP COLUMNS THAT NEEDED TO BE RENAMED 
#THIS IS A RESULT OF PACKAGES RENAMING BY DEFAULT
drops <- c('x.x', 'x.y', 'Freq', 'TARGET')
test_data <- test_data[,!(names(test_data) %in% drops)]

#simple nn
#ONLY USING THE GENERATED FEATURES FOR FIRST ITERATION
#FURTHER ANALYSIS WILL BE DONE ON ADDING MORE FEATURES
#PROVIDED IN THE DATASET
if(!require('nnet')) install.packages('nnet')
library(nnet)

#DONT RUN PAST HERE... IDK WHAT WILL HAPPEN
ann <- nnet(class~., MaxNWts = 10000, data=final_data, size = 50)
if(!require('e1071')) install.packages('e1071')
library(e1071)
cross_val <- tune.nnet(class~., MaxNWts = 10000, data=final_data, size = 50)

ann_pred <- predict(ann, newdata = application_test, type='raw')

if(!require('caret')) install.packages('caret')
library(caret)

confusionMatrix(ann_pred, , positive=levels(test_data$gen_election.class[2]))