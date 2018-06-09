#change this to your wd when you pull latest
dir <- 'Documents/R/HomeLoanDetection'

#setwd
getwd()
setwd(dir)

#load data
application_train <- read.csv('Data/application_train.csv')
bureau <-read.csv('Data/bureau.csv')
bureau_balance <- read.csv('Data/bureau_balance.csv')
cash_balance <- read.csv('Data/POS_CASH_balance.csv')
credit_card_balance <- read.csv('Data/credit_card_balance.csv')
installments_payments <- read.csv('Data/installments_payments.csv')
previous_application <- read.csv('Data/previous_application.csv')
application_test <- read.csv('Data/application_test.csv')

#preview
head(application_train)
head(bureau)
head(bureau_balance)
head(credit_card_balance)
head(installments_payments)

#drop shit
#application_train <- application_train[,!names(data) %in% c('cand_id', 'last_name', 'first_name', 'twitterbirth', 'facebookdate', 'facebookjan', 'youtubebirth')]

#joins
#temp <- merge(x = application_train, y = previous_application, by = 'SK_ID_CURR', ALL.x = TRUE)
#head(temp)

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

#Mode
sapply(dplyr::select_if(application_train, is.factor), mode)
print(dplyr::select_if(application_train, is.factor))

#plots
d <- density(application_train$AMT_INCOME_TOTAL)
plot(d) 

print(temp[1:300,])

levels(bureau$CREDIT_ACTIVE)

app_bur <- merge(application_train, bureau, by='SK_ID_CURR', how='left')

app_bur_inner <- merge(application_train, bureau, by='SK_ID_CURR')

#feature generation
final_data <- data.frame(id=application_train$SK_ID_CURR)
final_data$class <- application_train$TARGET
final_data$credit_vs_income <- application_train$AMT_CREDIT/application_train$AMT_INCOME_TOTAL
final_data$annuity_vs_income <- application_train$AMT_ANNUITY/application_train$AMT_INCOME_TOTAL
final_data$price_vs_loan <- application_train$AMT_GOODS_PRICE/application_train$AMT_CREDIT

#Generate feature for bad debt or sold credit report status
bad_records <- filter(app_bur, app_bur$CREDIT_ACTIVE %in% c('Bad debt', 'Sold'))
final_data$status_flag <- ifelse(final_data$id %in% bad_records$SK_ID_CURR, 1, 0)
#5243 records esketit
sum(final_data$status_flag == 1)

#Credit Type - Cash, Car, etc.
final_data$credit_type <- app_bur[match(unique(app_bur$SK_ID_BUREAU), app_bur$SK_ID_BUREAU),]

#app_bur %>% group_by(SK_ID_CURR) %>% 
  
levels(bureau$CREDIT_TYPE)

final_data <- app_bur %>%
  group_by(SK) %>%
  select('CREDIT_TYPE') %>%
  slice(1) %>%
  ungroup

#simple nn
if(!require('nnet')) install.packages('nnet')
library(nnet)

ann <- nnet(TARGET~., MaxNWts = 10000, data=application_train, size = 30)

ann_pred <- predict(ann, newdata = application_test, type='raw')

if(!require('caret')) install.packages('caret')
library(caret)

confusionMatrix(ann_pred, , positive=levels(test_data$gen_election.class[2]))