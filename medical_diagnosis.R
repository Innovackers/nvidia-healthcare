# gender + employment_status + education + marital_status + children + avg_commute + daily_internet_use + available_vehicles + military_service
library(liquidSVM)
library(caTools)
library(ROCR)
library(caret)

setwd("C:/Users/roshan.shetty/Desktop/Hack2Innovate")

med_data <- read.csv("disease.csv", header = T, stringsAsFactors = F)

# Gender
med_data$gender <- ifelse(med_data$gender == "male", 1, 0)

# Age
currDate <- as.Date("2017-11-18")
med_data$dob <- as.Date(med_data$dob, format = "%m/%d/%Y")
med_data$age <- round((currDate - med_data$dob)/365, 0)
med_data$age <- as.numeric(med_data$age)
med_data$binage <- cut(med_data$age, breaks = c(18,40,50,60,70,80,90,100), right = FALSE, labels = FALSE)

# Employment Status
med_data$employment_status[which(med_data$employment_status == "employed")] <- 1
med_data$employment_status[which(med_data$employment_status == "retired")] <- 2
med_data$employment_status[which(med_data$employment_status == "student")] <- 3
med_data$employment_status[which(med_data$employment_status == "unemployed")] <- 4
med_data$employment_status <- as.numeric(med_data$employment_status)

# education
med_data$education[which(med_data$education == "bachelors")] <- 1
med_data$education[which(med_data$education == "highschool")] <- 2
med_data$education[which(med_data$education == "masters")] <- 3
med_data$education[which(med_data$education == "phd/md")] <- 4
med_data$education <- as.numeric(med_data$education)

# Marital Status
med_data$marital_status[which(med_data$marital_status == "married")] <- 1
med_data$marital_status[which(med_data$marital_status == "single")] <- 2
med_data$marital_status <- as.numeric(med_data$marital_status)

# Ancestry
med_data$ancestry[which(med_data$ancestry == "Austria")] <- 1
med_data$ancestry[which(med_data$ancestry == "Belgium")] <- 2
med_data$ancestry[which(med_data$ancestry == "Czech Republic")] <- 3
med_data$ancestry[which(med_data$ancestry == "Denmark")] <- 4
med_data$ancestry[which(med_data$ancestry == "England")] <- 5
med_data$ancestry[which(med_data$ancestry == "Finland")] <- 6
med_data$ancestry[which(med_data$ancestry == "France")] <- 7
med_data$ancestry[which(med_data$ancestry == "Germany")] <- 8
med_data$ancestry[which(med_data$ancestry == "Hungary")] <- 9
med_data$ancestry[which(med_data$ancestry == "Ireland")] <- 10
med_data$ancestry[which(med_data$ancestry == "Italy")] <- 11
med_data$ancestry[which(med_data$ancestry == "Netherlands")] <- 12
med_data$ancestry[which(med_data$ancestry == "Poland")] <- 13
med_data$ancestry[which(med_data$ancestry == "Portugal")] <- 14
med_data$ancestry[which(med_data$ancestry == "Russia")] <- 15
med_data$ancestry[which(med_data$ancestry == "Scotland")] <- 16
med_data$ancestry[which(med_data$ancestry == "Spain")] <- 17
med_data$ancestry[which(med_data$ancestry == "Sweden")] <- 18
med_data$ancestry[which(med_data$ancestry == "Switzerland")] <- 19
med_data$ancestry[which(med_data$ancestry == "Ukraine")] <- 20
med_data$ancestry <- as.numeric(med_data$ancestry)

# Military Service
med_data$military_service[which(med_data$military_service == "yes")] <- 1
med_data$military_service[which(med_data$military_service == "no")] <- 0
med_data$military_service <- as.numeric(med_data$military_service)

# Disease
med_data$disease[which(med_data$disease == "Alzheimer's disease")] <- 1
med_data$disease[which(med_data$disease == "breast cancer")] <- 2
med_data$disease[which(med_data$disease == "diabetes")] <- 3
med_data$disease[which(med_data$disease == "endometriosis")] <- 4
med_data$disease[which(med_data$disease == "gastritis")] <- 5
med_data$disease[which(med_data$disease == "heart disease")] <- 6
med_data$disease[which(med_data$disease == "HIV/AIDS")] <- 7
med_data$disease[which(med_data$disease == "hypertension")] <- 8
med_data$disease[which(med_data$disease == "kidney disease")] <- 9
med_data$disease[which(med_data$disease == "multiple sclerosis")] <- 10
med_data$disease[which(med_data$disease == "prostate cancer")] <- 11
med_data$disease[which(med_data$disease == "schizophrenia")] <- 12
med_data$disease[which(med_data$disease == "skin cancer")] <- 13
med_data$disease <- as.numeric(med_data$disease)

med_data$id <- NULL
med_data$dob <- NULL
med_data$zipcode <- NULL
med_data$age <- NULL

# Splitting into train and test data sets
smp_size <- floor(0.8 * nrow(med_data))
train_ind <- sample(seq_len(nrow(med_data)), size = smp_size)
med_data_train <- med_data[train_ind,]
med_data_test <- med_data[-train_ind,]

# Running Liquid SVM Model for multiclass classification
model <- mcSVM(disease ~ gender + employment_status + marital_status + children, med_data_train, mc_type = "OvA_ls", display = 1)

preds <- predict(model, med_data_test)
sqrt(mean((med_data_test$disease - preds)*(med_data_test$disease - preds)))

#confusion matrix
confusionMatrix(preds, med_data_test$disease)

# Recall Precision Curve
pred <- prediction(preds, med_data_test$disease)
RP.perf <- performance(preds, "prec", "rec")
plot(RP.perf)