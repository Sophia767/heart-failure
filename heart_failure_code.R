install.packages("gmodels")
library(gmodels) # CrossTable()
install.packages("naniar")
library(naniar) # gg_miss_var()
install.packages("mice")
library(mice) # NAs imputation
install.packages("ggplot2")
library(ggplot2) # ggplot()
install.packages("corrplot")
library(corrplot)
library(reshape2) # melt()
library(GGally) # ggpairs()
install.packages('FactoMineR')
library(FactoMineR) # PCA
install.packages('caret')
library(caret) # k-fold
install.packages("RWeka")
library(RWeka) # JRip
install.packages("caTools")
library(caTools) # logistic regression
install.packages("rpart")
library("rpart") # decision tree
install.packages("correlation")
library(correlation) # correlation test
install.packages('randomForest')
library(randomForest)
library(class) # K-NN
install.packages('e1071')
library(e1071) # naive bayes, pca
install.packages('xgboost')
library(xgboost)
install.packages('pROC')
library(pROC)

options(scipen = FALSE)
options(ggrepel.max.overlaps = Inf)

############################# IMPORTING & MERGING ##############################

setwd("G:/My Drive/NCI/Semester 8/Software Project/Submissions/FINAL")
getwd()

# read the heart failures dataset
data <- read.csv("mimic3_heart_failures.csv")
head(data)

# read the patients dataset
pa <- read.csv("mimic3_patients.csv")
# keep only the columns of interest
pa <- data.frame(pa$hadm_id, pa$age, pa$ethnicity, pa$gender)
head(pa)

# change the patient "hadm_id" col name to "ID" for merging
colnames(pa)[1] <- "ID"
# merge data and patients
hf <- merge(data, pa, by="ID",all.x = TRUE)

# remove group column, keep merged dataset only
hf <- subset(hf, select = c(-group))
remove(data, pa)

# there are two age columns and two gender columns, we need to check if they match
# add a column that concatenates both gender columns
hf$gender_concat <- paste(hf$gendera, hf$pa.gender)
# create a table that shows the number of combinations found
table(hf$gender_concat)
# only two combinations: 1M and 2F which means that the two columns have matching data
# we can also see that 558 patients are Male and 618 are Female
# we can keep both in the hf dataframe and use meaningful col names
colnames(hf)[which(names(hf) == "gendera")] <- "gender_num"
colnames(hf)[which(names(hf) == "pa.gender")] <- "gender_desc"
# drop gender_concat column
hf <- subset(hf, select = c(-gender_concat))
hf$gender_desc[hf$gender_desc == "F"] = "female"
hf$gender_desc[hf$gender_desc == "M"] = "male"
hf$gender_desc <- as.factor(hf$gender_desc)

# add a column that shows the difference between the two age columns
hf$agediff <- hf$age - hf$pa.age
head(hf$agediff, 50)
min(hf$agediff)
# min diff is 0: age from heart failure data is equal to or higher than age from patient data
# the difference can be explained by two different ages used
# age from patient data is age at first hospital admission
# age from heart failure data is age at the time of heart failure
# we are only interested in the age at heart failure so we can discard the age from patient data
hf <- subset(hf, select = c(-pa.age, -agediff))


################################## CLEANING ####################################

# rename columns
names(hf)[names(hf) == "BMI"] <- "body_mass_index"
names(hf)[names(hf) == "hypertensive"] <- "hypertension"
names(hf)[names(hf) == "atrialfibrillation"] <- "atrial_fibrillation"
names(hf)[names(hf) == "CHD.with.no.MI"] <- "ischaemic_heart_disease"
names(hf)[names(hf) == "diabetes"] <- "diabetes_mellitus"
names(hf)[names(hf) == "deficiencyanemias"] <- "hypoferric_anaemia"
names(hf)[names(hf) == "Hyperlipemia"] <- "hyperlipidaemia"
names(hf)[names(hf) == "Renal.failure"] <- "chronic_kidney_disease"
names(hf)[names(hf) == "COPD"] <- "chronic_obstructive_pulmonary_disease"
names(hf)[names(hf) == "heart.rate"] <- "heart_rate"
names(hf)[names(hf) == "Systolic.blood.pressure"] <- "systolic_blood_pressure"
names(hf)[names(hf) == "Diastolic.blood.pressure"] <- "diastolic_blood_pressure"
names(hf)[names(hf) == "Respiratory.rate"] <- "respiratory_rate"
names(hf)[names(hf) == "temperature"] <- "body_temperature"
names(hf)[names(hf) == "SP.O2"] <- "saturation_pulse_oxygen"
names(hf)[names(hf) == "Urine.output"] <- "urine_output"
names(hf)[names(hf) == "hematocrit"] <- "haematocrit"
names(hf)[names(hf) == "RBC"] <- "red_blood_cells"
names(hf)[names(hf) == "MCH"] <- "mean_corpuscular_haemoglobin"
names(hf)[names(hf) == "MCHC"] <- "mean_corpuscular_haemoglobin_concentration"
names(hf)[names(hf) == "MCV"] <- "mean_corpuscular_volume"
names(hf)[names(hf) == "RDW"] <- "red_blood_cell_distribution_width"
names(hf)[names(hf) == "Leucocyte"] <- "white_blood_cells"
names(hf)[names(hf) == "Platelets"] <- "platelet_count"
names(hf)[names(hf) == "Neutrophils"] <- "neutrophils"
names(hf)[names(hf) == "Basophils"] <- "basophils"
names(hf)[names(hf) == "Lymphocyte"] <- "lymphocytes"
names(hf)[names(hf) == "PT"] <- "prothrombin_time"
names(hf)[names(hf) == "INR"] <- "international_normalised_ratio"
names(hf)[names(hf) == "NT.proBNP"] <- "NT_proBNP"
names(hf)[names(hf) == "Creatine.kinase"] <- "creatine_kinase"
names(hf)[names(hf) == "Creatinine"] <- "creatinine"
names(hf)[names(hf) == "Urea.nitrogen"] <- "blood_urea_nitrogen"
names(hf)[names(hf) == "Blood.potassium"] <- "potassium"
names(hf)[names(hf) == "Blood.sodium"] <- "sodium"
names(hf)[names(hf) == "Blood.calcium"] <- "calcium"
names(hf)[names(hf) == "Chloride"] <- "chloride"
names(hf)[names(hf) == "Anion.gap"] <- "anion_gap"
names(hf)[names(hf) == "Magnesium.ion"] <- "magnesium"
names(hf)[names(hf) == "PH"] <- "pH"
names(hf)[names(hf) == "Bicarbonate"] <- "bicarbonate"
names(hf)[names(hf) == "Lactic.acid"] <- "lactate"
names(hf)[names(hf) == "PCO2"] <- "CO2_partial_pressure"
names(hf)[names(hf) == "EF"] <- "left_ventricular_ejection_fraction"

# add outcome description as factor
hf$outcome_desc[hf$outcome == 0] <- "survived"
hf$outcome_desc[hf$outcome == 1] <- "died"
hf$outcome_desc <- as.factor(hf$outcome_desc)

# rename ethnicity column
names(hf)[names(hf) == "pa.ethnicity"] <- "ethnicity_desc"

# verify all observations are labelled
hf[is.na(hf$outcome),] # one label missing
hf <- hf[!is.na(hf$outcome),] # drop record
CrossTable(hf$outcome_desc, digits = 4) # 13.52% did not survive

# export file to csv
write.csv(hf,"G:/My Drive/NCI/Semester 8/Software Project/Submissions/FINAL/mimic3_merged.csv", row.names = FALSE)
hf <- subset(hf, select = c(-ID, -gender_desc)) # only needed for EDA in PBI


################################## IMPUTATION ##################################

# impute ethnicity
hf$ethnicity_num <- hf$ethnicity_desc
hf$ethnicity_num[hf$ethnicity_num == "AMERICAN INDIAN/ALASKA NATIVE"] <- 1
hf$ethnicity_num[hf$ethnicity_num == "ASIAN"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "ASIAN - ASIAN INDIAN"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "ASIAN - CAMBODIAN"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "ASIAN - CHINESE"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "ASIAN - FILIPINO"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "ASIAN - VIETNAMESE"] <- 2
hf$ethnicity_num[hf$ethnicity_num == "BLACK/AFRICAN"] <- 3
hf$ethnicity_num[hf$ethnicity_num == "BLACK/AFRICAN AMERICAN"] <- 3
hf$ethnicity_num[hf$ethnicity_num == "BLACK/CAPE VERDEAN"] <- 3
hf$ethnicity_num[hf$ethnicity_num == "BLACK/HAITIAN"] <- 3
hf$ethnicity_num[hf$ethnicity_num == "HISPANIC OR LATINO"] <- 4
hf$ethnicity_num[hf$ethnicity_num == "HISPANIC/LATINO - PUERTO RICAN"] <- 4
hf$ethnicity_num[hf$ethnicity_num == "MIDDLE EASTERN"] <- 5
hf$ethnicity_num[hf$ethnicity_num == "MULTI RACE ETHNICITY"] <- 6
hf$ethnicity_num[hf$ethnicity_num == "OTHER"] <- 7
hf$ethnicity_num[hf$ethnicity_num == "PORTUGUESE"] <- 8
hf$ethnicity_num[hf$ethnicity_num == "WHITE"] <- 8
hf$ethnicity_num[hf$ethnicity_num == "WHITE - OTHER EUROPEAN"] <- 8
hf$ethnicity_num[hf$ethnicity_num == "WHITE - RUSSIAN"] <- 8
hf$ethnicity_num[hf$ethnicity_num == "PATIENT DECLINED TO ANSWER"] <- 9
hf$ethnicity_num[hf$ethnicity_num == "UNABLE TO OBTAIN"] <- 9
hf$ethnicity_num[hf$ethnicity_num == "UNKNOWN/NOT SPECIFIED"] <- 9
hf$ethnicity_num <- as.integer(hf$ethnicity_num)
hf <- subset(hf, select = c(-ethnicity_desc)) # only needed for EDA in PBI

# visualising missing data
vis_miss(hf, sort_miss = TRUE)
sum(complete.cases(hf)) # 428 complete records
sum(complete.cases(hf))/1176
n_var_miss(hf) # 19 variables missing values
gg_miss_upset(hf, nsets = n_var_miss(hf))

# steps below removed after literature survey:
# remove records missing multiple attributes only if low number of positive outcomes
# hf <- hf[!is.na(hf$heart_rate),] # 13 records dropped (1 died), 16 variables missing values remain 
# hf <- hf[!is.na(hf$systolic_blood_pressure),] # 3 records dropped, 14 variables missing values remain
# hf <- hf[!is.na(hf$prothrombin_time),] # 20 records dropped (1 died), 12 variables missing values remain
# hf <- hf[!is.na(hf$urine_output),] # 22 records dropped (2 died), 11 variables missing values remain
# proportion of patients who did not survive after dropping records missing data
# CrossTable(hf$outcome_desc, digits = 4) # 13.85%

gg_miss_fct(hf, fct = outcome_desc)

# mice imputation for na's
mice_mod <- mice(hf, seed = 123)
loggedEvents <- mice_mod$loggedEvents
loggedEvents
# 2 columns with collinearity identified: outcome and outcome_desc

# complete missing values
hf <- complete(mice_mod)

# verify no NA's left
md.pattern(hf, plot = FALSE)
remove(mice_mod, loggedEvents)

# export file to csv
write.csv(hf,"G:/My Drive/NCI/Semester 8/Software Project/Submissions/FINAL/mimic3_merged_imputed.csv", row.names = FALSE)



############################## FEATURES SELECTION ##############################

# CORRELATION ANALYSIS

# correlation coefficient of all variables
hf_num <- subset(hf, select = c(-outcome_desc))
hf_cor <- as.data.frame(cor(hf_num))
hf_cor_abs <- abs(hf_cor)
hf_cor_abs
# top 5 correlations w/ outcome: anion_gap, bicarbonate, lactate, white_blood_cells, blood_urea_nitrogen
# corrplot
hf_cor_top <- subset(hf, select = c(outcome, anion_gap, bicarbonate, lactate, white_blood_cells, blood_urea_nitrogen))
corrplot(cor(hf_cor_top), method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black")
# anion gap and bicarbonate/bun are strongly correlated (explained in top 5 variables description)
# boxplot of correlated values
hf_cor_top <- subset(hf, select = c(outcome_desc, anion_gap, bicarbonate, lactate, white_blood_cells, blood_urea_nitrogen))
hf_cor_top_m <- melt(hf_cor_top, id.vars = c("outcome_desc"))
ggplot(data = hf_cor_top_m, aes(x = variable, y = value)) + geom_boxplot(aes(fill = outcome_desc)) +
  xlab("") + ylab("") + ggtitle("Top Correlated Features Boxplots") + theme(legend.position = "bottom") +
  facet_wrap( ~ variable, scales = "free", nrow = 1)
remove(hf_cor_abs, hf_cor_top, hf_cor_top_m)

# correlation test
# https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/
# the coefficient shows a strong correlation between the outcome and these 5 variables
# but it doesn't mean that the correlation is significantly different from 0 in the population
# null and alternative hypothesis:
# H0: there is no linear relationship between the two variables
# H1: there is a linear relationship between the two variables
set.seed(123)
correlation::correlation(data = subset(hf_cor, select = c(outcome)), data2 = subset(hf_cor, select = c(-outcome)),
                         include_factors = TRUE, method = "auto")

# Correlation Matrix
# Parameter1 |                                 Parameter2 |        r |         95% CI | t(47) |      p
# ----------------------------------------------------------------------------------------------------
# outcome    |                                    lactate |      0.51 | [ 0.27,  0.69] |      4.07 | 0.009**
# outcome    |                                bicarbonate |     -0.49 | [-0.68, -0.24] |     -3.89 | 0.015* 
# outcome    |                               urine_output |     -0.48 | [-0.67, -0.23] |     -3.77 | 0.021*
# outcome    |                                  anion_gap |      0.47 | [ 0.23,  0.66] |      3.73 | 0.024*
# outcome    |                          white_blood_cells |      0.46 | [ 0.21,  0.66] |      3.61 | 0.033*
# outcome    |                    systolic_blood_pressure |     -0.46 | [-0.65, -0.20] |     -3.56 | 0.037*
# outcome    |                        blood_urea_nitrogen |      0.40 | [ 0.13,  0.61] |      2.99 | 0.187  


# PCA

# PCA Principal Component Analysis = find components that maximise variance
# PCA components ordered from highest to lowest information content
# PCA requires data to be scaled
set.seed(123)
pca_obj <- PCA(scale(hf_num[-1]))
summary(pca_obj)
pca_obj$var$contrib
#plot.PCA(pca_obj)

# screeplot
hf_var_explained <- pca_obj$svd$vs^2 / sum(pca_obj$svd$vs^2)
qplot(c(1:49), hf_var_explained) + geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Screeplot") + ylim(0, 0.1)

# most significant features in PC1
# bicarbonate                                10.494498391
# blood_urea_nitrogen                         9.935057108
# anion_gap                                   9.371943602
# creatinine                                  7.290017556
# NT_proBNP                                   6.950161782


# DECISION TREE

# create classifier
set.seed(123)
rpart_obj <- rpart(formula = outcome ~ ., data = hf_num, method = "class")
plot(rpart_obj)
text(rpart_obj)
rpart_obj$variable.importance
# anion_gap                               lactate 
# 21.5504987                              17.2999089 
# chronic_kidney_disease                  lymphocytes 
# 12.3306667                              10.7798304 
# red_blood_cells                         creatinine 
# 9.4310160                               9.1516020 
# urine_output                            calcium 
# 8.9071013                               8.1773327 


remove(hf_cor, pca_obj, rpart_obj)

# splitting data into training/test sets - 75% observations going to the training set
set.seed(123)
split <- createDataPartition(hf_num$outcome, p = 0.7, list = FALSE) # stratified split
training_all <- hf_num[split, ]
test_all <- hf_num[-split, ]

# create training/testing sets for each 3 sets of selected features
# correlation analysis
training_cor <- subset(training_all, select = c(outcome, anion_gap, bicarbonate, lactate, white_blood_cells, 
                                                     urine_output, systolic_blood_pressure))
test_cor <- subset(test_all, select = c(outcome, anion_gap, bicarbonate, lactate, white_blood_cells, 
                                             urine_output, systolic_blood_pressure))
# PCA
training_pca <- subset(training_all, select = c(outcome, bicarbonate, blood_urea_nitrogen, anion_gap, 
                                                creatinine, NT_proBNP))
test_pca <- subset(test_all, select = c(outcome, bicarbonate, blood_urea_nitrogen, anion_gap, 
                                        creatinine, NT_proBNP))

# decision tree
training_tree <- subset(training_all, select = c(outcome, anion_gap, lactate, chronic_kidney_disease,
                                                 lymphocytes, red_blood_cells, creatinine,
                                                 urine_output, calcium))
test_tree <-subset(test_all, select = c(outcome, anion_gap, lactate, chronic_kidney_disease,
                                        lymphocytes, red_blood_cells, creatinine,
                                        urine_output, calcium))

# evaluation function
evaluateModel <- function(y_actual, y_pred)
{
  CM <- table(y_actual, y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP) # proportion of positive results that were correctly classified
  recall <- TP / (TP + FN) # this is sensitivity
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  print(paste("Precision: ", round(precision, 2)))
  print(paste("Recall/Sensitivity: ", round(recall, 2)))
  print(paste("f1-score: ", round(f1_score, 2)))
}


############################# CLASSIFICATION MODELS ############################


# # K-Means clustering (originally for unsupervised ML)
# K-Means clustering with 2 clusters --> all variables
set.seed(123)
kmeans_all <- kmeans(hf_num, 2, nstart = 50)
kmeans_all$cluster
kmeans_all_df <- cbind(hf_num, cluster = kmeans_all$cluster)
head(kmeans_all_df)
# confusion matrix - all
cm_km <- CrossTable(hf_num$outcome, kmeans_all$cluster, prop.c = TRUE, prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE)
# cluster 1: 22% died, 78% survived = DIED 1
# cluster 2: 11.5% died, 88.5% survived = SURVIVED 0
(843+49)/1176 # 75.85% accuracy... very low
# cannot be used for feature selection

  
# Model 1: K-NEAREST NEIGHBOR ##################################################

training_set <- training_cor
test_set <- test_cor

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_knn <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  y_pred <- knn(train = training_fold[, -1], test = test_fold[, -1], cl = training_fold[, 1], k = 3, prob = TRUE)
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(F1_score) # return recall or F1
})
round(mean(unlist(cv_knn), na.rm = TRUE), 2)

# fitting K-NN classifier to training set with k = 3
# create classifier
y_pred_knn <- knn(train = training_set[, -1], test = test_set[, -1], cl = training_set[, 1], k = 3, prob = TRUE)
#y_actual_knn <- as.factor(test_set[, 1])
# confusion matrix
#cm_knn <- CrossTable(test_set[, 1], y_pred_knn, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
#evaluateModel(test_set[, 1], y_pred_knn)
#confusionMatrix(y_pred_knn, y_actual_knn, positive = '1', mode = 'everything') # confirming scores with built-in function


# Model 2: LOGISTIC REGRESSION #################################################

training_set <- training_tree
test_set <- test_tree

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_lr <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- glm(formula = outcome ~ ., family = binomial, data = training_fold)
  prob_pred <- predict(classifier, type = 'response', newdata = test_fold[-1])
  y_pred <- ifelse(prob_pred > 0.3, 1, 0)
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(F1_score) # return recall or F1
})
round(mean(unlist(cv_lr), na.rm = TRUE), 2)

# fitting logistic regression classification to training set
# create classifier
classifier_lr <- glm(formula = outcome ~ ., family = binomial, data = training_set)
# glm = generalised linear model
# . = all the independent variables
# predict test set results --> create vector of predicted probabilities of test set observations (probability of outcome = 1)
prob_pred_lr <- predict(classifier_lr, type = 'response', newdata = test_set[-1]) # remove outcome from test set (= prediction)
# convert probabilities into binary
y_pred_lr <- ifelse(prob_pred_lr > 0.3, 1, 0)
# evaluating model (confusion matrix) - vector of real values (1st arg) vs vector of predicted values
cm_lr <- CrossTable(test_set[, 1], y_pred_lr, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
evaluateModel(test_set[, 1], y_pred_lr)


# Model 3: NAIVE BAYES #########################################################

training_set <- training_tree
test_set <- test_tree

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_nb <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- naiveBayes(x = training_fold[-1], y = training_fold$outcome, kernel = TRUE)
  y_pred <- predict(classifier, newdata = test_fold[-1])
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(recall) # return recall or F1
})
round(mean(unlist(cv_nb), na.rm = TRUE), 2)
  
# fitting naive bayes classification to training set
# create classifier
classifier_nb <- naiveBayes(x = training_set[-1], y = training_set$outcome, kernel = TRUE)
# predict test set results
y_pred_nb <- predict(classifier_nb, newdata = test_set[-1])
# confusion matrix
#cm_nb <- CrossTable(test_set[, 1], y_pred_nb, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)


# Model 4: SUPPORT VECTOR MACHINES #############################################

training_set <- training_cor
test_set <- test_cor

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_svm <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  #classifier <- svm(formula = outcome ~ ., data = training_fold, type = 'C-classification', kernel = 'linear')
  #classifier <- svm(formula = outcome ~ ., data = training_fold, type = 'C-classification', kernel = 'sigmoid')
  #classifier <- svm(formula = outcome ~ ., data = training_fold, type = 'C-classification', kernel = 'radial')
  classifier <- svm(formula = outcome ~ ., data = training_fold, type = 'C-classification', kernel = 'polynomial', degree = 5)
  y_pred <- predict(classifier, newdata = test_fold[-1])
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(F1_score) # return recall or F1
})
round(mean(unlist(cv_svm), na.rm = TRUE), 2)

# create classifier: polynomial kernel
classifier_svm_p <- svm(formula = outcome ~ ., data = training_set, type = 'C-classification', kernel = 'polynomial', degree = 5)
# predict test set results
y_pred_svm_p <- predict(classifier_svm_p, newdata = test_set[-1])
# confusion matrix
#cm_svm_p <- CrossTable(test_set[, 1], y_pred_svm_p, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)

# training_set$outcome <- as.factor(training_set$outcome)
# test_set$outcome <- as.factor(test_set$outcome)

# fitting SVM classification to training set
# create classifier
#classifier_svm <- svm(formula = outcome ~ ., data = training_set, type = 'C-classification', kernel = 'linear')
# predict test set results
#y_pred_svm <- predict(classifier_svm, newdata = test_set[-1])
# confusion matrix
#cm_svm <- CrossTable(test_set[, 1], y_pred_svm, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
# 0 death predicted --> data not linearly separable, decision boundary could not be found

# create classifier: radial kernel
#classifier_svm_r <- svm(formula = outcome ~ ., data = training_set, type = 'C-classification', kernel = 'radial')
# predict test set results
#y_pred_svm_r <- predict(classifier_svm_r, newdata = test_set[-1])
# confusion matrix
#cm_svm_r <- CrossTable(test_set[, 1], y_pred_svm_r, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
# 88.44% accuracy
# sensitivity = 25/159 = 15.72%
# specificity = 1015/1017 = 99.80%


### degree = 3 (default)
# 88.52% accuracy
# sensitivity = 30/159 = 18.87%
# specificity = 1011/1017 = 99.41%
### degree = 5
# 89.96% accuracy
# sensitivity = 50/159 = 31.45%
# specificity = 1008/1017 = 99.12%

# create classifier: sigmoid kernel
#classifier_svm_s <- svm(formula = outcome ~ ., data = training_set, type = 'C-classification', kernel = 'sigmoid')
# predict test set results
#y_pred_svm_s <- predict(classifier_svm_s, newdata = test_set[-1])
# confusion matrix
#cm_svm_s <- CrossTable(test_set[, 1], y_pred_svm_s, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
# 81.46% accuracy
# sensitivity = 10/159 = 6.29%
# specificity = 948/1017 = 93.22%


# Model 5: RANDOM FOREST #######################################################

training_set <- training_cor
test_set <- test_cor
training_set$outcome <- as.factor(training_set$outcome)
test_set$outcome <- as.factor(test_set$outcome)

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_rf <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- randomForest(x = training_fold[-1], y = training_fold$outcome, ntree = 60)
  y_pred <- predict(classifier, newdata = test_fold[-1])
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(recall) # return recall or F1
})
round(mean(unlist(cv_rf), na.rm = TRUE), 2)

# fitting random forest classification to training set
# create classifier
#set.seed(123)
classifier_rf <- randomForest(x = training_set[-1], y = training_set$outcome, ntree = 60)
# predict test set results
y_pred_rf <- predict(classifier_rf, newdata = test_set[-1])
# confusion matrix
#cm_rf <- CrossTable(test_set[, 1], y_pred_rf, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)


# Model 6: XGBoost #############################################################

training_set <- training_tree
test_set <- test_tree

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_rf <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- xgboost(data = as.matrix(training_fold[-1]), label = training_fold$outcome, nrounds = 60,
                        max.depth = 6, objective = "binary:logistic")
  prob_pred <- predict(classifier, newdata = as.matrix(test_fold[-1]))
  y_pred <- ifelse(prob_pred > 0.3, 1, 0)
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(F1_score) # return recall or F1
})
round(mean(unlist(cv_rf), na.rm = TRUE), 2)

# fitting XGBoost classification to training set
# create classifier
#set.seed(123)
classifier_xgb <- xgboost(data = as.matrix(training_set[-1]), label = training_set$outcome, nrounds = 60,
                          max.depth = 5, objective = "binary:logistic")
# predict test set results
prob_pred_xgb <- predict(classifier_xgb, newdata = as.matrix(test_set[-1]))
y_pred_xgb <- ifelse(prob_pred_xgb > 0.3, 1, 0) # convert probabilities into 0/1
# confusion matrix
#cm_xgb <- CrossTable(test_set[, 1], y_pred_xgb, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)
# evaluation(test_set[, 1], y_pred_xgb)
#xgb.importance(colnames(training_set[-1]), model = classifier_xgb) # display features


# Model 7: RIPPER ##############################################################

# JRip - Repeated Incremental Pruning to Produce Error Reduction (RIPPER)
# https://medium.com/swlh/the-ripper-algorithm-a5eebbe3661d

training_set <- training_pca
test_set <- test_pca
training_set$outcome <- as.factor(training_set$outcome)
test_set$outcome <- as.factor(test_set$outcome)

# 10-fold cross validation F1
folds <- createFolds(training_set$outcome, k = 10) # create list of folds
cv_rf <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier_rip <- JRip(outcome ~ ., data = training_fold)
  y_pred <- predict(classifier_rip, test_fold)
  CM <- table(test_fold[, 1], y_pred)
  TN <- CM[1,1]
  TP <- CM[2,2]
  FP <- CM[1,2]
  FN <- CM[2,1]
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1_score <- 2 * ((precision * recall) / (precision + recall))
  return(F1_score) # return recall or F1
})
round(mean(unlist(cv_rf), na.rm = TRUE), 2)

# fitting RIPPER classification to training set
# create classifier
#set.seed(123)
classifier_rip <- JRip(outcome ~ ., data = training_set)
# predict test set results
y_pred_rip <- predict(classifier_rip, test_set)
# confusion matrix
#cm_rip <- CrossTable(test_set[, 1], y_pred_rip, prop.chisq = FALSE, prop.c = FALSE, prop.r = TRUE, digits = 4)



# EVALUATION ###################################################################

# Logistic Regression with training tree set had the best performance scores

training_set <- training_tree
test_set <- test_tree

# get summary statistics
summary(classifier_lr)

# ROC curve LR
roc(test_set[, 1], predictor = prob_pred_lr, plot = TRUE, percent = TRUE, print.auc = TRUE, col = "#4daf4a")
# ROC curve XGBoost
plot.roc(test_set[, 1], predictor = prob_pred_xgb, add = TRUE, percent = TRUE, print.auc = TRUE, 
         col = "#377eb8", print.auc.y = 45)

# test 1 (all removed)
training_set1 <- subset(training_tree, select = c(-red_blood_cells, -creatinine, -lactate))
test_set1 <- subset(test_tree, select = c(-red_blood_cells, -creatinine, -lactate))
classifier_lr1 <- glm(formula = outcome ~ ., family = binomial, data = training_set1)
prob_pred_lr1 <- predict(classifier_lr1, type = 'response', newdata = test_set1[-1]) 
y_pred_lr1 <- ifelse(prob_pred_lr1 > 0.3, 1, 0)

# test 2 (+ lactate)
training_set2 <- subset(training_tree, select = c(-red_blood_cells, -creatinine))
test_set2 <- subset(test_tree, select = c(-red_blood_cells, -creatinine))
classifier_lr2 <- glm(formula = outcome ~ ., family = binomial, data = training_set2)
prob_pred_lr2 <- predict(classifier_lr2, type = 'response', newdata = test_set2[-1]) 
y_pred_lr2 <- ifelse(prob_pred_lr2 > 0.3, 1, 0)

# test 3 (+ creatinine)
training_set3 <- subset(training_tree, select = c(-red_blood_cells))
test_set3 <- subset(test_tree, select = c(-red_blood_cells))
classifier_lr3 <- glm(formula = outcome ~ ., family = binomial, data = training_set3)
prob_pred_lr3 <- predict(classifier_lr3, type = 'response', newdata = test_set3[-1]) 
y_pred_lr3 <- ifelse(prob_pred_lr3 > 0.3, 1, 0)

# test 4 (+ red blood cells)
training_set4 <- training_tree
test_set4 <- test_tree
classifier_lr4 <- glm(formula = outcome ~ ., family = binomial, data = training_set4)
prob_pred_lr4 <- predict(classifier_lr4, type = 'response', newdata = test_set4[-1]) 
y_pred_lr4 <- ifelse(prob_pred_lr4 > 0.3, 1, 0)

# anova chi-squared test: which model performs better?
anova(classifier_lr1, classifier_lr2, classifier_lr3, classifier_lr4, test = "Chisq")

remove(training_set1, test_set1, classifier_lr1, prob_pred_lr1, y_pred_lr1,
       training_set2, test_set2, classifier_lr2, prob_pred_lr2, y_pred_lr2,
       training_set3, test_set3, classifier_lr3, prob_pred_lr3, y_pred_lr3,
       training_set4, test_set4, classifier_lr4, prob_pred_lr4, y_pred_lr4)

training_set_anova <- subset(training_tree, select = c(-red_blood_cells, -creatinine)) # training_tree
test_set_anova <- subset(test_tree, select = c(-red_blood_cells, -creatinine)) # test_tree

# update classifier
classifier_lr_anova <- glm(formula = outcome ~ ., family = binomial, data = training_set_anova)
prob_pred_lr_anova <- predict(classifier_lr_anova, type = 'response', newdata = test_set_anova[-1]) # remove outcome from test set (= prediction)
y_pred_lr_anova <- ifelse(prob_pred_lr_anova > 0.3, 1, 0)

# get summary statistics
summary(classifier_lr_anova)


#############################################################################################################################


# ---------------------|--------------|-------------|
#  MODEL               |   ACCURACY   | SENSITIVITY |
# ---------------------|--------------|-------------|
#  LOGISTIC REGRESSION | 87.84% [5]   | 15.10% [6]  |
# ---------------------|--------------|-------------|
#  K-NEAREST NEIGHBOR  | 86.73% [6]   | 07.55% [7]  | (WORST)
# ---------------------|--------------|-------------|
#  NAIVE BAYES         | 85.63% [7]   | 30.82% [4]  |
# ---------------------|--------------|-------------|
#  DECISION TREE       | 88.60% [4]   | 23.90% [5]  |
# ---------------------|--------------|-------------|
#  RANDOM FOREST       | 95.59% [2]   | 79.25% [2]  | 
# ---------------------|--------------|-------------|
#  SVM                 | 89.96% [3]   | 31.45% [3]  |
# ---------------------|--------------|-------------|
#  XGBoost             | 96.34% [1]   | 83.65% [1]  | (BEST)
# ---------------------|--------------|-------------|


# ---------------------|------------|------------|------------|
#  MODEL               |    CORR    |    TREE    |    PCA     |
# ---------------------|------------|------------|------------|
#  K-NEAREST NEIGHBOR  | 04.23% [7] | 05.94% [7] | 09.24% [2] |
# ---------------------|------------|------------|------------|
#  NAIVE BAYES         | 30.67% [1] | 23.46% [2] | 03.10% [5] |
# ---------------------|------------|------------|------------|
#  LOGISTIC REGRESSION | 13.56% [6] | 13.55% [6] | ??.??% [7] |
# ---------------------|------------|------------|------------|
#  RANDOM FOREST       | 21.14% [4] | 18.33% [4] | 06.74% [3] |
# ---------------------|------------|------------|------------|
#  SVM                 | 17.36% [5] | 16.19% [5] | 00.00% [6] |
# ---------------------|------------|------------|------------|
#  RIPPER              | 21.97% [3] | 23.79% [1] | 05.00% [4] |
# ---------------------|------------|------------|------------|
#  XGBoost             | 23.12% [2] | 23.42% [3] | 16.00% [1] |
# ---------------------|------------|------------|------------|


################################## EDA (OLD) ###################################
#
#install.packages("epiDisplay")
#library(epiDisplay) # tab1() bar chart
#install.packages("dplyr")
#library(dplyr) # summarise()
#install.packages("psych")
#library(psych) # describe()
#install.packages("skimr")
#library(skimr)
#install.packages('lattice')
#library(lattice)
# 
# # use the skimr package to get an overview of the data
# skim(hf)
# 
# # EDA - DEMOGRAPHICS
# 
# # ranking of patients' ethnicities
# tab1(hf$ethnicity_desc, cex.names = 0.6, sort.group = "decreasing", cum.percent = TRUE, main = "Patients' Ethnicity Ranking")
# # the sample data is composed of 74.2% of white patients
# 
# # AGE
# # the age analysis was inspired from: https://towardsdatascience.com/an-exploratory-data-analysis-project-in-r-a51918ebf34d
# summary(hf$age)
# boxplot(hf$age)
# # most patients admitted for heart failure are aged between 65 and 85 years old
# hist(hf$age, main = "Age Histogram", breaks = max(hf$age)-min(hf$age))
# CrossTable(hf$age, digits = 4)
# # the data is left skewed, with a very high number of patients aged 89 (12%)
# # patients who died from heart failure by age group
# hf$age_grp <- cut(hf$age, breaks = seq(min(hf$age), max(hf$age), 4))
# # bar plot of proportion of deceased patients by age group
# age_distri <- hf %>%
#   group_by(age_grp) %>%
#   summarise(died = sum(outcome))
# age_distri
# prop_by_age <- hf %>%
#   group_by(age_grp) %>%
#   summarise(died_proportion = round(sum(outcome)/n(), 3)*100)
# prop_by_age
# prop_by_age %>%
#   ggplot(aes(x = age_grp, y = died_proportion)) +
#   geom_bar(stat = "identity", fill = "#f68060", width = .6) +
#   xlab("") + ylab("Count") + ggtitle("Deceased Patients by Age Group") + 
#   theme_bw()
# # the proportion of deceased is around 15% between 67 and 90 years old, then it increases
# # only one patient in the 27-31 age group who died, so this number is not significant
# # however, there seem to be a high propotion between 39 and 54 years old who did not survive
# (sum(hf$age > 38 & hf$age < 55 & hf$outcome == 1))/sum(hf$age > 38 & hf$age < 55)
# # for the whole group, the average of deceased patients goes down to 14.3%
# # the age does not seem to be a significant factor to predict if someone will survive a heart failure
# hf <- subset(hf, select = c(-age_grp))
# remove(age_distri, prop_by_age)
# 
# # BMI
# summary(hf$body_mass_index)
# boxplot(hf$body_mass_index)
# # there are a number of outliers which indicate that patients with an extremely high BMI are more likely to suffer a heart failure
# qplot(hf$body_mass_index, binwidth = 1, main = "Patients BMI Distribution", xlab = "BMI", ylab = "Count")
# # add bmi range columns based on the international standards
# hf <- hf %>%
#   mutate(
#     bmi_range = case_when(
#       body_mass_index < 20 ~ "underweight",
#       body_mass_index < 25 ~ "normal weight",
#       body_mass_index < 30 ~ "overweight",
#       body_mass_index >= 30 ~ "obese"
#     )
#   )
# hf$bmi_range <- as.factor(hf$bmi_range)
# # bmi_range of admitted patients
# ggplot(hf, aes(x = "", fill = bmi_range))+
#   geom_bar(width = 1)+
#   coord_polar("y")
# # most patients admitted for heart failure are obese (40.65%)
# ggplot(hf, aes(x = bmi_range, fill = outcome_desc)) + 
#   geom_bar(position = 'dodge') +
#   xlab("BMI Range") +
#   ylab("Count") +
#   ggtitle("Analysis of Mortality by BMI Ranges") +
#   scale_fill_discrete(name = "Died", labels = c("Yes", "No"))
# CrossTable(hf$bmi_range, hf$outcome_desc, prop.c = FALSE, prop.t = FALSE)
# mean(14.6, 10.3, 15.4, 21.6)
# # there seem to be a slighty higher risk for underweight patients with 21.62% but result to be taken with
# # caution as there is a small number of patients in this range (6.29% only)
# 
# # gender
# ggplot(hf, aes(x = gender_desc, fill = outcome_desc)) + 
#   geom_bar(position = 'dodge') +
#   xlab("Gender") +
#   ylab("Count") +
#   ggtitle("Analysis of Mortality by Gender") +
#   scale_fill_discrete(name = "Died", labels = c("Yes", "No"))
# CrossTable(hf$gender_desc, hf$outcome_desc, prop.t = FALSE, prop.c = FALSE)
# # women seem to have slightly higher chances to survive with 12.8% mortality against 14.3% for men
# 
# # DEMOG INSIGHT: from demographic data, we can define an age and body mass index profile for patients who
# # suffer heart failure:
# #     - age between 65 and 85
# #     - BMI between 24.22 and 33.36 (overweight or obese as 25 is the threshold)
# # These variables don't seem to be significant to predict mortality.
# # However, women have a slightly better survival rate than men (1.5% less chance to die)
# 
# # EDA - COMORBIDITIES
# 
# # create a subset to explore the patients' comorbidities
# hf_comor <- subset(hf, select = c(outcome, hypertension, atrial_fibrillation, ischaemic_heart_disease,
#                                   diabetes_mellitus, hypoferric_anaemia, depression, hyperlipidaemia,
#                                   chronic_kidney_disease, chronic_obstructive_pulmonary_disease))
# hf_comor_m <- melt(hf_comor, id.vars = c("outcome"))
# colnames(hf_comor_m) <- c("outcome", "comorbidity", "count")
# hf_comor_m
# hf_comor_all <- hf_comor_m[hf_comor_m$count == 1,]
# tab1(hf_comor_all$comorbidity, cex.names = 0.8, sort.group = "decreasing", cum.percent = TRUE, main = "Patients'
#      Comorbidities Ranking - All Patients")
# # the most frequent comorbidity present in patients admitted for heart failure is hypertension (844 cases)
# hf_comor_died <- hf_comor_all[hf_comor_all$outcome == 1,]
# tab1(hf_comor_died$comorbidity, cex.names = 0.8, sort.group = "decreasing", cum.percent = TRUE, main = "Patients'
#      Comorbidities Ranking - Deceased Patients")
# # the most frequent comorbidities present in patients who did not survive after heart failure are
# # hypertension (101 cases) and atrial fibrillation (92 cases)
# corrplot(cor(hf_comor), tl.srt = 45)
# cor(hf_comor)
# # the correlation matrix confirms a positive correlation between atrial fibrillation and outcome
# CrossTable(hf$atrial_fibrillation, hf$outcome_desc, prop.t = FALSE, prop.c = FALSE)
# # 45.2% of the patients had AF comorbidity when admitted for HF
# # 10.4% of patients with no AF died against 17.3% of patients with AF
# # the chances to die are multiplied by 1.6 when the patient has a AF condition
# remove(hf_comor, hf_comor_all, hf_comor_died, hf_comor_m)
