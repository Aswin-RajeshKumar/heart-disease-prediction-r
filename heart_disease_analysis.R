setwd("C:/Users/Windows/Downloads")

#Set seed for reproducible random sampling
set.seed(1101)

#Read and store the .csv data file as a dataframe
heart.data = read.csv("heart-disease-dsa.csv")

#Information related to the given data
print(names(heart.data)) 
print(dim(heart.data)) #300 records (rows) and 13 variables (columns)

#Changing character type response (yes/no) to numerical response (1/0) 
heart.data$disease = ifelse(heart.data$disease == "yes",1,0)

#Converting all the categorical variables into factors
factor_vars <- c("disease", "sex", "chest.pain", "fbs", "rest.ecg", "angina", "blood.disorder", "vessels")
heart.data[factor_vars] <- lapply(heart.data[factor_vars], as.factor)

attach(heart.data)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#PART I (Exploratory Data Analysis)

#Association between categorical response variable 'disease' with numerical input variables using boxplots

box_age = boxplot(age ~ disease, xlab = "Heart disease status (0=No,1=Yes)", ylab = "Age of patient (Years)", main = paste("Age by Heart Disease Status"), col = "red")
#The median is noticeably higher in the group with heart disease 
#The IQR (spread) is significantly wider in the group without heart disease, while the IQR for the group with heart disease is more concentrated around older ages
#Overall, positive association between age and the presence of heart disease
#This input variable is a good predictor of heart disease status and should be considered for classification

box_bp = boxplot(bp ~ disease, xlab = "Heart disease status (0=No,1=Yes)", ylab = "Resting blood pressure (mm Hg)", main = paste("Resting Blood Pressure by Heart Disease Status"), col = "blue")
#The medians are quite similar in both groups, but the spread is slightly larger and there are more high outliers in the group with heart disease
#This indicates slightly more variability in group with heart disease
#Overall, very weak positive association between resting blood pressure and presence of heart disease
#This input variable isn't a strong predictor of heart disease status and should be discarded

box_chol = boxplot(chol ~ disease, xlab = "Heart disease status (0=No,1=Yes)", ylab = "Patient's serum cholesterol (mg/dl)", main = paste("Serum Cholesterol by Heart Disease Status"), col = "orange")
#Similar medians (~240–250 mg/dl) in both groups, with no major difference in variability and a slightly higher IQR (spread) in the group with heart disease
#Overall, no strong association between serum cholesterol and heart disease status
#This input variable isn't a strong predictor of heart disease status and should be discarded

box_heart.rate = boxplot(heart.rate ~ disease, xlab = "Heart disease status (0=No,1=Yes)", ylab = "Highest heart rate on exercise testing (BPM)", main = paste("Highest Heart Rate by Disease Status"), col = "green")
#The median is significantly lower in the group with heart disease 
#The IQR (spread) is also slightly wider and more extreme low outliers in the heart disease group.
#Overall, strong negative association between highest heart rate on exercise testing and the presence of heart disease
#This input variable is a strong predictor of heart disease status and should be considered for classification

box_st.depression = boxplot(st.depression ~ disease, xlab = "Heart disease status (0=No,1=Yes)", ylab = "ST depression relative to rest (mm)", main = paste("ST Depression by Heart Disease Status"), col = "purple")
#The median is significantly higher in the group with heart disease
#The IQR (spread) is also wider and more extreme high outliers in the heart disease group
#Overall, strong positive association between ST Depression and presence of heart disease
#This input variable is a strong predictor of heart disease status and should be considered for classification

#Association between categorical response variable 'disease' with categorical input variables using contingency tables and odds ratios

tab_sex = table(disease,sex); tab_sex
odds_f = tab_sex[2]/tab_sex[1] #0.34286
odds_m = tab_sex[4]/tab_sex[3] #1.23913
odds.ratio_m = odds_m/odds_f;  #3.61413
#The odds of having heart disease are approximately 3.6 times higher in males compared to females
#This indicates a strong positive association between being male (sex = 1) and having heart disease (1 = Yes)
#This input variable is a highly informative response for predicting heart disease status and should be considered for classification

tab_chest.pain = table(disease,chest.pain); tab_chest.pain
odds_typ = tab_chest.pain[2]/tab_chest.pain[1]  #2.737 (reference group)
odds_atyp = tab_chest.pain[4]/tab_chest.pain[3] #0.2195
odds_non = tab_chest.pain[6]/tab_chest.pain[5]  #0.2647
odds_asym = tab_chest.pain[8]/tab_chest.pain[7] #0.4667
odds.ratio_typ = 1.0 #reference group 
odds.ratio_atyp = odds_atyp/odds_typ # ~0.080 (very low)
odds.ratio_non = odds_non/odds_typ   # ~0.097 (very low)
odds.ratio_asym = odds_asym/odds_typ # ~0.170 (low)
#typical angina has a very large odds ratio compared to rest of the types of chest pain indicating strong positive association between typical angina and presence of heart disease
#Both Atypical and Non-anginal chest pains have large negative associations (~0.08-0.10) with presence of heart disease
#Asymptomatic chest pain has a moderate negative association with presence of heart disease (> 0.10)
#This input variable is a highly informative predictor of heart disease status and should be considered for classification

tab_fbs = table(disease,fbs); tab_fbs
odds_false = tab_fbs[2]/tab_fbs[1] # ~0.8345 
odds_true = tab_fbs[4]/tab_fbs[3]  # ~0.9565
odds.ratio_true = odds_true/odds_false; # ~1.146
#The odds of having heart disease are only about 1.15 times higher in individuals with high fasting blood sugar compared to those with normal levels
#The odds ratio just above 1 indicates very weak positive association between high fasting blood sugar and presence of heart disease
#This input variable isn't significantly predictive of heart disease status and should be discarded

tab_rest.ecg = table(disease,rest.ecg); tab_rest.ecg
odds_norm = tab_rest.ecg[2]/tab_rest.ecg[1]   # ~1.197 (reference group)
odds_STT = tab_rest.ecg[4]/tab_rest.ecg[3] # ~0.589
odds_LVH = tab_rest.ecg[6]/tab_rest.ecg[5]  # 3.0 (very high but sample size is small)
odds.ratio_norm = 1.0 #reference group
odds.ratio_STT = odds_STT/odds_norm # ~0.492
odds.ratio_LVH = odds_LVH/odds_norm # ~2.506
#Compared to patients with normal ECG results, ST_T abnormalities show a moderate negative association with heart disease (with about half odds)
#Whereas, patients with left ventricular hypertropy exhibit a strong positive association with more than twice the odds of having heart disease
#But, sample size of LVH is very small (only 4) and needs careful interpretation based on medical research.
#This input variable may be predictive of heart disease and can be considered for classification

tab_angina = table(disease,angina); tab_angina
odds_no = tab_angina[2]/tab_angina[1]   # ~0.446
odds_yes =  tab_angina[4]/tab_angina[3] # ~3.304
odds.ratio_yes = odds_yes/odds_no # ~7.408 (very high)
#The odds of having heart disease are about 7.4 times higher in patients who experience exercise-induced angina compared to those who do not
#This indicates a very string positive association between exercise-induced angina and heart disease
#This input variable is a highly informative predictor of heart disease and should be considered for classification

tab_vessels = table(disease,vessels); tab_vessels
odds_0 = tab_vessels[2]/tab_vessels[1]  # ~0.35
odds_1 = tab_vessels[4]/tab_vessels[3]  # ~2.20
odds_2 = tab_vessels[6]/tab_vessels[5]  # ~4.43
odds_3 = tab_vessels[8]/tab_vessels[7]  # ~5.67
odds_4 = tab_vessels[10]/tab_vessels[9] # ~0.25
#The odds of heart disease increased sharply with the number of vessels, from 0.35 (vessels = 0) to 5.67 (vessels = 3)
#Compared to vessels = 0, vessels = 1,2 and 3 show strong positive association with presence of heart disease.
#But, vessels = 4 has low odds and only 5 cases, so the odds ratio (0.71) is unreliable and needs careful interpretation based on medical research
#This input variable is a highly informative predictor of heart disease status and should be considered for classification

#For the input variable blood.disorder, we first need to predict the two missing values using decision tree for further analysis
library(rpart)

missing_rows = which(heart.data$blood.disorder == '0')
known_rows = which(heart.data$blood.disorder != '0')

train_data = heart.data[known_rows,]
predict_data = heart.data[missing_rows,]

model = rpart(blood.disorder ~ ., data = train_data, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 10))

predicted_values = predict(model, newdata = predict_data, type = "class")  #predicted values are 2 and 3 for the two unknown values

heart.data$blood.disorder[missing_rows] = predicted_values
heart.data$blood.disorder = droplevels(heart.data$blood.disorder) #dropping level 0 

tab_blood.disorder = table(disease,heart.data$blood.disorder); tab_blood.disorder
odds_normal = tab_blood.disorder[2]/tab_blood.disorder[1] # 2 (reference)
odds_fixed = tab_blood.disorder[4]/tab_blood.disorder[3]  # ~0.28
odds_rever = tab_blood.disorder[6]/tab_blood.disorder[5]  # ~3.21
odds.ratio_normal = 1.0 #reference
odds.ratio_fixed = odds_fixed/odds_normal # ~0.14
odds.ratio_rever = odds_rever/odds_normal # ~1.61
#The odds ratio of normal (unusual) and reversible defect blood disorders are significantly higher compared to that of fixed defect
#This indicates that normal and reversible defect blood disorders show strong positive association with presence of heart disease
#But, the sample size for normal is very low which might be the reason for unusually high association than fixed defect and needs careful interpretation based on medical research.
#fixed defect blood disorder has show negative association with presence of heart disease (unusual)
#This input variable is a highly informative predictor of heart disease status and should be considered for classification

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#PART II (Building Classifiers)

#1.Optimizing classifiers

#K-Nearest Neighbors classification
library(class)

knn.data = heart.data

#Re-ordering the labels in correct direction of categories based on their severity/association with disease status and medical research
knn.data$blood.disorder = factor(heart.data$blood.disorder, levels = c(1,2,3), labels = c(0,1,2))
knn.data$chest.pain = factor(heart.data$chest.pain, levels = c(3,2,1,0), labels = c(3,2,1,0))
knn.data$rest.ecg = factor(heart.data$rest.ecg, levels = c(0,1,2), labels = c(0,1,2))

knn.data$vessels = as.numeric(heart.data$vessels)

num_features = c("age","heart.rate","st.depression","vessels") #Numerical features which are informative and appropriate for knn
cat_features = c("blood.disorder","chest.pain","rest.ecg") #Categorical features which are informative and treated as numerical(ordinal) using encoding for knn
scaled_data = data.frame(lapply(knn.data[,num_features],scale),knn.data[,cat_features] ,knn.data$disease) #Scaled numerical features, encoded categorical variables and the response variable
n = dim(scaled_data)[1] #number of rows

#5-fold Cross Validation
n_folds = 5
folds_j = sample(rep(1:n_folds, length.out = n))
table(folds_j) #Equal folds of size 60

X = scaled_data[,1:7]
Y = scaled_data[,8]

K = 50 # KNN with k = 1,2,....50
sensitivity_knn = numeric(K)
tpr_knn = numeric(n_folds)
for (i in 1:K){
  for (j in 1:n_folds){
    test_j = which(folds_j == j)
    pred = knn(train=X[-test_j,], test=X[test_j, ], cl=Y[-test_j], k=i)
    confusion.matrix_knn = table(actual = Y[test_j], predicted = pred)
    tpr_knn[j] = confusion.matrix_knn[4]/sum(confusion.matrix_knn[2,])
  }
  sensitivity_knn[i] = mean(tpr_knn)
}

sensitivity_df = data.frame(k = 1:K, sensitivity = sensitivity_knn)
sorted_sensitivity_df = sensitivity_df[order(-sensitivity_df$sensitivity),]
best_k = sorted_sensitivity_df$k[1]; best_k #The value of k which gives maximum sensitivity is 1
second_best_k = sorted_sensitivity_df$k[2]; second_best_k #The value of k which gives second highest sensitivity is 3

#Graphical visualization of sensitivity values corresponding to k 
plot(x=1:K,xlab = "k", sensitivity_knn, main = "Sensitivity vs k")
abline(v = c(best_k,second_best_k), col = "red")

#To avoid over-fitting, k = 3 is preferred over k = 1, as a slightly higher k value strikes a better balance between stability and generalization
#Additionally, the difference in sensitivity between k = 1 and 3 is not significant 

#Decision Tree classification
library(rpart)
library(rpart.plot)
drops = c("bp","chol","fbs") #Dropping the features determined insignificant in EDA
dt_data = heart.data[, !(names(heart.data) %in% drops)] #dropping non-predictive features
n = dim(dt_data)[1]

#5-fold cross validation

s = 30 # Decision tree with minsplit = 1,...,30 
sensitivity_dt = numeric(s)
tpr_dt = numeric(n_folds)

for(i in 1:s){
  for (j in 1:n_folds){
    test = which(folds_j == j)
    train = dt_data[-c(test),]
    fit = rpart(disease ~ .,
                method = "class",
                data = train,
                control = rpart.control(minsplit = i),
                parms = list(split = "information"))
    new.data = data.frame(dt_data[test,c(1:9)])
    pred = predict(fit,new.data,type = "class")
    confusion.matrix_dt = table(actual = dt_data[test,10],predicted = pred)
    tpr_dt[j] = confusion.matrix_dt[4]/sum(confusion.matrix_dt[2,])
  }
  sensitivity_dt[i] = mean(tpr_dt)
}

plot(c(1:s), xlab = "minsplit", sensitivity_dt,type = "b", col = "blue", pch = 19, ylim = c(0,0.8), main = "Sensitivity vs minsplit")

best_minsplit = which(sensitivity_dt == max(sensitivity_dt)) # minsplit = 8,9,10,17,18,19 give the same maximum sensitivity
sensitivity_dt[best_minsplit[1]] # ~0.786 (maximum sensitivity)

#The minsplit values 8,9 and 10 give very complex decision trees, while 17,18 and 19 give much simpler and interpretable tree with same sensitivity
#To avoid over-fitting and for model simplicity, minsplit = 17 is chosen
fit = rpart(disease ~ .,
            method = "class",
            data = dt_data,
            control = rpart.control(minsplit = 17),
            parms = list(split = "information"))

rpart.plot(fit, type=4, extra=2, clip.right.labs = FALSE, varlen = 0)
#This decision tree also reveals that the features "sex", "rest.ecg", "heart.rate", "st.depression" & "angina" are not informative enough (in terms of info gain) to be selected for splitting as they didn't show up

#Logistic Regression

#To make the model simple, dropping additional features that gives high p-values along with the features determined insignificant in EDA
drops = c("bp", "chol", "fbs", "rest.ecg", "angina","age") #Removed rest.ecg, angina and age because they had high p-values (insignificant) when included in the model
lr_data = heart.data[, !(names(heart.data) %in% drops)] #dropping non-predictive features
n = dim(lr_data)[1]

prop.table(table(lr_data$disease)) #46% of disease status is yes/1, so we choose delta = 0.46 to assign class labels from probabilities

#5-fold cross validation

tpr_lr = numeric(n_folds)

for (j in 1:n_folds){
  test = which(folds_j == j)
  train = lr_data[-c(test),]
  LR.Model = glm(disease ~ ., data = train, family = binomial)
  new.data = data.frame(lr_data[test,c(1:6)])
  pred = predict(LR.Model,newdata = new.data,type = "response")
  pred.disease = ifelse(pred >= 0.46, "1", "0")
  confusion.matrix_lr = table(actual = lr_data[test,7],predicted = pred.disease)
  tpr_lr[j] = confusion.matrix_lr[4]/sum(confusion.matrix_lr[2,])
}
sensitivity = mean(tpr_lr) # ~ 0.814
summary(LR.Model)

#2.Performance comparison

library(ROCR)

#--------------(i) For KNN-------------------------------------------------------------------------------------------------------------------------------
second_best_k = 3
X = scaled_data[,1:7]
Y = scaled_data[,8]

#Run KNN
knn_pred = knn(train = X, test = X, cl = Y, k = second_best_k, prob = TRUE)

knn_pred_numeric = as.numeric(knn_pred) - 1
knn_prob = ifelse(knn_pred_numeric == 1, attr(knn_pred,"prob"), 1 - attr(knn_pred, "prob"))

pred_knn = prediction(knn_prob, Y)

#ROC and AUC
perf_knn = performance(pred_knn, "tpr", "fpr")
auc_knn = performance(pred_knn, "auc")@y.values[[1]]; # ~0.957
plot(perf_knn, col = "red", lwd = 2, main = paste("ROC Curve for All Models"))

#TPR and Precision
cm_knn = table(Predicted = knn_pred_numeric, Actual = Y)
tpr_knn = cm_knn[4]/sum(cm_knn[,2])       # ~0.833
precision_knn = cm_knn[4]/sum(cm_knn[2,]) # ~0.885

#-------------(ii) For Decision Tree--------------------------------------------------------------------------------------------------------------------
best_minsplit = 17

#Fit DT
fit_dt = rpart(disease ~ .,method = "class", data = dt_data, control = rpart.control(minsplit = 17), parms = list(split = "information"))
prob_dt = predict(fit_dt, type = "prob")[,2]
pred_dt = prediction(prob_dt, dt_data[10])

#ROC and AUC
perf_dt = performance(pred_dt, "tpr", "fpr")
auc_dt = performance(pred_dt, "auc")@y.values[[1]] # ~0.895
plot(perf_dt, col = "green", lwd = 2, add = TRUE)

#TPR and Precision
class_dt = ifelse(prob_dt >= 0.5, 1, 0)
cm_dt = table(Predicted = class_dt, Actual = dt_data$disease)
tpr_dt = cm_dt[4]/sum(cm_dt[,2])       # ~0.848
precision_dt = cm_dt[4]/sum(cm_dt[2,]) # ~0.860

#------------(iii) For Logistic Regression--------------------------------------------------------------------------------------------------------------
drops = c("bp", "chol", "fbs", "rest.ecg", "angina","age") 
lr_data = heart.data[, !(names(heart.data) %in% drops)]

#Fit LR
lr_model = glm(disease ~ ., data = lr_data, family = binomial)
prob_lr = predict(lr_model, lr_data, type = "response")
pred_lr = prediction(prob_lr, lr_data$disease)

#ROC and AUC
perf_lr = performance(pred_lr, "tpr", "fpr")
auc_lr = performance(pred_lr, "auc")@y.values[[1]] # ~0.929
plot(perf_lr, col = "blue", lwd = 2, add = TRUE)

#TPR and Precision
class_lr = ifelse(prob_lr >= 0.46, 1, 0)
cm_lr = table(Predicted = class_lr, Actual = lr_data$disease)
tpr_lr = cm_lr[4]/sum(cm_lr[,2])       # ~0.833
precision_lr = cm_lr[4]/sum(cm_lr[2,]) # ~0.852

#Plot specifics
legend(x = 0.4, y = 0.4, cex = 1, bty = "n", 
       legend = c(paste("Logistic Regression (AUC =", round(auc_lr, 4), ")"),
                  paste("Decision Tree (AUC =", round(auc_dt, 4), ")"),
                  paste("KNN (AUC =", round(auc_knn, 4), ")")),
       col = c("blue", "green", "red"),
       lwd = 2)
abline(a=0, b=1, lty=2) #baseline

