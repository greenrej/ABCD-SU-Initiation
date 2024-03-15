# Random Forest: Model ---------------------------------------------------------

# create final dataset for random forest
data.3 <- data.2 %>%
  select(-c(src_subject_id)) %>% 
  mutate_if(is.character, as.factor)
str(data.3, list.len=ncol(data.3))

# rename DV as Y
colnames(data.3)[1]<-"Y"
is.data.frame(data.3)
names(data.3) 

# create dummy codes (on data.3)

# subset factor variables
data.3_factor <- data.3 %>% 
  select_if(is.factor)

# create dummy codes for all categories
data.3_factor_dummy <- fastDummies::dummy_cols(data.3_factor) 

# subset dummy codes for model (exclude reference grouo)
data.3_factor_dummy <- data.3_factor_dummy %>% 
  select(
    # Demographics
    race_4l_Asian, race_4l_Black, race_4l_Other_MultiRacial, 
    # race_4l_White (reference group)
    eth_hisp_Hispanic,
    # eth_hisp_non-Hispanic (reference group)
    income_inc_1, income_inc_2, income_inc_3, income_inc_4, income_inc_5,
    income_inc_6, income_inc_7, income_inc_8, income_inc_10,
    # income_inc_9 (reference group)
    religion_rp_1, religion_rp_2, religion_rp_3, religion_rp_4, 
    religion_rp_5, religion_rp_6, religion_rp_7, religion_rp_8,
    religion_rp_9, religion_rp_10, religion_rp_11, religion_rp_12,
    religion_rp_13, religion_rp_14, religion_rp_15, religion_rp_16,
    # religion_rp_17 (reference group) 
    p_edu_Less_than_HS_Degree_GED_Equivalent, 
    p_edu_HS_Graduate_GED_Equivalent,
    p_edu_Some_College_or_Associates_Degree, 
    p_edu_Masters_Degree,
    p_edu_Professional_School_or_Doctoral_Degree,
    # p_edu_Bachelors_Degree (reference group)
    sex_2l_Female,
    # sex_2l_Male (reference group)
    
    # Physical Health
    rec_bin_Yes,
    # rec_bin_No (reference group)
    exp_sub_Yes, 
    # exp_sub_No (reference group)
    tbi_injury_Yes,
    # tbi_injury_No (reference group)
    
    # Culture & Environment
    kbi_p_grades_in_school_Grade_B, kbi_p_grades_in_school_Grade_C, 
    kbi_p_grades_in_school_Grade_Fail,
    # kbi_p_grades_in_school_Grade_A (reference group)
    det_susp_Yes,
    # det_susp_No (reference group)
    se_services_Emotion_or_Learning_Support, se_services_Gifted,
    se_services_Other, se_services_Combined_Services,
    # se_services_None (reference group)
    
    # Neurocognitive
    cct_Immediate, 
    # cct_Delayed (reference group)
  )

# drop initial factor variables from dataset
data.3 <- data.3 %>% 
  select(
    # Demographics
    -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
    # Physical Health
    -rec_bin, -exp_sub, -tbi_injury,
    # Culture & Environment
    -kbi_p_grades_in_school, -det_susp, -se_services, 
    # Neurocog
    -cct)

# add dummy codes
data.3 <- cbind(data.3, data.3_factor_dummy)
cat(print(length(data.3)), 'variables in Split (1) training including dummy codes \n') 
rm(data.3_factor, data.3_factor_dummy)

# order variables based on variables included in each model 
names(data.3)
data.3 <- data.3 %>% 
  relocate(
    any_of(
      c(
        # Demographics
        'race_4l_Asian', 'race_4l_Black', 'race_4l_Other_MultiRacial', 
        'eth_hisp_Hispanic',
        'income_inc_1', 'income_inc_2', 'income_inc_3', 'income_inc_4',
        'income_inc_5', 'income_inc_6', 'income_inc_7', 'income_inc_8',
        'income_inc_10', 
        'religion_rp_1', 'religion_rp_2', 'religion_rp_3', 'religion_rp_4',
        'religion_rp_5', 'religion_rp_6', 'religion_rp_7', 'religion_rp_8',
        'religion_rp_9', 'religion_rp_10', 'religion_rp_11', 'religion_rp_12',
        'religion_rp_13', 'religion_rp_14', 'religion_rp_15', 'religion_rp_16',    
        'p_edu_Less_than_HS_Degree_GED_Equivalent',
        'p_edu_HS_Graduate_GED_Equivalent',
        'p_edu_Some_College_or_Associates_Degree', 'p_edu_Masters_Degree',  
        'p_edu_Professional_School_or_Doctoral_Degree',
        'sex_2l_Female')), 
    .before = cbcl_scr_syn_anxdep_r) %>% 
  relocate(
    any_of(
      c(
        # Physical Health
        'rec_bin_Yes',  'exp_sub_Yes', 'tbi_injury_Yes')), 
    .before = accult_q2_y) %>% 
  relocate(
    any_of(
      c(
        # Culture & Environment
        'kbi_p_grades_in_school_Grade_B', 'kbi_p_grades_in_school_Grade_C',              
        'kbi_p_grades_in_school_Grade_Fail', 
        'det_susp_Yes',                            
        'se_services_Emotion_or_Learning_Support', 'se_services_Gifted',                          
        'se_services_Other', 'se_services_Combined_Services')), 
    .before = hormone_scr_ert_mean) 

# identify factor variables: Y and dummy codes
data.3 <- data.3 %>% 
  mutate(
    Y = as.factor(Y),
    race_4l_Asian = as.factor(race_4l_Asian),
    race_4l_Black = as.factor(race_4l_Black), 
    race_4l_Other_MultiRacial = as.factor(race_4l_Other_MultiRacial), 
    eth_hisp_Hispanic = as.factor(eth_hisp_Hispanic),
    income_inc_1 = as.factor(income_inc_1),
    income_inc_2 = as.factor(income_inc_2),
    income_inc_3 = as.factor(income_inc_3),
    income_inc_4 = as.factor(income_inc_4),
    income_inc_5 = as.factor(income_inc_5),
    income_inc_6 = as.factor(income_inc_6),
    income_inc_7 = as.factor(income_inc_7),
    income_inc_8 = as.factor(income_inc_8),
    income_inc_10 = as.factor(income_inc_10),
    religion_rp_1 = as.factor(religion_rp_1),
    religion_rp_2 = as.factor(religion_rp_2),
    religion_rp_3 = as.factor(religion_rp_3),
    religion_rp_4 = as.factor(religion_rp_4),
    religion_rp_5 = as.factor(religion_rp_5),
    religion_rp_6 = as.factor(religion_rp_6),
    religion_rp_7 = as.factor(religion_rp_7),
    religion_rp_8 = as.factor(religion_rp_8),
    religion_rp_9 = as.factor(religion_rp_9),
    religion_rp_10 = as.factor(religion_rp_10),
    religion_rp_11 = as.factor(religion_rp_11),
    religion_rp_12 = as.factor(religion_rp_12),
    religion_rp_13 = as.factor(religion_rp_13),
    religion_rp_14 = as.factor(religion_rp_14),
    religion_rp_15 = as.factor(religion_rp_15),
    religion_rp_16 = as.factor(religion_rp_16), 
    p_edu_Less_than_HS_Degree_GED_Equivalent = as.factor(p_edu_Less_than_HS_Degree_GED_Equivalent),
    p_edu_HS_Graduate_GED_Equivalent = as.factor(p_edu_HS_Graduate_GED_Equivalent),
    p_edu_Some_College_or_Associates_Degree = as.factor(p_edu_Some_College_or_Associates_Degree),
    p_edu_Masters_Degree = as.factor(p_edu_Masters_Degree),
    p_edu_Professional_School_or_Doctoral_Degree = as.factor(p_edu_Professional_School_or_Doctoral_Degree),
    sex_2l_Female = as.factor(sex_2l_Female))

str(data.3$Y)

# create dataset split by models:
names(data.3)

# model 1:
data.3_m1 <- data.3[c(1,305:404)]
names(data.3_m1) # predictors: n = 100 predictors (w/dummy codes) + 1 DV

# model 2:
data.3_m2 <- data.3[c(1,305:421)]
names(data.3_m2) # predictors: n = 117 predictors (w/dummy codes) + 1 DV

# model 3:
data.3_m3 <- data.3
names(data.3_m3) # predictors: n = 420 predictors (w/dummy codes) + 1 DV

#------------------------------------------------------------------------------#
# Setting Up K-fold CV Model Fitting
#------------------------------------------------------------------------------#

# STEP 1: function to determine breakdown of data be stored into test and training datasets
trvl<-function(data, prop)
{
  csids<-which(data$Y==1) ### which rows are cases
  ctids<-which(data$Y==0) ### which rows are controls
  vcsids<-sample(csids, round(length(csids)*prop), replace=FALSE)
  vctids<-sample(ctids, round(length(ctids)*prop), replace=FALSE) 
  vdat<-data[sort(c(vcsids, vctids)),]
  tdat<-data[-sort(c(vcsids, vctids)),]
  ans<-list(valid.data=vdat, train.data=tdat)
  return(ans)
}

# STEP 2: fit RF w/imputation
df_m1 <- trvl(data.3_m1, prop=.25)
df_m2 <- trvl(data.3_m2, prop=.25)
df_m3 <- trvl(data.3_m3, prop=.25)

.doFitRF <- function(K, folds, data)
{
  print(paste("fold ",K,sep="")) 
  data4 <- rfImpute(Y~., data=data[-folds[[K]],], iter=5, ntree=500) 
  fit <- randomForest(as.factor(Y)~., data=data4, ntree=500, import=TRUE)
  var.imp <- fit$importance
  if(sum(is.na(data[folds[[K]],]))==0){new.x<-data[folds[[K]],]} 
  else{new.x <- rfImpute(Y~., data=data[folds[[K]],], iter=5, ntree=500)} 
  prd <- predict(fit, newdata=new.x, type="prob")[,2]
  ans <- list(prd=prd, imp.data=data4, var.imp=var.imp)
  return(ans)
} 

#------------------------------------------------------------------------------#
# Function to Generate K-fold datasets for K-fold cross-validation
#------------------------------------------------------------------------------#

cvFoldsR <- function(Y, K) 
{
  start<-54321
  set.seed(start)
  Y0 <- split(sample(which(Y==0)), rep(1:K, length=length(which(Y==0))))
  Y1 <- split(sample(which(Y==1)), rep(1:K, length=length(which(Y==1))))
  folds<-mapply(c, Y0, Y1)
  return(folds) 
}

folds_m1 <- cvFoldsR(Y=data.3_m1$Y, K=10) 
folds_m2 <- cvFoldsR(Y=data.3_m2$Y, K=10) 
folds_m3 <- cvFoldsR(Y=data.3_m3$Y, K=10) 

job::job({
  set.seed(39284)
  predictions_m1 <- sapply(seq(10), .doFitRF, folds=folds_m1, data=data.3_m1) 
})

job::job({
  set.seed(65489)
  predictions_m2 <- sapply(seq(10), .doFitRF, folds=folds_m2, data=data.3_m2)
})

job::job({
  set.seed(42155)
  predictions_m3 <- sapply(seq(10), .doFitRF, folds=folds_m2, data=data.3_m3)
})

data_imputed_m1 <- predictions_m1[c(2,5,8,11,14,17,20,23,26,29)]
data_imputed_m2 <- predictions_m2[c(2,5,8,11,14,17,20,23,26,29)]
data_imputed_m3 <- predictions_m3[c(2,5,8,11,14,17,20,23,26,29)]

all_var_imp_m1 <- predictions_m1[c(3,6,9,12,15,18,21,24,27,30)]
all_var_imp_m2 <- predictions_m2[c(3,6,9,12,15,18,21,24,27,30)]
all_var_imp_m3 <- predictions_m3[c(3,6,9,12,15,18,21,24,27,30)]

predictions2_m1 <- predictions_m1[c(1,4,7,10,13,16,19,22,25,28)]
predictions2_m2 <- predictions_m2[c(1,4,7,10,13,16,19,22,25,28)]
predictions2_m3 <- predictions_m3[c(1,4,7,10,13,16,19,22,25,28)]

fY_m1 <- vector("list",10)
for (k in 1:10) {fY_m1[[k]]<-as.vector(data.3_m1[folds_m1[[k]],1])} 

fY_m2 <- vector("list",10)
for (k in 1:10) {fY_m2[[k]]<-as.vector(data.3_m2[folds_m2[[k]],1])} 

fY_m3 <- vector("list",10)
for (k in 1:10) {fY_m3[[k]]<-as.vector(data.3_m3[folds_m3[[k]],1])} 

try_m1 <- cumsum(unlist(lapply(folds_m1, length)))
try_m2 <- cumsum(unlist(lapply(folds_m2, length)))
try_m3 <- cumsum(unlist(lapply(folds_m3, length)))

test_m1 <- vector('list', length(try_m1))
for(i in 1:length(try_m1))
{
  if(i==1){
    test_m1[[i]] <- 1:try_m1[i]
  }
  else{
    test_m1[[i]] <- (try_m1[i-1]+1):try_m1[i]
  }
}

test_m2 <- vector('list', length(try_m2))
for(i in 1:length(try_m2))
{
  if(i==1){
    test_m2[[i]] <- 1:try_m2[i]
  }
  else{
    test_m2[[i]] <- (try_m2[i-1]+1):try_m2[i]
  }
}

test_m3 <- vector('list', length(try_m3))
for(i in 1:length(try_m3))
{
  if(i==1){
    test_m3[[i]] <- 1:try_m3[i]
  }
  else{
    test_m3[[i]] <- (try_m3[i-1]+1):try_m3[i]
  }
}

cvauc_m1 <- cvAUC(unlist(predictions2_m1), unlist(fY_m1), folds=test_m1)
cvauc_m1
ci.cvauc_m1 <- ci.cvAUC(unlist(predictions2_m1), unlist(fY_m1), folds=test_m1)
ci.cvauc_m1

cvauc_m2 <- cvAUC(unlist(predictions2_m2), unlist(fY_m2), folds=test_m2)
cvauc_m2
ci.cvauc_m2 <- ci.cvAUC(unlist(predictions2_m2), unlist(fY_m2), folds=test_m2)
ci.cvauc_m2

cvauc_m3 <- cvAUC(unlist(predictions2_m3), unlist(fY_m3), folds=test_m3)
cvauc_m3
ci.cvauc_m3 <- ci.cvAUC(unlist(predictions2_m3), unlist(fY_m3), folds=test_m3)
ci.cvauc_m3

capture.output(cvauc_m1, file = 'output/random_forest/cvauc_m1.txt')
capture.output(cvauc_m2, file = 'output/random_forest/cvauc_m2.txt')
capture.output(cvauc_m3, file = 'output/random_forest/cvauc_m3.txt')

capture.output(ci.cvauc_m1, file = 'output/random_forest/ci.cvauc_m1.txt')
capture.output(ci.cvauc_m2, file = 'output/random_forest/ci.cvauc_m2.txt')
capture.output(ci.cvauc_m3, file = 'output/random_forest/ci.cvauc_m3.txt')


# Random Forest: Output --------------------------------------------------------

# print variable importance scores
options(max.print = 6000)

#--- model 1
m1_fold_1 <- all_var_imp_m1[1] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 1: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable') 

m1_fold_2 <- all_var_imp_m1[2]  %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 2: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_3 <- all_var_imp_m1[3] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 3: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_4 <- all_var_imp_m1[4] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 4: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_5 <- all_var_imp_m1[5] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 5: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_6 <- all_var_imp_m1[6] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 6: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_7 <- all_var_imp_m1[7] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 7: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_8 <- all_var_imp_m1[8] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 8: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_9 <- all_var_imp_m1[9] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 9: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m1_fold_10 <- all_var_imp_m1[10] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 10: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

#--- model 2
m2_fold_1 <- all_var_imp_m2[1] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 1: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_2 <- all_var_imp_m2[2]  %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 2: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_3 <- all_var_imp_m2[3] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 3: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_4 <- all_var_imp_m2[4] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 4: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_5 <- all_var_imp_m2[5] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 5: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_6 <- all_var_imp_m2[6] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 6: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_7 <- all_var_imp_m2[7] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 7: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_8 <- all_var_imp_m2[8] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 8: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_9 <- all_var_imp_m2[9] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 9: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m2_fold_10 <- all_var_imp_m2[10] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 10: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')


#--- model 3
m3_fold_1 <- all_var_imp_m3[1] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 1: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_2 <- all_var_imp_m3[2]  %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 2: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_3 <- all_var_imp_m3[3] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 3: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_4 <- all_var_imp_m3[4] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 4: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_5 <- all_var_imp_m3[5] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 5: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_6 <- all_var_imp_m3[6] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 6: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_7 <- all_var_imp_m3[7] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 7: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_8 <- all_var_imp_m3[8] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 8: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_9 <- all_var_imp_m3[9] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 9: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

m3_fold_10 <- all_var_imp_m3[10] %>% 
  as.data.frame() %>% 
  select(MeanDecreaseAccuracy) %>% 
  arrange(., desc(MeanDecreaseAccuracy)) %>% 
  rename('Fold 10: Mean Decrease Accuracy' = MeanDecreaseAccuracy) %>% 
  rownames_to_column(., var = 'variable')

#------------------------------------------------------------------------------#
#                    Summarize results across folds 
#------------------------------------------------------------------------------#

m1_rf_summary <- m1_fold_1 %>% 
  full_join(m1_fold_2, by = 'variable') %>% 
  full_join(m1_fold_3, by = 'variable') %>% 
  full_join(m1_fold_4, by = 'variable') %>% 
  full_join(m1_fold_5, by = 'variable') %>% 
  full_join(m1_fold_6, by = 'variable') %>% 
  full_join(m1_fold_7, by = 'variable') %>% 
  full_join(m1_fold_8, by = 'variable') %>% 
  full_join(m1_fold_9, by = 'variable') %>% 
  full_join(m1_fold_10, by = 'variable') %>% 
  mutate(decr_acc_avg = rowMeans(select(., ends_with('Accuracy')), na.rm = TRUE)) 

m2_rf_summary <- m2_fold_1 %>% 
  full_join(m2_fold_2, by = 'variable') %>% 
  full_join(m2_fold_3, by = 'variable') %>% 
  full_join(m2_fold_4, by = 'variable') %>% 
  full_join(m2_fold_5, by = 'variable') %>% 
  full_join(m2_fold_6, by = 'variable') %>% 
  full_join(m2_fold_7, by = 'variable') %>% 
  full_join(m2_fold_8, by = 'variable') %>% 
  full_join(m2_fold_9, by = 'variable') %>% 
  full_join(m2_fold_10, by = 'variable') %>% 
  mutate(decr_acc_avg = rowMeans(select(., ends_with('Accuracy')), na.rm = TRUE)) 

m3_rf_summary <- m3_fold_1 %>% 
  full_join(m3_fold_2, by = 'variable') %>% 
  full_join(m3_fold_3, by = 'variable') %>% 
  full_join(m3_fold_4, by = 'variable') %>% 
  full_join(m3_fold_5, by = 'variable') %>% 
  full_join(m3_fold_6, by = 'variable') %>% 
  full_join(m3_fold_7, by = 'variable') %>% 
  full_join(m3_fold_8, by = 'variable') %>% 
  full_join(m3_fold_9, by = 'variable') %>% 
  full_join(m3_fold_10, by = 'variable') %>% 
  mutate(decr_acc_avg = rowMeans(select(., ends_with('Accuracy')), na.rm = TRUE)) 

# decreasing order for accuracy
m1_rf_summary_accuracy <- m1_rf_summary %>% 
  arrange(desc(decr_acc_avg)) 

m2_rf_summary_accuracy <- m2_rf_summary %>% 
  arrange(desc(decr_acc_avg)) 

m3_rf_summary_accuracy <- m3_rf_summary %>% 
  arrange(desc(decr_acc_avg)) 

# retain select columns for supplemental materials
m1_rf_supplemental <- m1_rf_summary_accuracy %>% 
  select(variable, decr_acc_avg)

m2_rf_supplemental <- m2_rf_summary_accuracy %>% 
  select(variable, decr_acc_avg)

m3_rf_supplemental <- m3_rf_summary_accuracy %>% 
  select(variable, decr_acc_avg)

# add in table names for 
m1_rf_supplemental <- m1_rf_supplemental %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  mutate(decr_acc_avg = ifelse(decr_acc_avg == 0.000, '<0.001', decr_acc_avg))

m2_rf_supplemental <- m2_rf_supplemental %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  mutate(decr_acc_avg = ifelse(decr_acc_avg == 0.000, '<0.001', decr_acc_avg))

m3_rf_supplemental <- m3_rf_supplemental %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  mutate(decr_acc_avg = ifelse(decr_acc_avg == 0.000, '<0.001', decr_acc_avg))

# add in MRI metrics
m3_rf_supplemental <- m3_rf_supplemental %>% 
  mutate(
    
    # ROI: Banks of Superior Temporal Sulcus
    table_name = ifelse(variable == 'smri_area_cdk_banksstslh', 'Area: Banks of Superior Temporal Sulcus (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_banksstslh', 'Sulcul Depth: Banks of Superior Temporal Sulcus (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_banksstslh', 'Cortical Thickness: Banks of Superior Temporal Sulcus (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_banksstslh', 'Volume: Banks of Superior Temporal Sulcus (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_banksstsrh', 'Area: Banks of Superior Temporal Sulcus (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_banksstsrh', 'Sulcul Depth: Banks of Superior Temporal Sulcus (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_banksstsrh', 'Cortical Thickness: Banks of Superior Temporal Sulcus (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_banksstsrh', 'Volume: Banks of Superior Temporal Sulcus (R)', table_name),
    
    # ROI: Caudal Anterior Cingulate Cortex 
    table_name = ifelse(variable == 'smri_area_cdk_cdacatelh', 'Area: Caudal Anterior Cingulate Cortex (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cdacatelh', 'Sulcul Depth: Caudal Anterior Cingulate Cortex (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cdacatelh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cdacatelh', 'Volume: Caudal Anterior Cingulate Cortex (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_cdacaterh', 'Area: Caudal Anterior Cingulate Cortex (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cdacaterh', 'Sulcul Depth: Caudal Anterior Cingulate Cortex (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cdacaterh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cdacaterh', 'Volume: Caudal Anterior Cingulate Cortex (R)', table_name),
    
    # ROI: Caudal Anterior Cingulate Cortex 
    table_name = ifelse(variable == 'smri_area_cdk_cdmdfrlh', 'Area: Caudal Middle Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cdmdfrlh', 'Sulcul Depth: Caudal Middle Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cdmdfrlh', 'Cortical Thickness: Caudal Middle Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cdmdfrlh', 'Volume: Caudal Middle Frontal Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_cdmdfrrh', 'Area: Caudal Middle Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cdmdfrrh', 'Sulcul Depth: Caudal Middle Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cdmdfrrh', 'Cortical Thickness: Caudal Middle Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cdmdfrrh', 'Volume: Caudal Middle Frontal Lobe (R)', table_name),
    
    # ROI: Cuneus
    table_name = ifelse(variable == 'smri_area_cdk_cuneuslh', 'Area: Cuneus (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cuneuslh', 'Sulcul Depth: Cuneus (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cuneuslh', 'Cortical Thickness: Cuneus (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cuneuslh', 'Volume: Cuneus (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_cuneusrh', 'Area: Cuneus (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_cuneusrh', 'Sulcul Depth: Cuneus (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_cuneusrh', 'Cortical Thickness: Cuneus (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_cuneusrh', 'Volume: Cuneus (R)', table_name),
    
    # ROI: Entorhinal
    table_name = ifelse(variable == 'smri_area_cdk_ehinallh', 'Area: Entorhinal (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_ehinallh', 'Sulcul Depth: Entorhinal (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_ehinallh', 'Cortical Thickness: Entorhinal (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_ehinallh', 'Volume: Entorhinal (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_ehinalrh', 'Area: Entorhinal (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_ehinalrh', 'Sulcul Depth: Entorhinal (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_ehinalrh', 'Cortical Thickness: Entorhinal (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_ehinalrh', 'Volume: Entorhinal (R)', table_name),
    
    # ROI: Fusiform
    table_name = ifelse(variable == 'smri_area_cdk_fusiformlh', 'Area: Fusiform (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_fusiformlh', 'Sulcul Depth: Fusiform (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_fusiformlh', 'Cortical Thickness: Fusiform (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_fusiformlh', 'Volume: Fusiform (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_fusiformrh', 'Area: Fusiform (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_fusiformrh', 'Sulcul Depth: Fusiform (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_fusiformrh', 'Cortical Thickness: Fusiform (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_fusiformrh', 'Volume: Fusiform (R)', table_name),
    
    # ROI: Inferior Parietal Lobe 
    table_name = ifelse(variable == 'smri_area_cdk_ifpllh', 'Area: Inferior Parietal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_ifpllh', 'Sulcul Depth: Inferior Parietal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_ifpllh', 'Cortical Thickness: Inferior Parietal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_ifpllh', 'Volume: Inferior Parietal Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_ifplrh', 'Area: Inferior Parietal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_ifplrh', 'Sulcul Depth: Inferior Parietal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_ifplrh', 'Cortical Thickness: Inferior Parietal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_ifplrh', 'Volume: Inferior Parietal Lobe (R)', table_name),
    
    # ROI: Inferior Temporal Lobe 
    table_name = ifelse(variable == 'smri_area_cdk_iftmlh', 'Area: Inferior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_iftmlh', 'Sulcul Depth: Inferior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_iftmlh', 'Cortical Thickness: Inferior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_iftmlh', 'Volume: Inferior Temporal Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_iftmrh', 'Area: Inferior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_iftmrh', 'Sulcul Depth: Inferior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_iftmrh', 'Cortical Thickness: Inferior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_iftmrh', 'Volume: Inferior Temporal Lobe (R)', table_name),
    
    # ROI: Lateral Occipital Lobe 
    table_name = ifelse(variable == 'smri_area_cdk_locclh', 'Area: Lateral Occipital Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_locclh', 'Sulcul Depth: Lateral Occipital Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_locclh', 'Cortical Thickness: Lateral Occipital Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_locclh', 'Volume: Lateral Occipital Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_loccrh', 'Area: Lateral Occipital Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_loccrh', 'Sulcul Depth: Lateral Occipital Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_loccrh', 'Cortical Thickness: Lateral Occipital Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_loccrh', 'Volume: Lateral Occipital Lobe (R)', table_name),
    
    # ROI: Lateral Orbito-Frontal Lobe  
    table_name = ifelse(variable == 'smri_area_cdk_lobfrlh', 'Area: Lateral Orbito-Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_lobfrlh', 'Sulcul Depth: Lateral Orbito-Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_lobfrlh', 'Cortical Thickness: Lateral Orbito-Frontal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_lobfrlh', 'Volume: Lateral Orbito-Frontal Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_lobfrrh', 'Area: Lateral Orbito-Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_lobfrrh', 'Sulcul Depth: Lateral Orbito-Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_lobfrrh', 'Cortical Thickness: Lateral Orbito-Frontal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_lobfrrh', 'Volume: Lateral Orbito-Frontal Lobe (R)', table_name),
    
    # ROI: Lingual Lobe  
    table_name = ifelse(variable == 'smri_area_cdk_linguallh', 'Area: Lingual Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_linguallh', 'Sulcul Depth: Lingual Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_linguallh', 'Cortial Thickness: Lingual Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_linguallh', 'Volume: Lingual Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_lingualrh', 'Area: Lingual Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_lingualrh', 'Sulcul Depth: Lingual Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_lingualrh', 'Cortial Thickness: Lingual Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_lingualrh', 'Volume: Lingual Lobe (R)', table_name),
    
    # ROI: Medial Orbito-Frontal Lobe  
    table_name = ifelse(variable == 'smri_area_cdk_mobfrlh', 'Area: Medial Orbito-Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_mobfrlh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_mobfrlh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_mobfrlh', 'Volume: Medial Orbito-Frontal Lobe (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_mobfrrh', 'Area: Medial Orbito-Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_mobfrrh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_mobfrrh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_mobfrrh', 'Volume: Medial Orbito-Frontal Lobe (R)', table_name),
    
    # ROI: Middle Temporal Lobe
    table_name = ifelse(variable == 'smri_area_cdk_mdtmlh', 'Area: Middle Temporal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_mdtmlh', 'Sulcul Depth: Middle Temporal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_mdtmlh', 'Cortical Thickness: Middle Temporal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_mdtmlh', 'Volume: Middle Temporal Lobe (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_mdtmrh', 'Area: Middle Temporal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_mdtmrh', 'Sulcul Depth: Middle Temporal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_mdtmrh', 'Cortical Thickness: Middle Temporal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_mdtmrh', 'Volume: Middle Temporal Lobe (R)', table_name), 
    
    # ROI: Parahippocampal Region
    table_name = ifelse(variable == 'smri_area_cdk_parahpallh', 'Area: Parahippocampal Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parahpallh', 'Sulcul Depth: Parahippocampal Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parahpallh', 'Cortical Thickness: Parahippocampal Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parahpallh', 'Volume: Parahippocampal Region (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_parahpalrh', 'Area: Parahippocampal Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parahpalrh', 'Sulcul Depth: Parahippocampal Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parahpalrh', 'Cortical Thickness: Parahippocampal Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parahpalrh', 'Volume: Parahippocampal Region (R)', table_name), 
    
    # ROI: Paracentral Region
    table_name = ifelse(variable == 'smri_area_cdk_paracnlh', 'Area: Paracentral Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_paracnlh', 'Sulcul Depth: Paracentral Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_paracnlh', 'Cortical Thickness: Paracentral Region (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_paracnlh', 'Volume: Paracentral Region (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_paracnrh', 'Area: Paracentral Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_paracnrh', 'Sulcul Depth: Paracentral Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_paracnrh', 'Cortical Thickness: Paracentral Region (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_paracnrh', 'Volume: Paracentral Region (R)', table_name), 
    
    # ROI: Pars Opercularis
    table_name = ifelse(variable == 'smri_area_cdk_parsopclh', 'Area: Pars Opercularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parsopclh', 'Sulcul Depth: Pars Opercularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parsopclh', 'Cortical Thickness: Pars Opercularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parsopclh', 'Volume: Pars Opercularis (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_parsopcrh', 'Area: Pars Opercularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parsopcrh', 'Sulcul Depth: Pars Opercularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parsopcrh', 'Cortical Thickness: Pars Opercularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parsopcrh', 'Volume: Pars Opercularis (R)', table_name), 
    
    # ROI: Pars Orbitalis
    table_name = ifelse(variable == 'smri_area_cdk_parsobislh', 'Area: Pars Orbitalis (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parsobislh', 'Sulcul Depth: Pars Orbitalis (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parsobislh', 'Cortical Thickness: Pars Orbitalis (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parsobislh', 'Volume: Pars Orbitalis (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_parsobisrh', 'Area: Pars Orbitalis (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parsobisrh', 'Sulcul Depth: Pars Orbitalis (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parsobisrh', 'Cortical Thickness: Pars Orbitalis (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parsobisrh', 'Volume: Pars Orbitalis (R)', table_name), 
    
    # ROI: 	Pars Triangularis 
    table_name = ifelse(variable == 'smri_area_cdk_parstgrislh', 'Area: Pars Triangularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parstgrislh', 'Sulcul Depth: Pars Triangularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parstgrislh', 'Cortical Thickness: Pars Triangularis (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parstgrislh', 'Volume: Pars Triangularis (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_parstgrisrh', 'Area: Pars Triangularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_parstgrisrh', 'Sulcul Depth: Pars Triangularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_parstgrisrh', 'Cortical Thickness: Pars Triangularis (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_parstgrisrh', 'Volume: Pars Triangularis (R)', table_name), 
    
    # ROI: 	Pericalcarine 
    table_name = ifelse(variable == 'smri_area_cdk_pericclh', 'Area: Pericalcarine (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_pericclh', 'Sulcul Depth: Pericalcarine (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_pericclh', 'Cprtical Thickness: Pericalcarine (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_pericclh', 'Volume: Pericalcarine (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_periccrh', 'Area: Pericalcarine (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_periccrh', 'Sulcul Depth: Pericalcarine (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_periccrh', 'Cprtical Thickness: Pericalcarine (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_periccrh', 'Volume: Pericalcarine (R)', table_name), 
    
    # ROI: Postcentral Gyrus 
    table_name = ifelse(variable == 'smri_area_cdk_postcnlh', 'Area: Postcentral Gyrus  (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_postcnlh', 'Sulcul Depth: Postcentral Gyrus  (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_postcnlh', 'Cortical Thickness: Postcentral Gyrus  (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_postcnlh', 'Volume: Postcentral Gyrus  (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_postcnrh', 'Area: Postcentral Gyrus  (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_postcnrh', 'Sulcul Depth: Postcentral Gyrus  (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_postcnrh', 'Cortical Thickness: Postcentral Gyrus  (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_postcnrh', 'Volume: Postcentral Gyrus  (R)', table_name), 
    
    # ROI: Posterior Cingulate Cortex
    table_name = ifelse(variable == 'smri_area_cdk_ptcatelh', 'Area: Posterior Cingulate Cortex  (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_ptcatelh', 'Sulcul Depth: Posterior Cingulate Cortex  (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_ptcatelh', 'Cortical Thickness: Posterior Cingulate Cortex  (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_ptcatelh', 'Volume: Posterior Cingulate Cortex  (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_ptcaterh', 'Area: Posterior Cingulate Cortex  (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_ptcaterh', 'Sulcul Depth: Posterior Cingulate Cortex (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_ptcaterh', 'Cortical Thickness: Posterior Cingulate Cortex (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_ptcaterh', 'Volume: Posterior Cingulate Cortex (R)', table_name), 
    
    # ROI: Precentral Gyrus
    table_name = ifelse(variable == 'smri_area_cdk_precnlh', 'Area: Precentral Gyrus (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_precnlh', 'Sulcul Depth: Precentral Gyrus (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_precnlh', 'Cortical Thickness: Precentral Gyrus (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_precnlh', 'Volume: Precentral Gyrus (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_precnrh', 'Area: Precentral Gyrus (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_precnrh', 'Sulcul Depth: Precentral Gyrus (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_precnrh', 'Cortical Thickness: Precentral Gyrus (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_precnrh', 'Volume: Precentral Gyrus (R)', table_name), 
    
    # ROI: Precuneus
    table_name = ifelse(variable == 'smri_area_cdk_pclh', 'Area: Precuneus (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_pclh', 'Sulcul Depth: Precuneus (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_pclh', 'Cortical Thickness: Precuneus (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_pclh', 'Volume: Precuneus (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_pcrh', 'Area: Precuneus (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_pcrh', 'Sulcul Depth: Precuneus (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_pcrh', 'Cortical Thickness: Precuneus (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_pcrh', 'Volume: Precuneus (R)', table_name), 
    
    # ROI:  Rostral Anterior Cingulate Cortex 
    table_name = ifelse(variable == 'smri_area_cdk_rracatelh', 'Area: Rostral Anterior Cingulate Cortex (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_rracatelh', 'Sulcul Depth: Rostral Anterior Cingulate Cortex (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_rracatelh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_rracatelh', 'Volume: Rostral Anterior Cingulate Cortex (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_rracaterh', 'Area: Rostral Anterior Cingulate Cortex (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_rracaterh', 'Sulcul Depth: Rostral Anterior Cingulate Cortex (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_rracaterh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_rracaterh', 'Volume: Rostral Anterior Cingulate Cortex (R)', table_name), 
    
    # ROI: Rostral Middle Frontal Lobe
    table_name = ifelse(variable == 'smri_area_cdk_rrmdfrlh', 'Area: Rostral Middle Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_rrmdfrlh', 'Sulcul Depth: Rostral Middle Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_rrmdfrlh', 'Cortical Thickness: Rostral Middle Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_rrmdfrlh', 'Volume: Rostral Middle Frontal Lobe (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_rrmdfrrh', 'Area: Rostral Middle Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_rrmdfrrh', 'Sulcul Depth: Rostral Middle Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_rrmdfrrh', 'Cortical Thickness: Rostral Middle Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_rrmdfrrh', 'Volume: Rostral Middle Frontal Lobe (R)', table_name), 
    
    # ROI: Superior Frontal Lobe
    table_name = ifelse(variable == 'smri_area_cdk_sufrlh', 'Area: Superior Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_sufrlh', 'Sulcul Depth: Superior Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_sufrlh', 'Cortical Thickness: Superior Frontal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_sufrlh', 'Volume: Superior Frontal Lobe (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_sufrrh', 'Area: Superior Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_sufrrh', 'Sulcul Depth: Superior Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_sufrrh', 'Cortical Thickness: Superior Frontal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_sufrrh', 'Volume: Superior Frontal Lobe (R)', table_name), 
    
    # ROI: Superior Parietal Lobe
    table_name = ifelse(variable == 'smri_area_cdk_supllh', 'Area: Superior Parietal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_supllh', 'Sulcul Depth: Superior Parietal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_supllh', 'Cortical Thickness: Superior Parietal Lobe (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_supllh', 'Volume: Superior Parietal Lobe (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_suplrh', 'Area: Superior Parietal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_suplrh', 'Sulcul Depth: Superior Parietal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_suplrh', 'Cortical Thickness: Superior Parietal Lobe (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_suplrh', 'Volume: Superior Parietal Lobe (R)', table_name), 
    
    # ROI: Superior Temporal Lobe
    table_name = ifelse(variable == 'smri_area_cdk_sutmlh', 'Area: Superior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_sutmlh', 'Sulcul Depth: Superior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_sutmlh', 'Cortical Thickness: Superior Temporal Lobe (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_sutmlh', 'Volume: Superior Temporal Lobe (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_sutmrh', 'Area: Superior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_sutmrh', 'Sulcul Depth: Superior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_sutmrh', 'Cortical Thickness: Superior Temporal Lobe (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_sutmrh', 'Volume: Superior Temporal Lobe (R)', table_name),
    
    # ROI: Supramarginal
    table_name = ifelse(variable == 'smri_area_cdk_smlh', 'Area: Supramarginal (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_smlh', 'Sulcul Depth: Supramarginal (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_smlh', 'Cortical Thickness: Supramarginal (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_smlh', 'Volume: Supramarginal (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_smrh', 'Area: Supramarginal (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_smrh', 'Sulcul Depth: Supramarginal (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_smrh', 'Cortical Thickness: Supramarginal (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_smrh', 'Volume: Supramarginal (R)', table_name),
    
    # ROI: Frontal Pole
    table_name = ifelse(variable == 'smri_area_cdk_frpolelh', 'Area: Frontal Pole (L)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_frpolelh', 'Sulcul Depth: Frontal Pole (L)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_frpolelh', 'Cortical Thickness: Frontal Pole (L)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_frpolelh', 'Volume: Frontal Pole (L)', table_name),
    
    table_name = ifelse(variable == 'smri_area_cdk_frpolerh', 'Area: Frontal Pole (R)', table_name),
    table_name = ifelse(variable == 'smri_sulc_cdk_frpolerh', 'Sulcul Depth: Frontal Pole (R)', table_name),
    table_name = ifelse(variable == 'smri_thick_cdk_frpolerh', 'Cortical Thickness: Frontal Pole (R)', table_name),
    table_name = ifelse(variable == 'smri_vol_cdk_frpolerh', 'Volume: Frontal Pole (R)', table_name),
    
    # ROI: Temporal Pole
    table_name = ifelse(variable == 'smri_area_cdk_tmpolelh', 'Area: Temporal Pole (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_tmpolelh', 'Sulcul Depth: Temporal Pole (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_tmpolelh', 'Cortical Thickness: Temporal Pole (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_tmpolelh', 'Volume: Temporal Pole (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_tmpolerh', 'Area: Temporal Pole (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_tmpolerh', 'Sulcul Depth: Temporal Pole (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_tmpolerh', 'Cortical Thickness: Temporal Pole (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_tmpolerh', 'Volume: Temporal Pole (R)', table_name), 
    
    # ROI: Transversetemporal
    table_name = ifelse(variable == 'smri_area_cdk_trvtmlh', 'Area: Transversetemporal (L)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_trvtmlh', 'Sulcul Depth: Transversetemporal (L)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_trvtmlh', 'cortical Thickness: Transversetemporal (L)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_trvtmlh', 'Volume: Transversetemporal (L)', table_name), 
    
    table_name = ifelse(variable == 'smri_area_cdk_trvtmrh', 'Area: Transversetemporal (R)', table_name), 
    table_name = ifelse(variable == 'smri_sulc_cdk_trvtmrh', 'Sulcul Depth: Transversetemporal (R)', table_name), 
    table_name = ifelse(variable == 'smri_thick_cdk_trvtmrh', 'cortical Thickness: Transversetemporal (R)', table_name), 
    table_name = ifelse(variable == 'smri_vol_cdk_trvtmrh', 'Volume: Transversetemporal (R)', table_name), 
    
    # ROI: Insula
    table_name = ifelse(variable == 'smri_area_cdk_insulalh', 'Area: Insula (L)', table_name),  
    table_name = ifelse(variable == 'smri_sulc_cdk_insulalh', 'Sulcul Depth: Insula (L)', table_name),  
    table_name = ifelse(variable == 'smri_thick_cdk_insulalh', 'Cortical Thickness: Insula (L)', table_name),  
    table_name = ifelse(variable == 'smri_vol_cdk_insulalh', 'Volume: Insula (L)', table_name),  
    
    table_name = ifelse(variable == 'smri_area_cdk_insularh', 'Area: Insula (R)', table_name),  
    table_name = ifelse(variable == 'smri_sulc_cdk_insularh', 'Sulcul Depth: Insula (R)', table_name),  
    table_name = ifelse(variable == 'smri_thick_cdk_insularh', 'Cortical Thickness: Insula (R)', table_name),  
    table_name = ifelse(variable == 'smri_vol_cdk_insularh', 'Volume: Insula (R)', table_name))

write.csv(m1_rf_supplemental,'output/random_forest/m1_rf_supplemental.csv')  
write.csv(m2_rf_supplemental,'output/random_forest/m2_rf_supplemental.csv')  
write.csv(m3_rf_supplemental,'output/random_forest/m3_rf_supplemental.csv')  

write.csv(m1_rf_summary_accuracy,'output/random_forest/m1_rf_summary_accuracy.csv')  
write.csv(m2_rf_summary_accuracy,'output/random_forest/m2_rf_summary_accuracy.csv')  
write.csv(m3_rf_summary_accuracy,'output/random_forest/m3_rf_summary_accuracy.csv')  


