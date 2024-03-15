# Configure Parallelization ----------------------------------------------------

## Detect core count
nCores <- min(parallel::detectCores())

## Used by parallel::mclapply() as default
options(mc.cores = nCores)

## Used by doParallel as default
options(cores = nCores)

## Register doParallel as the parallel backend with foreach
doParallel::registerDoParallel(cores = nCores)

## Report multicore use
cat("### Using", foreach::getDoParWorkers(), "cores\n")
cat("### Using", foreach::getDoParName(), "as backend\n")

# Assign IDs to Folds -----------------------------------------------------

nfolds <- 10
set.seed(48109)
foldid <- sample(rep(seq(nfolds), length.out = nrow(trn_1_mice_m1[[1]]))) 
foldid # n = 5121 for training dataset
table(foldid) 
# n = 513 in fold 1, n = 512 in folds 2 - 9 (aligns w/final sample size)

# Elastic Net: Model 1 -----------------------------------------------------------------

#------------------------------------------------------------------------------#
# (A) run elastic net on training dataset
#------------------------------------------------------------------------------#

### split (1)
job::job(m1_split1 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_1_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_1_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (1): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_1_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_1_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_1_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (2)
job::job(m1_split2 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_2_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_2_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }

  cat(dim(x_trn_m1[[1]]), 'split (2): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_2_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_2_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_2_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (3)
job::job(m1_split3 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_3_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_3_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }

  cat(dim(x_trn_m1[[1]]), 'split (3): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_3_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_3_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_3_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (4)
job::job(m1_split4 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_4_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_4_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }

  cat(dim(x_trn_m1[[1]]), 'split (4): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_4_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_4_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_4_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (5)
job::job(m1_split5 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_5_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_5_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (5): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_5_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_5_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_5_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (6)
job::job(m1_split6 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_6_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_6_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }

  cat(dim(x_trn_m1[[1]]), 'split (6): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_6_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_6_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_6_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (7)
job::job(m1_split7 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_7_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_7_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (7): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_7_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_7_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_7_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (8)
job::job(m1_split8 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_8_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_8_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (8): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_8_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_8_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_8_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (9)
job::job(m1_split9 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_9_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_9_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (9): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_9_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_9_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_9_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})

### split (10)
job::job(m1_split10 ={
  # matrix of imputed values 
  dfs_trn_m1 <- lapply(1:5, function(i) complete(trn_10_mice_m1, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m1 <- list()
  y_trn_m1 <- list()
  for (i in 1:5) {
    x_trn_m1[[i]] <- as.matrix(dfs_trn_m1[[i]][,(2:length(trn_10_obs_df_m1))]) # drop column 1 DV
    y_trn_m1[[i]] <- dfs_trn_m1[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m1[[1]]), 'split (10): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m1 <- 1 - rowMeans(is.na(trn_10_obs_df_m1))
  pf_trn_m1 <- rep(1, length(trn_10_obs_df_m1)-1) # repetitions = total number of predictors
  adWeight_trn_m1 <- rep(1, length(trn_10_obs_df_m1)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m1 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m1 <- miselect::cv.saenet(
      x_trn_m1, y_trn_m1, pf_trn_m1, adWeight_trn_m1, weights_trn_m1, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m1
  }
  
})


#------------------------------------------------------------------------------#
# (B) extract coefficients retained and associated output
#------------------------------------------------------------------------------#

# objects retained from each split: 
# - model fit details, AUC, sensitivity and specificity 

### split (1)
job::job(m1_split1_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split1$fit_m1$alpha.1se
  m1_alpha.min <- m1_split1$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split1$fit_m1$lambda.1se
  m1_lambda.min <- m1_split1$fit_m1$lambda.min
  
  m1_split1_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split1_fit) <- c('split 1')
  m1_split1_fit <- m1_split1_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split1$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split1_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split1_lambda_range) <- c('split 1')
  
  m1_df <- m1_split1$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_1/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split1$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split1$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_1/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split1$fit_m1,
                    lambda = m1_split1$fit_m1$lambda.1se, 
                    alpha = m1_split1$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split1$fit_m1, 
                    lambda = m1_split1$fit_m1$lambda.min, 
                    alpha = m1_split1$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (1)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_1/model_1/m1_cf.csv', 
            row.names = TRUE)

  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_1_temp <- trn_1_imp_df_m1 %>% 
      subset(.imp == i)
    trn_1_temp[, names(trn_1_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_1_temp <- tst_1_imp_df_m1 %>% 
      subset(.imp == i)
    tst_1_temp[, names(tst_1_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:(length(m1_cf_min_nz)-1))]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:(length(m1_cf_min_nz)-1))]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (1)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #----------------------------------------------------------------------------#  
  #                       Calculate probabilities
  #----------------------------------------------------------------------------# 
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities across 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_1/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_1/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  
  # double check each probability across the imputations is different 
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
     
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
     
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_1_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)

  tst_m1_DV_actual <- tst_1_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_1_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split1 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split1) <- c('training: split 1')
  
  # - test dataset
  m1_roc_tst <- roc(tst_1_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split1 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split1) <- c('test: split 1')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>% 
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)

  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 1): 50% threshold')
  ss_trn_0.50 
    
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 1): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
    
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 1): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 1): best threshold')
  ss_tst_best
  
  ss_m1_split1 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
 
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (2)
job::job(m1_split2_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split2$fit_m1$alpha.1se
  m1_alpha.min <- m1_split2$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split2$fit_m1$lambda.1se
  m1_lambda.min <- m1_split2$fit_m1$lambda.min
  
  m1_split2_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split2_fit) <- c('split 2')
  m1_split2_fit <- m1_split2_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split2$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split2_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split2_lambda_range) <- c('split 2')
  
  m1_df <- m1_split2$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_2/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split2$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split2$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_2/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split2$fit_m1,
                    lambda = m1_split2$fit_m1$lambda.1se, 
                    alpha = m1_split2$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split2$fit_m1, 
                    lambda = m1_split2$fit_m1$lambda.min, 
                    alpha = m1_split2$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (2)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_2/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_2_temp <- trn_2_imp_df_m1 %>% 
      subset(.imp == i)
    trn_2_temp[, names(trn_2_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_2_temp <- tst_2_imp_df_m1 %>% 
      subset(.imp == i)
    tst_2_temp[, names(tst_2_temp) %in% names(m1_cf_min_nz)]    

    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (2)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_2/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_2/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC,
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_2_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_2_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_2_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split2 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split2) <- c('training: split 2')
  
  # - test dataset
  m1_roc_tst <- roc(tst_2_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split2 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split2) <- c('test: split 2')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <-  m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 2): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 2): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 2): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 2): best threshold')
  ss_tst_best
  
  ss_m1_split2 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (3)
job::job(m1_split3_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split3$fit_m1$alpha.1se
  m1_alpha.min <- m1_split3$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split3$fit_m1$lambda.1se
  m1_lambda.min <- m1_split3$fit_m1$lambda.min
  
  m1_split3_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split3_fit) <- c('split 3')
  m1_split3_fit <- m1_split3_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split3$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split3_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split3_lambda_range) <- c('split 3')
  
  m1_df <- m1_split3$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_3/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split3$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split3$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_3/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split3$fit_m1,
                    lambda = m1_split3$fit_m1$lambda.1se, 
                    alpha = m1_split3$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split3$fit_m1, 
                    lambda = m1_split3$fit_m1$lambda.min, 
                    alpha = m1_split3$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (3)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_3/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_3_temp <- trn_3_imp_df_m1 %>% 
      subset(.imp == i)
    trn_3_temp[, names(trn_3_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_3_temp <- tst_3_imp_df_m1 %>% 
      subset(.imp == i)
    tst_3_temp[, names(tst_3_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (3)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_3/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_3/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_3_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_3_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_3_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split3 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split3) <- c('training: split 3')
  
  # - test dataset
  m1_roc_tst <- roc(tst_3_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split3 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split3) <- c('test: split 3')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 3): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 3): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 3): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 3): best threshold')
  ss_tst_best
  
  ss_m1_split3 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)

})

### split (4)
job::job(m1_split4_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split4$fit_m1$alpha.1se
  m1_alpha.min <- m1_split4$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split4$fit_m1$lambda.1se
  m1_lambda.min <- m1_split4$fit_m1$lambda.min
  
  m1_split4_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split4_fit) <- c('split 4')
  m1_split4_fit <- m1_split4_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split4$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split4_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split4_lambda_range) <- c('split 4')
  
  m1_df <- m1_split4$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_4/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split4$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split4$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_4/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split4$fit_m1,
                    lambda = m1_split4$fit_m1$lambda.1se, 
                    alpha = m1_split4$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split4$fit_m1, 
                    lambda = m1_split4$fit_m1$lambda.min, 
                    alpha = m1_split4$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (4)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_4/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_4_temp <- trn_4_imp_df_m1 %>% 
      subset(.imp == i)
    trn_4_temp[, names(trn_4_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_4_temp <- tst_4_imp_df_m1 %>% 
      subset(.imp == i)
    tst_4_temp[, names(tst_4_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (4)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_4/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_4/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_4_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_4_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_4_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split4 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split4) <- c('training: split 4')
  
  # - test dataset
  m1_roc_tst <- roc(tst_4_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split4 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split4) <- c('test: split 4')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (2) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 4): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 4): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 4): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 4): best threshold')
  ss_tst_best
  
  ss_m1_split4 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)

})

### split (5)
job::job(m1_split5_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split5$fit_m1$alpha.1se
  m1_alpha.min <- m1_split5$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split5$fit_m1$lambda.1se
  m1_lambda.min <- m1_split5$fit_m1$lambda.min
  
  m1_split5_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split5_fit) <- c('split 5')
  m1_split5_fit <- m1_split5_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split5$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split5_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split5_lambda_range) <- c('split 5')
  
  m1_df <- m1_split5$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_5/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split5$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split5$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_5/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split5$fit_m1,
                    lambda = m1_split5$fit_m1$lambda.1se, 
                    alpha = m1_split5$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split5$fit_m1, 
                    lambda = m1_split5$fit_m1$lambda.min, 
                    alpha = m1_split5$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (5)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_5/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_5_temp <- trn_5_imp_df_m1 %>% 
      subset(.imp == i)
    trn_5_temp[, names(trn_5_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_5_temp <- tst_5_imp_df_m1 %>% 
      subset(.imp == i)
    tst_5_temp[, names(tst_5_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (5)  \n') 
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_5/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_5/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_5_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_5_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  ## training dataset
  m1_roc_trn <- roc(trn_5_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split5 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split5) <- c('training: split 5')
  
  ## test dataset
  m1_roc_tst <- roc(tst_5_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split5 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split5) <- c('test: split 5')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 5): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 5): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 5): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 5): best threshold')
  ss_tst_best
  
  ss_m1_split5 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (6)
job::job(m1_split6_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split6$fit_m1$alpha.1se
  m1_alpha.min <- m1_split6$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split6$fit_m1$lambda.1se
  m1_lambda.min <- m1_split6$fit_m1$lambda.min
  
  m1_split6_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split6_fit) <- c('split 6')
  m1_split6_fit <- m1_split6_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split6$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split6_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split6_lambda_range) <- c('split 6')
  
  m1_df <- m1_split6$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_6/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split6$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split6$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_6/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split6$fit_m1,
                    lambda = m1_split6$fit_m1$lambda.1se, 
                    alpha = m1_split6$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split6$fit_m1, 
                    lambda = m1_split6$fit_m1$lambda.min, 
                    alpha = m1_split6$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (6)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_6/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  # generate new list of imputed design matrices and imputed responses for 
  # variables retained
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_6_temp <- trn_6_imp_df_m1 %>% 
      subset(.imp == i)
    trn_6_temp[, names(trn_6_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_6_temp <- tst_6_imp_df_m1 %>% 
      subset(.imp == i)
    tst_6_temp[, names(tst_6_temp) %in% names(m1_cf_min_nz)]  
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (6)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_6/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_6/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_6_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_6_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_6_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split6 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split6) <- c('training: split 6')
  
  # - test dataset
  m1_roc_tst <- roc(tst_6_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split6 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split6) <- c('test: split 6')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 6): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 6): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 6): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 6): best threshold')
  ss_tst_best
  
  ss_m1_split6 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (7)
job::job(m1_split7_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split7$fit_m1$alpha.1se
  m1_alpha.min <- m1_split7$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split7$fit_m1$lambda.1se
  m1_lambda.min <- m1_split7$fit_m1$lambda.min
  
  m1_split7_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split7_fit) <- c('Split 7')
  m1_split7_fit <- m1_split7_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split7$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split7_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split7_lambda_range) <- c('Split 7')
  
  m1_df <- m1_split7$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_7/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split7$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split7$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_7/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split7$fit_m1,
                    lambda = m1_split7$fit_m1$lambda.1se, 
                    alpha = m1_split7$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split7$fit_m1, 
                    lambda = m1_split7$fit_m1$lambda.min, 
                    alpha = m1_split7$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (7)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_7/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_7_temp <- trn_7_imp_df_m1 %>% 
      subset(.imp == i)
    trn_7_temp[, names(trn_7_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_7_temp <- tst_7_imp_df_m1 %>% 
      subset(.imp == i)
    tst_7_temp[, names(tst_7_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (7)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_7/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_7/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_7_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_7_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'Training (across 5 imputations): Median of controls < median of cases')
  } else {
    print(
      'Training (across 5 imputations): Median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'Test (across 5 imputations): Median of controls < median of cases')
  } else {
    print(
      'Test (across 5 imputations): Median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_7_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split7 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split7) <- c('Training: split 7')
  
  # - test dataset
  m1_roc_tst <- roc(tst_7_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split7 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split7) <- c('Test: split 7')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('Training (split 7): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('Test (split 7): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('Training (split 7): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('Test (split 7): best threshold method')
  ss_tst_best
  
  ss_m1_split7 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (8)
job::job(m1_split8_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split8$fit_m1$alpha.1se
  m1_alpha.min <- m1_split8$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split8$fit_m1$lambda.1se
  m1_lambda.min <- m1_split8$fit_m1$lambda.min
  
  m1_split8_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split8_fit) <- c('split 8')
  m1_split8_fit <- m1_split8_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split8$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split8_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split8_lambda_range) <- c('split 8')
  
  m1_df <- m1_split8$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_8/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split8$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split8$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_8/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split8$fit_m1,
                    lambda = m1_split8$fit_m1$lambda.1se, 
                    alpha = m1_split8$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split8$fit_m1, 
                    lambda = m1_split8$fit_m1$lambda.min, 
                    alpha = m1_split8$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (8)  \n')  
  
  write.csv(m1_cf,'output/elastic_net/split_8/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  

  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_8_temp <- trn_8_imp_df_m1 %>% 
      subset(.imp == i)
    trn_8_temp[, names(trn_8_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_8_temp <- tst_8_imp_df_m1 %>% 
      subset(.imp == i)
    tst_8_temp[, names(tst_8_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
    }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (8)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_8/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_8/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_8_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_8_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_8_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split8 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split8) <- c('training: split 8')
  
  # - test dataset
  m1_roc_tst <- roc(tst_8_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split8 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split8) <- c('test: split 8')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 8): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 8): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 8): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 8): best threshold')
  ss_tst_best
  
  ss_m1_split8 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)

})

### split (9)
job::job(m1_split9_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split9$fit_m1$alpha.1se
  m1_alpha.min <- m1_split9$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split9$fit_m1$lambda.1se
  m1_lambda.min <- m1_split9$fit_m1$lambda.min
  
  m1_split9_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split9_fit) <- c('split 9')
  m1_split9_fit <- m1_split9_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split9$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split9_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split9_lambda_range) <- c('split 9')
  
  m1_df <- m1_split9$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_9/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split9$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split9$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_9/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split9$fit_m1,
                    lambda = m1_split9$fit_m1$lambda.1se, 
                    alpha = m1_split9$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split9$fit_m1, 
                    lambda = m1_split9$fit_m1$lambda.min, 
                    alpha = m1_split9$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (9)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_9/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_9_temp <- trn_9_imp_df_m1 %>% 
      subset(.imp == i)
    trn_9_temp[, names(trn_9_temp) %in% names(m1_cf_min_nz)]  
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_9_temp <- tst_9_imp_df_m1 %>% 
      subset(.imp == i)
    tst_9_temp[, names(tst_9_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
  }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (9)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_9/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_9/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_9_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_9_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_9_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split9 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split9) <- c('training: split 9')
  
  # - test dataset
  m1_roc_tst <- roc(tst_9_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split9 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split9) <- c('test: split 9')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 9): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 9): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 9): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 9): best threshold')
  ss_tst_best
  
  ss_m1_split9 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

### split (10)
job::job(m1_split10_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m1_alpha.1se <- m1_split10$fit_m1$alpha.1se
  m1_alpha.min <- m1_split10$fit_m1$alpha.min  
  m1_lambda.1se <- m1_split10$fit_m1$lambda.1se
  m1_lambda.min <- m1_split10$fit_m1$lambda.min
  
  m1_split10_fit <- cbind(
    m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  row.names(m1_split10_fit) <- c('split 10')
  m1_split10_fit <- m1_split10_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m1_alpha.1se, m1_alpha.min, m1_lambda.1se, m1_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m1_lambda <- m1_split10$fit_m1$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m1_split10_lambda_range <- m1_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m1_split10_lambda_range) <- c('split 10')
  
  m1_df <- m1_split10$fit_m1$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m1_lambda_df <- cbind(m1_lambda, m1_df)
  
  write.csv(
    m1_lambda_df,'output/elastic_net/split_10/model_1/m1_lambda_df.csv')
  rm(m1_lambda, m1_df, m1_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m1_cvm <- m1_split10$fit_m1$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m1_cvm
  
  m1_cvse <- m1_split10$fit_m1$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m1_cvse
  
  m1_cv <- cbind(m1_cvm, m1_cvse)
  m1_cv
  names(m1_cv)
  
  write.csv(m1_cv,'output/elastic_net/split_10/model_1/m1_cv.csv')
  rm(m1_cvm, m1_cvse, m1_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m1_cf_1se <- coef(m1_split10$fit_m1,
                    lambda = m1_split10$fit_m1$lambda.1se, 
                    alpha = m1_split10$fit_m1$alpha.1se) 
  m1_cf_min <- coef(m1_split10$fit_m1, 
                    lambda = m1_split10$fit_m1$lambda.min, 
                    alpha = m1_split10$fit_m1$alpha.min) 
  
  m1_cf <- cbind(m1_cf_1se, m1_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m1_cf_min_exp = exp(abs(m1_cf_min))) %>% 
    arrange(desc(abs(m1_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m1_cf <- m1_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m1_cf_min_nz <- m1_cf_min %>% 
    subset(. != 0)
  names(m1_cf_min_nz) 
  cat(print(length(m1_cf_min_nz) - 1), 'non-zero predictors retained in model (1) split (10)  \n') 
  
  write.csv(m1_cf,'output/elastic_net/split_10/model_1/m1_cf.csv', 
            row.names = TRUE)
  
  rm(m1_cf_1se, m1_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m1_AUC <- lapply(1:5, function(i){
    trn_10_temp <- trn_10_imp_df_m1 %>% 
      subset(.imp == i)
    trn_10_temp[, names(trn_10_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m1_AUC <- lapply(1:5, function(i){
    tst_10_temp <- tst_10_imp_df_m1 %>% 
      subset(.imp == i)
    tst_10_temp[, names(tst_10_temp) %in% names(m1_cf_min_nz)]    
    }
  ) 
  
  x_trn_m1_AUC <- list()
  for (i in 1:5) {
    x_trn_m1_AUC[[i]] <- as.matrix(dfs_trn_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
  }
  
  x_tst_m1_AUC <- list()
  for (i in 1:5) {
    x_tst_m1_AUC[[i]] <- as.matrix(dfs_tst_m1_AUC[[i]][,(1:length(m1_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m1_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (1) split (10)  \n') 
  
  rm(dfs_trn_m1_AUC, dfs_tst_m1_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m1_imp1_trn <-x_trn_m1_AUC[[1]] 
  m1_imp2_trn <-x_trn_m1_AUC[[2]] 
  m1_imp3_trn <-x_trn_m1_AUC[[3]] 
  m1_imp4_trn <-x_trn_m1_AUC[[4]] 
  m1_imp5_trn <-x_trn_m1_AUC[[5]] 
  
  m1_imp1_tst <-x_tst_m1_AUC[[1]] 
  m1_imp2_tst <-x_tst_m1_AUC[[2]] 
  m1_imp3_tst <-x_tst_m1_AUC[[3]] 
  m1_imp4_tst <-x_tst_m1_AUC[[4]] 
  m1_imp5_tst <-x_tst_m1_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m1_imp1_trn <- cbind(rep(1,5121),m1_imp1_trn)
  m1_imp2_trn <- cbind(rep(1,5121),m1_imp2_trn)
  m1_imp3_trn <- cbind(rep(1,5121),m1_imp3_trn)
  m1_imp4_trn <- cbind(rep(1,5121),m1_imp4_trn)
  m1_imp5_trn <- cbind(rep(1,5121),m1_imp5_trn)
  
  m1_imp1_tst <- cbind(rep(1,1708),m1_imp1_tst)
  m1_imp2_tst <- cbind(rep(1,1708),m1_imp2_tst)
  m1_imp3_tst <- cbind(rep(1,1708),m1_imp3_tst)
  m1_imp4_tst <- cbind(rep(1,1708),m1_imp4_tst)
  m1_imp5_tst <- cbind(rep(1,1708),m1_imp5_tst)
  
  # compute probabilities
  
  m1_perc_imp1_trn <- m1_imp1_trn%*%m1_cf_min_nz 
  m1_perc_imp2_trn <- m1_imp2_trn%*%m1_cf_min_nz
  m1_perc_imp3_trn <- m1_imp3_trn%*%m1_cf_min_nz
  m1_perc_imp4_trn <- m1_imp4_trn%*%m1_cf_min_nz
  m1_perc_imp5_trn <- m1_imp5_trn%*%m1_cf_min_nz
  
  m1_perc_imp1_tst <- m1_imp1_tst%*%m1_cf_min_nz 
  m1_perc_imp2_tst <- m1_imp2_tst%*%m1_cf_min_nz
  m1_perc_imp3_tst <- m1_imp3_tst%*%m1_cf_min_nz
  m1_perc_imp4_tst <- m1_imp4_tst%*%m1_cf_min_nz
  m1_perc_imp5_tst <- m1_imp5_tst%*%m1_cf_min_nz
  
  m1_imp1_prob_trn <- exp(m1_perc_imp1_trn/(1+exp(m1_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_trn = V1)
  m1_imp2_prob_trn <- exp(m1_perc_imp2_trn/(1+exp(m1_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_trn = V1)
  m1_imp3_prob_trn <- exp(m1_perc_imp3_trn/(1+exp(m1_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_trn = V1)
  m1_imp4_prob_trn <- exp(m1_perc_imp4_trn/(1+exp(m1_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_trn = V1)
  m1_imp5_prob_trn <- exp(m1_perc_imp5_trn/(1+exp(m1_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_trn = V1)
  
  m1_imp1_prob_tst <- exp(m1_perc_imp1_tst/(1+exp(m1_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp1_prob_tst = V1)
  m1_imp2_prob_tst <- exp(m1_perc_imp2_tst/(1+exp(m1_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp2_prob_tst = V1)
  m1_imp3_prob_tst <- exp(m1_perc_imp3_tst/(1+exp(m1_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp3_prob_tst = V1)
  m1_imp4_prob_tst <- exp(m1_perc_imp4_tst/(1+exp(m1_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp4_prob_tst = V1)
  m1_imp5_prob_tst <- exp(m1_perc_imp5_tst/(1+exp(m1_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m1_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m1_prob_trn <- cbind(
    m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn, m1_imp4_prob_trn, 
    m1_imp5_prob_trn) %>% 
    mutate(m1_prob_avg_trn = rowMeans(.))
  write.csv(m1_prob_trn,'output/elastic_net/split_10/model_1/m1_prob_trn.csv', 
            row.names = TRUE)  
  
  m1_prob_tst <- cbind(
    m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, m1_imp4_prob_tst, 
    m1_imp5_prob_tst) %>% 
    mutate(m1_prob_avg_tst = rowMeans(.))
  write.csv(m1_prob_tst,'output/elastic_net/split_10/model_1/m1_prob_tst.csv', 
            row.names = TRUE)
  
  m1_prob_avg_trn <- m1_prob_trn %>% 
    select(m1_prob_avg_trn)
  m1_prob_avg_trn <- as.vector(m1_prob_avg_trn$m1_prob_avg_trn)
  
  m1_prob_avg_tst <- m1_prob_tst %>% 
    select(m1_prob_avg_tst)
  m1_prob_avg_tst <- as.vector(m1_prob_avg_tst$m1_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m1_imp1_prob_tst, m1_imp2_prob_tst)
  dc2 <- identical(m1_imp1_prob_tst, m1_imp3_prob_tst)
  dc3 <- identical(m1_imp1_prob_tst, m1_imp4_prob_tst)
  dc4 <- identical(m1_imp1_prob_tst, m1_imp5_prob_tst)
  dc5 <- identical(m1_imp2_prob_tst, m1_imp3_prob_tst)
  dc6 <- identical(m1_imp2_prob_tst, m1_imp4_prob_tst)
  dc7 <- identical(m1_imp2_prob_tst, m1_imp5_prob_tst)
  dc8 <- identical(m1_imp3_prob_tst, m1_imp4_prob_tst)
  dc9 <- identical(m1_imp3_prob_tst, m1_imp5_prob_tst)
  dc10 <- identical(m1_imp4_prob_tst, m1_imp5_prob_tst)
  
  dc11 <- identical(m1_imp1_prob_trn, m1_imp2_prob_trn)
  dc12 <- identical(m1_imp1_prob_trn, m1_imp3_prob_trn)
  dc13 <- identical(m1_imp1_prob_trn, m1_imp4_prob_trn)
  dc14 <- identical(m1_imp1_prob_trn, m1_imp5_prob_trn)
  dc15 <- identical(m1_imp2_prob_trn, m1_imp3_prob_trn)
  dc16 <- identical(m1_imp2_prob_trn, m1_imp4_prob_trn)
  dc17 <- identical(m1_imp2_prob_trn, m1_imp5_prob_trn)
  dc18 <- identical(m1_imp3_prob_trn, m1_imp4_prob_trn)
  dc19 <- identical(m1_imp3_prob_trn, m1_imp5_prob_trn)
  dc20 <- identical(m1_imp4_prob_trn, m1_imp5_prob_trn)
  
  dc21 <- identical(m1_imp1_prob_tst, m1_imp1_prob_trn)
  dc22 <- identical(m1_imp2_prob_tst, m1_imp2_prob_trn)
  dc23 <- identical(m1_imp3_prob_tst, m1_imp3_prob_trn)
  dc24 <- identical(m1_imp4_prob_tst, m1_imp4_prob_trn)
  dc25 <- identical(m1_imp5_prob_tst, m1_imp5_prob_trn)
  
  dc26 <- identical(m1_prob_avg_tst, m1_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m1_AUC, x_tst_m1_AUC, 
    
    m1_prob_trn, m1_prob_tst, 
    
    m1_imp1_trn, m1_imp2_trn, m1_imp3_trn, m1_imp4_trn, m1_imp5_trn,
    m1_perc_imp1_trn, m1_perc_imp2_trn, m1_perc_imp3_trn, m1_perc_imp4_trn, 
    m1_perc_imp5_trn, m1_imp1_prob_trn, m1_imp2_prob_trn, m1_imp3_prob_trn,
    m1_imp4_prob_trn, m1_imp5_prob_trn,
    
    m1_imp1_tst, m1_imp2_tst, m1_imp3_tst, m1_imp4_tst, m1_imp5_tst,
    m1_perc_imp1_tst, m1_perc_imp2_tst, m1_perc_imp3_tst, m1_perc_imp4_tst, 
    m1_perc_imp5_tst, m1_imp1_prob_tst, m1_imp2_prob_tst, m1_imp3_prob_tst, 
    m1_imp4_prob_tst, m1_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m1_DV_actual <- trn_10_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m1_DV_actual <- tst_10_obs_df_m1 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m1_actual_prob <- cbind(trn_m1_DV_actual, m1_prob_avg_trn) 
  tst_m1_actual_prob <- cbind(tst_m1_DV_actual, m1_prob_avg_tst) 
  rm(trn_m1_DV_actual, tst_m1_DV_actual)
  
  names(trn_m1_actual_prob)
  names(tst_m1_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_trn))
  
  tst_median <- tst_m1_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m1_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m1_roc_trn <- roc(trn_10_obs_df_m1$DV, m1_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m1_split10 <- ci.auc(m1_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m1_split10) <- c('training: split 10')
  
  # - test dataset
  m1_roc_tst <- roc(tst_10_obs_df_m1$DV, m1_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m1_split10 <- ci.auc(m1_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m1_split10) <- c('test: split 10')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m1_binary_pred <- m1_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m1_binary_pred <- m1_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m1_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m1_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m1_cm_data <- cbind(trn_m1_actual_prob, trn_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m1_cm_data <- cbind(tst_m1_actual_prob, tst_m1_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m1_cm_data$predicted_avg_0.50, trn_m1_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m1_cm_data$predicted_avg_0.50, tst_m1_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_1/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_1/tst_cm_0.50.txt') 
  
  rm(trn_m1_cm_data, tst_m1_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m1_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 10): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m1_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 10): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m1_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m1_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m1_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 10): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m1_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 10): best threshold')
  ss_tst_best
  
  ss_m1_split10 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m1_cf_min_nz, 
    m1_prob_avg_trn, m1_prob_avg_tst, 
    trn_m1_actual_prob, tst_m1_actual_prob,
    trn_m1_binary_pred, tst_m1_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m1_roc_trn, m1_roc_tst)
  
})

# Elastic Net: Model 2 -----------------------------------------------------------------

#------------------------------------------------------------------------------#
# (A) run elastic net &on training dataset
#------------------------------------------------------------------------------#

### split (1)
job::job(m2_split1 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_1_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_1_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (1): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_1_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_1_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_1_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (2)
job::job(m2_split2 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_2_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_2_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (2): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_2_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_2_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_2_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (3)
job::job(m2_split3 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_3_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_3_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (3): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_3_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_3_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_3_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (4)
job::job(m2_split4 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_4_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_4_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (4): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_4_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_4_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_4_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (5)
job::job(m2_split5 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_5_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_5_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }

  cat(dim(x_trn_m2[[1]]), 'split (5): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_5_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_5_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_5_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (6)
job::job(m2_split6 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_6_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_6_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (6): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_6_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_6_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_6_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (7)
job::job(m2_split7 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_7_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_7_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (7): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_7_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_7_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_7_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (8)
job::job(m2_split8 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_8_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_8_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (8): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_8_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_8_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_8_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (9)
job::job(m2_split9 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_9_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_9_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (9): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_9_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_9_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_9_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

### split (10)
job::job(m2_split10 ={
  # matrix of imputed values 
  dfs_trn_m2 <- lapply(1:5, function(i) complete(trn_10_mice_m2, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m2 <- list()
  y_trn_m2 <- list()
  for (i in 1:5) {
    x_trn_m2[[i]] <- as.matrix(dfs_trn_m2[[i]][,(2:length(trn_10_obs_df_m2))]) # drop column 1 DV
    y_trn_m2[[i]] <- dfs_trn_m2[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m2[[1]]), 'split (10): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m2 <- 1 - rowMeans(is.na(trn_10_obs_df_m2))
  pf_trn_m2 <- rep(1, length(trn_10_obs_df_m2)-1) # repetitions = total number of predictors
  adWeight_trn_m2 <- rep(1, length(trn_10_obs_df_m2)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m2 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m2 <- miselect::cv.saenet(
      x_trn_m2, y_trn_m2, pf_trn_m2, adWeight_trn_m2, weights_trn_m2, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m2
  }
  
})

#------------------------------------------------------------------------------#
# (B) extract coefficients retained and associated output
#------------------------------------------------------------------------------#

# objects retained from each split: 
# - model fit details, AUC, sensitivity and specificity 

### split (1)
job::job(m2_split1_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split1$fit_m2$alpha.1se
  m2_alpha.min <- m2_split1$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split1$fit_m2$lambda.1se
  m2_lambda.min <- m2_split1$fit_m2$lambda.min
  
  m2_split1_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split1_fit) <- c('split 1')
  m2_split1_fit <- m2_split1_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split1$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split1_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split1_lambda_range) <- c('split 1')
  
  m2_df <- m2_split1$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_1/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split1$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split1$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_1/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split1$fit_m2,
                    lambda = m2_split1$fit_m2$lambda.1se, 
                    alpha = m2_split1$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split1$fit_m2, 
                    lambda = m2_split1$fit_m2$lambda.min, 
                    alpha = m2_split1$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (1)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_1/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_1_temp <- trn_1_imp_df_m2 %>% 
      subset(.imp == i)
    trn_1_temp[, names(trn_1_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_1_temp <- tst_1_imp_df_m2 %>% 
      subset(.imp == i)
    tst_1_temp[, names(tst_1_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (1)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_1/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_1/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_1_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_1_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_1_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split1 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split1) <- c('training: split 1')
  
  # - test dataset
  m2_roc_tst <- roc(tst_1_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split1 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split1) <- c('test: split 1')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 1): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 1): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 1): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 1): best threshold')
  ss_tst_best
  
  ss_m2_split1 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)

})

### split (2)
job::job(m2_split2_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split2$fit_m2$alpha.1se
  m2_alpha.min <- m2_split2$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split2$fit_m2$lambda.1se
  m2_lambda.min <- m2_split2$fit_m2$lambda.min
  
  m2_split2_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split2_fit) <- c('Split 3')
  m2_split2_fit <- m2_split2_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split2$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split2_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split2_lambda_range) <- c('Split 2')
  
  m2_df <- m2_split2$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_2/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split2$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split2$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_2/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split2$fit_m2,
                    lambda = m2_split2$fit_m2$lambda.1se, 
                    alpha = m2_split2$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split2$fit_m2, 
                    lambda = m2_split2$fit_m2$lambda.min, 
                    alpha = m2_split2$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (2)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_2/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_2_temp <- trn_2_imp_df_m2 %>% 
      subset(.imp == i)
    trn_2_temp[, names(trn_2_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_2_temp <- tst_2_imp_df_m2 %>% 
      subset(.imp == i)
    tst_2_temp[, names(tst_2_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (2)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_2/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_2/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_2_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_2_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # -training dataset
  m2_roc_trn <- roc(trn_2_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split2 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split2) <- c('training: split 2')
  
  # - test dataset
  m2_roc_tst <- roc(tst_2_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split2 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split2) <- c('test: split 2')
  
  # (2) confusion matrix
  # - threshold > 0.50  
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 2): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 2): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 2): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 2): best threshold')
  ss_tst_best
  
  ss_m2_split2 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (3)
job::job(m2_split3_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split3$fit_m2$alpha.1se
  m2_alpha.min <- m2_split3$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split3$fit_m2$lambda.1se
  m2_lambda.min <- m2_split3$fit_m2$lambda.min
  
  m2_split3_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split3_fit) <- c('Split 3')
  m2_split3_fit <- m2_split3_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split3$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split3_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split3_lambda_range) <- c('Split 3')
  
  m2_df <- m2_split3$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_3/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split3$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split3$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_3/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split3$fit_m2,
                    lambda = m2_split3$fit_m2$lambda.1se, 
                    alpha = m2_split3$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split3$fit_m2, 
                    lambda = m2_split3$fit_m2$lambda.min, 
                    alpha = m2_split3$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (3)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_3/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_3_temp <- trn_3_imp_df_m2 %>% 
      subset(.imp == i)
    trn_3_temp[, names(trn_3_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_3_temp <- tst_3_imp_df_m2 %>% 
      subset(.imp == i)
    tst_3_temp[, names(tst_3_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (3)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_3/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_3/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
 
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_3_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_3_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_3_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split3 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split3) <- c('training: split 3')
  
  # - test dataset
  m2_roc_tst <- roc(tst_3_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split3 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split3) <- c('test: split 3')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 3): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 3): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 3): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 3): best threshold')
  ss_tst_best
  
  ss_m2_split3 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (4)
job::job(m2_split4_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split4$fit_m2$alpha.1se
  m2_alpha.min <- m2_split4$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split4$fit_m2$lambda.1se
  m2_lambda.min <- m2_split4$fit_m2$lambda.min
  
  m2_split4_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split4_fit) <- c('Split 4')
  m2_split4_fit <- m2_split4_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split4$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split4_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split4_lambda_range) <- c('Split 4')
  
  m2_df <- m2_split4$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_4/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split4$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split4$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_4/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split4$fit_m2,
                    lambda = m2_split4$fit_m2$lambda.1se, 
                    alpha = m2_split4$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split4$fit_m2, 
                    lambda = m2_split4$fit_m2$lambda.min, 
                    alpha = m2_split4$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (4)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_4/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_4_temp <- trn_4_imp_df_m2 %>% 
      subset(.imp == i)
    trn_4_temp[, names(trn_4_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_4_temp <- tst_4_imp_df_m2 %>% 
      subset(.imp == i)
    tst_4_temp[, names(tst_4_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (4)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_4/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_4/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_4_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_4_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_4_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split4 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split4) <- c('training: split 4')
  
  # - test dataset
  m2_roc_tst <- roc(tst_4_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split4 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split4) <- c('test: split 4')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 4): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 4): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 4): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 4): best threshold')
  ss_tst_best
  
  ss_m2_split4 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (5)
job::job(m2_split5_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split5$fit_m2$alpha.1se
  m2_alpha.min <- m2_split5$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split5$fit_m2$lambda.1se
  m2_lambda.min <- m2_split5$fit_m2$lambda.min
  
  m2_split5_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split5_fit) <- c('Split 5')
  m2_split5_fit <- m2_split5_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split5$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split5_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split5_lambda_range) <- c('Split 5')
  
  m2_df <- m2_split5$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_5/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split5$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split5$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_5/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split5$fit_m2,
                    lambda = m2_split5$fit_m2$lambda.1se, 
                    alpha = m2_split5$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split5$fit_m2, 
                    lambda = m2_split5$fit_m2$lambda.min, 
                    alpha = m2_split5$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (5)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_5/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_5_temp <- trn_5_imp_df_m2 %>% 
      subset(.imp == i)
    trn_5_temp[, names(trn_5_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_5_temp <- tst_5_imp_df_m2 %>% 
      subset(.imp == i)
    tst_5_temp[, names(tst_5_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (5)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_5/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_5/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_5_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_5_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_5_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split5 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split5) <- c('training: split 5')
  
  # - test dataset
  m2_roc_tst <- roc(tst_5_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split5 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split5) <- c('test: split 5')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 5): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 5): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 5): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 5): best threshold')
  ss_tst_best
  
  ss_m2_split5 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)

})

### split (6)
job::job(m2_split6_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split6$fit_m2$alpha.1se
  m2_alpha.min <- m2_split6$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split6$fit_m2$lambda.1se
  m2_lambda.min <- m2_split6$fit_m2$lambda.min
  
  m2_split6_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split6_fit) <- c('Split 6')
  m2_split6_fit <- m2_split6_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split6$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split6_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split6_lambda_range) <- c('Split 6')
  
  m2_df <- m2_split6$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_6/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split6$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split6$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_6/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split6$fit_m2,
                    lambda = m2_split6$fit_m2$lambda.1se, 
                    alpha = m2_split6$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split6$fit_m2, 
                    lambda = m2_split6$fit_m2$lambda.min, 
                    alpha = m2_split6$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (6)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_6/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_6_temp <- trn_6_imp_df_m2 %>% 
      subset(.imp == i)
    trn_6_temp[, names(trn_6_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_6_temp <- tst_6_imp_df_m2 %>% 
      subset(.imp == i)
    tst_6_temp[, names(tst_6_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (6)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_6/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_6/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC,
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_6_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_6_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_6_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split6 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split6) <- c('training: split 6')
  
  # - test dataset
  m2_roc_tst <- roc(tst_6_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split6 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split6) <- c('test: split 6')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 6): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 6): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 6): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 6): best threshold')
  ss_tst_best
  
  ss_m2_split6 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (7)
job::job(m2_split7_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split7$fit_m2$alpha.1se
  m2_alpha.min <- m2_split7$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split7$fit_m2$lambda.1se
  m2_lambda.min <- m2_split7$fit_m2$lambda.min
  
  m2_split7_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split7_fit) <- c('Split 7')
  m2_split7_fit <- m2_split7_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split7$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split7_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split7_lambda_range) <- c('Split 7')
  
  m2_df <- m2_split7$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_7/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split7$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split7$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_7/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split7$fit_m2,
                    lambda = m2_split7$fit_m2$lambda.1se, 
                    alpha = m2_split7$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split7$fit_m2, 
                    lambda = m2_split7$fit_m2$lambda.min, 
                    alpha = m2_split7$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (7)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_7/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_7_temp <- trn_7_imp_df_m2 %>% 
      subset(.imp == i)
    trn_7_temp[, names(trn_7_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_7_temp <- tst_7_imp_df_m2 %>% 
      subset(.imp == i)
    tst_7_temp[, names(tst_7_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (7)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_7/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_7/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_7_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_7_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_7_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split7 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split7) <- c('training: split 7')
  
  # - test dataset
  m2_roc_tst <- roc(tst_7_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split7 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split7) <- c('test: split 7')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 7): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 7): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 7): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 7): best threshold')
  ss_tst_best
  
  ss_m2_split7 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (8)
job::job(m2_split8_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split8$fit_m2$alpha.1se
  m2_alpha.min <- m2_split8$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split8$fit_m2$lambda.1se
  m2_lambda.min <- m2_split8$fit_m2$lambda.min
  
  m2_split8_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split8_fit) <- c('Split 8')
  m2_split8_fit <- m2_split8_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split8$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split8_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split8_lambda_range) <- c('Split 8')
  
  m2_df <- m2_split8$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_8/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split8$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split8$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_8/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split8$fit_m2,
                    lambda = m2_split8$fit_m2$lambda.1se, 
                    alpha = m2_split8$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split8$fit_m2, 
                    lambda = m2_split8$fit_m2$lambda.min, 
                    alpha = m2_split8$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (8)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_8/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_8_temp <- trn_8_imp_df_m2 %>% 
      subset(.imp == i)
    trn_8_temp[, names(trn_8_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_8_temp <- tst_8_imp_df_m2 %>% 
      subset(.imp == i)
    tst_8_temp[, names(tst_8_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (8)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_8/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_8/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_8_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_8_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_8_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split8 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split8) <- c('training: split 8')
  
  # - test dataset
  m2_roc_tst <- roc(tst_8_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split8 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split8) <- c('test: split 8')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 8): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 8): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 8): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 8): best threshold')
  ss_tst_best
  
  ss_m2_split8 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

### split (9)
job::job(m2_split9_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split9$fit_m2$alpha.1se
  m2_alpha.min <- m2_split9$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split9$fit_m2$lambda.1se
  m2_lambda.min <- m2_split9$fit_m2$lambda.min
  
  m2_split9_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split9_fit) <- c('Split 9')
  m2_split9_fit <- m2_split9_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split9$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split9_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split9_lambda_range) <- c('Split 9')
  
  m2_df <- m2_split9$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_9/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split9$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split9$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_9/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split9$fit_m2,
                    lambda = m2_split9$fit_m2$lambda.1se, 
                    alpha = m2_split9$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split9$fit_m2, 
                    lambda = m2_split9$fit_m2$lambda.min, 
                    alpha = m2_split9$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (9)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_9/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_9_temp <- trn_9_imp_df_m2 %>% 
      subset(.imp == i)
    trn_9_temp[, names(trn_9_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_9_temp <- tst_9_imp_df_m2 %>% 
      subset(.imp == i)
    tst_9_temp[, names(tst_9_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (9)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_9/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_9/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_9_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_9_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_9_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split9 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split9) <- c('training: split 9')
  
  # - test dataset
  m2_roc_tst <- roc(tst_9_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split9 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split9) <- c('test: split 9')
  
  # (2) confusion matrix
  # - threshold > 0.50  
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 9): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 9): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 9): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 9): best threshold')
  ss_tst_best
  
  ss_m2_split9 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)

})

### split (10)
job::job(m2_split10_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m2_alpha.1se <- m2_split10$fit_m2$alpha.1se
  m2_alpha.min <- m2_split10$fit_m2$alpha.min  
  m2_lambda.1se <- m2_split10$fit_m2$lambda.1se
  m2_lambda.min <- m2_split10$fit_m2$lambda.min
  
  m2_split10_fit <- cbind(
    m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  row.names(m2_split10_fit) <- c('Split 10')
  m2_split10_fit <- m2_split10_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m2_alpha.1se, m2_alpha.min, m2_lambda.1se, m2_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m2_lambda <- m2_split10$fit_m2$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m2_split10_lambda_range <- m2_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m2_split10_lambda_range) <- c('Split 10')
  
  m2_df <- m2_split10$fit_m2$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m2_lambda_df <- cbind(m2_lambda, m2_df)
  
  write.csv(
    m2_lambda_df,'output/elastic_net/split_10/model_2/m2_lambda_df.csv')
  rm(m2_lambda, m2_df, m2_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m2_cvm <- m2_split10$fit_m2$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m2_cvm
  
  m2_cvse <- m2_split10$fit_m2$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m2_cvse
  
  m2_cv <- cbind(m2_cvm, m2_cvse)
  m2_cv
  names(m2_cv)
  
  write.csv(m2_cv,'output/elastic_net/split_10/model_2/m2_cv.csv')
  rm(m2_cvm, m2_cvse, m2_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m2_cf_1se <- coef(m2_split10$fit_m2,
                    lambda = m2_split10$fit_m2$lambda.1se, 
                    alpha = m2_split10$fit_m2$alpha.1se) 
  m2_cf_min <- coef(m2_split10$fit_m2, 
                    lambda = m2_split10$fit_m2$lambda.min, 
                    alpha = m2_split10$fit_m2$alpha.min) 
  
  m2_cf <- cbind(m2_cf_1se, m2_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m2_cf_min_exp = exp(abs(m2_cf_min))) %>% 
    arrange(desc(abs(m2_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m2_cf <- m2_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m2_cf_min_nz <- m2_cf_min %>% 
    subset(. != 0)
  names(m2_cf_min_nz) 
  cat(print(length(m2_cf_min_nz) - 1), 'non-zero predictors retained in model (2) split (10)  \n') 
  
  write.csv(m2_cf,'output/elastic_net/split_10/model_2/m2_cf.csv', 
            row.names = TRUE)
  
  rm(m2_cf_1se, m2_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m2_AUC <- lapply(1:5, function(i){
    trn_10_temp <- trn_10_imp_df_m2 %>% 
      subset(.imp == i)
    trn_10_temp[, names(trn_10_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m2_AUC <- lapply(1:5, function(i){
    tst_10_temp <- tst_10_imp_df_m2 %>% 
      subset(.imp == i)
    tst_10_temp[, names(tst_10_temp) %in% names(m2_cf_min_nz)]    
    }
  ) 
  
  x_trn_m2_AUC <- list()
  for (i in 1:5) {
    x_trn_m2_AUC[[i]] <- as.matrix(dfs_trn_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  x_tst_m2_AUC <- list()
  for (i in 1:5) {
    x_tst_m2_AUC[[i]] <- as.matrix(dfs_tst_m2_AUC[[i]][,(1:length(m2_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m2_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (2) split (10)  \n') 
  
  rm(dfs_trn_m2_AUC, dfs_tst_m2_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m2_imp1_trn <-x_trn_m2_AUC[[1]] 
  m2_imp2_trn <-x_trn_m2_AUC[[2]] 
  m2_imp3_trn <-x_trn_m2_AUC[[3]] 
  m2_imp4_trn <-x_trn_m2_AUC[[4]] 
  m2_imp5_trn <-x_trn_m2_AUC[[5]] 
  
  m2_imp1_tst <-x_tst_m2_AUC[[1]] 
  m2_imp2_tst <-x_tst_m2_AUC[[2]] 
  m2_imp3_tst <-x_tst_m2_AUC[[3]] 
  m2_imp4_tst <-x_tst_m2_AUC[[4]] 
  m2_imp5_tst <-x_tst_m2_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m2_imp1_trn <- cbind(rep(1,5121),m2_imp1_trn)
  m2_imp2_trn <- cbind(rep(1,5121),m2_imp2_trn)
  m2_imp3_trn <- cbind(rep(1,5121),m2_imp3_trn)
  m2_imp4_trn <- cbind(rep(1,5121),m2_imp4_trn)
  m2_imp5_trn <- cbind(rep(1,5121),m2_imp5_trn)
  
  m2_imp1_tst <- cbind(rep(1,1708),m2_imp1_tst)
  m2_imp2_tst <- cbind(rep(1,1708),m2_imp2_tst)
  m2_imp3_tst <- cbind(rep(1,1708),m2_imp3_tst)
  m2_imp4_tst <- cbind(rep(1,1708),m2_imp4_tst)
  m2_imp5_tst <- cbind(rep(1,1708),m2_imp5_tst)
  
  # compute probabilities
  
  m2_perc_imp1_trn <- m2_imp1_trn%*%m2_cf_min_nz 
  m2_perc_imp2_trn <- m2_imp2_trn%*%m2_cf_min_nz
  m2_perc_imp3_trn <- m2_imp3_trn%*%m2_cf_min_nz
  m2_perc_imp4_trn <- m2_imp4_trn%*%m2_cf_min_nz
  m2_perc_imp5_trn <- m2_imp5_trn%*%m2_cf_min_nz
  
  m2_perc_imp1_tst <- m2_imp1_tst%*%m2_cf_min_nz 
  m2_perc_imp2_tst <- m2_imp2_tst%*%m2_cf_min_nz
  m2_perc_imp3_tst <- m2_imp3_tst%*%m2_cf_min_nz
  m2_perc_imp4_tst <- m2_imp4_tst%*%m2_cf_min_nz
  m2_perc_imp5_tst <- m2_imp5_tst%*%m2_cf_min_nz
  
  m2_imp1_prob_trn <- exp(m2_perc_imp1_trn/(1+exp(m2_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_trn = V1)
  m2_imp2_prob_trn <- exp(m2_perc_imp2_trn/(1+exp(m2_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_trn = V1)
  m2_imp3_prob_trn <- exp(m2_perc_imp3_trn/(1+exp(m2_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_trn = V1)
  m2_imp4_prob_trn <- exp(m2_perc_imp4_trn/(1+exp(m2_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_trn = V1)
  m2_imp5_prob_trn <- exp(m2_perc_imp5_trn/(1+exp(m2_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_trn = V1)
  
  m2_imp1_prob_tst <- exp(m2_perc_imp1_tst/(1+exp(m2_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp1_prob_tst = V1)
  m2_imp2_prob_tst <- exp(m2_perc_imp2_tst/(1+exp(m2_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp2_prob_tst = V1)
  m2_imp3_prob_tst <- exp(m2_perc_imp3_tst/(1+exp(m2_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp3_prob_tst = V1)
  m2_imp4_prob_tst <- exp(m2_perc_imp4_tst/(1+exp(m2_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp4_prob_tst = V1)
  m2_imp5_prob_tst <- exp(m2_perc_imp5_tst/(1+exp(m2_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m2_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m2_prob_trn <- cbind(
    m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn, m2_imp4_prob_trn, 
    m2_imp5_prob_trn) %>% 
    mutate(m2_prob_avg_trn = rowMeans(.))
  write.csv(m2_prob_trn,'output/elastic_net/split_10/model_2/m2_prob_trn.csv', 
            row.names = TRUE)  
  
  m2_prob_tst <- cbind(
    m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, m2_imp4_prob_tst, 
    m2_imp5_prob_tst) %>% 
    mutate(m2_prob_avg_tst = rowMeans(.))
  write.csv(m2_prob_tst,'output/elastic_net/split_10/model_2/m2_prob_tst.csv', 
            row.names = TRUE)
  
  m2_prob_avg_trn <- m2_prob_trn %>% 
    select(m2_prob_avg_trn)
  m2_prob_avg_trn <- as.vector(m2_prob_avg_trn$m2_prob_avg_trn)
  
  m2_prob_avg_tst <- m2_prob_tst %>% 
    select(m2_prob_avg_tst)
  m2_prob_avg_tst <- as.vector(m2_prob_avg_tst$m2_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m2_imp1_prob_tst, m2_imp2_prob_tst)
  dc2 <- identical(m2_imp1_prob_tst, m2_imp3_prob_tst)
  dc3 <- identical(m2_imp1_prob_tst, m2_imp4_prob_tst)
  dc4 <- identical(m2_imp1_prob_tst, m2_imp5_prob_tst)
  dc5 <- identical(m2_imp2_prob_tst, m2_imp3_prob_tst)
  dc6 <- identical(m2_imp2_prob_tst, m2_imp4_prob_tst)
  dc7 <- identical(m2_imp2_prob_tst, m2_imp5_prob_tst)
  dc8 <- identical(m2_imp3_prob_tst, m2_imp4_prob_tst)
  dc9 <- identical(m2_imp3_prob_tst, m2_imp5_prob_tst)
  dc10 <- identical(m2_imp4_prob_tst, m2_imp5_prob_tst)
  
  dc11 <- identical(m2_imp1_prob_trn, m2_imp2_prob_trn)
  dc12 <- identical(m2_imp1_prob_trn, m2_imp3_prob_trn)
  dc13 <- identical(m2_imp1_prob_trn, m2_imp4_prob_trn)
  dc14 <- identical(m2_imp1_prob_trn, m2_imp5_prob_trn)
  dc15 <- identical(m2_imp2_prob_trn, m2_imp3_prob_trn)
  dc16 <- identical(m2_imp2_prob_trn, m2_imp4_prob_trn)
  dc17 <- identical(m2_imp2_prob_trn, m2_imp5_prob_trn)
  dc18 <- identical(m2_imp3_prob_trn, m2_imp4_prob_trn)
  dc19 <- identical(m2_imp3_prob_trn, m2_imp5_prob_trn)
  dc20 <- identical(m2_imp4_prob_trn, m2_imp5_prob_trn)
  
  dc21 <- identical(m2_imp1_prob_tst, m2_imp1_prob_trn)
  dc22 <- identical(m2_imp2_prob_tst, m2_imp2_prob_trn)
  dc23 <- identical(m2_imp3_prob_tst, m2_imp3_prob_trn)
  dc24 <- identical(m2_imp4_prob_tst, m2_imp4_prob_trn)
  dc25 <- identical(m2_imp5_prob_tst, m2_imp5_prob_trn)
  
  dc26 <- identical(m2_prob_avg_tst, m2_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m2_AUC, x_tst_m2_AUC, 
    
    m2_prob_trn, m2_prob_tst, 
    
    m2_imp1_trn, m2_imp2_trn, m2_imp3_trn, m2_imp4_trn, m2_imp5_trn,
    m2_perc_imp1_trn, m2_perc_imp2_trn, m2_perc_imp3_trn, m2_perc_imp4_trn, 
    m2_perc_imp5_trn, m2_imp1_prob_trn, m2_imp2_prob_trn, m2_imp3_prob_trn,
    m2_imp4_prob_trn, m2_imp5_prob_trn,
    
    m2_imp1_tst, m2_imp2_tst, m2_imp3_tst, m2_imp4_tst, m2_imp5_tst,
    m2_perc_imp1_tst, m2_perc_imp2_tst, m2_perc_imp3_tst, m2_perc_imp4_tst, 
    m2_perc_imp5_tst, m2_imp1_prob_tst, m2_imp2_prob_tst, m2_imp3_prob_tst, 
    m2_imp4_prob_tst, m2_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m2_DV_actual <- trn_10_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m2_DV_actual <- tst_10_obs_df_m2 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m2_actual_prob <- cbind(trn_m2_DV_actual, m2_prob_avg_trn) 
  tst_m2_actual_prob <- cbind(tst_m2_DV_actual, m2_prob_avg_tst) 
  rm(trn_m2_DV_actual, tst_m2_DV_actual)
  
  names(trn_m2_actual_prob)
  names(tst_m2_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_trn))
  
  tst_median <- tst_m2_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m2_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m2_roc_trn <- roc(trn_10_obs_df_m2$DV, m2_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m2_split10 <- ci.auc(m2_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m2_split10) <- c('training: split 10')
  
  # - test dataset
  m2_roc_tst <- roc(tst_10_obs_df_m2$DV, m2_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m2_split10 <- ci.auc(m2_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m2_split10) <- c('test: split 10')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m2_binary_pred <- m2_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m2_binary_pred <- m2_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m2_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m2_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m2_cm_data <- cbind(trn_m2_actual_prob, trn_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m2_cm_data <- cbind(tst_m2_actual_prob, tst_m2_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m2_cm_data$predicted_avg_0.50, trn_m2_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m2_cm_data$predicted_avg_0.50, tst_m2_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_2/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_2/tst_cm_0.50.txt') 
  
  rm(trn_m2_cm_data, tst_m2_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m2_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 10): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m2_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 10): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m2_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m2_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m2_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 10): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m2_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 10): best threshold')
  ss_tst_best
  
  ss_m2_split10 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m2_cf_min_nz, 
    m2_prob_avg_trn, m2_prob_avg_tst, 
    trn_m2_actual_prob, tst_m2_actual_prob,
    trn_m2_binary_pred, tst_m2_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m2_roc_trn, m2_roc_tst)
  
})

# Elastic Net: Model 3 -----------------------------------------------------------------

#------------------------------------------------------------------------------#
# (A) run elastic net &on training dataset
#------------------------------------------------------------------------------#

### split (1)
job::job(m3_split1 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_1_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_1_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (1): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_1_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_1_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_1_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (2)
job::job(m3_split2 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_2_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_2_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (2): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_2_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_2_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_2_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (3)
job::job(m3_split3 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_3_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_3_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (3): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_3_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_3_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_3_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (4)
job::job(m3_split4 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_4_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_4_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (4): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_4_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_4_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_4_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (5)
job::job(m3_split5 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_5_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_5_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (5): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_5_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_5_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_5_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (6)
job::job(m3_split6 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_6_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_6_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (6): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_6_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_6_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_6_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (7)
job::job(m3_split7 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_7_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_7_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (7): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_7_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_7_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_7_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (8)
job::job(m3_split8 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_8_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_8_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (8): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_8_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_8_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_8_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (9)
job::job(m3_split9 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_9_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_9_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (9): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_9_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_9_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_9_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

### split (10)
job::job(m3_split10 ={
  # matrix of imputed values 
  dfs_trn_m3 <- lapply(1:5, function(i) complete(trn_10_mice_m3, action = i)) 
  
  # generate list of imputed design matrices and imputed responses
  x_trn_m3 <- list()
  y_trn_m3 <- list()
  for (i in 1:5) {
    x_trn_m3[[i]] <- as.matrix(dfs_trn_m3[[i]][,(2:length(trn_10_obs_df_m3))]) # drop column 1 DV
    y_trn_m3[[i]] <- dfs_trn_m3[[i]]$DV # save DV column separately
  }
  
  cat(dim(x_trn_m3[[1]]), 'split (10): IDs by predictors  \n')
  
  # calculate observational weights
  weights_trn_m3 <- 1 - rowMeans(is.na(trn_10_obs_df_m3))
  pf_trn_m3 <- rep(1, length(trn_10_obs_df_m3)-1) # repetitions = total number of predictors
  adWeight_trn_m3 <- rep(1, length(trn_10_obs_df_m3)-1) # repetitions = total number of predictors
  
  # parallelized execution of cross-validated stacked adaptive elastic net reg.
  
  kind <- RNGkind()
  kind
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  fit_m3 <- foreach(i = seq_len(1), .combine = ibind) %dorng% {
    set.seed(48109, kind = kind[1]) # set to default 'Mersenne-Twister'
    fit_m3 <- miselect::cv.saenet(
      x_trn_m3, y_trn_m3, pf_trn_m3, adWeight_trn_m3, weights_trn_m3, 
      family = "binomial",
      alpha = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      nfolds = nfolds, foldid = foldid)
    fit_m3
  }
  
})

#------------------------------------------------------------------------------#
# (B) extract coefficients retained and associated output
#------------------------------------------------------------------------------#

# objects retained from each split: 
# - model fit details, AUC, sensitivity and specificity 

### split (1)
job::job(m3_split1_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split1$fit_m3$alpha.1se
  m3_alpha.min <- m3_split1$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split1$fit_m3$lambda.1se
  m3_lambda.min <- m3_split1$fit_m3$lambda.min
  
  m3_split1_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split1_fit) <- c('split 1')
  m3_split1_fit <- m3_split1_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)

  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split1$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split1_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split1_lambda_range) <- c('split 1')
  
  m3_df <- m3_split1$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_1/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split1$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split1$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_1/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split1$fit_m3,
                    lambda = m3_split1$fit_m3$lambda.1se, 
                    alpha = m3_split1$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split1$fit_m3, 
                    lambda = m3_split1$fit_m3$lambda.min, 
                    alpha = m3_split1$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (1)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_1/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_1_temp <- trn_1_imp_df_m3 %>% 
      subset(.imp == i)
    trn_1_temp[, names(trn_1_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_1_temp <- tst_1_imp_df_m3 %>% 
      subset(.imp == i)
    tst_1_temp[, names(tst_1_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:(length(m3_cf_min_nz)-1))]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:(length(m3_cf_min_nz)-1))]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (1)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_1/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_1/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_1_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_1_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_1_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split1 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split1) <- c('training: split 1')
  
  # - test dataset
  m3_roc_tst <- roc(tst_1_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split1 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split1) <- c('test: split 1')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_1/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 1): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 1): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 1): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 1): best threshold')
  ss_tst_best
  
  ss_m3_split1 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)

})

### split (2)
job::job(m3_split2_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split2$fit_m3$alpha.1se
  m3_alpha.min <- m3_split2$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split2$fit_m3$lambda.1se
  m3_lambda.min <- m3_split2$fit_m3$lambda.min
  
  m3_split2_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split2_fit) <- c('split 2')
  m3_split2_fit <- m3_split2_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split2$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split2_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split2_lambda_range) <- c('Split 2')
  
  m3_df <- m3_split2$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_2/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split2$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split2$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_2/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split2$fit_m3,
                    lambda = m3_split2$fit_m3$lambda.1se, 
                    alpha = m3_split2$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split2$fit_m3, 
                    lambda = m3_split2$fit_m3$lambda.min, 
                    alpha = m3_split2$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (2)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_2/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_2_temp <- trn_2_imp_df_m3 %>% 
      subset(.imp == i)
    trn_2_temp[, names(trn_2_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_2_temp <- tst_2_imp_df_m3 %>% 
      subset(.imp == i)
    tst_2_temp[, names(tst_2_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (2)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_2/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_2/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_2_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_2_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_2_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split2 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split2) <- c('training: split 2')
  
  # - test dataset
  m3_roc_tst <- roc(tst_2_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split2 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split2) <- c('test: split 2')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_2/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 2): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 2): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 2): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 2): best threshold')
  ss_tst_best
  
  ss_m3_split2 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

### split (3)
job::job(m3_split3_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split3$fit_m3$alpha.1se
  m3_alpha.min <- m3_split3$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split3$fit_m3$lambda.1se
  m3_lambda.min <- m3_split3$fit_m3$lambda.min
  
  m3_split3_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split3_fit) <- c('split 3')
  m3_split3_fit <- m3_split3_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split3$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split3_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split3_lambda_range) <- c('Split 3')
  
  m3_df <- m3_split3$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_3/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split3$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split3$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_3/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split3$fit_m3,
                    lambda = m3_split3$fit_m3$lambda.1se, 
                    alpha = m3_split3$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split3$fit_m3, 
                    lambda = m3_split3$fit_m3$lambda.min, 
                    alpha = m3_split3$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (3)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_3/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_3_temp <- trn_3_imp_df_m3 %>% 
      subset(.imp == i)
    trn_3_temp[, names(trn_3_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_3_temp <- tst_3_imp_df_m3 %>% 
      subset(.imp == i)
    tst_3_temp[, names(tst_3_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (3)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_3/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_3/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_3_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_3_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_3_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split3 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split3) <- c('training: split 3')
  
  # - test dataset
  m3_roc_tst <- roc(tst_3_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split3 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split3) <- c('test: split 3')
  
  # (2) confusion matrix
  # - threshold > 0.5
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_3/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 3): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 3): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 3): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 3): best threshold')
  ss_tst_best
  
  ss_m3_split3 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)

})

### split (4)
job::job(m3_split4_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split4$fit_m3$alpha.1se
  m3_alpha.min <- m3_split4$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split4$fit_m3$lambda.1se
  m3_lambda.min <- m3_split4$fit_m3$lambda.min
  
  m3_split4_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split4_fit) <- c('Split 4')
  m3_split4_fit <- m3_split4_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split4$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split4_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split4_lambda_range) <- c('Split 4')
  
  m3_df <- m3_split4$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_4/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split4$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split4$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_4/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split4$fit_m3,
                    lambda = m3_split4$fit_m3$lambda.1se, 
                    alpha = m3_split4$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split4$fit_m3, 
                    lambda = m3_split4$fit_m3$lambda.min, 
                    alpha = m3_split4$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (4)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_4/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_4_temp <- trn_4_imp_df_m3 %>% 
      subset(.imp == i)
    trn_4_temp[, names(trn_4_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_4_temp <- tst_4_imp_df_m3 %>% 
      subset(.imp == i)
    tst_4_temp[, names(tst_4_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (4)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_4/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_4/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_4_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_4_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_4_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split4 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split4) <- c('training: split 4')
  
  # - test dataset
  m3_roc_tst <- roc(tst_4_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split4 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split4) <- c('test: split 4')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_4/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 4): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 4): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 4): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 4): best threshold')
  ss_tst_best
  
  ss_m3_split4 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

### split (5)
job::job(m3_split5_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split5$fit_m3$alpha.1se
  m3_alpha.min <- m3_split5$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split5$fit_m3$lambda.1se
  m3_lambda.min <- m3_split5$fit_m3$lambda.min
  
  m3_split5_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split5_fit) <- c('Split 5')
  m3_split5_fit <- m3_split5_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split5$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split5_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split5_lambda_range) <- c('Split 5')
  
  m3_df <- m3_split5$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_5/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split5$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split5$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_5/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split5$fit_m3,
                    lambda = m3_split5$fit_m3$lambda.1se, 
                    alpha = m3_split5$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split5$fit_m3, 
                    lambda = m3_split5$fit_m3$lambda.min, 
                    alpha = m3_split5$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (5)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_5/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_5_temp <- trn_5_imp_df_m3 %>% 
      subset(.imp == i)
    trn_5_temp[, names(trn_5_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_5_temp <- tst_5_imp_df_m3 %>% 
      subset(.imp == i)
    tst_5_temp[, names(tst_5_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (5)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_5/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_5/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_5_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_5_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_5_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split5 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split5) <- c('training: split 5')
  
  # - test dataset
  m3_roc_tst <- roc(tst_5_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split5 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split5) <- c('test: split 5')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_5/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 5): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 5): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 5): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 5): best threshold')
  ss_tst_best
  
  ss_m3_split5 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

### split (6)
job::job(m3_split6_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split6$fit_m3$alpha.1se
  m3_alpha.min <- m3_split6$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split6$fit_m3$lambda.1se
  m3_lambda.min <- m3_split6$fit_m3$lambda.min
  
  m3_split6_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split6_fit) <- c('split 6')
  m3_split6_fit <- m3_split6_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split6$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split6_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split6_lambda_range) <- c('split 6')
  
  m3_df <- m3_split6$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_6/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split6$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split6$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_6/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split6$fit_m3,
                    lambda = m3_split6$fit_m3$lambda.1se, 
                    alpha = m3_split6$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split6$fit_m3, 
                    lambda = m3_split6$fit_m3$lambda.min, 
                    alpha = m3_split6$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (6)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_6/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_6_temp <- trn_6_imp_df_m3 %>% 
      subset(.imp == i)
    trn_6_temp[, names(trn_6_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_6_temp <- tst_6_imp_df_m3 %>% 
      subset(.imp == i)
    tst_6_temp[, names(tst_6_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (6)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_6/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_6/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_6_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_1_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_6_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split6 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split6) <- c('training: split 6')
  
  # - test dataset
  m3_roc_tst <- roc(tst_6_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split6 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split6) <- c('test: split 6')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_6/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 6): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 6): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 6): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 6): best threshold')
  ss_tst_best
  
  ss_m3_split6 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)

})

### split (7)
job::job(m3_split7_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split7$fit_m3$alpha.1se
  m3_alpha.min <- m3_split7$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split7$fit_m3$lambda.1se
  m3_lambda.min <- m3_split7$fit_m3$lambda.min
  
  m3_split7_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split7_fit) <- c('split 7')
  m3_split7_fit <- m3_split7_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split7$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split7_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split7_lambda_range) <- c('Split 7')
  
  m3_df <- m3_split7$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_7/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split7$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split7$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_7/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split7$fit_m3,
                    lambda = m3_split7$fit_m3$lambda.1se, 
                    alpha = m3_split7$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split7$fit_m3, 
                    lambda = m3_split7$fit_m3$lambda.min, 
                    alpha = m3_split7$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (7)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_7/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_7_temp <- trn_7_imp_df_m3 %>% 
      subset(.imp == i)
    trn_7_temp[, names(trn_7_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_7_temp <- tst_7_imp_df_m3 %>% 
      subset(.imp == i)
    tst_7_temp[, names(tst_7_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (7)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_7/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_7/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_7_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_7_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_7_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split7 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split7) <- c('training: split 7')
  
  # - test dataset
  m3_roc_tst <- roc(tst_7_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split7 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split7) <- c('test: split 7')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_7/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 7): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 7): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 7): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 7): best threshold')
  ss_tst_best
  
  ss_m3_split7 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

### split (8)
job::job(m3_split8_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split8$fit_m3$alpha.1se
  m3_alpha.min <- m3_split8$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split8$fit_m3$lambda.1se
  m3_lambda.min <- m3_split8$fit_m3$lambda.min
  
  m3_split8_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split8_fit) <- c('split 8')
  m3_split8_fit <- m3_split8_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split8$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split8_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split8_lambda_range) <- c('Split 8')
  
  m3_df <- m3_split8$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_8/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split8$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split8$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_8/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split8$fit_m3,
                    lambda = m3_split8$fit_m3$lambda.1se, 
                    alpha = m3_split8$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split8$fit_m3, 
                    lambda = m3_split8$fit_m3$lambda.min, 
                    alpha = m3_split8$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (8)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_8/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_8_temp <- trn_8_imp_df_m3 %>% 
      subset(.imp == i)
    trn_8_temp[, names(trn_8_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_8_temp <- tst_8_imp_df_m3 %>% 
      subset(.imp == i)
    tst_8_temp[, names(tst_8_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (8)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_8/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_8/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_8_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_8_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_8_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split8 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split8) <- c('training: split 8')
  
  # - test dataset
  m3_roc_tst <- roc(tst_8_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split8 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split8) <- c('test: split 8')
  
  # (2) confusion matrix
  # - threshold > 0.50 
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>%  
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_8/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 8): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 8): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 8): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 8): best threshold')
  ss_tst_best
  
  ss_m3_split8 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

### split (9)
job::job(m3_split9_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split9$fit_m3$alpha.1se
  m3_alpha.min <- m3_split9$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split9$fit_m3$lambda.1se
  m3_lambda.min <- m3_split9$fit_m3$lambda.min
  
  m3_split9_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split9_fit) <- c('split 9')
  m3_split9_fit <- m3_split9_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split9$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split9_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split9_lambda_range) <- c('Split 9')
  
  m3_df <- m3_split9$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_9/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split9$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split9$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_9/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split9$fit_m3,
                    lambda = m3_split9$fit_m3$lambda.1se, 
                    alpha = m3_split9$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split9$fit_m3, 
                    lambda = m3_split9$fit_m3$lambda.min, 
                    alpha = m3_split9$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (9)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_9/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_9_temp <- trn_9_imp_df_m3 %>% 
      subset(.imp == i)
    trn_9_temp[, names(trn_9_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_9_temp <- tst_9_imp_df_m3 %>% 
      subset(.imp == i)
    tst_9_temp[, names(tst_9_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (9)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_9/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_9/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_9_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_9_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_9_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split9 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split9) <- c('training: split 9')
  
  # - test dataset
  m3_roc_tst <- roc(tst_9_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split9 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split9) <- c('test: split 9')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_9/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 9): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 9): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 9): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 9): best threshold')
  ss_tst_best
  
  ss_m3_split9 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
 
})

### split (10)
job::job(m3_split10_output ={
  
  #----------------------------------------------------------------------------#
  #                   Extract indices of model fit
  #----------------------------------------------------------------------------#
  
  # - alpha.min & lambda.min: for model w/minimum cross validation error
  # - alpha.1se & lambda.1se: for sparsest model within 1SE of the 
  #   minimum cross validation error
  m3_alpha.1se <- m3_split10$fit_m3$alpha.1se
  m3_alpha.min <- m3_split10$fit_m3$alpha.min  
  m3_lambda.1se <- m3_split10$fit_m3$lambda.1se
  m3_lambda.min <- m3_split10$fit_m3$lambda.min
  
  m3_split10_fit <- cbind(
    m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  row.names(m3_split10_fit) <- c('split 10')
  m3_split10_fit <- m3_split10_fit %>% 
    as.data.frame() %>% 
    round(., 3)
  
  rm(m3_alpha.1se, m3_alpha.min, m3_lambda.1se, m3_lambda.min)
  
  # - lambda: sequence of lambdas fit
  # - df: number of nonzero coefficients for each value of lambda & alpha
  m3_lambda <- m3_split10$fit_m3$lambda %>% 
    as.data.frame() %>% 
    rename(lambda = '.')
  
  m3_split10_lambda_range <- m3_lambda %>% 
    summarise(
      lambda_min = min(lambda),
      lambda_max = max(lambda)) %>% 
    round(., 3) 
  row.names(m3_split10_lambda_range) <- c('Split 10')
  
  m3_df <- m3_split10$fit_m3$df %>% 
    as.data.frame() %>% 
    rename(alpha_0 = V1,
           alpha_0.2 = V2,
           alpha_0.4 = V3,
           alpha_0.6 = V4,
           alpha_0.8 = V5,
           alpha_1.0 = V6)
  
  m3_lambda_df <- cbind(m3_lambda, m3_df)
  
  write.csv(
    m3_lambda_df,'output/elastic_net/split_10/model_3/m3_lambda_df.csv')
  rm(m3_lambda, m3_df, m3_lambda_df)
  
  # - cvm: average cross validation error for each lambda and alpha
  # - cvse: standard error of ’cvm’
  m3_cvm <- m3_split10$fit_m3$cvm %>% 
    as.data.frame() %>% 
    rename(cvm_alpha_0 = V1,
           cvm_alpha_0.2 = V2,
           cvm_alpha_0.4 = V3,
           cvm_alpha_0.6 = V4,
           cvm_alpha_0.8 = V5,
           cvm_alpha_1 = V6)
  m3_cvm
  
  m3_cvse <- m3_split10$fit_m3$cvse %>% 
    as.data.frame() %>% 
    rename(cvse_alpha_0 = V1,
           cvse_alpha_0.2 = V2,
           cvse_alpha_0.4 = V3,
           cvse_alpha_0.6 = V4,
           cvse_alpha_0.8 = V5,
           cvse_alpha_1 = V6)
  m3_cvse
  
  m3_cv <- cbind(m3_cvm, m3_cvse)
  m3_cv
  names(m3_cv)
  
  write.csv(m3_cv,'output/elastic_net/split_10/model_3/m3_cv.csv')
  rm(m3_cvm, m3_cvse, m3_cv)
  
  # extract coefficients for alpha.min and alpha.1se 
  m3_cf_1se <- coef(m3_split10$fit_m3,
                    lambda = m3_split10$fit_m3$lambda.1se, 
                    alpha = m3_split10$fit_m3$alpha.1se) 
  m3_cf_min <- coef(m3_split10$fit_m3, 
                    lambda = m3_split10$fit_m3$lambda.min, 
                    alpha = m3_split10$fit_m3$alpha.min) 
  
  m3_cf <- cbind(m3_cf_1se, m3_cf_min) %>% 
    as.data.frame() %>% 
    mutate(m3_cf_min_exp = exp(abs(m3_cf_min))) %>% 
    arrange(desc(abs(m3_cf_min))) %>% 
    rownames_to_column() %>% 
    rename(variable = rowname)
  
  m3_cf <- m3_cf %>% 
    left_join(table_names, by = 'variable') %>% 
    relocate(domain_name, table_name, variable)
  
  # subset non-zero coefficients 
  m3_cf_min_nz <- m3_cf_min %>% 
    subset(. != 0)
  names(m3_cf_min_nz) 
  cat(print(length(m3_cf_min_nz) - 1), 'non-zero predictors retained in model (3) split (10)  \n') 
  
  write.csv(m3_cf,'output/elastic_net/split_10/model_3/m3_cf.csv', 
            row.names = TRUE)
  
  rm(m3_cf_1se, m3_cf_min)
  
  #----------------------------------------------------------------------------#  
  #    New list of imputed design matrices & responses for variables retained
  #----------------------------------------------------------------------------#  
  
  dfs_trn_m3_AUC <- lapply(1:5, function(i){
    trn_10_temp <- trn_10_imp_df_m3 %>% 
      subset(.imp == i)
    trn_10_temp[, names(trn_10_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  dfs_tst_m3_AUC <- lapply(1:5, function(i){
    tst_10_temp <- tst_10_imp_df_m3 %>% 
      subset(.imp == i)
    tst_10_temp[, names(tst_10_temp) %in% names(m3_cf_min_nz)]    
    }
  ) 
  
  x_trn_m3_AUC <- list()
  for (i in 1:5) {
    x_trn_m3_AUC[[i]] <- as.matrix(dfs_trn_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  x_tst_m3_AUC <- list()
  for (i in 1:5) {
    x_tst_m3_AUC[[i]] <- as.matrix(dfs_tst_m3_AUC[[i]][,(1:length(m3_cf_min_nz)-1)]) 
  }
  
  cat(print(dim(x_tst_m3_AUC[[1]])), 
      'sample size and number of non-zero predictors retained in test dataset for model (3) split (10)  \n') 
  
  rm(dfs_trn_m3_AUC, dfs_tst_m3_AUC)
  
  #------------------------------------------------------------------------------#  
  #                 Calculate AUC for training and test dataset
  #------------------------------------------------------------------------------#  
  
  # assign each imputation to an individual matrix
  m3_imp1_trn <-x_trn_m3_AUC[[1]] 
  m3_imp2_trn <-x_trn_m3_AUC[[2]] 
  m3_imp3_trn <-x_trn_m3_AUC[[3]] 
  m3_imp4_trn <-x_trn_m3_AUC[[4]] 
  m3_imp5_trn <-x_trn_m3_AUC[[5]] 
  
  m3_imp1_tst <-x_tst_m3_AUC[[1]] 
  m3_imp2_tst <-x_tst_m3_AUC[[2]] 
  m3_imp3_tst <-x_tst_m3_AUC[[3]] 
  m3_imp4_tst <-x_tst_m3_AUC[[4]] 
  m3_imp5_tst <-x_tst_m3_AUC[[5]] 
  
  # create a vector of 1s for intercept
  m3_imp1_trn <- cbind(rep(1,5121),m3_imp1_trn)
  m3_imp2_trn <- cbind(rep(1,5121),m3_imp2_trn)
  m3_imp3_trn <- cbind(rep(1,5121),m3_imp3_trn)
  m3_imp4_trn <- cbind(rep(1,5121),m3_imp4_trn)
  m3_imp5_trn <- cbind(rep(1,5121),m3_imp5_trn)
  
  m3_imp1_tst <- cbind(rep(1,1708),m3_imp1_tst)
  m3_imp2_tst <- cbind(rep(1,1708),m3_imp2_tst)
  m3_imp3_tst <- cbind(rep(1,1708),m3_imp3_tst)
  m3_imp4_tst <- cbind(rep(1,1708),m3_imp4_tst)
  m3_imp5_tst <- cbind(rep(1,1708),m3_imp5_tst)
  
  # compute probabilities
  
  m3_perc_imp1_trn <- m3_imp1_trn%*%m3_cf_min_nz 
  m3_perc_imp2_trn <- m3_imp2_trn%*%m3_cf_min_nz
  m3_perc_imp3_trn <- m3_imp3_trn%*%m3_cf_min_nz
  m3_perc_imp4_trn <- m3_imp4_trn%*%m3_cf_min_nz
  m3_perc_imp5_trn <- m3_imp5_trn%*%m3_cf_min_nz
  
  m3_perc_imp1_tst <- m3_imp1_tst%*%m3_cf_min_nz 
  m3_perc_imp2_tst <- m3_imp2_tst%*%m3_cf_min_nz
  m3_perc_imp3_tst <- m3_imp3_tst%*%m3_cf_min_nz
  m3_perc_imp4_tst <- m3_imp4_tst%*%m3_cf_min_nz
  m3_perc_imp5_tst <- m3_imp5_tst%*%m3_cf_min_nz
  
  m3_imp1_prob_trn <- exp(m3_perc_imp1_trn/(1+exp(m3_perc_imp1_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_trn = V1)
  m3_imp2_prob_trn <- exp(m3_perc_imp2_trn/(1+exp(m3_perc_imp2_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_trn = V1)
  m3_imp3_prob_trn <- exp(m3_perc_imp3_trn/(1+exp(m3_perc_imp3_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_trn = V1)
  m3_imp4_prob_trn <- exp(m3_perc_imp4_trn/(1+exp(m3_perc_imp4_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_trn = V1)
  m3_imp5_prob_trn <- exp(m3_perc_imp5_trn/(1+exp(m3_perc_imp5_trn))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_trn = V1)
  
  m3_imp1_prob_tst <- exp(m3_perc_imp1_tst/(1+exp(m3_perc_imp1_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp1_prob_tst = V1)
  m3_imp2_prob_tst <- exp(m3_perc_imp2_tst/(1+exp(m3_perc_imp2_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp2_prob_tst = V1)
  m3_imp3_prob_tst <- exp(m3_perc_imp3_tst/(1+exp(m3_perc_imp3_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp3_prob_tst = V1)
  m3_imp4_prob_tst <- exp(m3_perc_imp4_tst/(1+exp(m3_perc_imp4_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp4_prob_tst = V1)
  m3_imp5_prob_tst <- exp(m3_perc_imp5_tst/(1+exp(m3_perc_imp5_tst))) %>% 
    as.data.frame() %>% 
    rename(m3_imp5_prob_tst = V1)
  
  # average probabilities aross 5 imputations
  m3_prob_trn <- cbind(
    m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn, m3_imp4_prob_trn, 
    m3_imp5_prob_trn) %>% 
    mutate(m3_prob_avg_trn = rowMeans(.))
  write.csv(m3_prob_trn,'output/elastic_net/split_10/model_3/m3_prob_trn.csv', 
            row.names = TRUE)  
  
  m3_prob_tst <- cbind(
    m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, m3_imp4_prob_tst, 
    m3_imp5_prob_tst) %>% 
    mutate(m3_prob_avg_tst = rowMeans(.))
  write.csv(m3_prob_tst,'output/elastic_net/split_10/model_3/m3_prob_tst.csv', 
            row.names = TRUE)
  
  m3_prob_avg_trn <- m3_prob_trn %>% 
    select(m3_prob_avg_trn)
  m3_prob_avg_trn <- as.vector(m3_prob_avg_trn$m3_prob_avg_trn)
  
  m3_prob_avg_tst <- m3_prob_tst %>% 
    select(m3_prob_avg_tst)
  m3_prob_avg_tst <- as.vector(m3_prob_avg_tst$m3_prob_avg_tst)
  
  # double check each probability across the imputations is different 
  
  dc1 <- identical(m3_imp1_prob_tst, m3_imp2_prob_tst)
  dc2 <- identical(m3_imp1_prob_tst, m3_imp3_prob_tst)
  dc3 <- identical(m3_imp1_prob_tst, m3_imp4_prob_tst)
  dc4 <- identical(m3_imp1_prob_tst, m3_imp5_prob_tst)
  dc5 <- identical(m3_imp2_prob_tst, m3_imp3_prob_tst)
  dc6 <- identical(m3_imp2_prob_tst, m3_imp4_prob_tst)
  dc7 <- identical(m3_imp2_prob_tst, m3_imp5_prob_tst)
  dc8 <- identical(m3_imp3_prob_tst, m3_imp4_prob_tst)
  dc9 <- identical(m3_imp3_prob_tst, m3_imp5_prob_tst)
  dc10 <- identical(m3_imp4_prob_tst, m3_imp5_prob_tst)
  
  dc11 <- identical(m3_imp1_prob_trn, m3_imp2_prob_trn)
  dc12 <- identical(m3_imp1_prob_trn, m3_imp3_prob_trn)
  dc13 <- identical(m3_imp1_prob_trn, m3_imp4_prob_trn)
  dc14 <- identical(m3_imp1_prob_trn, m3_imp5_prob_trn)
  dc15 <- identical(m3_imp2_prob_trn, m3_imp3_prob_trn)
  dc16 <- identical(m3_imp2_prob_trn, m3_imp4_prob_trn)
  dc17 <- identical(m3_imp2_prob_trn, m3_imp5_prob_trn)
  dc18 <- identical(m3_imp3_prob_trn, m3_imp4_prob_trn)
  dc19 <- identical(m3_imp3_prob_trn, m3_imp5_prob_trn)
  dc20 <- identical(m3_imp4_prob_trn, m3_imp5_prob_trn)
  
  dc21 <- identical(m3_imp1_prob_tst, m3_imp1_prob_trn)
  dc22 <- identical(m3_imp2_prob_tst, m3_imp2_prob_trn)
  dc23 <- identical(m3_imp3_prob_tst, m3_imp3_prob_trn)
  dc24 <- identical(m3_imp4_prob_tst, m3_imp4_prob_trn)
  dc25 <- identical(m3_imp5_prob_tst, m3_imp5_prob_trn)
  
  dc26 <- identical(m3_prob_avg_tst, m3_prob_avg_trn)
  
  if (any(dc1 == FALSE && dc2 == FALSE && dc3 == FALSE && 
          dc4 == FALSE && dc5 == FALSE && dc6 == FALSE && 
          dc7 == FALSE && dc8 == FALSE && dc9 == FALSE && 
          dc10 == FALSE && dc11 == FALSE && dc12 == FALSE && 
          dc13 == FALSE && dc14 == FALSE && dc15 == FALSE && 
          dc16 == FALSE && dc17 == FALSE && dc18 == FALSE && 
          dc19 == FALSE && dc20 == FALSE && dc21 == FALSE && 
          dc22 == FALSE && dc23 == FALSE && dc24 == FALSE && 
          dc25 == FALSE && dc26 == FALSE)) {
    print('No issues w/probability double check')
  } else {
    print('Issues w/probability double check')
  }
  
  rm(
    x_trn_m3_AUC, x_tst_m3_AUC, 
    
    m3_prob_trn, m3_prob_tst, 
    
    m3_imp1_trn, m3_imp2_trn, m3_imp3_trn, m3_imp4_trn, m3_imp5_trn,
    m3_perc_imp1_trn, m3_perc_imp2_trn, m3_perc_imp3_trn, m3_perc_imp4_trn, 
    m3_perc_imp5_trn, m3_imp1_prob_trn, m3_imp2_prob_trn, m3_imp3_prob_trn,
    m3_imp4_prob_trn, m3_imp5_prob_trn,
    
    m3_imp1_tst, m3_imp2_tst, m3_imp3_tst, m3_imp4_tst, m3_imp5_tst,
    m3_perc_imp1_tst, m3_perc_imp2_tst, m3_perc_imp3_tst, m3_perc_imp4_tst, 
    m3_perc_imp5_tst, m3_imp1_prob_tst, m3_imp2_prob_tst, m3_imp3_prob_tst, 
    m3_imp4_prob_tst, m3_imp5_prob_tst,
    
    dc1, dc2, dc3, dc4, dc5, dc6, dc7, dc8, dc9, dc10,
    dc11, dc12, dc13, dc14, dc15, dc16, dc17, dc18, dc19, dc20,
    dc21, dc22, dc23, dc24, dc25, dc26)
  
  # double check direction for roc
  
  # - subset DV 
  trn_m3_DV_actual <- trn_10_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  tst_m3_DV_actual <- tst_10_obs_df_m3 %>% 
    rename(actual = DV) %>% 
    select(actual)
  
  # - bind actual and average predicted probability values across 5 imputations
  trn_m3_actual_prob <- cbind(trn_m3_DV_actual, m3_prob_avg_trn) 
  tst_m3_actual_prob <- cbind(tst_m3_DV_actual, m3_prob_avg_tst) 
  rm(trn_m3_DV_actual, tst_m3_DV_actual)
  
  names(trn_m3_actual_prob)
  names(tst_m3_actual_prob)
  
  # - calculate median probability split by DV
  trn_median <- trn_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_trn))
  
  tst_median <- tst_m3_actual_prob %>% 
    group_by(actual) %>% 
    summarise(median_value = median(m3_prob_avg_tst))
  
  trn_median_DV0 <- trn_median %>% 
    subset(actual == 0) 
  trn_median_DV1 <- trn_median %>% 
    subset(actual == 1) 
  
  tst_median_DV0 <- tst_median %>% 
    subset(actual == 0) 
  tst_median_DV1 <- tst_median %>% 
    subset(actual == 1) 
  
  if ((trn_median_DV0$median_value < trn_median_DV1$median_value)) {
    print(
      'training (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'training (across 5 imputations): median of controls > median of cases')
  }
  
  if ((tst_median_DV0$median_value < tst_median_DV1$median_value)) {
    print(
      'test (across 5 imputations): median of controls < median of cases')
  } else {
    print(
      'test (across 5 imputations): median of controls > median of cases')
  }
  
  rm(trn_median, tst_median, 
     trn_median_DV0, trn_median_DV1, 
     tst_median_DV0, tst_median_DV1)
  
  #----------------------------------------------------------------------------#  
  #                   Calculate AUC and additional metrics
  #----------------------------------------------------------------------------#  
  
  # (1) AUC
  
  # - training dataset
  m3_roc_trn <- roc(trn_10_obs_df_m3$DV, m3_prob_avg_trn) %>% 
    as.vector()
  
  AUC_CI_trn_m3_split10 <- ci.auc(m3_roc_trn) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_trn_m3_split10) <- c('training: split 10')
  
  # - test dataset
  m3_roc_tst <- roc(tst_10_obs_df_m3$DV, m3_prob_avg_tst) %>% 
    as.vector()
  
  AUC_CI_tst_m3_split10 <- ci.auc(m3_roc_tst) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() %>% 
    rename(
      lower_CI = V1,
      AUC = V2,
      upper_CI = V3) %>% 
    relocate(AUC) %>% 
    mutate(
      AUC = as.numeric(AUC),
      lower_CI = as.numeric(lower_CI),
      upper_CI = as.numeric(upper_CI)) %>% 
    round(., 3)
  row.names(AUC_CI_tst_m3_split10) <- c('test: split 10')
  
  # (2) confusion matrix
  # - threshold > 0.50
  
  trn_m3_binary_pred <- m3_prob_avg_trn %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_trn = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_trn > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  tst_m3_binary_pred <- m3_prob_avg_tst %>%
    as.data.frame() %>% 
    rename(m3_prob_avg_tst = '.') %>% 
    mutate(predicted_avg_0.50 = ifelse(m3_prob_avg_tst > 0.50, 1, 0)) %>% 
    select(predicted_avg_0.50)
  
  trn_m3_cm_data <- cbind(trn_m3_actual_prob, trn_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  tst_m3_cm_data <- cbind(tst_m3_actual_prob, tst_m3_binary_pred) %>% 
    select(actual, predicted_avg_0.50) %>% 
    mutate_at(c('actual', 'predicted_avg_0.50'), as.factor)
  
  trn_cm_0.50 <- confusionMatrix(
    trn_m3_cm_data$predicted_avg_0.50, trn_m3_cm_data$actual, positive = c("1")) 
  tst_cm_0.50 <- confusionMatrix(
    tst_m3_cm_data$predicted_avg_0.50, tst_m3_cm_data$actual, positive = c("1")) 
  
  capture.output(trn_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_3/trn_cm_0.50.txt') 
  capture.output(tst_cm_0.50, 
                 file = 'output/elastic_net/split_10/model_3/tst_cm_0.50.txt') 
  
  rm(trn_m3_cm_data, tst_m3_cm_data, trn_cm_0.50, tst_cm_0.50)
  
  # (3) sensitivity and specificity
  
  # - 50% threshold 
  ss_trn_0.50 <- coords(m3_roc_trn, 0.50, transpose = FALSE) 
  row.names(ss_trn_0.50) <- c('training (split 10): 50% threshold')
  ss_trn_0.50 
  
  ss_tst_0.50 <- coords(m3_roc_tst, 0.50, transpose = FALSE)
  row.names(ss_tst_0.50) <- c('test (split 10): 50% threshold')
  ss_tst_0.50
  
  # - best threshold
  trn_best <- coords(m3_roc_trn, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  tst_best <- coords(m3_roc_tst, 'best', ret = 'threshold', transpose = FALSE) %>% 
    as.numeric()
  
  ss_trn_best <- coords(
    m3_roc_trn, trn_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_trn_best) <- c('training (split 10): best threshold')
  ss_trn_best
  
  ss_tst_best <- coords(
    m3_roc_tst, tst_best, transpose = FALSE, best.method = 'youden') 
  row.names(ss_tst_best) <- c('test (split 10): best threshold')
  ss_tst_best
  
  ss_m3_split10 <- round(
    rbind(ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best), 3)
  
  rm(
    m3_cf_min_nz, 
    m3_prob_avg_trn, m3_prob_avg_tst, 
    trn_m3_actual_prob, tst_m3_actual_prob,
    trn_m3_binary_pred, tst_m3_binary_pred,
    trn_best, tst_best, 
    ss_trn_0.50, ss_tst_0.50, ss_trn_best, ss_tst_best,
    m3_roc_trn, m3_roc_tst)
  
})

# Average Results Across Splits ------------------------------------------------

# (1) fit values: alpha & lambda min and 1se
fit_m1 <- rbind(
  m1_split1_output$m1_split1_fit, m1_split2_output$m1_split2_fit,
  m1_split3_output$m1_split3_fit, m1_split4_output$m1_split4_fit,
  m1_split5_output$m1_split5_fit, m1_split6_output$m1_split6_fit,
  m1_split7_output$m1_split7_fit, m1_split8_output$m1_split8_fit,
  m1_split9_output$m1_split9_fit, m1_split10_output$m1_split10_fit)

fit_m2 <- rbind(
  m2_split1_output$m2_split1_fit, m2_split2_output$m2_split2_fit,
  m2_split3_output$m2_split3_fit, m2_split4_output$m2_split4_fit,
  m2_split5_output$m2_split5_fit, m2_split6_output$m2_split6_fit,
  m2_split7_output$m2_split7_fit, m2_split8_output$m2_split8_fit,
  m2_split9_output$m2_split9_fit, m2_split10_output$m2_split10_fit)

fit_m3 <- rbind(
  m3_split1_output$m3_split1_fit, m3_split2_output$m3_split2_fit,
  m3_split3_output$m3_split3_fit, m3_split4_output$m3_split4_fit,
  m3_split5_output$m3_split5_fit, m3_split6_output$m3_split6_fit,
  m3_split7_output$m3_split7_fit, m3_split8_output$m3_split8_fit,
  m3_split9_output$m3_split9_fit, m3_split10_output$m3_split10_fit)

write.csv(fit_m1, 'output/elastic_net/fit_m1.csv')  
write.csv(fit_m2, 'output/elastic_net/fit_m2.csv')  
write.csv(fit_m3, 'output/elastic_net/fit_m3.csv')  
  
# (2) range for lambda (individual lambda exported within job)
lambda_range_m1 <- rbind(
  m1_split1_output$m1_split1_lambda_range, m1_split2_output$m1_split2_lambda_range,
  m1_split3_output$m1_split3_lambda_range, m1_split4_output$m1_split4_lambda_range,
  m1_split5_output$m1_split5_lambda_range, m1_split6_output$m1_split6_lambda_range,
  m1_split7_output$m1_split7_lambda_range, m1_split8_output$m1_split8_lambda_range,
  m1_split9_output$m1_split9_lambda_range, m1_split10_output$m1_split10_lambda_range)

lambda_range_m2 <- rbind(
  m2_split1_output$m2_split1_lambda_range, m2_split2_output$m2_split2_lambda_range,
  m2_split3_output$m2_split3_lambda_range, m2_split4_output$m2_split4_lambda_range,
  m2_split5_output$m2_split5_lambda_range, m2_split6_output$m2_split6_lambda_range,
  m2_split7_output$m2_split7_lambda_range, m2_split8_output$m2_split8_lambda_range,
  m2_split9_output$m2_split9_lambda_range, m2_split10_output$m2_split10_lambda_range)

lambda_range_m3 <- rbind(
  m3_split1_output$m3_split1_lambda_range, m3_split2_output$m3_split2_lambda_range,
  m3_split3_output$m3_split3_lambda_range, m3_split4_output$m3_split4_lambda_range,
  m3_split5_output$m3_split5_lambda_range, m3_split6_output$m3_split6_lambda_range,
  m3_split7_output$m3_split7_lambda_range, m3_split8_output$m3_split8_lambda_range,
  m3_split9_output$m3_split9_lambda_range, m3_split10_output$m3_split10_lambda_range)
  
write.csv(lambda_range_m1, 'output/elastic_net/lambda_range_m1.csv')  
write.csv(lambda_range_m2, 'output/elastic_net/lambda_range_m2.csv')  
write.csv(lambda_range_m3, 'output/elastic_net/lambda_range_m3.csv')  

# (3) AUC
AUC_m1 <- rbind(
  m1_split1_output$AUC_CI_trn_m1_split1, m1_split2_output$AUC_CI_trn_m1_split2,
  m1_split3_output$AUC_CI_trn_m1_split3, m1_split4_output$AUC_CI_trn_m1_split4,
  m1_split5_output$AUC_CI_trn_m1_split5, m1_split6_output$AUC_CI_trn_m1_split6,
  m1_split7_output$AUC_CI_trn_m1_split7, m1_split8_output$AUC_CI_trn_m1_split8,
  m1_split9_output$AUC_CI_trn_m1_split9, m1_split10_output$AUC_CI_trn_m1_split10,
  
  m1_split1_output$AUC_CI_tst_m1_split1, m1_split2_output$AUC_CI_tst_m1_split2,
  m1_split3_output$AUC_CI_tst_m1_split3, m1_split4_output$AUC_CI_tst_m1_split4,
  m1_split5_output$AUC_CI_tst_m1_split5, m1_split6_output$AUC_CI_tst_m1_split6,
  m1_split7_output$AUC_CI_tst_m1_split7, m1_split8_output$AUC_CI_tst_m1_split8,
  m1_split9_output$AUC_CI_tst_m1_split9, m1_split10_output$AUC_CI_tst_m1_split10)

AUC_m2 <- rbind(
  m2_split1_output$AUC_CI_trn_m2_split1, m2_split2_output$AUC_CI_trn_m2_split2,
  m2_split3_output$AUC_CI_trn_m2_split3, m2_split4_output$AUC_CI_trn_m2_split4,
  m2_split5_output$AUC_CI_trn_m2_split5, m2_split6_output$AUC_CI_trn_m2_split6,
  m2_split7_output$AUC_CI_trn_m2_split7, m2_split8_output$AUC_CI_trn_m2_split8,
  m2_split9_output$AUC_CI_trn_m2_split9, m2_split10_output$AUC_CI_trn_m2_split10,
  
  m2_split1_output$AUC_CI_tst_m2_split1, m2_split2_output$AUC_CI_tst_m2_split2,
  m2_split3_output$AUC_CI_tst_m2_split3, m2_split4_output$AUC_CI_tst_m2_split4,
  m2_split5_output$AUC_CI_tst_m2_split5, m2_split6_output$AUC_CI_tst_m2_split6,
  m2_split7_output$AUC_CI_tst_m2_split7, m2_split8_output$AUC_CI_tst_m2_split8,
  m2_split9_output$AUC_CI_tst_m2_split9, m2_split10_output$AUC_CI_tst_m2_split10)

AUC_m3 <- rbind(
  m3_split1_output$AUC_CI_trn_m3_split1, m3_split2_output$AUC_CI_trn_m3_split2,
  m3_split3_output$AUC_CI_trn_m3_split3, m3_split4_output$AUC_CI_trn_m3_split4,
  m3_split5_output$AUC_CI_trn_m3_split5, m3_split6_output$AUC_CI_trn_m3_split6,
  m3_split7_output$AUC_CI_trn_m3_split7, m3_split8_output$AUC_CI_trn_m3_split8,
  m3_split9_output$AUC_CI_trn_m3_split9, m3_split10_output$AUC_CI_trn_m3_split10,
  
  m3_split1_output$AUC_CI_tst_m3_split1, m3_split2_output$AUC_CI_tst_m3_split2,
  m3_split3_output$AUC_CI_tst_m3_split3, m3_split4_output$AUC_CI_tst_m3_split4,
  m3_split5_output$AUC_CI_tst_m3_split5, m3_split6_output$AUC_CI_tst_m3_split6,
  m3_split7_output$AUC_CI_tst_m3_split7, m3_split8_output$AUC_CI_tst_m3_split8,
  m3_split9_output$AUC_CI_tst_m3_split9, m3_split10_output$AUC_CI_tst_m3_split10)

write.csv(AUC_m1, 'output/elastic_net/AUC_m1.csv')  
write.csv(AUC_m2, 'output/elastic_net/AUC_m2.csv')  
write.csv(AUC_m3, 'output/elastic_net/AUC_m3.csv')  

# (4) sensitivity and specificity 
ss_m1 <- rbind(
  m1_split1_output$ss_m1_split1, m1_split2_output$ss_m1_split2,
  m1_split3_output$ss_m1_split3, m1_split4_output$ss_m1_split4,
  m1_split5_output$ss_m1_split5, m1_split6_output$ss_m1_split6,
  m1_split7_output$ss_m1_split7, m1_split8_output$ss_m1_split8,
  m1_split9_output$ss_m1_split9, m1_split10_output$ss_m1_split10)

ss_m2 <- rbind(
  m2_split1_output$ss_m2_split1, m2_split2_output$ss_m2_split2,
  m2_split3_output$ss_m2_split3, m2_split4_output$ss_m2_split4,
  m2_split5_output$ss_m2_split5, m2_split6_output$ss_m2_split6,
  m2_split7_output$ss_m2_split7, m2_split8_output$ss_m2_split8,
  m2_split9_output$ss_m2_split9, m2_split10_output$ss_m2_split10)

ss_m3 <- rbind(
  m3_split1_output$ss_m3_split1, m3_split2_output$ss_m3_split2,
  m3_split3_output$ss_m3_split3, m3_split4_output$ss_m3_split4,
  m3_split5_output$ss_m3_split5, m3_split6_output$ss_m3_split6,
  m3_split7_output$ss_m3_split7, m3_split8_output$ss_m3_split8,
  m3_split9_output$ss_m3_split9, m3_split10_output$ss_m3_split10)
  
write.csv(ss_m1, 'output/elastic_net/ss_m1.csv')  
write.csv(ss_m2, 'output/elastic_net/ss_m2.csv')  
write.csv(ss_m3, 'output/elastic_net/ss_m3.csv')  
  

# (5) coefficients for variables retained + n splits retained 

## model 1
m1_split1_coef <- m1_split1_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split1 = m1_cf_min,
         m1_split1_exp = m1_cf_min_exp) 

m1_split2_coef <- m1_split2_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split2 = m1_cf_min,
         m1_split2_exp = m1_cf_min_exp) 

m1_split3_coef <- m1_split3_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split3 = m1_cf_min,
         m1_split3_exp = m1_cf_min_exp) 

m1_split4_coef <- m1_split4_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split4 = m1_cf_min,
         m1_split4_exp = m1_cf_min_exp) 

m1_split5_coef <- m1_split5_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split5 = m1_cf_min,
         m1_split5_exp = m1_cf_min_exp) 

m1_split6_coef <- m1_split6_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split6 = m1_cf_min,
         m1_split6_exp = m1_cf_min_exp) 

m1_split7_coef <- m1_split7_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split7 = m1_cf_min,
         m1_split7_exp = m1_cf_min_exp) 

m1_split8_coef <- m1_split8_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split8 = m1_cf_min,
         m1_split8_exp = m1_cf_min_exp) 

m1_split9_coef <- m1_split9_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split9 = m1_cf_min,
         m1_split9_exp = m1_cf_min_exp) 

m1_split10_coef <- m1_split10_output$m1_cf %>% 
  subset(m1_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m1_cf_min, m1_cf_min_exp) %>% 
  rename(m1_split10 = m1_cf_min,
         m1_split10_exp = m1_cf_min_exp) 

## model 2
m2_split1_coef <- m2_split1_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split1 = m2_cf_min,
         m2_split1_exp = m2_cf_min_exp) 

m2_split2_coef <- m2_split2_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split2 = m2_cf_min,
         m2_split2_exp = m2_cf_min_exp) 

m2_split3_coef <- m2_split3_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split3 = m2_cf_min,
         m2_split3_exp = m2_cf_min_exp) 

m2_split4_coef <- m2_split4_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split4 = m2_cf_min,
         m2_split4_exp = m2_cf_min_exp) 

m2_split5_coef <- m2_split5_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split5 = m2_cf_min,
         m2_split5_exp = m2_cf_min_exp) 

m2_split6_coef <- m2_split6_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split6 = m2_cf_min,
         m2_split6_exp = m2_cf_min_exp) 

m2_split7_coef <- m2_split7_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split7 = m2_cf_min,
         m2_split7_exp = m2_cf_min_exp) 

m2_split8_coef <- m2_split8_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split8 = m2_cf_min,
         m2_split8_exp = m2_cf_min_exp) 

m2_split9_coef <- m2_split9_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split9 = m2_cf_min,
         m2_split9_exp = m2_cf_min_exp) 

m2_split10_coef <- m2_split10_output$m2_cf %>% 
  subset(m2_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m2_cf_min, m2_cf_min_exp) %>% 
  rename(m2_split10 = m2_cf_min,
         m2_split10_exp = m2_cf_min_exp) 

## model 3
m3_split1_coef <- m3_split1_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split1 = m3_cf_min,
         m3_split1_exp = m3_cf_min_exp) 

m3_split2_coef <- m3_split2_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split2 = m3_cf_min,
         m3_split2_exp = m3_cf_min_exp) 

m3_split3_coef <- m3_split3_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split3 = m3_cf_min,
         m3_split3_exp = m3_cf_min_exp) 

m3_split4_coef <- m3_split4_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split4 = m3_cf_min,
         m3_split4_exp = m3_cf_min_exp) 

m3_split5_coef <- m3_split5_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split5 = m3_cf_min,
         m3_split5_exp = m3_cf_min_exp) 

m3_split6_coef <- m3_split6_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split6 = m3_cf_min,
         m3_split6_exp = m3_cf_min_exp) 

m3_split7_coef <- m3_split7_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split7 = m3_cf_min,
         m3_split7_exp = m3_cf_min_exp) 

m3_split8_coef <- m3_split8_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split8 = m3_cf_min,
         m3_split8_exp = m3_cf_min_exp) 

m3_split9_coef <- m3_split9_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split9 = m3_cf_min,
         m3_split9_exp = m3_cf_min_exp) 

m3_split10_coef <- m3_split10_output$m3_cf %>% 
  subset(m3_cf_min != 0) %>% 
  select(domain_name, table_name, variable, m3_cf_min, m3_cf_min_exp) %>% 
  rename(m3_split10 = m3_cf_min,
         m3_split10_exp = m3_cf_min_exp) 

m1_coef <- m1_split1_coef %>% 
  full_join(m1_split2_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split3_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split4_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split5_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split6_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split7_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split8_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split9_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m1_split10_coef, by = c('variable', 'domain_name', 'table_name')) 

m2_coef <- m2_split1_coef %>% 
  full_join(m2_split2_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split3_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split4_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split5_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split6_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split7_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split8_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split9_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m2_split10_coef, by = c('variable', 'domain_name', 'table_name')) 

m3_coef <- m3_split1_coef %>% 
  full_join(m3_split2_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split3_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split4_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split5_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split6_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split7_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split8_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split9_coef, by = c('variable', 'domain_name', 'table_name')) %>% 
  full_join(m3_split10_coef, by = c('variable', 'domain_name', 'table_name')) 

rm(
  m1_split1_coef, m1_split2_coef, m1_split3_coef, m1_split4_coef, m1_split5_coef, 
  m1_split6_coef, m1_split7_coef, m1_split8_coef, m1_split9_coef, m1_split10_coef,
  
  m2_split1_coef, m2_split2_coef, m2_split3_coef, m2_split4_coef, m2_split5_coef, 
  m2_split6_coef, m2_split7_coef, m2_split8_coef, m2_split9_coef, m2_split10_coef,
  
  m3_split1_coef, m3_split2_coef, m3_split3_coef, m3_split4_coef, m3_split5_coef, 
  m3_split6_coef, m3_split7_coef, m3_split8_coef, m3_split9_coef, m3_split10_coef)

### average across splits (supplementary materials)

# - create dataframe with coefficients and exponentiated coefficients 
m1_coef_supp <- m1_coef %>% 
  select(
    domain_name, table_name, variable,
    ends_with('split1') | ends_with('split2') | ends_with('split3') | 
      ends_with('split4') | ends_with('split5') | ends_with('split6') |
      ends_with('split7') | ends_with('split8') | ends_with('split9') | 
      ends_with('split10'))

m1_coef_supp_exp <- m1_coef %>% 
  select(
    domain_name, table_name, variable, ends_with('exp'))

m2_coef_supp <- m2_coef %>% 
  select(
    domain_name, table_name, variable,
    ends_with('split1') | ends_with('split2') | ends_with('split3') | 
      ends_with('split4') | ends_with('split5') | ends_with('split6') |
      ends_with('split7') | ends_with('split8') | ends_with('split9') | 
      ends_with('split10'))

m2_coef_supp_exp <- m2_coef %>% 
  select(
    domain_name, table_name, variable, ends_with('exp'))

m3_coef_supp <- m3_coef %>% 
  select(
    domain_name, table_name, variable,
    ends_with('split1') | ends_with('split2') | ends_with('split3') | 
      ends_with('split4') | ends_with('split5') | ends_with('split6') |
      ends_with('split7') | ends_with('split8') | ends_with('split9') | 
      ends_with('split10'))

m3_coef_supp_exp <- m3_coef %>% 
  select(
    domain_name, table_name, variable, ends_with('exp'))

# - averages for coefficients and exponentiated coefficients
m1_coef_supp <- m1_coef_supp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m1')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 

m1_coef_supp_exp <- m1_coef_supp_exp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m1')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 

m2_coef_supp <- m2_coef_supp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m2')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 

m2_coef_supp_exp <- m2_coef_supp_exp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m2')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 

m3_coef_supp <- m3_coef_supp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m3')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 

m3_coef_supp_exp <- m3_coef_supp_exp %>% 
  slice(-1) %>% # drop intercept
  mutate(n_split = rowSums(!is.na(select(., -domain_name, -table_name, -variable)))) %>% 
  mutate(coef_avg = rowMeans(select(., starts_with('m3')), na.rm = TRUE))  %>% 
  mutate(order_variable = abs(coef_avg)) %>% # create absolute value indicator for arranging variables 
  arrange(., desc(order_variable)) %>% 
  select(-order_variable) 
  
# add min and max for coefficients 
m1_coef_supp$coef_max <- pmax(
  m1_coef_supp$m1_split1, m1_coef_supp$m1_split2, m1_coef_supp$m1_split3, m1_coef_supp$m1_split4, 
  m1_coef_supp$m1_split5, m1_coef_supp$m1_split6, m1_coef_supp$m1_split7, m1_coef_supp$m1_split8,
  m1_coef_supp$m1_split9, m1_coef_supp$m1_split10, na.rm = TRUE)
m1_coef_supp$coef_min <- pmin(
  m1_coef_supp$m1_split1, m1_coef_supp$m1_split2, m1_coef_supp$m1_split3, m1_coef_supp$m1_split4, 
  m1_coef_supp$m1_split5, m1_coef_supp$m1_split6, m1_coef_supp$m1_split7, m1_coef_supp$m1_split8,
  m1_coef_supp$m1_split9, m1_coef_supp$m1_split10, na.rm = TRUE)

m1_coef_supp_exp$coef_max <- pmax(
  m1_coef_supp$m1_split1, m1_coef_supp$m1_split2, m1_coef_supp$m1_split3, m1_coef_supp$m1_split4, 
  m1_coef_supp$m1_split5, m1_coef_supp$m1_split6, m1_coef_supp$m1_split7, m1_coef_supp$m1_split8,
  m1_coef_supp$m1_split9, m1_coef_supp$m1_split10, na.rm = TRUE)
m1_coef_supp_exp$coef_min <- pmin(
  m1_coef_supp$m1_split1, m1_coef_supp$m1_split2, m1_coef_supp$m1_split3, m1_coef_supp$m1_split4, 
  m1_coef_supp$m1_split5, m1_coef_supp$m1_split6, m1_coef_supp$m1_split7, m1_coef_supp$m1_split8,
  m1_coef_supp$m1_split9, m1_coef_supp$m1_split10, na.rm = TRUE)

m2_coef_supp$coef_max <- pmax(
  m2_coef_supp$m2_split1, m2_coef_supp$m2_split2, m2_coef_supp$m2_split3, m2_coef_supp$m2_split4, 
  m2_coef_supp$m2_split5, m2_coef_supp$m2_split6, m2_coef_supp$m2_split7, m2_coef_supp$m2_split8,
  m2_coef_supp$m2_split9, m2_coef_supp$m2_split10, na.rm = TRUE)
m2_coef_supp$coef_min <- pmin(
  m2_coef_supp$m2_split1, m2_coef_supp$m2_split2, m2_coef_supp$m2_split3, m2_coef_supp$m2_split4, 
  m2_coef_supp$m2_split5, m2_coef_supp$m2_split6, m2_coef_supp$m2_split7, m2_coef_supp$m2_split8,
  m2_coef_supp$m2_split9, m2_coef_supp$m2_split10, na.rm = TRUE)

m2_coef_supp_exp$coef_max <- pmax(
  m2_coef_supp$m2_split1, m2_coef_supp$m2_split2, m2_coef_supp$m2_split3, m2_coef_supp$m2_split4, 
  m2_coef_supp$m2_split5, m2_coef_supp$m2_split6, m2_coef_supp$m2_split7, m2_coef_supp$m2_split8,
  m2_coef_supp$m2_split9, m2_coef_supp$m2_split10, na.rm = TRUE)
m2_coef_supp_exp$coef_min <- pmin(
  m2_coef_supp$m2_split1, m2_coef_supp$m2_split2, m2_coef_supp$m2_split3, m2_coef_supp$m2_split4, 
  m2_coef_supp$m2_split5, m2_coef_supp$m2_split6, m2_coef_supp$m2_split7, m2_coef_supp$m2_split8,
  m2_coef_supp$m2_split9, m2_coef_supp$m2_split10, na.rm = TRUE)

m3_coef_supp$coef_max <- pmax(
  m3_coef_supp$m3_split1, m3_coef_supp$m3_split2, m3_coef_supp$m3_split3, m3_coef_supp$m3_split4, 
  m3_coef_supp$m3_split5, m3_coef_supp$m3_split6, m3_coef_supp$m3_split7, m3_coef_supp$m3_split8,
  m3_coef_supp$m3_split9, m3_coef_supp$m3_split10, na.rm = TRUE)
m3_coef_supp$coef_min <- pmin(
  m3_coef_supp$m3_split1, m3_coef_supp$m3_split2, m3_coef_supp$m3_split3, m3_coef_supp$m3_split4, 
  m3_coef_supp$m3_split5, m3_coef_supp$m3_split6, m3_coef_supp$m3_split7, m3_coef_supp$m3_split8,
  m3_coef_supp$m3_split9, m3_coef_supp$m3_split10, na.rm = TRUE)

m3_coef_supp_exp$coef_max <- pmax(
  m3_coef_supp$m3_split1, m3_coef_supp$m3_split2, m3_coef_supp$m3_split3, m3_coef_supp$m3_split4, 
  m3_coef_supp$m3_split5, m3_coef_supp$m3_split6, m3_coef_supp$m3_split7, m3_coef_supp$m3_split8,
  m3_coef_supp$m3_split9, m3_coef_supp$m3_split10, na.rm = TRUE)
m3_coef_supp_exp$coef_min <- pmin(
  m3_coef_supp$m3_split1, m3_coef_supp$m3_split2, m3_coef_supp$m3_split3, m3_coef_supp$m3_split4, 
  m3_coef_supp$m3_split5, m3_coef_supp$m3_split6, m3_coef_supp$m3_split7, m3_coef_supp$m3_split8,
  m3_coef_supp$m3_split9, m3_coef_supp$m3_split10, na.rm = TRUE)

# rename variables
m1_coef_supp <- m1_coef_supp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m1_split1,
    'Split 2' = m1_split2,
    'Split 3' = m1_split3,
    'Split 4' = m1_split4,
    'Split 5' = m1_split5,
    'Split 6' = m1_split6,
    'Split 7' = m1_split7,
    'Split 8' = m1_split8,
    'Split 9' = m1_split9,
    'Split 10' = m1_split10,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 

m1_coef_supp_exp <- m1_coef_supp_exp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m1_split1_exp,
    'Split 2' = m1_split2_exp,
    'Split 3' = m1_split3_exp,
    'Split 4' = m1_split4_exp,
    'Split 5' = m1_split5_exp,
    'Split 6' = m1_split6_exp,
    'Split 7' = m1_split7_exp,
    'Split 8' = m1_split8_exp,
    'Split 9' = m1_split9_exp,
    'Split 10' = m1_split10_exp,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 


m2_coef_supp <- m2_coef_supp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m2_split1,
    'Split 2' = m2_split2,
    'Split 3' = m2_split3,
    'Split 4' = m2_split4,
    'Split 5' = m2_split5,
    'Split 6' = m2_split6,
    'Split 7' = m2_split7,
    'Split 8' = m2_split8,
    'Split 9' = m2_split9,
    'Split 10' = m2_split10,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,  
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 

m2_coef_supp_exp <- m2_coef_supp_exp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m2_split1_exp,
    'Split 2' = m2_split2_exp,
    'Split 3' = m2_split3_exp,
    'Split 4' = m2_split4_exp,
    'Split 5' = m2_split5_exp,
    'Split 6' = m2_split6_exp,
    'Split 7' = m2_split7_exp,
    'Split 8' = m2_split8_exp,
    'Split 9' = m2_split9_exp,
    'Split 10' = m2_split10_exp,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,  
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 


m3_coef_supp <- m3_coef_supp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m3_split1,
    'Split 2' = m3_split2,
    'Split 3' = m3_split3,
    'Split 4' = m3_split4,
    'Split 5' = m3_split5,
    'Split 6' = m3_split6,
    'Split 7' = m3_split7,
    'Split 8' = m3_split8,
    'Split 9' = m3_split9,
    'Split 10' = m3_split10,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 

m3_coef_supp_exp <- m3_coef_supp_exp %>% 
  rename(
    'Domain' = domain_name,
    'Variable' = table_name,
    'Split 1' = m3_split1_exp,
    'Split 2' = m3_split2_exp,
    'Split 3' = m3_split3_exp,
    'Split 4' = m3_split4_exp,
    'Split 5' = m3_split5_exp,
    'Split 6' = m3_split6_exp,
    'Split 7' = m3_split7_exp,
    'Split 8' = m3_split8_exp,
    'Split 9' = m3_split9_exp,
    'Split 10' = m3_split10_exp,
    'Number of Splits' = n_split,
    'Average Across Splits' = coef_avg,
    'Minimum Across Splits' = coef_min,
    'Maximum Across Splits' = coef_max) %>% 
  mutate(across(where(is.numeric), round, 3)) 

# update MRI variables with metric

temp <- m3_coef_supp %>% 
  subset(Domain == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  select(Domain, Variable, variable)

m3_coef_supp <- m3_coef_supp %>% 
  mutate(
    Variable = ifelse(variable == 'smri_thick_cdk_cdacatelh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rrmdfrrh', 'Volume: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_pclh', 'Area: Precuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sufrrh', 'Sulcul Depth: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_mobfrlh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_paracnlh', 'Volume: Paracentral Region (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_insularh', 'Sulcul Depth: Insula (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_paracnrh', 'Sulcul Depth: Paracentral Region (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_cdmdfrlh', 'Volume: Caudal Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_trvtmrh', 'Volume: Transversetemporal (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_tmpolerh', 'Area: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mobfrlh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ehinallh', 'Sulcul Depth: Entorhinal (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdmdfrrh', 'Cortical Thickness: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_precnlh', 'Volume: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_loccrh', 'Area: Lateral Occipital Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_fusiformrh', 'Volume: Fusiform (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ifplrh', 'Volume: Inferior Parietal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rracaterh', 'Volume: Rostral Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ptcaterh', 'Cortical Thickness: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_trvtmlh', 'Volume: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_linguallh', 'Sulcul Depth: Lingual Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_tmpolerh', 'Cortical Thickness: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_frpolelh', 'Sulcul Depth: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rracaterh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ifpllh', 'Area: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_precnrh', 'Volume: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ifpllh', 'Volume: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ihcaterh', 'Volume: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_cdmdfrrh', 'Area: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ehinalrh', 'Sulcul Depth: Entorhinal (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_fusiformlh', 'Cortical Thickness: Fusiform (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ptcaterh', 'Area: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_sufrrh', 'Volume: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_paracnlh', 'Sulcul Depth: Paracentral Region (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cuneuslh', 'Sulcul Depth: Cuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parahpalrh', 'Sulcul Depth: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdmdfrlh', 'Cortical Thickness: Caudal Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parstgrisrh', 'Sulcul Depth: Pars Triangularis (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsopclh', 'Area: Pars Opercularis (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_frpolerh', 'Volume: Frontal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_rrmdfrrh', 'Area: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsobisrh', 'Area: Pars Orbitalis (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_tmpolerh', 'Sulcul Depth: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_iftmrh', 'Volume: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cuneuslh', 'Cortical Thickness: Cuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_postcnrh', 'Cortical Thickness: Postcentral Gyrus (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_precnrh', 'Area: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_iftmrh', 'Cortical Thickness: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_iftmrh', 'Area: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_rrmdfrlh', 'Area: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parsobislh', 'Sulcul Depth: Pars Orbitalis (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_frpolelh', 'Cortical Thickness: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_cdacatelh', 'Volume: Caudal Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_iftmlh', 'Cortical Thickness: Inferior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rrmdfrlh', 'Volume: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_sutmlh', 'Area: Superior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sutmrh', 'Sulcul Depth: Superior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ehinalrh', 'Cortical Thickness: Entorhinal (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_mobfrrh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_insularh', 'Cortical Thickness: Insula (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_locclh', 'Area: Lateral Occipital Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rrmdfrrh', 'Cortical Thickness: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_loccrh', 'Sulcul Depth: Lateral Occipital Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ehinallh', 'Cortical Thickness: Entorhinal (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rracatelh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_parahpalrh', 'Cortical Thickness: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_precnrh', 'Sulcul Depth: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_iftmrh', 'Sulcul Depth: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_sufrrh', 'Area: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cdmdfrrh', 'Sulcul Depth: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_banksstslh', 'Sulcul Depth: Banks of Superior Temporal Sulcus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ifplrh', 'Area: Inferior Parietal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_frpolerh', 'Cortical Thickness: Frontal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sutmlh', 'Sulcul Depth: Superior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_smrh', 'Sulcul Depth: Supramarginal (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_lingualrh', 'Volume: Lingual Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_lingualrh', 'Sulcul Depth: Lingual Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mobfrrh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_tmpolelh', 'Sulcul Depth: Temporal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ifpllh', 'Sulcul Depth: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_periccrh', 'Cortical Thickness: Pericalcarine (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ihcaterh', 'Cortical Thickness: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_pcrh', 'Sulcul Depth: Precuneus (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cdacaterh', 'Sulcul Depth: Caudal Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cuneusrh', 'Sulcul Depth: Cuneus (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_insulalh', 'Cortical Thickness: Insula (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ihcaterh', 'Sulcul Depth: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsopclh', 'Volume: Pars Opercularis (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ptcatelh', 'Sulcul Depth: Posterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_trvtmlh', 'Sulcul Depth: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsobisrh', 'Volume: Pars Orbitalis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ptcaterh', 'Volume: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsopcrh', 'Area: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_rrmdfrrh', 'Sulcul Depth: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parahpallh', 'Volume: Parahippocampal Region (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_parahpallh', 'Cortical Thickness: Parahippocampal Region (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_linguallh', 'Cortical Thickness: Lingual Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_pclh', 'Sulcul Depth: Precuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_lobfrlh', 'Sulcul Depth: Lateral Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_periccrh', 'Volume: Pericalcarine (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdacaterh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsopcrh', 'Volume: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parahpalrh', 'Volume: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ihcaterh', 'Area: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parsopcrh', 'Sulcul Depth: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_frpolelh', 'Volume: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_postcnlh', 'Sulcul Depth: Postcentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_trvtmlh', 'Area: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_rrmdfrlh', 'Sulcul Depth: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mdtmrh', 'Sulcul Depth: Middle Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_fusiformrh', 'Area: Fusiform (R)', Variable))

m3_coef_supp_exp <- m3_coef_supp_exp %>% 
  mutate(
    Variable = ifelse(variable == 'smri_thick_cdk_cdacatelh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rrmdfrrh', 'Volume: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_pclh', 'Area: Precuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sufrrh', 'Sulcul Depth: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_mobfrlh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_paracnlh', 'Volume: Paracentral Region (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_insularh', 'Sulcul Depth: Insula (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_paracnrh', 'Sulcul Depth: Paracentral Region (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_cdmdfrlh', 'Volume: Caudal Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_trvtmrh', 'Volume: Transversetemporal (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_tmpolerh', 'Area: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mobfrlh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ehinallh', 'Sulcul Depth: Entorhinal (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdmdfrrh', 'Cortical Thickness: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_precnlh', 'Volume: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_loccrh', 'Area: Lateral Occipital Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_fusiformrh', 'Volume: Fusiform (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ifplrh', 'Volume: Inferior Parietal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rracaterh', 'Volume: Rostral Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ptcaterh', 'Cortical Thickness: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_trvtmlh', 'Volume: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_linguallh', 'Sulcul Depth: Lingual Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_tmpolerh', 'Cortical Thickness: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_frpolelh', 'Sulcul Depth: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rracaterh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ifpllh', 'Area: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_precnrh', 'Volume: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ifpllh', 'Volume: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ihcaterh', 'Volume: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_cdmdfrrh', 'Area: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ehinalrh', 'Sulcul Depth: Entorhinal (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_fusiformlh', 'Cortical Thickness: Fusiform (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ptcaterh', 'Area: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_sufrrh', 'Volume: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_paracnlh', 'Sulcul Depth: Paracentral Region (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cuneuslh', 'Sulcul Depth: Cuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parahpalrh', 'Sulcul Depth: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdmdfrlh', 'Cortical Thickness: Caudal Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parstgrisrh', 'Sulcul Depth: Pars Triangularis (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsopclh', 'Area: Pars Opercularis (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_frpolerh', 'Volume: Frontal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_rrmdfrrh', 'Area: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsobisrh', 'Area: Pars Orbitalis (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_tmpolerh', 'Sulcul Depth: Temporal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_iftmrh', 'Volume: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cuneuslh', 'Cortical Thickness: Cuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_postcnrh', 'Cortical Thickness: Postcentral Gyrus (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_precnrh', 'Area: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_iftmrh', 'Cortical Thickness: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_iftmrh', 'Area: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_rrmdfrlh', 'Area: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parsobislh', 'Sulcul Depth: Pars Orbitalis (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_frpolelh', 'Cortical Thickness: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_cdacatelh', 'Volume: Caudal Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_iftmlh', 'Cortical Thickness: Inferior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_rrmdfrlh', 'Volume: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_sutmlh', 'Area: Superior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sutmrh', 'Sulcul Depth: Superior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ehinalrh', 'Cortical Thickness: Entorhinal (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_mobfrrh', 'Cortical Thickness: Medial Orbito-Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_insularh', 'Cortical Thickness: Insula (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_locclh', 'Area: Lateral Occipital Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rrmdfrrh', 'Cortical Thickness: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_loccrh', 'Sulcul Depth: Lateral Occipital Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ehinallh', 'Cortical Thickness: Entorhinal (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_rracatelh', 'Cortical Thickness: Rostral Anterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_parahpalrh', 'Cortical Thickness: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_precnrh', 'Sulcul Depth: Precentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_iftmrh', 'Sulcul Depth: Inferior Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_sufrrh', 'Area: Superior Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cdmdfrrh', 'Sulcul Depth: Caudal Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_banksstslh', 'Sulcul Depth: Banks of Superior Temporal Sulcus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ifplrh', 'Area: Inferior Parietal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_frpolerh', 'Cortical Thickness: Frontal Pole (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_sutmlh', 'Sulcul Depth: Superior Temporal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_smrh', 'Sulcul Depth: Supramarginal (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_lingualrh', 'Volume: Lingual Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_lingualrh', 'Sulcul Depth: Lingual Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mobfrrh', 'Sulcul Depth: Medial Orbito-Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_tmpolelh', 'Sulcul Depth: Temporal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ifpllh', 'Sulcul Depth: Inferior Parietal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_periccrh', 'Cortical Thickness: Pericalcarine (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_ihcaterh', 'Cortical Thickness: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_pcrh', 'Sulcul Depth: Precuneus (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cdacaterh', 'Sulcul Depth: Caudal Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_cuneusrh', 'Sulcul Depth: Cuneus (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_insulalh', 'Cortical Thickness: Insula (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ihcaterh', 'Sulcul Depth: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsopclh', 'Volume: Pars Opercularis (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_ptcatelh', 'Sulcul Depth: Posterior Cingulate Cortex (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_trvtmlh', 'Sulcul Depth: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsobisrh', 'Volume: Pars Orbitalis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_ptcaterh', 'Volume: Posterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_parsopcrh', 'Area: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_rrmdfrrh', 'Sulcul Depth: Rostral Middle Frontal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parahpallh', 'Volume: Parahippocampal Region (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_parahpallh', 'Cortical Thickness: Parahippocampal Region (L)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_linguallh', 'Cortical Thickness: Lingual Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_pclh', 'Sulcul Depth: Precuneus (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_lobfrlh', 'Sulcul Depth: Lateral Orbito-Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_periccrh', 'Volume: Pericalcarine (R)', Variable),
    Variable = ifelse(variable == 'smri_thick_cdk_cdacaterh', 'Cortical Thickness: Caudal Anterior Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parsopcrh', 'Volume: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_parahpalrh', 'Volume: Parahippocampal Region (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_ihcaterh', 'Area: Isthmus Cingulate Cortex (R)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_parsopcrh', 'Sulcul Depth: Pars Opercularis (R)', Variable),
    Variable = ifelse(variable == 'smri_vol_cdk_frpolelh', 'Volume: Frontal Pole (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_postcnlh', 'Sulcul Depth: Postcentral Gyrus (L)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_trvtmlh', 'Area: Transversetemporal (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_rrmdfrlh', 'Sulcul Depth: Rostral Middle Frontal Lobe (L)', Variable),
    Variable = ifelse(variable == 'smri_sulc_cdk_mdtmrh', 'Sulcul Depth: Middle Temporal Lobe (R)', Variable),
    Variable = ifelse(variable == 'smri_area_cdk_fusiformrh', 'Area: Fusiform (R)', Variable))

rm(temp)

write.csv(m1_coef_supp, 'output/elastic_net/m1_coef_supp.csv') # n = 85 variables
write.csv(m2_coef_supp, 'output/elastic_net/m2_coef_supp.csv') # n = 100 variables
write.csv(m3_coef_supp, 'output/elastic_net/m3_coef_supp.csv') # n = 203 variables

write.csv(m1_coef_supp_exp, 'output/elastic_net/m1_coef_supp_exp.csv') # n = 85 variables
write.csv(m2_coef_supp_exp, 'output/elastic_net/m2_coef_supp_exp.csv') # n = 100 variables
write.csv(m3_coef_supp_exp, 'output/elastic_net/m3_coef_supp_exp.csv') # n = 203 variables

# indicator of number of variables retained within each domain
m1_coef_domain_n <- table(m1_coef_supp$Domain)
m2_coef_domain_n <- table(m2_coef_supp$Domain)
m3_coef_domain_n <- table(m3_coef_supp$Domain)

capture.output(m1_coef_domain_n, 
               file = 'output/elastic_net/m1_coef_domain_n.txt')
capture.output(m2_coef_domain_n, 
               file = 'output/elastic_net/m2_coef_domain_n.txt')
capture.output(m3_coef_domain_n, 
               file = 'output/elastic_net/m3_coef_domain_n.txt')

### average across splits for variables retained in >5 splits  
m1_coef_top5 <- m1_coef_supp %>% 
  filter(`Number of Splits` > 5) 

m2_coef_top5 <- m2_coef_supp %>% 
  filter(`Number of Splits` > 5) 

m3_coef_top5 <- m3_coef_supp %>% 
  filter(`Number of Splits` > 5) 

m1_coef_domain_n_top5 <- table(m1_coef_top5$Domain)
m2_coef_domain_n_top5 <- table(m2_coef_top5$Domain)
m3_coef_domain_n_top5 <- table(m3_coef_top5$Domain)

write.csv(m1_coef_top5, 'output/elastic_net/top_5/m1_coef_top5.csv') # n = 37 variables
write.csv(m2_coef_top5, 'output/elastic_net/top_5/m2_coef_top5.csv') # n = 45 variables
write.csv(m3_coef_top5, 'output/elastic_net/top_5/m3_coef_top5.csv') # n = 49 variables

capture.output(m1_coef_domain_n_top5, 
               file = 'output/elastic_net/top_5/m1_coef_domain_n_top5.txt')
capture.output(m2_coef_domain_n_top5, 
               file = 'output/elastic_net/top_5/m2_coef_domain_n_top5.txt')
capture.output(m3_coef_domain_n_top5, 
               file = 'output/elastic_net/top_5/m3_coef_domain_n_top5.txt')

# Figures -----------------------------------------------------------------

#--- Horizontal Boxplot ---#

# update names for plot
View(m3_coef_top5)
m3_coef_top5 <- m3_coef_top5 %>% 
  mutate(
    Variable = ifelse(Variable == 'Superior Corticostriate (R)', 
                      'FA: Superior Corticostriate (R)', Variable)) 

Fig_3 <- ggplot(m3_coef_top5, 
                aes(fill = Domain, y = `Average Across Splits`, 
                    x = fct_reorder(Variable, Domain))) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(y = 'Coefficient', x = 'Predictor') +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))
Fig_3

#--- Forest Plot ---#
# (1) estimate SD for each coefficient

df_forest <- m3_coef_supp %>% 
  mutate(n = 10) %>% 
  rename(est_avg = `Average Across Splits`)

temp <- df_forest %>% 
  select(starts_with('Split'))
temp$est_sd <- apply(temp, 1, sd, na.rm=TRUE) 

temp <- temp %>% 
  mutate(est_sd = ifelse(is.na(est_sd), 0, est_sd)) # if variable retained in only 1 split, then SD = 0

temp <- temp %>% 
  select(est_sd)

df_forest <- cbind(df_forest, temp)

# (2) get CI

#calculate margin of error
margin <- qt(0.975, df = df_forest$n - 1) * df_forest$est_sd/sqrt(df_forest$n)

#calculate lower and upper bounds of confidence interval
df_forest$CI_lower <- (df_forest$est_avg - margin)
df_forest$CI_lower

df_forest$CI_upper <- df_forest$est_avg + margin
df_forest$CI_upper

# (3) create forest plots
# - subset smaller datasets by domain

table(df_forest$Domain)

df_forest_dem <- df_forest %>% 
  subset(Domain == 'Demographics') %>% 
  mutate(index = rep(1:nrow(.))) # add variable count

df_forest_su <- df_forest %>% 
  subset(Domain == 'Self and Peer Involvement with Substance Use') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_pb <- df_forest %>% 
  subset(Domain == 'Parenting Behaviors') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_mh <- df_forest %>% 
  subset(Domain == 'Mental Health') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_ph <- df_forest %>% 
  subset(Domain == 'Physical Health') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_ce <- df_forest %>% 
  subset(Domain == 'Culture & Environment') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_hor <- df_forest %>% 
  subset(Domain == 'Hormones') %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_nc <- df_forest %>% 
  subset(Domain == 'Neurocognitive Factors') %>% 
  mutate(index = rep(1:nrow(.)))

# sMRI: by metric
df_forest_sMRI_area <- df_forest %>% 
  filter(str_detect(variable, 'smri_area')) %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_sMRI_vol <- df_forest %>% 
  filter(str_detect(variable, 'smri_vol')) %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_sMRI_thick <- df_forest %>% 
  filter(str_detect(variable, 'smri_thick')) %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_sMRI_sulc <- df_forest %>% 
  filter(str_detect(variable, 'smri_sulc')) %>% 
  mutate(index = rep(1:nrow(.)))

df_forest_DTI <- df_forest %>% 
  subset(Domain == 'Diffusion Tensor Imaging (DTI)') %>% 
  mutate(index = rep(1:nrow(.)))

# plot: demos
ggplot(data = df_forest_dem, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_dem), labels=df_forest_dem$Variable) +
  labs(title ='Domain: Demographics', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: substance use
ggplot(data = df_forest_su, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_su), labels=df_forest_su$Variable) +
  labs(title ='Domain: Self and Peer Involvement with Substance Use', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: parenting
ggplot(data = df_forest_pb, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_pb), labels=df_forest_pb$Variable) +
  labs(title ='Domain: Parenting Behaviors', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: mental health
ggplot(data = df_forest_mh, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_mh), labels=df_forest_mh$Variable) +
  labs(title ='Domain: Mental Health', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: physical health
ggplot(data = df_forest_ph, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_ph), labels=df_forest_ph$Variable) +
  labs(title ='Domain: Physical Health', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: culture & environment
ggplot(data = df_forest_ce, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_ce), labels=df_forest_ce$Variable) +
  labs(title ='Domain: Culture & Environment', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: hormones
ggplot(data = df_forest_hor, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_hor), labels=df_forest_hor$Variable) +
  labs(title ='Domain: Hormones', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: neurocognitive factors
ggplot(data = df_forest_nc, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_nc), labels=df_forest_nc$Variable) +
  labs(title ='Domain: Neurocognitive Factors', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: imaging - sMRI Area
ggplot(data = df_forest_sMRI_area, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_sMRI_area), labels=df_forest_sMRI_area$Variable) +
  labs(title ='Domain: sMRI - Area', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: imaging - sMRI Volume
ggplot(data = df_forest_sMRI_vol, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_sMRI_vol), labels=df_forest_sMRI_vol$Variable) +
  labs(title ='Domain: sMRI - Volume', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: imaging - sMRI Sulcul Depth
ggplot(data = df_forest_sMRI_sulc, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_sMRI_sulc), labels=df_forest_sMRI_sulc$Variable) +
  labs(title ='Domain: sMRI - Sulcul Depth', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: imaging - sMRI Cortical Thickness
ggplot(data = df_forest_sMRI_thick, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_sMRI_thick), labels=df_forest_sMRI_thick$Variable) +
  labs(title ='Domain: sMRI - Cortical Thickness', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

# plot: imaging (DTI)
ggplot(data = df_forest_DTI, aes(y = index, x = est_avg, xmin = CI_lower, xmax = CI_upper)) +
  geom_point() + 
  geom_errorbarh(height = .1) +
  scale_y_continuous(breaks=1:nrow(df_forest_DTI), labels=df_forest_DTI$Variable) +
  labs(title ='Domain: DTI', x = 'Coefficient Estimates', y = 'Variable') +
  geom_vline(xintercept = 0, color = 'black', linetype = 'dashed', alpha=.5) +
  theme_classic() +
  theme(
    axis.title.x = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.title.y = element_text(
      color="black", size = 12, face = 'bold', family = 'Times'),
    axis.text = element_text(
      color="black", size=10, family = 'Times'),
    legend.text = element_text(
      color="black", size = 10, family = 'Times'),
    legend.title = element_text(
      color="black", size = 12, family = 'Times'))

rm(
  m1_coef, m2_coef, m3_coef,
  m1_coef_supp, m2_coef_supp, m3_coef_supp,
  m1_coef_domain_n, m2_coef_domain_n, m3_coef_domain_n,
  m1_coef_top5, m2_coef_top5, m3_coef_top5,
  m1_coef_domain_n_top5, m2_coef_domain_n_top5, m3_coef_domain_n_top5)
