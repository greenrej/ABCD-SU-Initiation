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

# Split Dataset -----------------------------------------------------------

# n = 10 splits for repeated k-fold cross-validation
# seeds for each split
# - split 1: 11897
# - split 2: 95039
# - split 3: 48378
# - split 4: 21773
# - split 5: 48339
# - split 6: 88593
# - split 7: 87388
# - split 8: 12993
# - split 9: 52701
# - split 10: 34885

### split (1)
job::job({
  # create split
  set.seed(11897)
  split_1 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_1 <- rsample::training(split_1)
  tst_1 <- rsample::testing(split_1)
  rm(split_1)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training that are in test and vice versa
  trn_tst_check <- trn_1$src_subject_id %in% tst_1$src_subject_id
  tst_trn_check <- tst_1$src_subject_id %in% trn_1$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_1 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_1 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  
  if (nrow(factor_sum(trn_1$race_4l)) == 4 & 
      nrow(factor_sum(tst_1$race_4l)) == 4 &
      nrow(factor_sum(trn_1$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_1$eth_hisp)) == 2 &
      nrow(factor_sum(trn_1$income)) == 10 & 
      nrow(factor_sum(tst_1$income)) == 10 &
      nrow(factor_sum(trn_1$religion)) == 17 & 
      nrow(factor_sum(tst_1$religion)) == 17 &
      nrow(factor_sum(trn_1$p_edu)) == 6 & 
      nrow(factor_sum(tst_1$p_edu)) == 6 &
      nrow(factor_sum(trn_1$sex_2l)) == 2 & 
      nrow(factor_sum(tst_1$sex_2l)) == 2 &
      nrow(factor_sum(trn_1$rec_bin)) == 2 & 
      nrow(factor_sum(tst_1$rec_bin)) == 2 &
      nrow(factor_sum(trn_1$exp_sub)) == 2 & 
      nrow(factor_sum(tst_1$exp_sub)) == 2 &
      nrow(factor_sum(trn_1$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_1$tbi_injury)) == 2 &
      nrow(factor_sum(trn_1$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_1$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_1$det_susp)) == 2 & 
      nrow(factor_sum(tst_1$det_susp)) == 2 &
      nrow(factor_sum(trn_1$se_services)) == 5 & 
      nrow(factor_sum(tst_1$se_services)) == 5 &
      nrow(factor_sum(trn_1$cct)) == 2 & 
      nrow(factor_sum(tst_1$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (2)
job::job({
  # create split
  set.seed(95039)
  split_2 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_2 <- rsample::training(split_2)
  tst_2 <- rsample::testing(split_2)
  rm(split_2)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_2$src_subject_id %in% tst_2$src_subject_id
  tst_trn_check <- tst_2$src_subject_id %in% trn_2$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_2 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_2 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  
  if (nrow(factor_sum(trn_2$race_4l)) == 4 & 
      nrow(factor_sum(tst_2$race_4l)) == 4 &
      nrow(factor_sum(trn_2$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_2$eth_hisp)) == 2 &
      nrow(factor_sum(trn_2$income)) == 10 & 
      nrow(factor_sum(tst_2$income)) == 10 &
      nrow(factor_sum(trn_2$religion)) == 17 & 
      nrow(factor_sum(tst_2$religion)) == 17 &
      nrow(factor_sum(trn_2$p_edu)) == 6 & 
      nrow(factor_sum(tst_2$p_edu)) == 6 &
      nrow(factor_sum(trn_2$sex_2l)) == 2 & 
      nrow(factor_sum(tst_2$sex_2l)) == 2 &
      nrow(factor_sum(trn_2$rec_bin)) == 2 & 
      nrow(factor_sum(tst_2$rec_bin)) == 2 &
      nrow(factor_sum(trn_2$exp_sub)) == 2 & 
      nrow(factor_sum(tst_2$exp_sub)) == 2 &
      nrow(factor_sum(trn_2$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_2$tbi_injury)) == 2 &
      nrow(factor_sum(trn_2$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_2$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_2$det_susp)) == 2 & 
      nrow(factor_sum(tst_2$det_susp)) == 2 &
      nrow(factor_sum(trn_2$se_services)) == 5 & 
      nrow(factor_sum(tst_2$se_services)) == 5 &
      nrow(factor_sum(trn_2$cct)) == 2 & 
      nrow(factor_sum(tst_2$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (3)
job::job({
  # create split
  set.seed(48378)
  split_3 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_3 <- rsample::training(split_3)
  tst_3 <- rsample::testing(split_3)
  rm(split_3)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_3$src_subject_id %in% tst_3$src_subject_id
  tst_trn_check <- tst_3$src_subject_id %in% trn_3$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_3 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_3 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_3$race_4l)) == 4 & 
      nrow(factor_sum(tst_3$race_4l)) == 4 &
      nrow(factor_sum(trn_3$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_3$eth_hisp)) == 2 &
      nrow(factor_sum(trn_3$income)) == 10 & 
      nrow(factor_sum(tst_3$income)) == 10 &
      nrow(factor_sum(trn_3$religion)) == 17 & 
      nrow(factor_sum(tst_3$religion)) == 17 &
      nrow(factor_sum(trn_3$p_edu)) == 6 & 
      nrow(factor_sum(tst_3$p_edu)) == 6 &
      nrow(factor_sum(trn_3$sex_2l)) == 2 & 
      nrow(factor_sum(tst_3$sex_2l)) == 2 &
      nrow(factor_sum(trn_3$rec_bin)) == 2 & 
      nrow(factor_sum(tst_3$rec_bin)) == 2 &
      nrow(factor_sum(trn_3$exp_sub)) == 2 & 
      nrow(factor_sum(tst_3$exp_sub)) == 2 &
      nrow(factor_sum(trn_3$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_3$tbi_injury)) == 2 &
      nrow(factor_sum(trn_3$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_3$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_3$det_susp)) == 2 & 
      nrow(factor_sum(tst_3$det_susp)) == 2 &
      nrow(factor_sum(trn_3$se_services)) == 5 & 
      nrow(factor_sum(tst_3$se_services)) == 5 &
      nrow(factor_sum(trn_3$cct)) == 2 & 
      nrow(factor_sum(tst_3$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (4)
job::job({
  # create split
  set.seed(21773)
  split_4 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_4 <- rsample::training(split_4)
  tst_4 <- rsample::testing(split_4)
  rm(split_4)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_4$src_subject_id %in% tst_4$src_subject_id
  tst_trn_check <- tst_4$src_subject_id %in% trn_4$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_4 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_4 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_4$race_4l)) == 4 & 
      nrow(factor_sum(tst_4$race_4l)) == 4 &
      nrow(factor_sum(trn_4$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_4$eth_hisp)) == 2 &
      nrow(factor_sum(trn_4$income)) == 10 & 
      nrow(factor_sum(tst_4$income)) == 10 &
      nrow(factor_sum(trn_4$religion)) == 17 & 
      nrow(factor_sum(tst_4$religion)) == 17 &
      nrow(factor_sum(trn_4$p_edu)) == 6 & 
      nrow(factor_sum(tst_4$p_edu)) == 6 &
      nrow(factor_sum(trn_4$sex_2l)) == 2 & 
      nrow(factor_sum(tst_4$sex_2l)) == 2 &
      nrow(factor_sum(trn_4$rec_bin)) == 2 & 
      nrow(factor_sum(tst_4$rec_bin)) == 2 &
      nrow(factor_sum(trn_4$exp_sub)) == 2 & 
      nrow(factor_sum(tst_4$exp_sub)) == 2 &
      nrow(factor_sum(trn_4$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_4$tbi_injury)) == 2 &
      nrow(factor_sum(trn_4$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_4$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_4$det_susp)) == 2 & 
      nrow(factor_sum(tst_4$det_susp)) == 2 &
      nrow(factor_sum(trn_4$se_services)) == 5 & 
      nrow(factor_sum(tst_4$se_services)) == 5 &
      nrow(factor_sum(trn_4$cct)) == 2 & 
      nrow(factor_sum(tst_4$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (5)
job::job({
  # create split
  set.seed(48339)
  split_5 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_5 <- rsample::training(split_5)
  tst_5 <- rsample::testing(split_5)
  rm(split_5)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_5$src_subject_id %in% tst_5$src_subject_id
  tst_trn_check <- tst_5$src_subject_id %in% trn_5$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_5 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_5 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_5$race_4l)) == 4 & 
      nrow(factor_sum(tst_5$race_4l)) == 4 &
      nrow(factor_sum(trn_5$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_5$eth_hisp)) == 2 &
      nrow(factor_sum(trn_5$income)) == 10 & 
      nrow(factor_sum(tst_5$income)) == 10 &
      nrow(factor_sum(trn_5$religion)) == 17 & 
      nrow(factor_sum(tst_5$religion)) == 17 &
      nrow(factor_sum(trn_5$p_edu)) == 6 & 
      nrow(factor_sum(tst_5$p_edu)) == 6 &
      nrow(factor_sum(trn_5$sex_2l)) == 2 & 
      nrow(factor_sum(tst_5$sex_2l)) == 2 &
      nrow(factor_sum(trn_5$rec_bin)) == 2 & 
      nrow(factor_sum(tst_5$rec_bin)) == 2 &
      nrow(factor_sum(trn_5$exp_sub)) == 2 & 
      nrow(factor_sum(tst_5$exp_sub)) == 2 &
      nrow(factor_sum(trn_5$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_5$tbi_injury)) == 2 &
      nrow(factor_sum(trn_5$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_5$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_5$det_susp)) == 2 & 
      nrow(factor_sum(tst_5$det_susp)) == 2 &
      nrow(factor_sum(trn_5$se_services)) == 5 & 
      nrow(factor_sum(tst_5$se_services)) == 5 &
      nrow(factor_sum(trn_5$cct)) == 2 & 
      nrow(factor_sum(tst_5$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (6)
job::job({
  # create split
  set.seed(88593)
  split_6 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_6 <- rsample::training(split_6)
  tst_6 <- rsample::testing(split_6)
  rm(split_6)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_6$src_subject_id %in% tst_6$src_subject_id
  tst_trn_check <- tst_6$src_subject_id %in% trn_6$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_6 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_6 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_6$race_4l)) == 4 & 
      nrow(factor_sum(tst_6$race_4l)) == 4 &
      nrow(factor_sum(trn_6$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_6$eth_hisp)) == 2 &
      nrow(factor_sum(trn_6$income)) == 10 & 
      nrow(factor_sum(tst_6$income)) == 10 &
      nrow(factor_sum(trn_6$religion)) == 17 & 
      nrow(factor_sum(tst_6$religion)) == 17 &
      nrow(factor_sum(trn_6$p_edu)) == 6 & 
      nrow(factor_sum(tst_6$p_edu)) == 6 &
      nrow(factor_sum(trn_6$sex_2l)) == 2 & 
      nrow(factor_sum(tst_6$sex_2l)) == 2 &
      nrow(factor_sum(trn_6$rec_bin)) == 2 & 
      nrow(factor_sum(tst_6$rec_bin)) == 2 &
      nrow(factor_sum(trn_6$exp_sub)) == 2 & 
      nrow(factor_sum(tst_6$exp_sub)) == 2 &
      nrow(factor_sum(trn_6$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_6$tbi_injury)) == 2 &
      nrow(factor_sum(trn_6$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_6$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_6$det_susp)) == 2 & 
      nrow(factor_sum(tst_6$det_susp)) == 2 &
      nrow(factor_sum(trn_6$se_services)) == 5 & 
      nrow(factor_sum(tst_6$se_services)) == 5 &
      nrow(factor_sum(trn_6$cct)) == 2 & 
      nrow(factor_sum(tst_6$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (7)
job::job({
  # create split
  set.seed(87388)
  split_7 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_7 <- rsample::training(split_7)
  tst_7 <- rsample::testing(split_7)
  rm(split_7)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_7$src_subject_id %in% tst_7$src_subject_id
  tst_trn_check <- tst_7$src_subject_id %in% trn_7$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_7 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_7 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_7$race_4l)) == 4 & 
      nrow(factor_sum(tst_7$race_4l)) == 4 &
      nrow(factor_sum(trn_7$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_7$eth_hisp)) == 2 &
      nrow(factor_sum(trn_7$income)) == 10 & 
      nrow(factor_sum(tst_7$income)) == 10 &
      nrow(factor_sum(trn_7$religion)) == 17 & 
      nrow(factor_sum(tst_7$religion)) == 17 &
      nrow(factor_sum(trn_7$p_edu)) == 6 & 
      nrow(factor_sum(tst_7$p_edu)) == 6 &
      nrow(factor_sum(trn_7$sex_2l)) == 2 & 
      nrow(factor_sum(tst_7$sex_2l)) == 2 &
      nrow(factor_sum(trn_7$rec_bin)) == 2 & 
      nrow(factor_sum(tst_7$rec_bin)) == 2 &
      nrow(factor_sum(trn_7$exp_sub)) == 2 & 
      nrow(factor_sum(tst_7$exp_sub)) == 2 &
      nrow(factor_sum(trn_7$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_7$tbi_injury)) == 2 &
      nrow(factor_sum(trn_7$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_7$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_7$det_susp)) == 2 & 
      nrow(factor_sum(tst_7$det_susp)) == 2 &
      nrow(factor_sum(trn_7$se_services)) == 5 & 
      nrow(factor_sum(tst_7$se_services)) == 5 &
      nrow(factor_sum(trn_7$cct)) == 2 & 
      nrow(factor_sum(tst_7$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (8)
job::job({
  # create split
  set.seed(12993)
  split_8 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_8 <- rsample::training(split_8)
  tst_8 <- rsample::testing(split_8)
  rm(split_8)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_8$src_subject_id %in% tst_8$src_subject_id
  tst_trn_check <- tst_8$src_subject_id %in% trn_8$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_8 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_8 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_8$race_4l)) == 4 & 
      nrow(factor_sum(tst_8$race_4l)) == 4 &
      nrow(factor_sum(trn_8$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_8$eth_hisp)) == 2 &
      nrow(factor_sum(trn_8$income)) == 10 & 
      nrow(factor_sum(tst_8$income)) == 10 &
      nrow(factor_sum(trn_8$religion)) == 17 & 
      nrow(factor_sum(tst_8$religion)) == 17 &
      nrow(factor_sum(trn_8$p_edu)) == 6 & 
      nrow(factor_sum(tst_8$p_edu)) == 6 &
      nrow(factor_sum(trn_8$sex_2l)) == 2 & 
      nrow(factor_sum(tst_8$sex_2l)) == 2 &
      nrow(factor_sum(trn_8$rec_bin)) == 2 & 
      nrow(factor_sum(tst_8$rec_bin)) == 2 &
      nrow(factor_sum(trn_8$exp_sub)) == 2 & 
      nrow(factor_sum(tst_8$exp_sub)) == 2 &
      nrow(factor_sum(trn_8$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_8$tbi_injury)) == 2 &
      nrow(factor_sum(trn_8$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_8$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_8$det_susp)) == 2 & 
      nrow(factor_sum(tst_8$det_susp)) == 2 &
      nrow(factor_sum(trn_8$se_services)) == 5 & 
      nrow(factor_sum(tst_8$se_services)) == 5 &
      nrow(factor_sum(trn_8$cct)) == 2 & 
      nrow(factor_sum(tst_8$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (9)
job::job({
  # create split
  set.seed(52701)
  split_9 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_9 <- rsample::training(split_9)
  tst_9 <- rsample::testing(split_9)
  rm(split_9)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_9$src_subject_id %in% tst_9$src_subject_id
  tst_trn_check <- tst_9$src_subject_id %in% trn_9$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_9 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_9 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_9$race_4l)) == 4 & 
      nrow(factor_sum(tst_9$race_4l)) == 4 &
      nrow(factor_sum(trn_9$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_9$eth_hisp)) == 2 &
      nrow(factor_sum(trn_9$income)) == 10 & 
      nrow(factor_sum(tst_9$income)) == 10 &
      nrow(factor_sum(trn_9$religion)) == 17 & 
      nrow(factor_sum(tst_9$religion)) == 17 &
      nrow(factor_sum(trn_9$p_edu)) == 6 & 
      nrow(factor_sum(tst_9$p_edu)) == 6 &
      nrow(factor_sum(trn_9$sex_2l)) == 2 & 
      nrow(factor_sum(tst_9$sex_2l)) == 2 &
      nrow(factor_sum(trn_9$rec_bin)) == 2 & 
      nrow(factor_sum(tst_9$rec_bin)) == 2 &
      nrow(factor_sum(trn_9$exp_sub)) == 2 & 
      nrow(factor_sum(tst_9$exp_sub)) == 2 &
      nrow(factor_sum(trn_9$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_9$tbi_injury)) == 2 &
      nrow(factor_sum(trn_9$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_9$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_9$det_susp)) == 2 & 
      nrow(factor_sum(tst_9$det_susp)) == 2 &
      nrow(factor_sum(trn_9$se_services)) == 5 & 
      nrow(factor_sum(tst_9$se_services)) == 5 &
      nrow(factor_sum(trn_9$cct)) == 2 & 
      nrow(factor_sum(tst_9$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

### split (10)
job::job({
  # create split
  set.seed(34885)
  split_10 <- initial_split(data.2, prop = 0.75, strata = DV)
  
  # assign training and test
  trn_10 <- rsample::training(split_10)
  tst_10 <- rsample::testing(split_10)
  rm(split_10)
  
  # double check 
  
  # (1) ID check 
  # - no IDs in training in test and vice versa
  trn_tst_check <- trn_10$src_subject_id %in% tst_10$src_subject_id
  tst_trn_check <- tst_10$src_subject_id %in% trn_10$src_subject_id
  if(any(trn_tst_check == FALSE)) cat("No issues w/ID check \n")
  if(any(tst_trn_check == TRUE)) cat("Issues w/ID check \n")
  rm(trn_tst_check, tst_trn_check)
  
  # (2) DV in range check
  # - confirm expected breakdown of DV similar to whole sample
  DV_check <- trn_10 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/5121)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV training in range \n")
  rm(DV_check)
  
  DV_check <- tst_10 %>% 
    subset(DV == 1) %>% 
    mutate(check = n()/1708)
  if(any(DV_check$check > .13 & DV_check$check < .15 )) cat("DV test in range \n")
  rm(DV_check)
  
  # (3) all categories of factor variables represented in both test & training 
  if (nrow(factor_sum(trn_10$race_4l)) == 4 & 
      nrow(factor_sum(tst_10$race_4l)) == 4 &
      nrow(factor_sum(trn_10$eth_hisp)) == 2 & 
      nrow(factor_sum(tst_10$eth_hisp)) == 2 &
      nrow(factor_sum(trn_10$income)) == 10 & 
      nrow(factor_sum(tst_10$income)) == 10 &
      nrow(factor_sum(trn_10$religion)) == 17 & 
      nrow(factor_sum(tst_10$religion)) == 17 &
      nrow(factor_sum(trn_10$p_edu)) == 6 & 
      nrow(factor_sum(tst_10$p_edu)) == 6 &
      nrow(factor_sum(trn_10$sex_2l)) == 2 & 
      nrow(factor_sum(tst_10$sex_2l)) == 2 &
      nrow(factor_sum(trn_10$rec_bin)) == 2 & 
      nrow(factor_sum(tst_10$rec_bin)) == 2 &
      nrow(factor_sum(trn_10$exp_sub)) == 2 & 
      nrow(factor_sum(tst_10$exp_sub)) == 2 &
      nrow(factor_sum(trn_10$tbi_injury)) == 2 & 
      nrow(factor_sum(tst_10$tbi_injury)) == 2 &
      nrow(factor_sum(trn_10$kbi_p_grades_in_school)) == 4 & 
      nrow(factor_sum(tst_10$kbi_p_grades_in_school)) == 4 &
      nrow(factor_sum(trn_10$det_susp)) == 2 & 
      nrow(factor_sum(tst_10$det_susp)) == 2 &
      nrow(factor_sum(trn_10$se_services)) == 5 & 
      nrow(factor_sum(tst_10$se_services)) == 5 &
      nrow(factor_sum(trn_10$cct)) == 2 & 
      nrow(factor_sum(tst_10$cct)) == 2) {
    print(
      'Expected: all categories represented in test & training')
  } else {
    print(
      'Unexpected: all categories represented in test & training')
  }
  
})

# retained objects:
# - trn_1 - trn_10 and tst_1 - tst_10: 10 different splits of test & training 

# MICE --------------------------------------------------------------------

### split (1)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
    
  ## Create a before-MI dataset
  trn_before <- trn_1
    
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
    
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                           'income', 'religion', 'p_edu')
    
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
    
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
    
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                                 trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
    
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
    
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_1/trn_1_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)

  ## set seed for reproducibility
  set.seed(808915)
    
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
    
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)

  # rename
  trn_1_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
    
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
    
  ## Create a before-MI dataset
  tst_before <- tst_1 
    
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
    
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
    
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
    
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
    
    
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
    
    
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
    
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_1/tst_1_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")

  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
    
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
    
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_1_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (2)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_2
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_2/trn_2_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_2_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_2 
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_2/tst_2_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_2_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (3)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_3
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_3/trn_3_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_3_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_3
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_3/tst_3_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_3_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (4)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_4
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_4/trn_4_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_4_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_4
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_4/tst_4_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_4_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (5)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_5
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_5/trn_5_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_5_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_5
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_5/tst_5_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_5_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (6)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_6
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_6/trn_6_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_6_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_6
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_6/tst_6_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_6_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (7)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_7
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_7/trn_7_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_7_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_7
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_7/tst_7_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_7_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (8)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_8
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_8/trn_8_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_8_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_8
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_8/tst_8_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_8_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (9)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_9
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_9/trn_9_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_9_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_9
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_9/tst_9_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_9_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

### split (10)
job::job({
  #----------------------------------------------------------------------------#
  #----------------------------- TRAINING DATASET -----------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  trn_before <- trn_10
  
  ## Extract all variable names in dataset
  trn_allVars <- names(trn_before)
  
  ## names of variables with missingness
  trn_missVars <- names(trn_before)[colSums(is.na(trn_before)) > 0]
  
  ## mice predictorMatrix
  trn_predictorMatrix <- matrix(0, 
                                ncol = length(trn_allVars), 
                                nrow = length(trn_allVars))
  rownames(trn_predictorMatrix) <- trn_allVars
  colnames(trn_predictorMatrix) <- trn_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  trn_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 'eth_hisp', 
                       'income', 'religion', 'p_edu')
  
  ## keep variables that actually exist in dataset
  trn_imputerVars <- intersect(unique(trn_imputerVars), trn_allVars)
  trn_imputerVars
  trn_imputerMatrix <- trn_predictorMatrix
  trn_imputerMatrix[,trn_imputerVars] <- 1
  trn_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  trn_imputedOnlyVars <- c(unlist(trn_missVars))
  
  ## imputers that have missingness must be imputed
  trn_imputedVars <- intersect(unique(c(trn_imputedOnlyVars, trn_imputerVars)), 
                               trn_missVars)
  trn_imputedVars
  trn_imputedMatrix <- trn_predictorMatrix
  trn_imputedMatrix[trn_imputedVars,] <- 1
  trn_imputedMatrix
  
  cat("
  ###  Construct a full predictor matrix (rows: imputed variables; 
  ###  cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  trn_predictorMatrix <- trn_imputerMatrix * trn_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(trn_predictorMatrix) <- 0
  trn_predictorMatrix
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  trn_dryMice <- mice(data = trn_before, m = 1, 
                      predictorMatrix = trn_predictorMatrix, maxit = 0)
  
  ## Update predictor matrix
  trn_predictorMatrix <- trn_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  trn_imputerVars <- colnames(trn_predictorMatrix)[colSums(
    trn_predictorMatrix) > 0]
  trn_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  trn_imputedVars <- rownames(trn_predictorMatrix)[rowSums(
    trn_predictorMatrix) > 0]
  trn_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(trn_imputerVars, trn_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(trn_imputerVars, trn_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(trn_imputedVars, trn_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(trn_missVars, trn_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  trn_predictorMatrix[rowSums(trn_predictorMatrix) > 0, 
                      colSums(trn_predictorMatrix) > 0]
  
  trn_dryMice$method[setdiff(trn_allVars, trn_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  trn_methods <- trn_dryMice$method[sapply(trn_dryMice$method, nchar) > 0]
  capture.output(
    trn_methods, 
    file = 'output/model_dataset_prep/supplement/split_10/trn_10_methods.txt')
  
  cat("
  ##  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## Parallelized execution
  trn_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    trn_miceout <- mice::mice(
      data = trn_before, m = 1,
      print = TRUE, predictorMatrix = trn_predictorMatrix, 
      method = trn_dryMice$method, maxit=20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    trn_miceout
  }
  
  trn_logged_events <- trn_miceout$loggedEvents
  trn_logged_events 
  if (is.null(trn_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(trn_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  trn_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  trn_actuallyImputedVars <-
    setdiff(names(trn_before)[colSums(is.na(trn_before)) > 0],
            names(mice::complete(trn_miceout, action = 1))
            [colSums(is.na(mice::complete(trn_miceout, action = 1))) > 0])
  trn_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(trn_actuallyImputedVars, trn_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(trn_imputedVars, trn_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables were planned for MI but imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(
    trn_miceout, action = 1))[colSums(
      is.na(mice::complete(trn_miceout, action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  trn_imp_df <-mice::complete(trn_miceout, "long", include = TRUE) 
  table(trn_imp_df$.imp)
  
  # rename
  trn_10_imp_df <- trn_imp_df
  
  # remove unnecessary objects
  rm(trn_before, trn_dryMice, trn_imputedMatrix, trn_imputerMatrix,
     trn_predictorMatrix, trn_actuallyImputedVars, trn_allVars, 
     trn_imputedOnlyVars, trn_imputerVars, trn_imputedVars, trn_missVars,
     trn_methods, trn_miceout, trn_imp_df,
     disc_1, disc_2, disc_3)
  
  #----------------------------------------------------------------------------#
  #----------------------------- TEST DATASET ---------------------------------#
  #----------------------------------------------------------------------------#
  cat("
  ###
  ### Missing data imputation with mice
  ##########################################################################\n")
  
  ## Create a before-MI dataset
  tst_before <- tst_10
  
  ## Extract all variable names in dataset
  tst_allVars <- names(tst_before)
  
  ## names of variables with missingness
  tst_missVars <- names(tst_before)[colSums(is.na(tst_before)) > 0]
  
  ## mice predictorMatri
  tst_predictorMatrix <- matrix(0, ncol = length(tst_allVars), 
                                nrow = length(tst_allVars))
  rownames(tst_predictorMatrix) <- tst_allVars
  colnames(tst_predictorMatrix) <- tst_allVars
  
  ## Avoid perfect linear dependence
  cat("
  ###  Specify Variables informing imputation\n")
  ## These can be either complete variables or variables with missingness.
  ## Those with missingness must be imputed.
  ## Explicitly specify.
  tst_imputerVars <- c('age_baseline', 'sex_2l', 'race_4l', 
                       'eth_hisp', 'income', 'religion', 'p_edu')
  
  ## Keep variables that actually exist in dataset
  tst_imputerVars <- intersect(unique(tst_imputerVars), tst_allVars)
  tst_imputerVars
  tst_imputerMatrix <- tst_predictorMatrix
  tst_imputerMatrix[,tst_imputerVars] <- 1
  tst_imputerMatrix
  
  cat("
  ###  Specify variables with missingness to be imputed \n")
  tst_imputedOnlyVars <- c(unlist(tst_missVars))
  
  ## imputers that have missingness must be imputed.
  tst_imputedVars <- intersect(unique(c(tst_imputedOnlyVars, tst_imputerVars)), 
                               tst_missVars)
  tst_imputedVars
  tst_imputedMatrix <- tst_predictorMatrix
  tst_imputedMatrix[tst_imputedVars,] <- 1
  tst_imputedMatrix
  
  
  cat("
  ###  Construct a full predictor matrix 
  ###  (rows: imputed variables; cols: imputer variables)\n")
  ## keep correct imputer-imputed pairs only
  tst_predictorMatrix <- tst_imputerMatrix * tst_imputedMatrix
  ## diagonals must be zeros (a variable cannot impute itself)
  diag(tst_predictorMatrix) <- 0
  tst_predictorMatrix
  
  
  cat("
  ###  Dry-run mice for imputation methods\n")
  tst_dryMice <- mice(data = tst_before, m = 1, 
                      predictorMatrix = tst_predictorMatrix, maxit = 0)
  ## update predictor matrix
  tst_predictorMatrix <- tst_dryMice$predictorMatrix
  cat("###   Imputers (non-zero columns of predictorMatrix)\n")
  tst_imputerVars <- colnames(tst_predictorMatrix)[colSums(
    tst_predictorMatrix) > 0]
  tst_imputerVars
  cat("###   Imputed (non-zero rows of predictorMatrix)\n")
  tst_imputedVars <- rownames(tst_predictorMatrix)[rowSums(
    tst_predictorMatrix) > 0]
  tst_imputedVars
  cat("###   Imputers that are complete\n")
  setdiff(tst_imputerVars, tst_imputedVars)
  cat("###   Imputers with missingness\n")
  intersect(tst_imputerVars, tst_imputedVars)
  cat("###   Imputed-only variables without being imputers\n")
  setdiff(tst_imputedVars, tst_imputerVars)
  cat("###   Variables with missingness that are not imputed\n")
  setdiff(tst_missVars, tst_imputedVars)
  cat("###   Relevant part of predictorMatrix\n")
  tst_predictorMatrix[rowSums(tst_predictorMatrix) > 0, 
                      colSums(tst_predictorMatrix) > 0]
  
  tst_dryMice$method[setdiff(tst_allVars, tst_imputedVars)] <- ""
  cat("###   Methods used for imputation\n")
  
  # output methods used for each variable
  tst_methods <- tst_dryMice$method[sapply(tst_dryMice$method, nchar) > 0]
  capture.output(
    tst_methods, 
    file = 'output/model_dataset_prep/supplement/split_10/tst_10_methods.txt')
  
  cat("
  ###  Run mice\n")
  M <- 5
  cat("### Imputing", M, "times\n")
  
  ## Register doParallel as the parallel backend with foreach
  doParallel::registerDoParallel(cores = nCores)
  
  ## set seed for reproducibility
  set.seed(808915)
  
  ## parallelized execution
  tst_miceout <- foreach(i = seq_len(M), .combine = ibind) %dorng% {
    cat("### Started iteration", i, "\n")
    tst_miceout <- mice::mice(
      data = tst_before, m = 1, 
      print = TRUE, predictorMatrix = tst_predictorMatrix, 
      method = tst_dryMice$method, maxit = 20)
    cat("### Completed iteration", i, "\n")
    ## Make sure to return the output
    tst_miceout
  }
  
  tst_logged_events <- tst_miceout$loggedEvents
  tst_logged_events 
  if (is.null(tst_logged_events)) {
    print('No logged events')
  } else {
    print('Logged events')
  }
  rm(tst_logged_events)
  
  cat("
  ###  Show mice results\n")
  ## mice object ifself
  tst_miceout
  ## variables that no longer have missingness after imputation
  cat("###   Variables actually imputed\n")
  tst_actuallyImputedVars <-
    setdiff(names(tst_before)[colSums(is.na(tst_before)) > 0],
            names(mice::complete(tst_miceout, action = 1))
            [colSums(is.na(mice::complete(tst_miceout, action = 1))) > 0])
  tst_actuallyImputedVars
  
  ## examine discrepancies
  cat("###   Variables that were unexpectedly imputed\n")
  disc_1 <- setdiff(tst_actuallyImputedVars, tst_imputedVars)
  if (is.character(disc_1) && length(0)) {
    print('No variables were unexpectedly imputed')
  } else {
    print('Variables were unexpectedly imputed')
  }
  
  cat("###   Variables that were planned for MI but not imputed\n")
  disc_2 <- setdiff(tst_imputedVars, tst_actuallyImputedVars)
  if (is.character(disc_2) && length(0)) {
    print('No variables were planned for MI but not imputed')
  } else {
    print('Variables that were planned for MI but not imputed')
  }
  
  ## still missing variables
  cat("###   Variables still having missing values\n")
  disc_3 <- names(mice::complete(tst_miceout, action = 1))[colSums(
    is.na(mice::complete(tst_miceout,action = 1))) > 0]
  if (is.character(disc_3) && length(0)) {
    print('No variables still have missing values')
  } else {
    print('Variables still have missing values')
  }
  
  ## save MICE as df with original data + 5 imputations
  tst_imp_df <-mice::complete(tst_miceout, "long", include = TRUE) 
  table(tst_imp_df$.imp)
  
  # rename
  tst_10_imp_df <- tst_imp_df
  
  # remove unnecessary objects
  rm(tst_before, tst_dryMice, tst_imputedMatrix, tst_imputerMatrix,
     tst_predictorMatrix, tst_actuallyImputedVars, tst_allVars, 
     tst_imputedOnlyVars, tst_imputerVars, tst_imputedVars, tst_missVars,
     tst_methods, tst_miceout, tst_imp_df,
     disc_1, disc_2, disc_3)
})

# retained objects:
# - trn_1_imp_df - trn_10_imp_df & tst_1_imp_df - tst_10_imp_df: df w/observed
#   & imputed data across 10 splits

# MICE Follow-Up ---------------------------------------------------------------

#------------------------------------------------------------------------------#
#           Confirm variables with pre-set scale are in expected range
#------------------------------------------------------------------------------#

### split (1)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  
  trn_1_imp_df_0 <- trn_1_imp_df %>% 
    subset(.imp != 0) 

  # Self and Peer Involvement with Substance Use
  if (all(trn_1_imp_df_0$peer_alc >= 0) && all(trn_1_imp_df_0$peer_alc <= 8) &&
      all(trn_1_imp_df_0$peer_tob >= 0) && all(trn_1_imp_df_0$peer_tob <= 8) &&
      all(trn_1_imp_df_0$peer_cb >= 0) && all(trn_1_imp_df_0$peer_cb <= 4) &&
      all(trn_1_imp_df_0$peer_other >= 0) && all(trn_1_imp_df_0$peer_other <= 8) &&
      all(trn_1_imp_df_0$peer_prob >= 0) && all(trn_1_imp_df_0$peer_prob <= 8) &&
      all(trn_1_imp_df_0$path_alc >= 3) && all(trn_1_imp_df_0$path_alc <= 12) &&
      all(trn_1_imp_df_0$path_tob >= 3) && all(trn_1_imp_df_0$path_tob <= 12) &&
      all(trn_1_imp_df_0$path_cb >= 3) && all(trn_1_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_1_imp_df_0$crpf >= 0) && all(trn_1_imp_df_0$crpf <= 15) &&
      all(trn_1_imp_df_0$par_rules >= 3) && all(trn_1_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_1_imp_df_0$mh_density >= 0) && all(trn_1_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
 
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_1_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_1_imp_df_0$act_1 >= 0) && all(trn_1_imp_df_0$act_1 <= 7) &&
      all(trn_1_imp_df_0$act_2 >= 0) && all(trn_1_imp_df_0$act_2 <= 7) &&
      all(trn_1_imp_df_0$act_5 >= 0) && all(trn_1_imp_df_0$act_5 <= 5) &&
      all(trn_1_imp_df_0$exp_caf_rec >= 0) && all(trn_1_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_1_imp_df_0$accult_q2_y >= 0) && all(trn_1_imp_df_0$accult_q2_y <= 3) &&
      all(trn_1_imp_df_0$neighborhood_crime_y >= 1) && all(trn_1_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_1_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
        all(trn_1_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_1_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
        all(trn_1_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_1_imp_df_0$pea_ravlt_learn >= -15) && 
        all(trn_1_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_1_imp_df_0$lmt_acc >= 0) && 
        all(trn_1_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_1_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  
  tst_1_imp_df_0 <- tst_1_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_1_imp_df_0$peer_alc >= 0) && all(tst_1_imp_df_0$peer_alc <= 8) &&
      all(tst_1_imp_df_0$peer_tob >= 0) && all(tst_1_imp_df_0$peer_tob <= 8) &&
      all(tst_1_imp_df_0$peer_cb >= 0) && all(tst_1_imp_df_0$peer_cb <= 4) &&
      all(tst_1_imp_df_0$peer_other >= 0) && all(tst_1_imp_df_0$peer_other <= 8) &&
      all(tst_1_imp_df_0$peer_prob >= 0) && all(tst_1_imp_df_0$peer_prob <= 8) &&
      all(tst_1_imp_df_0$path_alc >= 3) && all(tst_1_imp_df_0$path_alc <= 12) &&
      all(tst_1_imp_df_0$path_tob >= 3) && all(tst_1_imp_df_0$path_tob <= 12) &&
      all(tst_1_imp_df_0$path_cb >= 3) && all(tst_1_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_1_imp_df_0$crpf >= 0) && all(tst_1_imp_df_0$crpf <= 15) &&
      all(tst_1_imp_df_0$par_rules >= 3) && all(tst_1_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_1_imp_df_0$mh_density >= 0) && all(tst_1_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_1_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_1_imp_df_0$act_1 >= 0) && all(tst_1_imp_df_0$act_1 <= 7) &&
      all(tst_1_imp_df_0$act_2 >= 0) && all(tst_1_imp_df_0$act_2 <= 7) &&
      all(tst_1_imp_df_0$act_5 >= 0) && all(tst_1_imp_df_0$act_5 <= 5) &&
      all(tst_1_imp_df_0$exp_caf_rec >= 0) && all(tst_1_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_1_imp_df_0$accult_q2_y >= 0) && all(tst_1_imp_df_0$accult_q2_y <= 3) &&
      all(tst_1_imp_df_0$neighborhood_crime_y >= 1) && all(tst_1_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_1_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_1_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_1_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_1_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_1_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_1_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_1_imp_df_0$lmt_acc >= 0) && 
      all(tst_1_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_1_imp_df_0, temp)
  
})

### split (2)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_2_imp_df_0 <- trn_2_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_2_imp_df_0$peer_alc >= 0) && all(trn_2_imp_df_0$peer_alc <= 8) &&
      all(trn_2_imp_df_0$peer_tob >= 0) && all(trn_2_imp_df_0$peer_tob <= 8) &&
      all(trn_2_imp_df_0$peer_cb >= 0) && all(trn_2_imp_df_0$peer_cb <= 4) &&
      all(trn_2_imp_df_0$peer_other >= 0) && all(trn_2_imp_df_0$peer_other <= 8) &&
      all(trn_2_imp_df_0$peer_prob >= 0) && all(trn_2_imp_df_0$peer_prob <= 8) &&
      all(trn_2_imp_df_0$path_alc >= 3) && all(trn_2_imp_df_0$path_alc <= 12) &&
      all(trn_2_imp_df_0$path_tob >= 3) && all(trn_2_imp_df_0$path_tob <= 12) &&
      all(trn_2_imp_df_0$path_cb >= 3) && all(trn_2_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_2_imp_df_0$crpf >= 0) && all(trn_2_imp_df_0$crpf <= 15) &&
      all(trn_2_imp_df_0$par_rules >= 3) && all(trn_2_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_2_imp_df_0$mh_density >= 0) && all(trn_2_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_2_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_2_imp_df_0$act_1 >= 0) && all(trn_2_imp_df_0$act_1 <= 7) &&
      all(trn_2_imp_df_0$act_2 >= 0) && all(trn_2_imp_df_0$act_2 <= 7) &&
      all(trn_2_imp_df_0$act_5 >= 0) && all(trn_2_imp_df_0$act_5 <= 5) &&
      all(trn_2_imp_df_0$exp_caf_rec >= 0) && all(trn_2_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_2_imp_df_0$accult_q2_y >= 0) && all(trn_2_imp_df_0$accult_q2_y <= 3) &&
      all(trn_2_imp_df_0$neighborhood_crime_y >= 1) && all(trn_2_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_2_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_2_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_2_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_2_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_2_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_2_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_2_imp_df_0$lmt_acc >= 0) && 
      all(trn_2_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_2_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_2_imp_df_0 <- tst_2_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_2_imp_df_0$peer_alc >= 0) && all(tst_2_imp_df_0$peer_alc <= 8) &&
      all(tst_2_imp_df_0$peer_tob >= 0) && all(tst_2_imp_df_0$peer_tob <= 8) &&
      all(tst_2_imp_df_0$peer_cb >= 0) && all(tst_2_imp_df_0$peer_cb <= 4) &&
      all(tst_2_imp_df_0$peer_other >= 0) && all(tst_2_imp_df_0$peer_other <= 8) &&
      all(tst_2_imp_df_0$peer_prob >= 0) && all(tst_2_imp_df_0$peer_prob <= 8) &&
      all(tst_2_imp_df_0$path_alc >= 3) && all(tst_2_imp_df_0$path_alc <= 12) &&
      all(tst_2_imp_df_0$path_tob >= 3) && all(tst_2_imp_df_0$path_tob <= 12) &&
      all(tst_2_imp_df_0$path_cb >= 3) && all(tst_2_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_2_imp_df_0$crpf >= 0) && all(tst_2_imp_df_0$crpf <= 15) &&
      all(tst_2_imp_df_0$par_rules >= 3) && all(tst_2_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_2_imp_df_0$mh_density >= 0) && all(tst_2_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_2_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_2_imp_df_0$act_1 >= 0) && all(tst_2_imp_df_0$act_1 <= 7) &&
      all(tst_2_imp_df_0$act_2 >= 0) && all(tst_2_imp_df_0$act_2 <= 7) &&
      all(tst_2_imp_df_0$act_5 >= 0) && all(tst_2_imp_df_0$act_5 <= 5) &&
      all(tst_2_imp_df_0$exp_caf_rec >= 0) && all(tst_2_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_2_imp_df_0$accult_q2_y >= 0) && all(tst_2_imp_df_0$accult_q2_y <= 3) &&
      all(tst_2_imp_df_0$neighborhood_crime_y >= 1) && all(tst_2_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_2_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_2_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_2_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_2_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_2_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_2_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_2_imp_df_0$lmt_acc >= 0) && 
      all(tst_2_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_2_imp_df_0, temp)
})

### split (3)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_3_imp_df_0 <- trn_3_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_3_imp_df_0$peer_alc >= 0) && all(trn_3_imp_df_0$peer_alc <= 8) &&
      all(trn_3_imp_df_0$peer_tob >= 0) && all(trn_3_imp_df_0$peer_tob <= 8) &&
      all(trn_3_imp_df_0$peer_cb >= 0) && all(trn_3_imp_df_0$peer_cb <= 4) &&
      all(trn_3_imp_df_0$peer_other >= 0) && all(trn_3_imp_df_0$peer_other <= 8) &&
      all(trn_3_imp_df_0$peer_prob >= 0) && all(trn_3_imp_df_0$peer_prob <= 8) &&
      all(trn_3_imp_df_0$path_alc >= 3) && all(trn_3_imp_df_0$path_alc <= 12) &&
      all(trn_3_imp_df_0$path_tob >= 3) && all(trn_3_imp_df_0$path_tob <= 12) &&
      all(trn_3_imp_df_0$path_cb >= 3) && all(trn_3_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_3_imp_df_0$crpf >= 0) && all(trn_3_imp_df_0$crpf <= 15) &&
      all(trn_3_imp_df_0$par_rules >= 3) && all(trn_3_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_3_imp_df_0$mh_density >= 0) && all(trn_3_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_3_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_3_imp_df_0$act_1 >= 0) && all(trn_3_imp_df_0$act_1 <= 7) &&
      all(trn_3_imp_df_0$act_2 >= 0) && all(trn_3_imp_df_0$act_2 <= 7) &&
      all(trn_3_imp_df_0$act_5 >= 0) && all(trn_3_imp_df_0$act_5 <= 5) &&
      all(trn_3_imp_df_0$exp_caf_rec >= 0) && all(trn_3_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_3_imp_df_0$accult_q2_y >= 0) && all(trn_3_imp_df_0$accult_q2_y <= 3) &&
      all(trn_3_imp_df_0$neighborhood_crime_y >= 1) && all(trn_3_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_3_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_3_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_3_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_3_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_3_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_3_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_3_imp_df_0$lmt_acc >= 0) && 
      all(trn_3_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_3_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_3_imp_df_0 <- tst_3_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_3_imp_df_0$peer_alc >= 0) && all(tst_3_imp_df_0$peer_alc <= 8) &&
      all(tst_3_imp_df_0$peer_tob >= 0) && all(tst_3_imp_df_0$peer_tob <= 8) &&
      all(tst_3_imp_df_0$peer_cb >= 0) && all(tst_3_imp_df_0$peer_cb <= 4) &&
      all(tst_3_imp_df_0$peer_other >= 0) && all(tst_3_imp_df_0$peer_other <= 8) &&
      all(tst_3_imp_df_0$peer_prob >= 0) && all(tst_3_imp_df_0$peer_prob <= 8) &&
      all(tst_3_imp_df_0$path_alc >= 3) && all(tst_3_imp_df_0$path_alc <= 12) &&
      all(tst_3_imp_df_0$path_tob >= 3) && all(tst_3_imp_df_0$path_tob <= 12) &&
      all(tst_3_imp_df_0$path_cb >= 3) && all(tst_3_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_3_imp_df_0$crpf >= 0) && all(tst_3_imp_df_0$crpf <= 15) &&
      all(tst_3_imp_df_0$par_rules >= 3) && all(tst_3_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_3_imp_df_0$mh_density >= 0) && all(tst_3_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_3_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_3_imp_df_0$act_1 >= 0) && all(tst_3_imp_df_0$act_1 <= 7) &&
      all(tst_3_imp_df_0$act_2 >= 0) && all(tst_3_imp_df_0$act_2 <= 7) &&
      all(tst_3_imp_df_0$act_5 >= 0) && all(tst_3_imp_df_0$act_5 <= 5) &&
      all(tst_3_imp_df_0$exp_caf_rec >= 0) && all(tst_3_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_3_imp_df_0$accult_q2_y >= 0) && all(tst_3_imp_df_0$accult_q2_y <= 3) &&
      all(tst_3_imp_df_0$neighborhood_crime_y >= 1) && all(tst_3_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_3_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_3_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_3_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_3_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_3_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_3_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_3_imp_df_0$lmt_acc >= 0) && 
      all(tst_3_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_3_imp_df_0, temp)
})

### split (4)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_4_imp_df_0 <- trn_4_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_4_imp_df_0$peer_alc >= 0) && all(trn_4_imp_df_0$peer_alc <= 8) &&
      all(trn_4_imp_df_0$peer_tob >= 0) && all(trn_4_imp_df_0$peer_tob <= 8) &&
      all(trn_4_imp_df_0$peer_cb >= 0) && all(trn_4_imp_df_0$peer_cb <= 4) &&
      all(trn_4_imp_df_0$peer_other >= 0) && all(trn_4_imp_df_0$peer_other <= 8) &&
      all(trn_4_imp_df_0$peer_prob >= 0) && all(trn_4_imp_df_0$peer_prob <= 8) &&
      all(trn_4_imp_df_0$path_alc >= 3) && all(trn_4_imp_df_0$path_alc <= 12) &&
      all(trn_4_imp_df_0$path_tob >= 3) && all(trn_4_imp_df_0$path_tob <= 12) &&
      all(trn_4_imp_df_0$path_cb >= 3) && all(trn_4_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_4_imp_df_0$crpf >= 0) && all(trn_4_imp_df_0$crpf <= 15) &&
      all(trn_4_imp_df_0$par_rules >= 3) && all(trn_4_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_4_imp_df_0$mh_density >= 0) && all(trn_4_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_4_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_4_imp_df_0$act_1 >= 0) && all(trn_4_imp_df_0$act_1 <= 7) &&
      all(trn_4_imp_df_0$act_2 >= 0) && all(trn_4_imp_df_0$act_2 <= 7) &&
      all(trn_4_imp_df_0$act_5 >= 0) && all(trn_4_imp_df_0$act_5 <= 5) &&
      all(trn_4_imp_df_0$exp_caf_rec >= 0) && all(trn_4_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_4_imp_df_0$accult_q2_y >= 0) && all(trn_4_imp_df_0$accult_q2_y <= 3) &&
      all(trn_4_imp_df_0$neighborhood_crime_y >= 1) && all(trn_4_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_4_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_4_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_4_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_4_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_4_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_4_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_4_imp_df_0$lmt_acc >= 0) && 
      all(trn_4_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_4_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_4_imp_df_0 <- tst_4_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_4_imp_df_0$peer_alc >= 0) && all(tst_4_imp_df_0$peer_alc <= 8) &&
      all(tst_4_imp_df_0$peer_tob >= 0) && all(tst_4_imp_df_0$peer_tob <= 8) &&
      all(tst_4_imp_df_0$peer_cb >= 0) && all(tst_4_imp_df_0$peer_cb <= 4) &&
      all(tst_4_imp_df_0$peer_other >= 0) && all(tst_4_imp_df_0$peer_other <= 8) &&
      all(tst_4_imp_df_0$peer_prob >= 0) && all(tst_4_imp_df_0$peer_prob <= 8) &&
      all(tst_4_imp_df_0$path_alc >= 3) && all(tst_4_imp_df_0$path_alc <= 12) &&
      all(tst_4_imp_df_0$path_tob >= 3) && all(tst_4_imp_df_0$path_tob <= 12) &&
      all(tst_4_imp_df_0$path_cb >= 3) && all(tst_4_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_4_imp_df_0$crpf >= 0) && all(tst_4_imp_df_0$crpf <= 15) &&
      all(tst_4_imp_df_0$par_rules >= 3) && all(tst_4_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_4_imp_df_0$mh_density >= 0) && all(tst_4_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_4_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_4_imp_df_0$act_1 >= 0) && all(tst_4_imp_df_0$act_1 <= 7) &&
      all(tst_4_imp_df_0$act_2 >= 0) && all(tst_4_imp_df_0$act_2 <= 7) &&
      all(tst_4_imp_df_0$act_5 >= 0) && all(tst_4_imp_df_0$act_5 <= 5) &&
      all(tst_4_imp_df_0$exp_caf_rec >= 0) && all(tst_4_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_4_imp_df_0$accult_q2_y >= 0) && all(tst_4_imp_df_0$accult_q2_y <= 3) &&
      all(tst_4_imp_df_0$neighborhood_crime_y >= 1) && all(tst_4_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_4_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_4_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_4_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_4_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_4_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_4_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_4_imp_df_0$lmt_acc >= 0) && 
      all(tst_4_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_4_imp_df_0, temp)
})

### split (5)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_5_imp_df_0 <- trn_5_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_5_imp_df_0$peer_alc >= 0) && all(trn_5_imp_df_0$peer_alc <= 8) &&
      all(trn_5_imp_df_0$peer_tob >= 0) && all(trn_5_imp_df_0$peer_tob <= 8) &&
      all(trn_5_imp_df_0$peer_cb >= 0) && all(trn_5_imp_df_0$peer_cb <= 4) &&
      all(trn_5_imp_df_0$peer_other >= 0) && all(trn_5_imp_df_0$peer_other <= 8) &&
      all(trn_5_imp_df_0$peer_prob >= 0) && all(trn_5_imp_df_0$peer_prob <= 8) &&
      all(trn_5_imp_df_0$path_alc >= 3) && all(trn_5_imp_df_0$path_alc <= 12) &&
      all(trn_5_imp_df_0$path_tob >= 3) && all(trn_5_imp_df_0$path_tob <= 12) &&
      all(trn_5_imp_df_0$path_cb >= 3) && all(trn_5_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_5_imp_df_0$crpf >= 0) && all(trn_5_imp_df_0$crpf <= 15) &&
      all(trn_5_imp_df_0$par_rules >= 3) && all(trn_5_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_5_imp_df_0$mh_density >= 0) && all(trn_5_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_5_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_5_imp_df_0$act_1 >= 0) && all(trn_5_imp_df_0$act_1 <= 7) &&
      all(trn_5_imp_df_0$act_2 >= 0) && all(trn_5_imp_df_0$act_2 <= 7) &&
      all(trn_5_imp_df_0$act_5 >= 0) && all(trn_5_imp_df_0$act_5 <= 5) &&
      all(trn_5_imp_df_0$exp_caf_rec >= 0) && all(trn_5_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_5_imp_df_0$accult_q2_y >= 0) && all(trn_5_imp_df_0$accult_q2_y <= 3) &&
      all(trn_5_imp_df_0$neighborhood_crime_y >= 1) && all(trn_5_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_5_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_5_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_5_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_5_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_5_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_5_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_5_imp_df_0$lmt_acc >= 0) && 
      all(trn_5_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_5_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_5_imp_df_0 <- tst_5_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_5_imp_df_0$peer_alc >= 0) && all(tst_5_imp_df_0$peer_alc <= 8) &&
      all(tst_5_imp_df_0$peer_tob >= 0) && all(tst_5_imp_df_0$peer_tob <= 8) &&
      all(tst_5_imp_df_0$peer_cb >= 0) && all(tst_5_imp_df_0$peer_cb <= 4) &&
      all(tst_5_imp_df_0$peer_other >= 0) && all(tst_5_imp_df_0$peer_other <= 8) &&
      all(tst_5_imp_df_0$peer_prob >= 0) && all(tst_5_imp_df_0$peer_prob <= 8) &&
      all(tst_5_imp_df_0$path_alc >= 3) && all(tst_5_imp_df_0$path_alc <= 12) &&
      all(tst_5_imp_df_0$path_tob >= 3) && all(tst_5_imp_df_0$path_tob <= 12) &&
      all(tst_5_imp_df_0$path_cb >= 3) && all(tst_5_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_5_imp_df_0$crpf >= 0) && all(tst_5_imp_df_0$crpf <= 15) &&
      all(tst_5_imp_df_0$par_rules >= 3) && all(tst_5_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_5_imp_df_0$mh_density >= 0) && all(tst_5_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_5_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_5_imp_df_0$act_1 >= 0) && all(tst_5_imp_df_0$act_1 <= 7) &&
      all(tst_5_imp_df_0$act_2 >= 0) && all(tst_5_imp_df_0$act_2 <= 7) &&
      all(tst_5_imp_df_0$act_5 >= 0) && all(tst_5_imp_df_0$act_5 <= 5) &&
      all(tst_5_imp_df_0$exp_caf_rec >= 0) && all(tst_5_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_5_imp_df_0$accult_q2_y >= 0) && all(tst_5_imp_df_0$accult_q2_y <= 3) &&
      all(tst_5_imp_df_0$neighborhood_crime_y >= 1) && all(tst_5_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_5_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_5_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_5_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_5_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_5_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_5_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_5_imp_df_0$lmt_acc >= 0) && 
      all(tst_5_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_5_imp_df_0, temp)
})

### split (6)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_6_imp_df_0 <- trn_6_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_6_imp_df_0$peer_alc >= 0) && all(trn_6_imp_df_0$peer_alc <= 8) &&
      all(trn_6_imp_df_0$peer_tob >= 0) && all(trn_6_imp_df_0$peer_tob <= 8) &&
      all(trn_6_imp_df_0$peer_cb >= 0) && all(trn_6_imp_df_0$peer_cb <= 4) &&
      all(trn_6_imp_df_0$peer_other >= 0) && all(trn_6_imp_df_0$peer_other <= 8) &&
      all(trn_6_imp_df_0$peer_prob >= 0) && all(trn_6_imp_df_0$peer_prob <= 8) &&
      all(trn_6_imp_df_0$path_alc >= 3) && all(trn_6_imp_df_0$path_alc <= 12) &&
      all(trn_6_imp_df_0$path_tob >= 3) && all(trn_6_imp_df_0$path_tob <= 12) &&
      all(trn_6_imp_df_0$path_cb >= 3) && all(trn_6_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_6_imp_df_0$crpf >= 0) && all(trn_6_imp_df_0$crpf <= 15) &&
      all(trn_6_imp_df_0$par_rules >= 3) && all(trn_6_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_6_imp_df_0$mh_density >= 0) && all(trn_6_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_6_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_6_imp_df_0$act_1 >= 0) && all(trn_6_imp_df_0$act_1 <= 7) &&
      all(trn_6_imp_df_0$act_2 >= 0) && all(trn_6_imp_df_0$act_2 <= 7) &&
      all(trn_6_imp_df_0$act_5 >= 0) && all(trn_6_imp_df_0$act_5 <= 5) &&
      all(trn_6_imp_df_0$exp_caf_rec >= 0) && all(trn_6_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_6_imp_df_0$accult_q2_y >= 0) && all(trn_6_imp_df_0$accult_q2_y <= 3) &&
      all(trn_6_imp_df_0$neighborhood_crime_y >= 1) && all(trn_6_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_6_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_6_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_6_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_6_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_6_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_6_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_6_imp_df_0$lmt_acc >= 0) && 
      all(trn_6_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_6_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_6_imp_df_0 <- tst_6_imp_df %>% 
    subset(.imp != 0) 
  
  # SSelf and Peer Involvement with Substance Use
  if (all(tst_6_imp_df_0$peer_alc >= 0) && all(tst_6_imp_df_0$peer_alc <= 8) &&
      all(tst_6_imp_df_0$peer_tob >= 0) && all(tst_6_imp_df_0$peer_tob <= 8) &&
      all(tst_6_imp_df_0$peer_cb >= 0) && all(tst_6_imp_df_0$peer_cb <= 4) &&
      all(tst_6_imp_df_0$peer_other >= 0) && all(tst_6_imp_df_0$peer_other <= 8) &&
      all(tst_6_imp_df_0$peer_prob >= 0) && all(tst_6_imp_df_0$peer_prob <= 8) &&
      all(tst_6_imp_df_0$path_alc >= 3) && all(tst_6_imp_df_0$path_alc <= 12) &&
      all(tst_6_imp_df_0$path_tob >= 3) && all(tst_6_imp_df_0$path_tob <= 12) &&
      all(tst_6_imp_df_0$path_cb >= 3) && all(tst_6_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_6_imp_df_0$crpf >= 0) && all(tst_6_imp_df_0$crpf <= 15) &&
      all(tst_6_imp_df_0$par_rules >= 3) && all(tst_6_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_6_imp_df_0$mh_density >= 0) && all(tst_6_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_6_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_6_imp_df_0$act_1 >= 0) && all(tst_6_imp_df_0$act_1 <= 7) &&
      all(tst_6_imp_df_0$act_2 >= 0) && all(tst_6_imp_df_0$act_2 <= 7) &&
      all(tst_6_imp_df_0$act_5 >= 0) && all(tst_6_imp_df_0$act_5 <= 5) &&
      all(tst_6_imp_df_0$exp_caf_rec >= 0) && all(tst_6_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_6_imp_df_0$accult_q2_y >= 0) && all(tst_6_imp_df_0$accult_q2_y <= 3) &&
      all(tst_6_imp_df_0$neighborhood_crime_y >= 1) && all(tst_6_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_6_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_6_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_6_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_6_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_6_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_6_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_6_imp_df_0$lmt_acc >= 0) && 
      all(tst_6_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_6_imp_df_0, temp)
})

### split (7)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_7_imp_df_0 <- trn_7_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_7_imp_df_0$peer_alc >= 0) && all(trn_7_imp_df_0$peer_alc <= 8) &&
      all(trn_7_imp_df_0$peer_tob >= 0) && all(trn_7_imp_df_0$peer_tob <= 8) &&
      all(trn_7_imp_df_0$peer_cb >= 0) && all(trn_7_imp_df_0$peer_cb <= 4) &&
      all(trn_7_imp_df_0$peer_other >= 0) && all(trn_7_imp_df_0$peer_other <= 8) &&
      all(trn_7_imp_df_0$peer_prob >= 0) && all(trn_7_imp_df_0$peer_prob <= 8) &&
      all(trn_7_imp_df_0$path_alc >= 3) && all(trn_7_imp_df_0$path_alc <= 12) &&
      all(trn_7_imp_df_0$path_tob >= 3) && all(trn_7_imp_df_0$path_tob <= 12) &&
      all(trn_7_imp_df_0$path_cb >= 3) && all(trn_7_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_7_imp_df_0$crpf >= 0) && all(trn_7_imp_df_0$crpf <= 15) &&
      all(trn_7_imp_df_0$par_rules >= 3) && all(trn_7_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_7_imp_df_0$mh_density >= 0) && all(trn_7_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_7_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_7_imp_df_0$act_1 >= 0) && all(trn_7_imp_df_0$act_1 <= 7) &&
      all(trn_7_imp_df_0$act_2 >= 0) && all(trn_7_imp_df_0$act_2 <= 7) &&
      all(trn_7_imp_df_0$act_5 >= 0) && all(trn_7_imp_df_0$act_5 <= 5) &&
      all(trn_7_imp_df_0$exp_caf_rec >= 0) && all(trn_7_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_7_imp_df_0$accult_q2_y >= 0) && all(trn_7_imp_df_0$accult_q2_y <= 3) &&
      all(trn_7_imp_df_0$neighborhood_crime_y >= 1) && all(trn_7_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_7_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_7_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_7_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_7_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_7_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_7_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_7_imp_df_0$lmt_acc >= 0) && 
      all(trn_7_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_7_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_7_imp_df_0 <- tst_7_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_7_imp_df_0$peer_alc >= 0) && all(tst_7_imp_df_0$peer_alc <= 8) &&
      all(tst_7_imp_df_0$peer_tob >= 0) && all(tst_7_imp_df_0$peer_tob <= 8) &&
      all(tst_7_imp_df_0$peer_cb >= 0) && all(tst_7_imp_df_0$peer_cb <= 4) &&
      all(tst_7_imp_df_0$peer_other >= 0) && all(tst_7_imp_df_0$peer_other <= 8) &&
      all(tst_7_imp_df_0$peer_prob >= 0) && all(tst_7_imp_df_0$peer_prob <= 8) &&
      all(tst_7_imp_df_0$path_alc >= 3) && all(tst_7_imp_df_0$path_alc <= 12) &&
      all(tst_7_imp_df_0$path_tob >= 3) && all(tst_7_imp_df_0$path_tob <= 12) &&
      all(tst_7_imp_df_0$path_cb >= 3) && all(tst_7_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_7_imp_df_0$crpf >= 0) && all(tst_7_imp_df_0$crpf <= 15) &&
      all(tst_7_imp_df_0$par_rules >= 3) && all(tst_7_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_7_imp_df_0$mh_density >= 0) && all(tst_7_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_7_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_7_imp_df_0$act_1 >= 0) && all(tst_7_imp_df_0$act_1 <= 7) &&
      all(tst_7_imp_df_0$act_2 >= 0) && all(tst_7_imp_df_0$act_2 <= 7) &&
      all(tst_7_imp_df_0$act_5 >= 0) && all(tst_7_imp_df_0$act_5 <= 5) &&
      all(tst_7_imp_df_0$exp_caf_rec >= 0) && all(tst_7_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_7_imp_df_0$accult_q2_y >= 0) && all(tst_7_imp_df_0$accult_q2_y <= 3) &&
      all(tst_7_imp_df_0$neighborhood_crime_y >= 1) && all(tst_7_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_7_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_7_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_7_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_7_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_7_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_7_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_7_imp_df_0$lmt_acc >= 0) && 
      all(tst_7_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_7_imp_df_0, temp)
})

### split (8)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_8_imp_df_0 <- trn_8_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_8_imp_df_0$peer_alc >= 0) && all(trn_8_imp_df_0$peer_alc <= 8) &&
      all(trn_8_imp_df_0$peer_tob >= 0) && all(trn_8_imp_df_0$peer_tob <= 8) &&
      all(trn_8_imp_df_0$peer_cb >= 0) && all(trn_8_imp_df_0$peer_cb <= 4) &&
      all(trn_8_imp_df_0$peer_other >= 0) && all(trn_8_imp_df_0$peer_other <= 8) &&
      all(trn_8_imp_df_0$peer_prob >= 0) && all(trn_8_imp_df_0$peer_prob <= 8) &&
      all(trn_8_imp_df_0$path_alc >= 3) && all(trn_8_imp_df_0$path_alc <= 12) &&
      all(trn_8_imp_df_0$path_tob >= 3) && all(trn_8_imp_df_0$path_tob <= 12) &&
      all(trn_8_imp_df_0$path_cb >= 3) && all(trn_8_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_8_imp_df_0$crpf >= 0) && all(trn_8_imp_df_0$crpf <= 15) &&
      all(trn_8_imp_df_0$par_rules >= 3) && all(trn_8_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_8_imp_df_0$mh_density >= 0) && all(trn_8_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_8_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_8_imp_df_0$act_1 >= 0) && all(trn_8_imp_df_0$act_1 <= 7) &&
      all(trn_8_imp_df_0$act_2 >= 0) && all(trn_8_imp_df_0$act_2 <= 7) &&
      all(trn_8_imp_df_0$act_5 >= 0) && all(trn_8_imp_df_0$act_5 <= 5) &&
      all(trn_8_imp_df_0$exp_caf_rec >= 0) && all(trn_8_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_8_imp_df_0$accult_q2_y >= 0) && all(trn_8_imp_df_0$accult_q2_y <= 3) &&
      all(trn_8_imp_df_0$neighborhood_crime_y >= 1) && all(trn_8_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_8_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_8_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_8_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_8_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_8_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_8_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_8_imp_df_0$lmt_acc >= 0) && 
      all(trn_8_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_8_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_8_imp_df_0 <- tst_8_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_8_imp_df_0$peer_alc >= 0) && all(tst_8_imp_df_0$peer_alc <= 8) &&
      all(tst_8_imp_df_0$peer_tob >= 0) && all(tst_8_imp_df_0$peer_tob <= 8) &&
      all(tst_8_imp_df_0$peer_cb >= 0) && all(tst_8_imp_df_0$peer_cb <= 4) &&
      all(tst_8_imp_df_0$peer_other >= 0) && all(tst_8_imp_df_0$peer_other <= 8) &&
      all(tst_8_imp_df_0$peer_prob >= 0) && all(tst_8_imp_df_0$peer_prob <= 8) &&
      all(tst_8_imp_df_0$path_alc >= 3) && all(tst_8_imp_df_0$path_alc <= 12) &&
      all(tst_8_imp_df_0$path_tob >= 3) && all(tst_8_imp_df_0$path_tob <= 12) &&
      all(tst_8_imp_df_0$path_cb >= 3) && all(tst_8_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_8_imp_df_0$crpf >= 0) && all(tst_8_imp_df_0$crpf <= 15) &&
      all(tst_8_imp_df_0$par_rules >= 3) && all(tst_8_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_8_imp_df_0$mh_density >= 0) && all(tst_8_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_8_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_8_imp_df_0$act_1 >= 0) && all(tst_8_imp_df_0$act_1 <= 7) &&
      all(tst_8_imp_df_0$act_2 >= 0) && all(tst_8_imp_df_0$act_2 <= 7) &&
      all(tst_8_imp_df_0$act_5 >= 0) && all(tst_8_imp_df_0$act_5 <= 5) &&
      all(tst_8_imp_df_0$exp_caf_rec >= 0) && all(tst_8_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_8_imp_df_0$accult_q2_y >= 0) && all(tst_8_imp_df_0$accult_q2_y <= 3) &&
      all(tst_8_imp_df_0$neighborhood_crime_y >= 1) && all(tst_8_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_8_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_8_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_8_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_8_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_8_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_8_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_8_imp_df_0$lmt_acc >= 0) && 
      all(tst_8_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_8_imp_df_0, temp)
})

### split (9)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_9_imp_df_0 <- trn_9_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_9_imp_df_0$peer_alc >= 0) && all(trn_9_imp_df_0$peer_alc <= 8) &&
      all(trn_9_imp_df_0$peer_tob >= 0) && all(trn_9_imp_df_0$peer_tob <= 8) &&
      all(trn_9_imp_df_0$peer_cb >= 0) && all(trn_9_imp_df_0$peer_cb <= 4) &&
      all(trn_9_imp_df_0$peer_other >= 0) && all(trn_9_imp_df_0$peer_other <= 8) &&
      all(trn_9_imp_df_0$peer_prob >= 0) && all(trn_9_imp_df_0$peer_prob <= 8) &&
      all(trn_9_imp_df_0$path_alc >= 3) && all(trn_9_imp_df_0$path_alc <= 12) &&
      all(trn_9_imp_df_0$path_tob >= 3) && all(trn_9_imp_df_0$path_tob <= 12) &&
      all(trn_9_imp_df_0$path_cb >= 3) && all(trn_9_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_9_imp_df_0$crpf >= 0) && all(trn_9_imp_df_0$crpf <= 15) &&
      all(trn_9_imp_df_0$par_rules >= 3) && all(trn_9_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_9_imp_df_0$mh_density >= 0) && all(trn_9_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_9_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_9_imp_df_0$act_1 >= 0) && all(trn_9_imp_df_0$act_1 <= 7) &&
      all(trn_9_imp_df_0$act_2 >= 0) && all(trn_9_imp_df_0$act_2 <= 7) &&
      all(trn_9_imp_df_0$act_5 >= 0) && all(trn_9_imp_df_0$act_5 <= 5) &&
      all(trn_9_imp_df_0$exp_caf_rec >= 0) && all(trn_9_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_9_imp_df_0$accult_q2_y >= 0) && all(trn_9_imp_df_0$accult_q2_y <= 3) &&
      all(trn_9_imp_df_0$neighborhood_crime_y >= 1) && all(trn_9_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_9_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_9_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_9_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_9_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_9_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_9_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_9_imp_df_0$lmt_acc >= 0) && 
      all(trn_9_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_9_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_9_imp_df_0 <- tst_9_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(tst_9_imp_df_0$peer_alc >= 0) && all(tst_9_imp_df_0$peer_alc <= 8) &&
      all(tst_9_imp_df_0$peer_tob >= 0) && all(tst_9_imp_df_0$peer_tob <= 8) &&
      all(tst_9_imp_df_0$peer_cb >= 0) && all(tst_9_imp_df_0$peer_cb <= 4) &&
      all(tst_9_imp_df_0$peer_other >= 0) && all(tst_9_imp_df_0$peer_other <= 8) &&
      all(tst_9_imp_df_0$peer_prob >= 0) && all(tst_9_imp_df_0$peer_prob <= 8) &&
      all(tst_9_imp_df_0$path_alc >= 3) && all(tst_9_imp_df_0$path_alc <= 12) &&
      all(tst_9_imp_df_0$path_tob >= 3) && all(tst_9_imp_df_0$path_tob <= 12) &&
      all(tst_9_imp_df_0$path_cb >= 3) && all(tst_9_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_9_imp_df_0$crpf >= 0) && all(tst_9_imp_df_0$crpf <= 15) &&
      all(tst_9_imp_df_0$par_rules >= 3) && all(tst_9_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_9_imp_df_0$mh_density >= 0) && all(tst_9_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_9_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_9_imp_df_0$act_1 >= 0) && all(tst_9_imp_df_0$act_1 <= 7) &&
      all(tst_9_imp_df_0$act_2 >= 0) && all(tst_9_imp_df_0$act_2 <= 7) &&
      all(tst_9_imp_df_0$act_5 >= 0) && all(tst_9_imp_df_0$act_5 <= 5) &&
      all(tst_9_imp_df_0$exp_caf_rec >= 0) && all(tst_9_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_9_imp_df_0$accult_q2_y >= 0) && all(tst_9_imp_df_0$accult_q2_y <= 3) &&
      all(tst_9_imp_df_0$neighborhood_crime_y >= 1) && all(tst_9_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_9_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_9_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_9_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_9_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_9_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_9_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_9_imp_df_0$lmt_acc >= 0) && 
      all(tst_9_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_9_imp_df_0, temp)
})

### split (10)
job::job({
  #----------------------------------------------------------------------------#
  #---------------------------- Training --------------------------------------#
  #----------------------------------------------------------------------------#
  trn_10_imp_df_0 <- trn_10_imp_df %>% 
    subset(.imp != 0) 
  
  # Self and Peer Involvement with Substance Use
  if (all(trn_10_imp_df_0$peer_alc >= 0) && all(trn_10_imp_df_0$peer_alc <= 8) &&
      all(trn_10_imp_df_0$peer_tob >= 0) && all(trn_10_imp_df_0$peer_tob <= 8) &&
      all(trn_10_imp_df_0$peer_cb >= 0) && all(trn_10_imp_df_0$peer_cb <= 4) &&
      all(trn_10_imp_df_0$peer_other >= 0) && all(trn_10_imp_df_0$peer_other <= 8) &&
      all(trn_10_imp_df_0$peer_prob >= 0) && all(trn_10_imp_df_0$peer_prob <= 8) &&
      all(trn_10_imp_df_0$path_alc >= 3) && all(trn_10_imp_df_0$path_alc <= 12) &&
      all(trn_10_imp_df_0$path_tob >= 3) && all(trn_10_imp_df_0$path_tob <= 12) &&
      all(trn_10_imp_df_0$path_cb >= 3) && all(trn_10_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(trn_10_imp_df_0$crpf >= 0) && all(trn_10_imp_df_0$crpf <= 15) &&
      all(trn_10_imp_df_0$par_rules >= 3) && all(trn_10_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(trn_10_imp_df_0$mh_density >= 0) && all(trn_10_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- trn_10_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(trn_10_imp_df_0$act_1 >= 0) && all(trn_10_imp_df_0$act_1 <= 7) &&
      all(trn_10_imp_df_0$act_2 >= 0) && all(trn_10_imp_df_0$act_2 <= 7) &&
      all(trn_10_imp_df_0$act_5 >= 0) && all(trn_10_imp_df_0$act_5 <= 5) &&
      all(trn_10_imp_df_0$exp_caf_rec >= 0) && all(trn_10_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(trn_10_imp_df_0$accult_q2_y >= 0) && all(trn_10_imp_df_0$accult_q2_y <= 3) &&
      all(trn_10_imp_df_0$neighborhood_crime_y >= 1) && all(trn_10_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(trn_10_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(trn_10_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(trn_10_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(trn_10_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(trn_10_imp_df_0$pea_ravlt_learn >= -15) && 
      all(trn_10_imp_df_0$pea_ravlt_learn <= 15) &&
      all(trn_10_imp_df_0$lmt_acc >= 0) && 
      all(trn_10_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(trn_10_imp_df_0, temp)
  
  #----------------------------------------------------------------------------#
  #-------------------------------- Test --------------------------------------#
  #----------------------------------------------------------------------------#
  tst_10_imp_df_0 <- tst_10_imp_df %>% 
    subset(.imp != 0) 
  
  # SSelf and Peer Involvement with Substance Use
  if (all(tst_10_imp_df_0$peer_alc >= 0) && all(tst_10_imp_df_0$peer_alc <= 8) &&
      all(tst_10_imp_df_0$peer_tob >= 0) && all(tst_10_imp_df_0$peer_tob <= 8) &&
      all(tst_10_imp_df_0$peer_cb >= 0) && all(tst_10_imp_df_0$peer_cb <= 4) &&
      all(tst_10_imp_df_0$peer_other >= 0) && all(tst_10_imp_df_0$peer_other <= 8) &&
      all(tst_10_imp_df_0$peer_prob >= 0) && all(tst_10_imp_df_0$peer_prob <= 8) &&
      all(tst_10_imp_df_0$path_alc >= 3) && all(tst_10_imp_df_0$path_alc <= 12) &&
      all(tst_10_imp_df_0$path_tob >= 3) && all(tst_10_imp_df_0$path_tob <= 12) &&
      all(tst_10_imp_df_0$path_cb >= 3) && all(tst_10_imp_df_0$path_cb <= 12)) {
    print('All Self and Peer Involvement with Substance Use variables within range')
  } else {
    print('At least 1 Self and Peer Involvement with Substance Use variable outside of range')
  }
  
  # Parenting Behaviors
  if (all(tst_10_imp_df_0$crpf >= 0) && all(tst_10_imp_df_0$crpf <= 15) &&
      all(tst_10_imp_df_0$par_rules >= 3) && all(tst_10_imp_df_0$par_rules <= 18)) {
    print('All Parenting Behavior variables within range')
  } else {
    print('At least 1 Parenting Behavior variable outside of range')
  }
  
  # Mental Health
  if (all(tst_10_imp_df_0$mh_density >= 0) && all(tst_10_imp_df_0$mh_density <= 4)) {
    print('All Mental Health variables within range')
  } else {
    print('At least 1 Mental Health variable outside of range')
  }
  
  # Physical Health
  # subset IDs w/recreational activity
  temp <- tst_10_imp_df_0 %>% 
    subset(rec_bin == 'Yes')
  if (all(temp$rec_con >= 1) && all(temp$rec_con <= 29) &&
      all(tst_10_imp_df_0$act_1 >= 0) && all(tst_10_imp_df_0$act_1 <= 7) &&
      all(tst_10_imp_df_0$act_2 >= 0) && all(tst_10_imp_df_0$act_2 <= 7) &&
      all(tst_10_imp_df_0$act_5 >= 0) && all(tst_10_imp_df_0$act_5 <= 5) &&
      all(tst_10_imp_df_0$exp_caf_rec >= 0) && all(tst_10_imp_df_0$exp_caf_rec <= 3)){
    print('All Physical Health variables within range')
  } else {
    print('At least 1 Physical Health variable outside of range')
  }
  
  # Culture & Environment
  if (all(tst_10_imp_df_0$accult_q2_y >= 0) && all(tst_10_imp_df_0$accult_q2_y <= 3) &&
      all(tst_10_imp_df_0$neighborhood_crime_y >= 1) && all(tst_10_imp_df_0$neighborhood_crime_y <= 5)) {
    print('All Culture & Environment variables within range')
  } else {
    print('At least 1 Culture & Environment variable outside of range')
  }
  
  # Neurocognitive
  if (all(tst_10_imp_df_0$pea_ravlt_sd_trial_vi_tc >= 0) && 
      all(tst_10_imp_df_0$pea_ravlt_sd_trial_vi_tc <= 15) &&
      all(tst_10_imp_df_0$pea_ravlt_sd_trial_vii_tc >= 1) && 
      all(tst_10_imp_df_0$pea_ravlt_sd_trial_vii_tc <= 15) &&
      all(tst_10_imp_df_0$pea_ravlt_learn >= -15) && 
      all(tst_10_imp_df_0$pea_ravlt_learn <= 15) &&
      all(tst_10_imp_df_0$lmt_acc >= 0) && 
      all(tst_10_imp_df_0$lmt_acc <= 1)) {
    print('All Neurocognitive variables within range')
  } else {
    print('At least 1 Neurocognitive variable outside of range')
  }
  
  rm(tst_10_imp_df_0, temp)
  
})

#------------------------------------------------------------------------------#
#  Summary stats averaged across splits & imputations (supplementary materials)
#------------------------------------------------------------------------------#

# subset imputed numeric data
trn_1_imp_df_num <- trn_1_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_2_imp_df_num <- trn_2_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_3_imp_df_num <- trn_3_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_4_imp_df_num <- trn_4_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_5_imp_df_num <- trn_5_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_6_imp_df_num <- trn_6_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_7_imp_df_num <- trn_7_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_8_imp_df_num <- trn_8_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_9_imp_df_num <- trn_9_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
trn_10_imp_df_num <- trn_10_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)

tst_1_imp_df_num <- tst_1_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_2_imp_df_num <- tst_2_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_3_imp_df_num <- tst_3_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_4_imp_df_num <- tst_4_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_5_imp_df_num <- tst_5_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_6_imp_df_num <- tst_6_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_7_imp_df_num <- tst_7_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_8_imp_df_num <- tst_8_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_9_imp_df_num <- tst_9_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)
tst_10_imp_df_num <- tst_10_imp_df %>% 
  subset(.imp != 0) %>% 
  select_if(is.numeric)

# merge across 10 splits
trn_imp_merge_df <- rbind(
  trn_1_imp_df_num, trn_2_imp_df_num, trn_3_imp_df_num, trn_4_imp_df_num,
  trn_5_imp_df_num, trn_6_imp_df_num, trn_7_imp_df_num, trn_8_imp_df_num,
  trn_9_imp_df_num, trn_10_imp_df_num) %>% 
  select(-.imp, -.id, -DV)
# expected obs: 
# 5121 (IDs in test) * 5 (imputations) = 25605 * 10 (splits) = 256050

tst_imp_merge_df <- rbind(
  tst_1_imp_df_num, tst_2_imp_df_num, tst_3_imp_df_num, tst_4_imp_df_num,
  tst_5_imp_df_num, tst_6_imp_df_num, tst_7_imp_df_num, tst_8_imp_df_num,
  tst_9_imp_df_num, tst_10_imp_df_num) %>% 
  select(-.imp, -.id, -DV)
# expected obs: 
# 1708 (IDs in test) * 5 (imputations) = 8540 * 10 (splits) = 85400

rm(trn_1_imp_df_num, trn_2_imp_df_num, trn_3_imp_df_num,
   trn_4_imp_df_num, trn_5_imp_df_num, trn_6_imp_df_num,
   trn_7_imp_df_num, trn_8_imp_df_num, trn_9_imp_df_num,
   trn_10_imp_df_num, 
   tst_1_imp_df_num, tst_2_imp_df_num, tst_3_imp_df_num,
   tst_4_imp_df_num, tst_5_imp_df_num, tst_6_imp_df_num,
   tst_7_imp_df_num, tst_8_imp_df_num, tst_9_imp_df_num,
   tst_10_imp_df_num)

# get summary stats
trn_imp_merge_df <- get_summary_stats(trn_imp_merge_df, type = "common") %>% 
  select(variable, n, mean, sd, min, max)
tst_imp_merge_df <- get_summary_stats(tst_imp_merge_df, type = "common") %>% 
  select(variable, n, mean, sd, min, max)

# add table names
trn_imp_merge_df <- trn_imp_merge_df %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)
tst_imp_merge_df <- tst_imp_merge_df %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)

# add headers
temp_demo_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Demographics') %>% 
  add_row(table_name = 'Demographics', .before = 1) 
temp_demo_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Demographics') %>% 
  add_row(table_name = 'Demographics', .before = 1) 

temp_su_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Self and Peer Involvement with Substance Use') %>% 
  add_row(table_name = 'Self and Peer Involvement with Substance Use', 
          .before = 1) 
temp_su_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Self and Peer Involvement with Substance Use') %>% 
  add_row(table_name = 'Self and Peer Involvement with Substance Use', 
          .before = 1) 

temp_pb_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Parenting Behaviors') %>% 
  add_row(table_name = 'Parenting Behaviors', .before = 1) 
temp_pb_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Parenting Behaviors') %>% 
  add_row(table_name = 'Parenting Behaviors', .before = 1) 

temp_mh_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Mental Health') %>% 
  add_row(table_name = 'Mental Health', .before = 1) 
temp_mh_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Mental Health') %>% 
  add_row(table_name = 'Mental Health', .before = 1) 

temp_ph_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Physical Health') %>% 
  add_row(table_name = 'Physical Health', .before = 1) 
temp_ph_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Physical Health') %>% 
  add_row(table_name = 'Physical Health', .before = 1) 

temp_ce_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Culture & Environment') %>% 
  add_row(table_name = 'Culture & Environment', .before = 1) 
temp_ce_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Culture & Environment') %>% 
  add_row(table_name = 'Culture & Environment', .before = 1) 

temp_hor_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Hormones') %>% 
  add_row(table_name = 'Hormones', .before = 1) 
temp_hor_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Hormones') %>% 
  add_row(table_name = 'Hormones', .before = 1) 

temp_nc_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Neurocognitive Factors') %>% 
  add_row(table_name = 'Neurocognitive Factors', .before = 1) 
temp_nc_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Neurocognitive Factors') %>% 
  add_row(table_name = 'Neurocognitive Factors', .before = 1) 

temp_smri_area_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_area')) %>% 
  add_row(table_name = 'sMRI: Area', .before = 1) 
temp_smri_area_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_area')) %>% 
  add_row(table_name = 'sMRI: Area', .before = 1) 

temp_smri_vol_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_vol')) %>% 
  add_row(table_name = 'sMRI: Volume', .before = 1) 
temp_smri_vol_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_vol')) %>% 
  add_row(table_name = 'sMRI: Volume', .before = 1) 

temp_smri_sulc_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_sulc')) %>% 
  add_row(table_name = 'sMRI: Sulcul Depth', .before = 1) 
temp_smri_sulc_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_sulc')) %>% 
  add_row(table_name = 'sMRI: Sulcul Depth', .before = 1) 

temp_smri_thick_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_thick')) %>% 
  add_row(table_name = 'sMRI: Cortical Thickness', .before = 1) 
temp_smri_thick_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_thick')) %>% 
  add_row(table_name = 'sMRI: Cortical Thickness', .before = 1) 

temp_dti_trn <- trn_imp_merge_df %>% 
  subset(domain_name == 'Diffusion Tensor Imaging (DTI)') %>% 
  add_row(table_name = 'DTI: FA', .before = 1) 
temp_dti_tst <- tst_imp_merge_df %>% 
  subset(domain_name == 'Diffusion Tensor Imaging (DTI)') %>% 
  add_row(table_name = 'DTI: FA', .before = 1) 

trn_imp_merge_df <- rbind(
  temp_demo_trn, temp_su_trn, temp_pb_trn, temp_mh_trn, temp_ph_trn, 
  temp_ce_trn, temp_hor_trn, temp_nc_trn, temp_smri_area_trn, temp_smri_vol_trn, 
  temp_smri_sulc_trn, temp_smri_thick_trn, temp_dti_trn) %>% 
  select(-domain_name) 

tst_imp_merge_df <- rbind(
  temp_demo_tst, temp_su_tst, temp_pb_tst, temp_mh_tst, temp_ph_tst, 
  temp_ce_tst, temp_hor_tst, temp_nc_tst, temp_smri_area_tst, temp_smri_vol_tst, 
  temp_smri_sulc_tst, temp_smri_thick_tst, temp_dti_tst) %>% 
  select(-domain_name) 

rm(temp_demo_trn, temp_su_trn, temp_pb_trn, temp_mh_trn, temp_ph_trn, 
   temp_ce_trn, temp_hor_trn, temp_nc_trn, temp_smri_area_trn, 
   temp_smri_vol_trn, temp_smri_sulc_trn, temp_smri_thick_trn, temp_dti_trn,
   
   temp_demo_tst, temp_su_tst, temp_pb_tst, temp_mh_tst, temp_ph_tst, 
   temp_ce_tst, temp_hor_tst, temp_nc_tst, temp_smri_area_tst, 
   temp_smri_vol_tst, temp_smri_sulc_tst, temp_smri_thick_tst, temp_dti_tst)

# merge with numeric variables in observed dataset (obs_num)
trn_imp_merge_df <- trn_imp_merge_df %>% 
  rename(
    n_trn = n,
    mean_trn = mean,
    sd_trn = sd,
    min_trn = min,
    max_trn = max)

tst_imp_merge_df <- tst_imp_merge_df %>% 
  rename(
    n_tst = n,
    mean_tst = mean,
    sd_tst = sd,
    min_tst = min,
    max_tst = max)

obs_trn_tst <- obs_num %>% 
  left_join(trn_imp_merge_df, by = c('table_name', 'variable')) %>% 
  left_join(tst_imp_merge_df, by = c('table_name', 'variable')) 

# export
write.csv(obs_trn_tst,'output/model_dataset_prep/supplement/obs_trn_tst.csv')  

# remove objects no longer needed
rm(trn_imp_merge_df, tst_imp_merge_df, obs_num, obs_trn_tst)

# Center & Scale ----------------------------------------------------------

### split (1)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_1_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_1_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separately with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_1_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_1_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_1_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_1_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  
  # expected output
  # - input: 1 outcome and 389 predictors (386 variables w/o dummy codes + .id, .imp, src_subject_id)
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_1_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_1_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_1_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_1_imp_df_cs_final$.imp >= 0) && 
      all(tst_1_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_1_cs_dc <- trn_1_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_1_cs_dc <- tst_1_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_1_cs_dc <- get_summary_stats(trn_1_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_1_cs_dc <- get_summary_stats(tst_1_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_1_cs_dc$mean >= -0.10) && all(trn_1_cs_dc$mean <= 0.10) &&
      all(tst_1_cs_dc$mean >= -0.10) && all(tst_1_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_1_cs_dc$sd >= -1.05) && all(trn_1_cs_dc$sd <= 1.05) &&
      all(tst_1_cs_dc$sd >= -1.05) && all(tst_1_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (2)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_2_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_2_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_2_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_2_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_2_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_2_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_2_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_2_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_2_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_2_imp_df_cs_final$.imp >= 0) && 
      all(tst_2_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_2_cs_dc <- trn_2_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_2_cs_dc <- tst_2_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_2_cs_dc <- get_summary_stats(trn_2_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_2_cs_dc <- get_summary_stats(tst_2_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_2_cs_dc$mean >= -0.10) && all(trn_2_cs_dc$mean <= 0.10) &&
      all(tst_2_cs_dc$mean >= -0.10) && all(tst_2_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_2_cs_dc$sd >= -1.05) && all(trn_2_cs_dc$sd <= 1.05) &&
      all(tst_2_cs_dc$sd >= -1.05) && all(tst_2_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (3)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_3_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_3_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_3_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_3_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_3_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_3_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_3_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_3_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_3_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_3_imp_df_cs_final$.imp >= 0) && 
      all(tst_3_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_3_cs_dc <- trn_3_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_3_cs_dc <- tst_3_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_3_cs_dc <- get_summary_stats(trn_3_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_3_cs_dc <- get_summary_stats(tst_3_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_3_cs_dc$mean >= -0.10) && all(trn_3_cs_dc$mean <= 0.10) &&
      all(tst_3_cs_dc$mean >= -0.10) && all(tst_3_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_3_cs_dc$sd >= -1.05) && all(trn_3_cs_dc$sd <= 1.05) &&
      all(tst_3_cs_dc$sd >= -1.05) && all(tst_3_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (4)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_4_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_4_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_4_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_4_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_4_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_4_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_4_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_4_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_4_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_4_imp_df_cs_final$.imp >= 0) && 
      all(tst_4_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_4_cs_dc <- trn_4_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_4_cs_dc <- tst_4_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_4_cs_dc <- get_summary_stats(trn_4_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_4_cs_dc <- get_summary_stats(tst_4_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_4_cs_dc$mean >= -0.10) && all(trn_4_cs_dc$mean <= 0.10) &&
      all(tst_4_cs_dc$mean >= -0.10) && all(tst_4_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_4_cs_dc$sd >= -1.05) && all(trn_4_cs_dc$sd <= 1.05) &&
      all(tst_4_cs_dc$sd >= -1.05) && all(tst_4_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (5)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_5_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_5_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_5_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_5_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_5_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_5_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_5_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_5_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_5_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_5_imp_df_cs_final$.imp >= 0) && 
      all(tst_5_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_5_cs_dc <- trn_5_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_5_cs_dc <- tst_5_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_5_cs_dc <- get_summary_stats(trn_5_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_5_cs_dc <- get_summary_stats(tst_5_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_5_cs_dc$mean >= -0.10) && all(trn_5_cs_dc$mean <= 0.10) &&
      all(tst_5_cs_dc$mean >= -0.10) && all(tst_5_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_5_cs_dc$sd >= -1.05) && all(trn_5_cs_dc$sd <= 1.05) &&
      all(tst_5_cs_dc$sd >= -1.05) && all(tst_5_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (6)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_6_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_6_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_6_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_6_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_6_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_6_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_6_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_6_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_6_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_6_imp_df_cs_final$.imp >= 0) && 
      all(tst_6_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_6_cs_dc <- trn_6_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_6_cs_dc <- tst_6_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_6_cs_dc <- get_summary_stats(trn_6_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_6_cs_dc <- get_summary_stats(tst_6_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_6_cs_dc$mean >= -0.10) && all(trn_6_cs_dc$mean <= 0.10) &&
      all(tst_6_cs_dc$mean >= -0.10) && all(tst_6_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_6_cs_dc$sd >= -1.05) && all(trn_6_cs_dc$sd <= 1.05) &&
      all(tst_6_cs_dc$sd >= -1.05) && all(tst_6_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (7)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_7_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_7_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_7_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_7_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_7_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_7_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_7_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_7_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_7_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_7_imp_df_cs_final$.imp >= 0) && 
      all(tst_7_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_7_cs_dc <- trn_7_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_7_cs_dc <- tst_7_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_7_cs_dc <- get_summary_stats(trn_7_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_7_cs_dc <- get_summary_stats(tst_7_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_7_cs_dc$mean >= -0.10) && all(trn_7_cs_dc$mean <= 0.10) &&
      all(tst_7_cs_dc$mean >= -0.10) && all(tst_7_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_7_cs_dc$sd >= -1.05) && all(trn_7_cs_dc$sd <= 1.05) &&
      all(tst_7_cs_dc$sd >= -1.05) && all(tst_7_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (8)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_8_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_8_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_8_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_8_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_8_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_8_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_8_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_8_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_8_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_8_imp_df_cs_final$.imp >= 0) && 
      all(tst_8_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_8_cs_dc <- trn_8_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_8_cs_dc <- tst_8_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_8_cs_dc <- get_summary_stats(trn_8_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_8_cs_dc <- get_summary_stats(tst_8_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_8_cs_dc$mean >= -0.10) && all(trn_8_cs_dc$mean <= 0.10) &&
      all(tst_8_cs_dc$mean >= -0.10) && all(tst_8_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_8_cs_dc$sd >= -1.05) && all(trn_8_cs_dc$sd <= 1.05) &&
      all(tst_8_cs_dc$sd >= -1.05) && all(tst_8_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (9)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_9_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_9_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_9_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_9_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_9_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_9_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_9_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_9_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_9_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_9_imp_df_cs_final$.imp >= 0) && 
      all(tst_9_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_9_cs_dc <- trn_9_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_9_cs_dc <- tst_9_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_9_cs_dc <- get_summary_stats(trn_9_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_9_cs_dc <- get_summary_stats(tst_9_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_9_cs_dc$mean >= -0.10) && all(trn_9_cs_dc$mean <= 0.10) &&
      all(tst_9_cs_dc$mean >= -0.10) && all(tst_9_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_9_cs_dc$sd >= -1.05) && all(trn_9_cs_dc$sd <= 1.05) &&
      all(tst_9_cs_dc$sd >= -1.05) && all(tst_9_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

### split (10)
job::job({
  
  # (1) 
  # - store DV as factor for center and scale process
  # - remove observed dataset from informing center and scale of imputed dataset
  trn_imp_df_cs <- trn_10_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  tst_imp_df_cs <- tst_10_imp_df %>% 
    subset(.imp != 0) %>% # remove original dataset
    mutate(DV = factor(DV, 
                       levels = c(0, 1),
                       labels = c('No', 'Yes')))
  
  if (is.factor(trn_imp_df_cs$DV) && is.factor(tst_imp_df_cs$DV)) {
    print('Pre-CS: DV saved as factor')
  } else {
    print('Pre-CS: DV NOT saved as factor')
  }  
  
  # (2) 
  # - store observed dataset separartely with .imp & .id to merge w/imputed data
  
  trn_imp_df_imp0 <- trn_10_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  tst_imp_df_imp0 <- tst_10_imp_df %>% 
    subset(.imp == 0) %>% 
    select(-src_subject_id) # no longer needed, replaced with .id from MICE 
  
  if (all(trn_imp_df_imp0$.imp == 0) && all(tst_imp_df_imp0$.imp == 0)) {
    print('Pre-CS: Observed dataset stored separately')
  } else {
    print('Pre-CS: Observed dataset NOT stored separately')
  }  
  
  # (3) 
  # - store 5 imputations with .id, .imp, and pds to add back to dataset 
  #   after center & scale
  # - pds is a z-score therefore no need to center and scale
  
  trn_imp_df_imp15 <- trn_10_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  tst_imp_df_imp15 <- tst_10_imp_df %>% 
    subset(.imp != 0) %>% 
    select(.imp, .id, pds)
  
  if (all(trn_imp_df_imp15$.imp != 0) && all(tst_imp_df_imp15$.imp != 0)) {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores stored separately')
  } else {
    print('Pre-CS: Imputed datasets & pre-calculated z-scores NOT stored separately')
  }  
  
  # (4) 
  # - build data processing recipe (from training dataset)
  recipe_prep <- trn_imp_df_cs %>% 
    recipe(DV ~ .) %>% 
    step_rm(ends_with('.id') | ends_with('.imp') | src_subject_id | pds) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    prep()
  recipe_prep 
  # expected output
  # - input: 1 outcome and 389 predictors 
  # - variables removed (n = 4): .id, .imp, src_subject_id, pds
  #   - 389 - 4 = 385 predictors as expected 
  #   - remove pds since it is already a z-score
  # - centering and scaling for: all numeric predictors listed 
  
  # double check means and SD calculated by step_normalize are as expected
  temp <- trn_10_imp_df %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-DV, -.imp, -.id, -pds)
  
  if (all(
    (identical(recipe_prep[["steps"]][[2]][["means"]], colMeans(temp)) == TRUE) &
    (identical(recipe_prep[["steps"]][[2]][["sds"]], sapply(temp, sd)) == TRUE))) {
    print('Post-CS: Means and SD to inform center and scaling as expected')
  } else {
    print('Post-CS: Means and SD to inform center and scaling NOT as expected')
  }  
  
  # (5) 
  # - apply preprocessing recipe to dataset and extract pre-processed data 
  
  trn_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = trn_imp_df_cs) 
  
  tst_imp_df_cs_2 <- recipe_prep %>%
    bake(new_data = tst_imp_df_cs)
  
  rm(recipe_prep)
  
  # (6) 
  # - re-code DV as a numeric variable for elastic net 
  
  trn_imp_df_cs_2 <- trn_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  tst_imp_df_cs_2 <- tst_imp_df_cs_2 %>% 
    mutate(DV_numeric = ifelse(DV == 'Yes', 1, 0)) %>% 
    select(-DV) %>% 
    rename(DV = DV_numeric) %>% 
    relocate(DV)
  
  if (is.numeric(trn_imp_df_cs_2$DV) && is.numeric(tst_imp_df_cs_2$DV)) {
    print('Post-CS: DV saved as numeric')
  } else {
    print('Post-CS: DV NOT saved as numeric')
  }  
  
  # (7) 
  # - add back in .id, .imp, and pds to create MICE object for elastic net
  trn_imp_df_cs_3 <- cbind(trn_imp_df_imp15, trn_imp_df_cs_2)
  tst_imp_df_cs_3 <- cbind(tst_imp_df_imp15, tst_imp_df_cs_2)
  
  # - add back in observed data to create MICE object for elastic net
  trn_10_imp_df_cs_final <- rbind(trn_imp_df_imp0, trn_imp_df_cs_3)
  tst_10_imp_df_cs_final <- rbind(tst_imp_df_imp0, tst_imp_df_cs_3)
  
  if (all(trn_10_imp_df_cs_final$.imp >= 0) && 
      all(tst_10_imp_df_cs_final$.imp >= 0)) {
    print('Post-CS: Final center & scale dataset complete')
  } else {
    print('Post-CS: Final center & scale dataset NOT complete')
  }  
  
  # double check mean and sd are 0 and 1
  trn_10_cs_dc <- trn_10_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  tst_10_cs_dc <- tst_10_imp_df_cs_final %>% 
    subset(.imp != 0) %>% 
    select_if(is.numeric) %>% 
    select(-.imp, -.id, -DV)
  
  trn_10_cs_dc <- get_summary_stats(trn_10_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  tst_10_cs_dc <- get_summary_stats(tst_10_cs_dc, type = "common") %>% 
    select(variable, mean, sd)
  
  if (all(trn_10_cs_dc$mean >= -0.10) && all(trn_10_cs_dc$mean <= 0.10) &&
      all(tst_10_cs_dc$mean >= -0.10) && all(tst_10_cs_dc$mean <= 0.10)) {
    print('Post-CS: Mean double check complete')
  } else {
    print('Post-CS: Mean double check NOT complete')
  }  
  
  if (all(trn_10_cs_dc$sd >= -1.05) && all(trn_10_cs_dc$sd <= 1.05) &&
      all(tst_10_cs_dc$sd >= -1.05) && all(tst_10_cs_dc$sd <= 1.05)) {
    print('Post-CS: SD double check complete')
  } else {
    print('Post-CS: SD double check NOT complete')
  }  
  
  # remove objects no longer needed
  rm(trn_imp_df_cs, trn_imp_df_cs_2, trn_imp_df_cs_3,
     tst_imp_df_cs, tst_imp_df_cs_2, tst_imp_df_cs_3,
     trn_imp_df_imp0, trn_imp_df_imp15,
     tst_imp_df_imp0, tst_imp_df_imp15, temp)
  
})

# follow-up on discrepancies: 

## training
cat(range(trn_1_cs_dc$mean), ' = Training: Split 1 range of mean values across predictors  \n')
cat(range(trn_2_cs_dc$mean), ' = Training: Split 2 range of mean values across predictors  \n')
cat(range(trn_3_cs_dc$mean), ' = Training: Split 3 range of mean values across predictors  \n')
cat(range(trn_4_cs_dc$mean), ' = Training: Split 4 range of mean values across predictors  \n')
cat(range(trn_5_cs_dc$mean), ' = Training: Split 5 range of mean values across predictors  \n')
cat(range(trn_6_cs_dc$mean), ' = Training: Split 6 range of mean values across predictors  \n')
cat(range(trn_7_cs_dc$mean), ' = Training: Split 7 range of mean values across predictors  \n')
cat(range(trn_8_cs_dc$mean), ' = Training: Split 8 range of mean values across predictors  \n')
cat(range(trn_9_cs_dc$mean), ' = Training: Split 9 range of mean values across predictors  \n')
cat(range(trn_10_cs_dc$mean), ' = Training: Split 10 range of mean values across predictors  \n')

cat(range(trn_1_cs_dc$sd), ' = Training: Split 1 range of sd values across predictors  \n')
cat(range(trn_2_cs_dc$sd), ' = Training: Split 2 range of sd values across predictors  \n')
cat(range(trn_3_cs_dc$sd), ' = Training: Split 3 range of sd values across predictors  \n')
cat(range(trn_4_cs_dc$sd), ' = Training: Split 4 range of sd values across predictors  \n')
cat(range(trn_5_cs_dc$sd), ' = Training: Split 5 range of sd values across predictors  \n')
cat(range(trn_6_cs_dc$sd), ' = Training: Split 6 range of sd values across predictors  \n')
cat(range(trn_7_cs_dc$sd), ' = Training: Split 7 range of sd values across predictors  \n')
cat(range(trn_8_cs_dc$sd), ' = Training: Split 8 range of sd values across predictors  \n')
cat(range(trn_9_cs_dc$sd), ' = Training: Split 9 range of sd values across predictors  \n')
cat(range(trn_10_cs_dc$sd), ' = Training: Split 10 range of sd values across predictors  \n')

## test
cat(range(tst_1_cs_dc$mean), ' = Test: Split 1 range of mean values across predictors  \n')
cat(range(tst_2_cs_dc$mean), ' = Test: Split 2 range of mean values across predictors  \n')
cat(range(tst_3_cs_dc$mean), ' = Test: Split 3 range of mean values across predictors  \n')
cat(range(tst_4_cs_dc$mean), ' = Test: Split 4 range of mean values across predictors  \n')
cat(range(tst_5_cs_dc$mean), ' = Test: Split 5 range of mean values across predictors  \n')
cat(range(tst_6_cs_dc$mean), ' = Test: Split 6 range of mean values across predictors  \n')
cat(range(tst_7_cs_dc$mean), ' = Test: Split 7 range of mean values across predictors  \n')
cat(range(tst_8_cs_dc$mean), ' = Test: Split 8 range of mean values across predictors  \n')
cat(range(tst_9_cs_dc$mean), ' = Test: Split 9 range of mean values across predictors  \n')
cat(range(tst_10_cs_dc$mean), ' = Test: Split 10 range of mean values across predictors  \n')

cat(range(tst_1_cs_dc$sd), ' = Test: Split 1 range of sd values across predictors  \n')
cat(range(tst_2_cs_dc$sd), ' = Test: Split 2 range of sd values across predictors  \n')
cat(range(tst_3_cs_dc$sd), ' = Test: Split 3 range of sd values across predictors  \n')
cat(range(tst_4_cs_dc$sd), ' = Test: Split 4 range of sd values across predictors  \n')
cat(range(tst_5_cs_dc$sd), ' = Test: Split 5 range of sd values across predictors  \n')
cat(range(tst_6_cs_dc$sd), ' = Test: Split 6 range of sd values across predictors  \n')
cat(range(tst_7_cs_dc$sd), ' = Test: Split 7 range of sd values across predictors  \n')
cat(range(tst_8_cs_dc$sd), ' = Test: Split 8 range of sd values across predictors  \n')
cat(range(tst_9_cs_dc$sd), ' = Test: Split 9 range of sd values across predictors  \n')
cat(range(tst_10_cs_dc$sd), ' = Test: Split 10 range of sd values across predictors  \n')

# all discrepancies are minor and within reason for training and test

# drop objects no longer needed
rm(trn_1_cs_dc, tst_1_cs_dc, trn_2_cs_dc, tst_2_cs_dc,
   trn_3_cs_dc, tst_3_cs_dc, trn_4_cs_dc, tst_4_cs_dc,
   trn_5_cs_dc, tst_5_cs_dc, trn_6_cs_dc, tst_6_cs_dc,
   trn_7_cs_dc, tst_7_cs_dc, trn_8_cs_dc, tst_8_cs_dc,
   trn_9_cs_dc, tst_9_cs_dc, trn_10_cs_dc, tst_10_cs_dc,
   
   trn_1_imp_df, trn_2_imp_df, trn_3_imp_df, trn_4_imp_df, trn_5_imp_df, 
   trn_6_imp_df, trn_7_imp_df, trn_8_imp_df, trn_9_imp_df, trn_10_imp_df, 
   tst_1_imp_df, tst_2_imp_df, tst_3_imp_df, tst_4_imp_df, tst_5_imp_df, 
   tst_6_imp_df, tst_7_imp_df, tst_8_imp_df, tst_9_imp_df, tst_10_imp_df)

# retained objects:
# - trn_1_imp_df_cs_final - trn_10_imp_df_cs_final & 
#   tst_1_imp_df_cs_final - tst_10_imp_df_cs_final: df w/observed & imputed 
#   center and scaled data across 10 splits


# Dummy Codes -------------------------------------------------------------

# - based on largest group in observed sample for income, religion, & parent edu
table(data.2$income, useNA = 'ifany') # largest group: inc_9
table(data.2$religion, useNA = 'ifany') # largest group: rp_17
table(data.2$p_edu, useNA = 'ifany') # largest group: Bachelors_Degree

# Demographics
# (1) race: 
# - reference group: white
# - # of categories: 4
# - # of dummy codes: 3

# (2) ethnicity: 
# - reference group: non-Hispanic
# - # of categories: 2
# - # of dummy codes: 1

# (3) income: 
# - reference group: inc_9
# - # of categories: 10
# - # of dummy codes: 9

# (4) religion: 
# - reference group: rp_17
# - # of categories: 17
# - # of dummy codes: 16

# (5) parent education: 
# - reference group: Bachelor's Degree
# - # of categories: 6
# - # of dummy codes: 5

# (6) sex: 
# - reference group: male
# - # of categories: 2
# - # of dummy codes: 1

# Physical Health
# (7) recreational activities (binary): 
# - reference group: no
# - # of categories: 2
# - # of dummy codes: 1

# (8) prenatal exposure to substances: 
# - reference group: no
# - # of categories: 2
# - # of dummy codes: 1

# (9) history of head injury: 
# - reference group: no
# - # of categories: 2
# - # of dummy codes: 1

# Culture & Environment
# (10) grades: 
# - reference group: average grade A
# - # of categories: 4
# - # of dummy codes: 3

# (11) detention / suspension: 
# - reference group: no
# - # of categories: 2
# - # of dummy codes: 1

# (12) special education services: 
# - reference group: none
# - # of categories: 5
# - # of dummy codes: 4

# Neurocog
# (13) CCT: 
# - reference group: delayed
# - # of categories: 2
# - # of dummy codes: 1

### split (1)
job::job({

  # subset factor variables
  trn_1_factor <- trn_1_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_1_factor <- tst_1_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_1_factor_dummy <- fastDummies::dummy_cols(trn_1_factor) 
  tst_1_factor_dummy <- fastDummies::dummy_cols(tst_1_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_1_factor_dummy <- trn_1_factor_dummy %>% 
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
  
  tst_1_factor_dummy <- tst_1_factor_dummy %>% 
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
  trn_1_imp_df_cs_final <- trn_1_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_1_imp_df_cs_final <- tst_1_imp_df_cs_final %>% 
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
  trn_1_imp_df_cs_final <- cbind(trn_1_imp_df_cs_final, trn_1_factor_dummy)
  tst_1_imp_df_cs_final <- cbind(tst_1_imp_df_cs_final, tst_1_factor_dummy)
  
  cat(length(trn_1_imp_df_cs_final), 'variables in Split (1) training including dummy codes \n') 
  cat(length(tst_1_imp_df_cs_final), 'variables in Split (1) test including dummy codes \n') 
  
  rm(trn_1_factor, trn_1_factor_dummy, tst_1_factor, tst_1_factor_dummy,
     trn_1_imp_df, tst_1_imp_df)
  
})

### split (2)
job::job({
  
  # subset factor variables
  trn_2_factor <- trn_2_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_2_factor <- tst_2_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_2_factor_dummy <- fastDummies::dummy_cols(trn_2_factor) 
  tst_2_factor_dummy <- fastDummies::dummy_cols(tst_2_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_2_factor_dummy <- trn_2_factor_dummy %>% 
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
  
  tst_2_factor_dummy <- tst_2_factor_dummy %>% 
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
  trn_2_imp_df_cs_final <- trn_2_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_2_imp_df_cs_final <- tst_2_imp_df_cs_final %>% 
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
  trn_2_imp_df_cs_final <- cbind(trn_2_imp_df_cs_final, trn_2_factor_dummy)
  tst_2_imp_df_cs_final <- cbind(tst_2_imp_df_cs_final, tst_2_factor_dummy)
  
  cat(length(trn_2_imp_df_cs_final), 'variables in Split (2) training including dummy codes \n') 
  cat(length(tst_2_imp_df_cs_final), 'variables in Split (2) test including dummy codes \n') 
  
  rm(trn_2_factor, trn_2_factor_dummy, tst_2_factor, tst_2_factor_dummy,
     trn_2_imp_df, tst_2_imp_df)
  
  
})

### split (3)
job::job({
  # subset factor variables
  trn_3_factor <- trn_3_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_3_factor <- tst_3_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_3_factor_dummy <- fastDummies::dummy_cols(trn_3_factor) 
  tst_3_factor_dummy <- fastDummies::dummy_cols(tst_3_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_3_factor_dummy <- trn_3_factor_dummy %>% 
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
  
  tst_3_factor_dummy <- tst_3_factor_dummy %>% 
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
  trn_3_imp_df_cs_final <- trn_3_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_3_imp_df_cs_final <- tst_3_imp_df_cs_final %>% 
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
  trn_3_imp_df_cs_final <- cbind(trn_3_imp_df_cs_final, trn_3_factor_dummy)
  tst_3_imp_df_cs_final <- cbind(tst_3_imp_df_cs_final, tst_3_factor_dummy)
  
  cat(length(trn_3_imp_df_cs_final), 'variables in Split (3) training including dummy codes \n') 
  cat(length(tst_3_imp_df_cs_final), 'variables in Split (3) test including dummy codes \n') 
  
  rm(trn_3_factor, trn_3_factor_dummy, tst_3_factor, tst_3_factor_dummy,
     trn_3_imp_df, tst_3_imp_df)
  
})

### split (4)
job::job({
  # subset factor variables
  trn_4_factor <- trn_4_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_4_factor <- tst_4_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_4_factor_dummy <- fastDummies::dummy_cols(trn_4_factor) 
  tst_4_factor_dummy <- fastDummies::dummy_cols(tst_4_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_4_factor_dummy <- trn_4_factor_dummy %>% 
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
  
  tst_4_factor_dummy <- tst_4_factor_dummy %>% 
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
  trn_4_imp_df_cs_final <- trn_4_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_4_imp_df_cs_final <- tst_4_imp_df_cs_final %>% 
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
  trn_4_imp_df_cs_final <- cbind(trn_4_imp_df_cs_final, trn_4_factor_dummy)
  tst_4_imp_df_cs_final <- cbind(tst_4_imp_df_cs_final, tst_4_factor_dummy)
  
  cat(length(trn_4_imp_df_cs_final), 'variables in Split (4) training including dummy codes \n') 
  cat(length(tst_4_imp_df_cs_final), 'variables in Split (4) test including dummy codes \n') 
  
  rm(trn_4_factor, trn_4_factor_dummy, tst_4_factor, tst_4_factor_dummy,
     trn_4_imp_df, tst_4_imp_df)
  
})

### split (5)
job::job({
  
  # subset factor variables
  trn_5_factor <- trn_5_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_5_factor <- tst_5_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_5_factor_dummy <- fastDummies::dummy_cols(trn_5_factor) 
  tst_5_factor_dummy <- fastDummies::dummy_cols(tst_5_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_5_factor_dummy <- trn_5_factor_dummy %>% 
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
  
  tst_5_factor_dummy <- tst_5_factor_dummy %>% 
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
  trn_5_imp_df_cs_final <- trn_5_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_5_imp_df_cs_final <- tst_5_imp_df_cs_final %>% 
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
  trn_5_imp_df_cs_final <- cbind(trn_5_imp_df_cs_final, trn_5_factor_dummy)
  tst_5_imp_df_cs_final <- cbind(tst_5_imp_df_cs_final, tst_5_factor_dummy)
  
  cat(length(trn_5_imp_df_cs_final), 'variables in Split (5) training including dummy codes \n') 
  cat(length(tst_5_imp_df_cs_final), 'variables in Split (5) test including dummy codes \n') 
  
  rm(trn_5_factor, trn_5_factor_dummy, tst_5_factor, tst_5_factor_dummy,
     trn_5_imp_df, tst_5_imp_df)
  
})

### split (6)
job::job({
  
  # subset factor variables
  trn_6_factor <- trn_6_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_6_factor <- tst_6_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_6_factor_dummy <- fastDummies::dummy_cols(trn_6_factor) 
  tst_6_factor_dummy <- fastDummies::dummy_cols(tst_6_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_6_factor_dummy <- trn_6_factor_dummy %>% 
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
  
  tst_6_factor_dummy <- tst_6_factor_dummy %>% 
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
  trn_6_imp_df_cs_final <- trn_6_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_6_imp_df_cs_final <- tst_6_imp_df_cs_final %>% 
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
  trn_6_imp_df_cs_final <- cbind(trn_6_imp_df_cs_final, trn_6_factor_dummy)
  tst_6_imp_df_cs_final <- cbind(tst_6_imp_df_cs_final, tst_6_factor_dummy)

  cat(length(trn_6_imp_df_cs_final), 'variables in Split (6) training including dummy codes \n') 
  cat(length(tst_6_imp_df_cs_final), 'variables in Split (6) test including dummy codes \n') 
  
  rm(trn_6_factor, trn_6_factor_dummy, tst_6_factor, tst_6_factor_dummy,
     trn_6_imp_df, tst_6_imp_df)
  
})

### split (7)
job::job({
  
  # subset factor variables
  trn_7_factor <- trn_7_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_7_factor <- tst_7_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_7_factor_dummy <- fastDummies::dummy_cols(trn_7_factor) 
  tst_7_factor_dummy <- fastDummies::dummy_cols(tst_7_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_7_factor_dummy <- trn_7_factor_dummy %>% 
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
  
  tst_7_factor_dummy <- tst_7_factor_dummy %>% 
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
  trn_7_imp_df_cs_final <- trn_7_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_7_imp_df_cs_final <- tst_7_imp_df_cs_final %>% 
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
  trn_7_imp_df_cs_final <- cbind(trn_7_imp_df_cs_final, trn_7_factor_dummy)
  tst_7_imp_df_cs_final <- cbind(tst_7_imp_df_cs_final, tst_7_factor_dummy)
  
  cat(length(trn_7_imp_df_cs_final), 'variables in Split (7) training including dummy codes \n') 
  cat(length(tst_7_imp_df_cs_final), 'variables in Split (7) test including dummy codes \n') 
  
  rm(trn_7_factor, trn_7_factor_dummy, tst_7_factor, tst_7_factor_dummy,
     trn_7_imp_df, tst_7_imp_df)
  
})

### split (8)
job::job({
  
  # subset factor variables
  trn_8_factor <- trn_8_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_8_factor <- tst_8_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_8_factor_dummy <- fastDummies::dummy_cols(trn_8_factor) 
  tst_8_factor_dummy <- fastDummies::dummy_cols(tst_8_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_8_factor_dummy <- trn_8_factor_dummy %>% 
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
  
  tst_8_factor_dummy <- tst_8_factor_dummy %>% 
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
  trn_8_imp_df_cs_final <- trn_8_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_8_imp_df_cs_final <- tst_8_imp_df_cs_final %>% 
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
  trn_8_imp_df_cs_final <- cbind(trn_8_imp_df_cs_final, trn_8_factor_dummy)
  tst_8_imp_df_cs_final <- cbind(tst_8_imp_df_cs_final, tst_8_factor_dummy)
  
  cat(length(trn_8_imp_df_cs_final), 'variables in Split (8) training including dummy codes \n') 
  cat(length(tst_8_imp_df_cs_final), 'variables in Split (8) test including dummy codes \n') 
  
  rm(trn_8_factor, trn_8_factor_dummy, tst_8_factor, tst_8_factor_dummy,
     trn_8_imp_df, tst_8_imp_df)
  
})

### split (9)
job::job({
  
  # subset factor variables
  trn_9_factor <- trn_9_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_9_factor <- tst_9_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_9_factor_dummy <- fastDummies::dummy_cols(trn_9_factor) 
  tst_9_factor_dummy <- fastDummies::dummy_cols(tst_9_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_9_factor_dummy <- trn_9_factor_dummy %>% 
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
  
  tst_9_factor_dummy <- tst_9_factor_dummy %>% 
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
  trn_9_imp_df_cs_final <- trn_9_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_9_imp_df_cs_final <- tst_9_imp_df_cs_final %>% 
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
  trn_9_imp_df_cs_final <- cbind(trn_9_imp_df_cs_final, trn_9_factor_dummy)
  tst_9_imp_df_cs_final <- cbind(tst_9_imp_df_cs_final, tst_9_factor_dummy)
  
  cat(length(trn_9_imp_df_cs_final), 'variables in Split (9) training including dummy codes \n') 
  cat(length(tst_9_imp_df_cs_final), 'variables in Split (9) test including dummy codes \n') 
  
  rm(trn_9_factor, trn_9_factor_dummy, tst_9_factor, tst_9_factor_dummy,
     trn_9_imp_df, tst_9_imp_df)
  
})

### split (10)
job::job({
  
  # subset factor variables
  trn_10_factor <- trn_10_imp_df_cs_final %>% 
    select_if(is.factor)
  tst_10_factor <- tst_10_imp_df_cs_final %>% 
    select_if(is.factor)
  
  # create dummy codes for all categories
  trn_10_factor_dummy <- fastDummies::dummy_cols(trn_10_factor) 
  tst_10_factor_dummy <- fastDummies::dummy_cols(tst_10_factor) 
  
  # subset dummy codes for model (exclude reference grouo)
  trn_10_factor_dummy <- trn_10_factor_dummy %>% 
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
  
  tst_10_factor_dummy <- tst_10_factor_dummy %>% 
    select(
      # Demographics
      race_4l_Asian, race_4l_Black, race_4l_Other_MultiRacial, 
      # race_4l_White (reference group)
      eth_hisp_Hispanic,
      # `eth_hisp_non-Hispanic` (reference group)
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
  trn_10_imp_df_cs_final <- trn_10_imp_df_cs_final %>% 
    select(
      # Demographics
      -race_4l, -eth_hisp, -income, -religion, -p_edu, -sex_2l,
      # Physical Health
      -rec_bin, -exp_sub, -tbi_injury,
      # Culture & Environment
      -kbi_p_grades_in_school, -det_susp, -se_services, 
      # Neurocog
      -cct)
  
  tst_10_imp_df_cs_final <- tst_10_imp_df_cs_final %>% 
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
  trn_10_imp_df_cs_final <- cbind(trn_10_imp_df_cs_final, trn_10_factor_dummy)
  tst_10_imp_df_cs_final <- cbind(tst_10_imp_df_cs_final, tst_10_factor_dummy)
  
  cat(length(trn_10_imp_df_cs_final), 'variables in Split (10) training including dummy codes \n') 
  cat(length(tst_10_imp_df_cs_final), 'variables in Split (10) test including dummy codes \n') 
  
  rm(trn_10_factor, trn_10_factor_dummy, tst_10_factor, tst_10_factor_dummy,
     trn_10_imp_df, tst_10_imp_df)
  
})

# objects retained:
# - trn_1_imp_df_cs_final - trn_10_imp_df_cs_final & tst_1_imp_df_cs_final -
#   tst_10_imp_df_cs_final: observed & imputed dataset with center and scale &
#   dummy codes
# - expected # of predictors (n = 420) + .id + .imp + DV = 423 variables

# Create Model Splits -----------------------------------------------------
# model 1
# - predictors: n = 100 (including dummy codes for factor variables)
# model 2
# - predictors: n = 117 (including dummy codes for factor variables)
# model 3
# - predictors: n = 420 (including dummy codes for factor variables)

#------------------------------------------------------------------------------#
#                                 Model 1
#                     number of predictors: n = 100
#------------------------------------------------------------------------------#

### split (1)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_1_obs_df_m1 <- trn_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_1_obs_df_m1) 
  cat(length(trn_1_obs_df_m1), 'Split (1) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_1_obs_df_m1 <- tst_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_1_obs_df_m1) 
  cat(length(tst_1_obs_df_m1), 'Split (1) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_1_imp_df_m1 <- trn_1_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_1_imp_df_m1) 
  cat(length(trn_1_imp_df_m1), 'Split (1) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_1_imp_df_m1 <- tst_1_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_1_imp_df_m1) 
  cat(length(tst_1_imp_df_m1), 'Split (1) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_1_obs_df_m1), ncol = ncol(trn_1_obs_df_m1))
  where_tst <- matrix(TRUE, nrow = nrow(tst_1_obs_df_m1), ncol = ncol(tst_1_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_1_obs_df_m1)
  colnames(where_tst) <- colnames(tst_1_obs_df_m1)
  
  # save as MICE object
  trn_1_mice_m1 <- as.mids(
    trn_1_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  tst_1_mice_m1 <- as.mids(
    tst_1_imp_df_m1, where = where_tst, .imp = ".imp", .id = ".id") 
  
  rm(where_trn, where_tst)
})

### split (2)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_2_obs_df_m1 <- trn_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_2_obs_df_m1) 
  cat(length(trn_2_obs_df_m1), 'Split (2) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_2_obs_df_m1 <- tst_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_2_obs_df_m1) 
  cat(length(tst_2_obs_df_m1), 'Split (2) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_2_imp_df_m1 <- trn_2_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_2_imp_df_m1) 
  cat(length(trn_2_imp_df_m1), 'Split (2) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_2_imp_df_m1 <- tst_2_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_2_imp_df_m1) 
  cat(length(tst_2_imp_df_m1), 'Split (2) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_2_obs_df_m1), ncol = ncol(trn_2_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_2_obs_df_m1)
  
  # save as MICE object
  trn_2_mice_m1 <- as.mids(
    trn_2_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (3)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_3_obs_df_m1 <- trn_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_3_obs_df_m1) 
  cat(length(trn_3_obs_df_m1), 'Split (3) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_3_obs_df_m1 <- tst_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_3_obs_df_m1) 
  cat(length(tst_3_obs_df_m1), 'Split (3) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_3_imp_df_m1 <- trn_3_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_3_imp_df_m1) 
  cat(length(trn_3_imp_df_m1), 'Split (3) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_3_imp_df_m1 <- tst_3_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_3_imp_df_m1) 
  cat(length(tst_3_imp_df_m1), 'Split (3) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_3_obs_df_m1), ncol = ncol(trn_3_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_3_obs_df_m1)
  
  # save as MICE object
  trn_3_mice_m1 <- as.mids(
    trn_3_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (4)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_4_obs_df_m1 <- trn_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_4_obs_df_m1) 
  cat(length(trn_4_obs_df_m1), 'Split (4) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_4_obs_df_m1 <- tst_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_4_obs_df_m1) 
  cat(length(tst_4_obs_df_m1), 'Split (4) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_4_imp_df_m1 <- trn_4_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_4_imp_df_m1) 
  cat(length(trn_4_imp_df_m1), 'Split (4) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_4_imp_df_m1 <- tst_4_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_4_imp_df_m1) 
  cat(length(tst_4_imp_df_m1), 'Split (4) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_4_obs_df_m1), ncol = ncol(trn_4_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_4_obs_df_m1)
  
  # save as MICE object
  trn_4_mice_m1 <- as.mids(
    trn_4_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (5)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_5_obs_df_m1 <- trn_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_5_obs_df_m1) 
  cat(length(trn_5_obs_df_m1), 'Split (5) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_5_obs_df_m1 <- tst_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_5_obs_df_m1) 
  cat(length(tst_5_obs_df_m1), 'Split (5) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_5_imp_df_m1 <- trn_5_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_5_imp_df_m1) 
  cat(length(trn_5_imp_df_m1), 'Split (5) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_5_imp_df_m1 <- tst_5_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_5_imp_df_m1) 
  cat(length(tst_5_imp_df_m1), 'Split (5) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_5_obs_df_m1), ncol = ncol(trn_5_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_5_obs_df_m1)
  
  # save as MICE object
  trn_5_mice_m1 <- as.mids(
    trn_5_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (6)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_6_obs_df_m1 <- trn_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_6_obs_df_m1) 
  cat(length(trn_6_obs_df_m1), 'Split (6) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_6_obs_df_m1 <- tst_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_6_obs_df_m1) 
  cat(length(tst_6_obs_df_m1), 'Split (6) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_6_imp_df_m1 <- trn_6_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_6_imp_df_m1) 
  cat(length(trn_6_imp_df_m1), 'Split (6) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_6_imp_df_m1 <- tst_6_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_6_imp_df_m1) 
  cat(length(tst_6_imp_df_m1), 'Split (6) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_6_obs_df_m1), ncol = ncol(trn_6_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_6_obs_df_m1)
  
  # save as MICE object
  trn_6_mice_m1 <- as.mids(
    trn_6_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (7)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_7_obs_df_m1 <- trn_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_7_obs_df_m1) 
  cat(length(trn_7_obs_df_m1), 'Split (7) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_7_obs_df_m1 <- tst_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_7_obs_df_m1) 
  cat(length(tst_7_obs_df_m1), 'Split (7) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_7_imp_df_m1 <- trn_7_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_7_imp_df_m1) 
  cat(length(trn_7_imp_df_m1), 'Split (7) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_7_imp_df_m1 <- tst_7_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_7_imp_df_m1) 
  cat(length(tst_7_imp_df_m1), 'Split (7) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_7_obs_df_m1), ncol = ncol(trn_7_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_7_obs_df_m1)
  
  # save as MICE object
  trn_7_mice_m1 <- as.mids(
    trn_7_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (8)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_8_obs_df_m1 <- trn_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_8_obs_df_m1) 
  cat(length(trn_8_obs_df_m1), 'Split (8) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_8_obs_df_m1 <- tst_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_8_obs_df_m1) 
  cat(length(tst_8_obs_df_m1), 'Split (8) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_8_imp_df_m1 <- trn_8_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_8_imp_df_m1) 
  cat(length(trn_8_imp_df_m1), 'Split (8) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_8_imp_df_m1 <- tst_8_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_8_imp_df_m1) 
  cat(length(tst_8_imp_df_m1), 'Split (8) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_8_obs_df_m1), ncol = ncol(trn_8_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_8_obs_df_m1)
  
  # save as MICE object
  trn_8_mice_m1 <- as.mids(
    trn_8_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (9)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_9_obs_df_m1 <- trn_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_9_obs_df_m1) 
  cat(length(trn_9_obs_df_m1), 'Split (9) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_9_obs_df_m1 <- tst_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_9_obs_df_m1) 
  cat(length(tst_9_obs_df_m1), 'Split (9) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_9_imp_df_m1 <- trn_9_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_9_imp_df_m1) 
  cat(length(trn_9_imp_df_m1), 'Split (9) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_9_imp_df_m1 <- tst_9_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_9_imp_df_m1) 
  cat(length(tst_9_imp_df_m1), 'Split (9) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_9_obs_df_m1), ncol = ncol(trn_9_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_9_obs_df_m1)
  
  # save as MICE object
  trn_9_mice_m1 <- as.mids(
    trn_9_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (10)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 100 predictors + 1 DV = 101 variables
  trn_10_obs_df_m1 <- trn_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_10_obs_df_m1) 
  cat(length(trn_10_obs_df_m1), 'Split (10)variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_10_obs_df_m1 <- tst_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_10_obs_df_m1) 
  cat(length(tst_10_obs_df_m1), 'Split (10)variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 100 + .id + .imp + DV = 103 variables
  trn_10_imp_df_m1 <- trn_10_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(trn_10_imp_df_m1) 
  cat(length(trn_10_imp_df_m1), 'Split (10)variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_10_imp_df_m1 <- tst_10_imp_df_cs_final %>% 
    select(-starts_with('hormone'), -starts_with('pea'), 
           -c(cct_Immediate), -starts_with('lmt'), -starts_with('nihtbx'), 
           -starts_with('smri'), -starts_with('dmri'))
  names(tst_10_imp_df_m1) 
  cat(length(tst_10_imp_df_m1), 'Split (10)variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_10_obs_df_m1), ncol = ncol(trn_10_obs_df_m1))
  
  colnames(where_trn) <- colnames(trn_10_obs_df_m1)
  
  # save as MICE object
  trn_10_mice_m1 <- as.mids(
    trn_10_imp_df_m1, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})


#------------------------------------------------------------------------------#
#                                 Model 2
#                     number of predictors: n = 117
#------------------------------------------------------------------------------#

### split (1)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_1_obs_df_m2 <- trn_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_1_obs_df_m2) 
  cat(length(trn_1_obs_df_m2), 'Split (1) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_1_obs_df_m2 <- tst_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_1_obs_df_m2) 
  cat(length(tst_1_obs_df_m2), 'Split (1) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_1_imp_df_m2 <- trn_1_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_1_imp_df_m2) 
  cat(length(trn_1_imp_df_m2), 'Split (1) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_1_imp_df_m2 <- tst_1_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_1_imp_df_m2) 
  cat(length(tst_1_imp_df_m2), 'Split (1) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_1_obs_df_m2), ncol = ncol(trn_1_obs_df_m2))
  where_tst <- matrix(TRUE, nrow = nrow(tst_1_obs_df_m2), ncol = ncol(tst_1_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_1_obs_df_m2)
  colnames(where_tst) <- colnames(tst_1_obs_df_m2)
  
  # save as MICE object
  trn_1_mice_m2 <- as.mids(
    trn_1_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  tst_1_mice_m2 <- as.mids(
    tst_1_imp_df_m2, where = where_tst, .imp = ".imp", .id = ".id") 
  
  rm(where_trn, where_tst)
})

### split (2)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_2_obs_df_m2 <- trn_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_2_obs_df_m2) 
  cat(length(trn_2_obs_df_m2), 'Split (2) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_2_obs_df_m2 <- tst_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_2_obs_df_m2) 
  cat(length(tst_2_obs_df_m2), 'Split (2) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_2_imp_df_m2 <- trn_2_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_2_imp_df_m2) 
  cat(length(trn_2_imp_df_m2), 'Split (2) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_2_imp_df_m2 <- tst_2_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_2_imp_df_m2) 
  cat(length(tst_2_imp_df_m2), 'Split (2) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_2_obs_df_m2), ncol = ncol(trn_2_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_2_obs_df_m2)
  
  # save as MICE object
  trn_2_mice_m2 <- as.mids(
    trn_2_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (3)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_3_obs_df_m2 <- trn_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_3_obs_df_m2) 
  cat(length(trn_3_obs_df_m2), 'Split (3) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_3_obs_df_m2 <- tst_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_3_obs_df_m2) 
  cat(length(tst_3_obs_df_m2), 'Split (3) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_3_imp_df_m2 <- trn_3_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_3_imp_df_m2) 
  cat(length(trn_3_imp_df_m2), 'Split (3) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_3_imp_df_m2 <- tst_3_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_3_imp_df_m2) 
  cat(length(tst_3_imp_df_m2), 'Split (3) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_3_obs_df_m2), ncol = ncol(trn_3_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_3_obs_df_m2)
  
  # save as MICE object
  trn_3_mice_m2 <- as.mids(
    trn_3_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (4)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_4_obs_df_m2 <- trn_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_4_obs_df_m2) 
  cat(length(trn_4_obs_df_m2), 'Split (4) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_4_obs_df_m2 <- tst_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_4_obs_df_m2) 
  cat(length(tst_4_obs_df_m2), 'Split (4) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_4_imp_df_m2 <- trn_4_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_4_imp_df_m2) 
  cat(length(trn_4_imp_df_m2), 'Split (4) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_4_imp_df_m2 <- tst_4_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_4_imp_df_m2) 
  cat(length(tst_4_imp_df_m2), 'Split (4) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_4_obs_df_m2), ncol = ncol(trn_4_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_4_obs_df_m2)
  
  # save as MICE object
  trn_4_mice_m2 <- as.mids(
    trn_4_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (5)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_5_obs_df_m2 <- trn_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_5_obs_df_m2) 
  cat(length(trn_5_obs_df_m2), 'Split (5) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_5_obs_df_m2 <- tst_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_5_obs_df_m2) 
  cat(length(tst_5_obs_df_m2), 'Split (5) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_5_imp_df_m2 <- trn_5_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_5_imp_df_m2) 
  cat(length(trn_5_imp_df_m2), 'Split (5) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_5_imp_df_m2 <- tst_5_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_5_imp_df_m2) 
  cat(length(tst_5_imp_df_m2), 'Split (5) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_5_obs_df_m2), ncol = ncol(trn_5_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_5_obs_df_m2)
  
  # save as MICE object
  trn_5_mice_m2 <- as.mids(
    trn_5_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (6)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_6_obs_df_m2 <- trn_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_6_obs_df_m2) 
  cat(length(trn_6_obs_df_m2), 'Split (6) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_6_obs_df_m2 <- tst_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_6_obs_df_m2) 
  cat(length(tst_6_obs_df_m2), 'Split (6) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_6_imp_df_m2 <- trn_6_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_6_imp_df_m2) 
  cat(length(trn_6_imp_df_m2), 'Split (6) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_6_imp_df_m2 <- tst_6_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_6_imp_df_m2) 
  cat(length(tst_6_imp_df_m2), 'Split (6) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_6_obs_df_m2), ncol = ncol(trn_6_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_6_obs_df_m2)
  
  # save as MICE object
  trn_6_mice_m2 <- as.mids(
    trn_6_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (7)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_7_obs_df_m2 <- trn_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_7_obs_df_m2) 
  cat(length(trn_7_obs_df_m2), 'Split (7) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_7_obs_df_m2 <- tst_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_7_obs_df_m2) 
  cat(length(tst_7_obs_df_m2), 'Split (7) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_7_imp_df_m2 <- trn_7_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_7_imp_df_m2) 
  cat(length(trn_7_imp_df_m2), 'Split (7) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_7_imp_df_m2 <- tst_7_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_7_imp_df_m2) 
  cat(length(tst_7_imp_df_m2), 'Split (7) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_7_obs_df_m2), ncol = ncol(trn_7_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_7_obs_df_m2)
  
  # save as MICE object
  trn_7_mice_m2 <- as.mids(
    trn_7_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (8)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_8_obs_df_m2 <- trn_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_8_obs_df_m2) 
  cat(length(trn_8_obs_df_m2), 'Split (8) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_8_obs_df_m2 <- tst_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_8_obs_df_m2) 
  cat(length(tst_8_obs_df_m2), 'Split (8) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_8_imp_df_m2 <- trn_8_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_8_imp_df_m2) 
  cat(length(trn_8_imp_df_m2), 'Split (8) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_8_imp_df_m2 <- tst_8_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_8_imp_df_m2) 
  cat(length(tst_8_imp_df_m2), 'Split (8) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_8_obs_df_m2), ncol = ncol(trn_8_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_8_obs_df_m2)
  
  # save as MICE object
  trn_8_mice_m2 <- as.mids(
    trn_8_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (9)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_9_obs_df_m2 <- trn_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_9_obs_df_m2) 
  cat(length(trn_9_obs_df_m2), 'Split (9) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_9_obs_df_m2 <- tst_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_9_obs_df_m2) 
  cat(length(tst_9_obs_df_m2), 'Split (9) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_9_imp_df_m2 <- trn_9_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_9_imp_df_m2) 
  cat(length(trn_9_imp_df_m2), 'Split (9) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_9_imp_df_m2 <- tst_9_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_9_imp_df_m2) 
  cat(length(tst_9_imp_df_m2), 'Split (9) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_9_obs_df_m2), ncol = ncol(trn_9_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_9_obs_df_m2)
  
  # save as MICE object
  trn_9_mice_m2 <- as.mids(
    trn_9_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (10)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 117 predictors + 1 DV = 118 variables
  trn_10_obs_df_m2 <- trn_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(trn_10_obs_df_m2) 
  cat(length(trn_10_obs_df_m2), 'Split (10) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_10_obs_df_m2 <- tst_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id, -starts_with('smri'), -starts_with('dmri'))
  names(tst_10_obs_df_m2) 
  cat(length(tst_10_obs_df_m2), 'Split (10) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 117 + .id + .imp + DV = 120 variables
  trn_10_imp_df_m2 <- trn_10_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(trn_10_imp_df_m2) 
  cat(length(trn_10_imp_df_m2), 'Split (10) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_10_imp_df_m2 <- tst_10_imp_df_cs_final %>% 
    select(-starts_with('smri'), -starts_with('dmri'))
  names(tst_10_imp_df_m2) 
  cat(length(tst_10_imp_df_m2), 'Split (10) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_10_obs_df_m2), ncol = ncol(trn_10_obs_df_m2))
  
  colnames(where_trn) <- colnames(trn_10_obs_df_m2)
  
  # save as MICE object
  trn_10_mice_m2 <- as.mids(
    trn_10_imp_df_m2, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

#------------------------------------------------------------------------------#
#                                 Model 3
#                     number of predictors: n = 420
#------------------------------------------------------------------------------#

### split (1)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_1_obs_df_m3 <- trn_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_1_obs_df_m3) 
  cat(length(trn_1_obs_df_m3), 'Split (1) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_1_obs_df_m3 <- tst_1_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_1_obs_df_m3) 
  cat(length(tst_1_obs_df_m3), 'Split (1) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_1_imp_df_m3 <- trn_1_imp_df_cs_final 
  names(trn_1_imp_df_m3) 
  cat(length(trn_1_imp_df_m3), 'Split (1) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_1_imp_df_m3 <- tst_1_imp_df_cs_final 
  names(tst_1_imp_df_m3) 
  cat(length(tst_1_imp_df_m3), 'Split (1) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_1_obs_df_m3), ncol = ncol(trn_1_obs_df_m3))
  where_tst <- matrix(TRUE, nrow = nrow(tst_1_obs_df_m3), ncol = ncol(tst_1_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_1_obs_df_m3)
  colnames(where_tst) <- colnames(tst_1_obs_df_m3)
  
  # save as MICE object
  trn_1_mice_m3 <- as.mids(
    trn_1_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  tst_1_mice_m3 <- as.mids(
    tst_1_imp_df_m3, where = where_tst, .imp = ".imp", .id = ".id") 
  
  rm(where_trn, where_tst)
})

### split (2)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_2_obs_df_m3 <- trn_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_2_obs_df_m3) 
  cat(length(trn_2_obs_df_m3), 'Split (2) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_2_obs_df_m3 <- tst_2_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_2_obs_df_m3) 
  cat(length(tst_2_obs_df_m3), 'Split (2) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_2_imp_df_m3 <- trn_2_imp_df_cs_final 
  names(trn_2_imp_df_m3) 
  cat(length(trn_2_imp_df_m3), 'Split (2) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_2_imp_df_m3 <- tst_2_imp_df_cs_final 
  names(tst_2_imp_df_m3) 
  cat(length(tst_2_imp_df_m3), 'Split (2) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_2_obs_df_m3), ncol = ncol(trn_2_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_2_obs_df_m3)
  
  # save as MICE object
  trn_2_mice_m3 <- as.mids(
    trn_2_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (3)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_3_obs_df_m3 <- trn_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_3_obs_df_m3) 
  cat(length(trn_3_obs_df_m3), 'Split (3) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_3_obs_df_m3 <- tst_3_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_3_obs_df_m3) 
  cat(length(tst_3_obs_df_m3), 'Split (3) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_3_imp_df_m3 <- trn_3_imp_df_cs_final 
  names(trn_3_imp_df_m3) 
  cat(length(trn_3_imp_df_m3), 'Split (3) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_3_imp_df_m3 <- tst_3_imp_df_cs_final 
  names(tst_3_imp_df_m3) 
  cat(length(tst_3_imp_df_m3), 'Split (3) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_3_obs_df_m3), ncol = ncol(trn_3_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_3_obs_df_m3)
  
  # save as MICE object
  trn_3_mice_m3 <- as.mids(
    trn_3_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (4)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_4_obs_df_m3 <- trn_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_4_obs_df_m3) 
  cat(length(trn_4_obs_df_m3), 'Split (4) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_4_obs_df_m3 <- tst_4_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_4_obs_df_m3) 
  cat(length(tst_4_obs_df_m3), 'Split (4) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_4_imp_df_m3 <- trn_4_imp_df_cs_final 
  names(trn_4_imp_df_m3) 
  cat(length(trn_4_imp_df_m3), 'Split (4) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_4_imp_df_m3 <- tst_4_imp_df_cs_final 
  names(tst_4_imp_df_m3) 
  cat(length(tst_4_imp_df_m3), 'Split (4) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_4_obs_df_m3), ncol = ncol(trn_4_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_4_obs_df_m3)
  
  # save as MICE object
  trn_4_mice_m3 <- as.mids(
    trn_4_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (5)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_5_obs_df_m3 <- trn_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_5_obs_df_m3) 
  cat(length(trn_5_obs_df_m3), 'Split (5) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_5_obs_df_m3 <- tst_5_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_5_obs_df_m3) 
  cat(length(tst_5_obs_df_m3), 'Split (5) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_5_imp_df_m3 <- trn_5_imp_df_cs_final 
  names(trn_5_imp_df_m3) 
  cat(length(trn_5_imp_df_m3), 'Split (5) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_5_imp_df_m3 <- tst_5_imp_df_cs_final 
  names(tst_5_imp_df_m3) 
  cat(length(tst_5_imp_df_m3), 'Split (5) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_5_obs_df_m3), ncol = ncol(trn_5_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_5_obs_df_m3)
  
  # save as MICE object
  trn_5_mice_m3 <- as.mids(
    trn_5_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (6)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_6_obs_df_m3 <- trn_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_6_obs_df_m3) 
  cat(length(trn_6_obs_df_m3), 'Split (6) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_6_obs_df_m3 <- tst_6_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_6_obs_df_m3) 
  cat(length(tst_6_obs_df_m3), 'Split (6) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_6_imp_df_m3 <- trn_6_imp_df_cs_final 
  names(trn_6_imp_df_m3) 
  cat(length(trn_6_imp_df_m3), 'Split (6) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_6_imp_df_m3 <- tst_6_imp_df_cs_final 
  names(tst_6_imp_df_m3) 
  cat(length(tst_6_imp_df_m3), 'Split (6) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_6_obs_df_m3), ncol = ncol(trn_6_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_6_obs_df_m3)
  
  # save as MICE object
  trn_6_mice_m3 <- as.mids(
    trn_6_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (7)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_7_obs_df_m3 <- trn_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_7_obs_df_m3) 
  cat(length(trn_7_obs_df_m3), 'Split (7) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_7_obs_df_m3 <- tst_7_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_7_obs_df_m3) 
  cat(length(tst_7_obs_df_m3), 'Split (7) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_7_imp_df_m3 <- trn_7_imp_df_cs_final 
  names(trn_7_imp_df_m3) 
  cat(length(trn_7_imp_df_m3), 'Split (7) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_7_imp_df_m3 <- tst_7_imp_df_cs_final 
  names(tst_7_imp_df_m3) 
  cat(length(tst_7_imp_df_m3), 'Split (7) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_7_obs_df_m3), ncol = ncol(trn_7_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_7_obs_df_m3)
  
  # save as MICE object
  trn_7_mice_m3 <- as.mids(
    trn_7_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (8)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_8_obs_df_m3 <- trn_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_8_obs_df_m3) 
  cat(length(trn_8_obs_df_m3), 'Split (8) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_8_obs_df_m3 <- tst_8_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_8_obs_df_m3) 
  cat(length(tst_8_obs_df_m3), 'Split (8) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_8_imp_df_m3 <- trn_8_imp_df_cs_final 
  names(trn_8_imp_df_m3) 
  cat(length(trn_8_imp_df_m3), 'Split (8) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_8_imp_df_m3 <- tst_8_imp_df_cs_final 
  names(tst_8_imp_df_m3) 
  cat(length(tst_8_imp_df_m3), 'Split (8) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_8_obs_df_m3), ncol = ncol(trn_8_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_8_obs_df_m3)
  
  # save as MICE object
  trn_8_mice_m3 <- as.mids(
    trn_8_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (9)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_9_obs_df_m3 <- trn_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_9_obs_df_m3) 
  cat(length(trn_9_obs_df_m3), 'Split (9) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_9_obs_df_m3 <- tst_9_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_9_obs_df_m3) 
  cat(length(tst_9_obs_df_m3), 'Split (9) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_9_imp_df_m3 <- trn_9_imp_df_cs_final 
  names(trn_9_imp_df_m3) 
  cat(length(trn_9_imp_df_m3), 'Split (9) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_9_imp_df_m3 <- tst_9_imp_df_cs_final 
  names(tst_9_imp_df_m3) 
  cat(length(tst_9_imp_df_m3), 'Split (9) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_9_obs_df_m3), ncol = ncol(trn_9_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_9_obs_df_m3)
  
  # save as MICE object
  trn_9_mice_m3 <- as.mids(
    trn_9_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

### split (10)
job::job({
  
  # (1) dataframe w/original data 
  # predictors w/dummy vars: n = 420 predictors + 1 DV = 421 variables
  trn_10_obs_df_m3 <- trn_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(trn_10_obs_df_m3) 
  cat(length(trn_10_obs_df_m3), 'Split (10) variables with dummy codes + DV in training observed dataframe  \n')
  
  tst_10_obs_df_m3 <- tst_10_imp_df_cs_final %>% 
    subset(.imp == 0) %>% 
    select(-.imp, -.id)
  names(tst_10_obs_df_m3) 
  cat(length(tst_10_obs_df_m3), 'Split (10) variables with dummy codes + DV in test observed dataframe  \n')
  
  # (2) dataframe w/imputed data (+ original data)
  # predictors w/dummy vars: n = 420 + .id + .imp + DV = 423 variables
  trn_10_imp_df_m3 <- trn_10_imp_df_cs_final 
  names(trn_10_imp_df_m3) 
  cat(length(trn_10_imp_df_m3), 'Split (10) variables with dummy codes + DV in training impued dataframe  \n')
  
  tst_10_imp_df_m3 <- tst_10_imp_df_cs_final 
  names(tst_10_imp_df_m3) 
  cat(length(tst_10_imp_df_m3), 'Split (10) variables with dummy codes + DV in test impued dataframe  \n')
  
  # (3) MICE object w/imputed center and scaled data 
  
  # where statement to retain all changes to predictors: center & scale + dummy codes
  where_trn <- matrix(TRUE, nrow = nrow(trn_10_obs_df_m3), ncol = ncol(trn_10_obs_df_m3))
  
  colnames(where_trn) <- colnames(trn_10_obs_df_m3)
  
  # save as MICE object
  trn_10_mice_m3 <- as.mids(
    trn_10_imp_df_m3, where = where_trn, .imp = ".imp", .id = ".id") 
  
  rm(where_trn)
})

# drop objects no longer needed
rm(
  trn_1, trn_2, trn_3, trn_4, trn_5, trn_6, trn_7, trn_8, trn_9, trn_10,
  tst_1, tst_2, tst_3, tst_4, tst_5, tst_6, tst_7, tst_8, tst_9, tst_10,
   
  trn_1_imp_df_cs_final, trn_2_imp_df_cs_final, trn_3_imp_df_cs_final,
  trn_4_imp_df_cs_final, trn_5_imp_df_cs_final, trn_6_imp_df_cs_final,
  trn_7_imp_df_cs_final, trn_8_imp_df_cs_final, trn_9_imp_df_cs_final,
  trn_10_imp_df_cs_final, 
  tst_1_imp_df_cs_final, tst_2_imp_df_cs_final, tst_3_imp_df_cs_final,
  tst_4_imp_df_cs_final, tst_5_imp_df_cs_final, tst_6_imp_df_cs_final,
  tst_7_imp_df_cs_final, tst_8_imp_df_cs_final, tst_9_imp_df_cs_final,
  tst_10_imp_df_cs_final)

# retained objects (per split for each model)
# (1) 
# - object names: 
#   - trn/tst_1_obs_df_m1 - trn/tst_10_obs_df_m1
#   - trn/tst_1_obs_df_m2 - trn/tst_10_obs_df_m2
#   - trn/tst_1_obs_df_m3 - trn/tst_10_obs_df_m3
# - description:
#   - dataframe of observed variables for each model
# - use:
#   - cv.saenet and calculating AUC
# (2)
# - object names: 
#   - trn/tst_1_imp_df_m1 - trn/tst_10_imp_df_m1
#   - trn/tst_1_imp_df_m2 - trn/tst_10_imp_df_m2
#   - trn/tst_1_imp_df_m3 - trn/tst_10_imp_df_m3
# - description:
#   - dataframe of observed and imputed variables for each model
# - use:
#   - cv.saenet and calculating AUC
# (3)
# - object names: 
#   - trn_1_mice_m1 - trn_10_mice_m1
#   - trn_1_mice_m2 - trn_10_mice_m2
#   - trn_1_mice_m3 - trn_10_mice_m3
# - description:
#   - MICE object for each model in training dataset
# - use:
#   - cv.saenet  
#
# (4) only for split 1 as logistic regression only run on 1 split
# - object names: 
#   - tst_1_mice_m1, tst_1_mice_m2, tst_1_mice_m3
# - description:
#   - MICE object for each model in test dataet
# - use:
#   - AUC for logistic regression 


# Additional: Table Names -------------------------------------------------

# table names for factor variables with dummy codes
table_names_factor <- trn_1_obs_df_m3 %>% 
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
    rec_bin_Yes, # rec_bin_No (reference group)
    exp_sub_Yes, # exp_sub_No (reference group)
    tbi_injury_Yes, # tbi_injury_No (reference group)
    
    # Culture & Environment
    kbi_p_grades_in_school_Grade_B, kbi_p_grades_in_school_Grade_C, 
    kbi_p_grades_in_school_Grade_Fail,
    # kbi_p_grades_in_school_Grade_A (reference group)
    det_susp_Yes, # det_susp_No (reference group)
    se_services_Emotion_or_Learning_Support, se_services_Gifted,
    se_services_Other, se_services_Combined_Services,
    # se_services_None (reference group)
    
    # Neurocognitive
    cct_Immediate)  %>%  # cct_Delayed (reference group)
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  select(variable) %>%
  mutate(
    table_name = case_when(
      # Demographics
      variable == 'race_4l_Asian' ~ 'Race - Asian',
      variable == 'race_4l_Black' ~ 'Race - Black',
      variable == 'race_4l_Other_MultiRacial' ~ 'Race - Other/MultiRacial',
      
      variable == 'eth_hisp_Hispanic' ~ 'Ethnicity - Hispanic',
      
      variable == 'income_inc_1' ~ 'Income - Less than $5,000',
      variable == 'income_inc_2' ~ 'Income - $5,000 through $11,999',
      variable == 'income_inc_3' ~ 'Income - $12,000 through $15,999',
      variable == 'income_inc_4' ~ 'Income - $16,000 through $24,999',
      variable == 'income_inc_5' ~ 'Income - $25,000 through $34,999',
      variable == 'income_inc_6' ~ 'Income - $35,000 through $49,999',
      variable == 'income_inc_7' ~ 'Income - $50,000 through $74,999',
      variable == 'income_inc_8' ~ 'Income - $75,000 through $99,999',
      variable == 'income_inc_10' ~ 'Income - $200,000 and greater',
      
      variable == 'religion_rp_1' ~ 'Religion - Mainline Protestant',
      variable == 'religion_rp_2' ~ 'Religion - Evangelical Protestant',
      variable == 'religion_rp_3' ~ 'Religion - Historically Black Church',
      variable == 'religion_rp_4' ~ 'Religion - Roman Catholic',
      variable == 'religion_rp_5' ~ 'Religion - Jewish (Judaism)',
      variable == 'religion_rp_6' ~ 'Religion - Mormon (Church of Jesus Christ of Latter Day Saints/LDS)',
      variable == 'religion_rp_7' ~ 'Religion - Jehovahs Witness',
      variable == 'religion_rp_8' ~ 'Religion - Muslim (Islam)',
      variable == 'religion_rp_9' ~ 'Religion - Buddhist',
      variable == 'religion_rp_10' ~ 'Religion - Hindu',
      variable == 'religion_rp_11' ~ 'Religion - Orthodox Christian',
      variable == 'religion_rp_12' ~ 'Religion - Unitarian (Universalist)',
      variable == 'religion_rp_13' ~ 'Religion - Other Christian',
      variable == 'religion_rp_14' ~ 'Religion - Atheist (do not believe in God)',
      variable == 'religion_rp_15' ~ 'Religion - Agnostic (not sure if there is a God)',
      variable == 'religion_rp_16' ~ 'Religion - Something else',
      
      variable == 'p_edu_Less_than_HS_Degree_GED_Equivalent' ~ 'Parent Education - Less than HS Degree or GED Equivalent',
      variable == 'p_edu_HS_Graduate_GED_Equivalent' ~ 'Parent Education - HS Degree or GED Equivalent',
      variable == 'p_edu_Some_College_or_Associates_Degree' ~ 'Parent Education - Some College or Associate Degree',
      variable == 'p_edu_Masters_Degree' ~ 'Parent Education - Masters Degree',
      variable == 'p_edu_Professional_School_or_Doctoral_Degree' ~ 'Parent Education - Professional or Doctoral Degree',
      
      variable == 'sex_2l_Female' ~ 'Sex - Female',
      
      # Physical Health
      variable == 'rec_bin_Yes' ~ 'Recreational Activities (binary) - Yes',
      variable == 'exp_sub_Yes' ~ 'Prenatal Exposure - Substance Use - Yes',
      variable == 'tbi_injury_Yes' ~ 'TBI Injury - Yes',
      
      # Culture & Environment
      variable == 'kbi_p_grades_in_school_Grade_B' ~ 'Grade - B',
      variable == 'kbi_p_grades_in_school_Grade_C' ~ 'Grade - C',
      variable == 'kbi_p_grades_in_school_Grade_Fail' ~ 'Grade - Fail',
      variable == 'det_susp_Yes' ~ 'Detention / Suspension - Yes',
      variable == 'se_services_Emotion_or_Learning_Support' ~ 'Special Education Services - Emotion or Learning Support',
      variable == 'se_services_Gifted' ~ 'Special Education Services - Gifted',
      variable == 'se_services_Other' ~ 'Special Education Services - Other',
      variable == 'se_services_Combined_Services' ~ 'Special Education Services - Combined Services',
      
      # Neurocognitive
      variable == 'cct_Immediate' ~ 'Cash Choice Task - Immediate'))

domain_names_factor <- trn_1_obs_df_m3 %>% 
  select(# Demographics
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
    rec_bin_Yes, # rec_bin_No (reference group)
    exp_sub_Yes, # exp_sub_No (reference group)
    tbi_injury_Yes, # tbi_injury_No (reference group)
    
    # Culture & Environment
    kbi_p_grades_in_school_Grade_B, kbi_p_grades_in_school_Grade_C, 
    kbi_p_grades_in_school_Grade_Fail,
    # kbi_p_grades_in_school_Grade_A (reference group)
    det_susp_Yes, # det_susp_No (reference group)
    se_services_Emotion_or_Learning_Support, se_services_Gifted,
    se_services_Other, se_services_Combined_Services,
    # se_services_None (reference group)
    
    # Neurocognitive
    cct_Immediate) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = "variable") %>% 
  select(variable) %>% 
  mutate(
    domain_name = case_when(
      # Demographics
      substr(variable, 1, 4) == 'race' ~ 'Demographics',
      variable == 'eth_hisp_Hispanic' ~ 'Demographics',
      substr(variable, 1, 3) == 'inc' ~ 'Demographics',
      substr(variable, 1, 3) == 'rel' ~ 'Demographics',
      substr(variable, 1, 5) == 'p_edu' ~ 'Demographics',
      variable == 'sex_2l_Female' ~ 'Demographics',

      # Physical Health
      variable == 'rec_bin_Yes' ~ 'Physical Health',
      variable == 'exp_sub_Yes' ~ 'Physical Health',
      variable == 'tbi_injury_Yes' ~ 'Physical Health',
      
      # Culture & Environment
      substr(variable, 1, 3) == 'kbi' ~ 'Culture & Environment',
      variable == 'det_susp_Yes' ~ 'Culture & Environment',
      substr(variable, 1, 3) == 'se_' ~ 'Culture & Environment',
      
      # Neurocognitive
      variable == 'cct_Immediate' ~ 'Neurocognitive Factors'))

table_names_factor <- table_names_factor %>% 
  full_join(domain_names_factor, by = 'variable') 

rm(domain_names_factor)  

table_names <- rbind(table_names_numeric, table_names_factor)


