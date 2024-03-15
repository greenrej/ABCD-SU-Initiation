# Correlations: Pearson ----------------------------------------------------

# subset numeric variables
data.2_num <- data.2 %>% 
  select_if(is.numeric) %>% 
  select(-DV)

# rank order of correlation pairs                            

corr_rank <- rcorr(as.matrix(data.2_num))
corr_rank <- corr_rank$r
corr_rank <- corr_rank %>% 
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>% 
  ungroup() %>% 
  subset(value != 1) # exclude diaganol of correlation matrix

summary(corr_rank$value) # minimum correlation: -0.69332 
write.csv(corr_rank,'output/correlations/corr_rank.csv', row.names = TRUE)

# subset correlations 
# breakdown of large correlations 
corr_rank_1 <- corr_rank %>% 
  subset((value >= 0.90 & value <= 1.0) | (value <= -0.90 & value >= -1.0)) 
dim(corr_rank_1) # n = 82 > 0.90 

corr_rank_2 <- corr_rank %>% 
  subset((value < 0.90 & value >= 0.80) | (value > -0.90 & value <= -0.80)) 
dim(corr_rank_2) # n = 96 between 0.80 - 0.89

corr_rank_3 <- corr_rank %>% 
  subset((value < 0.80 & value >= 0.70) | (value > -0.80 & value <= -0.70)) 
dim(corr_rank_3)  # n = 202 between 0.70 - 0.89

corr_rank_4 <- corr_rank %>% 
  subset((value < 0.70 & value >= 0.50) | (value > -0.70 & value <= -0.50))
dim(corr_rank_4) # n = 3274 between 0.50 - 0.70 | (-0.50) - (-0.70)

# breakdown of moderate correlations   
corr_rank_5 <- corr_rank %>% 
  subset((value >= 0.30 & value < 0.50) | (value <= -0.30 & value > -0.50))
dim(corr_rank_5)  # n = 13496 between 0.30 - 0.50 | (-0.30) - (-0.50)

# breakdown of small correlations  
corr_rank_6 <- corr_rank %>% 
  subset((value >= 0.10 & value < 0.30) | (value <= -0.10 & value > -0.30))
dim(corr_rank_6)  # n = 25904 between 0.10 - 0.30 | (-0.10) - (-0.30)

# breakdown of very small correlations  
corr_rank_7 <- corr_rank %>% 
  subset((value < 0.10 & value >= 0) | (value > -0.10 & value <= 0))
dim(corr_rank_7)  # n = 95702 < 0.10 -0 | (-0.10) - 0

write.csv(corr_rank_1,'output/correlations/corr_rank_1.csv', row.names = TRUE)
write.csv(corr_rank_2,'output/correlations/corr_rank_2.csv', row.names = TRUE)
write.csv(corr_rank_3,'output/correlations/corr_rank_3.csv', row.names = TRUE)
write.csv(corr_rank_4,'output/correlations/corr_rank_4.csv', row.names = TRUE)
write.csv(corr_rank_5,'output/correlations/corr_rank_5.csv', row.names = TRUE)
write.csv(corr_rank_6,'output/correlations/corr_rank_6.csv', row.names = TRUE)
write.csv(corr_rank_7,'output/correlations/corr_rank_7.csv', row.names = TRUE)

rm(corr_rank, corr_rank_1, corr_rank_2, corr_rank_3, corr_rank_4, corr_rank_5,
   corr_rank_6, corr_rank_7)


# Correlations: Point-Biserial ---------------------------------------------

# - numeric variables (n = 373) + dichotomous variables (n = 7)

# create numeric dichotomous variable 

data.2_pb <- data.2 %>% 
  mutate(
    sex_v = case_when(
      sex_2l == 'Female' ~ 1,
      sex_2l == 'Male' ~ 0),
    eth_v = case_when(
      eth_hisp == 'Hispanic' ~ 1,
      eth_hisp == 'non_Hispanic' ~ 0),
    rec_v = case_when(
      rec_bin == 'Yes' ~ 1,
      rec_bin == 'No' ~ 0),
    exp_v = case_when(
      exp_sub == 'Yes' ~ 1,
      exp_sub == 'No' ~ 0),
    tbi_v = case_when(
      tbi_injury == 'Yes' ~ 1,
      tbi_injury == 'No' ~ 0),
    det_v = case_when(
      det_susp == 'Yes' ~ 1,
      det_susp == 'No' ~ 0),
    cct_v = case_when(
      cct == 'Immediate' ~ 1,
      cct == 'Delayed' ~ 0)) %>%
  select_if(., is.numeric) %>% 
  # organize variables for point-biserial correlation
  select(
    # binary variables
    sex_v, eth_v, rec_v, exp_v, tbi_v, det_v, cct_v,
    # demographics
    age_baseline, 
    # Self and Peer Risk Factors for Substance Use
    peer_alc, peer_tob, peer_cb, peer_other, peer_prob, 
    path_alc, path_tob, path_cb, 
    # Parenting behaviors
    crpf, par_rules, pmq_y_ss_mean, fes_y_ss_fc, crpbi_y_ss_parent,
    # mental health 
    cbcl_scr_syn_anxdep_r,  cbcl_scr_syn_withdep_r, cbcl_scr_syn_somatic_r,
    cbcl_scr_syn_social_r, cbcl_scr_syn_thought_r, cbcl_scr_syn_attention_r,
    cbcl_scr_syn_rulebreak_r, cbcl_scr_syn_aggressive_r, 
    cbcl_scr_syn_internal_r, cbcl_scr_syn_external_r, cbcl_scr_syn_totprob_r,
    mh_density, pps_y_ss_number, upps_y_ss_negative_urgency, 
    upps_y_ss_lack_of_perseverance, upps_y_ss_lack_of_planning,
    upps_y_ss_sensation_seeking, upps_y_ss_positive_urgency, bis_y_ss_bis_sum,
    bis_y_ss_bas_drive, bis_y_ss_bas_fs, bis_y_ss_bas_rr,
    # physical health 
    rec_con, screentime, act1, act2, act5, exp_caf_rec,
    sds_p_ss_dims, sds_p_ss_sbd, sds_p_ss_da, sds_p_ss_swtd, sds_p_ss_does,
    sds_p_ss_shy, pds, 
    # culture and environment
    accult_q2_y, neighborhood_crime_y, srpf_y_ss_ses, srpf_y_ss_iiss, 
    srpf_y_ss_dfs,
    # hormones
    hormone_scr_ert_mean,
    hormone_scr_dhea_mean,
    # neurocognition
    pea_ravlt_sd_trial_vi_tc, pea_ravlt_ld_trial_vii_tc, pea_ravlt_learn,
    pea_wiscv_tss, lmt_acc, lmt_scr_rt_correct, lmt_scr_efficiency,
    nihtbx_picvocab_agecorrected, nihtbx_flanker_agecorrected,
    nihtbx_list_agecorrected, nihtbx_cardsort_agecorrected, 
    nihtbx_pattern_agecorrected, nihtbx_picture_agecorrected, 
    nihtbx_reading_agecorrected,
    # structural MRI - Area
    smri_area_cdk_banksstslh, smri_area_cdk_banksstsrh,
    smri_area_cdk_cdacatelh, smri_area_cdk_cdacaterh,
    smri_area_cdk_cdmdfrlh, smri_area_cdk_cdmdfrrh,
    smri_area_cdk_cuneuslh, smri_area_cdk_cuneusrh,
    smri_area_cdk_ehinallh, smri_area_cdk_ehinalrh,
    smri_area_cdk_fusiformlh, smri_area_cdk_fusiformrh,
    smri_area_cdk_ifpllh, smri_area_cdk_ifplrh,
    smri_area_cdk_iftmlh, smri_area_cdk_iftmrh,
    smri_area_cdk_ihcatelh, smri_area_cdk_ihcaterh,
    smri_area_cdk_locclh, smri_area_cdk_loccrh,
    smri_area_cdk_lobfrlh, smri_area_cdk_lobfrrh,
    smri_area_cdk_linguallh, smri_area_cdk_lingualrh,
    smri_area_cdk_mobfrlh, smri_area_cdk_mobfrrh,
    smri_area_cdk_mdtmlh, smri_area_cdk_mdtmrh,
    smri_area_cdk_parahpallh, smri_area_cdk_parahpalrh,
    smri_area_cdk_paracnlh, smri_area_cdk_paracnrh,
    smri_area_cdk_parsopclh, smri_area_cdk_parsopcrh,
    smri_area_cdk_parsobislh, smri_area_cdk_parsobisrh,
    smri_area_cdk_parstgrislh, smri_area_cdk_parstgrisrh,
    smri_area_cdk_pericclh, smri_area_cdk_periccrh,
    smri_area_cdk_postcnlh, smri_area_cdk_postcnrh,
    smri_area_cdk_ptcatelh, smri_area_cdk_ptcaterh,
    smri_area_cdk_precnlh, smri_area_cdk_precnrh,
    smri_area_cdk_pclh, smri_area_cdk_pcrh,
    smri_area_cdk_rracatelh, smri_area_cdk_rracaterh,
    smri_area_cdk_rrmdfrlh, smri_area_cdk_rrmdfrrh,
    smri_area_cdk_sufrlh, smri_area_cdk_sufrrh,
    smri_area_cdk_supllh ,smri_area_cdk_suplrh,
    smri_area_cdk_sutmlh, smri_area_cdk_sutmrh,
    smri_area_cdk_smlh, smri_area_cdk_smrh,
    smri_area_cdk_frpolelh, smri_area_cdk_frpolerh,
    smri_area_cdk_tmpolelh, smri_area_cdk_tmpolerh,
    smri_area_cdk_trvtmlh, smri_area_cdk_trvtmrh,
    smri_area_cdk_insulalh, smri_area_cdk_insularh,
    # structural MRI - Volume
    smri_vol_cdk_banksstslh, smri_vol_cdk_banksstsrh,
    smri_vol_cdk_cdacatelh, smri_vol_cdk_cdacaterh,
    smri_vol_cdk_cdmdfrlh, smri_vol_cdk_cdmdfrrh,
    smri_vol_cdk_cuneuslh, smri_vol_cdk_cuneusrh,
    smri_vol_cdk_ehinallh, smri_vol_cdk_ehinalrh,
    smri_vol_cdk_fusiformlh, smri_vol_cdk_fusiformrh,
    smri_vol_cdk_ifpllh, smri_vol_cdk_ifplrh,
    smri_vol_cdk_iftmlh, smri_vol_cdk_iftmrh,
    smri_vol_cdk_ihcatelh, smri_vol_cdk_ihcaterh,
    smri_vol_cdk_locclh, smri_vol_cdk_loccrh,
    smri_vol_cdk_lobfrlh, smri_vol_cdk_lobfrrh,
    smri_vol_cdk_linguallh, smri_vol_cdk_lingualrh,
    smri_vol_cdk_mobfrlh, smri_vol_cdk_mobfrrh,
    smri_vol_cdk_mdtmlh, smri_vol_cdk_mdtmrh,
    smri_vol_cdk_parahpallh, smri_vol_cdk_parahpalrh,
    smri_vol_cdk_paracnlh, smri_vol_cdk_paracnrh,
    smri_vol_cdk_parsopclh, smri_vol_cdk_parsopcrh,
    smri_vol_cdk_parsobislh, smri_vol_cdk_parsobisrh,
    smri_vol_cdk_parstgrislh, smri_vol_cdk_parstgrisrh,
    smri_vol_cdk_pericclh, smri_vol_cdk_periccrh,
    smri_vol_cdk_postcnlh, smri_vol_cdk_postcnrh,
    smri_vol_cdk_ptcatelh, smri_vol_cdk_ptcaterh,
    smri_vol_cdk_precnlh, smri_vol_cdk_precnrh,
    smri_vol_cdk_pclh, smri_vol_cdk_pcrh,
    smri_vol_cdk_rracatelh, smri_vol_cdk_rracaterh,
    smri_vol_cdk_rrmdfrlh, smri_vol_cdk_rrmdfrrh,
    smri_vol_cdk_sufrlh, smri_vol_cdk_sufrrh,
    smri_vol_cdk_supllh ,smri_vol_cdk_suplrh,
    smri_vol_cdk_sutmlh, smri_vol_cdk_sutmrh,
    smri_vol_cdk_smlh, smri_vol_cdk_smrh,
    smri_vol_cdk_frpolelh, smri_vol_cdk_frpolerh,
    smri_vol_cdk_tmpolelh, smri_vol_cdk_tmpolerh,
    smri_vol_cdk_trvtmlh, smri_vol_cdk_trvtmrh,
    smri_vol_cdk_insulalh, smri_vol_cdk_insularh,
    # structural MRI - Cortical Thickness
    smri_thick_cdk_banksstslh, smri_thick_cdk_banksstsrh,
    smri_thick_cdk_cdacatelh, smri_thick_cdk_cdacaterh,
    smri_thick_cdk_cdmdfrlh, smri_thick_cdk_cdmdfrrh,
    smri_thick_cdk_cuneuslh, smri_thick_cdk_cuneusrh,
    smri_thick_cdk_ehinallh, smri_thick_cdk_ehinalrh,
    smri_thick_cdk_fusiformlh, smri_thick_cdk_fusiformrh,
    smri_thick_cdk_ifpllh, smri_thick_cdk_ifplrh,
    smri_thick_cdk_iftmlh, smri_thick_cdk_iftmrh,
    smri_thick_cdk_ihcatelh, smri_thick_cdk_ihcaterh,
    smri_thick_cdk_locclh, smri_thick_cdk_loccrh,
    smri_thick_cdk_lobfrlh, smri_thick_cdk_lobfrrh,
    smri_thick_cdk_linguallh, smri_thick_cdk_lingualrh,
    smri_thick_cdk_mobfrlh, smri_thick_cdk_mobfrrh,
    smri_thick_cdk_mdtmlh, smri_thick_cdk_mdtmrh,
    smri_thick_cdk_parahpallh, smri_thick_cdk_parahpalrh,
    smri_thick_cdk_paracnlh, smri_thick_cdk_paracnrh,
    smri_thick_cdk_parsopclh, smri_thick_cdk_parsopcrh,
    smri_thick_cdk_parsobislh, smri_thick_cdk_parsobisrh,
    smri_thick_cdk_parstgrislh, smri_thick_cdk_parstgrisrh,
    smri_thick_cdk_pericclh, smri_thick_cdk_periccrh,
    smri_thick_cdk_postcnlh, smri_thick_cdk_postcnrh,
    smri_thick_cdk_ptcatelh, smri_thick_cdk_ptcaterh,
    smri_thick_cdk_precnlh, smri_thick_cdk_precnrh,
    smri_thick_cdk_pclh, smri_thick_cdk_pcrh,
    smri_thick_cdk_rracatelh, smri_thick_cdk_rracaterh,
    smri_thick_cdk_rrmdfrlh, smri_thick_cdk_rrmdfrrh,
    smri_thick_cdk_sufrlh, smri_thick_cdk_sufrrh,
    smri_thick_cdk_supllh ,smri_thick_cdk_suplrh,
    smri_thick_cdk_sutmlh, smri_thick_cdk_sutmrh,
    smri_thick_cdk_smlh, smri_thick_cdk_smrh,
    smri_thick_cdk_frpolelh, smri_thick_cdk_frpolerh,
    smri_thick_cdk_tmpolelh, smri_thick_cdk_tmpolerh,
    smri_thick_cdk_trvtmlh, smri_thick_cdk_trvtmrh,
    smri_thick_cdk_insulalh, smri_thick_cdk_insularh,
    # structural - Sulcel Depth
    smri_sulc_cdk_banksstslh, smri_sulc_cdk_banksstsrh,
    smri_sulc_cdk_cdacatelh, smri_sulc_cdk_cdacaterh,
    smri_sulc_cdk_cdmdfrlh, smri_sulc_cdk_cdmdfrrh,
    smri_sulc_cdk_cuneuslh, smri_sulc_cdk_cuneusrh,
    smri_sulc_cdk_ehinallh, smri_sulc_cdk_ehinalrh,
    smri_sulc_cdk_fusiformlh, smri_sulc_cdk_fusiformrh,
    smri_sulc_cdk_ifpllh, smri_sulc_cdk_ifplrh,
    smri_sulc_cdk_iftmlh, smri_sulc_cdk_iftmrh,
    smri_sulc_cdk_ihcatelh, smri_sulc_cdk_ihcaterh,
    smri_sulc_cdk_locclh, smri_sulc_cdk_loccrh,
    smri_sulc_cdk_lobfrlh, smri_sulc_cdk_lobfrrh,
    smri_sulc_cdk_linguallh, smri_sulc_cdk_lingualrh,
    smri_sulc_cdk_mobfrlh, smri_sulc_cdk_mobfrrh,
    smri_sulc_cdk_mdtmlh, smri_sulc_cdk_mdtmrh,
    smri_sulc_cdk_parahpallh, smri_sulc_cdk_parahpalrh,
    smri_sulc_cdk_paracnlh, smri_sulc_cdk_paracnrh,
    smri_sulc_cdk_parsopclh, smri_sulc_cdk_parsopcrh,
    smri_sulc_cdk_parsobislh, smri_sulc_cdk_parsobisrh,
    smri_sulc_cdk_parstgrislh, smri_sulc_cdk_parstgrisrh,
    smri_sulc_cdk_pericclh, smri_sulc_cdk_periccrh,
    smri_sulc_cdk_postcnlh, smri_sulc_cdk_postcnrh,
    smri_sulc_cdk_ptcatelh, smri_sulc_cdk_ptcaterh,
    smri_sulc_cdk_precnlh, smri_sulc_cdk_precnrh,
    smri_sulc_cdk_pclh, smri_sulc_cdk_pcrh,
    smri_sulc_cdk_rracatelh, smri_sulc_cdk_rracaterh,
    smri_sulc_cdk_rrmdfrlh, smri_sulc_cdk_rrmdfrrh,
    smri_sulc_cdk_sufrlh, smri_sulc_cdk_sufrrh,
    smri_sulc_cdk_supllh ,smri_sulc_cdk_suplrh,
    smri_sulc_cdk_sutmlh, smri_sulc_cdk_sutmrh,
    smri_sulc_cdk_smlh, smri_sulc_cdk_smrh,
    smri_sulc_cdk_frpolelh, smri_sulc_cdk_frpolerh,
    smri_sulc_cdk_tmpolelh, smri_sulc_cdk_tmpolerh,
    smri_sulc_cdk_trvtmlh, smri_sulc_cdk_trvtmrh,
    smri_sulc_cdk_insulalh, smri_sulc_cdk_insularh,
    # DTI
    dmri_dtifa_fiberat_fxrh, dmri_dtifa_fiberat_fxlh,
    dmri_dtifa_fiberat_cgcrh, dmri_dtifa_fiberat_cgclh,
    dmri_dtifa_fiberat_cghrh, dmri_dtifa_fiberat_cghlh,
    dmri_dtifa_fiberat_cstrh, dmri_dtifa_fiberat_cstlh,
    dmri_dtifa_fiberat_atrrh, dmri_dtifa_fiberat_atrlh,
    dmri_dtifa_fiberat_uncrh, dmri_dtifa_fiberat_unclh,
    dmri_dtifa_fiberat_ilfrh, dmri_dtifa_fiberat_ilflh,
    dmri_dtifa_fiberat_iforh, dmri_dtifa_fiberat_ifolh,
    dmri_dtifa_fiberat_slfrh, dmri_dtifa_fiberat_slflh,
    dmri_dtifa_fiberat_tslfrh, dmri_dtifa_fiberat_tslflh,
    dmri_dtifa_fiberat_pslfrh, dmri_dtifa_fiberat_pslflh,
    dmri_dtifa_fiberat_scsrh, dmri_dtifa_fiberat_scslh,
    dmri_dtifa_fiberat_sifcrh, dmri_dtifa_fiberat_sifclh,
    dmri_dtifa_fiberat_ifsfcrh, dmri_dtifa_fiberat_ifsfclh,
    dmri_dtifa_fiberat_fmaj, dmri_dtifa_fiberat_fmin,
    dmri_dtifa_fiberat_cc)

pb_sex <- sapply(data.2_pb, cor.test, y=data.2_pb$sex_v) %>% 
  as.data.frame()
pb_eth <- sapply(data.2_pb, cor.test, y=data.2_pb$eth_v) %>% 
  as.data.frame()
pb_rec <- sapply(data.2_pb, cor.test, y=data.2_pb$rec_v) %>% 
  as.data.frame()
pb_exp <- sapply(data.2_pb, cor.test, y=data.2_pb$exp_v) %>% 
  as.data.frame()
pb_tbi <- sapply(data.2_pb, cor.test, y=data.2_pb$tbi_v) %>% 
  as.data.frame()
pb_det <- sapply(data.2_pb, cor.test, y=data.2_pb$det_v) %>% 
  as.data.frame()
pb_cct <- sapply(data.2_pb, cor.test, y=data.2_pb$cct_v) %>% 
  as.data.frame()

pb_sex_corr <- pb_sex["estimate",]
pb_sex_p <- pb_sex["p.value",] 
pb_eth_corr <- pb_eth["estimate",]
pb_eth_p <- pb_eth["p.value",] 
pb_rec_corr <- pb_rec["estimate",]
pb_rec_p <- pb_rec["p.value",] 
pb_exp_corr <- pb_exp["estimate",]
pb_exp_p <- pb_exp["p.value",] 
pb_tbi_corr <- pb_tbi["estimate",]
pb_tbi_p <- pb_tbi["p.value",] 
pb_det_corr <- pb_det["estimate",]
pb_det_p <- pb_det["p.value",] 
pb_cct_corr <- pb_cct["estimate",]
pb_cct_p <- pb_cct["p.value",] 

pb_sex_corr <- apply(
  pb_sex_corr, 2, 
  function(pb_sex_corr) as.numeric(gsub('[c(cor=)]', '', pb_sex_corr))) %>% 
  as.data.frame() %>% 
  rename(sex_corr = '.')

pb_eth_corr <- apply(
  pb_eth_corr, 2, 
  function(pb_eth_corr) as.numeric(gsub('[c(cor=)]', '', pb_eth_corr))) %>% 
  as.data.frame() %>% 
  rename(eth_corr = '.')

pb_rec_corr <- apply(
  pb_rec_corr, 2, 
  function(pb_rec_corr) as.numeric(gsub('[c(cor=)]', '', pb_rec_corr))) %>% 
  as.data.frame() %>% 
  rename(rec_corr = '.')

pb_exp_corr <- apply(
  pb_exp_corr, 2, 
  function(pb_exp_corr) as.numeric(gsub('[c(cor=)]', '', pb_exp_corr))) %>% 
  as.data.frame() %>% 
  rename(exp_corr = '.')

pb_tbi_corr <- apply(
  pb_tbi_corr, 2, 
  function(pb_tbi_corr) as.numeric(gsub('[c(cor=)]', '', pb_tbi_corr))) %>% 
  as.data.frame() %>% 
  rename(tbi_corr = '.')

pb_det_corr <- apply(
  pb_det_corr, 2, 
  function(pb_det_corr) as.numeric(gsub('[c(cor=)]', '', pb_det_corr))) %>% 
  as.data.frame() %>% 
  rename(det_corr = '.')

pb_cct_corr <- apply(
  pb_cct_corr, 2, 
  function(pb_cct_corr) as.numeric(gsub('[c(cor=)]', '', pb_cct_corr))) %>% 
  as.data.frame() %>% 
  rename(cct_corr = '.')

pb_sex_p <- as.data.frame(t(pb_sex_p)) %>% 
  rename(sex_p.value = 'p.value')
pb_eth_p <- as.data.frame(t(pb_eth_p)) %>% 
  rename(eth_p.value = 'p.value')
pb_rec_p <- as.data.frame(t(pb_rec_p)) %>% 
  rename(rec_p.value = 'p.value')
pb_exp_p <- as.data.frame(t(pb_exp_p)) %>% 
  rename(exp_p.value = 'p.value')
pb_tbi_p <- as.data.frame(t(pb_tbi_p)) %>% 
  rename(tbi_p.value = 'p.value')
pb_det_p <- as.data.frame(t(pb_det_p)) %>% 
  rename(det_p.value = 'p.value')
pb_cct_p <- as.data.frame(t(pb_cct_p)) %>% 
  rename(cct_p.value = 'p.value')

corr_pb <- cbind(pb_sex_corr, pb_sex_p, pb_eth_corr, pb_eth_p,
                 pb_rec_corr, pb_rec_p, pb_exp_corr, pb_exp_p, 
                 pb_tbi_corr, pb_tbi_p, pb_det_corr, pb_det_p, 
                 pb_cct_corr, pb_cct_p) %>% 
  rename(
    'Sex - Correlation Coefficient' = sex_corr,
    'Sex - P-Value' = sex_p.value,
    'Ethnicity - Correlation Coefficient' = eth_corr,
    'Ethnicity - P-Value' = eth_p.value,
    'Recreational Activities (binary) - Correlation Coefficient' = rec_corr,
    'Recreational Activities (binary) - P-Value' = rec_p.value,
    'Prenatal Exposure to Substance Use - Correlation Coefficient' = exp_corr,
    'Prenatal Exposure to Substance Use - P-Value' = exp_p.value,
    'TBI - Correlation Coefficient' = tbi_corr,
    'TBI - P-Value' = tbi_p.value,
    'Detention / Suspension - Correlation Coefficient' = det_corr,
    'Detention / Suspension - P-Value' = det_p.value,
    'CCT - Correlation Coefficient' = cct_corr,
    'CCT - P-Value' = cct_p.value)

row.names.remove <- c(
  'sex_v', 'eth_v', 'rec_v', 'exp_v','tbi_v', 'det_v', 'cct_v')
corr_pb <- corr_pb[!(row.names(corr_pb) %in% row.names.remove), ]

# export
corr_pb <- tibble::rownames_to_column(corr_pb, "row_names") 
corr_pb <- apply(corr_pb,2,as.character)
write.csv(corr_pb,'output/correlations/corr_pb.csv', row.names = TRUE)

rm(data.2_pb, pb_sex, pb_eth, pb_rec, pb_tbi, pb_det, pb_cct,
   pb_sex_corr, pb_sex_p, pb_eth_corr, pb_eth_p, pb_rec_corr, pb_rec_p,
   pb_exp_corr, pb_exp_p, pb_tbi_corr, pb_tbi_p, pb_det_corr, pb_det_p, 
   pb_cct_corr, pb_cct_p, 
   row.names.remove, corr_pb)


# Correlations: Tetrachoric -----------------------------------------------------

# among dichotomous variables
# - dichotomous variables (n = 7) resulting in 21 correlation pairs
# - sex, ethnicity, recreational activities (binary), prenatal substance use exposure, TBI, detention / suspension, CCT

tcorr_1 <- as.matrix(table(data.2$sex_2l, data.2$eth_hisp), nrow = 2)
tcorr_1 <- tetrachoric(tcorr_1) 
corr_tec <- tcorr_1$rho %>% 
  as.data.frame() %>% 
  rename(sex_eth_corr = '.')

tcorr_2 <- as.matrix(table(data.2$sex_2l, data.2$rec_bin), nrow = 2)
tcorr_2 <- tetrachoric(tcorr_2)
tcorr_2 <- tcorr_2$rho %>% 
  as.data.frame() %>% 
  rename(sex_rec_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_2)

tcorr_3 <- as.matrix(table(data.2$sex_2l, data.2$exp_sub), nrow = 2)
tcorr_3 <- tetrachoric(tcorr_3)
tcorr_3 <- tcorr_3$rho %>% 
  as.data.frame() %>% 
  rename(sex_exp_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_3)

tcorr_4 <- as.matrix(table(data.2$sex_2l, data.2$tbi_injury), nrow = 2)
tcorr_4 <- tetrachoric(tcorr_4)
tcorr_4 <- tcorr_4$rho %>% 
  as.data.frame() %>% 
  rename(sex_tbi_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_4)

tcorr_5 <- as.matrix(table(data.2$sex_2l, data.2$det_susp), nrow = 2)
tcorr_5 <- tetrachoric(tcorr_5)
tcorr_5 <- tcorr_5$rho %>% 
  as.data.frame() %>% 
  rename(sex_det_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_5)

tcorr_6 <- as.matrix(table(data.2$sex_2l, data.2$cct), nrow = 2)
tcorr_6 <- tetrachoric(tcorr_6)
tcorr_6 <- tcorr_6$rho %>% 
  as.data.frame() %>% 
  rename(sex_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_6)

tcorr_7 <- as.matrix(table(data.2$eth_hisp, data.2$rec_bin), nrow = 2)
tcorr_7 <- tetrachoric(tcorr_7)
tcorr_7 <- tcorr_7$rho %>% 
  as.data.frame() %>% 
  rename(eth_rec_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_7)

tcorr_8 <- as.matrix(table(data.2$eth_hisp, data.2$exp_sub), nrow = 2)
tcorr_8 <- tetrachoric(tcorr_8)
tcorr_8 <- tcorr_8$rho %>% 
  as.data.frame() %>% 
  rename(eth_exp_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_8)

tcorr_9 <- as.matrix(table(data.2$eth_hisp, data.2$tbi_injury), nrow = 2)
tcorr_9 <- tetrachoric(tcorr_9)
tcorr_9 <- tcorr_9$rho %>% 
  as.data.frame() %>% 
  rename(eth_tbi_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_9)  

tcorr_10 <- as.matrix(table(data.2$eth_hisp, data.2$det_susp), nrow = 2)
tcorr_10 <- tetrachoric(tcorr_10)
tcorr_10 <- tcorr_10$rho %>% 
  as.data.frame() %>% 
  rename(eth_det_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_10)

tcorr_11 <- as.matrix(table(data.2$eth_hisp, data.2$cct), nrow = 2)
tcorr_11 <- tetrachoric(tcorr_11)
tcorr_11 <- tcorr_11$rho %>% 
  as.data.frame() %>% 
  rename(eth_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_11)

tcorr_12 <- as.matrix(table(data.2$rec_bin, data.2$exp_sub), nrow = 2)
tcorr_12 <- tetrachoric(tcorr_12)
tcorr_12 <- tcorr_12$rho %>% 
  as.data.frame() %>% 
  rename(rec_exp_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_12)

tcorr_13 <- as.matrix(table(data.2$rec_bin, data.2$tbi_injury), nrow = 2)
tcorr_13 <- tetrachoric(tcorr_13)
tcorr_13 <- tcorr_13$rho %>% 
  as.data.frame() %>% 
  rename(rec_tbi_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_13)

tcorr_14 <- as.matrix(table(data.2$rec_bin, data.2$det_susp), nrow = 2)
tcorr_14 <- tetrachoric(tcorr_14)
tcorr_14 <- tcorr_14$rho %>% 
  as.data.frame() %>% 
  rename(rec_det_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_14)

tcorr_15 <- as.matrix(table(data.2$rec_bin, data.2$cct), nrow = 2)
tcorr_15 <- tetrachoric(tcorr_15)
tcorr_15 <- tcorr_15$rho %>% 
  as.data.frame() %>% 
  rename(rec_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_15)

tcorr_16 <- as.matrix(table(data.2$exp_sub, data.2$tbi_injury), nrow = 2)
tcorr_16 <- tetrachoric(tcorr_16)
tcorr_16 <- tcorr_16$rho %>% 
  as.data.frame() %>% 
  rename(exp_tbi_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_16)

tcorr_17 <- as.matrix(table(data.2$exp_sub, data.2$det_susp), nrow = 2)
tcorr_17 <- tetrachoric(tcorr_17)
tcorr_17 <- tcorr_17$rho %>% 
  as.data.frame() %>% 
  rename(exp_det_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_17)

tcorr_18 <- as.matrix(table(data.2$exp_sub, data.2$cct), nrow = 2)
tcorr_18 <- tetrachoric(tcorr_18)
tcorr_18 <- tcorr_18$rho %>% 
  as.data.frame() %>% 
  rename(exp_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_18)

tcorr_19 <- as.matrix(table(data.2$tbi_injury, data.2$det_susp), nrow = 2)
tcorr_19 <- tetrachoric(tcorr_19)
tcorr_19 <- tcorr_19$rho %>% 
  as.data.frame() %>% 
  rename(tbi_det_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_19)

tcorr_20 <- as.matrix(table(data.2$tbi_injury, data.2$cct), nrow = 2)
tcorr_20 <- tetrachoric(tcorr_20)
tcorr_20 <- tcorr_20$rho %>% 
  as.data.frame() %>% 
  rename(tbi_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_20)

tcorr_21 <- as.matrix(table(data.2$det_susp, data.2$cct), nrow = 2)
tcorr_21 <- tetrachoric(tcorr_21)
tcorr_21 <- tcorr_21$rho %>% 
  as.data.frame() %>% 
  rename(det_cct_corr = '.')
corr_tec <- cbind(corr_tec, tcorr_21)

# export
write.csv(corr_tec,'output/correlations/corr_tec.csv', row.names = TRUE)
rm(tcorr_1, tcorr_2, tcorr_3, tcorr_4, tcorr_5, tcorr_6, tcorr_7, 
   tcorr_8, tcorr_9, tcorr_10, tcorr_11, tcorr_12, tcorr_13, tcorr_14, 
   tcorr_15, tcorr_16, tcorr_17, tcorr_18, tcorr_19, tcorr_20, tcorr_21,    
   corr_tec)


# Correlations: Cramer's V --------------------------------------------------------------

# correlation between Dichotomous and Categorical (>2 categories)
# dichotomous variables: n = 7
# - sex, ethnicity, recreational activities (binary), prenatal substance use exposure, TBI, detention / suspension, CCT
# categorical (>2 categories) variables: n = 6
# - race, special education services, grades, income, religion, parent education


#------------------------------------------------------------------------------#
#                       sex + categorical variables                            #
#------------------------------------------------------------------------------#

vcor_1a <- as.matrix(table(data.2$sex_2l, data.2$race_4l), nrow = 2)
vcor_1a <- cramerV(vcor_1a, ci = TRUE, conf = 0.95, type = "perc", R = 1000) 
corr_v <- as.data.frame(t(vcor_1a)) %>% 
  rename(sex_race = 'V1')

vcor_2a <- as.matrix(table(data.2$sex_2l, data.2$se_services), nrow = 2)
vcor_2a <- cramerV(vcor_2a, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2a <- as.data.frame(t(vcor_2a)) %>% 
  rename(sex_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2a)

vcor_3a <- as.matrix(table(data.2$sex_2l, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3a <- cramerV(vcor_3a, ci = TRUE, conf = 0.95, type = "perc", R = 1000) 
corr_v <- as.data.frame(t(vcor_3a)) %>% 
  rename(sex_grades = 'V1')

vcor_4a <- as.matrix(table(data.2$sex_2l, data.2$income), nrow = 2)
vcor_4a <- cramerV(vcor_4a, ci = TRUE, conf = 0.95, type = "perc", R = 1000) 
corr_v <- as.data.frame(t(vcor_4a)) %>% 
  rename(sex_income = 'V1')

vcor_5a <- as.matrix(table(data.2$sex_2l, data.2$religion), nrow = 2)
vcor_5a <- cramerV(vcor_5a, ci = TRUE, conf = 0.95, type = "perc", R = 1000) 
corr_v <- as.data.frame(t(vcor_5a)) %>% 
  rename(sex_religion = 'V1')

vcor_6a <- as.matrix(table(data.2$sex_2l, data.2$p_edu), nrow = 2)
vcor_6a <- cramerV(vcor_6a, ci = TRUE, conf = 0.95, type = "perc", R = 1000) 
corr_v <- as.data.frame(t(vcor_6a)) %>% 
  rename(sex_religion = 'V1')

#------------------------------------------------------------------------------#
#                 ethnicity + categorical variables                            #
#------------------------------------------------------------------------------#

vcor_1b <- as.matrix(table(data.2$eth_hisp, data.2$race_4l), nrow = 2)
vcor_1b <- cramerV(vcor_1b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1b <- as.data.frame(t(vcor_1b)) %>% 
  rename(eth_race = 'V1')
corr_v <- cbind(corr_v, vcor_1b)

vcor_2b <- as.matrix(table(data.2$eth_hisp, data.2$se_services), nrow = 2)
vcor_2b <- cramerV(vcor_2b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2b <- as.data.frame(t(vcor_2b)) %>% 
  rename(eth_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2b)

vcor_3b <- as.matrix(table(data.2$eth_hisp, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3b <- cramerV(vcor_3b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3b <- as.data.frame(t(vcor_3b)) %>% 
  rename(eth_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3b)

vcor_4b <- as.matrix(table(data.2$eth_hisp, data.2$income), nrow = 2)
vcor_4b <- cramerV(vcor_4b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4b <- as.data.frame(t(vcor_4b)) %>% 
  rename(eth_income = 'V1')
corr_v <- cbind(corr_v, vcor_4b)

vcor_5b <- as.matrix(table(data.2$eth_hisp, data.2$religion), nrow = 2)
vcor_5b <- cramerV(vcor_5b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5b <- as.data.frame(t(vcor_5b)) %>% 
  rename(eth_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5b)

vcor_6b <- as.matrix(table(data.2$eth_hisp, data.2$p_edu), nrow = 2)
vcor_6b <- cramerV(vcor_6b, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6b <- as.data.frame(t(vcor_6b)) %>% 
  rename(eth_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6b)

#------------------------------------------------------------------------------#
#               recreational activities + categorical variables                #
#------------------------------------------------------------------------------#  

vcor_1c <- as.matrix(table(data.2$rec_bin, data.2$race_4l), nrow = 2)
vcor_1c <- cramerV(vcor_1c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1c <- as.data.frame(t(vcor_1c)) %>% 
  rename(rec_race = 'V1')
corr_v <- cbind(corr_v, vcor_1c)

vcor_2c <- as.matrix(table(data.2$rec_bin, data.2$se_services), nrow = 2)
vcor_2c <- cramerV(vcor_2c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2c <- as.data.frame(t(vcor_2c)) %>% 
  rename(rec_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2c)

vcor_3c <- as.matrix(table(data.2$rec_bin, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3c <- cramerV(vcor_3c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3c <- as.data.frame(t(vcor_3c)) %>% 
  rename(rec_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3c)

vcor_4c <- as.matrix(table(data.2$rec_bin, data.2$income), nrow = 2)
vcor_4c <- cramerV(vcor_4c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4c <- as.data.frame(t(vcor_4c)) %>% 
  rename(rec_income = 'V1')
corr_v <- cbind(corr_v, vcor_4c)

vcor_5c <- as.matrix(table(data.2$rec_bin, data.2$religion), nrow = 2)
vcor_5c <- cramerV(vcor_5c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5c <- as.data.frame(t(vcor_5c)) %>% 
  rename(rec_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5c)

vcor_6c <- as.matrix(table(data.2$rec_bin, data.2$p_edu), nrow = 2)
vcor_6c <- cramerV(vcor_6c, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6c <- as.data.frame(t(vcor_6c)) %>% 
  rename(rec_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6c)

#------------------------------------------------------------------------------#
#             prenatal substance use exposure + categorical variables          #
#------------------------------------------------------------------------------#

vcor_1d <- as.matrix(table(data.2$exp_sub, data.2$race_4l), nrow = 2)
vcor_1d <- cramerV(vcor_1d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1d <- as.data.frame(t(vcor_1d)) %>% 
  rename(exp_race = 'V1')
corr_v <- cbind(corr_v, vcor_1d)

vcor_2d <- as.matrix(table(data.2$exp_sub, data.2$se_services), nrow = 2)
vcor_2d <- cramerV(vcor_2d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2d <- as.data.frame(t(vcor_2d)) %>% 
  rename(exp_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2d)

vcor_3d <- as.matrix(table(data.2$exp_sub, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3d <- cramerV(vcor_3d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3d <- as.data.frame(t(vcor_3d)) %>% 
  rename(exp_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3d)

vcor_4d <- as.matrix(table(data.2$exp_sub, data.2$income), nrow = 2)
vcor_4d <- cramerV(vcor_4d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4d <- as.data.frame(t(vcor_4d)) %>% 
  rename(exp_income = 'V1')
corr_v <- cbind(corr_v, vcor_4d)

vcor_5d <- as.matrix(table(data.2$exp_sub, data.2$religion), nrow = 2)
vcor_5d <- cramerV(vcor_5d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5d <- as.data.frame(t(vcor_5d)) %>% 
  rename(exp_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5d)

vcor_6d <- as.matrix(table(data.2$exp_sub, data.2$p_edu), nrow = 2)
vcor_6d <- cramerV(vcor_6d, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6d <- as.data.frame(t(vcor_6d)) %>% 
  rename(exp_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6d)

#------------------------------------------------------------------------------#
#                       TBI + categorical variables                            #
#------------------------------------------------------------------------------#   

vcor_1e <- as.matrix(table(data.2$tbi_injury, data.2$race_4l), nrow = 2)
vcor_1e <- cramerV(vcor_1e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1e <- as.data.frame(t(vcor_1e)) %>% 
  rename(tbi_race = 'V1')
corr_v <- cbind(corr_v, vcor_1e)

vcor_2e <- as.matrix(table(data.2$tbi_injury, data.2$se_services), nrow = 2)
vcor_2e <- cramerV(vcor_2e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2e <- as.data.frame(t(vcor_2e)) %>% 
  rename(tbi_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2e)

vcor_3e <- as.matrix(table(data.2$tbi_injury, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3e <- cramerV(vcor_3e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3e <- as.data.frame(t(vcor_3e)) %>% 
  rename(tbi_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3e)

vcor_4e <- as.matrix(table(data.2$tbi_injury, data.2$income), nrow = 2)
vcor_4e <- cramerV(vcor_4e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4e <- as.data.frame(t(vcor_4e)) %>% 
  rename(tbi_income = 'V1')
corr_v <- cbind(corr_v, vcor_4e)

vcor_5e <- as.matrix(table(data.2$tbi_injury, data.2$religion), nrow = 2)
vcor_5e <- cramerV(vcor_5e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5e <- as.data.frame(t(vcor_5e)) %>% 
  rename(tbi_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5e)

vcor_6e <- as.matrix(table(data.2$tbi_injury, data.2$p_edu), nrow = 2)
vcor_6e <- cramerV(vcor_6e, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6e <- as.data.frame(t(vcor_6e)) %>% 
  rename(tbi_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6e)

#------------------------------------------------------------------------------#
#               detention / suspension + categorical variables                 #
#------------------------------------------------------------------------------#

vcor_1f <- as.matrix(table(data.2$det_susp, data.2$race_4l), nrow = 2)
vcor_1f <- cramerV(vcor_1f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1f <- as.data.frame(t(vcor_1f)) %>% 
  rename(det_race = 'V1')
corr_v <- cbind(corr_v, vcor_1f)

vcor_2f <- as.matrix(table(data.2$det_susp, data.2$se_services), nrow = 2)
vcor_2f <- cramerV(vcor_2f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2f <- as.data.frame(t(vcor_2f)) %>% 
  rename(det_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2f)

vcor_3f <- as.matrix(table(data.2$det_susp, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3f <- cramerV(vcor_3f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3f <- as.data.frame(t(vcor_3f)) %>% 
  rename(det_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3f)

vcor_4f <- as.matrix(table(data.2$det_susp, data.2$income), nrow = 2)
vcor_4f <- cramerV(vcor_4f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4f <- as.data.frame(t(vcor_4f)) %>% 
  rename(det_income = 'V1')
corr_v <- cbind(corr_v, vcor_4f)

vcor_5f <- as.matrix(table(data.2$det_susp, data.2$religion), nrow = 2)
vcor_5f <- cramerV(vcor_5f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5f <- as.data.frame(t(vcor_5f)) %>% 
  rename(det_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5f)

vcor_6f <- as.matrix(table(data.2$det_susp, data.2$p_edu), nrow = 2)
vcor_6f <- cramerV(vcor_6f, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6f <- as.data.frame(t(vcor_6f)) %>% 
  rename(det_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6f)

#------------------------------------------------------------------------------#
#                       CCT + categorical variables                            #
#------------------------------------------------------------------------------#  

vcor_1g <- as.matrix(table(data.2$cct, data.2$race_4l), nrow = 2)
vcor_1g <- cramerV(vcor_1g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_1g <- as.data.frame(t(vcor_1g)) %>% 
  rename(cct_race = 'V1')
corr_v <- cbind(corr_v, vcor_1g)

vcor_2g <- as.matrix(table(data.2$cct, data.2$se_services), nrow = 2)
vcor_2g <- cramerV(vcor_2g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_2g <- as.data.frame(t(vcor_2g)) %>% 
  rename(cct_ses = 'V1')
corr_v <- cbind(corr_v, vcor_2g)

vcor_3g <- as.matrix(table(data.2$cct, data.2$kbi_p_grades_in_school), nrow = 2)
vcor_3g <- cramerV(vcor_3g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_3g <- as.data.frame(t(vcor_3g)) %>% 
  rename(cct_grades = 'V1')
corr_v <- cbind(corr_v, vcor_3g)

vcor_4g <- as.matrix(table(data.2$cct, data.2$income), nrow = 2)
vcor_4g <- cramerV(vcor_4g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_4g <- as.data.frame(t(vcor_4g)) %>% 
  rename(cct_income = 'V1')
corr_v <- cbind(corr_v, vcor_4g)

vcor_5g <- as.matrix(table(data.2$cct, data.2$religion), nrow = 2)
vcor_5g <- cramerV(vcor_5g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_5g <- as.data.frame(t(vcor_5g)) %>% 
  rename(cct_religion = 'V1')
corr_v <- cbind(corr_v, vcor_5g)

vcor_6g <- as.matrix(table(data.2$cct, data.2$p_edu), nrow = 2)
vcor_6g <- cramerV(vcor_6g, ci = TRUE, conf = 0.95, type = "perc", R = 1000)
vcor_6g <- as.data.frame(t(vcor_6g)) %>% 
  rename(cct_religion = 'V1')
corr_v <- cbind(corr_v, vcor_6g)

# export
write.csv(corr_v,'output/correlations/corr_v.csv', row.names = TRUE)
rm(vcor_1a, vcor_2a, vcor_3a, vcor_4a, vcor_5a, vcor_6a, 
   vcor_1b, vcor_2b, vcor_3b, vcor_4b, vcor_5b, vcor_6b, 
   vcor_1c, vcor_2c, vcor_3c, vcor_4c, vcor_5c, vcor_6c, 
   vcor_1d, vcor_2d, vcor_3d, vcor_4d, vcor_5d, vcor_6d, 
   vcor_1e, vcor_2e, vcor_3e, vcor_4e, vcor_5e, vcor_6e, 
   vcor_1f, vcor_2f, vcor_3f, vcor_4f, vcor_5f, vcor_6f, 
   vcor_1g, vcor_2g, vcor_3g, vcor_4g, vcor_5g, vcor_6g, 
   corr_v)



