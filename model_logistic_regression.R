
# Note: All logistic regressions performed on split(1), center and scaled imputed data

# Logistic Regression: Model 1 -------------------------------------------------

#------------------------------------------------------------------------------#
#                               initial model
#------------------------------------------------------------------------------#

fit_log_m1 <- with(
  trn_1_mice_m1,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_aggressive_r +
        cbcl_scr_syn_internal_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs,
      
      family = 'binomial'))

fit_log_m1_summary <- summary(pool(fit_log_m1), 
                              conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value)

# model follow-up
if (any(fit_log_m1_summary$Upper_CI == 'Inf') || any(fit_log_m1_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}  

# infinite upper CI for: 
# - cbcl_scr_syn_anxdep_r
# - cbcl_scr_syn_somatic_r
# - cbcl_scr_syn_aggressive_r
# - cbcl_scr_syn_internal_r
# - cbcl_scr_syn_external_r

#------------------------------------------------------------------------------#
#                         correlation: CBCL subscales
#------------------------------------------------------------------------------#

cbcl <- data.2 %>% 
  select(starts_with('cbcl'))

corr_rank <- rcorr(as.matrix(cbcl))
corr_rank <- corr_rank$r
corr_rank <- corr_rank %>% 
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value) %>% 
  ungroup() %>% 
  subset(value != 1) # exclude diagonal of correlation matrix
corr_rank

rm(cbcl, corr_rank)

# highest correlations: 
# - aggressive & external (.98), internal & anxiety-depression (.90)

#------------------------------------------------------------------------------#
#                             final model
#------------------------------------------------------------------------------#

# w/o CBCL aggressive and internal subscale due to high correlation 

fit_log_m1 <- with(
  trn_1_mice_m1,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs,
      
      family = 'binomial'))

fit_log_m1_summary <- summary(pool(fit_log_m1), 
                              conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  

# model follow-up
if (any(fit_log_m1_summary$Upper_CI == 'Inf') || any(fit_log_m1_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}  

write.csv(fit_log_m1_summary,'output/logistic_regression/fit_log_m1_summary.csv')
rm(fit_log_m1_summary)

# note: final logistic regression excludes CBCL aggressive and CBCL internal

#------------------------------------------------------------------------------#
#                       final model w/interaction terms
#------------------------------------------------------------------------------#

# interaction terms:
# - sex x puberty status.  
# - sex x race
# - sex x parent rules about substance use
# -	puberty status x race

fit_log_int_m1 <- with(
  trn_1_mice_m1,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs +
        
        # interaction terms:
        sex_2l_Female * pds +
        sex_2l_Female * race_4l_Asian + sex_2l_Female * race_4l_Black + sex_2l_Female * race_4l_Other_MultiRacial +
        sex_2l_Female * par_rules +
        pds * race_4l_Asian + pds * race_4l_Black + pds * race_4l_Other_MultiRacial, 
      
      family = 'binomial'))

fit_log_int_m1_summary <- summary(pool(fit_log_int_m1), 
                                  conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  %>% 
  mutate(
    table_name = ifelse(
      variable == 'sex_2l_Female:pds', 
      'Interaction: Sex (Female) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:sex_2l_Female', 
      'Interaction: Race (Asian) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:sex_2l_Female', 
      'Interaction: Race (Black) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:sex_2l_Female', 
      'Interaction: Race (Other/Multi-Racial) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'par_rules:sex_2l_Female', 
      'Interaction: Parent Rules x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:pds', 
      'Interaction: Race (Asian) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:pds', 
      'Interaction: Race (Black) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:pds', 
      'Interaction: Race (Other/Multi-Racial) x PDS', table_name),
    
    domain_name = ifelse(
      substr(table_name, 1, 5) == 'Inter', 'Interaction Terms', domain_name))

# model follow-up
if (any(fit_log_int_m1_summary$Upper_CI == 'Inf') || any(fit_log_int_m1_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}

write.csv(fit_log_int_m1_summary,'output/logistic_regression/fit_log_int_m1_summary.csv')
rm(fit_log_int_m1_summary)


# Logistic Regression: Model 2 -------------------------------------------------

#------------------------------------------------------------------------------#
#                             final model
#------------------------------------------------------------------------------#

# w/o CBCL aggressive and internal subscale due to high correlation 

fit_log_m2 <- with(
  trn_1_mice_m2,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs +
        
        # Domain: Hormones
        hormone_scr_ert_mean + hormone_scr_dhea_mean +
        
        # Domain: Neurocognitive Factors
        pea_ravlt_learn + pea_ravlt_sd_trial_vi_tc + 
        pea_ravlt_ld_trial_vii_tc + 
        pea_wiscv_tss + 
        cct_Immediate +                         
        lmt_acc + lmt_scr_rt_correct + lmt_scr_efficiency +
        nihtbx_picvocab_agecorrected + nihtbx_flanker_agecorrected +
        nihtbx_list_agecorrected + nihtbx_cardsort_agecorrected + 
        nihtbx_pattern_agecorrected + nihtbx_picture_agecorrected + 
        nihtbx_reading_agecorrected,
      
      family = 'binomial'))

fit_log_m2_summary <- summary(pool(fit_log_m2), 
                              conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  

# model follow-up
if (any(fit_log_m2_summary$Upper_CI == 'Inf') || any(fit_log_m2_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}  

write.csv(fit_log_m2_summary,'output/logistic_regression/fit_log_m2_summary.csv')
rm(fit_log_m2_summary)

#------------------------------------------------------------------------------#
#                     final model w/interaction terms
#------------------------------------------------------------------------------#

# interaction terms:
# - sex x puberty status.  
# - sex x race
# - sex x parent rules about substance use
# -	puberty status x race

fit_log_int_m2 <- with(
  trn_1_mice_m2,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs +
        
        # Domain: Hormones
        hormone_scr_ert_mean + hormone_scr_dhea_mean +
        
        # Domain: Neurocognitive Factors
        pea_ravlt_learn + pea_ravlt_sd_trial_vi_tc + 
        pea_ravlt_ld_trial_vii_tc + 
        pea_wiscv_tss + 
        cct_Immediate +                         
        lmt_acc + lmt_scr_rt_correct + lmt_scr_efficiency +
        nihtbx_picvocab_agecorrected + nihtbx_flanker_agecorrected +
        nihtbx_list_agecorrected + nihtbx_cardsort_agecorrected + 
        nihtbx_pattern_agecorrected + nihtbx_picture_agecorrected + 
        nihtbx_reading_agecorrected + 
        
        # interaction terms:
        sex_2l_Female * pds +
        sex_2l_Female * race_4l_Asian + sex_2l_Female * race_4l_Black + sex_2l_Female * race_4l_Other_MultiRacial +
        sex_2l_Female * par_rules +
        pds * race_4l_Asian + pds * race_4l_Black + pds * race_4l_Other_MultiRacial, 
      
      family = 'binomial'))

fit_log_int_m2_summary <- summary(pool(fit_log_int_m2), 
                                  conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  %>% 
  mutate(
    table_name = ifelse(
      variable == 'sex_2l_Female:pds', 
      'Interaction: Sex (Female) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:sex_2l_Female', 
      'Interaction: Race (Asian) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:sex_2l_Female', 
      'Interaction: Race (Black) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:sex_2l_Female', 
      'Interaction: Race (Other/Multi-Racial) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'par_rules:sex_2l_Female', 
      'Interaction: Parent Rules x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:pds', 
      'Interaction: Race (Asian) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:pds', 
      'Interaction: Race (Black) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:pds', 
      'Interaction: Race (Other/Multi-Racial) x PDS', table_name),
    
    domain_name = ifelse(
      substr(table_name, 1, 5) == 'Inter', 'Interaction Terms', domain_name))

# model follow-up
if (any(fit_log_int_m2_summary$Upper_CI == 'Inf') || any(fit_log_int_m2_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}

write.csv(fit_log_int_m2_summary,'output/logistic_regression/fit_log_int_m2_summary.csv')
rm(fit_log_int_m2_summary)


# Logistic Regression: Model 3 -------------------------------------------------

#------------------------------------------------------------------------------#
#                             final model
#------------------------------------------------------------------------------#

# w/o CBCL aggressive and internal subscale due to high correlation 

fit_log_m3 <- with(
  trn_1_mice_m3,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs +
        
        # Domain: Hormones
        hormone_scr_ert_mean + hormone_scr_dhea_mean +
        
        # Domain: Neurocognitive Factors
        pea_ravlt_learn + pea_ravlt_sd_trial_vi_tc + 
        pea_ravlt_ld_trial_vii_tc + 
        pea_wiscv_tss + 
        cct_Immediate +                         
        lmt_acc + lmt_scr_rt_correct + lmt_scr_efficiency +
        nihtbx_picvocab_agecorrected + nihtbx_flanker_agecorrected +
        nihtbx_list_agecorrected + nihtbx_cardsort_agecorrected + 
        nihtbx_pattern_agecorrected + nihtbx_picture_agecorrected + 
        nihtbx_reading_agecorrected +
        
        # Domain: Neuroimaging
        # sMRI 
        smri_area_cdk_banksstslh + smri_sulc_cdk_banksstslh + smri_thick_cdk_banksstslh + smri_vol_cdk_banksstslh +
        smri_area_cdk_banksstsrh + smri_sulc_cdk_banksstsrh + smri_thick_cdk_banksstsrh + smri_vol_cdk_banksstsrh +
        
        smri_area_cdk_cdacatelh + smri_sulc_cdk_cdacatelh + smri_thick_cdk_cdacatelh + smri_vol_cdk_cdacatelh + 
        smri_area_cdk_cdacaterh + smri_sulc_cdk_cdacaterh + smri_thick_cdk_cdacaterh + smri_vol_cdk_cdacaterh +
        
        smri_area_cdk_cdmdfrlh + smri_sulc_cdk_cdmdfrlh + smri_thick_cdk_cdmdfrlh + smri_vol_cdk_cdmdfrlh +
        smri_area_cdk_cdmdfrrh + smri_sulc_cdk_cdmdfrrh + smri_thick_cdk_cdmdfrrh + smri_vol_cdk_cdmdfrrh +
        
        smri_area_cdk_cuneuslh + smri_sulc_cdk_cuneuslh + smri_thick_cdk_cuneuslh + smri_vol_cdk_cuneuslh +
        smri_area_cdk_cuneusrh + smri_sulc_cdk_cuneusrh + smri_thick_cdk_cuneusrh + smri_vol_cdk_cuneusrh +
        
        smri_area_cdk_ehinallh + smri_sulc_cdk_ehinallh + smri_thick_cdk_ehinallh + smri_vol_cdk_ehinallh +
        smri_area_cdk_ehinalrh + smri_sulc_cdk_ehinalrh + smri_thick_cdk_ehinalrh + smri_vol_cdk_ehinalrh +
        
        smri_area_cdk_fusiformlh + smri_sulc_cdk_fusiformlh + smri_thick_cdk_fusiformlh + smri_vol_cdk_fusiformlh +
        smri_area_cdk_fusiformrh + smri_sulc_cdk_fusiformrh + smri_thick_cdk_fusiformrh + smri_vol_cdk_fusiformrh +
        
        smri_area_cdk_ifpllh + smri_sulc_cdk_ifpllh + smri_thick_cdk_ifpllh + smri_vol_cdk_ifpllh +
        smri_area_cdk_ifplrh + smri_sulc_cdk_ifplrh + smri_thick_cdk_ifplrh + smri_vol_cdk_ifplrh +
        
        smri_area_cdk_iftmlh + smri_sulc_cdk_iftmlh + smri_thick_cdk_iftmlh + smri_vol_cdk_iftmlh +
        smri_area_cdk_iftmrh + smri_sulc_cdk_iftmrh + smri_thick_cdk_iftmrh + smri_vol_cdk_iftmrh + 
        
        smri_area_cdk_ihcatelh + smri_sulc_cdk_ihcatelh + smri_thick_cdk_ihcatelh + smri_vol_cdk_ihcatelh +
        smri_area_cdk_ihcaterh + smri_sulc_cdk_ihcaterh + smri_thick_cdk_ihcaterh + smri_vol_cdk_ihcaterh +
        
        smri_area_cdk_locclh + smri_sulc_cdk_locclh + smri_thick_cdk_locclh + smri_vol_cdk_locclh +
        smri_area_cdk_loccrh + smri_sulc_cdk_loccrh + smri_thick_cdk_loccrh + smri_vol_cdk_loccrh +
        
        smri_area_cdk_lobfrlh + smri_sulc_cdk_lobfrlh + smri_thick_cdk_lobfrlh + smri_vol_cdk_lobfrlh + 
        smri_area_cdk_lobfrrh + smri_sulc_cdk_lobfrrh + smri_thick_cdk_lobfrrh + smri_vol_cdk_lobfrrh +
        
        smri_area_cdk_linguallh + smri_sulc_cdk_linguallh + smri_thick_cdk_linguallh + smri_vol_cdk_linguallh +
        smri_area_cdk_lingualrh + smri_sulc_cdk_lingualrh + smri_thick_cdk_lingualrh + smri_vol_cdk_lingualrh + 
        
        smri_area_cdk_mobfrlh + smri_sulc_cdk_mobfrlh + smri_thick_cdk_mobfrlh + smri_vol_cdk_mobfrlh +
        smri_area_cdk_mobfrrh + smri_sulc_cdk_mobfrrh + smri_thick_cdk_mobfrrh + smri_vol_cdk_mobfrrh + 
        
        smri_area_cdk_mdtmlh + smri_sulc_cdk_mdtmlh + smri_thick_cdk_mdtmlh + smri_vol_cdk_mdtmlh +
        smri_area_cdk_mdtmrh + smri_sulc_cdk_mdtmrh + smri_thick_cdk_mdtmrh + smri_vol_cdk_mdtmrh + 
        
        smri_area_cdk_parahpallh + smri_sulc_cdk_parahpallh + smri_thick_cdk_parahpallh + smri_vol_cdk_parahpallh +
        smri_area_cdk_parahpalrh + smri_sulc_cdk_parahpalrh + smri_thick_cdk_parahpalrh + smri_vol_cdk_parahpalrh +
        
        smri_area_cdk_paracnlh + smri_sulc_cdk_paracnlh + smri_thick_cdk_paracnlh + smri_vol_cdk_paracnlh +
        smri_area_cdk_paracnrh + smri_sulc_cdk_paracnrh + smri_thick_cdk_paracnrh + smri_vol_cdk_paracnrh + 
        
        smri_area_cdk_parsopclh + smri_sulc_cdk_parsopclh + smri_thick_cdk_parsopclh + smri_vol_cdk_parsopclh +
        smri_area_cdk_parsopcrh + smri_sulc_cdk_parsopcrh + smri_thick_cdk_parsopcrh + smri_vol_cdk_parsopcrh + 
        
        smri_area_cdk_parsobislh + smri_sulc_cdk_parsobislh + smri_thick_cdk_parsobislh + smri_vol_cdk_parsobislh +
        smri_area_cdk_parsobisrh + smri_sulc_cdk_parsobisrh + smri_thick_cdk_parsobisrh + smri_vol_cdk_parsobisrh + 
        
        smri_area_cdk_parstgrislh + smri_sulc_cdk_parstgrislh + smri_thick_cdk_parstgrislh + smri_vol_cdk_parstgrislh +
        smri_area_cdk_parstgrisrh + smri_sulc_cdk_parstgrisrh + smri_thick_cdk_parstgrisrh + smri_vol_cdk_parstgrisrh +
        
        smri_area_cdk_pericclh + smri_sulc_cdk_pericclh + smri_thick_cdk_pericclh + smri_vol_cdk_pericclh +
        smri_area_cdk_periccrh + smri_sulc_cdk_periccrh + smri_thick_cdk_periccrh + smri_vol_cdk_periccrh +
        
        smri_area_cdk_postcnlh + smri_sulc_cdk_postcnlh + smri_thick_cdk_postcnlh + smri_vol_cdk_postcnlh + 
        smri_area_cdk_postcnrh + smri_sulc_cdk_postcnrh + smri_thick_cdk_postcnrh + smri_vol_cdk_postcnrh + 
        
        smri_area_cdk_ptcatelh + smri_sulc_cdk_ptcatelh + smri_thick_cdk_ptcatelh + smri_vol_cdk_ptcatelh + 
        smri_area_cdk_ptcaterh + smri_sulc_cdk_ptcaterh + smri_thick_cdk_ptcaterh + smri_vol_cdk_ptcaterh + 
        
        smri_area_cdk_precnlh + smri_sulc_cdk_precnlh + smri_thick_cdk_precnlh + smri_vol_cdk_precnlh + 
        smri_area_cdk_precnrh + smri_sulc_cdk_precnrh + smri_thick_cdk_precnrh + smri_vol_cdk_precnrh + 
        
        smri_area_cdk_pclh + smri_sulc_cdk_pclh + smri_thick_cdk_pclh + smri_vol_cdk_pclh +
        smri_area_cdk_pcrh + smri_sulc_cdk_pcrh + smri_thick_cdk_pcrh + smri_vol_cdk_pcrh +
        
        smri_area_cdk_rracatelh + smri_sulc_cdk_rracatelh + smri_thick_cdk_rracatelh + smri_vol_cdk_rracatelh + 
        smri_area_cdk_rracaterh + smri_sulc_cdk_rracaterh + smri_thick_cdk_rracaterh + smri_vol_cdk_rracaterh + 
        
        smri_area_cdk_rrmdfrlh + smri_sulc_cdk_rrmdfrlh + smri_thick_cdk_rrmdfrlh + smri_vol_cdk_rrmdfrlh +
        smri_area_cdk_rrmdfrrh + smri_sulc_cdk_rrmdfrrh + smri_thick_cdk_rrmdfrrh + smri_vol_cdk_rrmdfrrh +
        
        smri_area_cdk_sufrlh + smri_sulc_cdk_sufrlh + smri_thick_cdk_sufrlh + smri_vol_cdk_sufrlh +
        smri_area_cdk_sufrrh + smri_sulc_cdk_sufrrh + smri_thick_cdk_sufrrh + smri_vol_cdk_sufrrh +
        
        smri_area_cdk_supllh + smri_sulc_cdk_supllh + smri_thick_cdk_supllh + smri_vol_cdk_supllh + 
        smri_area_cdk_suplrh + smri_sulc_cdk_suplrh + smri_thick_cdk_suplrh + smri_vol_cdk_suplrh + 
        
        smri_area_cdk_sutmlh + smri_sulc_cdk_sutmlh + smri_thick_cdk_sutmlh + smri_vol_cdk_sutmlh +
        smri_area_cdk_sutmrh + smri_sulc_cdk_sutmrh + smri_thick_cdk_sutmrh + smri_vol_cdk_sutmrh +
        
        smri_area_cdk_smlh + smri_sulc_cdk_smlh + smri_thick_cdk_smlh + smri_vol_cdk_smlh + 
        smri_area_cdk_smrh + smri_sulc_cdk_smrh + smri_thick_cdk_smrh + smri_vol_cdk_smrh + 
        
        smri_area_cdk_frpolelh + smri_sulc_cdk_frpolelh + smri_thick_cdk_frpolelh + smri_vol_cdk_frpolelh +
        smri_area_cdk_frpolerh + smri_sulc_cdk_frpolerh + smri_thick_cdk_frpolerh + smri_vol_cdk_frpolerh +
        
        smri_area_cdk_tmpolelh + smri_sulc_cdk_tmpolelh + smri_thick_cdk_tmpolelh + smri_vol_cdk_tmpolelh + 
        smri_area_cdk_tmpolerh + smri_sulc_cdk_tmpolerh + smri_thick_cdk_tmpolerh + smri_vol_cdk_tmpolerh + 
        
        smri_area_cdk_trvtmlh + smri_sulc_cdk_trvtmlh + smri_thick_cdk_trvtmlh + smri_vol_cdk_trvtmlh +
        smri_area_cdk_trvtmrh + smri_sulc_cdk_trvtmrh + smri_thick_cdk_trvtmrh + smri_vol_cdk_trvtmrh +
        
        smri_area_cdk_insulalh + smri_sulc_cdk_insulalh + smri_thick_cdk_insulalh + smri_vol_cdk_insulalh +
        smri_area_cdk_insularh + smri_sulc_cdk_insularh + smri_thick_cdk_insularh + smri_vol_cdk_insularh +
        
        # DTI
        dmri_dtifa_fiberat_fxlh + dmri_dtifa_fiberat_fxrh +
        dmri_dtifa_fiberat_cgclh + dmri_dtifa_fiberat_cgcrh +
        dmri_dtifa_fiberat_cghlh + dmri_dtifa_fiberat_cghrh +
        dmri_dtifa_fiberat_cstlh + dmri_dtifa_fiberat_cstrh +
        dmri_dtifa_fiberat_atrlh + dmri_dtifa_fiberat_atrrh +
        dmri_dtifa_fiberat_unclh + dmri_dtifa_fiberat_uncrh + 
        dmri_dtifa_fiberat_ilflh + dmri_dtifa_fiberat_ilfrh +
        dmri_dtifa_fiberat_ifolh + dmri_dtifa_fiberat_iforh +
        dmri_dtifa_fiberat_slflh + dmri_dtifa_fiberat_slfrh +
        dmri_dtifa_fiberat_tslflh + dmri_dtifa_fiberat_tslfrh +
        dmri_dtifa_fiberat_pslflh + dmri_dtifa_fiberat_pslfrh +
        dmri_dtifa_fiberat_scslh + dmri_dtifa_fiberat_scsrh +
        dmri_dtifa_fiberat_sifclh + dmri_dtifa_fiberat_sifcrh +
        dmri_dtifa_fiberat_ifsfclh + dmri_dtifa_fiberat_ifsfcrh + 
        dmri_dtifa_fiberat_fmaj + dmri_dtifa_fiberat_fmin + dmri_dtifa_fiberat_cc,
      
      family = 'binomial'))

fit_log_m3_summary <- summary(pool(fit_log_m3), 
                              conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  

# add in MRI metrics
fit_log_m3_summary <- fit_log_m3_summary %>% 
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

# model follow-up
if (any(fit_log_m3_summary$Upper_CI == 'Inf') || any(fit_log_m3_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}  

write.csv(fit_log_m3_summary,'output/logistic_regression/fit_log_m3_summary.csv')
rm(fit_log_m3_summary)

#------------------------------------------------------------------------------#
#                      final model w/interaction terms
#------------------------------------------------------------------------------#

# interaction terms:
# - sex x puberty status.  
# - sex x race
# - sex x parent rules about substance use
# -	puberty status x race

fit_log_int_m3 <- with(
  trn_1_mice_m3,
  glm(DV ~  
        # Domain: Self and Peer Involvement with Substance Use
        peer_alc + peer_tob + peer_cb + peer_prob + peer_other +
        path_alc + path_tob + path_cb + 
        
        # Domain: Parenting Behaviors
        crpf + par_rules + pmq_y_ss_mean + fes_y_ss_fc + crpbi_y_ss_parent +  
        
        # Domain: Demographics
        age_baseline + 
        race_4l_Asian + race_4l_Black + race_4l_Other_MultiRacial + 
        # race_4l_White (reference group)
        eth_hisp_Hispanic +
        # eth_hisp_non_Hispanic (reference group)
        income_inc_1 + income_inc_2 + income_inc_3 + income_inc_4 + 
        income_inc_5 + income_inc_6 + income_inc_7 + income_inc_8 + 
        income_inc_10 +
        # income_inc_9 (reference group)
        religion_rp_1 + religion_rp_2 + religion_rp_3 + religion_rp_4 + 
        religion_rp_5 + religion_rp_6 + religion_rp_7 + religion_rp_8 +
        religion_rp_9 + religion_rp_10 + religion_rp_11 + religion_rp_12 +
        religion_rp_13 + religion_rp_14 + religion_rp_15 + religion_rp_16 +
        # religion_rp_17 (reference group) 
        p_edu_Less_than_HS_Degree_GED_Equivalent + 
        p_edu_HS_Graduate_GED_Equivalent +
        p_edu_Some_College_or_Associates_Degree + 
        p_edu_Masters_Degree +
        p_edu_Professional_School_or_Doctoral_Degree +
        # p_edu_Bachelors_Degree (reference group)
        sex_2l_Female +
        # sex_2l_Male (reference group)
        
        # Domain: Mental Health
        cbcl_scr_syn_anxdep_r + cbcl_scr_syn_withdep_r + 
        cbcl_scr_syn_somatic_r + cbcl_scr_syn_social_r + 
        cbcl_scr_syn_thought_r + cbcl_scr_syn_attention_r +         
        cbcl_scr_syn_rulebreak_r + cbcl_scr_syn_external_r + 
        cbcl_scr_syn_totprob_r +  
        mh_density + pps_y_ss_number + 
        upps_y_ss_negative_urgency + upps_y_ss_lack_of_perseverance+
        upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking+   
        upps_y_ss_positive_urgency +
        bis_y_ss_bis_sum + bis_y_ss_bas_drive + bis_y_ss_bas_fs + 
        bis_y_ss_bas_rr +
        
        # Domain: Physical Health
        rec_con + rec_bin_Yes + screentime + act1 + act2 + act5 + exp_sub_Yes + 
        exp_caf_rec + 
        sds_p_ss_dims + sds_p_ss_sbd + sds_p_ss_da + sds_p_ss_swtd + 
        sds_p_ss_does + sds_p_ss_shy + 
        pds + tbi_injury_Yes +
        
        # Domain: Culture & Environment
        accult_q2_y + neighborhood_crime_y + kbi_p_grades_in_school_Grade_B +
        kbi_p_grades_in_school_Grade_C + kbi_p_grades_in_school_Grade_Fail +
        det_susp_Yes + 
        se_services_Emotion_or_Learning_Support + se_services_Gifted + 
        se_services_Other + se_services_Combined_Services +
        srpf_y_ss_ses + srpf_y_ss_iiss + srpf_y_ss_dfs +
        
        # Domain: Hormones
        hormone_scr_ert_mean + hormone_scr_dhea_mean +
        
        # Domain: Neurocognitive Factors
        pea_ravlt_learn + pea_ravlt_sd_trial_vi_tc + 
        pea_ravlt_ld_trial_vii_tc + 
        pea_wiscv_tss + 
        cct_Immediate +                         
        lmt_acc + lmt_scr_rt_correct + lmt_scr_efficiency +
        nihtbx_picvocab_agecorrected + nihtbx_flanker_agecorrected +
        nihtbx_list_agecorrected + nihtbx_cardsort_agecorrected + 
        nihtbx_pattern_agecorrected + nihtbx_picture_agecorrected + 
        nihtbx_reading_agecorrected + 
        
        # Domain: Neuroimaging
        # sMRI 
        smri_area_cdk_banksstslh + smri_sulc_cdk_banksstslh + smri_thick_cdk_banksstslh + smri_vol_cdk_banksstslh +
        smri_area_cdk_banksstsrh + smri_sulc_cdk_banksstsrh + smri_thick_cdk_banksstsrh + smri_vol_cdk_banksstsrh +
        
        smri_area_cdk_cdacatelh + smri_sulc_cdk_cdacatelh + smri_thick_cdk_cdacatelh + smri_vol_cdk_cdacatelh + 
        smri_area_cdk_cdacaterh + smri_sulc_cdk_cdacaterh + smri_thick_cdk_cdacaterh + smri_vol_cdk_cdacaterh +
        
        smri_area_cdk_cdmdfrlh + smri_sulc_cdk_cdmdfrlh + smri_thick_cdk_cdmdfrlh + smri_vol_cdk_cdmdfrlh +
        smri_area_cdk_cdmdfrrh + smri_sulc_cdk_cdmdfrrh + smri_thick_cdk_cdmdfrrh + smri_vol_cdk_cdmdfrrh +
        
        smri_area_cdk_cuneuslh + smri_sulc_cdk_cuneuslh + smri_thick_cdk_cuneuslh + smri_vol_cdk_cuneuslh +
        smri_area_cdk_cuneusrh + smri_sulc_cdk_cuneusrh + smri_thick_cdk_cuneusrh + smri_vol_cdk_cuneusrh +
        
        smri_area_cdk_ehinallh + smri_sulc_cdk_ehinallh + smri_thick_cdk_ehinallh + smri_vol_cdk_ehinallh +
        smri_area_cdk_ehinalrh + smri_sulc_cdk_ehinalrh + smri_thick_cdk_ehinalrh + smri_vol_cdk_ehinalrh +
        
        smri_area_cdk_fusiformlh + smri_sulc_cdk_fusiformlh + smri_thick_cdk_fusiformlh + smri_vol_cdk_fusiformlh +
        smri_area_cdk_fusiformrh + smri_sulc_cdk_fusiformrh + smri_thick_cdk_fusiformrh + smri_vol_cdk_fusiformrh +
        
        smri_area_cdk_ifpllh + smri_sulc_cdk_ifpllh + smri_thick_cdk_ifpllh + smri_vol_cdk_ifpllh +
        smri_area_cdk_ifplrh + smri_sulc_cdk_ifplrh + smri_thick_cdk_ifplrh + smri_vol_cdk_ifplrh +
        
        smri_area_cdk_iftmlh + smri_sulc_cdk_iftmlh + smri_thick_cdk_iftmlh + smri_vol_cdk_iftmlh +
        smri_area_cdk_iftmrh + smri_sulc_cdk_iftmrh + smri_thick_cdk_iftmrh + smri_vol_cdk_iftmrh + 
        
        smri_area_cdk_ihcatelh + smri_sulc_cdk_ihcatelh + smri_thick_cdk_ihcatelh + smri_vol_cdk_ihcatelh +
        smri_area_cdk_ihcaterh + smri_sulc_cdk_ihcaterh + smri_thick_cdk_ihcaterh + smri_vol_cdk_ihcaterh +
        
        smri_area_cdk_locclh + smri_sulc_cdk_locclh + smri_thick_cdk_locclh + smri_vol_cdk_locclh +
        smri_area_cdk_loccrh + smri_sulc_cdk_loccrh + smri_thick_cdk_loccrh + smri_vol_cdk_loccrh +
        
        smri_area_cdk_lobfrlh + smri_sulc_cdk_lobfrlh + smri_thick_cdk_lobfrlh + smri_vol_cdk_lobfrlh + 
        smri_area_cdk_lobfrrh + smri_sulc_cdk_lobfrrh + smri_thick_cdk_lobfrrh + smri_vol_cdk_lobfrrh +
        
        smri_area_cdk_linguallh + smri_sulc_cdk_linguallh + smri_thick_cdk_linguallh + smri_vol_cdk_linguallh +
        smri_area_cdk_lingualrh + smri_sulc_cdk_lingualrh + smri_thick_cdk_lingualrh + smri_vol_cdk_lingualrh + 
        
        smri_area_cdk_mobfrlh + smri_sulc_cdk_mobfrlh + smri_thick_cdk_mobfrlh + smri_vol_cdk_mobfrlh +
        smri_area_cdk_mobfrrh + smri_sulc_cdk_mobfrrh + smri_thick_cdk_mobfrrh + smri_vol_cdk_mobfrrh + 
        
        smri_area_cdk_mdtmlh + smri_sulc_cdk_mdtmlh + smri_thick_cdk_mdtmlh + smri_vol_cdk_mdtmlh +
        smri_area_cdk_mdtmrh + smri_sulc_cdk_mdtmrh + smri_thick_cdk_mdtmrh + smri_vol_cdk_mdtmrh + 
        
        smri_area_cdk_parahpallh + smri_sulc_cdk_parahpallh + smri_thick_cdk_parahpallh + smri_vol_cdk_parahpallh +
        smri_area_cdk_parahpalrh + smri_sulc_cdk_parahpalrh + smri_thick_cdk_parahpalrh + smri_vol_cdk_parahpalrh +
        
        smri_area_cdk_paracnlh + smri_sulc_cdk_paracnlh + smri_thick_cdk_paracnlh + smri_vol_cdk_paracnlh +
        smri_area_cdk_paracnrh + smri_sulc_cdk_paracnrh + smri_thick_cdk_paracnrh + smri_vol_cdk_paracnrh + 
        
        smri_area_cdk_parsopclh + smri_sulc_cdk_parsopclh + smri_thick_cdk_parsopclh + smri_vol_cdk_parsopclh +
        smri_area_cdk_parsopcrh + smri_sulc_cdk_parsopcrh + smri_thick_cdk_parsopcrh + smri_vol_cdk_parsopcrh + 
        
        smri_area_cdk_parsobislh + smri_sulc_cdk_parsobislh + smri_thick_cdk_parsobislh + smri_vol_cdk_parsobislh +
        smri_area_cdk_parsobisrh + smri_sulc_cdk_parsobisrh + smri_thick_cdk_parsobisrh + smri_vol_cdk_parsobisrh + 
        
        smri_area_cdk_parstgrislh + smri_sulc_cdk_parstgrislh + smri_thick_cdk_parstgrislh + smri_vol_cdk_parstgrislh +
        smri_area_cdk_parstgrisrh + smri_sulc_cdk_parstgrisrh + smri_thick_cdk_parstgrisrh + smri_vol_cdk_parstgrisrh +
        
        smri_area_cdk_pericclh + smri_sulc_cdk_pericclh + smri_thick_cdk_pericclh + smri_vol_cdk_pericclh +
        smri_area_cdk_periccrh + smri_sulc_cdk_periccrh + smri_thick_cdk_periccrh + smri_vol_cdk_periccrh +
        
        smri_area_cdk_postcnlh + smri_sulc_cdk_postcnlh + smri_thick_cdk_postcnlh + smri_vol_cdk_postcnlh + 
        smri_area_cdk_postcnrh + smri_sulc_cdk_postcnrh + smri_thick_cdk_postcnrh + smri_vol_cdk_postcnrh + 
        
        smri_area_cdk_ptcatelh + smri_sulc_cdk_ptcatelh + smri_thick_cdk_ptcatelh + smri_vol_cdk_ptcatelh + 
        smri_area_cdk_ptcaterh + smri_sulc_cdk_ptcaterh + smri_thick_cdk_ptcaterh + smri_vol_cdk_ptcaterh + 
        
        smri_area_cdk_precnlh + smri_sulc_cdk_precnlh + smri_thick_cdk_precnlh + smri_vol_cdk_precnlh + 
        smri_area_cdk_precnrh + smri_sulc_cdk_precnrh + smri_thick_cdk_precnrh + smri_vol_cdk_precnlh + 
        
        smri_area_cdk_pclh + smri_sulc_cdk_pclh + smri_thick_cdk_pclh + smri_vol_cdk_pclh +
        smri_area_cdk_pcrh + smri_sulc_cdk_pcrh + smri_thick_cdk_pcrh + smri_vol_cdk_pcrh +
        
        smri_area_cdk_rracatelh + smri_sulc_cdk_rracatelh + smri_thick_cdk_rracatelh + smri_vol_cdk_rracatelh + 
        smri_area_cdk_rracaterh + smri_sulc_cdk_rracaterh + smri_thick_cdk_rracaterh + smri_vol_cdk_rracaterh + 
        
        smri_area_cdk_rrmdfrlh + smri_sulc_cdk_rrmdfrlh + smri_thick_cdk_rrmdfrlh + smri_vol_cdk_rrmdfrlh +
        smri_area_cdk_rrmdfrrh + smri_sulc_cdk_rrmdfrrh + smri_thick_cdk_rrmdfrrh + smri_vol_cdk_rrmdfrrh +
        
        smri_area_cdk_sufrlh + smri_sulc_cdk_sufrlh + smri_thick_cdk_sufrlh + smri_vol_cdk_sufrlh +
        smri_area_cdk_sufrrh + smri_sulc_cdk_sufrrh + smri_thick_cdk_sufrrh + smri_vol_cdk_sufrrh +
        
        smri_area_cdk_supllh + smri_sulc_cdk_supllh + smri_thick_cdk_supllh + smri_vol_cdk_supllh + 
        smri_area_cdk_suplrh + smri_sulc_cdk_suplrh + smri_thick_cdk_suplrh + smri_vol_cdk_suplrh + 
        
        smri_area_cdk_sutmlh + smri_sulc_cdk_sutmlh + smri_thick_cdk_sutmlh + smri_vol_cdk_sutmlh +
        smri_area_cdk_sutmrh + smri_sulc_cdk_sutmrh + smri_thick_cdk_sutmrh + smri_vol_cdk_sutmrh +
        
        smri_area_cdk_smlh + smri_sulc_cdk_smlh + smri_thick_cdk_smlh + smri_vol_cdk_smlh + 
        smri_area_cdk_smrh + smri_sulc_cdk_smrh + smri_thick_cdk_smrh + smri_vol_cdk_smrh + 
        
        smri_area_cdk_frpolelh + smri_sulc_cdk_frpolelh + smri_thick_cdk_frpolelh + smri_vol_cdk_frpolelh +
        smri_area_cdk_frpolerh + smri_sulc_cdk_frpolerh + smri_thick_cdk_frpolerh + smri_vol_cdk_frpolerh +
        
        smri_area_cdk_tmpolelh + smri_sulc_cdk_tmpolelh + smri_thick_cdk_tmpolelh + smri_vol_cdk_tmpolelh + 
        smri_area_cdk_tmpolerh + smri_sulc_cdk_tmpolerh + smri_thick_cdk_tmpolerh + smri_vol_cdk_tmpolerh + 
        
        smri_area_cdk_trvtmlh + smri_sulc_cdk_trvtmlh + smri_thick_cdk_trvtmlh + smri_vol_cdk_trvtmlh +
        smri_area_cdk_trvtmrh + smri_sulc_cdk_trvtmrh + smri_thick_cdk_trvtmrh + smri_vol_cdk_trvtmrh +
        
        smri_area_cdk_insulalh + smri_sulc_cdk_insulalh + smri_thick_cdk_insulalh + smri_vol_cdk_insulalh +
        smri_area_cdk_insularh + smri_sulc_cdk_insularh + smri_thick_cdk_insularh + smri_vol_cdk_insularh +
        
        # DTI
        dmri_dtifa_fiberat_fxlh + dmri_dtifa_fiberat_fxrh +
        dmri_dtifa_fiberat_cgclh + dmri_dtifa_fiberat_cgcrh +
        dmri_dtifa_fiberat_cghlh + dmri_dtifa_fiberat_cghrh +
        dmri_dtifa_fiberat_cstlh + dmri_dtifa_fiberat_cstrh +
        dmri_dtifa_fiberat_atrlh + dmri_dtifa_fiberat_atrrh +
        dmri_dtifa_fiberat_unclh + dmri_dtifa_fiberat_uncrh + 
        dmri_dtifa_fiberat_ilflh + dmri_dtifa_fiberat_ilfrh +
        dmri_dtifa_fiberat_ifolh + dmri_dtifa_fiberat_iforh +
        dmri_dtifa_fiberat_slflh + dmri_dtifa_fiberat_slfrh +
        dmri_dtifa_fiberat_tslflh + dmri_dtifa_fiberat_tslfrh +
        dmri_dtifa_fiberat_pslflh + dmri_dtifa_fiberat_pslfrh +
        dmri_dtifa_fiberat_scslh + dmri_dtifa_fiberat_scsrh +
        dmri_dtifa_fiberat_sifclh + dmri_dtifa_fiberat_sifcrh +
        dmri_dtifa_fiberat_ifsfclh + dmri_dtifa_fiberat_ifsfcrh + 
        dmri_dtifa_fiberat_fmaj + dmri_dtifa_fiberat_fmin + dmri_dtifa_fiberat_cc +
        
        # interaction terms:
        sex_2l_Female * pds +
        sex_2l_Female * race_4l_Asian + sex_2l_Female * race_4l_Black + sex_2l_Female * race_4l_Other_MultiRacial +
        sex_2l_Female * par_rules +
        pds * race_4l_Asian + pds * race_4l_Black + pds * race_4l_Other_MultiRacial, 
      
      family = 'binomial'))

fit_log_int_m3_summary <- summary(pool(fit_log_int_m3), 
                                  conf.int = TRUE, exponentiate = TRUE) %>% 
  rename(OR = estimate ,
         t.statistic = statistic,
         SE = std.error,
         Lower_CI = `2.5 %`,
         Upper_CI = `97.5 %`) %>% 
  mutate_if(is.numeric,
            round,
            digits = 3) %>% 
  relocate(term, OR, Lower_CI, Upper_CI, SE, t.statistic, df, p.value) %>% 
  rename(variable = term) %>% 
  left_join(table_names, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)  %>% 
  mutate(
    table_name = ifelse(
      variable == 'sex_2l_Female:pds', 
      'Interaction: Sex (Female) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:sex_2l_Female', 
      'Interaction: Race (Asian) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:sex_2l_Female', 
      'Interaction: Race (Black) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:sex_2l_Female', 
      'Interaction: Race (Other/Multi-Racial) x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'par_rules:sex_2l_Female', 
      'Interaction: Parent Rules x Sex (Female)', table_name),
    table_name = ifelse(
      variable == 'race_4l_Asian:pds', 
      'Interaction: Race (Asian) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Black:pds', 
      'Interaction: Race (Black) x PDS', table_name),
    table_name = ifelse(
      variable == 'race_4l_Other_MultiRacial:pds', 
      'Interaction: Race (Other/Multi-Racial) x PDS', table_name),
    
    domain_name = ifelse(
      substr(table_name, 1, 5) == 'Inter', 'Interaction Terms', domain_name))


# add in MRI metrics
fit_log_int_m3_summary <- fit_log_int_m3_summary %>% 
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

# model follow-up
if (any(fit_log_int_m3_summary$Upper_CI == 'Inf') || any(fit_log_int_m3_summary$Lower_CI == 'Inf')) {
  print('Infinite CI present')
} else {
  print('Infinite CI NOT present')
}

write.csv(fit_log_int_m3_summary,'output/logistic_regression/fit_log_int_m3_summary.csv')
rm(fit_log_int_m3_summary)


# AUC --------------------------------------------------------------------------

# AUC for test dataset

# (1) save coefficients from model on training dataset
m1_coef_log <- summary(pool(fit_log_m1)) %>% 
  select(term, estimate)
m2_coef_log <- summary(pool(fit_log_m2)) %>% 
  select(term, estimate)
m3_coef_log <- summary(pool(fit_log_m3)) %>% 
  select(term, estimate)

m1_coef_log_est <- setNames(as.list(as.numeric(m1_coef_log$estimate)), as.character(m1_coef_log$term)) 
m1_coef_log_est <- unlist(m1_coef_log_est)

m2_coef_log_est <- setNames(as.list(as.numeric(m2_coef_log$estimate)), as.character(m2_coef_log$term)) 
m2_coef_log_est <- unlist(m2_coef_log_est)

m3_coef_log_est <- setNames(as.list(as.numeric(m3_coef_log$estimate)), as.character(m3_coef_log$term)) 
m3_coef_log_est <- unlist(m3_coef_log_est)

class(m1_coef_log_est)
str(m1_coef_log_est)
print(m1_coef_log_est)

# (2) create matrix of center and scaled data each imputation 

# create MICE object w/variables used in logistic regression 

df_tst_log_m1 <-mice::complete(tst_1_mice_m1, "long", include = TRUE) %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)
names(df_tst_log_m1)

df_tst_log_m2 <-mice::complete(tst_1_mice_m2, "long", include = TRUE) %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)
names(df_tst_log_m2)

df_tst_log_m3 <-mice::complete(tst_1_mice_m3, "long", include = TRUE) %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)
names(df_tst_log_m3)

# drop aggressive and internal CBCL subscale 
temp_1 <- tst_1_obs_df_m1 %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)
temp_2 <- tst_1_obs_df_m2 %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)
temp_3 <- tst_1_obs_df_m3 %>% 
  select(-cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

# save as MIDS object 
where_tst_m1 <- matrix(TRUE, nrow = nrow(temp_1), ncol(temp_1))
where_tst_m2 <- matrix(TRUE, nrow = nrow(temp_2), ncol(temp_2))
where_tst_m3 <- matrix(TRUE, nrow = nrow(temp_3), ncol(temp_3))

colnames(where_tst_m1) <- colnames(temp_1)
colnames(where_tst_m2) <- colnames(temp_2)
colnames(where_tst_m3) <- colnames(temp_3)

mids_tst_log_m1 <- as.mids(
  df_tst_log_m1, where = where_tst_m1, .imp = ".imp", .id = ".id")
mids_tst_log_m2 <- as.mids(
  df_tst_log_m2, where = where_tst_m2, .imp = ".imp", .id = ".id")
mids_tst_log_m3 <- as.mids(
  df_tst_log_m3, where = where_tst_m3, .imp = ".imp", .id = ".id")

dfs_tst_log_m1 <- lapply(1:5, function(i) complete(mids_tst_log_m1, action = i))
dfs_tst_log_m2 <- lapply(1:5, function(i) complete(mids_tst_log_m2, action = i))
dfs_tst_log_m3 <- lapply(1:5, function(i) complete(mids_tst_log_m3, action = i))

matrix_tst_log_m1 <- list()
for (i in 1:5) {
  matrix_tst_log_m1[[i]] <- as.matrix(dfs_tst_log_m1[[i]][,(1:(length(m1_coef_log_est)-1))]) 
} # drop 1st column of DV

matrix_tst_log_m2 <- list()
for (i in 1:5) {
  matrix_tst_log_m2[[i]] <- as.matrix(dfs_tst_log_m2[[i]][,(1:(length(m2_coef_log_est)-1))]) 
} # drop 1st column of DV

matrix_tst_log_m3 <- list()
for (i in 1:5) {
  matrix_tst_log_m3[[i]] <- as.matrix(dfs_tst_log_m3[[i]][,(1:(length(m3_coef_log_est)-1))]) 
} # drop 1st column of DV

# (3) calculate probabilities for test dataset

# assign each imputation to an individual matrix
m1_temp1 <-matrix_tst_log_m1[[1]] 
m1_temp2 <-matrix_tst_log_m1[[2]] 
m1_temp3 <-matrix_tst_log_m1[[3]] 
m1_temp4 <-matrix_tst_log_m1[[4]] 
m1_temp5 <-matrix_tst_log_m1[[5]] 

m2_temp1 <-matrix_tst_log_m2[[1]] 
m2_temp2 <-matrix_tst_log_m2[[2]] 
m2_temp3 <-matrix_tst_log_m2[[3]] 
m2_temp4 <-matrix_tst_log_m2[[4]] 
m2_temp5 <-matrix_tst_log_m2[[5]] 

m3_temp1 <-matrix_tst_log_m3[[1]] 
m3_temp2 <-matrix_tst_log_m3[[2]] 
m3_temp3 <-matrix_tst_log_m3[[3]] 
m3_temp4 <-matrix_tst_log_m3[[4]] 
m3_temp5 <-matrix_tst_log_m3[[5]] 

# create a vector of 1s for intercept
v_tst_log_m1_imp1 <- cbind(rep(1,1708), m1_temp1)
v_tst_log_m1_imp2 <- cbind(rep(1,1708), m1_temp2)
v_tst_log_m1_imp3 <- cbind(rep(1,1708), m1_temp3)
v_tst_log_m1_imp4 <- cbind(rep(1,1708), m1_temp4)
v_tst_log_m1_imp5 <- cbind(rep(1,1708), m1_temp5)

v_tst_log_m2_imp1 <- cbind(rep(1,1708), m2_temp1)
v_tst_log_m2_imp2 <- cbind(rep(1,1708), m2_temp2)
v_tst_log_m2_imp3 <- cbind(rep(1,1708), m2_temp3)
v_tst_log_m2_imp4 <- cbind(rep(1,1708), m2_temp4)
v_tst_log_m2_imp5 <- cbind(rep(1,1708), m2_temp5)

v_tst_log_m3_imp1 <- cbind(rep(1,1708), m3_temp1)
v_tst_log_m3_imp2 <- cbind(rep(1,1708), m3_temp2)
v_tst_log_m3_imp3 <- cbind(rep(1,1708), m3_temp3)
v_tst_log_m3_imp4 <- cbind(rep(1,1708), m3_temp4)
v_tst_log_m3_imp5 <- cbind(rep(1,1708), m3_temp5)

# compute probabilities
perc_tst_log_m1_imp1 <- v_tst_log_m1_imp1%*%m1_coef_log_est 
perc_tst_log_m1_imp2 <- v_tst_log_m1_imp2%*%m1_coef_log_est 
perc_tst_log_m1_imp3 <- v_tst_log_m1_imp3%*%m1_coef_log_est 
perc_tst_log_m1_imp4 <- v_tst_log_m1_imp4%*%m1_coef_log_est 
perc_tst_log_m1_imp5 <- v_tst_log_m1_imp5%*%m1_coef_log_est 

perc_tst_log_m2_imp1 <- v_tst_log_m2_imp1%*%m2_coef_log_est 
perc_tst_log_m2_imp2 <- v_tst_log_m2_imp2%*%m2_coef_log_est 
perc_tst_log_m2_imp3 <- v_tst_log_m2_imp3%*%m2_coef_log_est 
perc_tst_log_m2_imp4 <- v_tst_log_m2_imp4%*%m2_coef_log_est 
perc_tst_log_m2_imp5 <- v_tst_log_m2_imp5%*%m2_coef_log_est 

perc_tst_log_m3_imp1 <- v_tst_log_m3_imp1%*%m3_coef_log_est 
perc_tst_log_m3_imp2 <- v_tst_log_m3_imp2%*%m3_coef_log_est 
perc_tst_log_m3_imp3 <- v_tst_log_m3_imp3%*%m3_coef_log_est 
perc_tst_log_m3_imp4 <- v_tst_log_m3_imp4%*%m3_coef_log_est 
perc_tst_log_m3_imp5 <- v_tst_log_m3_imp5%*%m3_coef_log_est 

prob_tst_log_m1_imp1 <- exp(perc_tst_log_m1_imp1/(1+exp(perc_tst_log_m1_imp1))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m1_imp1  = V1)
prob_tst_log_m1_imp2  <- exp(perc_tst_log_m1_imp2/(1+exp(perc_tst_log_m1_imp2))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m1_imp2  = V1)
prob_tst_log_m1_imp3  <- exp(perc_tst_log_m1_imp3/(1+exp(perc_tst_log_m1_imp3))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m1_imp3  = V1)
prob_tst_log_m1_imp4  <- exp(perc_tst_log_m1_imp4/(1+exp(perc_tst_log_m1_imp4))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m1_imp4  = V1)
prob_tst_log_m1_imp5  <- exp(perc_tst_log_m1_imp5/(1+exp(perc_tst_log_m1_imp5))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m1_imp5  = V1)

prob_tst_log_m2_imp1 <- exp(perc_tst_log_m2_imp1/(1+exp(perc_tst_log_m2_imp1))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m2_imp1  = V1)
prob_tst_log_m2_imp2  <- exp(perc_tst_log_m2_imp2/(1+exp(perc_tst_log_m2_imp2))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m2_imp2  = V1)
prob_tst_log_m2_imp3  <- exp(perc_tst_log_m2_imp3/(1+exp(perc_tst_log_m2_imp3))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m2_imp3  = V1)
prob_tst_log_m2_imp4  <- exp(perc_tst_log_m2_imp4/(1+exp(perc_tst_log_m2_imp4))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m2_imp4  = V1)
prob_tst_log_m2_imp5  <- exp(perc_tst_log_m2_imp5/(1+exp(perc_tst_log_m2_imp5))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m2_imp5  = V1)

prob_tst_log_m3_imp1 <- exp(perc_tst_log_m3_imp1/(1+exp(perc_tst_log_m3_imp1))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m3_imp1  = V1)
prob_tst_log_m3_imp2  <- exp(perc_tst_log_m3_imp2/(1+exp(perc_tst_log_m3_imp2))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m3_imp2  = V1)
prob_tst_log_m3_imp3  <- exp(perc_tst_log_m3_imp3/(1+exp(perc_tst_log_m3_imp3))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m3_imp3  = V1)
prob_tst_log_m3_imp4  <- exp(perc_tst_log_m3_imp4/(1+exp(perc_tst_log_m3_imp4))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m3_imp4  = V1)
prob_tst_log_m3_imp5  <- exp(perc_tst_log_m3_imp5/(1+exp(perc_tst_log_m3_imp5))) %>% 
  as.data.frame() %>% 
  rename(prob_tst_log_m3_imp5  = V1)

# average probabilities aross 5 imputations
m1_prob_tst_log <- cbind(
  prob_tst_log_m1_imp1, prob_tst_log_m1_imp2, prob_tst_log_m1_imp3, 
  prob_tst_log_m1_imp4, prob_tst_log_m1_imp5) %>% 
  mutate(m1_prob_avg = rowMeans(.))

m2_prob_tst_log <- cbind(
  prob_tst_log_m2_imp1, prob_tst_log_m2_imp2, prob_tst_log_m2_imp3, 
  prob_tst_log_m2_imp4, prob_tst_log_m2_imp5) %>% 
  mutate(m2_prob_avg = rowMeans(.))

m3_prob_tst_log <- cbind(
  prob_tst_log_m3_imp1, prob_tst_log_m3_imp2, prob_tst_log_m3_imp3, 
  prob_tst_log_m3_imp4, prob_tst_log_m3_imp5) %>% 
  mutate(m3_prob_avg = rowMeans(.))

write.csv(
  m1_prob_tst_log,'output/logistic_regression/m1_prob_tst_log.csv', 
  row.names = TRUE)

write.csv(
  m2_prob_tst_log,'output/logistic_regression/m2_prob_tst_log.csv', 
  row.names = TRUE)

write.csv(
  m3_prob_tst_log,'output/logistic_regression/m3_prob_tst_log.csv', 
  row.names = TRUE)

# retain only average probability across imputations for AUC
m1_prob_tst <- m1_prob_tst_log %>% 
  select(m1_prob_avg)
m2_prob_tst <- m2_prob_tst_log %>% 
  select(m2_prob_avg)
m3_prob_tst <- m3_prob_tst_log %>% 
  select(m3_prob_avg)

m1_prob_tst <- as.vector(m1_prob_tst$m1_prob_avg)
m2_prob_tst <- as.vector(m2_prob_tst$m2_prob_avg)
m3_prob_tst <- as.vector(m3_prob_tst$m3_prob_avg)

# double check each probability across the imputations is different 

identical(prob_tst_log_m1_imp1, prob_tst_log_m1_imp2)
identical(prob_tst_log_m1_imp1, prob_tst_log_m1_imp3)
identical(prob_tst_log_m1_imp1, prob_tst_log_m1_imp4)
identical(prob_tst_log_m1_imp1, prob_tst_log_m1_imp5)
identical(prob_tst_log_m1_imp2, prob_tst_log_m1_imp3)
identical(prob_tst_log_m1_imp2, prob_tst_log_m1_imp4)
identical(prob_tst_log_m1_imp2, prob_tst_log_m1_imp5)
identical(prob_tst_log_m1_imp3, prob_tst_log_m1_imp4)
identical(prob_tst_log_m1_imp3, prob_tst_log_m1_imp5)
identical(prob_tst_log_m1_imp4, prob_tst_log_m1_imp5)

identical(prob_tst_log_m2_imp1, prob_tst_log_m2_imp2)
identical(prob_tst_log_m2_imp1, prob_tst_log_m2_imp3)
identical(prob_tst_log_m2_imp1, prob_tst_log_m2_imp4)
identical(prob_tst_log_m2_imp1, prob_tst_log_m2_imp5)
identical(prob_tst_log_m2_imp2, prob_tst_log_m2_imp3)
identical(prob_tst_log_m2_imp2, prob_tst_log_m2_imp4)
identical(prob_tst_log_m2_imp2, prob_tst_log_m2_imp5)
identical(prob_tst_log_m2_imp3, prob_tst_log_m2_imp4)
identical(prob_tst_log_m2_imp3, prob_tst_log_m2_imp5)
identical(prob_tst_log_m2_imp4, prob_tst_log_m2_imp5)

identical(prob_tst_log_m3_imp1, prob_tst_log_m3_imp2)
identical(prob_tst_log_m3_imp1, prob_tst_log_m3_imp3)
identical(prob_tst_log_m3_imp1, prob_tst_log_m3_imp4)
identical(prob_tst_log_m3_imp1, prob_tst_log_m3_imp5)
identical(prob_tst_log_m3_imp2, prob_tst_log_m3_imp3)
identical(prob_tst_log_m3_imp2, prob_tst_log_m3_imp4)
identical(prob_tst_log_m3_imp2, prob_tst_log_m3_imp5)
identical(prob_tst_log_m3_imp3, prob_tst_log_m3_imp4)
identical(prob_tst_log_m3_imp3, prob_tst_log_m3_imp5)
identical(prob_tst_log_m3_imp4, prob_tst_log_m3_imp5)

# double check direction for AUC

# - subset actual DV in observed test sample
tst_m1_DV <- df_tst_log_m1 %>% 
  subset(.imp == 0) %>% 
  rename(actual = DV) %>% 
  select(actual)
table(tst_m1_DV)

tst_m2_DV <- df_tst_log_m2 %>% 
  subset(.imp == 0) %>% 
  rename(actual = DV) %>% 
  select(actual)
table(tst_m2_DV)

tst_m3_DV <- df_tst_log_m3 %>% 
  subset(.imp == 0) %>% 
  rename(actual = DV) %>% 
  select(actual)
table(tst_m3_DV)

# - bind actual and average predicted probability values across 5 imputations
tst_m1_DV_prob <- cbind(tst_m1_DV, m1_prob_tst) 
tst_m2_DV_prob <- cbind(tst_m2_DV, m2_prob_tst) 
tst_m3_DV_prob <- cbind(tst_m3_DV, m3_prob_tst) 

# - calculate median probability split by DV
tst_m1_median <- tst_m1_DV_prob %>% 
  group_by(actual) %>% 
  summarise(median_value = median(m1_prob_tst))
tst_m2_median <- tst_m2_DV_prob %>% 
  group_by(actual) %>% 
  summarise(median_value = median(m2_prob_tst))
tst_m3_median <- tst_m3_DV_prob %>% 
  group_by(actual) %>% 
  summarise(median_value = median(m3_prob_tst))

tst_m1_median_DV0 <- tst_m1_median %>% 
  subset(actual == 0) 
tst_m1_median_DV1 <- tst_m1_median %>% 
  subset(actual == 1) 

tst_m2_median_DV0 <- tst_m2_median %>% 
  subset(actual == 0) 
tst_m2_median_DV1 <- tst_m2_median %>% 
  subset(actual == 1)

tst_m3_median_DV0 <- tst_m3_median %>% 
  subset(actual == 0) 
tst_m3_median_DV1 <- tst_m3_median %>% 
  subset(actual == 1) 

if ((tst_m1_median_DV0$median_value < tst_m1_median_DV1$median_value)) {
  print(
    'test (across 5 imputations): median of controls < median of cases')
} else {
  print(
    'test (across 5 imputations): median of controls > median of cases')
}

if ((tst_m2_median_DV0$median_value < tst_m2_median_DV1$median_value)) {
  print(
    'test (across 5 imputations): median of controls < median of cases')
} else {
  print(
    'test (across 5 imputations): median of controls > median of cases')
}

if ((tst_m3_median_DV0$median_value < tst_m3_median_DV1$median_value)) {
  print(
    'test (across 5 imputations): median of controls < median of cases')
} else {
  print(
    'test (across 5 imputations): median of controls > median of cases')
}

# (4) AUC test

#--- model 1 ---#
m1_roc_tst_log <- roc(tst_1_obs_df_m1$DV, m1_prob_tst) # controls < cases
auc(m1_roc_tst_log) # Area under the curve: 0.5176
ci.auc(m1_roc_tst_log) # 95% CI: 0.4786-0.5566 (DeLong)

#--- model 2 ---#
m2_roc_tst_log <- roc(tst_1_obs_df_m2$DV, m2_prob_tst) # controls < cases
auc(m2_roc_tst_log) # Area under the curve: 0.5276
ci.auc(m2_roc_tst_log) # 95% CI: 0.489-0.5661 (DeLong)

#--- model 3 ---#
m3_roc_tst_log <- roc(tst_1_obs_df_m3$DV, m3_prob_tst) # controls < cases
auc(m3_roc_tst_log) # Area under the curve: 0.5472
ci.auc(m3_roc_tst_log) # 95% CI: 0.5094-0.585 (DeLong)

rm(m1_coef_log, m2_coef_log, m3_coef_log, 
   m1_coef_log_est, m2_coef_log_est, m3_coef_log_est,
   df_tst_log_m1, df_tst_log_m2, df_tst_log_m3,
   where_tst_m1, where_tst_m2, where_tst_m3,
   mids_tst_log_m1, mids_tst_log_m2, mids_tst_log_m3,
   dfs_tst_log_m1, dfs_tst_log_m2, dfs_tst_log_m3,
   matrix_tst_log_m1, matrix_tst_log_m2, matrix_tst_log_m3,
   
   m1_temp1, m1_temp2, m1_temp3, m1_temp4, m1_temp5,
   m2_temp1, m2_temp2, m2_temp3, m2_temp4, m2_temp5,
   m3_temp1, m3_temp2, m3_temp3, m3_temp4, m3_temp5,
   
   v_tst_log_m1_imp1, v_tst_log_m1_imp2, v_tst_log_m1_imp3, v_tst_log_m1_imp4, v_tst_log_m1_imp5,
   v_tst_log_m2_imp1, v_tst_log_m2_imp2, v_tst_log_m2_imp3, v_tst_log_m2_imp4, v_tst_log_m2_imp5,
   v_tst_log_m3_imp1, v_tst_log_m3_imp2, v_tst_log_m3_imp3, v_tst_log_m3_imp4, v_tst_log_m3_imp5,
   
   perc_tst_log_m1_imp1, perc_tst_log_m1_imp2, perc_tst_log_m1_imp3, perc_tst_log_m1_imp4, perc_tst_log_m1_imp5, 
   perc_tst_log_m2_imp1, perc_tst_log_m2_imp2, perc_tst_log_m2_imp3, perc_tst_log_m2_imp4, perc_tst_log_m2_imp5, 
   perc_tst_log_m3_imp1, perc_tst_log_m3_imp2, perc_tst_log_m3_imp3, perc_tst_log_m3_imp4, perc_tst_log_m3_imp5, 
   
   prob_tst_log_m1_imp1, prob_tst_log_m1_imp2, prob_tst_log_m1_imp3, prob_tst_log_m1_imp4, prob_tst_log_m1_imp5,
   prob_tst_log_m2_imp1, prob_tst_log_m2_imp2, prob_tst_log_m2_imp3, prob_tst_log_m2_imp4, prob_tst_log_m2_imp5,
   prob_tst_log_m3_imp1, prob_tst_log_m3_imp2, prob_tst_log_m3_imp3, prob_tst_log_m3_imp4, prob_tst_log_m3_imp5,
   
   m1_prob_tst_log, m2_prob_tst_log, m3_prob_tst_log, m1_prob_tst, m2_prob_tst, m3_prob_tst,
   
   tst_m1_DV, tst_m2_DV, tst_m3_DV, tst_m1_DV_prob, tst_m2_DV_prob, tst_m3_DV_prob,
   tst_m1_median, tst_m2_median, tst_m3_median,
   tst_m1_median_DV0, tst_m1_median_DV1, tst_m2_median_DV0, tst_m2_median_DV1, tst_m3_median_DV0, tst_m3_median_DV1,
   
   m1_roc_tst_log, m2_roc_tst_log, m3_roc_tst_log)  


# Likelihood Ratio Test (LRT) ---------------------------------------------

# create dataframe per imputation with variables included in each model
# - note: excluding CBCL aggressive and internal subscale in logistic regressions

# create interaction terms for MICE obect
df_m1_imp <- mice::complete(trn_1_mice_m1, "long", include = FALSE) %>% 
  mutate(
    sex_pdf = sex_2l_Female * pds,
    sex_raceA = sex_2l_Female * race_4l_Asian,
    sex_raceB = sex_2l_Female * race_4l_Black,
    sex_raceOM = sex_2l_Female * race_4l_Other_MultiRacial,
    sex_prules = sex_2l_Female * par_rules,
    pds_raceA = pds * race_4l_Asian,
    pds_raceB = pds * race_4l_Black,
    pds_raceOM = pds * race_4l_Other_MultiRacial)

df_m2_imp <- mice::complete(trn_1_mice_m2, "long", include = FALSE) %>% 
  mutate(
    sex_pdf = sex_2l_Female * pds,
    sex_raceA = sex_2l_Female * race_4l_Asian,
    sex_raceB = sex_2l_Female * race_4l_Black,
    sex_raceOM = sex_2l_Female * race_4l_Other_MultiRacial,
    sex_prules = sex_2l_Female * par_rules,
    pds_raceA = pds * race_4l_Asian,
    pds_raceB = pds * race_4l_Black,
    pds_raceOM = pds * race_4l_Other_MultiRacial)

df_m3_imp <- mice::complete(trn_1_mice_m3, "long", include = FALSE) %>% 
  mutate(
    sex_pdf = sex_2l_Female * pds,
    sex_raceA = sex_2l_Female * race_4l_Asian,
    sex_raceB = sex_2l_Female * race_4l_Black,
    sex_raceOM = sex_2l_Female * race_4l_Other_MultiRacial,
    sex_prules = sex_2l_Female * par_rules,
    pds_raceA = pds * race_4l_Asian,
    pds_raceB = pds * race_4l_Black,
    pds_raceOM = pds * race_4l_Other_MultiRacial)


# model 1
df_m1_imp1 <- df_m1_imp %>% 
  subset(.imp == 1) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m1_imp2 <- df_m1_imp %>% 
  subset(.imp == 2) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m1_imp3 <- df_m1_imp %>% 
  subset(.imp == 3) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m1_imp4 <- df_m1_imp %>% 
  subset(.imp == 4) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m1_imp5 <- df_m1_imp %>% 
  subset(.imp == 5) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m1_imp1_int <- df_m1_imp %>% 
  subset(.imp == 1) %>% 
  select(-.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m1_imp2_int <- df_m1_imp %>% 
  subset(.imp == 2) %>% 
  select(-.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m1_imp3_int <- df_m1_imp %>% 
  subset(.imp == 3) %>% 
  select(-.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m1_imp4_int <- df_m1_imp %>% 
  subset(.imp == 4) %>% 
  select(-.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m1_imp5_int <- df_m1_imp %>% 
  subset(.imp == 5) %>% 
  select(-.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

# model 2
df_m2_imp1 <- df_m2_imp %>% 
  subset(.imp == 1) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m2_imp2 <- df_m2_imp %>% 
  subset(.imp == 2) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m2_imp3 <- df_m2_imp %>% 
  subset(.imp == 3) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m2_imp4 <- df_m2_imp %>% 
  subset(.imp == 4) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m2_imp5 <- df_m2_imp %>% 
  subset(.imp == 5) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m2_imp1_int <- df_m2_imp %>% 
  subset(.imp == 1) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m2_imp2_int <- df_m2_imp %>% 
  subset(.imp == 2) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m2_imp3_int <- df_m2_imp %>% 
  subset(.imp == 3) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m2_imp4_int <- df_m2_imp %>% 
  subset(.imp == 4) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m2_imp5_int <- df_m2_imp %>% 
  subset(.imp == 5) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

# model 3
df_m3_imp1 <- df_m3_imp %>% 
  subset(.imp == 1) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m3_imp2 <- df_m3_imp %>% 
  subset(.imp == 2) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m3_imp3 <- df_m3_imp %>% 
  subset(.imp == 3) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m3_imp4 <- df_m3_imp %>% 
  subset(.imp == 4) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m3_imp5 <- df_m3_imp %>% 
  subset(.imp == 5) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r,
    -sex_pdf, -sex_raceA, -sex_raceB, -sex_raceOM, -sex_prules,
    -pds_raceA, -pds_raceB, -pds_raceOM)

df_m3_imp1_int <- df_m3_imp %>% 
  subset(.imp == 1) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m3_imp2_int <- df_m3_imp %>% 
  subset(.imp == 2) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m3_imp3_int <- df_m3_imp %>% 
  subset(.imp == 3) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m3_imp4_int <- df_m3_imp %>% 
  subset(.imp == 4) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

df_m3_imp5_int <- df_m3_imp %>% 
  subset(.imp == 5) %>% 
  select(
    -.imp, -.id, -cbcl_scr_syn_aggressive_r, -cbcl_scr_syn_internal_r)

#------------------------------------------------------------------------------#
#                   Logistic Regression per Imputation
#------------------------------------------------------------------------------#

# model 1
fit_log_m1_imp1 <- glm(DV ~ ., data = df_m1_imp1, family = 'binomial')
fit_log_m1_imp2 <- glm(DV ~ ., data = df_m1_imp2, family = 'binomial')
fit_log_m1_imp3 <- glm(DV ~ ., data = df_m1_imp3, family = 'binomial')
fit_log_m1_imp4 <- glm(DV ~ ., data = df_m1_imp4, family = 'binomial')
fit_log_m1_imp5 <- glm(DV ~ ., data = df_m1_imp5, family = 'binomial')

fit_log_int_m1_imp1 <- glm(DV ~ ., data = df_m1_imp1_int, family = 'binomial')
fit_log_int_m1_imp2 <- glm(DV ~ ., data = df_m1_imp2_int, family = 'binomial')
fit_log_int_m1_imp3 <- glm(DV ~ ., data = df_m1_imp3_int, family = 'binomial')
fit_log_int_m1_imp4 <- glm(DV ~ ., data = df_m1_imp4_int, family = 'binomial')
fit_log_int_m1_imp5 <- glm(DV ~ ., data = df_m1_imp5_int, family = 'binomial')

# model 2
fit_log_m2_imp1 <- glm(DV ~ ., data = df_m2_imp1, family = 'binomial')
fit_log_m2_imp2 <- glm(DV ~ ., data = df_m2_imp2, family = 'binomial')
fit_log_m2_imp3 <- glm(DV ~ ., data = df_m2_imp3, family = 'binomial')
fit_log_m2_imp4 <- glm(DV ~ ., data = df_m2_imp4, family = 'binomial')
fit_log_m2_imp5 <- glm(DV ~ ., data = df_m2_imp5, family = 'binomial')

fit_log_int_m2_imp1 <- glm(DV ~ ., data = df_m2_imp1_int, family = 'binomial')
fit_log_int_m2_imp2 <- glm(DV ~ ., data = df_m2_imp2_int, family = 'binomial')
fit_log_int_m2_imp3 <- glm(DV ~ ., data = df_m2_imp3_int, family = 'binomial')
fit_log_int_m2_imp4 <- glm(DV ~ ., data = df_m2_imp4_int, family = 'binomial')
fit_log_int_m2_imp5 <- glm(DV ~ ., data = df_m2_imp5_int, family = 'binomial')

# model 3
fit_log_m3_imp1 <- glm(DV ~ ., data = df_m3_imp1, family = 'binomial')
fit_log_m3_imp2 <- glm(DV ~ ., data = df_m3_imp2, family = 'binomial')
fit_log_m3_imp3 <- glm(DV ~ ., data = df_m3_imp3, family = 'binomial')
fit_log_m3_imp4 <- glm(DV ~ ., data = df_m3_imp4, family = 'binomial')
fit_log_m3_imp5 <- glm(DV ~ ., data = df_m3_imp5, family = 'binomial')

fit_log_int_m3_imp1 <- glm(DV ~ ., data = df_m3_imp1_int, family = 'binomial')
fit_log_int_m3_imp2 <- glm(DV ~ ., data = df_m3_imp2_int, family = 'binomial')
fit_log_int_m3_imp3 <- glm(DV ~ ., data = df_m3_imp3_int, family = 'binomial')
fit_log_int_m3_imp4 <- glm(DV ~ ., data = df_m3_imp4_int, family = 'binomial')
fit_log_int_m3_imp5 <- glm(DV ~ ., data = df_m3_imp5_int, family = 'binomial')

rm(
  df_m1_imp, df_m2_imp, df_m3_imp,
  df_m1_imp1, df_m1_imp2, df_m1_imp3, df_m1_imp4, df_m1_imp5,
  df_m1_imp1_int, df_m1_imp2_int, df_m1_imp3_int, df_m1_imp4_int, df_m1_imp5_int,
  df_m2_imp1, df_m2_imp2, df_m2_imp3, df_m2_imp4, df_m2_imp5,
  df_m2_imp1_int, df_m2_imp2_int, df_m2_imp3_int, df_m2_imp4_int, df_m2_imp5_int,
  df_m3_imp1, df_m3_imp2, df_m3_imp3, df_m3_imp4, df_m3_imp5,
  df_m3_imp1_int, df_m3_imp2_int, df_m3_imp3_int, df_m3_imp4_int, df_m3_imp5_int)

#------------------------------------------------------------------------------#
#                        Likelihood Ratio Test
#------------------------------------------------------------------------------#
# - LRT performed on each imputation 

LRT_imp1 <- lrtest(
  fit_log_m1_imp1, fit_log_int_m1_imp1, 
  fit_log_m2_imp1, fit_log_int_m2_imp1, 
  fit_log_m3_imp1, fit_log_int_m3_imp1) %>% 
  add_column(
    model = 
      c('imp1 - model 1', 'imp1 - model 1 w/interaction',
        'imp1 - model 2', 'imp1 - model 2 w/interaction',
        'imp1 - model 3', 'imp1 - model 3 w/interaction'), 
    .before = '#Df')

LRT_imp2 <- lrtest(
  fit_log_m1_imp2, fit_log_int_m1_imp2, 
  fit_log_m2_imp2, fit_log_int_m2_imp2, 
  fit_log_m3_imp2, fit_log_int_m3_imp2) %>% 
  add_column(
    model = 
      c('imp2 - model 1', 'imp2 - model 1 w/interaction',
        'imp2 - model 2', 'imp2 - model 2 w/interaction',
        'imp2 - model 3', 'imp2 - model 3 w/interaction'), 
    .before = '#Df')

LRT_imp3 <- lrtest(
  fit_log_m1_imp3, fit_log_int_m1_imp3, 
  fit_log_m2_imp3, fit_log_int_m2_imp3, 
  fit_log_m3_imp3, fit_log_int_m3_imp3) %>% 
  add_column(
    model = 
      c('imp3 - model 1', 'imp3 - model 1 w/interaction',
        'imp3 - model 2', 'imp3 - model 2 w/interaction',
        'imp3 - model 3', 'imp3 - model 3 w/interaction'), 
    .before = '#Df')

LRT_imp4 <- lrtest(
  fit_log_m1_imp4, fit_log_int_m1_imp4, 
  fit_log_m2_imp4, fit_log_int_m2_imp4, 
  fit_log_m3_imp4, fit_log_int_m3_imp4) %>% 
  add_column(
    model = 
      c('imp4 - model 1', 'imp4 - model 1 w/interaction',
        'imp4 - model 2', 'imp4 - model 2 w/interaction',
        'imp4 - model 3', 'imp4 - model 3 w/interaction'), 
    .before = '#Df')

LRT_imp5 <- lrtest(
  fit_log_m1_imp5, fit_log_int_m1_imp5, 
  fit_log_m2_imp5, fit_log_int_m2_imp5, 
  fit_log_m3_imp5, fit_log_int_m3_imp5) %>% 
  add_column(
    model = 
      c('imp5 - model 1', 'imp5 - model 1 w/interaction',
        'imp5 - model 2', 'imp5 - model 2 w/interaction',
        'imp5 - model 3', 'imp5 - model 3 w/interaction'), 
    .before = '#Df')


write.csv(LRT_imp1,'output/logistic_regression/LRT_imp1.csv')  
write.csv(LRT_imp2,'output/logistic_regression/LRT_imp2.csv')  
write.csv(LRT_imp3,'output/logistic_regression/LRT_imp3.csv')  
write.csv(LRT_imp4,'output/logistic_regression/LRT_imp4.csv')  
write.csv(LRT_imp5,'output/logistic_regression/LRT_imp5.csv')  

rm(
  fit_log_m1, fit_log_int_m1, fit_log_m2, fit_log_int_m2, fit_log_m3, fit_log_int_m3, 
  fit_log_m1_imp1, fit_log_m1_imp2, fit_log_m1_imp3, fit_log_m1_imp4, fit_log_m1_imp5,
  fit_log_int_m1_imp1, fit_log_int_m1_imp2, fit_log_int_m1_imp3, fit_log_int_m1_imp4, fit_log_int_m1_imp5,
  fit_log_m2_imp1, fit_log_m2_imp2, fit_log_m2_imp3, fit_log_m2_imp4, fit_log_m2_imp5,
  fit_log_int_m2_imp1, fit_log_int_m2_imp2, fit_log_int_m2_imp3, fit_log_int_m2_imp4, fit_log_int_m2_imp5,  
  fit_log_m3_imp1, fit_log_m3_imp2, fit_log_m3_imp3, fit_log_m3_imp4, fit_log_m3_imp5,
  fit_log_int_m3_imp1, fit_log_int_m3_imp2, fit_log_int_m3_imp3, fit_log_int_m3_imp4, fit_log_int_m3_imp5,
  LRT_imp1, LRT_imp2, LRT_imp3, LRT_imp4, LRT_imp5)


