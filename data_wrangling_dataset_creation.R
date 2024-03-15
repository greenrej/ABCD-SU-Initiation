# Load Packages -----------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(tidyverse)
library(psych) 
library(DescTools) 
library(Hmisc) 
library(rcompanion) 
library(statmod) 
library(skimr) 
library(caret)
library(rsample)
library(mice) 
library(doParallel) 
library(doRNG)
library(recipes)
library(miselect) 
library(ggplot2) 
library(pROC)
library(diffdf)
library(data.table)
library(moments)
library(lmtest)
library(gtsummary)
library(summarytools)
library(arsenal) 
library(rstatix)
library(fastDummies)
library(randomForest)
library(cvAUC)
library(svMisc)

# Create final dataset based on inclusion criteria & define outcome ------------

# import substance use data (baseline and annual follow-up)
su_y_sui <- read_csv('data/su_y_sui.csv',
                     col_types = list(tlfb_blunt_use = col_double(),
                                      tlfb_mdma_use = col_double(),
                                      tlfb_ket_use = col_double())) %>% 
  type_convert()

# subset baseline substance use variables 
su_base <- su_y_sui %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(
    src_subject_id, 
    # primary substance use variables 
    isip_1b_yn,
    tlfb_tob_puff, tlfb_chew_use, tlfb_cigar_use, tlfb_hookah_use,
    tlfb_pipes_use, tlfb_nicotine_use,
    tlfb_mj_puff, tlfb_blunt_use, tlfb_mj_conc_use, tlfb_mj_drink_use,
    tlfb_tincture_use, 
    tlfb_mj_synth_use, tlfb_coc_use, tlfb_bsalts_use, tlfb_meth_use, 
    tlfb_mdma_use, tlfb_ket_use, tlfb_ghb_use, tlfb_opi_use, tlfb_hall_use, 
    tlfb_shrooms_use, tlfb_salvia_use, tlfb_steroids_use, tlfb_sniff_use, 
    tlfb_amp_use, tlfb_tranq_use, tlfb_vicodin_use, tlfb_cough_use, 
    tlfb_other_use,
    # gating criteria variables
    tlfb_alc, tlfb_alc_sip, tlfb_tob, tlfb_mj, tlfb_mj_synth, 
    tlfb_list_yes_no, tlfb_list___1:tlfb_list___12, tlfb_inhalant, 
    tlfb_rx_misuse) 

# add suffix to indicate timepoint for baseline variables 
colnames(su_base) <- paste(
  colnames(su_base), 'base', sep = '.')
su_base <- su_base %>% 
  rename(src_subject_id = src_subject_id.base)

# subset annual follow-up substance use variables 
su_yr <- su_y_sui %>% 
  arrange(src_subject_id) %>% 
  subset(
    eventname == '1_year_follow_up_y_arm_1' |
      eventname == '2_year_follow_up_y_arm_1' | 
      eventname == '3_year_follow_up_y_arm_1') %>%  
  mutate(
    eventname = 
      case_when(
        eventname == '1_year_follow_up_y_arm_1' ~ '1_year',
        eventname == '2_year_follow_up_y_arm_1' ~ '2_year',
        eventname == '3_year_follow_up_y_arm_1' ~ '3_year')) %>% 
  select(
    src_subject_id, eventname,
    # primary substance use variables 
    isip_1b_yn_l, 
    tlfb_tob_puff_l, tlfb_chew_use_l, tlfb_hookah_use_l, tlfb_nicotine_use_l, 
    tlfb_mj_puff_l, tlfb_blunt_use_l, tlfb_mj_conc_use_l, tlfb_mj_drink_use_l, 
    tlfb_tincture_use_l, 
    tlfb_mj_synth_use_l, tlfb_coc_use_l, tlfb_bsalts_use_l, 
    tlfb_meth_use_l, tlfb_mdma_use_l, tlfb_ket_use_l, tlfb_ghb_use_l, 
    tlfb_opi_use_l, tlfb_lsd_use_l, tlfb_shrooms_use_l, tlfb_salvia_use_l, 
    tlfb_steroids_use_l, tlfb_inhalant_use_l, 
    tlfb_amp_use_l, tlfb_tranq_use_l, 
    tlfb_vicodin_use_l, tlfb_cough_use_l, tlfb_other_use_l,
    # gating criteria
    tlfb_alc_l, tlfb_alc_sip_l, tlfb_tob_l, 
    tlfb_mj_l, tlfb_mj_synth_l, tlfb_list_yes_no_l, tlfb_list_l___1, 
    tlfb_list_l___2, tlfb_list_l___3, tlfb_list_l___4, tlfb_list_l___5, 
    tlfb_list_l___6, tlfb_list_l___7, tlfb_list_l___8, tlfb_list_l___9, 
    tlfb_list_l___10, tlfb_list_l___11, tlfb_list_l___12, tlfb_inhalant_l, 
    tlfb_rx_misuse_l)

# import mid-year substance use data 
su_y_mypi <- read_delim('data/su_y_mypi.csv') %>% 
  type_convert()

# subset mid-year substance use variables 
su_mid <- su_y_mypi %>% 
  subset(
    eventname == '6_month_follow_up_arm_1' | 
      eventname == '18_month_follow_up_arm_1' |
      eventname == '30_month_follow_up_arm_1') %>% 
  mutate(
    eventname = 
      case_when(
        eventname == '6_month_follow_up_arm_1' ~ '6_month',
        eventname == '18_month_follow_up_arm_1' ~ '18_month',
        eventname == '30_month_follow_up_arm_1' ~ '30_month')) %>% 
  select(
    src_subject_id, eventname,
    # primary substance use variables 
    mypi_alc_sip_1b, 
    mypi_ecig, mypi_cigar_used, mypi_flavoring, mypi_chew_pst_used, 
    mypi_mj_used, mypi_mj_edible, mypi_mj_oils, mypi_mj_tinc_used, mypi_mj_vape,
    mypi_mj_oils_vaped, 
    mypi_mj_synth_used, mypi_coke_used, mypi_meth_used, mypi_ghb_used, 
    mypi_heroin_used, mypi_sniff_used, mypi_pills_used, mypi_pills_dep_used, 
    mypi_pr_used, mypi_cold_used, mypi_high_other_used,
    # gating criteria
    mypi_alc, mypi_alc_full_drink, mypi_alc_sip,
    mypi_tob, mypi_tob_used, mypi_chew, mypi_mj, mypi_mj_30, mypi_sniff, mypi_pills, 
    mypi_high_other)

# transpose annual and mid-year substance use data from long to wide 
su_yr_wide <- su_yr %>%
  pivot_wider(id_cols = 'src_subject_id',
              names_from = 'eventname', 
              names_sep = ".",
              values_from = c(
                # primary substance use variables
                isip_1b_yn_l, 
                tlfb_tob_puff_l, tlfb_chew_use_l, tlfb_hookah_use_l, 
                tlfb_nicotine_use_l, 
                tlfb_mj_puff_l, tlfb_blunt_use_l, tlfb_mj_conc_use_l, 
                tlfb_mj_drink_use_l, tlfb_tincture_use_l, 
                tlfb_mj_synth_use_l, tlfb_coc_use_l, tlfb_bsalts_use_l, 
                tlfb_meth_use_l, tlfb_mdma_use_l, tlfb_ket_use_l, 
                tlfb_ghb_use_l, tlfb_opi_use_l, tlfb_lsd_use_l, 
                tlfb_shrooms_use_l, tlfb_salvia_use_l, tlfb_steroids_use_l, 
                tlfb_inhalant_use_l, tlfb_amp_use_l, tlfb_tranq_use_l, 
                tlfb_vicodin_use_l, tlfb_cough_use_l, tlfb_other_use_l,
                # gating criteria variables 
                tlfb_alc_l, tlfb_alc_sip_l, tlfb_tob_l, tlfb_mj_l, 
                tlfb_mj_synth_l, tlfb_list_yes_no_l,  tlfb_inhalant_l, 
                tlfb_rx_misuse_l))

su_mid_wide <- su_mid %>%
  pivot_wider(id_cols = 'src_subject_id',
              names_from = 'eventname', 
              names_sep = ".",
              values_from = c(
                # primary substance use variables
                mypi_alc_sip_1b, 
                mypi_ecig, mypi_cigar_used, mypi_flavoring, mypi_chew_pst_used, 
                mypi_mj_used, mypi_mj_edible, mypi_mj_oils, mypi_mj_tinc_used, 
                mypi_mj_vape, mypi_mj_oils_vaped, 
                mypi_mj_synth_used, mypi_coke_used, mypi_meth_used, 
                mypi_ghb_used, mypi_heroin_used, mypi_sniff_used, 
                mypi_pills_used, mypi_pills_dep_used, mypi_pr_used, 
                mypi_cold_used, mypi_high_other_used,
                # gating criteria variables
                mypi_alc, mypi_alc_full_drink, mypi_alc_sip, mypi_tob, 
                mypi_tob_used, mypi_chew, mypi_mj, mypi_mj_30, mypi_sniff, 
                mypi_pills, mypi_high_other))

# merge all substance use data 
data.1 <- su_base %>%
  full_join(su_yr_wide, by = 'src_subject_id') %>% 
  full_join(su_mid_wide, by = 'src_subject_id') 

names(data.1)

# drop objects no longer needed
rm(su_mid, su_mid_wide, su_yr, su_yr_wide)

#------------------------------------------------------------------------------#
#                         Inclusion Criteria #1                                #
#------------------------------------------------------------------------------#
# (1) criteria: 
# - remove those who have insufficient substance use data (i.e., missing gating 
#   criteria heard of variables) at baseline and follow-up timepoints

data.1 <- data.1 %>% 
  mutate(
    exclude_1 = ifelse(
      # baseline
      is.na(tlfb_alc.base) & is.na(tlfb_tob.base) & is.na(tlfb_mj.base) &
        is.na(tlfb_mj_synth.base) & is.na(tlfb_list_yes_no.base) &
        is.na(tlfb_rx_misuse.base) & is.na(tlfb_inhalant.base) &
        
        # 6-month
        is.na(mypi_alc.6_month) & is.na(mypi_tob.6_month) & 
        is.na(mypi_chew.6_month) & is.na(mypi_mj.6_month) & 
        is.na(mypi_sniff.6_month) & is.na(mypi_pills.6_month) &
        is.na(mypi_high_other.6_month) &
        
        # 18-month
        is.na(mypi_alc.18_month) & is.na(mypi_tob.18_month) & 
        is.na(mypi_chew.18_month) & is.na(mypi_mj.18_month) & 
        is.na(mypi_sniff.18_month) & is.na(mypi_pills.18_month) & 
        is.na(mypi_high_other.18_month) &
        
        # 30-month 
        is.na(mypi_alc.30_month) & is.na(mypi_tob.30_month) & 
        is.na(mypi_chew.30_month) & is.na(mypi_mj.30_month) & 
        is.na(mypi_sniff.30_month) & is.na(mypi_pills.30_month) &
        is.na(mypi_high_other.30_month) &
        
        # 1-year
        is.na(tlfb_alc_l.1_year) & is.na(tlfb_tob_l.1_year) &
        is.na(tlfb_mj_l.1_year) & is.na(tlfb_mj_synth_l.1_year) & 
        is.na(tlfb_list_yes_no_l.1_year) & is.na(tlfb_inhalant_l.1_year) & 
        is.na(tlfb_rx_misuse_l.1_year) & 
        
        # 2-year
        is.na(tlfb_alc_l.2_year) & is.na(tlfb_tob_l.2_year) &
        is.na(tlfb_mj_l.2_year) & is.na(tlfb_mj_synth_l.2_year) & 
        is.na(tlfb_list_yes_no_l.2_year) & is.na(tlfb_inhalant_l.2_year) & 
        is.na(tlfb_rx_misuse_l.2_year) & 
        
        # 3-year  
        is.na(tlfb_alc_l.3_year) & is.na(tlfb_tob_l.3_year) &
        is.na(tlfb_mj_l.3_year) & is.na(tlfb_mj_synth_l.3_year) & 
        is.na(tlfb_list_yes_no_l.3_year) & is.na(tlfb_inhalant_l.3_year) & 
        is.na(tlfb_rx_misuse_l.3_year),
      'Drop', 'Keep'))

table(data.1$exclude_1, useNA = 'ifany') 
# n = 1 missing gating at baseline and follow-up

#--- double check ---#
# confirm no baseline or follow-up substance use data present

temp <- data.1 %>% 
  subset(exclude_1 == 'Drop') %>% 
  mutate(
    data_check = ifelse(
      # baseline
      is.na(isip_1b_yn.base) &
        is.na(tlfb_tob_puff.base) & is.na(tlfb_chew_use.base) & 
        is.na(tlfb_cigar_use.base) & is.na(tlfb_hookah_use.base) & 
        is.na(tlfb_pipes_use.base) & is.na(tlfb_nicotine_use.base) &
        is.na(tlfb_mj_puff.base) & is.na(tlfb_blunt_use.base) & 
        is.na(tlfb_mj_conc_use.base) & is.na(tlfb_mj_drink_use.base) & 
        is.na(tlfb_tincture_use.base) & 
        is.na(tlfb_mj_synth_use.base) & is.na(tlfb_coc_use.base) & 
        is.na(tlfb_bsalts_use.base) & is.na(tlfb_meth_use.base) & 
        is.na(tlfb_mdma_use.base) & is.na(tlfb_ket_use.base) & 
        is.na(tlfb_ghb_use.base) & is.na(tlfb_opi_use.base) & 
        is.na(tlfb_hall_use.base) & is.na(tlfb_shrooms_use.base) & 
        is.na(tlfb_salvia_use.base) & is.na(tlfb_steroids_use.base) & 
        is.na(tlfb_sniff_use.base) & is.na(tlfb_amp_use.base) & 
        is.na(tlfb_tranq_use.base) & is.na(tlfb_vicodin_use.base) & 
        is.na(tlfb_cough_use.base) & is.na(tlfb_other_use.base) &
        
        # annual  
        is.na(isip_1b_yn_l.1_year) & 
        is.na(isip_1b_yn_l.2_year) & 
        is.na(isip_1b_yn_l.3_year) & 
        is.na(tlfb_tob_puff_l.1_year) & 
        is.na(tlfb_tob_puff_l.2_year) & 
        is.na(tlfb_tob_puff_l.3_year) & 
        is.na(tlfb_chew_use_l.1_year) & 
        is.na(tlfb_chew_use_l.2_year) & 
        is.na(tlfb_chew_use_l.3_year) & 
        is.na(tlfb_hookah_use_l.1_year) & 
        is.na(tlfb_hookah_use_l.2_year) & 
        is.na(tlfb_hookah_use_l.3_year) & 
        is.na(tlfb_nicotine_use_l.1_year) & 
        is.na(tlfb_nicotine_use_l.2_year) & 
        is.na(tlfb_nicotine_use_l.3_year) & 
        is.na(tlfb_mj_puff_l.1_year) & 
        is.na(tlfb_mj_puff_l.2_year) & 
        is.na(tlfb_mj_puff_l.3_year) & 
        is.na(tlfb_blunt_use_l.1_year) & 
        is.na(tlfb_blunt_use_l.2_year) & 
        is.na(tlfb_blunt_use_l.3_year) & 
        is.na(tlfb_mj_conc_use_l.1_year) & 
        is.na(tlfb_mj_conc_use_l.2_year) & 
        is.na(tlfb_mj_conc_use_l.3_year) & 
        is.na(tlfb_mj_drink_use_l.1_year) & 
        is.na(tlfb_mj_drink_use_l.2_year) & 
        is.na(tlfb_mj_drink_use_l.3_year) & 
        is.na(tlfb_tincture_use_l.1_year) & 
        is.na(tlfb_tincture_use_l.2_year) & 
        is.na(tlfb_tincture_use_l.3_year) & 
        is.na(tlfb_mj_synth_use_l.1_year) & 
        is.na(tlfb_mj_synth_use_l.2_year) & 
        is.na(tlfb_mj_synth_use_l.3_year) & 
        is.na(tlfb_coc_use_l.1_year) & 
        is.na(tlfb_coc_use_l.2_year) & 
        is.na(tlfb_coc_use_l.3_year) & 
        is.na(tlfb_bsalts_use_l.1_year) & 
        is.na(tlfb_bsalts_use_l.2_year) & 
        is.na(tlfb_bsalts_use_l.3_year) & 
        is.na(tlfb_meth_use_l.1_year) & 
        is.na(tlfb_meth_use_l.2_year) & 
        is.na(tlfb_meth_use_l.3_year) & 
        is.na(tlfb_mdma_use_l.1_year) & 
        is.na(tlfb_mdma_use_l.2_year) & 
        is.na(tlfb_mdma_use_l.3_year) & 
        is.na(tlfb_ket_use_l.1_year) & 
        is.na(tlfb_ket_use_l.2_year) & 
        is.na(tlfb_ket_use_l.3_year) & 
        is.na(tlfb_ghb_use_l.1_year) & 
        is.na(tlfb_ghb_use_l.2_year) & 
        is.na(tlfb_ghb_use_l.3_year) & 
        is.na(tlfb_opi_use_l.1_year) & 
        is.na(tlfb_opi_use_l.2_year) & 
        is.na(tlfb_opi_use_l.3_year) & 
        is.na(tlfb_lsd_use_l.1_year) & 
        is.na(tlfb_lsd_use_l.2_year) & 
        is.na(tlfb_lsd_use_l.3_year) & 
        is.na(tlfb_shrooms_use_l.1_year) & 
        is.na(tlfb_shrooms_use_l.2_year) & 
        is.na(tlfb_shrooms_use_l.3_year) & 
        is.na(tlfb_salvia_use_l.1_year) & 
        is.na(tlfb_salvia_use_l.2_year) & 
        is.na(tlfb_salvia_use_l.3_year) & 
        is.na(tlfb_steroids_use_l.1_year) & 
        is.na(tlfb_steroids_use_l.2_year) & 
        is.na(tlfb_steroids_use_l.3_year) & 
        is.na(tlfb_inhalant_use_l.1_year) & 
        is.na(tlfb_inhalant_use_l.2_year) & 
        is.na(tlfb_inhalant_use_l.3_year) & 
        is.na(tlfb_amp_use_l.1_year) & 
        is.na(tlfb_amp_use_l.2_year) & 
        is.na(tlfb_amp_use_l.3_year) & 
        is.na(tlfb_tranq_use_l.1_year) & 
        is.na(tlfb_tranq_use_l.2_year) & 
        is.na(tlfb_tranq_use_l.3_year) & 
        is.na(tlfb_vicodin_use_l.1_year) & 
        is.na(tlfb_vicodin_use_l.2_year) & 
        is.na(tlfb_vicodin_use_l.3_year) & 
        is.na(tlfb_cough_use_l.1_year) & 
        is.na(tlfb_cough_use_l.2_year) & 
        is.na(tlfb_cough_use_l.3_year) & 
        is.na(tlfb_other_use_l.1_year) &  
        is.na(tlfb_other_use_l.2_year) &  
        is.na(tlfb_other_use_l.3_year) &  
        
        # mid-year
        is.na(mypi_alc_sip_1b.6_month) &
        is.na(mypi_alc_sip_1b.18_month) &
        is.na(mypi_alc_sip_1b.30_month) &
        is.na(mypi_ecig.6_month) &
        is.na(mypi_ecig.18_month) &
        is.na(mypi_ecig.30_month) &
        is.na(mypi_cigar_used.6_month) &
        is.na(mypi_cigar_used.18_month) &
        is.na(mypi_cigar_used.30_month) &
        is.na(mypi_flavoring.6_month) &
        is.na(mypi_flavoring.18_month) &
        is.na(mypi_flavoring.30_month) &
        is.na(mypi_chew_pst_used.6_month) &
        is.na(mypi_chew_pst_used.18_month) &
        is.na(mypi_chew_pst_used.30_month) &
        is.na(mypi_mj_used.6_month) &
        is.na(mypi_mj_used.18_month) &
        is.na(mypi_mj_used.30_month) &
        is.na(mypi_mj_edible.6_month) &
        is.na(mypi_mj_edible.18_month) &
        is.na(mypi_mj_edible.30_month) &
        is.na(mypi_mj_oils.6_month) &
        is.na(mypi_mj_oils.18_month) &
        is.na(mypi_mj_oils.30_month) &
        is.na(mypi_mj_tinc_used.6_month) &
        is.na(mypi_mj_tinc_used.18_month) &
        is.na(mypi_mj_tinc_used.30_month) &
        is.na(mypi_mj_vape.6_month) &
        is.na(mypi_mj_vape.18_month) &
        is.na(mypi_mj_vape.30_month) &
        is.na(mypi_mj_oils_vaped.6_month) &
        is.na(mypi_mj_oils_vaped.18_month) &
        is.na(mypi_mj_oils_vaped.30_month) &
        is.na(mypi_mj_synth_used.6_month) &
        is.na(mypi_mj_synth_used.18_month) &
        is.na(mypi_mj_synth_used.30_month) &
        is.na(mypi_coke_used.6_month) &
        is.na(mypi_coke_used.18_month) &
        is.na(mypi_coke_used.30_month) &
        is.na(mypi_meth_used.6_month) &
        is.na(mypi_meth_used.18_month) &
        is.na(mypi_meth_used.30_month) &
        is.na(mypi_heroin_used.6_month) &
        is.na(mypi_heroin_used.18_month) &
        is.na(mypi_heroin_used.30_month) &
        is.na(mypi_sniff_used.6_month) &
        is.na(mypi_sniff_used.18_month) &
        is.na(mypi_sniff_used.30_month) &
        is.na(mypi_pills_used.6_month) &
        is.na(mypi_pills_used.18_month) &
        is.na(mypi_pills_used.30_month) &
        is.na(mypi_pills_dep_used.6_month) &
        is.na(mypi_pills_dep_used.18_month) &
        is.na(mypi_pills_dep_used.30_month) &
        is.na(mypi_pr_used.6_month) &
        is.na(mypi_pr_used.18_month) &
        is.na(mypi_pr_used.30_month) &
        is.na(mypi_cold_used.6_month) &
        is.na(mypi_cold_used.18_month) &
        is.na(mypi_cold_used.30_month) &
        is.na(mypi_high_other_used.6_month) &
        is.na(mypi_high_other_used.18_month) &
        is.na(mypi_high_other_used.30_month), 1, 0))

table(temp$data_check, useNA = 'ifany')
rm(temp)
# confirmed no baseline or substance use data present
#--- double check ---#

data.1 <- data.1 %>% 
  subset(exclude_1 == 'Keep')

#----- expected sample size -----#
# n = 11867 (11868 - 1)
#----- expected sample size -----#

# (2) criteria: 
# - remove those who are missing baseline but have follow-up gating, on the 
#   basis that we cannot establish initial use at baseline if they are missing
#   gating criteria

data.1 <- data.1 %>% 
  mutate(
    exclude_2 = ifelse(
      is.na(tlfb_alc.base) & is.na(tlfb_tob.base) & is.na(tlfb_mj.base) &
        is.na(tlfb_mj_synth.base) & is.na(tlfb_list_yes_no.base) &
        is.na(tlfb_rx_misuse.base) & is.na(tlfb_inhalant.base), 'Drop', 'Keep'))

table(data.1$exclude_2, useNA = 'ifany') 
# n = 8 to drop

#--- double check ---#
# confirm no baseline substance use data present

# indicator of missing baseline data
data.1 <- data.1 %>% 
  mutate(
    base_NA = ifelse(
      is.na(isip_1b_yn.base) & 
        is.na(tlfb_tob_puff.base) & is.na(tlfb_chew_use.base) & 
        is.na(tlfb_cigar_use.base) & is.na(tlfb_hookah_use.base) & 
        is.na(tlfb_pipes_use.base) & is.na(tlfb_nicotine_use.base) & 
        is.na(tlfb_mj_puff.base) & is.na(tlfb_blunt_use.base) & 
        is.na(tlfb_mj_conc_use.base) & is.na(tlfb_tincture_use.base) & 
        is.na(tlfb_mj_synth_use.base) & is.na(tlfb_coc_use.base) & 
        is.na(tlfb_bsalts_use.base) & is.na(tlfb_meth_use.base) & 
        is.na(tlfb_mdma_use.base) & is.na(tlfb_ket_use.base) & 
        is.na(tlfb_ghb_use.base) & is.na(tlfb_opi_use.base) & 
        is.na(tlfb_hall_use.base) & is.na(tlfb_shrooms_use.base) & 
        is.na(tlfb_salvia_use.base) & is.na(tlfb_steroids_use.base) & 
        is.na(tlfb_sniff_use.base) & is.na(tlfb_amp_use.base) &
        is.na(tlfb_tranq_use.base) & is.na(tlfb_vicodin_use.base) & 
        is.na(tlfb_cough_use.base) & is.na(tlfb_other_use.base), 
      'Missing All Baseline', 'Not Missing All Baseline'))

data.1 <- data.1 %>% 
  mutate(
    exclude_2_rev = case_when(
      exclude_2 == 'Keep' ~ 'Keep',
      exclude_2 == 'Drop' & base_NA == 'Missing All Baseline' ~ 
        'Gating Missing & SU Data Missing ',
      exclude_2 == 'Drop' & base_NA == 'Not Missing All Baseline' ~ 
        'Gating Missing & SU Data Present'))

table(data.1$exclude_2_rev, useNA = 'ifany') 
# n = 2 to retain due to SU data present
#--- double check ---#

data.1 <- data.1 %>% 
  subset(exclude_2_rev == 'Keep' | 
           exclude_2_rev == 'Gating Missing & SU Data Present')

#----- expected sample size -----#
# n = 11861 (11867 - 6)
#----- expected sample size -----#

#------------------------------------------------------------------------------#
#                         Inclusion Criteria #2                                #
#------------------------------------------------------------------------------#
# criteria: 
# - remove those who reported substance use at baseline

# create summary variables for substances w/multiple sub-categories
data.1 <- data.1 %>% 
  mutate(
    tlfb_nicotine.base = ifelse(
      (tlfb_tob_puff.base == 1 | tlfb_chew_use.base == 1 | 
         tlfb_cigar_use.base == 1 | tlfb_hookah_use.base == 1 | 
         tlfb_pipes_use.base == 1 | tlfb_nicotine_use.base == 1), 1, 0),
    tlfb_cannabis.base = ifelse(
      (tlfb_mj_puff.base == 1 | tlfb_blunt_use.base == 1 | 
         tlfb_mj_conc_use.base == 1 | tlfb_mj_drink_use.base == 1 | 
         tlfb_tincture_use.base == 1), 1, 0),
    tlfb_rx.base = ifelse(
      (tlfb_amp_use.base == 1 | tlfb_tranq_use.base == 1 | 
         tlfb_vicodin_use.base == 1 | tlfb_cough_use.base == 1), 1, 0),
    tlfb_hall.base = 
      ifelse(
        (tlfb_hall_use.base == 1 | tlfb_shrooms_use.base == 1 | 
           tlfb_salvia_use.base == 1), 1, 0)) 

# substance use data at baseline
su_base_export <- data.1 %>% 
  select(
    isip_1b_yn.base, tlfb_nicotine.base, tlfb_cannabis.base, 
    tlfb_mj_synth_use.base, tlfb_coc_use.base, tlfb_bsalts_use.base,
    tlfb_meth_use.base, tlfb_mdma_use.base, tlfb_ket_use.base, 
    tlfb_ghb_use.base, tlfb_opi_use.base, tlfb_hall.base, 
    tlfb_steroids_use.base, tlfb_sniff_use.base, tlfb_rx.base, 
    tlfb_other_use.base)

su_base_export <- su_base_export %>% 
  Desc(., plotit = FALSE) 
capture.output(su_base_export, 
               file = 'output/data_management/su_base_export.txt')

rm(su_base, su_base_export)

# check for any cases where indicated sip but missing religious context
table_alc.base <- table(
  data.1$tlfb_alc_sip.base, data.1$isip_1b_yn.base, useNA = 'ifany')
table_alc.1y <- table(
  data.1$tlfb_alc_sip_l.1_year, data.1$isip_1b_yn_l.1_year, useNA = 'ifany')
table_alc.2y <- table(
  data.1$tlfb_alc_sip_l.2_year, data.1$isip_1b_yn_l.2_year, useNA = 'ifany')
table_alc.3y <- table(
  data.1$tlfb_alc_sip_l.3_year, data.1$isip_1b_yn_l.3_year, useNA = 'ifany')

table_alc.6m <- table(
  data.1$mypi_alc_sip.6_month, data.1$mypi_alc_sip_1b.6_month, useNA = 'ifany')
table_alc.18m <- table(
  data.1$mypi_alc_sip.18_month, data.1$mypi_alc_sip_1b.18_month, useNA = 'ifany')
table_alc.30m <- table(
  data.1$mypi_alc_sip.30_month, data.1$mypi_alc_sip_1b.30_month, useNA = 'ifany')

names(dimnames(table_alc.base)) <- c('tlfb_alc_sip', 'isip_1b_yn')
names(dimnames(table_alc.1y)) <- c('tlfb_alc_sip_l', 'isip_1b_yn_l')
names(dimnames(table_alc.2y)) <- c('tlfb_alc_sip_l', 'isip_1b_yn_l')
names(dimnames(table_alc.3y)) <- c('tlfb_alc_sip_l', 'isip_1b_yn_l')
names(dimnames(table_alc.6m)) <- c('mypi_alc_sip', 'mypi_alc_sip_1b')
names(dimnames(table_alc.18m)) <- c('mypi_alc_sip', 'mypi_alc_sip_1b')
names(dimnames(table_alc.30m)) <- c('mypi_alc_sip', 'mypi_alc_sip_1b')

table_alc.base 
table_alc.1y
table_alc.2y
table_alc.3y
table_alc.6m
table_alc.18m
table_alc.30m
# no cases where indicated sip but missing religious context

rm(table_alc.base, table_alc.1y, table_alc.2y, table_alc.3y,
   table_alc.6m, table_alc.18m, table_alc.30m)

# subset participants w/no substance use at baseline
data.1 <- data.1 %>% 
  mutate(
    .cols = if_any(
      isip_1b_yn.base | tlfb_tob_puff.base |
        tlfb_chew_use.base | tlfb_cigar_use.base | tlfb_hookah_use.base |
        tlfb_pipes_use.base | tlfb_nicotine_use.base |
        tlfb_mj_puff.base | tlfb_blunt_use.base | tlfb_mj_conc_use.base | 
        tlfb_mj_drink_use.base | tlfb_tincture_use.base |
        tlfb_mj_synth_use.base | tlfb_coc_use.base | tlfb_bsalts_use.base | 
        tlfb_meth_use.base | tlfb_mdma_use.base | tlfb_ket_use.base | 
        tlfb_ghb_use.base | tlfb_opi_use.base | tlfb_hall_use.base | 
        tlfb_shrooms_use.base | tlfb_salvia_use.base | tlfb_steroids_use.base |
        tlfb_sniff_use.base | tlfb_amp_use.base | tlfb_tranq_use.base | 
        tlfb_vicodin_use.base | tlfb_cough_use.base | tlfb_other_use.base,
      .fns = ~ifelse(.x == 1, 1, 0)),
    .names = NULL) %>% 
  rename(su_base_use = .cols)

table(data.1$su_base_use, useNA = 'ifany') # TRUE = 2110

data.1 <- data.1 %>% 
  subset(is.na(su_base_use)) %>% 
  select(-su_base_use)

#----- expected sample size -----#
# n = 9751 (11861 - 2110)
#----- expected sample size -----#

#------------------------------------------------------------------------------#
#                     Inclusion Criteria #3                                    #
#------------------------------------------------------------------------------#

# criteria: 
# - age 12 or greater to allow for 3-years time pass since baseline

# check number missing age (& substance use variables of interest at 3-year) 
# from age variable in longitudinal TLFB questionnaire (su_y_sui)

criteria_3 <- su_y_sui %>% 
  subset(eventname == '3_year_follow_up_y_arm_1') %>% 
  mutate(age_3yr = tlfb_age_l) %>% 
  select(src_subject_id, age_3yr)

data.1 <- data.1 %>% 
  left_join(criteria_3, by = 'src_subject_id')

table(data.1$age_3yr, useNA = 'ifany') 
# n = 1330 missing age at 3-year

temp <- data.1 %>% 
  subset(is.na(age_3yr)) %>% 
  select(
    isip_1b_yn_l.3_year, 
    tlfb_tob_puff_l.3_year, tlfb_chew_use_l.3_year, tlfb_hookah_use_l.3_year, 
    tlfb_nicotine_use_l.3_year, 
    tlfb_mj_puff_l.3_year, tlfb_blunt_use_l.3_year, tlfb_mj_conc_use_l.3_year, 
    tlfb_mj_drink_use_l.3_year, tlfb_tincture_use_l.3_year, 
    tlfb_mj_synth_use_l.3_year, tlfb_coc_use_l.3_year, tlfb_bsalts_use_l.3_year, 
    tlfb_meth_use_l.3_year, tlfb_mdma_use_l.3_year, tlfb_ket_use_l.3_year, 
    tlfb_ghb_use_l.3_year, tlfb_opi_use_l.3_year, tlfb_lsd_use_l.3_year, 
    tlfb_shrooms_use_l.3_year, tlfb_salvia_use_l.3_year, 
    tlfb_steroids_use_l.3_year, tlfb_inhalant_use_l.3_year, 
    tlfb_amp_use_l.3_year, tlfb_tranq_use_l.3_year, tlfb_vicodin_use_l.3_year, 
    tlfb_cough_use_l.3_year, tlfb_other_use_l.3_year)

na_count <- temp %>% 
  summarise(
    across(everything(), ~ sum(is.na(.)))) %>% 
  t() 

# - of the n = 1330 missing age at the 3-year follow-up (majority missing most 
#   substance use variables, n's = 1329 or 1330)

rm(temp, na_count, criteria_3)

# check number of participants < age 12 at 3-year timepoint
temp <- data.1 %>%
  subset(age_3yr < 12) # n = 530
rm(temp)

# subset participants >= age 12
data.1 <- data.1 %>%
  subset(age_3yr >= 12) 
# n = 1330 exclude: missing age (& substance use variables of interest at 3-year)
# n = 530 exclude: <12 at 3-year follow-up

#----- expected sample size -----#
# n = 7891 (9751 - 1330 - 530)
#----- expected sample size -----#

# recode 'refuse to answer' responses at mid-year assessment to NA
data.1 <- data.1 %>%  
  mutate( 
    across(.cols = starts_with('mypi_') & ends_with('.6_month'), 
           .fns = ~replace(.x, .x == 777, NA))) %>% 
  mutate( 
    across(.cols = starts_with('mypi_') & ends_with('.18_month'), 
           .fns = ~replace(.x, .x == 777, NA))) %>% 
  mutate( 
    across(.cols = starts_with('mypi_') & ends_with('.30_month'), 
           .fns = ~replace(.x, .x == 777, NA))) 

#------------------------------------------------------------------------------#
#                         Inclusion Criteria #4a                               #
#------------------------------------------------------------------------------#

# criteria: 
# - substance use data present for at least 1 follow-up timepoint, exclude if 
#   missing substance use variables across all timepoints 

# subset gating criteria heard of use variables 
su_gating <- data.1 %>% 
  select(
    src_subject_id, 
    
    # base
    tlfb_alc.base, tlfb_tob.base, tlfb_mj.base, tlfb_mj_synth.base, 
    tlfb_list_yes_no.base, tlfb_inhalant.base, tlfb_rx_misuse.base,  
    
    # annual
    tlfb_alc_l.1_year, tlfb_alc_l.2_year, tlfb_alc_l.3_year, 
    tlfb_tob_l.1_year, tlfb_tob_l.2_year, tlfb_tob_l.3_year, 
    tlfb_mj_l.1_year, tlfb_mj_l.2_year, tlfb_mj_l.3_year, 
    tlfb_mj_synth_l.1_year, tlfb_mj_synth_l.2_year, tlfb_mj_synth_l.3_year, 
    tlfb_list_yes_no_l.1_year, tlfb_list_yes_no_l.2_year, 
    tlfb_list_yes_no_l.3_year , 
    tlfb_inhalant_l.1_year, tlfb_inhalant_l.2_year, tlfb_inhalant_l.3_year, 
    tlfb_rx_misuse_l.1_year, tlfb_rx_misuse_l.2_year, tlfb_rx_misuse_l.3_year, 
    
    # mid-year
    mypi_alc.6_month, mypi_alc.18_month, mypi_alc.30_month, 
    mypi_tob.6_month, mypi_tob.18_month, mypi_tob.30_month,  
    mypi_chew.6_month, mypi_chew.18_month, mypi_chew.30_month, 
    mypi_mj.6_month, mypi_mj.18_month, mypi_mj.30_month,
    mypi_sniff.6_month, mypi_sniff.18_month, mypi_sniff.30_month, 
    mypi_pills.6_month, mypi_pills.18_month, mypi_pills.30_month, 
    mypi_high_other.6_month, mypi_high_other.18_month, mypi_high_other.30_month)

# subset primary substance use variables 
data.1 <- data.1 %>% 
  select(
    src_subject_id, 
    # baseline (n = 30)
    isip_1b_yn.base,
    tlfb_tob_puff.base, tlfb_chew_use.base, tlfb_cigar_use.base, 
    tlfb_hookah_use.base, tlfb_pipes_use.base, tlfb_nicotine_use.base,
    tlfb_mj_puff.base, tlfb_blunt_use.base, tlfb_mj_conc_use.base, 
    tlfb_mj_drink_use.base, tlfb_tincture_use.base, 
    tlfb_mj_synth_use.base, tlfb_coc_use.base, tlfb_bsalts_use.base, 
    tlfb_meth_use.base, tlfb_mdma_use.base, tlfb_ket_use.base, 
    tlfb_ghb_use.base, tlfb_opi_use.base, tlfb_hall_use.base, 
    tlfb_shrooms_use.base, tlfb_salvia_use.base, tlfb_steroids_use.base, 
    tlfb_sniff_use.base, tlfb_amp_use.base, tlfb_tranq_use.base, 
    tlfb_vicodin_use.base, tlfb_cough_use.base, tlfb_other_use.base,
    
    # annual follow-up (n = 28)
    isip_1b_yn_l.1_year, isip_1b_yn_l.2_year, isip_1b_yn_l.3_year, 
    tlfb_tob_puff_l.1_year, tlfb_tob_puff_l.2_year, tlfb_tob_puff_l.3_year, 
    tlfb_chew_use_l.1_year, tlfb_chew_use_l.2_year, tlfb_chew_use_l.3_year, 
    tlfb_hookah_use_l.1_year, tlfb_hookah_use_l.2_year, tlfb_hookah_use_l.3_year, 
    tlfb_nicotine_use_l.1_year, tlfb_nicotine_use_l.2_year, tlfb_nicotine_use_l.3_year, 
    tlfb_mj_puff_l.1_year, tlfb_mj_puff_l.2_year, tlfb_mj_puff_l.3_year, 
    tlfb_blunt_use_l.1_year, tlfb_blunt_use_l.2_year, tlfb_blunt_use_l.3_year, 
    tlfb_mj_conc_use_l.1_year, tlfb_mj_conc_use_l.2_year, tlfb_mj_conc_use_l.3_year, 
    tlfb_mj_drink_use_l.1_year, tlfb_mj_drink_use_l.2_year, tlfb_mj_drink_use_l.3_year, 
    tlfb_tincture_use_l.1_year, tlfb_tincture_use_l.2_year, tlfb_tincture_use_l.3_year, 
    tlfb_mj_synth_use_l.1_year, tlfb_mj_synth_use_l.2_year, tlfb_mj_synth_use_l.3_year, 
    tlfb_coc_use_l.1_year, tlfb_coc_use_l.2_year, tlfb_coc_use_l.3_year, 
    tlfb_bsalts_use_l.1_year, tlfb_bsalts_use_l.2_year, tlfb_bsalts_use_l.3_year, 
    tlfb_meth_use_l.1_year, tlfb_meth_use_l.2_year, tlfb_meth_use_l.3_year, 
    tlfb_mdma_use_l.1_year, tlfb_mdma_use_l.2_year, tlfb_mdma_use_l.3_year,  
    tlfb_ket_use_l.1_year, tlfb_ket_use_l.2_year, tlfb_ket_use_l.3_year, 
    tlfb_ghb_use_l.1_year, tlfb_ghb_use_l.2_year, tlfb_ghb_use_l.3_year, 
    tlfb_opi_use_l.1_year, tlfb_opi_use_l.2_year, tlfb_opi_use_l.3_year, 
    tlfb_lsd_use_l.1_year, tlfb_lsd_use_l.2_year, tlfb_lsd_use_l.3_year, 
    tlfb_shrooms_use_l.1_year, tlfb_shrooms_use_l.2_year, tlfb_shrooms_use_l.3_year, 
    tlfb_salvia_use_l.1_year, tlfb_salvia_use_l.2_year, tlfb_salvia_use_l.3_year,
    tlfb_steroids_use_l.1_year, tlfb_steroids_use_l.2_year, tlfb_steroids_use_l.3_year,
    tlfb_inhalant_use_l.1_year, tlfb_inhalant_use_l.2_year, tlfb_inhalant_use_l.3_year,
    tlfb_amp_use_l.1_year, tlfb_amp_use_l.2_year, tlfb_amp_use_l.3_year,
    tlfb_tranq_use_l.1_year, tlfb_tranq_use_l.2_year, tlfb_tranq_use_l.3_year,
    tlfb_vicodin_use_l.1_year, tlfb_vicodin_use_l.2_year, tlfb_vicodin_use_l.3_year,
    tlfb_cough_use_l.1_year, tlfb_cough_use_l.2_year, tlfb_cough_use_l.3_year,
    tlfb_other_use_l.1_year, tlfb_other_use_l.2_year, tlfb_other_use_l.3_year,
    
    # mid-year follow-up (n = 21)
    mypi_alc_sip_1b.6_month, mypi_alc_sip_1b.18_month, mypi_alc_sip_1b.30_month,
    mypi_ecig.6_month, mypi_ecig.18_month, mypi_ecig.30_month, 
    mypi_cigar_used.6_month, mypi_cigar_used.18_month, mypi_cigar_used.30_month,
    mypi_flavoring.6_month, mypi_flavoring.18_month, mypi_flavoring.30_month,
    mypi_chew_pst_used.6_month, mypi_chew_pst_used.18_month, mypi_chew_pst_used.30_month,
    mypi_mj_used.6_month, mypi_mj_used.18_month, mypi_mj_used.30_month,
    mypi_mj_edible.6_month, mypi_mj_edible.18_month, mypi_mj_edible.30_month,
    mypi_mj_oils.6_month, mypi_mj_oils.18_month, mypi_mj_oils.30_month, 
    mypi_mj_tinc_used.6_month, mypi_mj_tinc_used.18_month, mypi_mj_tinc_used.30_month, 
    mypi_mj_vape.6_month, mypi_mj_vape.18_month, mypi_mj_vape.30_month,
    mypi_mj_oils_vaped.6_month, mypi_mj_oils_vaped.18_month, mypi_mj_oils_vaped.30_month,
    mypi_mj_synth_used.6_month, mypi_mj_synth_used.18_month, mypi_mj_synth_used.30_month, 
    mypi_coke_used.6_month, mypi_coke_used.18_month, mypi_coke_used.30_month, 
    mypi_meth_used.6_month, mypi_meth_used.18_month, mypi_meth_used.30_month, 
    mypi_ghb_used.6_month, mypi_ghb_used.18_month, mypi_ghb_used.30_month,
    mypi_heroin_used.6_month, mypi_heroin_used.18_month, mypi_heroin_used.30_month, 
    mypi_sniff_used.6_month, mypi_sniff_used.18_month, mypi_sniff_used.30_month, 
    mypi_pills_used.6_month, mypi_pills_used.18_month, mypi_pills_used.30_month, 
    mypi_pills_dep_used.6_month, mypi_pills_dep_used.18_month, mypi_pills_dep_used.30_month, 
    mypi_pr_used.6_month, mypi_pr_used.18_month, mypi_pr_used.30_month, 
    mypi_cold_used.6_month, mypi_cold_used.18_month, mypi_cold_used.30_month, 
    mypi_high_other_used.6_month, mypi_high_other_used.18_month, mypi_high_other_used.30_month)

names(data.1)

# expected # of vars: n = 181
# - baseline: n = 30 SU vars
# - annual: n = 28 SU vars x 3 timepoints = 84
# - mid-year: n = 22 SU vars x 3 timepoints = 66
# - other: n = 1 ID

# check for missing substance use at each timepoint
time_6m <- data.1 %>% 
  select(src_subject_id, ends_with('.6_month')) %>% 
  mutate(tlfb_6m_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_6m_NA)
time_18m <- data.1 %>% 
  select(src_subject_id, ends_with('.18_month')) %>% 
  mutate(tlfb_18m_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_18m_NA)
time_30m <- data.1 %>% 
  select(src_subject_id, ends_with('.30_month')) %>% 
  mutate(tlfb_30m_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_30m_NA)
time_1y <- data.1 %>% 
  select(src_subject_id, ends_with('.1_year')) %>% 
  mutate(tlfb_1y_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_1y_NA)
time_2y <- data.1 %>% 
  select(src_subject_id, ends_with('.2_year')) %>% 
  mutate(tlfb_2y_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_2y_NA)
time_3y <- data.1 %>% 
  select(src_subject_id, ends_with('.3_year')) %>% 
  mutate(tlfb_3y_NA = rowSums(is.na(.))) %>% 
  select(src_subject_id, tlfb_3y_NA)

data.1 <- data.1 %>% 
  left_join(time_6m, by = 'src_subject_id') %>% 
  left_join(time_18m, by = 'src_subject_id') %>% 
  left_join(time_30m, by = 'src_subject_id') %>% 
  left_join(time_1y, by = 'src_subject_id') %>% 
  left_join(time_2y, by = 'src_subject_id') %>% 
  left_join(time_3y, by = 'src_subject_id') 
rm(time_6m, time_18m, time_30m, time_1y, time_2y, time_3y)

# create indicator of missing substance use data across all timepoints    
data.1 <- data.1 %>%   
  mutate(
    tlfb_NA = ifelse ((
      tlfb_6m_NA == 22 & tlfb_18m_NA == 22 & tlfb_30m_NA == 22 & 
        tlfb_1y_NA == 28 & tlfb_2y_NA == 28 & tlfb_3y_NA == 28), 1, 0)) 
table(data.1$tlfb_NA, useNA = 'ifany') # (0) = 7891
# - no IDs missing all substance use variables across timepoints of interest

#------------------------------------------------------------------------------#
#                        Create Dependent Variable                             #
#------------------------------------------------------------------------------#

# calculate substance use at each timepoint 
data.1 <- data.1 %>%  
  mutate(
    .cols = if_any(ends_with('6_month'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_6m = .cols) %>% 
  mutate(
    .cols = if_any(ends_with('18_month'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_18m = .cols) %>% 
  mutate(
    .cols = if_any(ends_with('30_month'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_30m = .cols) %>% 
  mutate(
    .cols = if_any(ends_with('1_year'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_1y = .cols) %>% 
  mutate(
    .cols = if_any(ends_with('2_year'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_2y = .cols) %>% 
  mutate(
    .cols = if_any(ends_with('3_year'),
                   .fns = ~ifelse (.x == 1, TRUE, FALSE))) %>% 
  rename(DV_3y = .cols) %>% 
  replace_na(
    list(DV_6m = FALSE, DV_18m = FALSE, DV_30m = FALSE,
         DV_1y = FALSE, DV_2y = FALSE, DV_3y = FALSE))

#------------------------------------------------------------------------------#
#                         Inclusion Criteria #4b                               #
#------------------------------------------------------------------------------#

# criteria: 
# - if missing at 3-year follow-up & indicated no use prior, exclude 
#   due to potential substance use between 30-mo and 3-yr that was not captured
#   due to missing substance use data at 3-year follow-up

data.1 <- data.1 %>%  
  mutate(
    DV_3yr_check = ifelse(
      (DV_6m == FALSE & DV_1y == FALSE & 
         DV_18m == FALSE & DV_2y == FALSE & 
         DV_30m == FALSE & tlfb_3y_NA == 28), 1, 0))

table(data.1$DV_3yr_check, useNA = 'ifany')
# (1) = 1 missing all substance use at 3-yr & report no substance use prior

# subset those that do not meet inclusion criteria 
data.1 <- data.1 %>% 
  subset(DV_3yr_check == 0) %>% 
  select(-ends_with('_NA'))  

#----- expected sample size -----#
# n = 7890 (7891 - 1)
#----- expected sample size -----#

# create single dependent variable for any substance use across all timepoints 
data.1 <- data.1 %>% 
  mutate(
    DV = ifelse(DV_6m == TRUE | DV_1y == TRUE | 
                  DV_18m == TRUE | DV_2y == TRUE |
                  DV_30m == TRUE | DV_3y == TRUE, 1, 0))
table(data.1$DV, useNA = 'ifany') # (0) = 6746, (1) = 1144

#------------------------------------------------------------------------------#
#                         Inclusion Criteria #5                                #
#------------------------------------------------------------------------------#

# criteria:  
# - exclude if does not meet ABCD quality control inclusion criteria
# - exclude if missing all neuroimaging variables of primary interest

# import structural MRI (n = 272 variables)
smri_area <- read_csv('data/mri_y_smr_area_dsk.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(-eventname, -smri_area_cdk_totallh, -smri_area_cdk_totalrh, 
         -smri_area_cdk_total)
smri_sulc <- read_csv('data/mri_y_smr_sulc_dsk.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(-eventname, -smri_sulc_cdk_meanlh, -smri_sulc_cdk_meanrh, 
         -smri_sulc_cdk_mean)
smri_thk <- read_csv('data/mri_y_smr_thk_dsk.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(-eventname, -smri_thick_cdk_meanlh, -smri_thick_cdk_meanrh, 
         -smri_thick_cdk_mean)
smri_vol <- read_csv('data/mri_y_smr_vol_dsk.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(-eventname,-smri_vol_cdk_totallh, -smri_vol_cdk_totalrh, 
         -smri_vol_cdk_total)

# import DTI (n = 31 variables)
DTI <- read_csv('data/mri_y_dti_fa_is_at.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  select(src_subject_id, dmri_dtifa_fiberat_fxrh, dmri_dtifa_fiberat_fxlh,
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

# import Quality Control - Recommended Image Inclusion
mri_qc_incl <- read_csv('data/mri_y_qc_incl.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(-eventname)

data.1 <- data.1 %>% 
  left_join(smri_area, by = 'src_subject_id') %>% 
  left_join(smri_sulc, by = 'src_subject_id') %>% 
  left_join(smri_thk, by = 'src_subject_id') %>% 
  left_join(smri_vol, by = 'src_subject_id') %>% 
  left_join(DTI, by = 'src_subject_id') %>% 
  left_join(mri_qc_incl, by = 'src_subject_id') 

# exclude due to not meeting ABCD quality control indicator

# structural MRI data to be excluded due to poor quality
table(data.1$imgincl_t1w_include, useNA = 'ifany')
# (0) = 310; (NA) = 60; total to be removed: 370 (= 310 + 60)

data.1 <- data.1 %>% 
  subset(imgincl_t1w_include == 1) 

#----- expected sample size -----#
# n = 7520 (7890 - 370)
#----- expected sample size -----#

# DTI data to be excluded due to poor quality
table(data.1$imgincl_dmri_include, useNA = 'ifany')
# (0) = 691; (NA) = 0; total to be removed: 691

data.1 <- data.1 %>% 
  subset(imgincl_dmri_include == 1) 

#----- expected sample size -----#
# n = 6829 (7520 - 691)
#----- expected sample size -----#

# create indicator of missing dmri & smri variables
data.1 <- data.1 %>% 
  mutate(smri_NA = rowSums(is.na(select(., starts_with('smri'))))) %>% 
  mutate(DTI_NA = rowSums(is.na(select(., starts_with('dmri'))))) %>% 
  # overlap between IDs missing all smri & DTI variables for this analysis
  mutate(neuro_NA_overlap = ifelse((smri_NA == 272 & DTI_NA == 31), 1, 0)) 

table(data.1$smri_NA, useNA = 'ifany')
# n = 0 missing all 272 smri variables

table(data.1$DTI_NA, useNA = 'ifany')
# n = 1 missing 1 DTI variable
# n = 0 missing all 31 DTI variables

table(data.1$neuro_NA_overlap, useNA = 'ifany')
# n = 0 missing all 272 sMRI variables and all 31 DTI variables

# n = 0 participants missing majority of imaging variables 

# drop variables no longer needed
data.1 <- data.1 %>% 
  select(-smri_NA)

#----- expected sample size -----#
# n = 6829 (6829 - 0)
#----- expected sample size -----#

rm(smri_area, smri_sulc, smri_thk, smri_vol, DTI, mri_qc_incl)

#------------------------------------------------------------------------------#
#                 Breakdown of SU at Each Timepoint                            #
#------------------------------------------------------------------------------#

# create summary variables for substances w/multiple sub-categories
# - nicotine (annual): 4 variables
# - nicotine (mid-year): 4 variables
# - cannabis (annual): 5 variables
# - cannabis (mid-year): 6 variables
# - Rx (annual): 4 variables
# - Rx (mid-year): 4 variables

data.1 <- data.1 %>% 
  mutate(
    nicotine.1_year = 
      ifelse(
        (tlfb_tob_puff_l.1_year == 1 | tlfb_chew_use_l.1_year == 1 | 
           tlfb_hookah_use_l.1_year == 1 | tlfb_nicotine_use_l.1_year == 1), 
        1, 0),
    nicotine.2_year = 
      ifelse(
        (tlfb_tob_puff_l.2_year == 1 | tlfb_chew_use_l.2_year == 1 | 
           tlfb_hookah_use_l.2_year == 1 | tlfb_nicotine_use_l.2_year == 1), 
        1, 0),
    nicotine.3_year = 
      ifelse(
        (tlfb_tob_puff_l.3_year == 1 | tlfb_chew_use_l.3_year == 1 | 
           tlfb_hookah_use_l.3_year == 1 | tlfb_nicotine_use_l.3_year == 1), 
        1, 0),
    
    cannabis.1_year = 
      ifelse(
        (tlfb_mj_puff_l.1_year == 1 | tlfb_blunt_use_l.1_year == 1 | 
           tlfb_mj_conc_use_l.1_year == 1 | tlfb_mj_drink_use_l.1_year == 1 | 
           tlfb_tincture_use_l.1_year == 1), 
        1, 0),
    cannabis.2_year = 
      ifelse(
        (tlfb_mj_puff_l.2_year == 1 | tlfb_blunt_use_l.2_year == 1 | 
           tlfb_mj_conc_use_l.2_year == 1 | tlfb_mj_drink_use_l.2_year == 1 | 
           tlfb_tincture_use_l.2_year == 1), 
        1, 0),
    cannabis.3_year = 
      ifelse(
        (tlfb_mj_puff_l.3_year == 1 | tlfb_blunt_use_l.3_year == 1 | 
           tlfb_mj_conc_use_l.3_year == 1 | tlfb_mj_drink_use_l.3_year == 1 | 
           tlfb_tincture_use_l.3_year == 1), 
        1, 0),
    
    hall.1_year = 
      ifelse(
        (tlfb_lsd_use_l.1_year == 1 | tlfb_shrooms_use_l.1_year == 1 | 
           tlfb_salvia_use_l.1_year == 1), 
        1, 0),
    
    hall.2_year = 
      ifelse(
        (tlfb_lsd_use_l.2_year == 1 | tlfb_shrooms_use_l.2_year == 1 | 
           tlfb_salvia_use_l.2_year == 1), 
        1, 0),
    
    hall.3_year = 
      ifelse(
        (tlfb_lsd_use_l.3_year == 1 | tlfb_shrooms_use_l.3_year == 1 | 
           tlfb_salvia_use_l.3_year == 1), 
        1, 0),
    
    Rx.1_year = 
      ifelse(
        (tlfb_amp_use_l.1_year == 1 | tlfb_tranq_use_l.1_year == 1 | 
           tlfb_vicodin_use_l.1_year == 1 | tlfb_cough_use_l.1_year == 1), 
        1, 0),
    Rx.2_year = 
      ifelse(
        (tlfb_amp_use_l.2_year == 1 | tlfb_tranq_use_l.2_year == 1 | 
           tlfb_vicodin_use_l.2_year == 1 | tlfb_cough_use_l.2_year == 1), 
        1, 0),
    Rx.3_year = 
      ifelse(
        (tlfb_amp_use_l.3_year == 1 | tlfb_tranq_use_l.3_year == 1 | 
           tlfb_vicodin_use_l.3_year == 1 | tlfb_cough_use_l.3_year == 1), 
        1, 0),
    
    nicotine.6_month = 
      ifelse(
        (mypi_ecig.6_month == 1 | mypi_cigar_used.6_month == 1 | 
           mypi_flavoring.6_month == 1 | mypi_chew_pst_used.6_month == 1), 
        1, 0),
    nicotine.18_month = 
      ifelse(
        (mypi_ecig.18_month == 1 | mypi_cigar_used.18_month == 1 | 
           mypi_flavoring.18_month == 1 | mypi_chew_pst_used.18_month == 1), 
        1, 0),
    nicotine.30_month = 
      ifelse(
        (mypi_ecig.30_month == 1 | mypi_cigar_used.30_month == 1 | 
           mypi_flavoring.30_month == 1 | mypi_chew_pst_used.30_month == 1), 
        1, 0),
    
    cannabis.6_month = 
      ifelse(
        (mypi_mj_used.6_month == 1 | mypi_mj_edible.6_month == 1 | 
           mypi_mj_oils.6_month == 1 | mypi_mj_tinc_used.6_month == 1 | 
           mypi_mj_vape.6_month == 1 | mypi_mj_oils_vaped.6_month == 1), 
        1, 0),
    cannabis.18_month = 
      ifelse(
        (mypi_mj_used.18_month == 1 | mypi_mj_edible.18_month == 1 | 
           mypi_mj_oils.18_month == 1 | mypi_mj_tinc_used.18_month == 1 | 
           mypi_mj_vape.18_month == 1 | mypi_mj_oils_vaped.18_month == 1), 
        1, 0),
    cannabis.30_month = 
      ifelse(
        (mypi_mj_used.30_month == 1 | mypi_mj_edible.30_month == 1 | 
           mypi_mj_oils.30_month == 1 | mypi_mj_tinc_used.30_month == 1 | 
           mypi_mj_vape.30_month == 1 | mypi_mj_oils_vaped.30_month == 1), 
        1, 0),
    
    Rx.6_month = 
      ifelse(
        (mypi_pills_used.6_month == 1 | mypi_pills_dep_used.6_month == 1 | 
           mypi_pr_used.6_month == 1 | mypi_cold_used.6_month == 1), 
        1, 0),
    Rx.18_month = 
      ifelse(
        (mypi_pills_used.18_month == 1 | mypi_pills_dep_used.18_month == 1 | 
           mypi_pr_used.18_month == 1 | mypi_cold_used.18_month == 1), 
        1, 0),
    Rx.30_month = 
      ifelse(
        (mypi_pills_used.30_month == 1 | mypi_pills_dep_used.30_month == 1 | 
           mypi_pr_used.30_month == 1 | mypi_cold_used.30_month == 1), 
        1, 0)) 

su_yr_export <- data.1 %>% 
  select(
    isip_1b_yn_l.1_year, isip_1b_yn_l.2_year, isip_1b_yn_l.3_year, 
    nicotine.1_year, nicotine.2_year, nicotine.3_year,
    cannabis.1_year, cannabis.2_year, cannabis.3_year,
    tlfb_mj_synth_use_l.1_year, tlfb_mj_synth_use_l.2_year, 
    tlfb_mj_synth_use_l.3_year, 
    tlfb_coc_use_l.1_year, tlfb_coc_use_l.2_year, tlfb_coc_use_l.3_year, 
    tlfb_bsalts_use_l.1_year, tlfb_bsalts_use_l.2_year, 
    tlfb_bsalts_use_l.3_year, 
    tlfb_meth_use_l.1_year, tlfb_meth_use_l.2_year, tlfb_meth_use_l.3_year, 
    tlfb_mdma_use_l.1_year, tlfb_mdma_use_l.2_year, tlfb_mdma_use_l.3_year,  
    tlfb_ket_use_l.1_year, tlfb_ket_use_l.2_year, tlfb_ket_use_l.3_year, 
    tlfb_ghb_use_l.1_year, tlfb_ghb_use_l.2_year, tlfb_ghb_use_l.3_year, 
    tlfb_opi_use_l.1_year, tlfb_opi_use_l.2_year, tlfb_opi_use_l.3_year, 
    hall.1_year, hall.2_year, hall.3_year,
    tlfb_steroids_use_l.1_year, tlfb_steroids_use_l.2_year, 
    tlfb_steroids_use_l.3_year,
    tlfb_inhalant_use_l.1_year, tlfb_inhalant_use_l.2_year, 
    tlfb_inhalant_use_l.3_year,
    Rx.1_year, Rx.2_year, Rx.3_year,
    tlfb_other_use_l.1_year, tlfb_other_use_l.2_year, tlfb_other_use_l.3_year)

su_mid_export <- data.1 %>% 
  select(
    mypi_alc_sip_1b.6_month, mypi_alc_sip_1b.18_month, mypi_alc_sip_1b.30_month,
    nicotine.6_month, nicotine.18_month, nicotine.30_month,
    cannabis.6_month, cannabis.18_month, cannabis.30_month,
    mypi_mj_synth_used.6_month, mypi_mj_synth_used.18_month, 
    mypi_mj_synth_used.30_month, 
    mypi_coke_used.6_month, mypi_coke_used.18_month, mypi_coke_used.30_month, 
    mypi_meth_used.6_month, mypi_meth_used.18_month, mypi_meth_used.30_month, 
    mypi_ghb_used.6_month, mypi_ghb_used.18_month, mypi_ghb_used.30_month,
    mypi_heroin_used.6_month, mypi_heroin_used.18_month, 
    mypi_heroin_used.30_month, 
    mypi_sniff_used.6_month, mypi_sniff_used.18_month, mypi_sniff_used.30_month, 
    Rx.6_month, Rx.18_month, Rx.30_month,
    mypi_high_other_used.6_month, mypi_high_other_used.18_month, 
    mypi_high_other_used.30_month)

su_yr_export <- su_yr_export %>% 
  Desc(., plotit = FALSE) 
capture.output(su_yr_export, 
               file = 'output/data_management/su_yr_export.txt')

su_mid_export <- su_mid_export %>% 
  Desc(., plotit = FALSE) 
capture.output(su_mid_export, 
               file = 'output/data_management/su_mid_export.txt')

# remove datasets no longer needed
rm(su_yr_export, su_mid_export)

table(data.1$DV, useNA = 'ifany')
#    0    1 
# 5847  982

# supplement: final sample size - age range for follow-up

# pull max age from longitudinal TLFB questionnaire (3-year follow-up)
table(temp_max$tlfb_age_l, useNA = 'ifany') # NA = 0
table(temp_max$tlfb_age_calc_inmonths_l, useNA = 'ifany') # NA = 1
table(temp_max$tlfb_age_month_l, useNA = 'ifany') # NA = 1

temp_max <- su_y_sui %>% 
  subset(eventname == '3_year_follow_up_y_arm_1') %>% 
  right_join(data.1, by = 'src_subject_id') %>% 
  select(src_subject_id, tlfb_age_l, tlfb_age_calc_inmonths_l, tlfb_age_month_l) %>% 
  mutate(age_2dec = tlfb_age_calc_inmonths_l/12) 
temp_max$age_2dec <- round(temp_max$age_2dec, digits = 2)

mean(temp_max$age_2dec, na.rm = TRUE) # 13.00
sd(temp_max$age_2dec, na.rm = TRUE) # 0.61
range(temp_max$age_2dec, na.rm = TRUE) # 12.00 - 15.08
# note: age in months missing for 1 participant

rm(temp_max, temp_min)

# Independent Variables: Self and Peer Involvement with Substance Use  ---------

# subset relevant data to build full dataset
data.2 <- data.1 %>% 
  select(src_subject_id, DV, 
         starts_with('smri_'), starts_with('dmri_')) 
# expected # of variables: 1 ID + 1 DV + 272 sMRI + 31 DTI = 305

# subset IDs for final dataset
data.2_ID <- data.2 %>% 
  select(src_subject_id)

# - subset baseline 'heard of' gating criteria in final sample for use in 
#   substance-related predictors
su_gating_pred <- su_y_sui %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(
    src_subject_id, tlfb_alc, tlfb_tob, tlfb_mj, tlfb_mj_synth, tlfb_bitta, 
    tlfb_inhalant, tlfb_list_yes_no, tlfb_list_yes_no_l) %>% 
  right_join(data.2_ID, by = 'src_subject_id') 

colnames(su_gating_pred) <- paste(
  colnames(su_gating_pred), 'base', sep = '.')

su_gating_pred <- su_gating_pred %>% 
  rename(src_subject_id = src_subject_id.base)

names(su_gating_pred)

#------------------------------------------------------------------------------#
#                           Peer Group Deviance                                #
#------------------------------------------------------------------------------#

# import questionnaire
su_y_peerdevia <- read_csv('data/su_y_peerdevia.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(src_subject_id,
         peer_deviance_1_4bbe5d, peer_deviance_2_dd1457, 
         peer_deviance_3_e1ec2e, peer_deviance_4_b6c588, 
         peer_deviance_5_bffa44, peer_deviance_6_69562e,
         peer_deviance_7_beb683, peer_deviance_8_35702e, 
         peer_deviance_9_6dd4ef) %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  right_join(su_gating_pred, by = 'src_subject_id')

#-- Supplementary Materials ---#
# NA due to gating criteria 
su_y_peerdevia <- su_y_peerdevia %>% 
  mutate(
    peer_1_NA = ifelse(
      tlfb_tob.base == 0 & is.na(peer_deviance_1_4bbe5d), 1, 0),
    peer_2_NA = ifelse(
      tlfb_alc.base == 0 & is.na(peer_deviance_2_dd1457), 1, 0),
    peer_3_NA = ifelse(
      tlfb_alc.base == 0 & is.na(peer_deviance_3_e1ec2e), 1, 0),
    peer_multi = ifelse(
      tlfb_alc.base == 1 | tlfb_tob.base == 1 | tlfb_mj.base == 1 | 
        tlfb_mj_synth.base == 1 | tlfb_bitta.base == 1 | 
        tlfb_list_yes_no.base == 1, 1, 0),
    peer_4_NA = ifelse(
      peer_multi == 0 & is.na(peer_deviance_4_b6c588), 1, 0),
    peer_5_NA = ifelse(
      tlfb_mj.base == 0 & is.na(peer_deviance_5_bffa44), 1, 0),
    peer_6_NA = ifelse(
      tlfb_inhalant.base == 0 & is.na(peer_deviance_6_69562e), 1, 0),
    peer_7_NA = ifelse(
      tlfb_list_yes_no.base == 0 & is.na(peer_deviance_7_beb683), 1, 0),
    peer_8_NA = ifelse(
      peer_multi == 0 & is.na(peer_deviance_8_35702e), 1, 0),
    peer_9_NA = ifelse(
      tlfb_tob.base == 0 & is.na(peer_deviance_9_6dd4ef), 1, 0))

su_y_peerdevia_NA <- su_y_peerdevia %>% 
  select(peer_1_NA:peer_9_NA, -peer_multi) %>% 
  gather(variable, value, peer_1_NA:peer_9_NA) %>%
  group_by(variable, value) %>%
  summarise (n = n()) %>%
  subset(value == 1) %>% 
  select(-value) %>% 
  mutate(
    variable = case_when(
      variable == 'peer_1_NA' ~ 'peer_deviance_1_4bbe5d',
      variable == 'peer_2_NA' ~ 'peer_deviance_2_dd1457',
      variable == 'peer_3_NA' ~ 'peer_deviance_3_e1ec2e',
      variable == 'peer_4_NA' ~ 'peer_deviance_4_b6c588',
      variable == 'peer_5_NA' ~ 'peer_deviance_5_bffa44',
      variable == 'peer_6_NA' ~ 'peer_deviance_6_69562e',
      variable == 'peer_7_NA' ~ 'peer_deviance_7_beb683',
      variable == 'peer_8_NA' ~ 'peer_deviance_8_35702e',
      variable == 'peer_9_NA' ~ 'peer_deviance_9_6dd4ef'),
    gating_criteria = case_when(
      variable == 'peer_deviance_1_4bbe5d' ~ 'tobacco',
      variable == 'peer_deviance_2_dd1457' ~ 'alcohol',
      variable == 'peer_deviance_3_e1ec2e' ~ 'alcohol',
      variable == 'peer_deviance_4_b6c588' ~
        'alcohol, tobacco, cannabis, synthetic cannabis, or any illicit substance, 
       bittamugen or byphoditin (fake substance), or using anything else to feel 
       high, dizzy, or different',
      variable == 'peer_deviance_5_bffa44' ~ 'cannabis',
      variable == 'peer_deviance_6_69562e' ~ 'inhalants',
      variable == 'peer_deviance_7_beb683' ~ 'using anything else to feel high, 
      dizzy, or different',
      variable == 'peer_deviance_8_35702e' ~ 'alcohol, tobacco, cannabis, 
      synthetic cannabis, or any illicit substance, bittamugen or byphoditin 
      (fake substance), or using anything else to feel high, dizzy, or 
      different',
      variable == 'peer_deviance_9_6dd4ef' ~ 'tobacco')) %>% 
  relocate(variable, gating_criteria, n) %>% 
  rename(
    `Questionnaire: Peer Group Deviance` = variable,
    `Gating Criteria` = gating_criteria,
    `NA due to Gating Criteria (N)` = n) 

write.csv(su_y_peerdevia_NA,
          'output/data_management/supplement/su_peerdevia_NA.csv')  
rm(su_y_peerdevia_NA)
#-- Supplementary Materials ---#

# re-code due to gating criteria
su_y_peerdevia <- su_y_peerdevia %>% 
  mutate(
    peer_dev_1 = ifelse(
      (tlfb_tob.base == 0 & is.na(peer_deviance_1_4bbe5d)), 0, 
      peer_deviance_1_4bbe5d),
    peer_dev_2 = ifelse(
      (tlfb_alc.base == 0 & is.na(peer_deviance_2_dd1457)), 0, 
      peer_deviance_2_dd1457),
    peer_dev_3 = ifelse(
      (tlfb_alc.base == 0 & is.na(peer_deviance_3_e1ec2e)), 0, 
      peer_deviance_3_e1ec2e),
    peer_dev_4 = ifelse(
      ((tlfb_alc.base == 0 & tlfb_tob.base == 0 & tlfb_mj.base == 0 & 
          tlfb_mj_synth.base == 0 & tlfb_bitta.base == 0 & 
          tlfb_list_yes_no.base == 0) & is.na(peer_deviance_4_b6c588)), 0, 
      peer_deviance_4_b6c588),
    peer_dev_5 = ifelse(
      (tlfb_mj.base == 0 & is.na(peer_deviance_5_bffa44)), 0, 
      peer_deviance_5_bffa44),
    peer_dev_6 = ifelse(
      (tlfb_inhalant.base == 0 & is.na(peer_deviance_6_69562e)), 
      0, peer_deviance_6_69562e),
    peer_dev_7 = ifelse(
      (tlfb_list_yes_no.base == 0 & is.na(peer_deviance_7_beb683)), 
      0, peer_deviance_7_beb683),
    peer_dev_8 = ifelse(
      ((tlfb_alc.base == 0 & tlfb_tob.base == 0 & tlfb_mj.base == 0 & 
          tlfb_mj_synth.base == 0 & tlfb_bitta.base == 0 & 
          tlfb_list_yes_no.base == 0) & is.na(peer_deviance_8_35702e)), 0, 
      peer_deviance_8_35702e),
    peer_dev_9 = ifelse(
      (tlfb_tob.base == 0 & is.na(peer_deviance_9_6dd4ef)), 0, 
      peer_deviance_9_6dd4ef))

#-- Supplementary Materials ---#
# missing data after re-coding due to gating criteria
su_peerdevia_NA_postrecode <- su_y_peerdevia %>% 
  select(peer_dev_1, peer_dev_2, peer_dev_3, peer_dev_4, peer_dev_5, 
         peer_dev_6, peer_dev_7, peer_dev_8, peer_dev_9) %>% 
  Desc(., plotit = FALSE) 
capture.output(
  su_peerdevia_NA_postrecode, 
  file = 'output/data_management/supplement/su_peerdevia_NA_postrecode.txt')
rm(su_peerdevia_NA_postrecode)
#-- Supplementary Materials ---#

# create summary scores
su_y_peerdevia <- su_y_peerdevia %>% 
  mutate(
    peer_alc = peer_dev_2 + peer_dev_3,
    peer_tob = peer_dev_1 + peer_dev_9,
    peer_cb = peer_dev_5, 
    peer_other = peer_dev_6 + peer_dev_7, 
    peer_prob = peer_dev_4 + peer_dev_8) 

# subset relevant variables 
su_y_peerdevia <- su_y_peerdevia %>% 
  select(src_subject_id, peer_alc, peer_tob, peer_cb, peer_other, peer_prob)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(su_y_peerdevia, by = 'src_subject_id')

rm(su_y_peerdevia)

#------------------------------------------------------------------------------#
#                             Intent to Use
#------------------------------------------------------------------------------#

# import questionnaire
su_y_path_intuse <- read_csv('data/su_y_path_intuse.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(src_subject_id,
         path_alc_youth1, path_alc_youth2, path_alc_youth3, path_alc_youth4,
         path_alc_youth5, path_alc_youth6, path_alc_youth7, path_alc_youth8,
         path_alc_youth9) %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  right_join(su_gating_pred, by = 'src_subject_id')

#-- Supplementary Materials ---#
# NA due to gating criteria 
su_y_path_intuse <- su_y_path_intuse %>% 
  mutate(
    path_1_NA = ifelse(tlfb_tob.base == 0 & is.na(path_alc_youth1), 1, 0),
    path_2_NA = ifelse(tlfb_alc.base == 0 & is.na(path_alc_youth2), 1, 0),
    path_3_NA = ifelse(tlfb_mj.base == 0 & is.na(path_alc_youth3), 1, 0),
    path_4_NA = ifelse(tlfb_tob.base == 0 & is.na(path_alc_youth4), 1, 0),
    path_5_NA = ifelse(tlfb_alc.base == 0 & is.na(path_alc_youth5), 1, 0),
    path_6_NA = ifelse(tlfb_mj.base == 0 & is.na(path_alc_youth6), 1, 0),
    path_7_NA = ifelse(tlfb_tob.base == 0 & is.na(path_alc_youth7), 1, 0),
    path_8_NA = ifelse(tlfb_alc.base == 0 & is.na(path_alc_youth8), 1, 0),
    path_9_NA = ifelse(tlfb_mj.base == 0 & is.na(path_alc_youth9), 1, 0))

su_y_path_intuse_NA <- su_y_path_intuse %>% 
  select(path_1_NA:path_9_NA) %>% 
  gather(variable, value, path_1_NA:path_9_NA) %>%
  group_by(variable, value) %>%
  summarise (n = n()) %>%
  subset(value == 1) %>% 
  select(-value) %>% 
  mutate(
    variable = case_when(
      variable == 'path_1_NA' ~ 'path_alc_youth1',
      variable == 'path_2_NA' ~ 'path_alc_youth2',
      variable == 'path_3_NA' ~ 'path_alc_youth3',
      variable == 'path_4_NA' ~ 'path_alc_youth4',
      variable == 'path_5_NA' ~ 'path_alc_youth5',
      variable == 'path_6_NA' ~ 'path_alc_youth6',
      variable == 'path_7_NA' ~ 'path_alc_youth7',
      variable == 'path_8_NA' ~ 'path_alc_youth8',
      variable == 'path_9_NA' ~ 'path_alc_youth9'),
    gating_criteria = case_when(
      variable == 'path_alc_youth1' ~ 'tobacco',
      variable == 'path_alc_youth2' ~ 'alcohol',
      variable == 'path_alc_youth3' ~ 'cannabis',
      variable == 'path_alc_youth4' ~ 'tobacco',
      variable == 'path_alc_youth5' ~ 'alcohol',
      variable == 'path_alc_youth6' ~ 'cannabis',
      variable == 'path_alc_youth7' ~ 'tobacco',
      variable == 'path_alc_youth8' ~ 'alcohol',
      variable == 'path_alc_youth9' ~ 'cannabis')) %>% 
  relocate(variable, gating_criteria, n) %>% 
  rename(
    `Questionnaire: Intent to Use` = variable,
    `Gating Criteria` = gating_criteria,
    `NA due to Gating Criteria (N)` = n)

write.csv(su_y_path_intuse_NA,
          'output/data_management/supplement/su_path_intuse_NA.csv')  
rm(su_y_path_intuse_NA)
#-- Supplementary Materials ---#

# re-code due to gating criteria
su_y_path_intuse <- su_y_path_intuse %>% 
  mutate(
    path_1 = ifelse(
      (tlfb_tob.base == 0 & is.na(path_alc_youth1)), 4, path_alc_youth1),
    path_2 = ifelse(
      (tlfb_alc.base == 0 & is.na(path_alc_youth2)), 4, path_alc_youth2),
    path_3 = ifelse(
      (tlfb_mj.base == 0 & is.na(path_alc_youth3)), 4, path_alc_youth3), 
    path_4 = ifelse(
      (tlfb_tob.base == 0 & is.na(path_alc_youth4)), 4, path_alc_youth4),
    path_5 = ifelse(
      (tlfb_alc.base == 0 & is.na(path_alc_youth5)), 4, path_alc_youth5),
    path_6 = ifelse(
      (tlfb_mj.base == 0 & is.na(path_alc_youth6)), 4, path_alc_youth6),
    path_7 = ifelse(
      (tlfb_tob.base == 0 & is.na(path_alc_youth7)), 4, path_alc_youth7),
    path_8 = ifelse(
      (tlfb_alc.base == 0 & is.na(path_alc_youth8)), 4, path_alc_youth8),
    path_9 = ifelse(
      (tlfb_mj.base == 0 & is.na(path_alc_youth9)), 4, path_alc_youth9)) 

#-- Supplementary Materials ---#
# after re-coding, values of 'Don't Know' and N/A 
su_y_path_intuse_56 <- su_y_path_intuse %>% 
  mutate(
    path_1_56 = case_when(
      path_1 == 5 ~ "Response: Don’t Know (N)",
      path_1 == 6 ~ "Response: Refuse to Answer (N)",
      path_1 <5 ~ NA),
    path_2_56 = case_when(
      path_2 == 5 ~ "Response: Don’t Know (N)",
      path_2 == 6 ~ "Response: Refuse to Answer (N)",
      path_2 <5 ~ NA),
    path_3_56 = case_when(
      path_3 == 5 ~ "Response: Don’t Know (N)",
      path_3 == 6 ~ "Response: Refuse to Answer (N)",
      path_3 <5 ~ NA),
    path_4_56 = case_when(
      path_4 == 5 ~ "Response: Don’t Know (N)",
      path_4 == 6 ~ "Response: Refuse to Answer (N)",
      path_4 <5 ~ NA),
    path_5_56 = case_when(
      path_5 == 5 ~ "Response: Don’t Know (N)",
      path_5 == 6 ~ "Response: Refuse to Answer (N)",
      path_5 <5 ~ NA),
    path_6_56 = case_when(
      path_6 == 5 ~ "Response: Don’t Know (N)",
      path_6 == 6 ~ "Response: Refuse to Answer (N)",
      path_6 <5 ~ NA),
    path_7_56 = case_when(
      path_7 == 5 ~ "Response: Don’t Know (N)",
      path_7 == 6 ~ "Response: Refuse to Answer (N)",
      path_7 <5 ~ NA),
    path_8_56 = case_when(
      path_8 == 5 ~ "Response: Don’t Know (N)",
      path_8 == 6 ~ "Response: Refuse to Answer (N)",
      path_8 <5 ~ NA),
    path_9_56 = case_when(
      path_9 == 5 ~ "Response: Don’t Know (N)",
      path_9 == 6 ~ "Response: Refuse to Answer (N)",
      path_9 <5 ~ NA))

p1 <- table(su_y_path_intuse_56$path_1_56)
p2 <- table(su_y_path_intuse_56$path_2_56)
p3 <- table(su_y_path_intuse_56$path_3_56)
p4 <- table(su_y_path_intuse_56$path_4_56)
p5 <- table(su_y_path_intuse_56$path_5_56)
p6 <- table(su_y_path_intuse_56$path_6_56)
p7 <- table(su_y_path_intuse_56$path_7_56)
p8 <- table(su_y_path_intuse_56$path_8_56)
p9 <- table(su_y_path_intuse_56$path_9_56)

path_56_summary <- cbind(p1, p2, p3, p4, p5, p6, p7, p8, p9) %>% 
  as.data.frame() %>% 
  t() 

rownames(path_56_summary) <- c(
  'path_alc_youth_1','path_alc_youth_2', 'path_alc_youth_3',
  'path_alc_youth_4','path_alc_youth_5', 'path_alc_youth_6',
  'path_alc_youth_7','path_alc_youth_8', 'path_alc_youth_9')

# pre-existing missing
p_pre_NA <- su_y_path_intuse %>% 
  select(path_1:path_9) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1)

path_56_summary <- cbind(path_56_summary, p_pre_NA)

path_56_summary <- path_56_summary %>% 
  mutate(
    `Response: Don’t Know (N)` = as.numeric(`Response: Don’t Know (N)`),
    `Response: Refuse to Answer (N)` = as.numeric(`Response: Refuse to Answer (N)`))

# total missing
path_56_summary$`Total Missing (N)` <- rowSums(
  select(path_56_summary, ends_with('(N)')))

write.csv(path_56_summary,
          'output/data_management/supplement/su_path_intuse_56.csv')  

rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p_pre_NA, su_y_path_intuse_56,
   path_56_summary)
#-- Supplementary Materials ---#

# re-code Don't Know and Refuse to Answer
su_y_path_intuse <- su_y_path_intuse %>% 
  mutate(
    across(.cols = path_1:path_9,  
           .fns = ~replace(.x, .x %in% c(5, 6), NA))) %>% 
  # re-code to be on the same scale as peer deviance questions 
  # greater scores, greater intent to use
  mutate(
    path_1 = 5 - path_1,
    path_2 = 5 - path_2,
    path_3 = 5 - path_3,
    path_4 = 5 - path_4,
    path_5 = 5 - path_5,
    path_6 = 5 - path_6,
    path_7 = 5 - path_7,
    path_8 = 5 - path_8,
    path_9 = 5 - path_9)

# create summary scores
su_y_path_intuse <- su_y_path_intuse %>% 
  mutate(
    path_alc = path_2 + path_5 + path_8, 
    path_tob = path_1 + path_4 + path_7, 
    path_cb = path_3 + path_6 + path_9)

# subset relevant variables 
su_y_path_intuse <- su_y_path_intuse %>% 
  select(src_subject_id, path_alc, path_tob, path_cb)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(su_y_path_intuse, by = 'src_subject_id')

rm(su_y_path_intuse)

# Independent Variables: Parenting Behaviors  ----------------------------------

#------------------------------------------------------------------------------#
#             Community Risk and Protective Factors Survey
#------------------------------------------------------------------------------#

# import questionnaire
su_p_crpf <- read_csv('data/su_p_crpf.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(src_subject_id,
         su_risk_p_1, su_risk_p_2, su_risk_p_3, su_risk_p_4, su_risk_p_5) %>% 
  right_join(data.2_ID, by = 'src_subject_id') 

#-- Supplementary Materials ---#
# values of Don't Know 
su_p_crpf_summary <- su_p_crpf %>% 
  mutate(
    crpf_1 = case_when(
      su_risk_p_1 == 4 ~ "Response: Don’t Know (N)",
      su_risk_p_1 != 4 ~ NA),
    crpf_2 = case_when(
      su_risk_p_2 == 4 ~ "Response: Don’t Know (N)",
      su_risk_p_2 != 4 ~ NA),
    crpf_3 = case_when(
      su_risk_p_3 == 4 ~ "Response: Don’t Know (N)",
      su_risk_p_3 != 4 ~ NA),
    crpf_4 = case_when(
      su_risk_p_4 == 4 ~ "Response: Don’t Know (N)",
      su_risk_p_4 != 4 ~ NA),
    crpf_5 = case_when(
      su_risk_p_5 == 4 ~ "Response: Don’t Know (N)",
      su_risk_p_5 != 4 ~ NA))

p1 <- table(su_p_crpf_summary$crpf_1)
p2 <- table(su_p_crpf_summary$crpf_2)
p3 <- table(su_p_crpf_summary$crpf_3)
p4 <- table(su_p_crpf_summary$crpf_4)
p5 <- table(su_p_crpf_summary$crpf_5)

su_p_crpf_summary <- cbind(p1, p2, p3, p4, p5) %>% 
  as.data.frame() %>% 
  t() 

rownames(su_p_crpf_summary) <- c(
  'su_risk_p_1','su_risk_p_2', 'su_risk_p_3', 'su_risk_p_4','su_risk_p_5')

# pre-existing missing
p_pre_NA <- su_p_crpf %>% 
  select(su_risk_p_1:su_risk_p_5) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1)

su_p_crpf_summary <- cbind(su_p_crpf_summary, p_pre_NA)

su_p_crpf_summary <- su_p_crpf_summary %>% 
  mutate(
    `Response: Don’t Know (N)` = as.numeric(`Response: Don’t Know (N)`))

# total missing
su_p_crpf_summary$`Total Missing (N)` <- rowSums(
  select(su_p_crpf_summary, ends_with('(N)')))

write.csv(su_p_crpf_summary,
          'output/data_management/supplement/p_crpf_NA.csv')  
rm(p1, p2, p3, p4, p5, p_pre_NA, su_p_crpf_summary)
#-- Supplementary Materials ---#

# re- code values of (4) 'Don't Know' to NA
su_p_crpf <- su_p_crpf %>% 
  mutate(
    across(
      .cols = starts_with('su_risk'),
      .fns = ~ ifelse(.x == 4, NA, .x))) %>% 
  mutate(
    crpf = su_risk_p_1 + su_risk_p_2 + su_risk_p_3 + 
      su_risk_p_4 + su_risk_p_5) %>% 
  select(src_subject_id, crpf)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(su_p_crpf, by = 'src_subject_id') 

rm(su_p_crpf)

#------------------------------------------------------------------------------#
#                           Parent Rules
#------------------------------------------------------------------------------#

su_p_pr <- read_csv('data/su_p_pr.csv') %>% 
  subset(eventname == 'baseline_year_1_arm_1') %>% 
  select(src_subject_id,parent_rules_q1, parent_rules_q4, parent_rules_q7) %>% 
  right_join(data.2, by = 'src_subject_id') %>% 
  mutate(par_rules = parent_rules_q1 + parent_rules_q4 + parent_rules_q7) 

#-- Supplementary Materials ---#
# missing data on individual questions 
table(su_p_pr$parent_rules_q1, useNA = 'ifany')
table(su_p_pr$parent_rules_q4, useNA = 'ifany')
table(su_p_pr$parent_rules_q7, useNA = 'ifany')

temp <- su_p_pr %>% 
  mutate(
    data_check = ifelse(
      is.na(parent_rules_q1) & is.na(parent_rules_q4) & is.na(parent_rules_q7),
      1, 0))

table(temp$data_check, useNA = 'ifany') 
# n = 1 participant missing data for each of the 3 items
rm(temp)
#-- Supplementary Materials ---#

# retain total score
su_p_pr <- su_p_pr %>% 
  select(src_subject_id, par_rules)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(su_p_pr, by = 'src_subject_id')

rm(su_p_pr, su_gating, su_gating_pred, su_y_mypi, su_y_sui)

#------------------------------------------------------------------------------#
#                     Parental Monitoring Questionnaire                        #
#------------------------------------------------------------------------------#

ce_y_pm <- read_csv('data/ce_y_pm.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, pmq_y_ss_mean) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_pm, by = 'src_subject_id') 

rm(ce_y_pm)

#------------------------------------------------------------------------------#
#                       Family Environment Scale                               #
#------------------------------------------------------------------------------#

ce_y_fes <- read_csv('data/ce_y_fes.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, fes_y_ss_fc) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_fes, by = 'src_subject_id') 

rm(ce_y_fes)

#------------------------------------------------------------------------------#
#                            CRPBI                                             #
#------------------------------------------------------------------------------#

ce_y_crpbi <- read_csv('data/ce_y_crpbi.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, crpbi_y_ss_parent) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_crpbi, by = 'src_subject_id') 

rm(ce_y_crpbi)


# Independent Variables: Demographics  -----------------------------------------

#------------------------------------------------------------------------------#
#                         ABCD General: Baseline Age
#------------------------------------------------------------------------------#

abcd_y_lt <- read_csv('data/abcd_y_lt.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  mutate(age_baseline = interview_age/12) %>% 
  select(src_subject_id, age_baseline) 
abcd_y_lt$age_baseline <- round(abcd_y_lt$age_baseline, digits = 0)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(abcd_y_lt, by = 'src_subject_id')

rm(abcd_y_lt)

#------------------------------------------------------------------------------#
#                 ABCD Parent Demographics Questionnaire
#------------------------------------------------------------------------------#

abcd_p_demo <- read_csv('data/abcd_p_demo.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, demo_prnt_age_v2, demo_sex_v2, demo_gender_id_v2, 
         demo_comb_income_v2,
         demo_race_a_p___10:demo_race_a_p___25, demo_race_a_p___0, 
         demo_race_a_p___77, demo_race_a_p___99, 
         demo_ethn_v2, demo_relig_v2, demo_prnt_ed_v2) %>% 
  mutate(
    sex = factor(demo_sex_v2, 
                 levels = c(1,2,3,4), 
                 labels = c('Male', 'Female', 
                            'Intersex-Male', 'Intersex-Female')),
    eth_hisp = factor(demo_ethn_v2, 
                      levels = c(1,2), 
                      labels = c('Hispanic', 'non_Hispanic')),
    gender = factor(demo_gender_id_v2, 
                    levels = c(1,2,3,4,5,6), 
                    labels = c('Male', 'Female', 'Trans male', 'Trans female',
                               'Gender queer', 'Different')),
    income = factor(demo_comb_income_v2, 
                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                    labels = c('inc_1', 'inc_2', 'inc_3', 'inc_4', 'inc_5', 
                               'inc_6', 'inc_7', 'inc_8', 'inc_9', 'inc_10')),
    religion = factor(demo_relig_v2,
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
                                 14, 15, 16, 17),
                      labels = c('rp_1', 'rp_2', 'rp_3', 'rp_4', 'rp_5',
                                 'rp_6', 'rp_7', 'rp_8', 'rp_9', 'rp_10',
                                 'rp_11', 'rp_12', 'rp_13', 'rp_14', 
                                 'rp_15', 'rp_16', 'rp_17'))) %>% 
  right_join(data.2_ID, by = 'src_subject_id')

#-- Supplementary Materials ---#
# values of Refuse to Answer and Don't Know 

abcd_p_demo_summary <- abcd_p_demo %>% 
  mutate(
    Gender = case_when(
      demo_gender_id_v2 == 777 ~ "Response: Refuse to Answer (N)",
      demo_gender_id_v2 == 999 ~ "Response: Don't Know (N)",
      demo_gender_id_v2 < 777 ~ NA),
    Ethnicity = case_when(
      demo_ethn_v2 == 777 ~ "Response: Refuse to Answer (N)",
      demo_ethn_v2 == 999 ~ "Response: Don't Know (N)",
      demo_ethn_v2 < 777 ~ NA),
    Income = case_when(
      demo_comb_income_v2 == 777 ~ "Response: Refuse to Answer (N)",
      demo_comb_income_v2 == 999 ~ "Response: Don't Know (N)",
      demo_comb_income_v2 < 777 ~ NA),
    Religion = case_when(
      demo_relig_v2 == 777 ~ "Response: Refuse to Answer (N)",
      demo_relig_v2 == 999 ~ "Response: Don't Know (N)",
      demo_relig_v2 < 777 ~ NA))

p1 <- table(abcd_p_demo_summary$Gender)
p2 <- table(abcd_p_demo_summary$Ethnicity)
p3 <- table(abcd_p_demo_summary$Income)
p4 <- table(abcd_p_demo_summary$Religion)

abcd_p_demo_summary <- cbind(p1, p2, p3, p4) %>% 
  as.data.frame() %>% 
  t() 

rownames(abcd_p_demo_summary) <- c(
  'Gender', 'Ethnicity', 'Income','Religion')

# pre-existing missing
p_pre_NA <- abcd_p_demo %>% 
  select(demo_gender_id_v2, demo_ethn_v2, demo_comb_income_v2, demo_relig_v2) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1) 
rownames(p_pre_NA) <- c(
  'Gender','Ethnicity', 'Income','Religion')

abcd_p_demo_summary <- cbind(abcd_p_demo_summary, p_pre_NA)

write.csv(abcd_p_demo_summary,
          'output/data_management/supplement/demo_NA.csv')  
rm(p1, p2, p3, p4, p_pre_NA, abcd_p_demo_summary)

# Parent Education
table(abcd_p_demo$demo_prnt_ed_v2, useNA = 'ifany')
# n = 8 refuse to answer (777)

p_edu_freq <- table(abcd_p_demo$demo_prnt_ed_v2, useNA = 'ifany') %>% 
  as.data.frame() %>% 
  mutate(
    Category = case_when(
      Var1 == 0 ~ 'Never attended / Kindergarten only',
      Var1 == 1 ~ '1st grade',
      Var1 == 2 ~ '2nd grade',
      Var1 == 3 ~ '3rd grade',
      Var1 == 4 ~ '4th grade',
      Var1 == 5 ~ '5th grade',
      Var1 == 6 ~ '6th grade',
      Var1 == 7 ~ '7th grade',
      Var1 == 8 ~ '8th grade',
      Var1 == 9 ~ '9th grade',
      Var1 == 10 ~ '10th grade',
      Var1 == 11 ~ '11th grade',
      Var1 == 12 ~ '12th grade',
      Var1 == 13 ~ 'HS Graduate',
      Var1 == 14 ~ 'GED or Equivalent',
      Var1 == 15 ~ 'Some College',
      Var1 == 16 ~ 'Associate Degree: Occupational',
      Var1 == 17 ~ 'Associate Degree: Academic Program',
      Var1 == 18 ~ 'Bachelor’s Degree',
      Var1 == 19 ~ 'Master’s Degree',
      Var1 == 20 ~ 'Professional School Degree ',
      Var1 == 21 ~ 'Doctoral Degree')) %>% 
  select(-Var1) %>% 
  relocate(Category)
p_edu_freq

write.csv(p_edu_freq,
          'output/data_management/supplement/demo_edu_freq.csv')  
rm(p_edu_freq)
#-- Supplementary Materials ---#

# recode values of (777) Refuse to Answer and (999) as NA
abcd_p_demo <- abcd_p_demo %>%  
  mutate(
    across(.cols = ends_with('_v2'),  
           .fns = ~replace(.x, .x %in% c(999, 777), NA)))

# create summary education variable 
abcd_p_demo <- abcd_p_demo %>% 
  mutate(
    p_edu = case_when(
      demo_prnt_ed_v2 < 13 ~ 'Less_than_HS_Degree_GED_Equivalent',
      demo_prnt_ed_v2 == 13 ~ 'HS_Graduate_GED_Equivalent',
      demo_prnt_ed_v2 == 14 ~ 'HS_Graduate_GED_Equivalent',
      demo_prnt_ed_v2 == 15 ~ 'Some_College_or_Associates_Degree',
      demo_prnt_ed_v2 == 16 ~ 'Some_College_or_Associates_Degree',
      demo_prnt_ed_v2 == 17 ~ 'Some_College_or_Associates_Degree',
      demo_prnt_ed_v2 == 18 ~ 'Bachelors_Degree',
      demo_prnt_ed_v2 == 19 ~ 'Masters_Degree',
      demo_prnt_ed_v2 >= 20 ~ 'Professional_School_or_Doctoral_Degree')) %>% 
  mutate(p_edu = as.factor(p_edu))
table(abcd_p_demo$p_edu, useNA = 'ifany')

# create summary race variables           
abcd_p_demo <- abcd_p_demo %>%   
  mutate(
    White = ifelse(demo_race_a_p___10 == 1, 1, 0),
    Black = ifelse(demo_race_a_p___11 == 1, 1, 0),
    Asian = ifelse(demo_race_a_p___18 == 1 | demo_race_a_p___19 == 1 | 
                     demo_race_a_p___20 == 1 | demo_race_a_p___21 == 1 | 
                     demo_race_a_p___22 == 1 | demo_race_a_p___23 == 1 |
                     demo_race_a_p___24 == 1, 1, 0),
    AIAN = ifelse( #AIAN: American Indian Alaskan Native
      demo_race_a_p___12 == 1 | demo_race_a_p___13 == 1, 1, 0),
    NHPI = ifelse( #NHPI: Native Hawaiian and Other Pacific
      demo_race_a_p___14 == 1 | demo_race_a_p___15 == 1 | 
        demo_race_a_p___16 == 1 | demo_race_a_p___17, 1, 0),
    Other = ifelse(demo_race_a_p___25 == 1 | demo_race_a_p___0 == 1, 1, 0)) %>% 
  mutate(
    MultiRacial = ifelse(
      (White + Black + Asian + AIAN + NHPI + Other) > 1, 1, 0)) %>% 
  mutate(
    race_4l = case_when(
      White == 1 & (Black == 0 & Asian == 0 & AIAN == 0 & NHPI == 0 & Other == 0
                    & MultiRacial == 0) ~ 'White',
      Black == 1 & (White == 0 & Asian == 0 & AIAN == 0 & NHPI == 0 & Other == 0
                    & MultiRacial == 0) ~ 'Black',
      Asian == 1 & (White == 0 & Black == 0 & AIAN == 0 & NHPI == 0 & Other == 0
                    & MultiRacial == 0) ~ 'Asian',
      AIAN == 1 | NHPI == 1 | Other == 1 | MultiRacial == 1 ~ 
        'Other_MultiRacial'),
    race_4l = factor(race_4l)) 

#-- Supplementary Materials ---#
# race
# n missing race after creating 4-level race variable
table(abcd_p_demo$race_4l, useNA = 'ifany') # n = 98

# (1) breakdown of n = 98 missing
temp <- abcd_p_demo %>% 
  subset(is.na(race_4l))

table(temp$demo_race_a_p___77, useNA = 'ifany') # (1) 32
table(temp$demo_race_a_p___99, useNA = 'ifany') # (1) 54
temp_1 <- temp %>% 
  mutate(
    race_NA = ifelse(
      is.na(demo_race_a_p___77) & is.na(demo_race_a_p___99) &
        is.na(demo_race_a_p___10) & is.na(demo_race_a_p___11) &
        is.na(demo_race_a_p___12) & is.na(demo_race_a_p___13) &
        is.na(demo_race_a_p___14) & is.na(demo_race_a_p___15) &
        is.na(demo_race_a_p___16) & is.na(demo_race_a_p___17) &
        is.na(demo_race_a_p___18) & is.na(demo_race_a_p___19) &
        is.na(demo_race_a_p___20) & is.na(demo_race_a_p___21) &
        is.na(demo_race_a_p___22) & is.na(demo_race_a_p___23) &
        is.na(demo_race_a_p___24) & is.na(demo_race_a_p___25), 1, 0))
table(temp_1$race_NA, useNA = 'ifany') 
rm(temp_1)
# - n = 12 with values of NA for demo_race_a_p___77 also have NA for
#   demo_race_a_p___99 and missing all individual race variables

# note:
# - n = 32 indicated 'Refuse to Answer'
# - n = 54 indicated 'Don't Know'
# - n = 12 missing data on all 16 race variables, and variables to indicate
#   'Refuse to Answer' & 'Don't Know' 

# (2) breakdown of other possible race combinations

# check for overlap between indicated Race and 'Refuse to Answer' or 
# 'Don't Know'
temp <- abcd_p_demo %>% 
  mutate(
    race_77 = ifelse(
      (demo_race_a_p___77 == 1) &
        (demo_race_a_p___10 == 1 | demo_race_a_p___11 == 1 | 
           demo_race_a_p___12 == 1 | demo_race_a_p___13 == 1 |
           demo_race_a_p___14 == 1 | demo_race_a_p___15 == 1 |
           demo_race_a_p___16 == 1 | demo_race_a_p___17 == 1 |
           demo_race_a_p___18 == 1 | demo_race_a_p___19 == 1 |
           demo_race_a_p___20 == 1 | demo_race_a_p___21 == 1 |
           demo_race_a_p___22 == 1 | demo_race_a_p___23 == 1 |
           demo_race_a_p___24 == 1 | demo_race_a_p___25 == 1), 1, 0),
    race_99 = ifelse(
      (demo_race_a_p___99 == 1) &
        (demo_race_a_p___10 == 1 | demo_race_a_p___11 == 1 | 
           demo_race_a_p___12 == 1 | demo_race_a_p___13 == 1 |
           demo_race_a_p___14 == 1 | demo_race_a_p___15 == 1 |
           demo_race_a_p___16 == 1 | demo_race_a_p___17 == 1 |
           demo_race_a_p___18 == 1 | demo_race_a_p___19 == 1 |
           demo_race_a_p___20 == 1 | demo_race_a_p___21 == 1 |
           demo_race_a_p___22 == 1 | demo_race_a_p___23 == 1 |
           demo_race_a_p___24 == 1 | demo_race_a_p___25 == 1), 1, 0),
    race_77_99 = ifelse(
      (demo_race_a_p___77 == 1 & demo_race_a_p___99 == 1), 1, 0)) 
table(temp$race_77, temp$race_4l, useNA = 'ifany')
table(temp$race_99, temp$race_4l, useNA = 'ifany')
table(temp$race_77_99, temp$race_4l, useNA = 'ifany')
# note:
# - n = 1 'Refuse to Answer' and a race ('Other_MultiRacial')
# - n = 5 'Don't Know' & race (n = 1: Black & n = 4 Other_MultiRacial)
# - n = 1 'Refuse to Answer', 'Don't Know', and a race 
#   (Other_MultiRacial)
# - in all cases, coded as Race identified 

#-- Supplementary Materials ---#

# subset relevant variables
abcd_p_demo <- abcd_p_demo %>% 
  select(src_subject_id, 
         sex, gender, race_4l, eth_hisp, income, religion, p_edu)
str(abcd_p_demo)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(abcd_p_demo, by = 'src_subject_id')

rm(abcd_p_demo)

# recode sex due to small cell size
table(data.2$sex, useNA = 'ifany')
# n = 1 identified as "Intersex - Male"

data.2 <- data.2 %>% 
  mutate(
    sex_2l = case_when(
      sex == 'Intersex-Male' ~ 'Male',
      sex == 'Male' ~ 'Male', 
      sex == 'Female' ~ 'Female')) %>% 
  select(-sex) %>% 
  mutate(sex_2l = as.factor(sex_2l))

# due to high correlation between gender and sex, removing gender
cramerV(data.2$sex_2l, data.2$gender) # Cramer's V = 0.9988
data.2 <- data.2 %>% 
  select(-gender)


# Independent Variables: Mental Health  ----------------------------------------

#------------------------------------------------------------------------------#
#                    Child Behavior Checklist Scores (CBCL)                    #
#------------------------------------------------------------------------------#

mh_p_cbcl <- read_csv('data/mh_p_cbcl.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, starts_with('cbcl_scr_syn_') & ends_with('_r'))

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(mh_p_cbcl, by = 'src_subject_id')

rm(mh_p_cbcl)

#------------------------------------------------------------------------------#
#                           Family History                                     #
#------------------------------------------------------------------------------#

mh_p_fhx <- read_csv('data/mh_p_fhx.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id,
         famhx_ss_fath_prob_alc_p, famhx_ss_patgf_prob_alc_p,
         famhx_ss_patgm_prob_alc_p, famhx_ss_moth_prob_alc_p,
         famhx_ss_matgf_prob_alc_p, famhx_ss_matgm_prob_alc_p,
         famhx_ss_fath_prob_dg_p, famhx_ss_patgf_prob_dg_p,
         famhx_ss_patgm_prob_dg_p, famhx_ss_moth_prob_dg_p,
         famhx_ss_matgf_prob_dg_p, famhx_ss_matgm_prob_dg_p) 

#--- Supplementary Materials ---#
mh_p_fhx_summary <- mh_p_fhx %>% 
  mutate(alc_fath_NA = rowSums(is.na(select(., famhx_ss_fath_prob_alc_p))),
         alc_patgf_NA = rowSums(is.na(select(., famhx_ss_patgf_prob_alc_p))),
         alc_patgm_NA = rowSums(is.na(select(., famhx_ss_patgm_prob_alc_p))),
         alc_moth_NA = rowSums(is.na(select(., famhx_ss_moth_prob_alc_p))),
         alc_matgf_NA = rowSums(is.na(select(., famhx_ss_matgf_prob_alc_p))),
         alc_matgm_NA = rowSums(is.na(select(., famhx_ss_matgm_prob_alc_p))),
         dg_fath_NA = rowSums(is.na(select(., famhx_ss_fath_prob_dg_p))),
         dg_patgf_NA = rowSums(is.na(select(., famhx_ss_patgf_prob_dg_p))),
         dg_patgm_NA = rowSums(is.na(select(., famhx_ss_patgm_prob_dg_p))),
         dg_moth_NA = rowSums(is.na(select(., famhx_ss_moth_prob_dg_p))),
         dg_matgf_NA = rowSums(is.na(select(., famhx_ss_matgf_prob_dg_p))),
         dg_matgm_NA = rowSums(is.na(select(., famhx_ss_matgm_prob_dg_p))))

p1 <- table(mh_p_fhx_summary$alc_fath_NA)
p2 <- table(mh_p_fhx_summary$alc_patgf_NA)
p3 <- table(mh_p_fhx_summary$alc_patgm_NA)
p4 <- table(mh_p_fhx_summary$alc_moth_NA)
p5 <- table(mh_p_fhx_summary$alc_matgf_NA)
p6 <- table(mh_p_fhx_summary$alc_matgm_NA)
p7 <- table(mh_p_fhx_summary$dg_fath_NA)
p8 <- table(mh_p_fhx_summary$dg_patgf_NA)
p9 <- table(mh_p_fhx_summary$dg_patgm_NA)
p10 <- table(mh_p_fhx_summary$dg_moth_NA)
p11 <- table(mh_p_fhx_summary$dg_matgf_NA)
p12 <- table(mh_p_fhx_summary$dg_matgm_NA)

mh_p_fhx_summary <- cbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Missing Data (N)' = '1') %>% 
  select(-'0')

rownames(mh_p_fhx_summary) <- c(
  'famhx_ss_fath_prob_alc_p', 
  'famhx_ss_patgf_prob_alc_p',
  'famhx_ss_patgm_prob_alc_p', 
  'famhx_ss_moth_prob_alc_p',
  'famhx_ss_matgf_prob_alc_p', 
  'famhx_ss_matgm_prob_alc_p',
  'famhx_ss_fath_prob_dg_p', 
  'famhx_ss_patgf_prob_dg_p',
  'famhx_ss_patgm_prob_dg_p', 
  'famhx_ss_moth_prob_dg_p', 
  'famhx_ss_matgf_prob_dg_p', 
  'famhx_ss_matgm_prob_dg_p')

mh_p_fhx_summary$Description <- c(
  'father alcohol problem',
  'paternal grandfather alcohol problem',
  'paternal grandmother alcohol problem',
  'mother alcohol problem',
  'maternal grandfather alcohol problem',
  'maternal grandmother alcohol problem',
  'father drug use problem',
  'paternal grandfather drug use problem',
  'paternal grandmother drug use problem',
  'mother drug use problem',
  'maternal grandfather drug use problem',
  'maternal grandmother drug use problem')

mh_p_fhx_summary <- mh_p_fhx_summary %>% 
  relocate(Description, everything())

write.csv(mh_p_fhx_summary,
          'output/data_management/supplement/mh_fhx_summary.csv')  
rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, mh_p_fhx_summary)

#--- Supplementary Materials ---#

mh_p_fhx <- mh_p_fhx %>% 
  mutate(
    across(.cols = starts_with('famhx_ss_moth_prob') | 
             starts_with('famhx_ss_fath_prob'), 
           .fns = ~replace(.x, .x == 1, .50))) %>% 
  mutate(
    across(.cols = starts_with('famhx_ss_matg') | 
             starts_with('famhx_ss_patg'), 
           .fns = ~replace(.x, .x == 1, .25))) 

mh_p_fhx$count_na <- rowSums(is.na(select(mh_p_fhx, starts_with('famhx'))))

#--- Supplementary Materials ---#
# - number missing variables used to inform density score
mh_density_breakdown <- table(mh_p_fhx$count_na, useNA = 'ifany')
mh_density_breakdown
capture.output(
  mh_density_breakdown, 
  file = 'output/data_management/supplement/mh_fhx_density_breakdown.txt') 
rm(mh_density_breakdown)
#--- Supplementary Materials ---#

# create density score
mh_p_fhx$mh_density <- rowSums(
  (select(mh_p_fhx, starts_with('famhx'))), na.rm = TRUE)
table(mh_p_fhx$mh_density, useNA = 'ifany')

# if missing all 12 variables used in density score, code as NA 
mh_p_fhx <- mh_p_fhx %>% 
  mutate(mh_density = ifelse(count_na == 12, NA, mh_density))
table(mh_p_fhx$mh_density, useNA = 'ifany') # (NA) = 127

# merge w/full dataset 
mh_p_fhx <- mh_p_fhx %>%
  select(src_subject_id, mh_density)

data.2 <- data.2 %>% 
  left_join(mh_p_fhx, by = 'src_subject_id')

rm(mh_p_fhx)

#------------------------------------------------------------------------------#
#                       Prodromal Psychosis Symptoms                           #
#------------------------------------------------------------------------------#

mh_y_pps <- read_csv('data/mh_y_pps.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, pps_y_ss_number)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(mh_y_pps, by = 'src_subject_id')

rm(mh_y_pps)

#------------------------------------------------------------------------------#
#                               UPPS-P                                         #
#------------------------------------------------------------------------------#

mh_y_upps <- read_csv('data/mh_y_upps.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, upps_y_ss_negative_urgency,
         upps_y_ss_lack_of_perseverance, upps_y_ss_lack_of_planning,
         upps_y_ss_sensation_seeking, upps_y_ss_positive_urgency)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(mh_y_upps, by = 'src_subject_id')

rm(mh_y_upps)

#------------------------------------------------------------------------------#
#                               BIS/BAS                                        #
#------------------------------------------------------------------------------#

mh_y_bisbas <- read_csv('data/mh_y_bisbas.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, bis_y_ss_bis_sum, bis_y_ss_bas_drive, 
         bis_y_ss_bas_fs, bis_y_ss_bas_rr)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(mh_y_bisbas, by = 'src_subject_id')

rm(mh_y_bisbas)

# Independent Variables: Physical Health  --------------------------------------

#------------------------------------------------------------------------------#
#                 Sports Activities Involvement Questionnaire                  #
#------------------------------------------------------------------------------#

ph_p_saiq <- read_csv('data/ph_p_saiq.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, starts_with('sai_ss_') & ends_with('_nyr_p')) 

#--- Supplementary Materials ---#

ph_p_saiq_summary <- ph_p_saiq %>% 
  mutate(
    var1 = case_when(
      sai_ss_dance_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_dance_nyr_p != 999 ~ NA),
    var2 = case_when(
      sai_ss_base_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_base_nyr_p != 999 ~ NA),
    var3 = case_when(
      sai_ss_basket_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_basket_nyr_p != 999 ~ NA),
    var4 = case_when(
      sai_ss_climb_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_climb_nyr_p != 999 ~ NA),
    var5 = case_when(
      sai_ss_fhock_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_fhock_nyr_p != 999 ~ NA),
    var6 = case_when(
      sai_ss_fball_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_fball_nyr_p != 999 ~ NA),
    var7 = case_when(
      sai_ss_gym_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_gym_nyr_p != 999 ~ NA),
    var8 = case_when(
      sai_ss_ihock_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_ihock_nyr_p != 999 ~ NA),
    var9 = case_when(
      sai_ss_polo_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_polo_nyr_p != 999 ~ NA),  
    var10 = case_when(
      sai_ss_iskate_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_iskate_nyr_p != 999 ~ NA),  
    var11 = case_when(
      sai_ss_m_arts_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_m_arts_nyr_p != 999 ~ NA),  
    var12 = case_when(
      sai_ss_lax_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_lax_nyr_p != 999 ~ NA),  
    var13 = case_when(
      sai_ss_rugby_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_rugby_nyr_p != 999 ~ NA),  
    var14 = case_when(
      sai_ss_skate_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_skate_nyr_p != 999 ~ NA),  
    var15 = case_when(
      sai_ss_sboard_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_sboard_nyr_p != 999 ~ NA),
    var16 = case_when(
      sai_ss_soc_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_soc_nyr_p != 999 ~ NA),  
    var17 = case_when(
      sai_ss_surf_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_surf_nyr_p != 999 ~ NA),  
    var18 = case_when(
      sai_ss_wpolo_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_wpolo_nyr_p != 999 ~ NA),  
    var19 = case_when(
      sai_ss_tennis_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_tennis_nyr_p != 999 ~ NA), 
    var20 = case_when(
      sai_ss_run_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_run_nyr_p != 999 ~ NA),  
    var21 = case_when(
      sai_ss_mma_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_mma_nyr_p != 999 ~ NA),  
    var22 = case_when(
      sai_ss_vball_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_vball_nyr_p != 999 ~ NA),  
    var23 = case_when(
      sai_ss_yoga_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_yoga_nyr_p != 999 ~ NA),  
    var24 = case_when(
      sai_ss_music_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_music_nyr_p != 999 ~ NA),  
    var25 = case_when(
      sai_ss_art_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_art_nyr_p != 999 ~ NA),  
    var26 = case_when(
      sai_ss_drama_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_drama_nyr_p != 999 ~ NA),  
    var27 = case_when(
      sai_ss_crafts_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_crafts_nyr_p != 999 ~ NA),  
    var28 = case_when(
      sai_ss_chess_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_chess_nyr_p != 999 ~ NA),  
    var29 = case_when(
      sai_ss_collect_nyr_p == 999 ~ "Response: Don’t Know (N)",
      sai_ss_collect_nyr_p != 999 ~ NA))

p1 <- table(ph_p_saiq_summary$var1)
p2 <- table(ph_p_saiq_summary$var2)
p3 <- table(ph_p_saiq_summary$var3)
p4 <- table(ph_p_saiq_summary$var4)
p5 <- table(ph_p_saiq_summary$var5)
p6 <- table(ph_p_saiq_summary$var6)
p7 <- table(ph_p_saiq_summary$var7)
p8 <- table(ph_p_saiq_summary$var8)
p9 <- table(ph_p_saiq_summary$var9)
p10 <- table(ph_p_saiq_summary$var10)
p11 <- table(ph_p_saiq_summary$var11)
p12 <- table(ph_p_saiq_summary$var12)
p13 <- table(ph_p_saiq_summary$var13)
p14 <- table(ph_p_saiq_summary$var14)
p15 <- table(ph_p_saiq_summary$var15)
p16 <- table(ph_p_saiq_summary$var16)
p17 <- table(ph_p_saiq_summary$var17)
p18 <- table(ph_p_saiq_summary$var18)
p19 <- table(ph_p_saiq_summary$var19)
p20 <- table(ph_p_saiq_summary$var20)
p21 <- table(ph_p_saiq_summary$var21)
p22 <- table(ph_p_saiq_summary$var22)
p23 <- table(ph_p_saiq_summary$var23)
p24 <- table(ph_p_saiq_summary$var24)
p25 <- table(ph_p_saiq_summary$var25)
p26 <- table(ph_p_saiq_summary$var26)
p27 <- table(ph_p_saiq_summary$var27)
p28 <- table(ph_p_saiq_summary$var28)
p29 <- table(ph_p_saiq_summary$var29)

ph_p_saiq_summary <- cbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                           p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
                           p21, p22, p23, p24, p25, p26, p27, p28, p29) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame()

ph_p_saiq_summary$Variable <- c(
  'sai_ss_dance_nyr_p',
  'sai_ss_base_nyr_p',
  'sai_ss_basket_nyr_p',
  'sai_ss_climb_nyr_p',
  'sai_ss_fhock_nyr_p',
  'sai_ss_fball_nyr_p',
  'sai_ss_gym_nyr_p',
  'sai_ss_ihock_nyr_p',
  'sai_ss_polo_nyr_p',
  'sai_ss_iskate_nyr_p',
  'sai_ss_m_arts_nyr_p',
  'sai_ss_lax_nyr_p',
  'sai_ss_rugby_nyr_p',
  'sai_ss_skate_nyr_p',
  'sai_ss_sboard_nyr_p',
  'sai_ss_soc_nyr_p',
  'sai_ss_surf_nyr_p',
  'sai_ss_wpolo_nyr_p',
  'sai_ss_tennis_nyr_p',
  'sai_ss_run_nyr_p',
  'sai_ss_mma_nyr_p',
  'sai_ss_vball_nyr_p',
  'sai_ss_yoga_nyr_p',
  'sai_ss_music_nyr_p',
  'sai_ss_art_nyr_p',
  'sai_ss_drama_nyr_p',
  'sai_ss_crafts_nyr_p',
  'sai_ss_chess_nyr_p',
  'sai_ss_collect_nyr_p')

ph_p_saiq_summary$Description <- c(
  'Dance',
  'Baseball',
  'Basketball',
  'Climbing',
  'Field Hockey',
  'Football',
  'Gymnastics',
  'Ice Hockey',
  'Horseback Riding',
  'Ice or Inline Skating',
  'Martial Arts',
  'Lacrosse',
  'Rugby',
  'Skateboarding',
  'Snowboarding',
  'Soccer',
  'Surfing',
  'Water Polo',
  'Tennis',
  'Track, Running, Cross-Country',
  'Wrestling, Mixed Martial Arts',
  'Volleyball',
  'Yoga, Tai Chi',
  'Musical Instrument',
  'Drawing, Painting, Graphic Art, Photography, Pottery, Sculpting',
  'Drama, Theater, Acting, Film',
  'Crafts like Knitting, Building Model Cars, or Airplanes',
  'Competitive Games like Chess, Cards, or Darts',
  'Hobbies like collecting stamps or coins')

# pre-existing missing
p_pre_NA <- ph_p_saiq %>% 
  select(starts_with('sai_ss_') & ends_with('_nyr_p')) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1)

ph_p_saiq_summary <- cbind(ph_p_saiq_summary, p_pre_NA)

ph_p_saiq_summary <- ph_p_saiq_summary %>% 
  mutate(
    `Response: Don’t Know (N)` = as.numeric(`Response: Don’t Know (N)`))

# total missing
ph_p_saiq_summary$`Total Missing (N)` <- rowSums(
  select(ph_p_saiq_summary, ends_with('(N)')))

ph_p_saiq_summary <- ph_p_saiq_summary %>% 
  relocate(Variable, Description, `Response: Don’t Know (N)`, 
           `Pre-Existing Missing (N)`, `Total Missing (N)`)

write.csv(ph_p_saiq_summary,
          'output/data_management/supplement/ph_saiq_NA.csv')  
rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
   p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
   p21, p22, p23, p24, p25, p26, p27, p28, p29,
   p_pre_NA, ph_p_saiq_summary)
#--- Supplementary Materials ---#

# recode values of '999' to NA
ph_p_saiq <- ph_p_saiq %>% 
  mutate(
    across(.cols = starts_with('sai_ss_'), 
           .fns = ~replace(.x, .x == 999, NA))) %>% 
  # any activity duration > 0 years = 1
  mutate(
    across(.cols = starts_with('sai_ss_'), 
           .fns = ~replace(.x, .x > 0, 1)))

# count number missing recreational activity vars (n=29 activities)
ph_p_saiq$count_na <- rowSums(is.na(select(ph_p_saiq, starts_with('sai_ss_')))) 

#--- Supplementary Materials ---#
ph_p_saiq_NA <- table(ph_p_saiq$count_na, useNA = 'ifany') 
# n = 1 missing all 29 vars

capture.output(
  ph_p_saiq_NA,
  file = 'output/data_management/supplement/ph_saiq_NAcount.txt') 
rm(ph_p_saiq_NA)
#--- Supplementary Materials ---#

# create continuous and binary recreational activity variables
ph_p_saiq <- ph_p_saiq %>% 
  mutate(
    rec_con = rowSums((select(ph_p_saiq, starts_with('sai_ss_'))), 
                      na.rm = TRUE),
    rec_con = ifelse(count_na == 29, NA, rec_con),
    rec_bin = ifelse(rec_con == 0, 0, 1),
    rec_bin = factor(rec_bin,
                     levels = c(0, 1),
                     labels = c('No', 'Yes'))) %>% 
  select(src_subject_id, rec_con, rec_bin)

table(ph_p_saiq$rec_con, useNA = 'ifany') # NA = 1
table(ph_p_saiq$rec_bin, useNA = 'ifany') # NA = 1

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ph_p_saiq, by = 'src_subject_id')

rm(ph_p_saiq)

#------------------------------------------------------------------------------#
#                             Screen Time Survey (STQ)                         #
#------------------------------------------------------------------------------#

nt_p_stq <- read_csv('data/nt_p_stq.csv')  %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, screentime1_p_hours, screentime2_p_hours) 

#--- Supplementary Materials ---#  
temp <- nt_p_stq %>% 
  mutate(
    nt18_weekday = ifelse(screentime1_p_hours >= 18, 1, 0),
    nt18_weekend = ifelse(screentime2_p_hours >= 18, 1, 0)) %>% 
  select(nt18_weekday, nt18_weekend)

table(temp$nt18_weekday, useNA = 'ifany') # (1) = 39
table(temp$nt18_weekend, useNA = 'ifany') # (1) = 26
rm(temp)
#--- Supplementary Materials ---#  

# recode if >= 18 to NA
nt_p_stq <- nt_p_stq %>% 
  mutate(
    screentime1_p_hours = ifelse(
      screentime1_p_hours >= 18, NA, screentime1_p_hours),
    screentime2_p_hours = ifelse(
      screentime2_p_hours >= 18, NA, screentime2_p_hours)) 

#--- Supplementary Materials ---#  
# - missing after recoding >= 18

table(nt_p_stq$screentime1_p_hours, useNA = 'ifany') # (NA) = 43
table(nt_p_stq$screentime2_p_hours, useNA = 'ifany') # (NA) = 30

#--- Supplementary Materials ---#  

nt_p_stq <-  nt_p_stq %>% 
  mutate(screentime = screentime1_p_hours + screentime2_p_hours) %>% 
  select(src_subject_id, screentime)

table(nt_p_stq$screentime, useNA = 'ifany')

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(nt_p_stq, by = 'src_subject_id')

rm(nt_p_stq)

#------------------------------------------------------------------------------#
#         Youth Risk Behavior Survey (YRB) - Exercise Physical Activity        #
#------------------------------------------------------------------------------#

ph_y_yrb <- read_csv('data/ph_y_yrb.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, physical_activity1_y, physical_activity2_y, 
         physical_activity5_y)%>% 
  mutate(act1 = physical_activity1_y,
         # recode physical_activity2_y to be on the same scale as question 1
         act2 = physical_activity2_y - 1, 
         act5 = physical_activity5_y) %>% 
  select(src_subject_id, act1, act2, act5)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ph_y_yrb, by = 'src_subject_id')

rm(ph_y_yrb)

#------------------------------------------------------------------------------#
#                 Developmental History Questionnaire                          #
#------------------------------------------------------------------------------#

ph_p_dhx <- read_csv('data/ph_p_dhx.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, 
         devhx_8_alcohol, devhx_8_tobacco, devhx_8_marijuana, 
         devhx_8_coc_crack, devhx_8_her_morph, devhx_8_oxycont, 
         devhx_8_other_drugs,
         devhx_9_alcohol, devhx_9_tobacco, devhx_9_marijuana, 
         devhx_9_coc_crack, devhx_9_her_morph, devhx_9_oxycont,
         devhx_9_other_drugs, devhx_caffeine_11) 

#--- Supplementary Materials ---#

ph_p_dhx_summary <- ph_p_dhx %>% 
  mutate(
    pre_alcohol = case_when(
      devhx_8_alcohol == 999 ~ "Response: Don’t Know (N)",
      devhx_8_alcohol != 999 ~ NA),
    pre_tobacco = case_when(
      devhx_8_tobacco == 999 ~ "Response: Don’t Know (N)",
      devhx_8_tobacco != 999 ~ NA),
    pre_marijuana = case_when(
      devhx_8_marijuana == 999 ~ "Response: Don’t Know (N)",
      devhx_8_marijuana != 999 ~ NA),
    pre_coc_crack = case_when(
      devhx_8_coc_crack == 999 ~ "Response: Don’t Know (N)",
      devhx_8_coc_crack != 999 ~ NA),
    pre_her_morph = case_when(
      devhx_8_her_morph == 999 ~ "Response: Don’t Know (N)",
      devhx_8_her_morph != 999 ~ NA),
    pre_oxycont = case_when(
      devhx_8_oxycont == 999 ~ "Response: Don’t Know (N)",
      devhx_8_oxycont != 999 ~ NA),
    pre_other_drugs = case_when(
      devhx_8_other_drugs == 999 ~ "Response: Don’t Know (N)",
      devhx_8_other_drugs != 999 ~ NA),
    post_alcohol = case_when(
      devhx_9_alcohol == 999 ~ "Response: Don’t Know (N)",
      devhx_9_alcohol != 999 ~ NA),
    post_tobacco = case_when(
      devhx_9_tobacco == 999 ~ "Response: Don’t Know (N)",
      devhx_9_tobacco != 999 ~ NA),
    post_marijuana = case_when(
      devhx_9_marijuana == 999 ~ "Response: Don’t Know (N)",
      devhx_9_marijuana != 999 ~ NA),
    post_coc_crack = case_when(
      devhx_9_coc_crack == 999 ~ "Response: Don’t Know (N)",
      devhx_9_coc_crack != 999 ~ NA),
    post_her_morph = case_when(
      devhx_9_her_morph == 999 ~ "Response: Don’t Know (N)",
      devhx_9_her_morph != 999 ~ NA),
    post_oxycont = case_when(
      devhx_9_oxycont == 999 ~ "Response: Don’t Know (N)",
      devhx_9_oxycont != 999 ~ NA),
    post_other_drugs = case_when(
      devhx_9_other_drugs == 999 ~ "Response: Don’t Know (N)",
      devhx_9_other_drugs != 999 ~ NA))

p1 <- table(ph_p_dhx_summary$pre_alcohol)
p2 <- table(ph_p_dhx_summary$pre_tobacco)
p3 <- table(ph_p_dhx_summary$pre_marijuana)
p4 <- table(ph_p_dhx_summary$pre_coc_crack)
p5 <- table(ph_p_dhx_summary$pre_her_morph)
p6 <- table(ph_p_dhx_summary$pre_oxycont)
p7 <- table(ph_p_dhx_summary$pre_other_drugs)
p8 <- table(ph_p_dhx_summary$post_alcohol)
p9 <- table(ph_p_dhx_summary$post_tobacco)
p10 <- table(ph_p_dhx_summary$post_marijuana)
p11 <- table(ph_p_dhx_summary$post_coc_crack)
p12 <- table(ph_p_dhx_summary$post_her_morph)
p13 <- table(ph_p_dhx_summary$post_oxycont)
p14 <- table(ph_p_dhx_summary$post_other_drugs)

ph_p_dhx_summary <- cbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                          p11, p12, p13, p14) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame()

ph_p_dhx_summary$Variable <- c(
  'devhx_8_alcohol', 
  'devhx_8_tobacco', 
  'devhx_8_marijuana', 
  'devhx_8_coc_crack', 
  'devhx_8_her_morph', 
  'devhx_8_oxycont', 
  'devhx_8_other_drugs',
  'devhx_9_alcohol', 
  'devhx_9_tobacco', 
  'devhx_9_marijuana', 
  'devhx_9_coc_crack', 
  'devhx_9_her_morph', 
  'devhx_9_oxycont',
  'devhx_9_other_drugs')

ph_p_dhx_summary$Description <- c(
  'Alcohol',
  'Tobacco',
  'Cannabis',
  'Cocaine / Crack',
  'Heroin / Morphine',
  'OxyContin',
  'Any other drugs', 
  'Alcohol',
  'Tobacco',
  'Cannabis',
  'Cocaine / Crack',
  'Heroin / Morphine',
  'OxyContin',
  'Any other drugs')

# pre-existing missing
p_pre_NA <- ph_p_dhx %>% 
  select(devhx_8_alcohol, devhx_8_tobacco, devhx_8_marijuana, 
         devhx_8_coc_crack, devhx_8_her_morph, devhx_8_oxycont, 
         devhx_8_other_drugs,
         devhx_9_alcohol, devhx_9_tobacco, devhx_9_marijuana, 
         devhx_9_coc_crack, devhx_9_her_morph, devhx_9_oxycont,
         devhx_9_other_drugs) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1)

ph_p_dhx_summary <- cbind(ph_p_dhx_summary, p_pre_NA)

ph_p_dhx_summary <- ph_p_dhx_summary %>% 
  mutate(
    `Response: Don’t Know (N)` = as.numeric(`Response: Don’t Know (N)`))

# total missing
ph_p_dhx_summary$`Total Missing (N)` <- rowSums(
  select(ph_p_dhx_summary, ends_with('(N)')))

ph_p_dhx_summary <- ph_p_dhx_summary %>% 
  relocate(Variable, Description, `Response: Don’t Know (N)`, 
           `Pre-Existing Missing (N)`, `Total Missing (N)`)

# add row indicating before and after knowledge
ph_p_dhx_summary <- ph_p_dhx_summary %>% 
  add_row(
    Variable = 'Before Knowing of Pregnancy', Description = 'Use of...',
    `Response: Don’t Know (N)` = NA,
    `Pre-Existing Missing (N)` = NA,
    `Total Missing (N)` = NA,
    .before = 1) %>% 
  add_row(
    Variable = 'After Knowing of Pregnancy', Description = 'Use of...',
    `Response: Don’t Know (N)` = NA,
    `Pre-Existing Missing (N)` = NA,
    `Total Missing (N)` = NA,
    .before = 9) 

write.csv(ph_p_dhx_summary,
          'output/data_management/supplement/ph_dhx_NA.csv')  
rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
   p11, p12, p13, p14, 
   p_pre_NA, ph_p_dhx_summary)

#--- Supplementary Materials ---#

# re-code values of 999 to NA for non-caffeine variables
ph_p_dhx <- ph_p_dhx %>% 
  mutate(
    across(.cols = starts_with('devhx_8') | starts_with('devhx_9'),  
           .fns = ~replace(.x, .x %in% c(999), NA))) 

#--- Supplementary Materials ---#
# count NA across all vars (except caffeine)
ph_p_dhx$count_na <- rowSums(is.na(select(ph_p_dhx, starts_with('devhx_8') |
                                            starts_with('devhx_9'))))

table(ph_p_dhx$count_na, useNA = 'ifany') 
#--- Supplementary Materials ---#

# create binary substance exposure variable 
ph_p_dhx <- ph_p_dhx %>% 
  mutate(exp_sub = rowSums((select(
    ph_p_dhx, starts_with('devhx_8') | starts_with('devhx_9'))), 
    na.rm = TRUE),
    exp_sub = ifelse(exp_sub == 0, 0, 1),
    exp_sub = ifelse(count_na == 14, NA, exp_sub),   
    exp_sub = factor(exp_sub,
                     levels = c(0, 1),
                     labels = c('No', 'Yes')))

table(ph_p_dhx$exp_sub, useNA = 'ifany')# NA = 88

#--- Supplementary Materials ---#

# - missing data on caffeine variable
table(ph_p_dhx$devhx_caffeine_11, useNA = 'ifany')
# (-1) = 18
# (999) = 540
# (NA) = 2

#--- Supplementary Materials ---#

# create caffeine use variable
ph_p_dhx <- ph_p_dhx %>% 
  mutate(
    exp_caf = ifelse(
      (devhx_caffeine_11 == -1 | devhx_caffeine_11 == 999), NA, 
      devhx_caffeine_11)) %>% 
  mutate(
    exp_caf_rec = case_when(
      exp_caf == 0 ~ '0', # no caffeine
      exp_caf == 3 ~ '1', # less than once a week
      exp_caf == 2 ~ '2', # not daily but more than 1x/week
      exp_caf == 1 ~ '3')) %>%  # daily
  mutate(exp_caf_rec = as.numeric(exp_caf_rec))  

# merge w/full dataset 
ph_p_dhx <- ph_p_dhx %>% 
  select(src_subject_id, exp_sub, exp_caf_rec)

data.2 <- data.2 %>% 
  left_join(ph_p_dhx, by = 'src_subject_id')

rm(ph_p_dhx)

#------------------------------------------------------------------------------#
#                            Sleep Disorder Scale                              #
#------------------------------------------------------------------------------#

ph_p_sds <- read_csv('data/ph_p_sds.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, sds_p_ss_dims, sds_p_ss_sbd, sds_p_ss_da,
         sds_p_ss_swtd, sds_p_ss_does, sds_p_ss_shy)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ph_p_sds, by = 'src_subject_id')

rm(ph_p_sds)

#------------------------------------------------------------------------------#
#                         Puberty Development Scale                            #
#------------------------------------------------------------------------------#

ph_p_pds <- read_csv('data/ph_p_pds.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, pds_p_ss_female_category, pds_p_ss_male_category)

# create z-scores for each sex
df_sex <- data.2 %>% 
  select(src_subject_id, sex_2l) 

ph_p_pds <- ph_p_pds %>% 
  left_join(df_sex, by = 'src_subject_id') %>% 
  mutate(pds_female_z = 
           ifelse(sex_2l == 'Female', scale(pds_p_ss_female_category), NA),
         pds_male_z = 
           ifelse(sex_2l == 'Male', scale(pds_p_ss_male_category), NA)) 

# combine z-scores for each sex into a single variable
ph_p_pds$pds <- rowSums(ph_p_pds[5:6], na.rm = TRUE)

# if sex is missing PDS, recode to NA
ph_p_pds <- ph_p_pds %>% 
  mutate(
    pds = ifelse(
      (sex_2l == 'Female' & is.na(pds_p_ss_female_category) |
         sex_2l == 'Male' & is.na(pds_p_ss_male_category) ), NA, pds))

#--- Supplementary Materials ---#

# check for missing variable in summary score
table(ph_p_pds$pds_p_ss_female_category, ph_p_pds$sex_2l, useNA = 'ifany') 
# NA for female: 107
table(ph_p_pds$pds_p_ss_male_category, ph_p_pds$sex_2l, useNA = 'ifany')
# NA for male: 125
table(ph_p_pds$pds, ph_p_pds$sex_2l, useNA = 'ifany')
# NA for female (107) and NA for male (125)

#--- Supplementary Materials ---#

# merge w/full dataset 
ph_p_pds <- ph_p_pds %>% 
  select(src_subject_id, pds)

data.2 <- data.2 %>% 
  left_join(ph_p_pds, by = 'src_subject_id')

rm(ph_p_pds, df_sex)

#------------------------------------------------------------------------------#
#                  Ohio State Traumatic Brain Injury                           #
#------------------------------------------------------------------------------#

ph_p_otbi <- read_csv('data/ph_p_otbi.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, tbi_1, tbi_2, tbi_3, tbi_4) 

#--- Supplementary Materials ---#
# - check for missing data across each of the 4 items

ph_p_otbi_summary <- ph_p_otbi %>% 
  mutate(otbi_NA = rowSums(is.na(select(., starts_with('tbi_')))))

table(ph_p_otbi_summary$otbi_NA, useNA = 'ifany')
# no ID missing data across each of the 4 items

rm(ph_p_otbi_summary)

#--- Supplementary Materials ---#

ph_p_otbi <- ph_p_otbi %>% 
  mutate(
    tbi_injury = ifelse(
      (tbi_1 == 1 | tbi_2 == 1 | tbi_3 == 1 | tbi_4 == 1), 'Yes', 'No'),
    tbi_injury = factor(tbi_injury)) %>% 
  select(src_subject_id, tbi_injury) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ph_p_otbi, by = 'src_subject_id')

rm(ph_p_otbi)

# Independent Variables: Culture & Environment  --------------------------------

#------------------------------------------------------------------------------#
#                        Acculturation Survey  (ACC)                           #
#------------------------------------------------------------------------------#

ce_y_acc <- read_csv('data/ce_y_acc.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') 

#--- Supplementary Materials ---#
# - values of Refuse to Answer and Don't Know

ce_y_acc_summary <- ce_y_acc %>% 
  mutate(
    var1 = case_when(
      accult_q2_y == 777 ~ "Response: Refuse to Answer (N)",
      accult_q2_y == 999 ~ "Response: Refuse to Answer (N)",
      accult_q2_y <777 ~ NA))

ce_y_acc_summary <- table(ce_y_acc_summary$var1) %>% 
  as.data.frame()  

rownames(ce_y_acc_summary) <- c('accult_q2_y')

# pre-existing missing
p_pre_NA <- ce_y_acc %>% 
  select(accult_q2_y) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename('Pre-Existing Missing (N)' = V1)

ce_y_acc_summary <- cbind(ce_y_acc_summary, p_pre_NA)

write.csv(ce_y_acc_summary,
          'output/data_management/supplement/ce_acc_NA.csv')  

rm(ce_y_acc_summary, p_pre_NA)

#--- Supplementary Materials ---#

# recode (999) Don't Know and (777) Refuse to answer as NA
ce_y_acc <- ce_y_acc %>% 
  select(src_subject_id, accult_q2_y) %>% 
  mutate(
    across(.cols = accult_q2_y,  
           .fns = ~replace(.x, .x %in% c(999, 777), NA))) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_acc, by = 'src_subject_id')

rm(ce_y_acc)

#------------------------------------------------------------------------------#
#                   Neighborhood Safety/Crime Survey NSC                       #
#------------------------------------------------------------------------------#

ce_y_nsc <- read_csv('data/ce_y_nsc.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, neighborhood_crime_y) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_nsc, by = 'src_subject_id')

rm(ce_y_nsc)

#------------------------------------------------------------------------------#
#             Diagnostic Interview for DSM-5 Background Items                  #
#------------------------------------------------------------------------------#

mh_p_ksads_bg <- read_csv('data/mh_p_ksads_bg.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, kbi_p_grades_in_school, kbi_p_c_det_susp,
         kbi_p_c_spec_serv___1:kbi_p_c_spec_serv___10) 

#--- Supplementary Materials ---#
# Grades: 'Ungraded' or 'Not Applicable' responses
# Detention / suspension: 'Refuse to Answer' or 'Don't Know' responses 

# values of 'Don't Know' and N/A

mh_p_ksads_bg_summary <- mh_p_ksads_bg %>% 
  mutate(
    grades = case_when(
      kbi_p_grades_in_school == 6 ~ "Ungraded (N)",
      kbi_p_grades_in_school == -1 ~ "Not Applicable (N)",
      kbi_p_grades_in_school > -1 & kbi_p_grades_in_school < 6  ~ NA),
    det_susp = case_when(
      kbi_p_c_det_susp == 3 ~ "Response: Not Sure (N)",
      kbi_p_c_det_susp == 777 ~ "Response: Decline to Answer (N)",
      kbi_p_c_det_susp <3 ~ NA))

p1 <- table(mh_p_ksads_bg_summary$grades)
p2 <- table(mh_p_ksads_bg_summary$det_susp)
p1 <- as.data.frame(p1)
p2 <- as.data.frame(p2)

# pre-existing missing
p_pre_NA_grades <- mh_p_ksads_bg %>% 
  select(kbi_p_grades_in_school) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  mutate(Var1 = c('Pre-Existing Missing (N)')) %>% 
  rename(Freq = kbi_p_grades_in_school) %>% 
  relocate(Var1)

p_pre_NA_det <- mh_p_ksads_bg %>% 
  select(kbi_p_c_det_susp) %>% 
  summarise_all(~ sum(is.na(.))) %>% 
  mutate(Var1 = c('Pre-Existing Missing (N)')) %>% 
  rename(Freq = kbi_p_c_det_susp) %>% 
  relocate(Var1)

mh_p_ksads_bg_grades_sum <- rbind(p1, p_pre_NA_grades)
mh_p_ksads_bg_det_sum <- rbind(p2, p_pre_NA_det)

write.csv(mh_p_ksads_bg_grades_sum,
          'output/data_management/supplement/ce_ksads_bg_grades_NA.csv',
          row.names = TRUE)  

write.csv(mh_p_ksads_bg_det_sum,
          'output/data_management/supplement/ce_ksads_bg_det_NA.csv',
          row.names = TRUE)  

rm(p1, p2, p_pre_NA_grades, p_pre_NA_det, 
   mh_p_ksads_bg_grades_sum, mh_p_ksads_bg_det_sum,
   mh_p_ksads_bg_summary)

# values of 'Not Sure' (3) and 'Decline to Answer' (777)
table(mh_p_ksads_bg$kbi_p_c_det_susp)
# (3) = 27
# (777) = 3

#--- Supplementary Materials ---#

# recode variables as needed
mh_p_ksads_bg <- mh_p_ksads_bg %>% 
  mutate(
    across(.cols = kbi_p_grades_in_school,  
           .fns = ~replace(.x, .x %in% c(6, -1), NA)), 
    across(.cols = kbi_p_c_det_susp,  
           .fns = ~replace(.x, .x %in% c(777, 3), NA))) %>% 
  mutate(kbi_p_grades_in_school = 
           factor(kbi_p_grades_in_school,
                  levels = c(1, 2, 3, 4, 5),
                  labels = c('Grade_A', 'Grade_B', 'Grade_C', 
                             'Grade_D', 'Grade_F'))) %>% 
  mutate(
    kbi_p_c_det_susp = case_when(
      kbi_p_c_det_susp == 1 ~ 'Yes',
      kbi_p_c_det_susp == 2 ~ 'No'), 
    kbi_p_c_det_susp = as.factor(kbi_p_c_det_susp))

# check if multiple special education categories are selected 
mh_p_ksads_bg$se_multi <- rowSums(
  (select(mh_p_ksads_bg, starts_with('kbi_p_c_spec'))), na.rm = TRUE)

table(mh_p_ksads_bg$se_multi, useNA = 'ifany')
# (0) = 101, (1) = 6365, (2) = 296, (3) = 48, (4) = 14, (5) = 5
# 0 = no service selected of the 10 options
# 1 = 1 service selected of the 10 options (n = 6364 indicated 1 option)
# 2 = 2 services selected of the 10 options ... continue through 5 services
# n = 363 indicated >1 response (296 + 48 + 14 + 5)

# create special education services groups
mh_p_ksads_bg <- mh_p_ksads_bg %>% 
  mutate(
    se_services = case_when(
      se_multi == 0 ~ as.character(NA),
      se_multi == 1 & 
        (kbi_p_c_spec_serv___1 == 1 | kbi_p_c_spec_serv___2 == 1 |
           kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
           kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
           kbi_p_c_spec_serv___7 == 1 ) ~ 'Emotion_or_Learning_Support',
      se_multi == 1 & kbi_p_c_spec_serv___8 == 1 ~ 'Gifted', 
      se_multi == 1 & kbi_p_c_spec_serv___9 == 1 ~ 'Other', 
      se_multi == 1 & kbi_p_c_spec_serv___10 == 1 ~ 'None', 
      se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | 
                         kbi_p_c_spec_serv___2 == 1 |
                         kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                         kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                         kbi_p_c_spec_serv___7 == 1) & (kbi_p_c_spec_serv___8 == 0 & 
                                                          kbi_p_c_spec_serv___9 == 0 & kbi_p_c_spec_serv___10 == 0) 
      ~ 'Emotion_or_Learning_Support',
      se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | 
                         kbi_p_c_spec_serv___2 == 1 |
                         kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                         kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                         kbi_p_c_spec_serv___7 == 1) & 
        (kbi_p_c_spec_serv___8 == 1 & kbi_p_c_spec_serv___9 == 0) & 
        (kbi_p_c_spec_serv___10 == 0) 
      ~ 'Combined Services_1', 
      se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | 
                         kbi_p_c_spec_serv___2 == 1 |
                         kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                         kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                         kbi_p_c_spec_serv___7 == 1) & 
        (kbi_p_c_spec_serv___8 == 0 & kbi_p_c_spec_serv___9 == 1) & 
        (kbi_p_c_spec_serv___10 == 0) 
      ~ 'Combined Services_2', 
      se_multi >= 2 & (kbi_p_c_spec_serv___1 == 0 & 
                         kbi_p_c_spec_serv___2 == 0 &
                         kbi_p_c_spec_serv___3 == 0 & kbi_p_c_spec_serv___4 == 0 &
                         kbi_p_c_spec_serv___5 == 0 & kbi_p_c_spec_serv___6 == 0 &
                         kbi_p_c_spec_serv___7 == 0) & 
        (kbi_p_c_spec_serv___8 == 1 & kbi_p_c_spec_serv___9 == 1) & 
        (kbi_p_c_spec_serv___10 == 0) 
      ~ 'Combined Services_3', 
      se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | 
                         kbi_p_c_spec_serv___2 == 1 |
                         kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                         kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                         kbi_p_c_spec_serv___7 == 1) & 
        (kbi_p_c_spec_serv___8 == 1 & kbi_p_c_spec_serv___9 == 1) & 
        (kbi_p_c_spec_serv___10 == 0)
      ~ 'Combined Services_4',
      se_multi >= 2 & kbi_p_c_spec_serv___10 == 1 ~ 'None'))  

table(mh_p_ksads_bg$se_services, useNA = 'ifany')
# Combined services_1 = Emotion or learning support + Gifted 
# Combined services_2 = Emotion or learning support + Other
# Combined services_3 = Gifted + Other
# Combined services_4 = Emotion or learning support + Gifted + Other  

# breakdown of those who indicated multiple groups
table(mh_p_ksads_bg$se_multi, mh_p_ksads_bg$se_services, useNA = 'ifany')

# combine combined services groups into larger category
mh_p_ksads_bg <- mh_p_ksads_bg %>% 
  mutate(
    se_services = if_else(
      se_services == 'Combined Services_1' | 
        se_services == 'Combined Services_2' |
        se_services == 'Combined Services_3' |
        se_services == 'Combined Services_4', 'Combined_Services', se_services))
table(mh_p_ksads_bg$se_services, useNA = 'ifany') # (NA) = 101

#--- Supplementary Materials ---#

# Special education services: breakdown for multiple types of services

# (1) None and any other service (coded as 'None')
mh_p_ksads_bg_1 <- mh_p_ksads_bg %>% 
  mutate(
    none_v1 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___1 == 1, 1, 0),
    none_v2 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___2 == 1, 1, 0),
    none_v3 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___3 == 1, 1, 0),
    none_v4 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___4 == 1, 1, 0),
    none_v5 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___5 == 1, 1, 0),
    none_v6 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___6 == 1, 1, 0),
    none_v7 = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___7 == 1, 1, 0),
    none_Gifted = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___8 == 1, 1, 0),
    none_Other = ifelse(
      kbi_p_c_spec_serv___10 == 1 & kbi_p_c_spec_serv___9 == 1, 1, 0)) %>% 
  mutate(
    none_ELS = ifelse(
      none_v1 == 1 | none_v2 == 1 | none_v3 == 1 | none_v4 == 1 | none_v5 == 1 | 
        none_v6 == 1 | none_v1 == 1, 1, 0)) %>% 
  select(src_subject_id, none_ELS, none_Gifted, none_Other)

mh_p_ksads_bg_1_print <- mh_p_ksads_bg_1 %>% 
  Desc(., plotit = FALSE) 
capture.output(
  mh_p_ksads_bg_1_print, 
  file = 'output/data_management/supplement/mh_ksads_bg_1.txt')

rm(mh_p_ksads_bg_1, mh_p_ksads_bg_1_print)

# (2) ELS and Gifted (coded as 'Combined Services')
mh_p_ksads_bg_2 <- mh_p_ksads_bg %>% 
  mutate(ELS_Gifted = ifelse(
    se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | kbi_p_c_spec_serv___2 == 1 |
                       kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                       kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                       kbi_p_c_spec_serv___7 == 1) & kbi_p_c_spec_serv___8 == 1 & 
      kbi_p_c_spec_serv___9 == 0 & kbi_p_c_spec_serv___10 == 0, 1, 0)) %>% 
  mutate(ELS_Other = ifelse(
    se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | kbi_p_c_spec_serv___2 == 1 |
                       kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                       kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                       kbi_p_c_spec_serv___7 == 1) & kbi_p_c_spec_serv___8 == 0 &
      kbi_p_c_spec_serv___9 == 1 & kbi_p_c_spec_serv___10 == 0, 1, 0)) %>% 
  mutate(Gifted_Other = ifelse(
    se_multi >= 2 & (kbi_p_c_spec_serv___1 == 0 & kbi_p_c_spec_serv___2 == 0 &
                       kbi_p_c_spec_serv___3 == 0 & kbi_p_c_spec_serv___4 == 0 &
                       kbi_p_c_spec_serv___5 == 0 & kbi_p_c_spec_serv___6 == 0 &
                       kbi_p_c_spec_serv___7 == 0) & kbi_p_c_spec_serv___8 == 1 & 
      kbi_p_c_spec_serv___9 == 1 & kbi_p_c_spec_serv___10 == 0, 1, 0)) %>% 
  mutate(ELS_Gifted_Other = ifelse(
    se_multi >= 2 & (kbi_p_c_spec_serv___1 == 1 | kbi_p_c_spec_serv___2 == 1 |
                       kbi_p_c_spec_serv___3 == 1 | kbi_p_c_spec_serv___4 == 1 |
                       kbi_p_c_spec_serv___5 == 1 | kbi_p_c_spec_serv___6 == 1 |
                       kbi_p_c_spec_serv___7 == 1) & kbi_p_c_spec_serv___8 == 1 & 
      kbi_p_c_spec_serv___9 == 1 & kbi_p_c_spec_serv___10 == 0, 1, 0)) %>% 
  select(ELS_Gifted, ELS_Other, Gifted_Other, ELS_Gifted_Other)

mh_p_ksads_bg_2_print <- mh_p_ksads_bg_2 %>% 
  Desc(., plotit = FALSE) 
capture.output(
  mh_p_ksads_bg_2_print, 
  file = 'output/data_management/supplement/mh_ksads_bg_2.txt')
rm(mh_p_ksads_bg_2, mh_p_ksads_bg_2_print)
#--- Supplementary Materials ---#

# merge w/full dataset 
mh_p_ksads_bg <- mh_p_ksads_bg %>%  
  rename(det_susp = kbi_p_c_det_susp) %>% 
  select(src_subject_id, kbi_p_grades_in_school, det_susp, se_services) %>% 
  mutate(se_services = factor(se_services))

data.2 <- data.2 %>% 
  left_join(mh_p_ksads_bg, by = 'src_subject_id') 

# create summary score for failing grades (D and F)
data.2 <- data.2 %>% 
  mutate(kbi_p_grades_in_school = case_when(
    kbi_p_grades_in_school == 'Grade_A' ~ 'Grade_A',
    kbi_p_grades_in_school == 'Grade_B' ~ 'Grade_B',
    kbi_p_grades_in_school == 'Grade_C' ~ 'Grade_C',
    kbi_p_grades_in_school == 'Grade_D' ~ 'Grade_Fail',
    kbi_p_grades_in_school == 'Grade_F' ~ 'Grade_Fail')) %>% 
  mutate(kbi_p_grades_in_school = as.factor(kbi_p_grades_in_school))
class(data.2$kbi_p_grades_in_school) # factor

rm(mh_p_ksads_bg)

#------------------------------------------------------------------------------#
#                                  SRPF                                        #
#------------------------------------------------------------------------------#

ce_y_srpf <- read_csv('data/ce_y_srpf.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, srpf_y_ss_ses, srpf_y_ss_iiss, srpf_y_ss_dfs) 

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ce_y_srpf, by = 'src_subject_id') 

rm(ce_y_srpf)

# Independent Variables: Biospecimens  -----------------------------------------

#------------------------------------------------------------------------------#
#                   Hormone Saliva Salimetric Scores                           #
#------------------------------------------------------------------------------#

ph_y_sal_horm <- read_csv('data/ph_y_sal_horm.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, hormone_scr_ert_mean, hormone_scr_dhea_mean)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(ph_y_sal_horm, by = 'src_subject_id') 

rm(ph_y_sal_horm)

# Independent Variables: Neurocognitive Factors  -------------------------------

#------------------------------------------------------------------------------#
#                                   RAVLT                                      #
#------------------------------------------------------------------------------#

nc_y_ravlt <- read_csv('data/nc_y_ravlt.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, pea_ravlt_sd_trial_i_tc, 
         pea_ravlt_sd_trial_v_tc,  pea_ravlt_sd_trial_vi_tc,
         pea_ravlt_ld_trial_vii_tc) 

#--- Supplementary Materials ---#

table(nc_y_ravlt$pea_ravlt_sd_trial_i_tc, useNA = 'ifany') # (NA) = 125
table(nc_y_ravlt$pea_ravlt_sd_trial_v_tc, useNA = 'ifany') # (NA) = 105

#--- Supplementary Materials ---#

nc_y_ravlt <- nc_y_ravlt %>% 
  mutate(pea_ravlt_learn = 
           pea_ravlt_sd_trial_v_tc - pea_ravlt_sd_trial_i_tc) %>%  
  select(src_subject_id, pea_ravlt_learn, pea_ravlt_sd_trial_vi_tc, 
         pea_ravlt_ld_trial_vii_tc)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(nc_y_ravlt, by = 'src_subject_id') 

rm(nc_y_ravlt)

#------------------------------------------------------------------------------#
#                                  WISC-V                                      #
#------------------------------------------------------------------------------#

nc_y_wisc <- read_csv('data/nc_y_wisc.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, pea_wiscv_tss)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(nc_y_wisc, by = 'src_subject_id') 

rm(nc_y_wisc)

#------------------------------------------------------------------------------#
#                         Cash Choice Task                                     #
#------------------------------------------------------------------------------#

nc_y_cct <- read_csv('data/nc_y_cct.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, cash_choice_task)

#--- Supplementary Materials ---#

table(nc_y_cct$cash_choice_task, useNA = 'ifany') 
# (3) = 108 Don't Know; (NA) = 8

#--- Supplementary Materials ---#

# recode values of (3) Don't Know to NA
nc_y_cct <- nc_y_cct %>% 
  mutate(
    across(.cols = cash_choice_task,  
           .fns = ~replace(.x, .x == 3, NA))) %>% 
  mutate(
    cct = factor(cash_choice_task, levels = c(1,2), 
                 labels = c('Immediate', 'Delayed'))) %>% 
  select(src_subject_id, cct)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(nc_y_cct, by = 'src_subject_id')

rm(nc_y_cct)

#------------------------------------------------------------------------------#
#                           Little Man Task                                    #
#------------------------------------------------------------------------------#

nc_y_lmt <- read_csv('data/nc_y_lmt.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, lmt_scr_num_correct, lmt_scr_num_wrong, 
         lmt_scr_perc_correct, lmt_scr_rt_correct, lmt_scr_efficiency)

#--- Supplementary Materials ---#

table(nc_y_lmt$lmt_scr_num_correct, useNA = 'ifany') # NA = 193
table(nc_y_lmt$lmt_scr_num_wrong, useNA = 'ifany') # NA = 193
summary(nc_y_lmt$lmt_scr_efficiency) # NA = 195

#--- Supplementary Materials ---#

nc_y_lmt <- nc_y_lmt %>% 
  mutate(
    lmt_acc = (lmt_scr_num_correct)/(lmt_scr_num_correct + lmt_scr_num_wrong),
    lmt_acc = ifelse(
      (lmt_scr_num_correct == 0 & lmt_scr_num_wrong == 0), NA, lmt_acc)) %>% 
  select(src_subject_id, lmt_acc, lmt_scr_rt_correct, lmt_scr_efficiency)

# merge w/full dataset  
data.2 <- data.2 %>% 
  left_join(nc_y_lmt, by = 'src_subject_id')

rm(nc_y_lmt)

#------------------------------------------------------------------------------#
#                                NIH ToolBox                                   #
#------------------------------------------------------------------------------#

nc_y_nihtb <- read_csv('data/nc_y_nihtb.csv') %>% 
  subset (eventname == 'baseline_year_1_arm_1') %>% 
  right_join(data.2_ID, by = 'src_subject_id') %>% 
  select(src_subject_id, nihtbx_picvocab_agecorrected, 
         nihtbx_flanker_agecorrected, nihtbx_list_agecorrected,
         nihtbx_cardsort_agecorrected, nihtbx_pattern_agecorrected, 
         nihtbx_picture_agecorrected, nihtbx_reading_agecorrected)

# merge w/full dataset 
data.2 <- data.2 %>% 
  left_join(nc_y_nihtb, by = 'src_subject_id')

rm(nc_y_nihtb)


# Sample Characteristics: Numeric Variables ------------------------------------

# drop objects no longer needed
rm(data.1, data.2_ID)

#------------------------------------------------------------------------------#
#                      create table & domain names 
#------------------------------------------------------------------------------#

# create table and domain names
temp <- data.2 %>% 
  select_if(is.factor)
names(temp)
rm(temp)

table_names_numeric <- data.2 %>% 
  select(-src_subject_id, -DV, -race_4l, -eth_hisp, -income, -religion, -p_edu,
         -sex_2l, -rec_bin, -exp_sub, -tbi_injury, -kbi_p_grades_in_school, 
         -det_susp, -se_services, -cct) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  select(variable) %>%
  mutate(
    table_name = 
      case_when(
        # sMRI 
        variable == 'smri_area_cdk_banksstslh' |
          variable == 'smri_sulc_cdk_banksstslh' |
          variable == 'smri_thick_cdk_banksstslh' |
          variable == 'smri_vol_cdk_banksstslh' ~ 'Banks of Superior Temporal Sulcus (L)',
        variable == 'smri_area_cdk_banksstsrh' |
          variable == 'smri_sulc_cdk_banksstsrh' |
          variable == 'smri_thick_cdk_banksstsrh' |
          variable == 'smri_vol_cdk_banksstsrh' ~ 'Banks of Superior Temporal Sulcus (R)',
        
        variable == 'smri_area_cdk_cdacatelh' |
          variable == 'smri_sulc_cdk_cdacatelh' |
          variable == 'smri_thick_cdk_cdacatelh' |
          variable == 'smri_vol_cdk_cdacatelh' ~ 'Caudal Anterior Cingulate Cortex (L)', 
        variable == 'smri_area_cdk_cdacaterh' |
          variable == 'smri_sulc_cdk_cdacaterh' |
          variable == 'smri_thick_cdk_cdacaterh' |
          variable == 'smri_vol_cdk_cdacaterh' ~ 'Caudal Anterior Cingulate Cortex (R)',
        
        variable == 'smri_area_cdk_cdmdfrlh' |
          variable == 'smri_sulc_cdk_cdmdfrlh' |
          variable == 'smri_thick_cdk_cdmdfrlh' |
          variable == 'smri_vol_cdk_cdmdfrlh' ~ 'Caudal Middle Frontal Lobe (L)',
        variable == 'smri_area_cdk_cdmdfrrh' |
          variable == 'smri_sulc_cdk_cdmdfrrh' |
          variable == 'smri_thick_cdk_cdmdfrrh' |
          variable == 'smri_vol_cdk_cdmdfrrh' ~ 'Caudal Middle Frontal Lobe (R)',
        
        variable == 'smri_area_cdk_cuneuslh'|
          variable == 'smri_sulc_cdk_cuneuslh' |
          variable == 'smri_thick_cdk_cuneuslh' |
          variable == 'smri_vol_cdk_cuneuslh' ~ 'Cuneus (L)',
        variable == 'smri_area_cdk_cuneusrh' |
          variable == 'smri_sulc_cdk_cuneusrh' |
          variable == 'smri_thick_cdk_cuneusrh' |
          variable == 'smri_vol_cdk_cuneusrh'~ 'Cuneus (R)',
        
        variable == 'smri_area_cdk_ehinallh' |
          variable == 'smri_sulc_cdk_ehinallh' |
          variable == 'smri_thick_cdk_ehinallh' |
          variable == 'smri_vol_cdk_ehinallh' ~ 'Entorhinal (L)',
        variable == 'smri_area_cdk_ehinalrh' |
          variable == 'smri_sulc_cdk_ehinalrh' |
          variable == 'smri_thick_cdk_ehinalrh' |
          variable == 'smri_vol_cdk_ehinalrh' ~ 'Entorhinal (R)',
        
        variable == 'smri_area_cdk_fusiformlh' |
          variable == 'smri_sulc_cdk_fusiformlh' |
          variable == 'smri_thick_cdk_fusiformlh' |
          variable == 'smri_vol_cdk_fusiformlh' ~ 'Fusiform (L)',
        variable == 'smri_area_cdk_fusiformrh' |
          variable == 'smri_sulc_cdk_fusiformrh' |
          variable == 'smri_thick_cdk_fusiformrh' |
          variable == 'smri_vol_cdk_fusiformrh' ~ 'Fusiform (R)',
        
        variable == 'smri_area_cdk_ifpllh' |
          variable == 'smri_sulc_cdk_ifpllh' |
          variable == 'smri_thick_cdk_ifpllh' |
          variable == 'smri_vol_cdk_ifpllh' ~ 'Inferior Parietal Lobe (L)',
        variable == 'smri_area_cdk_ifplrh' |
          variable == 'smri_sulc_cdk_ifplrh' |
          variable == 'smri_thick_cdk_ifplrh' |
          variable == 'smri_vol_cdk_ifplrh' ~ 'Inferior Parietal Lobe (R)',
        
        variable == 'smri_area_cdk_iftmlh' |
          variable == 'smri_sulc_cdk_iftmlh' |
          variable == 'smri_thick_cdk_iftmlh' |
          variable == 'smri_vol_cdk_iftmlh' ~ 'Inferior Temporal Lobe (L)',
        variable == 'smri_area_cdk_iftmrh' |
          variable == 'smri_sulc_cdk_iftmrh' |
          variable == 'smri_thick_cdk_iftmrh' |
          variable == 'smri_vol_cdk_iftmrh' ~ 'Inferior Temporal Lobe (R)',
        
        variable == 'smri_area_cdk_ihcatelh' |
          variable == 'smri_sulc_cdk_ihcatelh' |
          variable == 'smri_thick_cdk_ihcatelh' |
          variable == 'smri_vol_cdk_ihcatelh' ~ 'Isthmus Cingulate Cortex (L)',
        variable == 'smri_area_cdk_ihcaterh' |
          variable == 'smri_sulc_cdk_ihcaterh' |
          variable == 'smri_thick_cdk_ihcaterh' |
          variable == 'smri_vol_cdk_ihcaterh' ~ 'Isthmus Cingulate Cortex (R)',
        
        variable == 'smri_area_cdk_locclh' |
          variable == 'smri_sulc_cdk_locclh' |
          variable == 'smri_thick_cdk_locclh' |
          variable == 'smri_vol_cdk_locclh' ~ 'Lateral Occipital Lobe (L)',
        variable == 'smri_area_cdk_loccrh' |
          variable == 'smri_sulc_cdk_loccrh' |
          variable == 'smri_thick_cdk_loccrh' |
          variable == 'smri_vol_cdk_loccrh' ~ 'Lateral Occipital Lobe (R)',
        
        variable == 'smri_area_cdk_lobfrlh' |
          variable == 'smri_sulc_cdk_lobfrlh' |
          variable == 'smri_thick_cdk_lobfrlh' |
          variable == 'smri_vol_cdk_lobfrlh' ~ 'Lateral Orbito-Frontal Lobe (L)',
        variable == 'smri_area_cdk_lobfrrh' |
          variable == 'smri_sulc_cdk_lobfrrh' |
          variable == 'smri_thick_cdk_lobfrrh' |
          variable == 'smri_vol_cdk_lobfrrh' ~ 'Lateral Orbito-Frontal Lobe (R)',
        
        variable == 'smri_area_cdk_linguallh' |
          variable == 'smri_sulc_cdk_linguallh' |
          variable == 'smri_thick_cdk_linguallh' |
          variable == 'smri_vol_cdk_linguallh' ~ 'Lingual Lobe (L)',
        variable == 'smri_area_cdk_lingualrh' |
          variable == 'smri_sulc_cdk_lingualrh' |
          variable == 'smri_thick_cdk_lingualrh' |
          variable == 'smri_vol_cdk_lingualrh' ~ 'Lingual Lobe (R)',
        
        variable == 'smri_area_cdk_mobfrlh' |
          variable == 'smri_sulc_cdk_mobfrlh' |
          variable == 'smri_thick_cdk_mobfrlh' |
          variable == 'smri_vol_cdk_mobfrlh' ~ 'Medial Orbito-Frontal Lobe (L)',
        variable == 'smri_area_cdk_mobfrrh' |
          variable == 'smri_sulc_cdk_mobfrrh' |
          variable == 'smri_thick_cdk_mobfrrh' |
          variable == 'smri_vol_cdk_mobfrrh' ~ 'Medial Orbito-Frontal Lobe (R)',
        
        variable == 'smri_area_cdk_mdtmlh' |
          variable == 'smri_sulc_cdk_mdtmlh' |
          variable == 'smri_thick_cdk_mdtmlh' |
          variable == 'smri_vol_cdk_mdtmlh' ~ 'Middle Temporal Lobe (L)',
        variable == 'smri_area_cdk_mdtmrh' |
          variable == 'smri_sulc_cdk_mdtmrh' |
          variable == 'smri_thick_cdk_mdtmrh' |
          variable == 'smri_vol_cdk_mdtmrh' ~ 'Middle Temporal Lobe (R)',
        
        variable == 'smri_area_cdk_parahpallh' |
          variable == 'smri_sulc_cdk_parahpallh' |
          variable == 'smri_thick_cdk_parahpallh' |
          variable == 'smri_vol_cdk_parahpallh' ~ 'Parahippocampal Region (L)',
        variable == 'smri_area_cdk_parahpalrh' |
          variable == 'smri_sulc_cdk_parahpalrh' |
          variable == 'smri_thick_cdk_parahpalrh' |
          variable == 'smri_vol_cdk_parahpalrh' ~ 'Parahippocampal Region (R)',
        
        variable == 'smri_area_cdk_paracnlh' |
          variable == 'smri_sulc_cdk_paracnlh' |
          variable == 'smri_thick_cdk_paracnlh' |
          variable == 'smri_vol_cdk_paracnlh' ~ 'Paracentral Region (L)',
        variable == 'smri_area_cdk_paracnrh' |
          variable == 'smri_sulc_cdk_paracnrh' |
          variable == 'smri_thick_cdk_paracnrh' |
          variable == 'smri_vol_cdk_paracnrh' ~ 'Paracentral Region (R)',
        
        variable == 'smri_area_cdk_parsopclh' |
          variable == 'smri_sulc_cdk_parsopclh' |
          variable == 'smri_thick_cdk_parsopclh' |
          variable == 'smri_vol_cdk_parsopclh' ~ 'Pars Opercularis (L)',
        variable == 'smri_area_cdk_parsopcrh' |
          variable == 'smri_sulc_cdk_parsopcrh' |
          variable == 'smri_thick_cdk_parsopcrh' |
          variable == 'smri_vol_cdk_parsopcrh' ~ 'Pars Opercularis (R)',
        
        variable == 'smri_area_cdk_parsobislh' |
          variable == 'smri_sulc_cdk_parsobislh' |
          variable == 'smri_thick_cdk_parsobislh' |
          variable == 'smri_vol_cdk_parsobislh' ~ 'Pars Orbitalis (L)',
        variable == 'smri_area_cdk_parsobisrh' |
          variable == 'smri_sulc_cdk_parsobisrh' |
          variable == 'smri_thick_cdk_parsobisrh' |
          variable == 'smri_vol_cdk_parsobisrh' ~ 'Pars Orbitalis (R)',
        
        variable == 'smri_area_cdk_parstgrislh' |
          variable == 'smri_sulc_cdk_parstgrislh' |
          variable == 'smri_thick_cdk_parstgrislh' |
          variable == 'smri_vol_cdk_parstgrislh' ~ 'Pars Triangularis (L)',
        variable == 'smri_area_cdk_parstgrisrh' |
          variable == 'smri_sulc_cdk_parstgrisrh' |
          variable == 'smri_thick_cdk_parstgrisrh' |
          variable == 'smri_vol_cdk_parstgrisrh' ~ 'Pars Triangularis (R)',
        
        variable == 'smri_area_cdk_pericclh' |
          variable == 'smri_sulc_cdk_pericclh' |
          variable == 'smri_thick_cdk_pericclh' |
          variable == 'smri_vol_cdk_pericclh' ~ 'Pericalcarine (L)',
        variable == 'smri_area_cdk_periccrh' |
          variable == 'smri_sulc_cdk_periccrh' |
          variable == 'smri_thick_cdk_periccrh' |
          variable == 'smri_vol_cdk_periccrh' ~ 'Pericalcarine (R)',
        
        variable == 'smri_area_cdk_postcnlh' |
          variable == 'smri_sulc_cdk_postcnlh' |
          variable == 'smri_thick_cdk_postcnlh' |
          variable == 'smri_vol_cdk_postcnlh' ~ 'Postcentral Gyrus (L)',
        variable == 'smri_area_cdk_postcnrh' |
          variable == 'smri_sulc_cdk_postcnrh' |
          variable == 'smri_thick_cdk_postcnrh' |
          variable == 'smri_vol_cdk_postcnrh' ~ 'Postcentral Gyrus (R)',
        
        variable == 'smri_area_cdk_ptcatelh' |
          variable == 'smri_sulc_cdk_ptcatelh' |
          variable == 'smri_thick_cdk_ptcatelh' |
          variable == 'smri_vol_cdk_ptcatelh' ~ 'Posterior Cingulate Cortex (L)',
        variable == 'smri_area_cdk_ptcaterh' |
          variable == 'smri_sulc_cdk_ptcaterh' |
          variable == 'smri_thick_cdk_ptcaterh' |
          variable == 'smri_vol_cdk_ptcaterh' ~ 'Posterior Cingulate Cortex (R)',
        
        variable == 'smri_area_cdk_precnlh' |
          variable == 'smri_sulc_cdk_precnlh' |
          variable == 'smri_thick_cdk_precnlh' |
          variable == 'smri_vol_cdk_precnlh' ~ 'Precentral Gyrus (L)',
        variable == 'smri_area_cdk_precnrh' |
          variable == 'smri_sulc_cdk_precnrh' |
          variable == 'smri_thick_cdk_precnrh' |
          variable == 'smri_vol_cdk_precnrh' ~ 'Precentral Gyrus (L)',
        
        variable == 'smri_area_cdk_pclh' |
          variable == 'smri_sulc_cdk_pclh' |
          variable == 'smri_thick_cdk_pclh' |
          variable == 'smri_vol_cdk_pclh' ~ 'Precuneus (L)',
        variable == 'smri_area_cdk_pcrh' |
          variable == 'smri_sulc_cdk_pcrh' |
          variable == 'smri_thick_cdk_pcrh' |
          variable == 'smri_vol_cdk_pcrh' ~ 'Precuneus (R)',
        
        variable == 'smri_area_cdk_rracatelh' |
          variable == 'smri_sulc_cdk_rracatelh' |
          variable == 'smri_thick_cdk_rracatelh' |
          variable == 'smri_vol_cdk_rracatelh' ~ 'Rostral Anterior Cingulate Cortex (L)',
        variable == 'smri_area_cdk_rracaterh' |
          variable == 'smri_sulc_cdk_rracaterh' |
          variable == 'smri_thick_cdk_rracaterh' |
          variable == 'smri_vol_cdk_rracaterh' ~ 'Rostral Anterior Cingulate Cortex (R)',
        
        variable == 'smri_area_cdk_rrmdfrlh' |
          variable == 'smri_sulc_cdk_rrmdfrlh' |
          variable == 'smri_thick_cdk_rrmdfrlh' |
          variable == 'smri_vol_cdk_rrmdfrlh' ~ 'Rostral Middle Frontal Lobe (L)',
        variable == 'smri_area_cdk_rrmdfrrh' |
          variable == 'smri_sulc_cdk_rrmdfrrh' |
          variable == 'smri_thick_cdk_rrmdfrrh' |
          variable == 'smri_vol_cdk_rrmdfrrh' ~ 'Rostral Middle Frontal Lobe (R)',
        
        variable == 'smri_area_cdk_sufrlh' |
          variable == 'smri_sulc_cdk_sufrlh' |
          variable == 'smri_thick_cdk_sufrlh' |
          variable == 'smri_vol_cdk_sufrlh' ~ 'Superior Frontal Lobe (L)',
        variable == 'smri_area_cdk_sufrrh' |
          variable == 'smri_sulc_cdk_sufrrh' |
          variable == 'smri_thick_cdk_sufrrh' |
          variable == 'smri_vol_cdk_sufrrh' ~ 'Superior Frontal Lobe (R)',
        
        variable == 'smri_area_cdk_supllh' |
          variable == 'smri_sulc_cdk_supllh' |
          variable == 'smri_thick_cdk_supllh' |
          variable == 'smri_vol_cdk_supllh' ~ 'Superior Parietal Lobe (L)',
        variable == 'smri_area_cdk_suplrh' |
          variable == 'smri_sulc_cdk_suplrh' |
          variable == 'smri_thick_cdk_suplrh' |
          variable == 'smri_vol_cdk_suplrh' ~ 'Superior Parietal Lobe (R)',
        
        variable == 'smri_area_cdk_sutmlh' |
          variable == 'smri_sulc_cdk_sutmlh' |
          variable == 'smri_thick_cdk_sutmlh' |
          variable == 'smri_vol_cdk_sutmlh' ~ 'Superior Temporal Lobe (L)',
        variable == 'smri_area_cdk_sutmrh' |
          variable == 'smri_sulc_cdk_sutmrh' |
          variable == 'smri_thick_cdk_sutmrh' |
          variable == 'smri_vol_cdk_sutmrh' ~ 'Superior Temporal Lobe (R)',
        
        variable == 'smri_area_cdk_smlh' |
          variable == 'smri_sulc_cdk_smlh' |
          variable == 'smri_thick_cdk_smlh' |
          variable == 'smri_vol_cdk_smlh' ~ 'Supramarginal (L)',
        variable == 'smri_area_cdk_smrh' |
          variable == 'smri_sulc_cdk_smrh' |
          variable == 'smri_thick_cdk_smrh' |
          variable == 'smri_vol_cdk_smrh' ~ 'Supramarginal (R)',
        
        variable == 'smri_area_cdk_frpolelh' |
          variable == 'smri_sulc_cdk_frpolelh' |
          variable == 'smri_thick_cdk_frpolelh' |
          variable == 'smri_vol_cdk_frpolelh' ~ 'Frontal Pole (L)',
        variable == 'smri_area_cdk_frpolerh' |
          variable == 'smri_sulc_cdk_frpolerh' |
          variable == 'smri_thick_cdk_frpolerh' |
          variable == 'smri_vol_cdk_frpolerh' ~ 'Frontal Pole (R)',
        
        variable == 'smri_area_cdk_tmpolelh' |
          variable == 'smri_sulc_cdk_tmpolelh' |
          variable == 'smri_thick_cdk_tmpolelh' |
          variable == 'smri_vol_cdk_tmpolelh' ~ 'Temporal Pole (L)',
        variable == 'smri_area_cdk_tmpolerh' |
          variable == 'smri_sulc_cdk_tmpolerh' |
          variable == 'smri_thick_cdk_tmpolerh' |
          variable == 'smri_vol_cdk_tmpolerh' ~ 'Temporal Pole (R)',
        
        variable == 'smri_area_cdk_trvtmlh' |
          variable == 'smri_sulc_cdk_trvtmlh' |
          variable == 'smri_thick_cdk_trvtmlh' |
          variable == 'smri_vol_cdk_trvtmlh' ~ 'Transversetemporal (L)',
        variable == 'smri_area_cdk_trvtmrh' |
          variable == 'smri_sulc_cdk_trvtmrh' |
          variable == 'smri_thick_cdk_trvtmrh' |
          variable == 'smri_vol_cdk_trvtmrh' ~ 'Transversetemporal (R)',
        
        variable == 'smri_area_cdk_insulalh' |
          variable == 'smri_sulc_cdk_insulalh' |
          variable == 'smri_thick_cdk_insulalh' |
          variable == 'smri_vol_cdk_insulalh' ~ 'Insula (L)',
        variable == 'smri_area_cdk_insularh' |
          variable == 'smri_sulc_cdk_insularh' |
          variable == 'smri_thick_cdk_insularh' |
          variable == 'smri_vol_cdk_insularh' ~ 'Insula (R)',
        
        # DTI
        variable == 'dmri_dtifa_fiberat_fxlh' ~ 'Fornix (L)',
        variable == 'dmri_dtifa_fiberat_fxrh' ~ 'Fornix (R)',
        
        variable == 'dmri_dtifa_fiberat_cgclh' ~ 'Cingulate Cingulum (L)',
        variable == 'dmri_dtifa_fiberat_cgcrh' ~ 'Cingulate Cingulum (R)',
        
        variable == 'dmri_dtifa_fiberat_cghlh' ~ 'Parahippocampal Cingulum (L)',
        variable == 'dmri_dtifa_fiberat_cghrh' ~ 'Parahippocampal Cingulum (R)',
        
        variable == 'dmri_dtifa_fiberat_cstlh' ~ 'Corticospinal/Pyramidal (L)',
        variable == 'dmri_dtifa_fiberat_cstrh' ~ 'Corticospinal/Pyramidal (R)',
        
        variable == 'dmri_dtifa_fiberat_atrlh' ~ 'Anteroir Thalamic Radiations (L)',
        variable == 'dmri_dtifa_fiberat_atrrh' ~ 'Anteroir Thalamic Radiations (R)',
        
        variable == 'dmri_dtifa_fiberat_unclh' ~ 'Uncinate Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_uncrh' ~ 'Uncinate Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_ilflh' ~ 'Inferior Longitudinal Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_ilfrh' ~ 'Inferior Longitudinal Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_ifolh' ~ 'Inferior-Fronto-Occipital Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_iforh' ~ 'Inferior-Fronto-Occipital Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_slflh' ~ 'Superior Longitudinal Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_slfrh' ~ 'Superior Longitudinal Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_tslflh' ~ 'Temporal Superior Longitudinal Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_tslfrh' ~ 'Temporal Superior Longitudinal Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_pslflh' ~ 'Parietal Superior Longitudinal Fasiculus (L)',
        variable == 'dmri_dtifa_fiberat_pslfrh' ~ 'Parietal Superior Longitudinal Fasiculus (R)',
        
        variable == 'dmri_dtifa_fiberat_scslh' ~ 'Superior Corticostriate (L)',
        variable == 'dmri_dtifa_fiberat_scsrh' ~ 'Superior Corticostriate (R)',
        
        variable == 'dmri_dtifa_fiberat_sifclh' ~ 'Striatal Inferior Frontal Cortex (L)',
        variable == 'dmri_dtifa_fiberat_sifcrh' ~ 'Striatal Inferior Frontal Cortex (R)',
        
        variable == 'dmri_dtifa_fiberat_ifsfclh' ~ 'Inferior Frontal Superior Frontal Cortex (L)',
        variable == 'dmri_dtifa_fiberat_ifsfcrh' ~ 'Inferior Frontal Superior Frontal Cortex (R)',
        
        variable == 'dmri_dtifa_fiberat_fmaj' ~ 'Forceps (major)',
        variable == 'dmri_dtifa_fiberat_fmin' ~ 'Forceps (minor)',
        variable == 'dmri_dtifa_fiberat_cc' ~ 'Corpus Collosum',
        
        # Self and Peer Involvement with Substance Use 
        variable == 'peer_alc' ~ 'Peer Substance Use - Alcohol',
        variable == 'peer_tob' ~ 'Peer Substance Use - Nicotine',
        variable == 'peer_cb' ~ 'Peer Substance Use - Cannabis',
        variable == 'peer_other' ~ 'Peer Substance Use - Other Substance Use',
        variable == 'peer_prob' ~ 'Peer Substance Use - Problems with Substance Use',
        variable == 'path_alc' ~ 'Intent to Use - Alcohol',
        variable == 'path_tob' ~ 'Intent to Use - Nicotine',
        variable == 'path_cb' ~ 'Intent to Use - Cannabis',
        
        # Parenting Behaviors
        variable == 'crpf' ~ 'CRPF',
        variable == 'par_rules' ~ 'Parent Rules',
        variable == 'pmq_y_ss_mean' ~ 'PMQ - Parental Monitoring',
        variable == 'fes_y_ss_fc' ~ 'FES - Conflict Subscale',
        variable == 'crpbi_y_ss_parent' ~ 'CRPBI - Acceptance Subscale',
        
        # Demographics
        variable == 'age_baseline' ~ 'Age at Baseline',
        
        # Mental Health
        variable == 'cbcl_scr_syn_anxdep_r' ~ 'CBCL - Anxiety / Depression',
        variable == 'cbcl_scr_syn_withdep_r' ~ 'CBCL - Withdraw / Depression',
        variable == 'cbcl_scr_syn_somatic_r' ~ 'CBCL - Somatic Symptoms',
        variable == 'cbcl_scr_syn_social_r' ~ 'CBCL - Social Problems',
        variable == 'cbcl_scr_syn_thought_r' ~ 'CBCL - Thought Problems',
        variable == 'cbcl_scr_syn_attention_r' ~ 'CBCL - Attention Problems',
        variable == 'cbcl_scr_syn_rulebreak_r' ~ 'CBCL - Rule-breaking Behavior',
        variable == 'cbcl_scr_syn_aggressive_r' ~ 'CBCL - Aggressive Behavior',
        variable == 'cbcl_scr_syn_internal_r' ~ 'CBCL - Internalizing Disorders',
        variable == 'cbcl_scr_syn_external_r' ~ 'CBCL - Externalizing Disorders',
        variable == 'cbcl_scr_syn_totprob_r' ~ 'CBCL - Total Problems',
        variable == 'mh_density' ~ 'Family History of Substance Use',
        variable == 'pps_y_ss_number' ~ 'PPS - Positive Symptoms Endorsed',
        variable == 'upps_y_ss_negative_urgency' ~ 'UPPS-P - Negative Urgency',
        variable == 'upps_y_ss_lack_of_perseverance' ~ 'UPPS-P - Lack of Perseverance',
        variable == 'upps_y_ss_lack_of_planning' ~ 'UPPS-P - Lack of Planning',
        variable == 'upps_y_ss_sensation_seeking' ~ 'UPPS-P - Sensation Seeking',
        variable == 'upps_y_ss_positive_urgency' ~ 'UPPS-P - Positive Urgency',
        variable == 'bis_y_ss_bis_sum' ~ 'BIS/BAS - BIS Sum',
        variable == 'bis_y_ss_bas_drive' ~ 'BIS/BAS - Drive',
        variable == 'bis_y_ss_bas_fs' ~ 'BIS/BAS - Fun Seeking',
        variable == 'bis_y_ss_bas_rr' ~ 'BIS/BAS - Reward Responsiveness',
        
        # Physical Health
        variable == 'rec_con' ~ 'Recreational Activities (continuous)',
        variable == 'screentime' ~ 'Screentime',
        variable == 'act1' ~ 'Physical Activity - Days of 60-min.',
        variable == 'act2' ~ 'Physical Activity - Days of Strength Building',
        variable == 'act5' ~ 'Physical Activity - Days of P.E. Course',
        variable == 'exp_caf_rec' ~ 'Prenatal Exposure - Caffeine',
        variable == 'sds_p_ss_dims' ~ 'SDS - Disorders of Initiating and Maintaining Sleep',
        variable == 'sds_p_ss_sbd' ~ 'SDS - Sleep Breathing disorders',
        variable == 'sds_p_ss_da' ~ 'SDS - Disorder of Arousal',
        variable == 'sds_p_ss_swtd' ~ 'SDS - Sleep-Wake transition Disorders',
        variable == 'sds_p_ss_does' ~ 'SDS - Disorders of Excessive Somnolence',
        variable == 'sds_p_ss_shy' ~ 'SDS - Sleep Hyperhydrosis',
        variable == 'pds' ~ 'PDS - Puberty Development',
        
        # Culture & Environment
        variable == 'accult_q2_y' ~ 'Bilingual Status',
        variable == 'neighborhood_crime_y' ~ 'Neighborhood Safety',
        variable == 'srpf_y_ss_ses' ~ 'SRPF - School Environment Subscale',
        variable == 'srpf_y_ss_iiss' ~ 'SRPF - School Involvement Subscale',
        variable == 'srpf_y_ss_dfs' ~ 'SRPF - School Disengagement Subscale',
        
        # Biospecimens
        variable == 'hormone_scr_ert_mean' ~ 'Hormones - Testosterone',
        variable == 'hormone_scr_dhea_mean' ~ 'Hormones - DHEA',
        
        # Neurocog
        variable == 'pea_ravlt_learn' ~ 'RAVLT - Learning Score',
        variable == 'pea_ravlt_sd_trial_vi_tc' ~ 'RAVLT - Short Delay',
        variable == 'pea_ravlt_ld_trial_vii_tc' ~ 'RAVLT - Long Delay',
        variable == 'pea_wiscv_tss' ~ 'WISC-V - Matrix Reasoning',
        variable == 'lmt_acc' ~ 'LMT - Accuracy',
        variable == 'lmt_scr_rt_correct' ~ 'LMT - Reaction Time for Correct Trials',
        variable == 'lmt_scr_efficiency' ~ 'LMT - Efficiency',
        variable == 'nihtbx_picvocab_agecorrected' ~ 'NIH Toolbox - Picture Vocabulary Test',
        variable == 'nihtbx_flanker_agecorrected' ~ 'NIH Toolbox - Flanker Inhibitory Control and Attention Test',
        variable == 'nihtbx_list_agecorrected' ~ 'NIH Toolbox - List Sorting Working Memory Test',
        variable == 'nihtbx_cardsort_agecorrected' ~ 'NIH Toolbox - Dimensional Change Card Sort Test',
        variable == 'nihtbx_pattern_agecorrected' ~ 'NIH Toolbox - Pattern Comparison Processing Speed Test',
        variable == 'nihtbx_picture_agecorrected' ~ 'NIH Toolbox - Picture Sequence Memory Test',
        variable == 'nihtbx_reading_agecorrected' ~ 'NIH Toolbox - Oral Reading Recognition Test')) 

domain_names_numeric <- data.2 %>% 
  select(-src_subject_id, -DV, -race_4l, -eth_hisp, -income, -religion, -p_edu,
         -sex_2l, -rec_bin, -exp_sub, -tbi_injury, -kbi_p_grades_in_school, 
         -det_susp, -se_services, -cct) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = "variable") %>% 
  select(variable) %>% 
  mutate(
    domain_name = case_when(
      # Imaging 
      substr(variable, 1, 6) == 'smri_a' ~ 'Structural Magnetic Resonance Imaging (sMRI)',
      substr(variable, 1, 6) == 'smri_s' ~ 'Structural Magnetic Resonance Imaging (sMRI)',
      substr(variable, 1, 6) == 'smri_t' ~ 'Structural Magnetic Resonance Imaging (sMRI)',
      substr(variable, 1, 6) == 'smri_v' ~ 'Structural Magnetic Resonance Imaging (sMRI)',
      substr(variable, 1, 4) == 'dmri' ~ 'Diffusion Tensor Imaging (DTI)',
      
      # Self and Peer Involvement with Substance Use
      substr(variable, 1, 4) == 'peer' ~ 'Self and Peer Involvement with Substance Use',
      substr(variable, 1, 4) == 'path' ~ 'Self and Peer Involvement with Substance Use',
      
      # Parenting Behaviors
      variable == 'crpf' ~ 'Parenting Behaviors',
      variable == 'par_rules' ~ 'Parenting Behaviors',
      variable == 'pmq_y_ss_mean' ~ 'Parenting Behaviors',
      variable == 'fes_y_ss_fc' ~ 'Parenting Behaviors',
      variable == 'crpbi_y_ss_parent' ~ 'Parenting Behaviors',
      
      # Demographics
      variable == 'age_baseline' ~ 'Demographics',
      
      # Mental Health
      substr(variable, 1, 4) == 'cbcl' ~ 'Mental Health',
      variable == 'mh_density' ~ 'Mental Health',
      variable == 'pps_y_ss_number' ~ 'Mental Health',
      substr(variable, 1, 4) == 'upps' ~ 'Mental Health',
      substr(variable, 1, 3) == 'bis' ~ 'Mental Health',
      
      # Physical Health
      variable == 'rec_con' ~ 'Physical Health',
      variable == 'screentime' ~ 'Physical Health',
      variable == 'act1' ~ 'Physical Health',
      variable == 'act2' ~ 'Physical Health',
      variable == 'act5' ~ 'Physical Health',
      variable == 'exp_caf_rec' ~ 'Physical Health',
      substr(variable, 1, 3) == 'sds' ~ 'Physical Health',
      variable == 'pds' ~ 'Physical Health',
      
      # Culture & Environment
      variable == 'accult_q2_y' ~ 'Culture & Environment',
      variable == 'neighborhood_crime_y' ~ 'Culture & Environment',
      substr(variable, 1, 4) == 'srpf' ~ 'Culture & Environment',
      
      # Biospecimens
      substr(variable, 1, 4) == 'horm' ~ 'Hormones',
      
      # Neurocog
      substr(variable, 1, 3) == 'pea' ~ 'Neurocognitive Factors',
      substr(variable, 1, 3) == 'lmt' ~ 'Neurocognitive Factors',
      substr(variable, 1, 3) == 'nih' ~ 'Neurocognitive Factors'))

table_names_numeric <- table_names_numeric %>% 
  full_join(domain_names_numeric, by = 'variable') 
rm(domain_names_numeric)

#------------------------------------------------------------------------------#
#                    descriptives split by DV
#------------------------------------------------------------------------------#

# split by DV
demos_DV0 <- data.2 %>% 
  subset(DV == 0) %>% 
  select(-src_subject_id, -DV)

demos_DV1 <- data.2 %>% 
  subset(DV == 1) %>% 
  select(-src_subject_id, -DV)

# summary stats
obs_num_DV0 <- get_summary_stats(demos_DV0, type = "common") %>% 
  select(variable, n, mean, sd, min, max) %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable) %>% 
  rename(
    n_0 = n,
    mean_0 = mean,
    sd_0 = sd,
    min_0 = min,
    max_0 = max)

obs_num_DV1 <- get_summary_stats(demos_DV1, type = "common") %>% 
  select(variable, n, mean, sd, min, max) %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable) %>% 
  rename(
    n_1 = n,
    mean_1 = mean,
    sd_1 = sd,
    min_1 = min,
    max_1 = max)

# merge DV0 and DV1 summary stats
obs_num_DV <- obs_num_DV0 %>% 
  full_join(by = c('domain_name', 'table_name', 'variable'), obs_num_DV1)
names(obs_num_DV)

# t-test for difference between DV0 and DV1
temp <- data.2 %>% 
  mutate(DV_char = as.character(DV)) %>% 
  select(-DV)
table(temp$DV_char)
DV_char <- temp$DV_char

t.test_obs <- temp %>% 
  select_if(is.numeric) %>%
  map_df(~ broom::tidy(t.test(. ~ DV_char)), .id = 'var') %>% 
  select(var, statistic, p.value) %>% 
  rename(
    variable = var,
    t.statistic = statistic) %>% 
  mutate(
    across(
      .cols = c('t.statistic', 'p.value'),
      .fns = round, 3)) 

t.test_obs <- t.test_obs %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable, t.statistic, p.value)

obs_num_DV <- obs_num_DV %>% 
  full_join(by = c('domain_name', 'table_name', 'variable'), t.test_obs)

obs_num_DV <- obs_num_DV %>% 
  mutate(p.value = ifelse(p.value == 0.000, '<0.001', p.value))

# add headers
temp_su <- obs_num_DV %>% 
  subset(domain_name == 'Self and Peer Involvement with Substance Use') %>% 
  add_row(table_name = 'Self and Peer Involvement with Substance Use', 
          .before = 1) 

temp_pb <- obs_num_DV %>% 
  subset(domain_name == 'Parenting Behaviors') %>% 
  add_row(table_name = 'Parenting Behaviors', .before = 1) 

temp_mh <- obs_num_DV %>% 
  subset(domain_name == 'Mental Health') %>% 
  add_row(table_name = 'Mental Health', .before = 1) 

temp_ph <- obs_num_DV %>% 
  subset(domain_name == 'Physical Health') %>% 
  add_row(table_name = 'Physical Health', .before = 1) 

temp_ce <- obs_num_DV %>% 
  subset(domain_name == 'Culture & Environment') %>% 
  add_row(table_name = 'Culture & Environment', .before = 1) 

temp_hor <- obs_num_DV %>% 
  subset(domain_name == 'Hormones') %>% 
  add_row(table_name = 'Hormones', .before = 1) 

temp_nc <- obs_num_DV %>% 
  subset(domain_name == 'Neurocognitive Factors') %>% 
  add_row(table_name = 'Neurocognitive Factors', .before = 1) 

temp_smri_area <- obs_num_DV %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_area')) %>% 
  add_row(table_name = 'sMRI: Area', .before = 1) 

temp_smri_vol <- obs_num_DV %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_vol')) %>% 
  add_row(table_name = 'sMRI: Volume', .before = 1) 

temp_smri_sulc <- obs_num_DV %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_sulc')) %>% 
  add_row(table_name = 'sMRI: Sulcul Depth', .before = 1) 

temp_smri_thick <- obs_num_DV %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_thick')) %>% 
  add_row(table_name = 'sMRI: Cortical Thickness', .before = 1) 

temp_dti <- obs_num_DV %>% 
  subset(domain_name == 'Diffusion Tensor Imaging (DTI)') %>% 
  add_row(table_name = 'DTI: FA', .before = 1) 

obs_num_DV <- rbind(
  temp_su, temp_pb, temp_mh, temp_ph, temp_ce, temp_hor, temp_nc, 
  temp_smri_area, temp_smri_vol, temp_smri_sulc, temp_smri_thick, temp_dti) %>% 
  select(-domain_name)

rm(
  temp_su, temp_pb, temp_mh, temp_ph, temp_ce, temp_hor, temp_nc,
  temp_smri_area, temp_smri_vol, temp_smri_sulc, temp_smri_thick, temp_dti)

# export
write.csv(obs_num_DV, 'output/data_management/obs_num_DV.csv') 
rm(temp, DV_char, obs_num_DV, obs_num_DV0, obs_num_DV1, t.test_obs)

#------------------------------------------------------------------------------#
#                    descriptives for whole sample
#------------------------------------------------------------------------------#

obs_num <- get_summary_stats(data.2, type = "common") %>% 
  select(variable, n, mean, sd, min, max) %>% 
  rename(
    n_obs = n,
    mean_obs = mean,
    sd_obs = sd,
    min_obs = min,
    max_obs = max) %>% 
  slice(-1) %>% 
  left_join(table_names_numeric, by = 'variable') %>% 
  relocate(domain_name, table_name, variable)

# add headers
temp_demo <- obs_num %>% 
  subset(domain_name == 'Demographics') %>% 
  add_row(table_name = 'Demographics', .before = 1) 

temp_su <- obs_num %>% 
  subset(domain_name == 'Self and Peer Involvement with Substance Use') %>% 
  add_row(table_name = 'Self and Peer Involvement with Substance Use', 
          .before = 1) 

temp_pb <- obs_num %>% 
  subset(domain_name == 'Parenting Behaviors') %>% 
  add_row(table_name = 'Parenting Behaviors', .before = 1) 

temp_mh <- obs_num %>% 
  subset(domain_name == 'Mental Health') %>% 
  add_row(table_name = 'Mental Health', .before = 1) 

temp_ph <- obs_num %>% 
  subset(domain_name == 'Physical Health') %>% 
  add_row(table_name = 'Physical Health', .before = 1) 

temp_ce <- obs_num %>% 
  subset(domain_name == 'Culture & Environment') %>% 
  add_row(table_name = 'Culture & Environment', .before = 1) 

temp_hor <- obs_num %>% 
  subset(domain_name == 'Hormones') %>% 
  add_row(table_name = 'Hormones', .before = 1) 

temp_nc <- obs_num %>% 
  subset(domain_name == 'Neurocognitive Factors') %>% 
  add_row(table_name = 'Neurocognitive Factors', .before = 1) 

temp_smri_area <- obs_num %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_area')) %>% 
  add_row(table_name = 'sMRI: Area', .before = 1) 

temp_smri_vol <- obs_num %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_vol')) %>% 
  add_row(table_name = 'sMRI: Volume', .before = 1) 

temp_smri_sulc <- obs_num %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_sulc')) %>% 
  add_row(table_name = 'sMRI: Sulcul Depth', .before = 1) 

temp_smri_thick <- obs_num %>% 
  subset(domain_name == 'Structural Magnetic Resonance Imaging (sMRI)') %>% 
  filter(str_detect(variable, 'smri_thick')) %>% 
  add_row(table_name = 'sMRI: Cortical Thickness', .before = 1) 

temp_dti <- obs_num %>% 
  subset(domain_name == 'Diffusion Tensor Imaging (DTI)') %>% 
  add_row(table_name = 'DTI: FA', .before = 1) 

obs_num <- rbind(
  temp_demo, temp_su, temp_pb, temp_mh, temp_ph, temp_ce, temp_hor, temp_nc, 
  temp_smri_area, temp_smri_vol, temp_smri_sulc, temp_smri_thick, temp_dti) %>% 
  select(-domain_name)

rm(
  temp_demo, temp_su, temp_pb, temp_mh, temp_ph, temp_ce, temp_hor, temp_nc,
  temp_smri_area, temp_smri_vol, temp_smri_sulc, temp_smri_thick, temp_dti)

write.csv(obs_num, 'output/data_management/supplement/obs_num.csv') 

# Sample Characteristics: Categorical Variables --------------------------------

factor_sum <- function(x) {
  n <- table(x) # frequency
  proportion <- round(prop.table(n), 4) # proportion of non-missing values
  percentage <- (round(prop.table(n), 4))*100
  OUT <- cbind(n, proportion, percentage)
}

# split by DV
p1_DV0 <- factor_sum(demos_DV0$race_4l) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = race_4l', .before = 1) 
p1_DV1 <- factor_sum(demos_DV1$race_4l) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = race_4l', .before = 1) 

p2_DV0 <- factor_sum(demos_DV0$eth_hisp) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = eth_hisp', .before = 1) 
p2_DV1 <- factor_sum(demos_DV1$eth_hisp) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = eth_hisp', .before = 1) 

p3_DV0 <- factor_sum(demos_DV0$income) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = income', .before = 1) 
p3_DV1 <- factor_sum(demos_DV1$income) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = income', .before = 1) 

p4_DV0 <- factor_sum(demos_DV0$religion) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = religion', .before = 1) 
p4_DV1 <- factor_sum(demos_DV1$religion) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = religion', .before = 1) 

p5_DV0 <- factor_sum(demos_DV0$p_edu) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = p_edu', .before = 1) 
p5_DV1 <- factor_sum(demos_DV1$p_edu) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = p_edu', .before = 1) 

p6_DV0 <- factor_sum(demos_DV0$sex_2l) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = sex_2l', .before = 1) 
p6_DV1 <- factor_sum(demos_DV1$sex_2l) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = sex_2l', .before = 1) 

p7_DV0 <- factor_sum(demos_DV0$rec_bin) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = rec_bin', .before = 1) 
p7_DV1 <- factor_sum(demos_DV1$rec_bin) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = rec_bin', .before = 1) 

p8_DV0 <- factor_sum(demos_DV0$exp_sub) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = exp_sub', .before = 1) 
p8_DV1 <- factor_sum(demos_DV1$exp_sub) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = exp_sub', .before = 1) 

p9_DV0 <- factor_sum(demos_DV0$tbi_injury) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = tbi_injury', .before = 1) 
p9_DV1 <- factor_sum(demos_DV1$tbi_injury) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = tbi_injury', .before = 1) 

p10_DV0 <- factor_sum(demos_DV0$kbi_p_grades_in_school) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = kbi_p_grades_in_school', .before = 1) 
p10_DV1 <- factor_sum(demos_DV1$kbi_p_grades_in_school) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = kbi_p_grades_in_school', .before = 1) 

p11_DV0 <- factor_sum(demos_DV0$det_susp) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = det_susp', .before = 1) 
p11_DV1 <- factor_sum(demos_DV1$det_susp) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = det_susp', .before = 1) 

p12_DV0 <- factor_sum(demos_DV0$se_services) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = se_services', .before = 1) 
p12_DV1 <- factor_sum(demos_DV1$se_services) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = se_services', .before = 1) 

p13_DV0 <- factor_sum(demos_DV0$cct) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 0; Variable = cct', .before = 1) 
p13_DV1 <- factor_sum(demos_DV1$cct) %>% 
  as.data.frame() %>% 
  rownames_to_column(., var = 'variable') %>% 
  add_row(variable = 'DV = 1; Variable = cct', .before = 1) 

p1 <- cbind(p1_DV0, p1_DV1)
p2 <- cbind(p2_DV0, p2_DV1) 
p3 <- cbind(p3_DV0, p3_DV1)
p4 <- cbind(p4_DV0, p4_DV1)
p5 <- cbind(p5_DV0, p5_DV1) 
p6 <- cbind(p6_DV0, p6_DV1) 
p7 <- cbind(p7_DV0, p7_DV1) 
p8 <- cbind(p8_DV0, p8_DV1) 
p9 <- cbind(p9_DV0, p9_DV1) 
p10 <- cbind(p10_DV0, p10_DV1) 
p11 <- cbind(p11_DV0, p11_DV1) 
p12 <- cbind(p12_DV0, p12_DV1) 
p13 <- cbind(p13_DV0, p13_DV1) 

obs_factor_DV <- rbind(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)

write.csv(obs_factor_DV, 'output/data_management/obs_factor_DV.csv') 

rm(p1_DV0, p1_DV1, p2_DV0, p2_DV1, p3_DV0, p3_DV1, p4_DV0, p4_DV1,
   p5_DV0, p5_DV1, p6_DV0, p6_DV1, p7_DV0, p7_DV1, p8_DV0, p8_DV1,
   p9_DV0, p9_DV1, p10_DV0, p10_DV1, p11_DV0, p11_DV1, p12_DV0, p12_DV1,
   p13_DV0, p13_DV1,
   p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13,
   obs_factor_DV)

# chi-square and Fisher's Test

chisq_race <- tidy(chisq.test(data.2$DV, data.2$race_4l)) %>% 
  mutate(variable = 'Race') %>% 
  relocate(variable) 
chisq_eth_hisp <- tidy(chisq.test(data.2$DV, data.2$eth_hisp)) %>% 
  mutate(variable = 'Ethnicity') %>% 
  relocate(variable) 
chisq_income <- tidy(chisq.test(data.2$DV, data.2$income)) %>% 
  mutate(variable = 'Income') %>% 
  relocate(variable)
chisq_religion <- tidy(chisq.test(data.2$DV, data.2$religion))  
# follow-up with Fisher's Test due to small cell sizes: 
fisher_religion <- as.matrix(
  table(data.2$DV, data.2$religion, useNA = 'ifany'))
fisher.test(fisher_religion, simulate.p.value=TRUE) 
rm(fisher_religion)

chisq_sex <- tidy(chisq.test(data.2$DV, data.2$sex_2l)) %>% 
  mutate(variable = 'Sex') %>% 
  relocate(variable)
chisq_p_edu <- tidy(chisq.test(data.2$DV, data.2$p_edu)) %>% 
  mutate(variable = 'P_edu') %>% 
  relocate(variable)
chisq_rec_bin <- tidy(chisq.test(data.2$DV, data.2$rec_bin)) %>% 
  mutate(variable = 'rec_bin') %>% 
  relocate(variable)
chisq_exp_sub <- tidy(chisq.test(data.2$DV, data.2$exp_sub)) %>% 
  mutate(variable = 'exp_sub') %>% 
  relocate(variable)
chisq_tbi_injury <- tidy(chisq.test(data.2$DV, data.2$tbi_injury)) %>% 
  mutate(variable = 'tbi_injury') %>% 
  relocate(variable)
chisq_grades <- tidy(chisq.test(data.2$DV, data.2$kbi_p_grades_in_school)) %>% 
  mutate(variable = 'grades') %>% 
  relocate(variable) 
chisq_det_susp <- tidy(chisq.test(data.2$DV, data.2$det_susp)) %>% 
  mutate(variable = 'det_susp') %>% 
  relocate(variable)
chisq_se_services <- tidy(chisq.test(data.2$DV, data.2$se_services)) %>% 
  mutate(variable = 'se_services') %>% 
  relocate(variable)
chisq_cct <- tidy(chisq.test(data.2$DV, data.2$cct)) %>% 
  mutate(variable = 'cct') %>% 
  relocate(variable)

obs_factor_diff <- rbind(
  chisq_race, chisq_eth_hisp, chisq_income, chisq_sex,
  chisq_p_edu, chisq_rec_bin, chisq_exp_sub, chisq_tbi_injury, chisq_grades,
  chisq_det_susp, chisq_se_services, chisq_cct) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(p.value = ifelse(p.value == 0.000, '<0.001', p.value))

write.csv(obs_factor_diff, 'output/data_management/obs_factor_diff.csv')

rm(demos_DV0, demos_DV1, 
   chisq_race, chisq_eth_hisp, chisq_income, chisq_religion,
   chisq_sex, chisq_p_edu, chisq_rec_bin, chisq_exp_sub, chisq_tbi_injury, 
   chisq_grades, chisq_det_susp, chisq_se_services, chisq_cct,
   obs_factor_diff)

# Missing Data in Observed Sample ----------------------------------------------

# names of variables w/missing data
NA_obs_numeric <- data.2 %>% 
  select_if(~ any(is.na(.))) %>% 
  select_if(~is.numeric(.)) 

NA_obs_numeric <- NA_obs_numeric %>% 
  Desc(., plotit = FALSE) 
capture.output(
  NA_obs_numeric, 
  file = 'output/data_management/supplement/NA_obs_numeric.txt')

NA_obs_factor <- data.2 %>% 
  select_if(~ any(is.na(.))) %>% 
  select_if(~is.factor(.))

NA_obs_factor <- NA_obs_factor %>% 
  Desc(., plotit = FALSE) 
capture.output(
  NA_obs_factor, 
  file = 'output/data_management/supplement/NA_obs_factor.txt')

rm(NA_obs_numeric, NA_obs_factor)



