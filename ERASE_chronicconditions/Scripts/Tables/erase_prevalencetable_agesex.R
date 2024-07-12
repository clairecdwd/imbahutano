# By Sex and age categories
# 
# Adolescents
res_prev_adolfemale <- temp_df %>%
    filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Women", AgeCat = "10-17 years")

res_prev_adolmale <- temp_df %>%
    filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Men", AgeCat = "10-17 years")

# res_med_adolfemale <- temp_df %>%
#     filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
#     calculate_medians(., condition_variables = "HIV_CD4", household_id_var = "HholdID") %>%
#     mutate(Sex = "Women", AgeCat = "10-17 years")
# 
# res_med_adolmale <- temp_df %>%
#     filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
#     calculate_medians(., condition_variables = "HIV_CD4", household_id_var = "HholdID") %>%
#     mutate(Sex = "Men", AgeCat = "10-17 years")

# Adults
res_prev_adulfemale <- temp_df %>%
    filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Women", AgeCat = "18-39 years")

res_prev_adulmale <- temp_df %>%
    filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Men", AgeCat = "18-39 years")

res_med_adulfemale <- temp_df %>%
    filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Women", AgeCat = "18-39 years")

res_med_adulmale <- temp_df %>%
    filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Men", AgeCat = "18-39 years")

# Older adults
res_prev_oadulfemale <- temp_df %>%
    filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Women", AgeCat = "40+ years")

res_prev_oadulmale <- temp_df %>%
    filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Men", AgeCat = "40+ years")

res_med_oadulfemale <- temp_df %>%
    filter(Sex_Use == "Women" & Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Women", AgeCat = "40+ years")

res_med_oadulmale <- temp_df %>%
    filter(Sex_Use == "Men" & Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Sex = "Men", AgeCat = "40+ years")

prevalence_results_agesex <- rbind(res_prev_adolfemale, res_prev_adolmale,
                                   #res_med_adolfemale, res_med_adolmale,
                                   res_prev_adulfemale, res_prev_adulmale,
                                   res_med_adulfemale, res_med_adulmale,
                                   res_prev_oadulfemale, res_prev_oadulmale,
                                   res_med_oadulfemale, res_med_oadulmale)

rm(res_prev_adolfemale, res_prev_adolmale,
                                   #res_med_adolfemale, res_med_adolmale,
                                   res_prev_adulfemale, res_prev_adulmale,
                                   res_med_adulfemale, res_med_adulmale,
                                   res_prev_oadulfemale, res_prev_oadulmale,
                                   res_med_oadulfemale, res_med_oadulmale)