# Summary table of prevalence of chronic diseases for paper

# In all sites together

# Overall
res_prev_overall <- temp_df %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Overall")

res_med_overall <- temp_df %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Overall")

# By Sex
res_prev_female <- temp_df %>%
    filter(Sex_Use == "Women") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Women")

res_prev_male <- temp_df %>%
    filter(Sex_Use == "Men") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Men")

res_med_female <- temp_df %>%
    filter(Sex_Use == "Women") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Women")

res_med_male <- temp_df %>%
    filter(Sex_Use == "Men") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Men")

# By site
res_prev_brti <- temp_df %>%
    filter(SiteID == "BRTI") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "BRTI")

res_prev_ins <- temp_df %>%
    filter(SiteID == "INS") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "INS")

res_prev_mmrc <- temp_df %>%
    filter(SiteID == "MMRC") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "MMRC")

res_med_brti <- temp_df %>%
    filter(SiteID == "BRTI") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "BRTI")

res_med_ins <- temp_df %>%
    filter(SiteID == "INS") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "INS")

res_med_mmrc <- temp_df %>%
    filter(SiteID == "MMRC") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "MMRC")

# By age group
res_prev_adol <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "10-17 years")

res_prev_adul <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "18-39 years")

res_prev_oadl <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "40+ years")

res_med_adol <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "10-17 years") %>% droplevels() %>%
    calculate_medians(., condition_variables = "HIV_CD4", household_id_var = "HholdID") %>%
    mutate(Stratum = "10-17 years")

res_med_adul <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "18-39 years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "18-39 years")

res_med_oadl <- temp_df %>%
    filter(Age_Baseline_Cat_CJC == "40+ years") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "40+ years")


prevalence_results <- rbind(res_prev_overall, res_med_overall,
                     res_prev_female, res_prev_male, res_med_female, res_med_male,
                     res_prev_brti, res_prev_ins, res_prev_mmrc, res_med_brti, res_med_ins, res_med_mmrc,
                     res_prev_adol, res_prev_adul, res_prev_oadl, res_med_adol, res_med_adul, res_med_oadl)

rm(res_prev_overall, res_med_overall,
    res_prev_female, res_prev_male, res_med_female, res_med_male,
    res_prev_adol, res_prev_adul, res_prev_olad, res_med_adol, res_med_adul, res_med_olad,
    res_prev_brti, res_prev_ins, res_prev_mmrc, res_med_brti, res_med_ins, res_med_mmrc,
   res_prev_adol, res_prev_adul, res_prev_oadl, res_med_adol, res_med_adul, res_med_oadl)





# Main version - just percentages
N_all <- prevalence_results %>%
    filter(Condition == "HIV_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "All participants",
           Condition = NA,
           Level = NA) %>%
    select(Category, Condition, Level, everything())

Prev_all <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("HIV_Status", "HIV_CD4_Grade", "HIV_CD4", "BMI_Grade", "Anaemia_Grade", "Anaemia_Status", "Hb", "ImpLungFn_Status", "Comorb_TBEver")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    group_by(Stratum) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "All participants")

N_adol <- prevalence_results %>%
    filter(Condition == "Stunting" & Level == "Normal") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adolescents",
           Condition = NA,
           Level = NA,
           `18-39 years` = NA,
           `40+ years` = NA)

Prev_adol <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("Stunting")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "Adolescents",
           `18-39 years` = NA,
           `40+ years` = NA)

N_adul <- prevalence_results %>%
    filter(Condition == "Diabetes_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adults",
           Condition = NA,
           Level = NA,
           `10-17 years` = NA)

Prev_adul <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("Diabetes_Status", "Diabetes_Grade", "HbA1c",
                            "Hypertension_Status", "Hypertension_Grade", "BP_Sys_Overall", "BP_Dia_Overall")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "Adults",
           `10-17 years` = NA)

prevalencetable_main <- rbind(N_all, Prev_all, N_adol, Prev_adol, N_adul, Prev_adul)

# Supplement version - confidence intervals

# Summary table of prevalence of chronic diseases for paper

N_all <- prevalence_results %>%
    filter(Condition == "HIV_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "All participants",
           Condition = NA,
           Level = NA) %>%
    select(Category, Condition, Level, everything())

Prev_all <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("HIV_Status", "HIV_CD4_Grade", "HIV_CD4", "BMI_Grade", "Anaemia_Status", "Anaemia_Grade", "Hb", "ImpLungFn_Status", "Comorb_TBEver")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "All participants")

N_adol <- prevalence_results %>%
    filter(Condition == "Stunting" & Level == "Normal") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adolescents",
           Condition = NA,
           Level = NA,
           `18-39 years` = NA,
           `40+ years` = NA)

Prev_adol <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Stunting")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adolescents",
           `18-39 years` = NA,
           `40+ years` = NA)

N_adul <- prevalence_results %>%
    filter(Condition == "Diabetes_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adults",
           Condition = NA,
           Level = NA,
           `10-17 years` = NA)

Prev_adul <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "Women", "Men", "10-17 years", "18-39 years", "40+ years")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Diabetes_Status", "Diabetes_Grade", "HbA1c",
                            "Hypertension_Status", "Hypertension_Grade", "BP_Sys_Overall", "BP_Dia_Overall")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adults",
           `10-17 years` = NA)

prevalencetable_supplement <- rbind(N_all, Prev_all, N_adol, Prev_adol, N_adul, Prev_adul)

# Add labels

prevalencetable_main <- prevalencetable_main %>%
    mutate(Condition =
               case_when(Condition == "HIV_CD4_Grade" ~ "CD4 category",
                         Condition == "HIV_CD4" ~ "CD4 (cells / uL)",
                         Condition == "BMI_Grade" ~ "BMI category",
                         Condition == "ImpLungFn" ~ "Chronic lung disease",
                         Condition == "Comorb_TBEver" ~ "Previous TB",
                         Condition == "Anaemia_Grade" ~ "Anaemia category",
                         Condition == "Hb" ~ "Hb (g/dL)",
                         Condition == "Diabetes_Grade" ~ "HbA1c category",
                         Condition == "HbA1c" ~ "HbA1c (%)",
                         Condition == "Hypertension_Grade" ~ "BP category",
                         Condition == "BP_Sys_Overall" ~ "Systolic BP (mmHg)",
                         Condition == "BP_Dia_Overall" ~ "Diastolic BP (mmHg)",
                         complete.cases(Condition) ~ Condition),
           Condition = factor(Condition, levels = c(
               "HIV", "CD4 category", "CD4 (cells / uL)",
               "BMI category",
               "Chronic lung disease", "Previous TB",
               "Anaemia", "Anaemia category", "Hb (g/dL)",
               "Stunting",
               "Diabetes", "HbA1c category", "HbA1c (%)",
               "Hypertension", "BP category", "Systolic BP (mmHg)", "Diastolic BP (mmHg)")),
           Level = case_when(Level == "Yes" ~ NA,
                             complete.cases(Level) ~ Level),
           Category = factor(Category, levels = c("All participants", "Adolescents", "Adults"))) %>%
    arrange(Category, desc(is.na(Condition)), Condition)

prevalencetable_supplement <- prevalencetable_supplement %>%
    mutate(Condition =
               case_when(Condition == "HIV_CD4_Grade" ~ "CD4 category",
                         Condition == "HIV_CD4" ~ "CD4 (cells / uL)",
                         Condition == "BMI_Grade" ~ "BMI category",
                         Condition == "ImpLungFn" ~ "Chronic lung disease",
                         Condition == "Comorb_TBEver" ~ "Previous TB",
                         Condition == "Anaemia_Grade" ~ "Anaemia category",
                         Condition == "Hb" ~ "Hb (g/dL)",
                         Condition == "Diabetes_Grade" ~ "HbA1c category",
                         Condition == "HbA1c" ~ "HbA1c (%)",
                         Condition == "Hypertension_Grade" ~ "BP category",
                         Condition == "BP_Sys_Overall" ~ "Systolic BP (mmHg)",
                         Condition == "BP_Dia_Overall" ~ "Diastolic BP (mmHg)",
                         complete.cases(Condition) ~ Condition),
           Condition = factor(Condition, levels = c(
               "HIV", "CD4 category", "CD4 (cells / uL)",
               "BMI category",
               "Chronic lung disease", "Previous TB",
               "Anaemia", "Anaemia category", "Hb (g/dL)",
               "Stunting",
               "Diabetes", "HbA1c category", "HbA1c (%)",
               "Hypertension", "BP category", "Systolic BP (mmHg)", "Diastolic BP (mmHg)")),
           Level = case_when(Level == "Yes" ~ NA,
                             complete.cases(Level) ~ Level),
           Category = factor(Category, levels = c("All participants", "Adolescents", "Adults"))) %>%
    arrange(Category, desc(is.na(Condition)), Condition)


# By site for supplement

# Main version - just percentages
N_all <- prevalence_results %>%
    filter(Condition == "HIV_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "All participants",
           Condition = NA,
           Level = NA) %>%
    select(Category, Condition, Level, everything())

Prev_all <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("HIV_Status", "HIV_CD4_Grade", "HIV_CD4", "BMI_Grade", "Anaemia_Grade", "Anaemia_Status", "Hb", "ImpLungFn_Status", "Comorb_TBEver")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    group_by(Stratum) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "All participants")

N_adol <- prevalence_results %>%
    filter(Condition == "Stunting" & Level == "Normal") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adolescents",
           Condition = NA,
           Level = NA)

Prev_adol <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("Stunting")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "Adolescents")

N_adul <- prevalence_results %>%
    filter(Condition == "Diabetes_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adults",
           Condition = NA,
           Level = NA)

Prev_adul <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate, Percent, Stratum) %>%
    filter(Condition %in% c("Diabetes_Status", "Diabetes_Grade", "HbA1c",
                            "Hypertension_Status", "Hypertension_Grade", "BP_Sys_Overall", "BP_Dia_Overall")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    mutate(Estimate = ifelse(complete.cases(Percent), Percent, Estimate)) %>%
    select(-Percent) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate")) %>%
    mutate(Category = "Adults")

prevalencetable_bysite <- rbind(N_all, Prev_all, N_adol, Prev_adol, N_adul, Prev_adul)

prevalencetable_bysite <- prevalencetable_bysite %>%
    mutate(Condition =
               case_when(Condition == "HIV_CD4_Grade" ~ "CD4 category",
                         Condition == "HIV_CD4" ~ "CD4 (cells / uL)",
                         Condition == "BMI_Grade" ~ "BMI category",
                         Condition == "ImpLungFn" ~ "Chronic lung disease",
                         Condition == "Comorb_TBEver" ~ "Previous TB",
                         Condition == "Anaemia_Grade" ~ "Anaemia category",
                         Condition == "Hb" ~ "Hb (g/dL)",
                         Condition == "Diabetes_Grade" ~ "HbA1c category",
                         Condition == "HbA1c" ~ "HbA1c (%)",
                         Condition == "Hypertension_Grade" ~ "BP category",
                         Condition == "BP_Sys_Overall" ~ "Systolic BP (mmHg)",
                         Condition == "BP_Dia_Overall" ~ "Diastolic BP (mmHg)",
                         complete.cases(Condition) ~ Condition),
           Condition = factor(Condition, levels = c(
               "HIV", "CD4 category", "CD4 (cells / uL)",
               "BMI category",
               "Chronic lung disease", "Previous TB",
               "Anaemia", "Anaemia category", "Hb (g/dL)",
               "Stunting",
               "Diabetes", "HbA1c category", "HbA1c (%)",
               "Hypertension", "BP category", "Systolic BP (mmHg)", "Diastolic BP (mmHg)")),
           Level = case_when(Level == "Yes" ~ NA,
                             complete.cases(Level) ~ Level),
           Category = factor(Category, levels = c("All participants", "Adolescents", "Adults"))) %>%
    arrange(Category, desc(is.na(Condition)), Condition)

# By Site
# Perecent and CI
N_all <- prevalence_results %>%
    filter(Condition == "HIV_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "All participants",
           Condition = NA,
           Level = NA) %>%
    select(Category, Condition, Level, everything())

Prev_all <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("HIV_Status", "HIV_CD4_Grade", "HIV_CD4", "BMI_Grade", "Anaemia_Grade", "Anaemia_Status", "Hb", "ImpLungFn_Status", "Comorb_TBEver")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    group_by(Stratum) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "All participants")

N_adol <- prevalence_results %>%
    filter(Condition == "Stunting" & Level == "Normal") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adolescents",
           Condition = NA,
           Level = NA)

Prev_adol <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Stunting")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adolescents")

N_adul <- prevalence_results %>%
    filter(Condition == "Diabetes_Status" & Level == "Yes") %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adults",
           Condition = NA,
           Level = NA)

Prev_adul <- prevalence_results %>%
    filter(Stratum %in% c("Overall", "BRTI", "INS", "MMRC")) %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Diabetes_Status", "Diabetes_Grade", "HbA1c",
                            "Hypertension_Status", "Hypertension_Grade", "BP_Sys_Overall", "BP_Dia_Overall")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adults")

prevalencetable_bysite <- rbind(N_all, Prev_all, N_adol, Prev_adol, N_adul, Prev_adul)

prevalencetable_bysite <- prevalencetable_bysite %>%
    mutate(Condition =
               case_when(Condition == "HIV_CD4_Grade" ~ "CD4 category",
                         Condition == "HIV_CD4" ~ "CD4 (cells / uL)",
                         Condition == "BMI_Grade" ~ "BMI category",
                         Condition == "ImpLungFn" ~ "Chronic lung disease",
                         Condition == "Comorb_TBEver" ~ "Previous TB",
                         Condition == "Anaemia_Grade" ~ "Anaemia category",
                         Condition == "Hb" ~ "Hb (g/dL)",
                         Condition == "Diabetes_Grade" ~ "HbA1c category",
                         Condition == "HbA1c" ~ "HbA1c (%)",
                         Condition == "Hypertension_Grade" ~ "BP category",
                         Condition == "BP_Sys_Overall" ~ "Systolic BP (mmHg)",
                         Condition == "BP_Dia_Overall" ~ "Diastolic BP (mmHg)",
                         complete.cases(Condition) ~ Condition),
           Condition = factor(Condition, levels = c(
               "HIV", "CD4 category", "CD4 (cells / uL)",
               "BMI category",
               "Chronic lung disease", "Previous TB",
               "Anaemia", "Anaemia category", "Hb (g/dL)",
               "Stunting",
               "Diabetes", "HbA1c category", "HbA1c (%)",
               "Hypertension", "BP category", "Systolic BP (mmHg)", "Diastolic BP (mmHg)")),
           Level = case_when(Level == "Yes" ~ NA,
                             complete.cases(Level) ~ Level),
           Category = factor(Category, levels = c("All participants", "Adolescents", "Adults"))) %>%
    arrange(Category, desc(is.na(Condition)), Condition)

