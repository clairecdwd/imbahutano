
categoricalvars <- c("HIV_Status", "HIV_Category", "HIV_CD4_Grade",
           "Diabetes_Status", "Diabetes_Category", "Diabetes_Grade",
           "Hypertension_Status", "Hypertension_Category", "Hypertension_Grade",
           "BMI_Grade", "BMI_Underweight", "Stunting", "TB_Status", "Smoking_Cat", "AUDIT_Score_Cat_CJC",
           "ImpLungFn_Status", "ImpLungFn_Type", "ImpLungFn_Grade", "FVC_Grade", "FEV1FVC_Grade", "Comorb_TBEver",
           "Anaemia_Grade")

if (group == "Adolescents") {
    categoricalvars <- categoricalvars[! categoricalvars %in% c("Diabetes_Status", "Diabetes_Category", "Diabetes_Grade",
           "Hypertension_Status", "Hypertension_Category", "Hypertension_Grade")]
}

temp_df <- temp_df %>%
    mutate(BMI_Underweight = ifelse(BMI_Grade %in% c("Moderate/severe underweight
", "Mild underweight"), "Yes", "No"),
BMI_Underweight = factor(BMI_Underweight, levels = c("No", "Yes")))

continuousvars <- c("HIV_CD4", "HbA1c", "BP_Sys_Overall", "BP_Dia_Overall")

calculate_proportions <- function(data, condition_variables, household_id_var) {
  # Create survey design object
  survey_design <- svydesign(id = ~eval(sym(household_id_var)), data = data)
  
  # Initialize empty lists to store results
  estimates <- list()
  lower_bounds <- list()
  upper_bounds <- list()
  total_counts <- list()
  n_with_condition <- list()
  
  # Calculate proportions, confidence intervals, and other statistics for each condition variable
  for (condition_variable in condition_variables) {
      
      levels <- levels(data[[condition_variable]])
      
        for(level in levels) {
            
        # Calculate proportion and confidence interval
        proportion_ci <- svyciprop(as.formula( paste0( "~I(", condition_variable, "=='", level, "')") ), design = survey_design, na.rm = T)
    
        # Extract proportion, lower bound, upper bound, total count, and count with condition
        proportion <- as.vector(proportion_ci)
        ci <- attr(proportion_ci, "ci")
        lower_bound <- ci[[1]]
        upper_bound <- ci[[2]]
        total_count <- sum(complete.cases(data[[condition_variable]]) == T)
        n_condition <- sum(data[[condition_variable]] == level, na.rm = TRUE)

        label <- paste0(condition_variable, ".", level)
        
        # Store results in lists
        estimates[[label]] <- proportion
        lower_bounds[[label]] <- lower_bound
        upper_bounds[[label]] <- upper_bound
        total_counts[[label]] <- total_count
        n_with_condition[[label]] <- n_condition
        }
  }
  
  # Create a data frame to store results
  results <- data.frame(
    ConditionVariable = names(estimates),
    Estimate = unlist(estimates),
    LowerBound = unlist(lower_bounds),
    UpperBound = unlist(upper_bounds),
    TotalCount = unlist(total_counts),
    NWithCondition = unlist(n_with_condition)
  ) %>%
      separate(ConditionVariable, into = c("Condition", "Level"), sep = "\\.", remove = T, extra = "merge") %>%
    mutate(n_N = paste0(NWithCondition, "/", TotalCount),
           Estimate_CI = paste0(trimws(format(round(Estimate*100,1),nsmall=1)),
                                  "% (", trimws(format(round(LowerBound*100,1),nsmall=1)), "%–",
                                  trimws(format(round(UpperBound*100,1),nsmall=1)), "%)"),
           Percent = paste0(format(round(Estimate*100, 1), nsmall = 1),"%")) %>%
      remove_rownames()
  
  return(results)
}

calculate_medians <- function(data, condition_variables, household_id_var) {
  # Create survey design object
  survey_design <- svydesign(id = ~HholdID + SiteID, data = data)
  
  # Initialize empty lists to store results
  estimates <- list()
  lower_bounds <- list()
  upper_bounds <- list()
  total_counts <- list()

  # Calculate proportions, confidence intervals, and other statistics for each condition variable
  for (condition_variable in condition_variables) {
      
        # Calculate proportion and confidence interval
        median_ci <- svyquantile(as.formula( paste0( "~", condition_variable) ), 0.5, design = survey_design, na.rm = T)
    
        # Extract proportion, lower bound, upper bound, total count, and count with condition
        temp <- as.vector(median_ci[[1]])
        median <- temp[[1]]
        lower_bound <- temp[[2]]
        upper_bound <- temp[[3]]
        total_count <- sum(complete.cases(data[[condition_variable]]) == T)

        label <- paste0(condition_variable)
        
        # Store results in lists
        estimates[[label]] <- median
        lower_bounds[[label]] <- lower_bound
        upper_bounds[[label]] <- upper_bound
        total_counts[[label]] <- total_count
        }

  # Create a data frame to store results
  results <- data.frame(
    ConditionVariable = names(estimates),
    Estimate = unlist(estimates),
    LowerBound = unlist(lower_bounds),
    UpperBound = unlist(upper_bounds),
    TotalCount = unlist(total_counts)
  ) %>%
    mutate(n_N = TotalCount,
           Estimate_CI = paste0(trimws(format(round(Estimate,1),nsmall=1)),
                                  " (", trimws(format(round(LowerBound,1),nsmall=1)), "–",
                                  trimws(format(round(UpperBound,1),nsmall=1)), ")"),
           Percent = NA) %>%
    mutate(NWithCondition = NA,
           Level = NA,
           Condition = ConditionVariable) %>%
      select(-ConditionVariable) %>%
      remove_rownames()
  
  return(results)
}

# Cascade of care

if (group %in% c("Adults", "All")) {
    
df_cascade <- temp_df %>%
    mutate(HIV_Known = Comorb_HIV,
        HIV_Treated = HIV_ART_CJC,
           Diabetes_Known = Comorb_Diabetes,
           Diabetes_Treated = Treated_Diabetes,
           Hypertension_Known = Comorb_Hypertension,
           Hypertension_Treated = Treated_Hypertension) 

res_casc_hiv_overall <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Overall")

res_casc_diab_overall <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Overall")

res_casc_hypertension_overall <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Overall")

# By sex
res_casc_hiv_female <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Women") %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Women")

res_casc_diab_female <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Women") %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Women")

res_casc_hypertension_female <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Women") %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Women")

res_casc_hiv_male <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Men") %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Men")

res_casc_diab_male <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Men") %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Men")

res_casc_hypertension_male <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(Sex_Use == "Men") %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Men")
    
# By Site
res_casc_hiv_brti <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "BRTI") %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Zimbabwe")

res_casc_diab_brti <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "BRTI") %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Zimbabwe")

res_casc_hypertension_brti <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "BRTI") %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Zimbabwe")

res_casc_hiv_ins <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "INS") %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Mozambique")

res_casc_diab_ins <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "INS") %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Mozambique")

res_casc_hypertension_ins <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "INS") %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Mozambique")

res_casc_hiv_mmrc <- df_cascade %>%
    filter(HIV_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "MMRC") %>%
    calculate_proportions(., condition_variables = c("HIV_Known", "HIV_Treated", "HIV_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Tanzania")

res_casc_diab_mmrc <- df_cascade %>%
    filter(Diabetes_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "MMRC") %>%
    calculate_proportions(., condition_variables = c("Diabetes_Known", "Diabetes_Treated", "Diabetes_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Tanzania")

res_casc_hypertension_mmrc <- df_cascade %>%
    filter(Hypertension_Status == "Yes") %>%
    filter(Age_Baseline >= 18) %>%
    filter(SiteID == "MMRC") %>%
    calculate_proportions(., condition_variables = c("Hypertension_Known", "Hypertension_Treated", "Hypertension_Controlled"), household_id_var = "HholdID") %>%
    mutate(Stratum = "Tanzania")

cascade_results <- rbind(res_casc_hiv_overall, res_casc_diab_overall, res_casc_hypertension_overall,
     res_casc_hiv_female, res_casc_diab_female, res_casc_hypertension_female, res_casc_hiv_male, res_casc_diab_male, res_casc_hypertension_male
     , res_casc_hiv_brti, res_casc_diab_brti, res_casc_hypertension_brti, res_casc_hiv_ins, res_casc_diab_ins, res_casc_hypertension_ins,
     res_casc_hiv_mmrc, res_casc_diab_mmrc, res_casc_hypertension_mmrc
     ) %>%
    separate(Condition, into = c("Condition", "Status"), extra = "merge") %>%
    mutate(Status = factor(Status, levels = c("Known", "Treated", "Controlled")))

rm(res_casc_hiv_overall, res_casc_diab_overall, res_casc_hypertension_overall,
     res_casc_hiv_female, res_casc_diab_female, res_casc_hypertension_female, res_casc_hiv_male, res_casc_diab_male, res_casc_hypertension_male
     , res_casc_hiv_brti, res_casc_diab_brti, res_casc_hypertension_brti, res_casc_hiv_ins, res_casc_diab_ins, res_casc_hypertension_ins,
     res_casc_hiv_mmrc, res_casc_diab_mmrc, res_casc_hypertension_mmrc
     )

}
