# https://stackoverflow.com/questions/13402829/r-looping-through-in-survey-package

library(survey)
library(dplyr)

categoricalvars <- c("HIV_Status", "HIV_Category", "HIV_CD4_Grade",
           "Diabetes_Status", "Diabetes_Category", "Diabetes_Grade",
           "Hypertension_Status", "Hypertension_Category", "Hypertension_Grade",
           "BMI_Grade", "BMI_Underweight", "BMI_Obese", "Stunting", "TB_Status", "Smoking_Cat", "AUDIT_Score_Cat_CJC",
           "ImpLungFn_Status", "ImpLungFn_Type", "ImpLungFn_Grade", "FVC_Grade", "FEV1FVC_Grade",
           "Anaemia_Status", "Anaemia_Grade")

if (group == "Adolescents") {
    categoricalvars <- categoricalvars[! categoricalvars %in% c("Diabetes_Status", "Diabetes_Category", "Diabetes_Grade",
           "Hypertension_Status", "Hypertension_Category", "Hypertension_Grade")]
}

temp_df <- temp_df %>%
    mutate(BMI_Underweight = ifelse(BMI_Grade %in% 
                                        c("Moderate/severe underweight", "Mild underweight"), "Yes", "No"),
           BMI_Underweight = factor(BMI_Underweight, levels = c("No", "Yes")),
           BMI_Obese = ifelse(BMI_Grade %in% 
                                        c("Overweight", "Obese"), "Yes", "No"),
           BMI_Obese = factor(BMI_Obese, levels = c("No", "Yes"))
)

continuousvars <- c("HIV_CD4", "HbA1c", "BP_Sys_Overall", "BP_Dia_Overall", "Hb")

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


temp_df %>%
    tabyl(Hhold_ResidenceArea, SiteID)

# Overall
res_prev_tanzrur <- temp_df %>%
    filter(Hhold_ResidenceArea == "Rural") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Rural")

res_prev_tanzper <- temp_df %>%
    filter(Hhold_ResidenceArea == "Peri-Urban") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Peri-Urban")

res_prev_tanzurb <- temp_df %>%
    filter(Hhold_ResidenceArea == "Urban") %>% droplevels() %>%
    calculate_proportions(., condition_variables = categoricalvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Urban")

res_med_tanzrur <- temp_df %>%
    filter(Hhold_ResidenceArea == "Rural") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Rural")

res_med_tanzper <- temp_df %>%
    filter(Hhold_ResidenceArea == "Peri-Urban") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Peri-Urban")

res_med_tanzurb <- temp_df %>%
    filter(Hhold_ResidenceArea == "Urban") %>% droplevels() %>%
    calculate_medians(., condition_variables = continuousvars, household_id_var = "HholdID") %>%
    mutate(Stratum = "Urban")


prevalence_results <- rbind(res_prev_tanzrur, res_prev_tanzper, res_prev_tanzurb, 
                     res_med_tanzrur, res_med_tanzper, res_med_tanzurb)

rm(res_prev_tanzrur, res_prev_tanzper, res_prev_tanzurb, 
                     res_med_tanzrur, res_med_tanzper, res_med_tanzurb)

N_all <- prevalence_results %>%
    filter(Condition == "HIV_Status" & Level == "Yes") %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "All participants",
           Condition = NA,
           Level = NA) %>%
    select(Category, Condition, Level, everything())

Prev_all <- prevalence_results %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("HIV_Status", "HIV_CD4_Grade", "HIV_CD4", "BMI_Grade", "Anaemia_Status", "Anaemia_Grade", "Hb", "ImpLungFn_Status", "Comorb_TBEver")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "All participants")

N_adol <- prevalence_results %>%
    filter(Condition == "Stunting" & Level == "Normal") %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adolescents",
           Condition = NA,
           Level = NA)

Prev_adol <- prevalence_results %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Stunting")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adolescents")

N_adul <- prevalence_results %>%
    filter(Condition == "Diabetes_Status" & Level == "Yes") %>%
    select(Stratum, TotalCount) %>%
    mutate(N = paste0("N = ", TotalCount)) %>%
    select(-TotalCount) %>%
    pivot_wider(names_from = "Stratum", values_from = "N") %>%
    mutate(Category = "Adults",
           Condition = NA,
           Level = NA)

Prev_adul <- prevalence_results %>%
    select(Condition, Level, Estimate_CI, Stratum) %>%
    filter(Condition %in% c("Diabetes_Status", "Diabetes_Grade", "HbA1c",
                            "Hypertension_Status", "Hypertension_Grade", "BP_Sys_Overall", "BP_Dia_Overall")) %>%
    mutate(Condition = str_remove(Condition, "_Status")) %>%
    filter(Level != "No" | is.na(Level)) %>%
    pivot_wider(names_from = "Stratum", values_from = c("Estimate_CI")) %>%
    mutate(Category = "Adults")

prevalencetable_tanzrururb <- rbind(N_all, Prev_all, N_adol, Prev_adol, N_adul, Prev_adul)

prevalencetable_tanzrururb <- prevalencetable_tanzrururb %>%
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