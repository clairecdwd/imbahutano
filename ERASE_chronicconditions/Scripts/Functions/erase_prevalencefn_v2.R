# https://stackoverflow.com/questions/13402829/r-looping-through-in-survey-package

library(survey)
library(dplyr)

if (group == "Adolescents") {
    categoricalvars <- categoricalvars[! categoricalvars %in% c("Diabetes_Status", "Diabetes_Category", "Diabetes_Grade",
           "Hypertension_Status", "Hypertension_Category", "Hypertension_Grade")]
}

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