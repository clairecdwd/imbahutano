

worldstandard <- with_dir(
    dir = paste0(datadir, "Reference population/"),
    expr = read_csv("world_standard_population_by_sex.csv")) %>%
    separate(AgeGroup, c("AgeBand", NULL, NULL)) %>%
    filter(Sex == "Male") %>% # Same numbers for men and women - lets just use men for all for now
    mutate(AgeBand = ifelse(AgeBand == "85plus", 85, AgeBand)) %>%
    mutate(AgeBand2 = as.numeric(AgeBand)) %>%
    group_by(AgeBand2) %>%
    mutate(Pop = sum(WorldStandardPopulation)) %>% # Sum across
    distinct(AgeBand2, Pop) %>%
    filter(AgeBand2 != 0)

temp <- df_analysis %>%
    filter(complete.cases(Age_Baseline)) %>%
    mutate(AgeBand2 = cut(Age_Baseline, seq(from = 0, to = 100, by = 5), right = F)) %>% # Create 5 year age bands
    separate(AgeBand2, c("var1", "AgeBandLHE", "var3"), remove = F) %>%
    mutate(AgeBandLHE = as.numeric(AgeBandLHE)) %>%
    select(-var1, -var3) %>%
    left_join(., worldstandard, by = c("AgeBandLHE" = "AgeBand2"))

pop = temp %>% # Create a vector which corresponds to the age bands in the data, in order
    select(AgeBandLHE, Pop) %>%
    arrange(AgeBandLHE) %>%
    distinct(AgeBandLHE, Pop) %>%
    filter(complete.cases(Pop))

temp %>% tabyl(AgeBandLHE) # Should be the same N unique values here as in the above vector

survey_design <- temp %>%
    svydesign(id = ~HholdID, data = ., strata = ~SiteID) # Create the survey design object

# adj <- svystandardize(survey_design, # Create the svystandardise object
#                  by = ~ AgeBandLHE, over = ~ 1,
#                  population = pop$Pop)
# 
# hiv <- svymean(~I(HIV_Status == "Yes"), design=adj) %>% as.data.frame()
# confidence_interval <- confint(hiv, level = 0.95)

#Across all 3 sites
# I am not sure if this is the correct method or whether to leave site out of the 
# standardisation. I think it should stay in, this means that it is standardised within each
# and then combined (I am not sure how combined - that is my issue). This then accounts for
# different age distributions in each site ( I think)
adj <- svystandardize(survey_design, # Create the svystandardise object
                 by = ~ AgeBandLHE, over = ~ Sex_Use+SiteID,
                 population = pop$Pop)

est_ci <- NULL

# HIV
est <- svyby(~I(HIV_Status == "Yes"), ~Sex_Use, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Diabetes
est <- svyby(~I(Diabetes_Status == "Yes"), ~Sex_Use, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Hypertension
est <- svyby(~I(Hypertension_Status == "Yes"), ~Sex_Use, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Anaemia
est <- svyby(~I(Anaemia_Status == "Yes"), ~Sex_Use, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Underweight
est <- svyby(~I(BMI_Underweight == "Yes"), ~Sex_Use, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Obesity (may include overweight - need to check)
est <- svyby(~I(BMI_Obese == "Yes"), ~Sex_Use, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Chronic lung disease
est <- svyby(~I(ImpLungFn_Status == "Yes"), ~Sex_Use, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

est_ci_bysex <- est_ci

# Overall ---------------------------------

est_ci = NULL

# HIV
est <- svymean(~I(HIV_Status == "Yes"), design=adj)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Diabetes
est <- svymean(~I(Diabetes_Status == "Yes"), design=adj, na.rm=TRUE)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Hypertension
est <- svymean(~I(Hypertension_Status == "Yes"), design=adj, na.rm=TRUE)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Anaemia
est <- svymean(~I(Anaemia_Status == "Yes"), design=adj)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Underweight
est <- svymean(~I(BMI_Underweight == "Yes"), design=adj)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Obesity (may include overweight - need to check)
est <- svymean(~I(BMI_Obese == "Yes"), design=adj)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Chronic lung disease
est <- svymean(~I(ImpLungFn_Status == "Yes"), design=adj, na.rm=TRUE)

temp <- est %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    mutate(value = mean) %>%
    select(var, value)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

est_ci_overall <- est_ci

table3_agestandardised_overall <- est_ci_overall %>%
    separate(var, into = c("A", "B", "C", "D"), remove = F) %>%
    select(-A) %>%
    mutate(B = case_when(
        C == "UWStatus" ~ "Underweight",
        C == "ObsStatus" ~ "Overweight/Obese",
        C == "ImpLungFn" ~ "Chronic lung disease",
        complete.cases(B) ~ B)) %>%
    mutate(Estimate_CI = paste0(trimws(format(round(value*100,1),nsmall=1)),
                                "% (", trimws(format(round(`2.5 %`*100,1),nsmall=1)), "%–",
                                trimws(format(round(`97.5 %`*100,1),nsmall=1)), "%)"),
           C = "Overall",
           Condition = B
           ) %>%
    select(Condition, C, Estimate_CI) %>%
    pivot_wider(names_from = "C", values_from = "Estimate_CI")

table3_agestandardised_bysex <- est_ci_bysex %>%
    separate(var, into = c("A", "B", "C", "D"), remove = F) %>%
    mutate(C = case_when(
        D == "UWStatus" ~ "Underweight",
        D == "ObsStatus" ~ "Overweight/Obese",
        D == "ImpLungFn" ~ "Chronic lung disease",
        complete.cases(C) ~ C)) %>%
    mutate(Estimate_CI = paste0(trimws(format(round(value*100,1),nsmall=1)),
                                "% (", trimws(format(round(`2.5 %`*100,1),nsmall=1)), "%–",
                                trimws(format(round(`97.5 %`*100,1),nsmall=1)), "%)"),
           Condition = C) %>%
    select(A, Condition, Estimate_CI) %>%
    pivot_wider(names_from = "A", values_from = "Estimate_CI")

table3_agestandardised <- left_join(table3_agestandardised_overall, table3_agestandardised_bysex)

# By site -----------------------
# 
# 
adj <- svystandardize(survey_design, # Create the svystandardise object
                 by = ~ AgeBandLHE, over = ~ Sex_Use+SiteID,
                 population = pop$Pop)

est_ci <- NULL

# HIV
est <- svyby(~I(HIV_Status == "Yes"), ~Sex_Use+SiteID, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Diabetes
est <- svyby(~I(Diabetes_Status == "Yes"), ~Sex_Use+SiteID, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Hypertension
est <- svyby(~I(Hypertension_Status == "Yes"), ~Sex_Use+SiteID, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Anaemia
est <- svyby(~I(Anaemia_Status == "Yes"), ~Sex_Use+SiteID, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Underweight
est <- svyby(~I(BMI_Underweight == "Yes"), ~Sex_Use+SiteID, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Obesity (may include overweight - need to check)
est <- svyby(~I(BMI_Obese == "Yes"), ~Sex_Use+SiteID, svymean, design=adj)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)

# Chronic lung disease
est <- svyby(~I(ImpLungFn_Status == "Yes"), ~Sex_Use+SiteID, svymean, design=adj, na.rm=TRUE)

temp <- est %>%
    rownames_to_column("stratum") %>%
    select(-starts_with("se")) %>%
    pivot_longer(starts_with("I")) %>%
    mutate(var = paste0(stratum, ":", name)) %>%
    select(-stratum, -name)

est_ci <- confint(est, level = 0.95) %>%
    as.data.frame() %>%
    rownames_to_column("var") %>%
    left_join(temp) %>%
    filter(str_detect(var, "TRUE")) %>%
    rbind(est_ci, .)


table3_agestandardised_site <- est_ci %>%
    separate(var, into = c("A", "B", "C", "D", "E"), remove = F) %>%
    select(-C) %>%
    mutate(D = case_when(
        E == "UWStatus" ~ "Underweight",
        E == "ObsStatus" ~ "Overweight/Obese",
        E == "ImpLungFn" ~ "Chronic lung disease", 
        complete.cases(D) ~ D)) %>%
    mutate(Estimate_CI = paste0(trimws(format(round(value*100,1),nsmall=1)),
                                "% (", trimws(format(round(`2.5 %`*100,1),nsmall=1)), "%–",
                                trimws(format(round(`97.5 %`*100,1),nsmall=1)), "%)")
           ) %>%
    select(A, B, D, Estimate_CI) %>%
    pivot_wider(names_from = c("A", "B"), values_from = "Estimate_CI",
                names_glue = "{B}_{A}")

