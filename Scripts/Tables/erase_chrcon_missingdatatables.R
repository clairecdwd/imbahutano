
# Missing data by age

df_full %>%
    tabyl(completecase, completemedhx)

temp <- df_full %>%
    select(
        Age_Baseline,
        SiteID,
        HIV_test = HIV_Test,
        CD4_count = HIV_CD4,
        Known_HIV = Comorb_HIV,
        Spiro = ImpLungFn_Type,
        HbA1c,
        BP = BP_Sys,
        BMI = Weight,
        Hb
        ) %>%
    mutate(across(-Age_Baseline, ~ifelse(complete.cases(.x), "Yes", "No"),
                  .names = "{.col}_done")) %>%
    mutate(BP_done = ifelse(Age_Baseline < 18, "Not applicable", BP_done),
           HbA1c_done = ifelse(Age_Baseline < 18, "Not applicable", HbA1c_done)) %>%
    mutate(CD4_count_done = case_when(HIV_test == "Negative" & Known_HIV == "No" ~ "Not applicable",
                                      complete.cases(CD4_count) ~ "Yes",
                                      is.na(CD4_count) & (HIV_test == "Positive" | Known_HIV == "Yes") ~ "No"),
           HIV_test_done = ifelse(Known_HIV == "Yes", "Not applicable", HIV_test_done)) %>%
    mutate(across(all_of(c("CD4_count_done", "HIV_test_done")), ~factor(.x, levels = c("No", "Yes", "Not applicable")))) %>%
    select(SiteID, ends_with("done"), -SiteID_done, -Known_HIV_done) %>%
    rename_with(., ~str_replace_all(.x, "_", " ")) 

supptab2_missingresults <- temp %>%
    tbl_summary(
        by = "SiteID",
        type = list(all_categorical() ~ "categorical")) %>%
    add_overall() %>%
    bold_labels() %>%
    as_flex_table() %>%
    add_footer_lines("HIV test is not applicable if the person self-reported that they were living with HIV. HbA1c and BP are not applicable if the person is under the age of 18 at recruitment. CD4 is unknown where the HIV status is missing, therefore whether a CD4 count is needed is missing. HbA1c and BP are unknown where age is missing. Spiro done means that there is a spirometry result meeting ERS/ATS quality criteria available") %>%
    set_table_properties(layout = "autofit")

# Notes -------

temp %>%
    tabyl(`HIV test done`, `CD4 count done`)
# 3 are HIV test done "no" but CD4 done "yes" which seems odd
# I could assume they have HIV but will leave as a query to be resolved for now

df_full %>%
    filter(is.na(HIV_Status)) %>%
    filter(complete.cases(HIV_CD4)) %>%
    select(PersID, HIV_CD4)
    # Where a CD4 is present but the HIV status is missing

# Which visit were tests done at

supptab3_visitwresults <- df_analysis %>%
    select(
        Site,
        HIV_Visit,
        HIV_CD4_Visit,
        HbA1c_Visit,
        BP_Visit,
        Weight_Visit,
        Hb_Visit) %>%
    droplevels() %>%
    rename_with(., ~str_replace_all(.x, "_", " ")) %>%
    rename_with(., ~str_replace_all(.x, " V", " v")) %>%
    mutate(across(everything(), ~ifelse(.x %in% c("Visit-3", "Visit-4", "Visit-5", "Unwell-Visit"), "Visit-3-5", as.character(.x)))) %>%
    tbl_summary(by = "Site",
                missing = "no") %>%
    add_overall() %>%
    bold_labels() %>%
    as_flex_table() %>%
    set_table_properties(layout = "autofit")

# Compare included and excluded participants

# Comorbidities with prevalence of more than 1%
temp <- df_full %>%
    select(Comorb_TBEver,
        Comorb_Hypertension,
        Comorb_Diabetes,
        Comorb_LungDis_Any,
        Comorb_LungDis_Asthma,
        Comorb_LungDis_LungCancer,
        Comorb_LungDis_ChestInj,
        Comorb_LungDis_COVID19,
        Comorb_LungDis_Pneumonia,
        Comorb_LungDis_Silicosis,
        Comorb_CVD,
        Comorb_CKD,
        Comorb_MentalHealth,
        #HistoryMining,
        #HistoryPrison
        ) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(everything()) %>%
    filter(value == "Yes") %>%
    group_by(name) %>%
    dplyr::summarize(N = n()) %>%
    mutate(perc = N/nrow(df_analysis)) %>%
    filter(perc >= 0.01) 

temp

comorbs <- temp %>%
    select(name) %>%
    distinct() %>%
    pull()

supptab4_incvsexc <- df_full %>%
    mutate(include = ifelse(completecase == T & completemedhx == T, "Included", "Excluded")) %>%
    dplyr::select(
        include,
        Site,
        Sex_Use,
        Age_Baseline,
        Age_Baseline_Cat_CJC,
        Education,
        Pregnancy_Status_Baseline,
        Smoking_Cat_Bin,
        Smoking_PackYears,
        AUDIT_Score_Cat_CJC,
        InsufficientFood,
        Comorb_HIV,
        HIV_ART_CJC,
        any_of(comorbs),
        Index_Relation
        ) %>%
    gtsummary::tbl_strata(strata = Site,
                          .tbl_fun = ~ .x %>%
                              tbl_summary(by = include,
                                          statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                                           all_categorical() ~ "{n} ({p}%)"),
                                          type = list(all_dichotomous() ~ "categorical"),
                                          percent = "col",
                                          missing = "no") %>%
                              bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)"),
                          .header = "**{strata}**, N = {n}"
  ) %>%
    as_flex_table() %>%
    set_table_properties(layout = "autofit")
