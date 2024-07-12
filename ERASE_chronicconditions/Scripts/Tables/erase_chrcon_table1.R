# Making table 1

# Comorbidities with prevalence of more than 1%
temp <- df_analysis %>%
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

footer_comorbs <- paste0("Participants were asked about a history of the following conditions: asthma, cardiac disease, chronic kidney disease, diabetes, HIV, hypertension, lung cancer, mental health conditions, pneumonia, silicosis, TB, traumatic chest injuries. Conditions with prevalence of at least 1% are shown.")

# Make the table
table_pp_df <- df_analysis %>%
    dplyr::select(
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
        #Comorb_HIV,
        #HIV_ART_CJC,
        #any_of(comorbs),
        Index_Relation
        )

table_hh_df <- df_analysis %>%
    distinct(HholdID, .keep_all = T) %>%
    select(Site,
           Hhold_Total_People,
           #Hhold_Crowding_PPR,
           Hhold_Crowding_UN,
           Hhold_Income_Day,
           Hhold_IndexBreadwinner,
           Hhold_Poverty,
           Hhold_ResidenceArea,
           HIV_IndexStatus)

missing_educ <- paste0("Educational level not known for ", df_analysis %>% filter(is.na(Education)) %>% nrow(), " participants.")

missing_income <- paste0("Income not known for ", df_analysis %>%
    distinct(HholdID, .keep_all = T) %>% filter(is.na(Hhold_Income_Day)) %>% nrow(), " households.")

missing_indexHIV <- paste0("HIV status not known for ", df_analysis %>%
    distinct(HholdID, .keep_all = T) %>% filter(is.na(HIV_IndexStatus)) %>% nrow(), " people with TB.")

note_relations <- paste0("Other relations included aunt/uncle (10%) cousins (3%), niece/nephews (3%), grandparents (2%) and others.")

# By site
tab1_demsbysite <- table_pp_df %>%
    tbl_summary(by ="Site",
                statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                 all_categorical() ~ "{n} ({p}%)"),
                missing = "no") %>%
    add_overall() %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)") %>%
    as_flex_table() %>%
    #add_footer_lines(footer_comorbs) %>%
    add_footer_lines(missing_educ) %>%
    add_footer_lines(note_relations) %>%
    set_table_properties(layout = "autofit")


tab1_hhcbysite <- table_hh_df %>%
    tbl_summary(by = Site,
                statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = list("Hhold_Total_People" ~ 0),
                missing = "no") %>%
    add_overall() %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)") %>%
    as_flex_table() %>%
    set_caption(caption = "Characteristics of study households") %>%
    add_footer_lines("Crowding using UN definition (≥3 people per room).") %>%
    add_footer_lines(missing_income) %>%
    add_footer_lines(missing_indexHIV) %>%
    set_table_properties(layout = "autofit") 

# By age
tab1_demsbyage <- table_pp_df %>%
    tbl_summary(by =Age_Baseline_Cat_CJC,
                statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                 all_categorical() ~ "{n} ({p}%)")) %>%
    add_overall() %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)") %>%
    as_flex_table() %>%
    add_footer_lines(footer_comorbs) %>%
    add_footer_lines(missing_educ) %>%
    set_table_properties(layout = "autofit")

# By sex
tab1_demsbysex <- table_pp_df %>%
    tbl_summary(by =Sex_Use,
                statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                 all_categorical() ~ "{n} ({p}%)")) %>%
    add_overall() %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)") %>%
    as_flex_table() %>%
    add_footer_lines(footer_comorbs) %>%
    add_footer_lines(missing_educ) %>%
    set_table_properties(layout = "autofit")

# By sex and site
tab1_demsbysexsite <- table_pp_df %>% 
    gtsummary::tbl_strata(strata = Site,
                          .tbl_fun = ~ .x %>%
                              tbl_summary(by = Sex_Use,
                                          statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                                           all_categorical() ~ "{n} ({p}%)"),
                                          missing = "no") %>%
                              bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)"),
                          .header = "**{strata}**, N = {n}"
  ) %>%
    as_flex_table() %>%
    add_footer_lines(missing_educ) %>%
    set_table_properties(layout = "autofit")

tab1_demsbysite <- table_pp_df %>%
    tbl_summary(by ="Site",
                statistic = list(all_continuous() ~ "{median} ({p25}–{p75})",
                                 all_categorical() ~ "{n} ({p}%)"),
                missing = "no") %>%
    add_overall() %>%
    bold_labels() %>%
    modify_footnote(all_stat_cols() ~ "n (%); median (interquartile range)") %>%
    as_flex_table() %>%
    #add_footer_lines(footer_comorbs) %>%
    add_footer_lines(missing_educ) %>%
    set_table_properties(layout = "autofit")


# Exploring missingness in household income
erase_initial %>%
    filter(IndexCase_Use == "IndexCase") %>%
    select(Hhold_Income_Day, Q_210_QKi, Q_210_QKi_USD, Q_705_QIc) %>%
    mutate(KI_byindex = complete.cases(Q_210_QKi_USD)) %>%
    tabyl(KI_byindex)

erase_initial %>%
    filter(IndexCase_Use == "IndexCase") %>%
    select(Hhold_Income_Day, Q_210_QKi, Q_210_QKi_USD, Q_705_QIc) %>%
    mutate(KI_byindex = complete.cases(Q_210_QKi_USD)) %>%
    filter(is.na(Hhold_Income_Day)) %>%
    tabyl(Q_705_QIc)
    
erase_initial %>%
    select(HholdID, Hhold_Income_Day, Q_210_QKi, Q_210_QKi_USD, Q_705_QIc) %>%
    arrange(Q_210_QKi) %>%
    distinct(HholdID, .keep_all = T) %>%
    mutate(Hhold_Income_complete = case_when(
        Q_210_QKi == "Don't know/Not willing to disclose" ~ "Not known",
        is.na(Q_210_QKi) ~ "Missing",
        complete.cases(Q_210_QKi) ~ "Complete")
    ) %>%
    tabyl(Hhold_Income_complete)

# Missingness in HIV index case status
temp <- erase_initial %>%
    filter(IndexCase_Use == "IndexCase") %>%
    select(Q_303_QIc, HIV_IndexStatus)

