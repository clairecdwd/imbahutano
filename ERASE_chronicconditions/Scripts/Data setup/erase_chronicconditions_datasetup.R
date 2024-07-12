# erase_collidinepidemics_datasetup

library(tidyverse)
library(haven)
library(readxl)
library(zscorer)

# Import Stata files from most recent version saved in data directory
erase_initial <- with_dir(
    dir = paste0(datadir, "/ERASE_OC/"),
    expr = read_dta(paste0(last_kumbox, "/DerA01_BaselinePerPerson.dta"))) %>%
    dplyr::mutate_if(haven::is.labelled, haven::as_factor)

source(paste0(projdir, "Scripts/Data setup/Generic/erase_subset.R"))

# Import Stata files from most recent version saved in data directory
A02 <- with_dir(
    dir = paste0(datadir, "/ERASE_OC/"),
    expr = read_dta(paste0(last_kumbox, "/DerA02_CompositePerVisit.dta"))) %>%
    dplyr::mutate_if(haven::is.labelled, haven::as_factor)

A02 %>%
    dplyr::mutate(vis = as.numeric(VisitNumUni)) %>%
    tabyl(vis, VisitNumUni)

# Add in MMRC results
last_mmrc <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb"),
    expr = list.dirs(full.names = F) %>% max())

# Select Hb
A02_PerVisit_MMRCHb <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB_663_HAEMATOLOGY SResult 23_06_08_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal"))

# Extract first results
source(paste0(projdir, "Scripts/Data setup/Generic/erase_firstresults.R"))

erase_df <- erase_df %>%
    left_join(., erase_firstresults) %>%
    dplyr::mutate(BP_Sys = ifelse(is.na(BP_Sys_Overall), BP_Sys, BP_Sys_Overall),
           BP_Dia = ifelse(is.na(BP_Dia_Overall), BP_Dia, BP_Dia_Overall),
           BP_Visit = ifelse(is.na(BP_Visit) & complete.cases(BP_Sys_Overall), "Visit-1", as.character(BP_Visit)),
           BP_Visit = factor(BP_Visit, levels = c("Visit-1", "Visit-2", "Visit-3", "Visit-4", "Visit-5", "Unwell-Visit")),
           HbA1c = ifelse(is.na(HbA1c_Overall), HbA1c, HbA1c_Overall))

# Source spiro file
source(paste0(projdir, "Scripts/Data setup/Generic/erase_spirometry.R"))

# Tidying of spiro data - set spiro reference in the main file
erase_df <- erase_spiro %>%
    filter(Reference == spiro_reference) %>%
    select(PersID, starts_with("pre"), starts_with("post")) %>%
    left_join(erase_df, .)

# erase_df %>%
#     dplyr::mutate(Pre_Done = ifelse(complete.cases(pre_FEV1_Z), "Yes", "No"),
#            Post_Done = ifelse(complete.cases(post_FEV1_Z), "Yes", "No")) %>%
#     tabyl(Pre_Done, Post_Done) %>%
#     adorn_totals("col") %>%
#     adorn_percentages("all") %>%
#     adorn_pct_formatting() %>%
#     adorn_ns("front")

# erase_df %>%
#     select(pre_Grade, post_Grade) %>%
#     dplyr::mutate(pre_Grade = ifelse(is.na(pre_Grade), "Not done", as.character(pre_Grade)),
#            post_Grade = ifelse(is.na(post_Grade), "Not done", as.character(post_Grade)),
#            post_Grade = factor(post_Grade, levels = c("Good quality", "Poor quality", "Not done")),
#            pre_Grade = factor(pre_Grade, levels = c("Good quality", "Poor quality", "Not done"))
#            ) %>%
#     tbl_summary()

notreference <- if (spiro_prepost == "pre") {"post_"} else {"pre_"}

erase_df <- erase_df %>%
    select(-starts_with(notreference)) %>%
    rename_with(~str_remove(.x, paste0(spiro_prepost, "_")), starts_with(spiro_prepost))

# Some cleaning
erase_df <- erase_df %>%
    dplyr::mutate(Weight = ifelse(Weight == 999, NA, Weight),
           HIV_Test = ifelse(is.na(HIV_Test) & Comorb_HIV == "No" & HIV_TestResult != "Unknown", as.character(HIV_TestResult), as.character(HIV_Test)),
           HIV_Test = factor(HIV_Test))

# Censor screening results for under 18 year olds
erase_df <- erase_df %>%
    dplyr::mutate(HbA1c = ifelse(Age_Baseline < 18, NA, HbA1c),
           BP_Sys = ifelse(Age_Baseline < 18, NA, BP_Sys),
           BP_Dia = ifelse(Age_Baseline < 18, NA, BP_Dia)
           )

# Filter to only HHC
erase_df <- erase_df %>%
    filter(IndexCase_Use == "Hhold contact") %>%
    dplyr::mutate(Site = case_when(
        SiteID == "INS" ~ "Mozambique",
        SiteID == "MMRC" ~ "Tanzania",
        SiteID == "BRTI" ~ "Zimbabwe"
    ))

erase_initial <- erase_initial %>%
     dplyr::mutate(Site = case_when(
        SiteID == "Ins" ~ "Mozambique",
        SiteID == "Mmrc" ~ "Tanzania",
        SiteID == "Brti" ~ "Zimbabwe"
    ))

# Create new variables for analysis
source(paste0(projdir, "Scripts/Data setup/Generic/erase_createvars.R"))

created_variables <- c("HIV_Status", "HIV_Category", "Diabetes_Status", "Hypertension_Status",
                       "BMI_Grade", "Stunting", "Anaemia", "Hhold_Underweight", "Hhold_UWOther",
                       "BMI_Obese",
                       "Hhold_StStatus", "Hhold_StOther", "Hhold_DBMStatus", "Hhold_HIVStatus",
                       "Hhold_HIVOther", "Hhold_HIVStatus_incIndx", "Hhold_HIVOther_incIndx",
                       "Hhold_NCDStatus")

# Filter to site if needed
if (sites == "Not MMRC") {
    sitefilter <- c("BRTI", "INS")
}

if (sites == "All") {
    sitefilter <- c("BRTI", "INS", "MMRC")
}

if (sites %in% c("BRTI", "INS", "MMRC")) {
    sitefilter <- sites
}

df_full <- erase_df %>%
    filter(SiteID %in% sitefilter)

df_full <- df_full %>%
    dplyr::mutate(completecase = case_when(
        Age_Baseline >= 18 & 
            (HIV_Test != "Not done" | Comorb_HIV == "Yes") &
            (complete.cases(HIV_CD4) | Comorb_HIV == "No" | HIV_Test == "Negative") & 
            complete.cases(HbA1c) & 
            complete.cases(BP_Sys) & 
            complete.cases(Weight) &
            complete.cases(Hb) ~ TRUE,
        Age_Baseline < 18 & 
            (HIV_Test != "Not done" | Comorb_HIV == "Yes") &
            (complete.cases(HIV_CD4) | Comorb_HIV == "No" | HIV_Test == "Negative") & 
            complete.cases(Weight) &
            complete.cases(Hb) ~ TRUE)) %>%
    dplyr::mutate(completecase = ifelse(is.na(completecase), FALSE, completecase),
           completemedhx = ifelse(complete.cases(Comorb_Diabetes) & complete.cases(Comorb_Hypertension),
                                  TRUE, FALSE))

# Redo sex categories

df_full <- df_full %>%
    mutate(Sex_Use = case_when(
        Sex_Use == "Female" ~ "Women",
        Sex_Use == "Male" ~ "Men"),
        Sex_Use = factor(Sex_Use, levels = c("Women", "Men")))

var_label(df_full) <- list(Sex_Use = "Sex")

# Make variables NA for people under the age of 18

df_full <- df_full %>%
    mutate(Diabetes_Status = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Diabetes_Status)),
           Diabetes_Category = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Diabetes_Category)),
           Diabetes_Grade = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Diabetes_Grade)),
           Hypertension_Status = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Hypertension_Status)),
           Hypertension_Category = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Hypertension_Category)),
           Hypertension_Grade = ifelse(Age_Baseline_Cat_CJC == "10-17 years", NA, as.character(Hypertension_Grade)),
           across(any_of(c("Diabetes_Status", "Hypertension_Status")), ~factor(.x, levels = c("No", "Yes"))),
           across(any_of(c("Diabetes_Category", "Hypertension_Category")), ~factor(.x, levels = c("None", "Known", "Screening detected"))),
           Hypertension_Grade = factor(Hypertension_Grade, levels = c("Normal BP",
                                                      "High-normal BP",
                                                      "Grade 1 hypertension",
                                                      "Grade 2 hypertension")),
           Diabetes_Grade = factor(Diabetes_Grade, levels = c("<6.0%",
                                                      "6.0-6.4%",
                                                      "6.5-6.9%",
                                                      "≥7.0%"))
    )


# Save data
 
dir_exists <- with_dir(dir = datadir, expr = dir.exists(file.path(paste0("ERASE_processed/", last_kumbox))))

if (dir_exists == FALSE) {
    with_dir(dir = datadir, expr = dir.create(file.path(paste0("ERASE_processed/", last_kumbox))))
}
  
saveRDS(df_full, paste0(datadir, "ERASE_processed/", last_kumbox, "/CollEpid", sites, "_", spiro_reference, spiro_prepost, ".Rds"))

df_full %>%
    foreign::write.dta(.,
    paste0(datadir, "ERASE_processed/", last_kumbox, "/CollEpid", sites, "_", spiro_reference, spiro_prepost, ".dta")
)