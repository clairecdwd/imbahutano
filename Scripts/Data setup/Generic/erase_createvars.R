# Create multimorbidity variables
# Written with ChatGPT to convert from Stata code

library(tidyverse)

# Check unique values of HIV_ART_Baseline
erase_df %>%
  dplyr::count(HIV_ART_Baseline) %>%
  print()

# Generate HIV_ART_CJC
erase_df <- erase_df %>%
  dplyr::mutate(HIV_ART_CJC = ifelse(HIV_ART_Baseline == "On ART", 1, 0)) %>%
  dplyr::mutate(HIV_ART_CJC = factor(HIV_ART_CJC, levels = c(0, 1), labels = c("No", "Yes")))

# Check the modified column
erase_df %>%
  dplyr::count(HIV_ART_CJC) %>%
  print()

# Create HIV_Status variable
erase_df <- erase_df %>%
  dplyr::mutate(HIV_Status = case_when(
    HIV_Test == "Negative" & Comorb_HIV == "No" ~ 0,
    HIV_Test == "Positive" | Comorb_HIV == "Yes" ~ 1
  )) %>%
  dplyr::mutate(HIV_Status = factor(HIV_Status, levels = c(0, 1), labels = c("No", "Yes")))

# Create HIV_Category variable
erase_df <- erase_df %>%
  dplyr::mutate(HIV_Category = case_when(
    HIV_Test == "Negative" & Comorb_HIV == "No" ~ 0,
    Comorb_HIV == "Yes" ~ 1,
    HIV_Test == "Positive" & Comorb_HIV == "No" ~ 2
  )) %>%
  dplyr::mutate(HIV_Category = factor(HIV_Category, levels = c(0, 1, 2), labels = c("None", "Known", "Screening detected")))

# Create HIV_Controlled variable
erase_df <- erase_df %>%
  dplyr::mutate(HIV_Controlled = case_when(
    HIV_CD4 < 350 & HIV_ART_CJC == "Yes" ~ 0,
    HIV_CD4 >= 350 & !is.na(HIV_CD4) & HIV_ART_CJC == "Yes" ~ 1
  )) %>%
  dplyr::mutate(HIV_Controlled = factor(HIV_Controlled, levels = c(0, 1), labels = c("No", "Yes")))

# Create HIV_CD4_Grade variable
erase_df <- erase_df %>%
  dplyr::mutate(HIV_CD4_Grade = case_when(
    HIV_CD4 >= 500 & !is.na(HIV_CD4) ~ 1,
    HIV_CD4 < 500 & HIV_CD4 >= 200 ~ 2,
    HIV_CD4 < 200 ~ 3
  )) %>%
  dplyr::mutate(HIV_CD4_Grade = factor(HIV_CD4_Grade, levels = c(1, 2, 3), labels = c("≥500cells/uL", "200-499cells/uL", "<200cells/uL")))

# Create Diabetes_Status variable
erase_df <- erase_df %>%
  dplyr::mutate(Diabetes_Status = case_when(
    HbA1c < 6.5 & Comorb_Diabetes == "No" ~ "No",
    HbA1c >= 6.5 | Comorb_Diabetes == "Yes" ~ "Yes"
  )) %>%
  dplyr::mutate(Diabetes_Status = factor(Diabetes_Status, levels = c("No", "Yes")))

# Create Diabetes_Category variable
erase_df <- erase_df %>%
  dplyr::mutate(Diabetes_Category = case_when(
    Diabetes_Status == "No" ~ 0,
    Comorb_Diabetes == "Yes" ~ 1,
    HbA1c >= 6.5 & Comorb_Diabetes == "No" ~ 2
  )) %>%
  dplyr::mutate(Diabetes_Category = factor(Diabetes_Category, levels = c(0, 1, 2), labels = c("None", "Known", "Screening detected")))

# Create Treated_Diabetes variable
erase_df <- erase_df %>%
  dplyr::mutate(Treated_Diabetes = ifelse(Comorb_Diabetes == "No", NA, Treated_Diabetes),
         Treated_Diabetes = factor(Treated_Diabetes, levels = c("No", "Yes")))

# Create Diabetes_Controlled variable
erase_df <- erase_df %>%
  dplyr::mutate(Diabetes_Controlled = case_when(
    HbA1c >= 6.5 & Treated_Diabetes == "Yes" ~ 0,
    HbA1c < 6.5 & Treated_Diabetes == "Yes" ~ 1
  )) %>%
  dplyr::mutate(Diabetes_Controlled = factor(Diabetes_Controlled, levels = c(0, 1), labels = c("No", "Yes")))

# Create Diabetes_Grade variable
erase_df <- erase_df %>%
  dplyr::mutate(Diabetes_Grade = case_when(
    HbA1c <= 5.9 ~ 0,
    HbA1c >= 6.0 & HbA1c < 6.5 ~ 1,
    HbA1c >= 6.5 & HbA1c < 7.0 ~ 2,
    HbA1c >= 7.0 & !is.na(HbA1c) ~ 3
  )) %>%
  dplyr::mutate(Diabetes_Grade = factor(Diabetes_Grade, levels = c(0, 1, 2, 3), labels = c("<6.0%", "6.0-6.4%", "6.5-6.9%", "≥7.0%")))

# Create Hypertension_Status variable
erase_df <- erase_df %>%
  dplyr::mutate(Hypertension_Status = case_when(
    BP_Sys < 140 & BP_Dia < 90 & Comorb_Hypertension == "No" ~ 0,
    (BP_Sys >= 140 & !is.na(BP_Sys)) | (BP_Dia >= 90 & !is.na(BP_Dia)) | Comorb_Hypertension == "Yes" ~ 1
  )) %>%
  dplyr::mutate(Hypertension_Status = factor(Hypertension_Status, levels = c(0, 1), labels = c("No", "Yes")))

# Create Hypertension_Category variable
erase_df <- erase_df %>%
  dplyr::mutate(Hypertension_Category = case_when(
    Hypertension_Status == "No" ~ 0,
    Comorb_Hypertension == "Yes" ~ 1,
    ((BP_Sys >= 140) | (BP_Dia >= 90)) & Comorb_Hypertension == "No" ~ 2
  )) %>%
  dplyr::mutate(Hypertension_Category = factor(Hypertension_Category, levels = c(0, 1, 2), labels = c("None", "Known", "Screening detected")))

# Create Treated_Hypertension variable
erase_df <- erase_df %>%
  dplyr::mutate(Treated_Hypertension = ifelse(Comorb_Hypertension == 0, NA, Treated_Hypertension),
         Treated_Hypertension = factor(Treated_Hypertension, levels = c("No", "Yes")))

# Create Hypertension_Controlled variable
erase_df <- erase_df %>%
  dplyr::mutate(Hypertension_Controlled = case_when(
    ((BP_Sys >= 140 & !is.na(BP_Sys)) | (BP_Dia >= 90 & !is.na(BP_Dia))) & Treated_Hypertension == "Yes" ~ 0,
    BP_Sys < 140 & BP_Dia < 90 & Treated_Hypertension == "Yes" ~ 1
  )) %>%
  dplyr::mutate(Hypertension_Controlled = factor(Hypertension_Controlled, levels = c(0, 1), labels = c("No", "Yes")))

# # Create Hypertension_Grade variable
# Now using Leyla's version
# erase_df <- erase_df %>%
#   dplyr::mutate(Hypertension_Grade = case_when(
#     BP_Sys < 130 & BP_Dia < 85 ~ 0,
#     (BP_Sys >= 130 & BP_Sys < 140) | (BP_Dia >= 85 & BP_Dia < 90) ~ 1,
#     (BP_Sys >= 140 & BP_Sys < 160) | (BP_Dia >= 90 & BP_Dia < 100) ~ 2,
#     (BP_Sys >= 160 & !is.na(BP_Sys)) | (BP_Dia >= 100 & !is.na(BP_Dia)) ~ 3
#   )) %>%
#   dplyr::mutate(Hypertension_Grade = factor(Hypertension_Grade, levels = c(0, 1, 2, 3), labels = c("Normal, SBP <130 / DBP<85", "Prehypertension, SBP 130-139 / DBP 85-89", "Grade 1 hypertension, SBP 140-159 / DBP 90-99", "Grade 2 hypertension, SBP ≥160 / DBP ≥100")))

erase_df <- erase_df %>%
    mutate(Hypertension_Grade = case_when(
        BP_Hypertension_Overall == "<130 SBP or <85 DBP, normal" ~ 0,
        BP_Hypertension_Overall == "130-139 SBP or 85-89 BDP, prehypertension" ~ 1,
        BP_Hypertension_Overall == "140-159 SBP or 90-99 DBP, stage 1 hypertension" ~ 2,
        BP_Hypertension_Overall == ">160 SBP or >100 DBP, stage 2 hypertension" ~ 3)) %>%
   dplyr::mutate(Hypertension_Grade = factor(Hypertension_Grade, levels = c(0, 1, 2, 3), labels = c("Normal BP", "High-normal BP", "Grade 1 hypertension", "Grade 2 hypertension"))) %>%
    dplyr::select(-BP_Hypertension_Overall)

# Calculate BMI
# 
# Load the necessary package
library(zscorer)

erase_df <- erase_df %>%
    dplyr::mutate(
        BMI = Weight / ((Height / 100) * (Height / 100)),
        Weight_Age = ifelse(complete.cases(Weight_Age), Weight_Age, Age_Baseline),
        sex = case_when(
        Sex_Use == "Male" ~ "1",
        Sex_Use == "Female" ~ "2"),
        sex = as.numeric(sex)) %>% # This is the format needed for the command to work
    dplyr::mutate(Age_days = Weight_Age*365.242199) %>%
    addWGSR(sex = "sex", firstPart = "Weight",
            secondPart = "Height", thirdPart = "Age_days",
            index = "bfa", output = "BMI_ZBA", digits = 3) %>%
    addWGSR(sex = "sex", firstPart = "Height",
            secondPart = "Age_days",
            index = "hfa", output = "BMI_ZHA")

# Define BMI categories for adolescents
erase_df <- erase_df %>%
  dplyr::mutate(BMI_Grade = case_when(
    Weight_Age < 19 & BMI_ZBA < -2 ~ 0, # Moderate/severe underweight
    Weight_Age >= 19 & BMI < 17 ~ 0,
    Weight_Age < 19 & BMI_ZBA >= -2 & BMI_ZBA < -1 ~ 1, # Mild underweight
    Weight_Age >= 19 & BMI >= 17 & BMI < 18.5 ~ 1,
    Weight_Age < 19 & BMI_ZBA >= -1 & BMI_ZBA <= 1 ~ 2, # Healthy weight
    Weight_Age >= 19 & BMI >= 18.5 & BMI < 25 ~ 2,
    Weight_Age < 19 & BMI_ZBA > 1 & BMI_ZBA <= 2 ~ 3, # Overweight
    Weight_Age >= 19 & BMI >= 25 & BMI < 30 ~ 3,
    Weight_Age < 19 & BMI_ZBA > 2 ~ 4, # Obese
    Weight_Age >= 19 & BMI >= 30 ~ 4
  ))

# Label BMI categories
erase_df$BMI_Grade <- factor(erase_df$BMI_Grade, levels = 0:4, labels = c("Moderate/severe underweight", "Mild underweight", "Healthy weight", "Overweight", "Obese"))

# Create Stunting variable
erase_df <- erase_df %>%
  dplyr::mutate(Stunting = case_when(
    Weight_Age < 19 & BMI_ZHA < -3 ~ 3, # Severe stunting
    Weight_Age < 19 & BMI_ZHA >= -3 & BMI_ZHA < -2 ~ 2, # Moderate stunting
    Weight_Age < 19 & BMI_ZHA >= -2 & BMI_ZHA < -1 ~ 1, # Mild stunting
    Weight_Age < 19 & BMI_ZHA >= -1 ~ 0, # Normal
  ),
  Stunting = ifelse(Age_Baseline >= 18, NA, Stunting))

# Label Stunting categories
erase_df$Stunting <- factor(erase_df$Stunting, levels = 0:3, labels = c("Normal", "Mild stunting", "Moderate stunting", "Severe stunting"))

# Create Stunting_Status variable
erase_df$Stunting_Status <- ifelse(erase_df$Stunting >= 2, 1, 0)

# Create BMI_UWStatus variable
erase_df <- erase_df %>%
    dplyr::mutate(BMI_Underweight = ifelse(BMI_Grade %in% 
                                        c("Moderate/severe underweight", "Mild underweight"), "Yes", "No"),
           BMI_Underweight = factor(BMI_Underweight, levels = c("No", "Yes")),
           BMI_Obese = ifelse(BMI_Grade %in% 
                                        c("Overweight", "Obese"), "Yes", "No"),
           BMI_Obese = factor(BMI_Obese, levels = c("No", "Yes"))
)

## Anaemia
## 
# Convert Hb from g/dL to g/L
erase_df$Hb <- erase_df$Hb * 10

# Create Anaemia variable
erase_df <- erase_df %>%
  dplyr::mutate(Anaemia_Grade = case_when(
    Hb >= 115 & Age_Baseline < 12 ~ "None",
    Hb >= 120 & Age_Baseline >= 12 & Age_Baseline < 15 ~ "None",
    Hb >= 120 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "No" ~ "None",
    Hb >= 110 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "Yes" ~ "None",
    Hb >= 130 & Age_Baseline >= 15 & Sex_Use == "Male" ~ "None",
    Hb >= 110 & Hb < 115 & Age_Baseline < 12 ~ "Mild anaemia",
    Hb >= 110 & Hb < 120 & Age_Baseline >= 12 & Age_Baseline < 15 ~ "Mild anaemia",
    Hb >= 110 & Hb < 120 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "No" ~ "Mild anaemia",
    Hb >= 100 & Hb < 110 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "Yes" ~ "Mild anaemia",
    Hb >= 110 & Hb < 130 & Age_Baseline >= 15 & Sex_Use == "Male" ~ "Mild anaemia",
    Hb >= 80 & Hb < 110 & Age_Baseline < 12 ~ "Moderate anaemia",
    Hb >= 80 & Hb < 110 & Age_Baseline >= 12 & Age_Baseline < 15 ~ "Moderate anaemia",
    Hb >= 80 & Hb < 110 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "No" ~ "Moderate anaemia",
    Hb >= 70 & Hb < 100 & Age_Baseline >= 15 & Sex_Use == "Female" & Pregnancy_Status_Baseline == "Yes" ~ "Moderate anaemia",
    Hb >= 80 & Hb < 110 & Age_Baseline >= 15 & Sex_Use == "Male" ~ "Moderate anaemia",
    Hb < 80 & (Pregnancy_Status_Baseline == "No" | Sex_Use == "Male") ~ "Severe anaemia",
    Hb < 70 & Pregnancy_Status_Baseline == "Yes" ~ "Severe anaemia"
  )) %>%
  dplyr::mutate(Anaemia_Grade = factor(Anaemia_Grade, levels = c("None", "Mild anaemia", "Moderate anaemia", "Severe anaemia")),
         Anaemia_Status = ifelse(Anaemia_Grade == "None", "No", "Yes"),
         Anaemia_Status = factor(Anaemia_Status, levels = c("No", "Yes")))

# Chronic lung disease

erase_df <- erase_df %>%
    dplyr::mutate(ImpLungFn_Status = ifelse(ImpLungFn_Type %in% c("Obstruction", "Mixed") | 
                                         Comorb_LungDis_Asthma == "Yes" | 
                                         Comorb_LungDis_Silicosis == "Yes",
                                     "Yes", "No"),
           ImpLungFn_Status = factor(ImpLungFn_Status, levels = c("No", "Yes")))

# Household level variables
erase_df <- erase_df %>%
  group_by(HholdID) %>%
  dplyr::mutate(
    Hhold_UWStatus = sum(str_detect(BMI_Grade, "underweight"), na.rm = T),
    Hhold_UWOther = Hhold_UWStatus - str_detect(BMI_Grade, "underweight"),
    Hhold_UWStatus = ifelse(Hhold_UWStatus > 1, 1, Hhold_UWStatus),
    Hhold_UWOther = ifelse(Hhold_UWOther > 1, 1, Hhold_UWOther),
    Hhold_StStatus = sum(Stunting == "Moderate stunting" | Stunting == "Severe stunting", na.rm =T),
    Hhold_StOther = Hhold_StStatus - (Stunting == "Moderate stunting" | Stunting == "Severe stunting"),
    Hhold_StStatus = ifelse(Hhold_StStatus > 1, 1, Hhold_StStatus),
    Hhold_StOther = ifelse(Hhold_StOther > 1, 1, Hhold_StOther),
    BMI_UwS = ifelse((str_detect(BMI_Grade, "underweight") & is.na(Stunting)) | 
                         str_detect(BMI_Grade, "underweight") & (Stunting == "Moderate stunting" | Stunting == "Severe stunting"), "Yes", "No"),
    Hhold_UwSStatus = sum(BMI_UwS == "Yes", na.rm = T),
    Hhold_UwSStatus = ifelse(Hhold_UwSStatus > 0, 1, 0),
    Hhold_ObsStatus = sum((BMI_Grade == "Obese" | BMI_Grade == "Overweight") , na.rm = T),
    Hhold_ObsOther = Hhold_ObsStatus - (BMI_Grade == "Obese" | BMI_Grade == "Overweight"),
    Hhold_ObsStatus = ifelse(Hhold_ObsStatus > 0, 1, 0),
    Hhold_ObsOther = ifelse(Hhold_ObsOther > 0, 1, 0),
    Hhold_DBMStatus = ifelse(Hhold_UwSStatus > 0 & Hhold_ObsStatus > 0, 1, 0),
    
    Hhold_HIVStatus = sum(HIV_Status == "Yes", na.rm = T),
    Hhold_HIVOther = Hhold_HIVStatus - (HIV_Status == "Yes"),
    Hhold_HIVStatus = ifelse(Hhold_HIVStatus > 0, 1, 0),
    Hhold_HIVOther = ifelse(Hhold_HIVOther > 0, 1, 0),
    
    Hhold_HIVStatus_incIndx = sum(HIV_Status == "Yes" | str_detect(HIV_IndexStatus, "Positive"), na.rm = T),
    Hhold_HIVOther_incIndx = Hhold_HIVStatus_incIndx - (HIV_Status == "Yes" | str_detect(HIV_IndexStatus, "Positive")),
    Hhold_HIVStatus_incIndx = ifelse(Hhold_HIVStatus_incIndx > 0, 1, 0),
    Hhold_HIVOther_incIndx = ifelse(Hhold_HIVOther_incIndx > 0, 1, 0),
    Hhold_NCDStatus = sum(Diabetes_Status == "Yes" | Hypertension_Status == "Yes", na.rm = T),
    Hhold_NCDStatus = ifelse(Hhold_NCDStatus > 0, 1, 0),
    Hhold_DiabStatus = sum(Diabetes_Status == "Yes", na.rm = T),
    Hhold_DiabOther = Hhold_DiabStatus - (Diabetes_Status == "Yes"),
    Hhold_DiabStatus = ifelse(Hhold_DiabStatus > 0, 1, 0),
    Hhold_DiabOther = ifelse(Hhold_DiabOther > 0, 1, 0),
    Hhold_HypertensionStatus = sum(Hypertension_Status == "Yes", na.rm = T),
    Hhold_HypertensionOther = Hhold_HypertensionStatus - (Hypertension_Status == "Yes"),
    Hhold_HypertensionStatus = ifelse(Hhold_HypertensionStatus > 0, 1, 0),
    Hhold_HypertensionOther = ifelse(Hhold_HypertensionOther > 0, 1, 0)
  ) %>%
  ungroup()

erase_df <- erase_df %>%
        dplyr::mutate(TB_Status = ifelse(Study_Outcome_BLTB %in% c("TB", "Subclinical TB"), "Yes", "No"),
           TB_Status = factor(TB_Status))

# Label variables

var_label(erase_df) <- list(
    HIV_Status = "HIV",
    HIV_Category = "HIV",
    HIV_ART_CJC = "On ART",
    Diabetes_Status = "Diabetes",
    Diabetes_Category = "Diabetes",
    Hypertension_Status = "Hypertension",
    Hypertension_Category = "Hypertension",
    Diabetes_Grade = "Diabetes",
    Hypertension_Grade = "Hypertension",
    HIV_CD4 = "CD4 count / cells/uL",
     HIV_CD4_Grade = "CD4 category",
    HbA1c = "HbA1c / %",
    Diabetes_Grade = "HbA1c category",
    BP_Sys = "Systolic BP / mmHg",
    BP_Dia = "Diastolic BP / mmHg",
    Hypertension_Grade = "BP category",
    BMI_Grade = "BMI category",
    BMI = "BMI / kg/m2 (≥18 years)",
    BMI_ZBA = "BMI for age Z score (<18 years)",
    BMI_ZHA = "Height for age Z score (<18 years)",
    Hb = "Haemoglobin / g/L",
    Anaemia_Grade = "Anaemia category",
    Anaemia_Status = "Anaemia",
    Stunting = "Height for age category (<18 years)",
    TB_Status = "Coprevalent TB")



