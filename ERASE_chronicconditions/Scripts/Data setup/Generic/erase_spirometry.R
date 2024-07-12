
# Spirometry predicted values

library(rspiro)
library(tidyverse)
library(readr)
library(haven)
library(ggpubr)
library(janitor)
library(zscorer)

# Import Stata files from most recent version saved in data directory
A02 <- with_dir(
    dir = paste0(datadir, "/ERASE_OC/"),
    expr = read_dta(paste0(last_kumbox, "/DerA02_CompositePerVisit.dta"))) %>%
    dplyr::mutate_if(haven::is.labelled, haven::as_factor)


# ethnicity Ethnicity (1 = Caucasian, 2 = African-American, 3 = NE Asian, 4 = SE Asian, 5 = Other/mixed). Default is 1.
# https://erj.ersjournals.com/content/erj/early/2021/12/16/13993003.01499-2021.full.pdf


erase_spiro_initial <- A02 %>%
    dplyr::mutate(Diff = time_length(difftime(VisitDate_Spr, EnrolDate_Use), "years"),
           Diff = ifelse(Diff < 0 , 0 , Diff),
           Age = Age_Baseline + Diff,
           Sex = ifelse(Sex_Use %in% c("Male", "male", "Men", "men"), 1, 2),
           Height = as.numeric(as.character(Q_302_Exm))/100,
           ethnicity = 1,
           # Assume that if FEV1 is more than FVC, then the ratio should be 1
           Fev1ByFvcPre_Spr = ifelse(Fev1ByFvcPre_Spr > 1, 1, Fev1ByFvcPre_Spr),
           Fev1ByFvcPost_Spr = ifelse(Fev1ByFvcPost_Spr > 1, 1, Fev1ByFvcPost_Spr),
           Q_202_Spr = ifelse(Q_202_Spr == "", NA, Q_202_Spr),
           Q_302_Spr = ifelse(Q_302_Spr == "", NA, Q_302_Spr),
           pre_Grade = ifelse(Q_202_Spr %in% c("A", "B", "C"), "Good quality", "Poor quality"),
           pre_Grade = case_when(is.na(Q_202_Spr) & Q_201_Spr == "No" ~ "Not useable",
                              is.na(Q_202_Spr) & Q_201_Spr == "Yes" ~ "Useable, no grade",
                              is.na(Q_202_Spr) & is.na(Q_201_Spr) ~ "Not done",
                              complete.cases(pre_Grade) ~ pre_Grade),
           post_Grade = ifelse(Q_302_Spr %in% c("A", "B", "C"), "Good quality", "Poor quality"),
           post_Grade = case_when(is.na(Q_302_Spr) & Q_301_Spr == "No" ~ "Not useable",
                              is.na(Q_302_Spr) & Q_301_Spr == "Yes" ~ "Useable, no grade",
                              is.na(Q_302_Spr) & (is.na(Q_301_Spr) | Q_105_Spr == "No") ~ "Not done",
                              complete.cases(post_Grade) ~ post_Grade),
           post_Grade = factor(post_Grade, levels = c("Good quality", "Poor quality", "Useable, no grade", "Not useable", "Not done")),
           pre_Grade = factor(pre_Grade, levels = c("Good quality", "Poor quality", "Useable, no grade", "Not useable", "Not done"))
           ) %>%
    filter(complete.cases(Height))

erase_spiro <- erase_spiro_initial

erase_spiro <- erase_spiro %>%
    dplyr::mutate(
        # African American reference standard
        FEV1_pre_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Pre_Spr),
        FVC_pre_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FVC = erase_spiro$FvcPre_Spr),
        FEV1FVC_pre_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPre_Spr),
        FEV1_post_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Post_Spr),
        FVC_post_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FVC = erase_spiro$FvcPost_Spr),
        FEV1FVC_post_Z_AfrAm = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(2,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPost_Spr),
        # Other reference standard
        FEV1_pre_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Pre_Spr),
        FVC_pre_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FVC = erase_spiro$FvcPre_Spr),
        FEV1FVC_pre_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPre_Spr),
        FEV1_post_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Post_Spr),
        FVC_post_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FVC = erase_spiro$FvcPost_Spr),
        FEV1FVC_post_Z_Other = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(5,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPost_Spr),
        # Race neutral reference standard
        FEV1_pre_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FEV1 = erase_spiro$Fev1Pre_Spr),
        FVC_pre_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FVC = erase_spiro$FvcPre_Spr),
        FEV1FVC_pre_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FEV1FVC = erase_spiro$Fev1ByFvcPre_Spr),
        FEV1_post_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FEV1 = erase_spiro$Fev1Post_Spr),
        FVC_post_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FVC = erase_spiro$FvcPost_Spr),
        FEV1FVC_post_Z_Neutral = zscore_GLIgl(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, FEV1FVC = erase_spiro$Fev1ByFvcPost_Spr),
        # Caucasian reference standard
        FEV1_pre_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Pre_Spr),
        FVC_pre_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FVC = erase_spiro$FvcPre_Spr),
        FEV1FVC_pre_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPre_Spr),
        FEV1_post_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FEV1 = erase_spiro$Fev1Post_Spr),
        FVC_post_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FVC = erase_spiro$FvcPost_Spr),
        FEV1FVC_post_Z_Caucasian = zscore_GLI(age = erase_spiro$Age, height = erase_spiro$Height,
                               gender = erase_spiro$Sex, ethnicity = rep(1,length(erase_spiro$Age)), FEV1FVC = erase_spiro$Fev1ByFvcPost_Spr),
           )

erase_spiro <- erase_spiro %>%
    select(PersID, SiteID, ends_with("AfrAm"), ends_with("Other"), ends_with("Neutral"), ends_with("Caucasian"), pre_Grade, post_Grade,
           Sex_Use, Age, Height, Weight = Q_301_Exm, HIV_Overall, BMI_Adult, BMI_ZBA_Child, BMI_ZHA_Child, BMI_ZWA_Child, 
           ENum, VisitDateMod,
           Fev1Pre_Spr, FvcPre_Spr, Fev1ByFvcPre_Spr, Fev1Post_Spr, FvcPost_Spr, Fev1ByFvcPost_Spr) %>%
    dplyr::mutate(BMI_Adult = as.numeric(as.character(BMI_Adult)),
           BMI_ZBA_Child = as.numeric(as.character(BMI_ZBA_Child)),
           BMI_ZHA_Child = as.numeric(as.character(BMI_ZHA_Child)),
           BMI_ZWA_Child = as.numeric(as.character(BMI_ZWA_Child)),
           Weight = as.numeric(as.character(Weight))) %>%
    select(-starts_with("Comorb")) %>%
    pivot_longer(cols = c(starts_with("FEV1_"), starts_with("FVC_"), starts_with("FEV1FVC_")),
                 names_to = c("Measure", "PrePost", "Z", "Reference"),
                 names_pattern = "(.*)_(.*)_(.*)_(.*)") %>%
    dplyr::mutate(Grade = case_when(
        value > -1.65 ~ "None",
        value <= -1.65 & value >= -2.5 ~ "Mild",
        value < -2.5 & value >= -4.0 ~ "Moderate",
        value < -4.0 ~ "Severe"),
        lesslln = ifelse(value < -1.645, "Impaired", "None"),
        Grade = factor(Grade, levels = c("None", "Mild", "Moderate", "Severe"))
    ) %>%
    select(-Z) %>%
    dplyr::rename(Z = value) %>%
    pivot_wider(id_cols = c(PersID, SiteID, Reference, pre_Grade, post_Grade, Sex_Use, Age, Height, Weight, HIV_Overall, 
                            BMI_Adult, BMI_ZBA_Child, BMI_ZHA_Child, BMI_ZWA_Child , ENum, VisitDateMod,
                            Fev1Pre_Spr, FvcPre_Spr, Fev1ByFvcPre_Spr, Fev1Post_Spr, FvcPost_Spr, Fev1ByFvcPost_Spr),
                names_from = c(Measure, PrePost),
                values_from = c(Z, Grade, lesslln),
                names_glue = "{PrePost}_{Measure}_{.value}") %>%
    dplyr::mutate(post_ImpLungFn_Status = case_when(
            post_FEV1FVC_lesslln == "Impaired" ~ "Yes",
            post_FVC_lesslln == "Impaired" ~ "Yes",
            post_FEV1FVC_lesslln == "None" & post_FVC_lesslln == "None" ~ "No"),
           post_ImpLungFn_Status = factor(post_ImpLungFn_Status, levels = c("No", "Yes")),
        pre_ImpLungFn_Status = case_when(
            pre_FEV1FVC_lesslln == "Impaired" ~ "Yes",
            pre_FVC_lesslln == "Impaired" ~ "Yes",
            pre_FEV1FVC_lesslln == "None" & pre_FVC_lesslln == "None" ~ "No"),
           pre_ImpLungFn_Status = factor(pre_ImpLungFn_Status, levels = c("No", "Yes")),
        pre_ImpLungFn_Type = case_when(
            pre_FEV1FVC_lesslln == "Impaired" ~ "Obstruction",
            pre_FVC_lesslln == "Impaired" & pre_FEV1FVC_lesslln == "None" ~ "PRISm",
            pre_FEV1FVC_lesslln == "None" & pre_FVC_lesslln == "None" ~ "None"),
        post_ImpLungFn_Type = case_when(
            post_FEV1FVC_lesslln == "Impaired" ~ "Obstruction",
            post_FVC_lesslln == "Impaired" & post_FEV1FVC_lesslln == "None" ~ "PRISm",
            post_FEV1FVC_lesslln == "None" & post_FVC_lesslln == "None" ~ "None"),
        post_ImpLungFn_Grade = case_when(
            post_ImpLungFn_Type == "Obstruction" ~ post_FEV1FVC_Grade,
            post_ImpLungFn_Type == "PRISm" ~ post_FVC_Grade),
        pre_ImpLungFn_Grade = case_when(
            pre_ImpLungFn_Type == "Obstruction" ~ pre_FEV1FVC_Grade,
            pre_ImpLungFn_Type == "PRISm" ~ pre_FVC_Grade)
        )

## Add in BMI z scores for all

temp <- erase_spiro %>%
    select(PersID, Sex_Use, Age, Weight, Height) %>%
    filter(complete.cases(Age)) %>%
    dplyr::mutate(sex = ifelse(Sex_Use %in% c("Male", "male", "Men", "men"), 1, 2),
        Age = ifelse(Age > 18.9, 18.9, Age)) %>% # This is the format needed for the command to work
    dplyr::mutate(Age_days = Age*365.25,
           Height_cm = Height*100) %>%
    distinct(PersID, .keep_all = T) %>%
    addWGSR(sex = "sex", firstPart = "Weight",
            secondPart = "Height_cm", thirdPart = "Age_days",
            index = "bfa", output = "ZFA_BMI") %>%
    dplyr::mutate(ZFA_BMI_Cat = case_when(
        ZFA_BMI >= -1 & ZFA_BMI <= 1 ~ "+-1SD",
        ZFA_BMI < -1 & ZFA_BMI >= -2 ~ "<-1SD & ≥-2SD",
        ZFA_BMI < -2 ~ "<-2SD",
        ZFA_BMI > 1 & ZFA_BMI <= 2 ~ ">1SD & ≤2SD",
        ZFA_BMI > 2 ~ ">2SD"),
        ZFA_BMI_Cat = factor(ZFA_BMI_Cat, levels = c("<-2SD", "<-1SD & ≥-2SD", "+-1SD", ">1SD & ≤2SD", ">2SD"))) %>%
    addWGSR(sex = "sex", firstPart = "Height_cm",
            secondPart = "Age_days",
            index = "hfa", output = "ZFA_Height") %>%
    dplyr::mutate(ZFA_Height_Cat = case_when(
        ZFA_Height >= -1 & ZFA_Height <= 1 ~ "+-1SD",
        ZFA_Height < -1 & ZFA_Height >= -2 ~ "<-1SD & ≥-2SD",
        ZFA_Height < -2 ~ "<-2SD",
        ZFA_Height > 1 & ZFA_Height <= 2 ~ ">1SD & ≤2SD",
        ZFA_Height > 2 ~ ">2SD"),
        ZFA_Height_Cat = factor(ZFA_Height_Cat, levels = c("<-2SD", "<-1SD & ≥-2SD", "+-1SD", ">1SD & ≤2SD", ">2SD"))) %>%
    select(-sex, -Sex_Use, -Age, -Height, -Weight)


erase_spiro <- erase_spiro %>%
    left_join(temp, by = "PersID")

# Identify and remove duplicates

erase_spiro_duplicates <- erase_spiro %>%
    group_by(PersID, Reference) %>%
    filter(n() > 1) %>%
    ungroup()

erase_spiro <- erase_spiro %>%
    arrange(pre_Grade, post_Grade, ENum) %>%
    distinct(PersID, Reference, .keep_all = T)

    