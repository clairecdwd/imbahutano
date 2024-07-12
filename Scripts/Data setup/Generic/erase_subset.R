# Claire's code to:
#       - Clean variables needed for analysis
#       - Subset to required variables only
#       - Label variables used in analysis

library(labelled)

erase_dict <- labelled::generate_dictionary(erase_initial)

erase_df <- erase_initial %>%
    dplyr::mutate(
        SiteID = toupper(SiteID),
        SiteID = factor(SiteID),
        Sex_Use = case_when(
            Sex_Use == "female" ~ 0,
            Sex_Use == "male" ~ 1),
        Sex_Use = factor(Sex_Use, levels = c(0, 1), labels = c("Female", "Male")),
        Age_Baseline_Cat_CJC = 
               case_when(
                   Age_Baseline >= 40 ~ "40+ years",
                   Age_Baseline >= 18 & Age_Baseline < 40 ~ "18-39 years",
                   Age_Baseline < 18 ~ "10-17 years"),
        Age_Baseline_Cat_CJC = factor(Age_Baseline_Cat_CJC, levels = c("10-17 years",
                                                                       "18-39 years",
                                                                       "40+ years")),
        Married = ifelse(complete.cases(Q_107_QHc), Q_107_QHc, Q_106_QIc),
        Education = case_when(
            Education == "Illiterate/No education completed" ~ "None or primary school",
            Education == "Any primary school" ~ "None or primary school",
            Education == "Any secondary school" ~ "At least secondary school",
            Education == "Any College/Vocational" ~ "At least secondary school"
        ),
        Education = factor(Education, levels = c("None or primary school", "At least secondary school")),
        Employment_CJC = case_when(
            Employment == "Full-time (formal)" | Employment == "Part-time (formal)" |
                Q_702a_QIc == "DRIVER" ~ "Formal employment",
            Employment == "Full-time (informal)" | 
                Employment == "Part-time (informal)" | 
                Employment == "Occasional/Seasonal short-term" |
                Q_702a_QIc == "CARPENTER" | Q_702a_QIc == "PEASANT" | 
                Q_702a_QIc == "SELF EMPLOYED TAILOR" | Q_702a_QIc == "Self employed" |
                Q_702a_QIc == "Sex worker" | Q_702a_QIc == "Tout" |
                Q_702a_QIc == "Vendor" | Q_702a_QIc == "self employed"
                ~ "Informal / occasional employment",
            Employment == "Unemployed" ~ "Unemployed",
            Employment == "Unable to work" ~ "Unable to work",
            Employment == "House man/house wife" | Employment == "Retired" | Employment == "Student" ~ "Student / homemaker / retired"),
        IntervalSinceWorking = EnrolDate_Use - Q_703Date_QIc,
        Comorb_Diabetes = ifelse(
            ((Q_215_QHc == "Yes" | Q_233Dum1_QHc == "Oral anti diabetics" |
                Q_233Dum2_QHc == "Insulin") & is.na(Q_511_QIc)) | (Q_511_QIc == "Yes" & is.na(Q_215_QHc)), 1, 0),
        # If missing, assume do not have - this reflects what Alfred has said but is not in the data
        Comorb_Diabetes = ifelse(is.na(Comorb_Diabetes), 0, Comorb_Diabetes),
        Comorb_Diabetes = factor(Comorb_Diabetes, levels = c(0,1), labels = c("No","Yes")),
        Treated_Diabetes = case_when(
            Comorb_Diabetes == "Yes" & (Q_233Dum1_QHc == "Oral anti diabetics" | Q_233Dum2_QHc == "Insulin") ~ "Yes", 
            Comorb_Diabetes == "Yes" & ((Q_233Dum1_QHc != "Oral anti diabetics" | is.na(Q_233Dum1_QHc)) &
                                            (Q_233Dum2_QHc != "Insulin" | is.na(Q_233Dum2_QHc)))  ~ "No"),
        Comorb_Hypertension = ifelse(
            Q_228_QHc == "Yes" | Q_233Dum3_QHc == "Antihypertensives" |
                Q_226a_QHc %in% c("HIGH BLOOD PRESSURE", "HYPENTENSION", "HYPERTENSION", "Anti hypertensives drugs", "HPT",
                                  "Hypertension", "Known hypertensive", "Known hypertensive on Rx"),1, 0),
        # If missing, assume do not have - this reflects what Alfred has said but is not in the data
        Comorb_Hypertension = ifelse(is.na(Comorb_Hypertension), 0, Comorb_Hypertension),
        Comorb_Hypertension = factor(Comorb_Hypertension, levels = c(0,1), labels = c("No", "Yes")),
        Treated_Hypertension = case_when(
            Comorb_Hypertension == "Yes" & Q_233Dum3_QHc == "Antihypertensives" ~ "Yes", 
            Comorb_Hypertension == "Yes" & (Q_233Dum3_QHc != "Antihypertensives" | is.na(Q_233Dum3_QHc) | Q_226a_QHc == "Known hypertensive on Rx")  ~ "No"),
        Comorb_LungDis_Any = ifelse(
            Q_220_QHc == "Yes" | Q_221a_QHc == "Currently" | Q_221a_QHc == "In the last 2 years but not now" |
                Q_225_QHc == "Currently" | Q_225_QHc == "In the last 2 years but not now" |
                Q_222a_QHc == "Currently" | Q_222a_QHc == "In the last 2 years but not now" |
               Q_233Dum6_QHc == "Inhalers", "Yes", "No"),
        Comorb_LungDis_Asthma = ifelse(
            Q_221a_QHc == "Currently" | Q_221a_QHc == "In the last 2 years but not now" | Q_233Dum6_QHc == "Inhalers" |
                Q_233Dum6_QHc == "Inhalers", "Yes", "No"),
        Comorb_LungDis_Silicosis = ifelse(
            Q_225_QHc == "Currently" | Q_225_QHc == "In the last 2 years but not now", "Yes", "No"),
        Comorb_LungDis_Pneumonia = ifelse(
            Q_224b_QHc == "Currently" | Q_224b_QHc == "In the last 2 years but not now", "Yes", "No"),
        Comorb_LungDis_COVID19 = ifelse(
            Q_224a_QHc == "Don't know/Refuse to answer" , "No", Q_224a_QHc),
        Comorb_LungDis_ChestInj = ifelse(
            Q_223a_QHc == "Currently" | Q_223a_QHc == "In the last 2 years but not now", "Yes", "No"),
        Comorb_LungDis_LungCancer = ifelse(
            Q_222a_QHc == "Currently" | Q_222a_QHc == "In the last 2 years but not now", "Yes", "No"),
        across(starts_with("Comorb_LungDis"), ~ factor(.x, levels = c("No", "Yes"), ordered = T)),
        Comorb_HIV = ifelse(HIV_ReportedStatus == "Positive" | Q_303_QIc == "Positive" | 
                                Q_233a_QHc == "ANTIRETROVIRAL DRUGS" | Q_233a_QHc == "ART", 1, 0),
        Comorb_HIV = ifelse(is.na(Comorb_HIV), 0, Comorb_HIV),
        Comorb_HIV = factor(Comorb_HIV, levels = c(0,1), labels = c("No", "Yes")),
        HIV_ART_Baseline = ifelse(is.na(HIV_ART_Baseline) & (Q_233a_QHc == "ANTIRETROVIRAL DRUGS" | Q_233a_QHc == "ART"), "On ART", as.character(HIV_ART_Baseline)),
        Comorb_HIV_WithTB = case_when(
            Q_303a_QIc == "At the same time as diagnosed with TB (this TB episode)" ~ "At TB diagnosis",
            Q_303a_QIc == "Before I was diagnosed with TB: specify" ~ "Prior to TB diagnosis"),
        Comorb_HIV_WithTB = factor(Comorb_HIV_WithTB, levels = c("Prior to TB diagnosis", "At TB diagnosis")),
        Comorb_TBEver = ifelse(Q_209_QHc == "Yes" | Q_308_QIc == "Yes", 1, 0),
        Comorb_TBEver = ifelse(is.na(Comorb_TBEver), 0, Comorb_TBEver),
        Comorb_TBEver = factor(Comorb_TBEver, levels = c(0,1), labels = c("No", "Yes")),
        #Hhold_IndexBreadwinner = case_when(
        #    Hhold_Breadwinner == "Index case" ~ "Yes",
        #    Hhold_Breadwinner == "One of the household contacts (not necessarily this person)" ~ "No"),
        Hhold_Poverty = case_when(
            Hhold_Poverty == "≥ 1.90 USD" ~ 0,
            Hhold_Poverty == "< 1.90 USD" ~ 1),
        Hhold_Poverty = factor(Hhold_Poverty, levels = c(0, 1), labels = c("No", "Yes"), ordered = T),
        Pregnancy_Status_Baseline = case_when(
            Pregnancy_Status == "Pregnant" ~ 1,
            Pregnancy_Status == "Not pregnant at any point during the study but pregnant before" ~ 0,
            Pregnancy_Status == "Never pregnant" ~ 0),
        Pregnancy_Status_Baseline = factor(Pregnancy_Status_Baseline, levels = c(0, 1), labels = c("No", "Yes")),
        AUDIT_Score_Cat_CJC = case_when(
            Q_309_QHc == "No" | Q_506_QIc == "No" ~ "Never drunk alcohol",
            (Q_309_QHc == "Yes" & Q_310_QHc == "No") | 
                (Q_310_QHc == "Yes" & AUDIT_Score_Cat == "Negative") | 
                (Q_506_QIc == "Yes" & Q_507_QIc == "No") |
                (Q_507_QIc == "Yes" & AUDIT_Score_Cat == "Negative") ~ "Alcohol, AUDIT-C negative",
            (Q_310_QHc == "Yes" & AUDIT_Score_Cat == "Positive") |
                (Q_507_QIc == "Yes" & AUDIT_Score_Cat == "Positive") ~ "Alcohol, AUDIT-C positive"),
        AUDIT_Score_Cat_CJC = factor(AUDIT_Score_Cat_CJC, levels = c("Never drunk alcohol", "Alcohol, AUDIT-C negative", "Alcohol, AUDIT-C positive")),
        HistoryMining = ifelse((Q_315_QHc == "Yes" & is.na(Q_518_QIc)) | (Q_518_QIc == "Yes" & is.na(Q_315_QHc)), 1, 0),
        HistoryMining = factor(HistoryMining, levels = c(0, 1), labels = c("No", "Yes")),
        HistoryMining_Cat = case_when(
            Q_521_QIc == "Artisanal / small-scale mine" ~ "Artisanal / small-scale mine",
            Q_521_QIc == "Formally registered and run mine" ~ "Formally registered and run mine"),
        HistoryMining_Type = case_when(
            Q_521b_QIc == "Alluvial mining (mining along the river bed)" ~ "Alluvial mining",
            Q_521b_QIc == "Open / pit mine" ~ "Open / pit mining",
            Q_521b_QIc == "Shaft / underground mind" ~ "Shaft / underground mining"),
        HistoryMining_Dur = Q_520_QIc,
        Q_522_QIc = ifelse(Q_523a_QIc == "Helmets", "Infrequently / no", as.character(Q_522_QIc)),
        HistoryMining_PPEType = ifelse(Q_522_QIc != "Infrequently / no", as.character(Q_523_QIc), NA),
        HistoryMining_PPEType = ifelse(Q_523a_QIc == "respiratory mask", "Respiratory mask", HistoryMining_PPEType),
        HistoryMining_PPEType = ifelse(Q_523Str_QIc == "Respiratory mask & Other (Specify)", "Respiratory mask", HistoryMining_PPEType),
        HistoryMining_PPEType = ifelse(HistoryMining_PPEType == "", NA, HistoryMining_PPEType),
        HistoryPrison = ifelse((Q_312_QHc == "Yes" & is.na(Q_512_QIc)) | (Q_512_QIc == "Yes" & is.na(Q_312_QHc)), 1, 0),
        HistoryPrison = factor(HistoryPrison, levels = c(0, 1), labels = c("No", "Yes")),
        HistorySA = ifelse((Q_318_QHc == "Yes" & is.na(Q_515_QIc)) | (Q_515_QIc == "Yes" & is.na(Q_318_QHc)), 1, 0),
        HistorySA = factor(HistorySA, levels = c(0, 1), labels = c("No", "Yes")),
        HIV_IndexStatus = ifelse(HIV_IndexStatus == "Unknown", NA, as.character(HIV_IndexStatus)),
        HIV_IndexStatus = factor(HIV_IndexStatus, levels = c("Negative", "Positive, on ART", "Positive, not on ART")),
        Index_Relation = case_when(
            Q_401_QHc == "Spouse" ~ "Spouse",
            Q_401_QHc %in% c("Father", "Mother") ~ "Parent",
            Q_401_QHc %in% c("Brother", "Sister") ~ "Sibling",
            Q_401_QHc %in% c("Son/Daughter") ~ "Child",
            Q_401a_QHc %in% c("Daughter", "Daugther") ~ "Child",
            Q_401a_QHc %in% c("Girlfriend", "Husband", "wife/ girl friend") ~ "Spouse",
            Q_401a_QHc %in% c("sister", "Husband") ~ "Sibling"),
        Index_Relation = ifelse(is.na(Index_Relation) & complete.cases(Q_401_QHc), "Other", Index_Relation),
        Index_Relation = factor(Index_Relation, levels = c("Spouse", "Parent", "Sibling", "Child", "Other")),
        Index_Spouse = ifelse(Q_401_QHc == 'Spouse', "Spouse", "Other"),
        Index_Spouse = factor(Index_Spouse, levels = c("Spouse", "Other")),
        Study_Outcome_BLTB = case_when(
            Q_201_Eos == "Symptomatic TB diagnosed at baseline" ~ "TB",
            Q_201_Eos == "Subclinical TB diagnosed at baseline – (no symptoms)" ~ "Subclinical TB",
            (Q_201_Eos != "Symptomatic TB diagnosed at baseline" &
                 Q_201_Eos != "Subclinical TB diagnosed at baseline – (no symptoms)") |
                is.na(Q_201_Eos) ~ "No TB"),
        Study_Outcome_allTB = ifelse(
            str_detect(Q_206Str_Eos, "TB diagnosed") | str_detect(Q_206Str_Eos, "Died: possible TB")
            | str_detect(Q_206Str_Eos, "Died: confirmed TB"), "TB", "No TB"),
        InsufficientFood = ifelse(Q_218_QHc == "Don't know/Refuse to answer", NA, as.character(Q_218_QHc)),
        InsufficientFood = factor(InsufficientFood, levels = c("No", "Yes")),
        Index_SameRoom = ifelse(Q_408_QHc == "Don't know/Refuse to answer", NA, as.character(Q_408_QHc)),
        Index_SameRoom = factor(Index_SameRoom, levels = c("No", "Yes"))
    ) %>%
    dplyr::select(
        PersID,
        SiteID,
        HholdID,
        IndexCase_Use,
        EnrolDate_Use,
        Sex_Use,
        Age_Baseline,
        Age_Baseline_Cat,
        Age_Baseline_Cat_CJC,
        Age_Baseline_Cat_Bin,
        Married,
        Index_Relation,
        Index_Spouse,
        Comorb_Hypertension,
        Treated_Hypertension,
        Comorb_HIV,
        Comorb_HIV_WithTB,
        HIV_ART_Baseline,
        Hhold_Total_People,
        Hhold_Crowding_PPR,
        Hhold_Crowding_UN,
        Employment_CJC,
        IntervalSinceWorking,
        Education,
        Pregnancy_Status_Baseline,
        Smoking_Cat,
        Smoking_Cat_Bin,
        Smoking_PackYears,
        AUDIT_Alcohol_Ever = Q_309_QHc,
        AUDIT_Alcohol_Curr = Q_310_QHc,
        AUDIT_Q1,
        AUDIT_Q2,
        AUDIT_Q3,
        AUDIT_Score,
        AUDIT_Score_Cat,
        AUDIT_Score_Cat_CJC,
        Hhold_Income_Day,
        Hhold_IndexBreadwinner,
        Hhold_Poverty,
        Income_Bracket,
        SES,
        SES_quintiles,
        Hhold_ResidenceArea,
        Comorb_TBEver,
        Comorb_Diabetes,
        Treated_Diabetes,
        InsufficientFood,
        MealsPerDay = Q_219_QHc,
        Comorb_LungDis_Any,
        Comorb_LungDis_Asthma,
        Comorb_LungDis_LungCancer,
        Comorb_LungDis_ChestInj,
        Comorb_LungDis_COVID19,
        Comorb_LungDis_Pneumonia,
        Comorb_LungDis_Silicosis,
        Comorb_CVD,
        Comorb_CKD = Q_231_QHc,
        Comorb_MentalHealth = Q_232_QHc,
        HIV_TestResult,
        HIV_IndexStatus,
        HbA1c_Cat_Overall,
        Hb_Anaemia_Cat,
        BMI_Overall = BMI,
        MUAC_Left_Overall,
        HistoryMining,
        HistoryMining_Dur,
        HistoryMining_Cat,
        HistoryMining_Type,
        HistoryMining_PPE = Q_522_QIc,
        HistoryMining_PPEType,
        HistoryPrison,
        HistorySA,
        IGRA_Result_Overall,
        Symp_TB_Bin,
        CXR_Result = Q_203a_Xry,
        Micro_Result_Det_Pv,
        BP_Sys_Overall,
        BP_Dia_Overall,
        BP_Hypertension_Overall,
        HbA1c_Overall,
        Study_Outcome = Q_201_Eos,
        Study_Outcome_BLTB,
        Study_Outcome_allTB,
        Index_SameRoom
    ) %>%
    droplevels() %>%
    dplyr::mutate(MealsUnder2 = ifelse(MealsPerDay < 2, "Yes", "No"),
           CXR_Result_Bin = ifelse(CXR_Result == "Findings attributable to TB", "Yes", "No"))

var_label(erase_df) <- list(
    SiteID = "Site",
    Sex_Use = "Sex",
    Age_Baseline = "Age, years",
    Age_Baseline_Cat_CJC = "Age category",
    Pregnancy_Status_Baseline = "Pregnant",
    Married = "Marital status",
    Education = "Highest educational level",
    Employment_CJC = "Employment",
    IntervalSinceWorking = "Time since last working / days",
    Smoking_Cat_Bin = "Smoking status",
    Smoking_PackYears = "Pack years smoking",
    AUDIT_Alcohol_Curr = "Currently drink alcohol",
    AUDIT_Score_Cat = "AUDIT-C result",
    AUDIT_Score_Cat_CJC = "Alcohol consumption",
    InsufficientFood = "Insufficient food (last 6 months)",
    Comorb_HIV = "Known HIV",
    Comorb_HIV_WithTB = "Timing of HIV diagnosis",
    Comorb_Hypertension = "Known hypertension",
    Comorb_TBEver = "Previous TB",
    Comorb_Diabetes = "Known diabetes",
    Comorb_LungDis_Any = "Any lung disease",
    Comorb_LungDis_Asthma = "Asthma",
    Comorb_LungDis_LungCancer = "Lung cancer",
    Comorb_LungDis_ChestInj = "Traumatic chest injury",
    Comorb_LungDis_COVID19 = "COVID-19",
    Comorb_LungDis_Pneumonia = "Pneumonia",
    Comorb_LungDis_Silicosis = "Silicosis",
    Comorb_CVD = "Heart disease",
    Comorb_CKD = "CKD",
    Comorb_MentalHealth = "Mental health condition",
    Hhold_Total_People = "N people in household",
    Hhold_Crowding_PPR = "N people per room",
    Hhold_Crowding_UN = "Crowding",
    Hhold_Income_Day = "Household income per person per day, USD",
    Hhold_Poverty = "Household income <1.90USD per person per day",
    Hhold_IndexBreadwinner = "Primary earner is the person with TB",
    Hhold_ResidenceArea = "Area of residence",
    HistoryMining = "History of mining",
    HistoryMining_Dur = "Duration of mine work / years",
    HistoryMining_Cat = "Category of mine work",
    HistoryMining_Type = "Type of mine",
    HistoryMining_PPE = "Use of respiratory PPE",
    HistoryMining_PPEType = "Type of PPE used",
    HistoryPrison = "History of imprisonment",
    HistorySA = "History of living in SA",
    Index_Relation = "Relationship to index case",
    IGRA_Result_Overall = "IGRA result",
    Symp_TB_Bin = "TB symptom screen",
    CXR_Result = "CXR result",
    CXR_Result_Bin = "CXR suggestive of TB",
    Micro_Result_Det_Pv = "TB test result",
    Study_Outcome_BLTB = "TB diagnosed (baseline)",
    Study_Outcome_allTB = "TB diagnosed (any time in study)",
    Index_SameRoom = "Sleep in the same room as index case",
    HIV_IndexStatus = "HIV status of index case"
)