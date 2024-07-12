## Get the first result for each screening test per person

dict <- labelled::generate_dictionary(A02)

# Set up
erase_firstresults <- A02 %>%
    filter(IndexCase_Use == "Hhold contact") %>%
    select(PersID) %>%
    distinct(PersID)

# HIV
erase_firstresults <- A02 %>%
    filter(complete.cases(Q_103_Hiv)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, HIV_Test = Q_103_Hiv, HIV_Date = VisitDateMod, HIV_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# CD4
erase_firstresults <- A02 %>%
    dplyr::mutate(Q_401a_Hiv = as.numeric(Q_401a_Hiv)) %>%
    filter(complete.cases(Q_401a_Hiv)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, HIV_CD4 = Q_401a_Hiv, HIV_CD4_Date = VisitDateMod, HIV_CD4_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# HbA1c
erase_firstresults <- A02 %>%
    dplyr::mutate(Q_103_HbA = as.numeric(Q_103_HbA)) %>%
    filter(complete.cases(Q_103_HbA)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, HbA1c = Q_103_HbA, HbA1c_Date = VisitDateMod, HbA1c_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# Hb
erase_firstresults <- A02 %>%
    dplyr::mutate(Q_106_HbA = as.numeric(Q_106_HbA)) %>%
    filter(complete.cases(Q_106_HbA)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, Hb = Q_106_HbA, Hb_Date = VisitDateMod, Hb_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# Height and weight
erase_firstresults <- A02 %>%
    dplyr::mutate(Q_302_Exm = as.character(Q_302_Exm),
           Q_302_Exm = as.numeric(Q_302_Exm)) %>%
    filter(complete.cases(Q_302_Exm)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, Height = Q_302_Exm, Height_Date = VisitDateMod, Height_Visit = ENum) %>%
    left_join(erase_firstresults, .)

erase_firstresults <- A02 %>%
    dplyr::mutate(Q_301_Exm = as.character(Q_301_Exm),
           Q_301_Exm = as.numeric(Q_301_Exm)) %>%
    filter(complete.cases(Q_301_Exm)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, Weight = Q_301_Exm, Weight_Date = VisitDateMod, Weight_Visit = ENum, Weight_Age = Age_Visit) %>%
    left_join(erase_firstresults, .)

# BP
# Use the Overall variables in preference to this for analyis - see setup file

erase_firstresults <- A02 %>%
    dplyr::mutate(BP_Sys = rowMeans(select(. , c(Q_308aa1_Exm, Q_309aa1_Exm)), na.rm = T),
           BP_Dia = rowMeans(select(. , c(Q_308aa2_Exm, Q_309aa2_Exm)), na.rm = T),
           BP_Sys = ifelse(is.na(BP_Sys), Q_307aa1_Exm, BP_Sys),
           BP_Dia = ifelse(is.na(BP_Dia), Q_307aa2_Exm, BP_Dia)) %>%
    filter(complete.cases(BP_Sys)) %>%
    arrange(PersID, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, BP_Sys, BP_Dia, BP_Date = VisitDateMod, BP_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# Spiro

erase_firstresults <- A02 %>%
    filter(complete.cases(Q_302_Spr, Q_202_Spr)) %>%
    dplyr::mutate(Spr_Acceptable = ifelse(Q_302_Spr %in% c("A", "B", "C") &
                      Q_202_Spr %in% c("A", "B", "C"), "A-C", "Not A-C")) %>%
    arrange(PersID, Spr_Acceptable, VisitDateMod) %>%
    distinct(PersID, .keep_all = T) %>%
    select(PersID, Spr_Acceptable, Spr_Date = VisitDateMod, Spr_Visit = ENum) %>%
    left_join(erase_firstresults, .)

# Add in MMRC results

A02_PerVisit_MMRCHb <- A02_PerVisit_MMRCHb %>%
    select(PersID = `Participant.ID`, TimePoint,
           Hb_Date_DISA = `Date.........25`, Hb_DISA = HB) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           Hb_Date_DISA = dmy(Hb_Date_DISA),
           Hb_Visit_DISA = case_when(
               TimePoint == "01 HHC's (" ~ 1,
               TimePoint == "02 HHC's (" ~ 3,
               TimePoint == "03 HHC's (" ~ 5,
               TimePoint == "04 HHC's (" ~ 7,
               TimePoint == "05 HHC's (" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    filter(Hb_DISA < 25) %>% # Drop outliers
    group_by(PersID) %>%
    arrange(Hb_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()

# Select CD4
A02_PerVisit_MMRCCD4 <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB_663_CD4 SResult 23_06_09_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal")) %>%
    select(PersID = `Participant.ID`, TimePoint,
           HIV_CD4_Date_DISA = `Date.........25`, HIV_CD4_DISA = CD4.Count) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           HIV_CD4_Date_DISA = dmy(HIV_CD4_Date_DISA),
           HIV_CD4_Visit_DISA = case_when(
               TimePoint == "01 HHC's (" ~ 1,
               TimePoint == "02 HHC's (" ~ 3,
               TimePoint == "03 HHC's (" ~ 5,
               TimePoint == "04 HHC's (" ~ 7,
               TimePoint == "05 HHC's (" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    group_by(PersID) %>%
    arrange(HIV_CD4_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()

# Select HIV
A02_PerVisit_MMRCHIV <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB_663_ HIV RAPID SResult 23_06_08_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal")) %>%
    select(PersID = `Participant.ID`, TimePoint,
           HIV_Date_DISA = `Date.........17`, HIV_DISA = HIV.Rapid) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           HIV_Date_DISA = dmy(HIV_Date_DISA),
           HIV_Visit_DISA = case_when(
               TimePoint == "01 HHC's (" ~ 1,
               TimePoint == "02 HHC's (" ~ 3,
               TimePoint == "03 HHC's (" ~ 5,
               TimePoint == "04 HHC's (" ~ 7,
               TimePoint == "05 HHC's (" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    group_by(PersID) %>%
    arrange(HIV_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()

# Frozen HbA1c
A02_PerVisit_MMRCHBA1C_frozen <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB-663_HBA1C  FROZEN SAMPLE SResult 24_02_27_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal")) %>%
    select(PersID = `Participant.ID`, TimePoint,
           HbA1c_Date_DISA = `Date.........25`, HbA1c_DISA = HBA1C) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           HbA1c_Date_DISA = dmy(HbA1c_Date_DISA),
           HbA1c_Visit_DISA = case_when(
               TimePoint == "HHC's01" ~ 1,
               TimePoint == "HHC's02" ~ 3,
               TimePoint == "HHC's03" ~ 5,
               TimePoint == "HHC's04" ~ 7,
               TimePoint == "HHC's05" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    group_by(PersID) %>%
    arrange(HbA1c_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()  %>%
    dplyr::mutate(HbA1c_Source = "Frozen")

# Fresh HbA1c
A02_PerVisit_MMRCHBA1C_fresh <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB-663_HBA1C FRESH SAMPLE SResult 24_02_27_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal")) %>%
    select(PersID = `Participant.ID`, TimePoint,
           HbA1c_Date_DISA = `Date.........25`, HbA1c_DISA = HBA1C) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           HbA1c_Date_DISA = dmy(HbA1c_Date_DISA),
           HbA1c_Visit_DISA = case_when(
               TimePoint == "HHC's01" ~ 1,
               TimePoint == "HHC's02" ~ 3,
               TimePoint == "HHC's03" ~ 5,
               TimePoint == "HHC's04" ~ 7,
               TimePoint == "HHC's05" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    group_by(PersID) %>%
    arrange(HbA1c_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()  %>%
    dplyr::mutate(HbA1c_Source = "Fresh")

# Select HbA1c
A02_PerVisit_MMRCHBA1C <- with_dir(
    dir = paste0(datadir, "/ERASE_MMRC_Hb/"),
    expr = readxl::read_xlsx(paste0(last_mmrc, "/ERASE_TB_663_HBA1C SResult 23_06_09_006.xlsx"),
                     sheet = 2, skip = 3, .name_repair = "universal")) %>%
    select(PersID = `Participant.ID`, TimePoint,
           HbA1c_Date_DISA = `Date.........25`, HbA1c_DISA = HBA1C) %>%
    dplyr::mutate(PersID = str_remove(PersID, "-"),
           PersID = substr(PersID,1,8),
           HbA1c_Date_DISA = dmy(HbA1c_Date_DISA),
           HbA1c_Visit_DISA = case_when(
               TimePoint == "01 HHC's (" ~ 1,
               TimePoint == "02 HHC's (" ~ 3,
               TimePoint == "03 HHC's (" ~ 5,
               TimePoint == "04 HHC's (" ~ 7,
               TimePoint == "05 HHC's (" ~ 9,
               )) %>%
    select(-TimePoint) %>%
    filter(complete.cases(PersID)) %>%
    group_by(PersID) %>%
    arrange(HbA1c_Date_DISA) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup() %>%
    dplyr::mutate(HbA1c_Source = "All") %>%
    rbind(., A02_PerVisit_MMRCHBA1C_fresh) %>%
    rbind(., A02_PerVisit_MMRCHBA1C_frozen)

list_mmrc_hba1c_duplicates <- A02_PerVisit_MMRCHBA1C %>%
    group_by(PersID) %>%
    filter(n() >1) %>%
    ungroup()

A02_PerVisit_MMRCHBA1C <- A02_PerVisit_MMRCHBA1C %>%
    group_by(PersID) %>%
    filter(row_number()==1) %>% # Keep first result per person
    ungroup()

erase_firstresults <- erase_firstresults %>%
    # Hb
    left_join(., A02_PerVisit_MMRCHb) %>%
    dplyr::mutate(Hb_Source = ifelse(is.na(Hb) & complete.cases(Hb_DISA), "DISA", "OC"),
           Hb = ifelse(is.na(Hb), Hb_DISA, Hb),
           Hb_Date = ifelse(is.na(Hb_Date), Hb_Date_DISA, Hb_Date),
           Hb_Date = as_date(Hb_Date),
           Hb_Visit = ifelse(is.na(Hb_Visit), Hb_Visit_DISA, as.numeric(Hb_Visit)),
           Hb_Visit = factor(Hb_Visit, levels = c(1:9), labels = c("Visit-1", "V1-TBLAB", "Visit-2", "V2-TBLAB", 
                                                  "Visit-3", "V3-TBLAB", "Visit-4", "V4-TBLAB", "Visit-5"))) %>%
    select(-Hb_DISA, -Hb_Date_DISA, -Hb_Visit_DISA) %>%
    # CD4
    left_join(., A02_PerVisit_MMRCCD4) %>%
    dplyr::mutate(HIV_CD4_Source = ifelse(is.na(HIV_CD4) & complete.cases(HIV_CD4_DISA), "DISA", "OC"),
           HIV_CD4 = ifelse(is.na(HIV_CD4), HIV_CD4_DISA, HIV_CD4),
           HIV_CD4_Date = ifelse(is.na(HIV_CD4_Date), HIV_CD4_Date_DISA, HIV_CD4_Date),
           HIV_CD4_Date = as_date(HIV_CD4_Date),
           HIV_CD4_Visit = ifelse(is.na(HIV_CD4_Visit), HIV_CD4_Visit_DISA, as.numeric(HIV_CD4_Visit)),
           HIV_CD4_Visit = factor(HIV_CD4_Visit, levels = c(1:9), labels = c("Visit-1", "V1-TBLAB", "Visit-2", "V2-TBLAB", 
                                                  "Visit-3", "V3-TBLAB", "Visit-4", "V4-TBLAB", "Visit-5"))) %>%
    select(-HIV_CD4_DISA, -HIV_CD4_Date_DISA, -HIV_CD4_Visit_DISA) %>%
    # HIV
    left_join(., A02_PerVisit_MMRCHIV) %>%
    dplyr::mutate(HIV_Source = ifelse(is.na(HIV_Test) & complete.cases(HIV_DISA), "DISA", "OC"),
           HIV_Test = ifelse(is.na(HIV_Test), HIV_DISA, as.character(HIV_Test)),
           HIV_Test = factor(HIV_Test, levels = c("Negative", "Positive")),
           HIV_Date = ifelse(is.na(HIV_Date), HIV_Date_DISA, HIV_Date),
           HIV_Date = as_date(HIV_Date),
           HIV_Visit = ifelse(is.na(HIV_Visit), HIV_Visit_DISA, as.numeric(HIV_Visit)),
           HIV_Visit = factor(HIV_Visit, levels = c(1:9), labels = c("Visit-1", "V1-TBLAB", "Visit-2", "V2-TBLAB", 
                                                  "Visit-3", "V3-TBLAB", "Visit-4", "V4-TBLAB", "Visit-5"))) %>%
    select(-HIV_DISA, -HIV_Date_DISA, -HIV_Visit_DISA) %>%
    # HBA1C
    left_join(., A02_PerVisit_MMRCHBA1C) %>%
    dplyr::mutate(HbA1c_Source = ifelse(is.na(HbA1c) & complete.cases(HbA1c_DISA), "DISA", "OC"),
           HbA1c = ifelse(is.na(HbA1c), HbA1c_DISA, HbA1c),
           HbA1c_Date = ifelse(is.na(HbA1c_Date), HbA1c_Date_DISA, HbA1c_Date),
           HbA1c_Date = as_date(HbA1c_Date),
           HbA1c_Visit = ifelse(is.na(HbA1c_Visit), HbA1c_Visit_DISA, as.numeric(HbA1c_Visit)),
           HbA1c_Visit = factor(HbA1c_Visit, levels = c(1:9), labels = c("Visit-1", "V1-TBLAB", "Visit-2", "V2-TBLAB", 
                                                  "Visit-3", "V3-TBLAB", "Visit-4", "V4-TBLAB", "Visit-5"))) %>%
    select(-HbA1c_DISA, -HbA1c_Date_DISA, -HbA1c_Visit_DISA)

rm(A02_PerVisit_MMRCCD4, A02_PerVisit_MMRCHb, A02_PerVisit_MMRCHBA1C, A02_PerVisit_MMRCHIV)
