df_analysis %>%
    mutate(Diabetes6.0_Status = ifelse(HbA1c >= 6.0, "Yes", "No")) %>%
    tabyl(Diabetes6.0_Status)

df_analysis %>%
    tabyl(Diabetes_Status)

0.38095/0.0944444

df_analysis %>%
    mutate(Diabetes6.0_Status = ifelse(HbA1c >= 6.0, "Yes", "No")) %>%
    tabyl(Diabetes6.0_Status, Sex_Use, show_na = FALSE) %>%
    adorn_percentages("col")

df_analysis %>%
    tabyl(Diabetes_Status, Sex_Use, show_na = FALSE) %>%
    adorn_percentages("col") 

df_analysis %>%
    mutate(Diabetes6.0_Status = ifelse(HbA1c >= 6.0, "Yes", "No")) %>%
    tabyl(Diabetes6.0_Status, Age_Baseline_Cat_CJC, show_na = FALSE) %>%
    adorn_percentages("col")

df_analysis %>%
    tabyl(Diabetes_Status, Age_Baseline_Cat_CJC, show_na = FALSE) %>%
    adorn_percentages("col")