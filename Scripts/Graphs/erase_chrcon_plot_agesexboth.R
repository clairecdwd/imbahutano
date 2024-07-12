
label_overall = df_analysis %>% 
    group_by(Sex_Use) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(label = paste0(Sex_Use, " (N = ", n, ")")) %>%
    select(-n) %>%
    ungroup()

label_site = df_analysis %>% 
    group_by(Site, Sex_Use) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(label = paste0(Sex_Use, " (N = ", n, ")")) %>%
    select(-n) %>%
    ungroup()

plot_agesexboth <- prevalence_results_agesex %>%
    filter(Condition %in% c("HIV_Status", "BMI_Underweight", "BMI_Obese", 
                            "ImpLungFn_Status", "Anaemia_Status", 
                            "Diabetes_Status", "Hypertension_Status")) %>%
    mutate(Condition = case_when(
               Condition == "BMI_Underweight" ~ "Underweight",
               Condition == "BMI_Obese" ~ "Overweight/obese",
               Condition == "Diabetes_Status" ~ "Elevated HbA1c",
               Condition == "ImpLungFn_Status" ~ "Chronic lung disease",
               Condition == "HIV_Status" ~ "HIV",
               Condition == "Anaemia_Status" ~ "Anaemia",
               Condition == "Hypertension_Status" ~ "Elevated BP",
               ),
           Condition = factor(Condition, levels = c("HIV", "Underweight", "Overweight/obese",
                                                    "Anaemia",
                                                    "Chronic lung disease",
                                                    "Elevated HbA1c",
                                                    "Elevated BP")),
           Estimate = Estimate*100,
           LowerBound = LowerBound*100,
           UpperBound = UpperBound*100,
           ) %>%
    filter(complete.cases(Condition)) %>%
    filter(Level == "Yes") %>%
    ggplot(aes(y = fct_rev(Condition), x=Estimate, xmin=LowerBound, xmax=UpperBound, col = AgeCat, fill = AgeCat, shape=AgeCat)) + 
    geom_linerange(size=6, position=position_dodge(width = 0.5), alpha = 0.3) +
    geom_point(size=2, position=position_dodge(width = 0.5)) +
    scale_shape_discrete(name = "Age category") +
    scale_x_continuous(name="Prevalence", breaks = c(0,10, 20, 30, 40, 50,60),
                       labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%")) +
    #coord_cartesian(xlim = c(0,50)) +
    scale_y_discrete(name="Condition", drop = FALSE) +
    theme_pubr(border = T, margin = F) +
    theme(legend.spacing.x = unit(0.05, "cm"),
          text = element_text(size = plot_fontsize),
          axis.title.y = element_blank()) +
    ylab(NULL) +
    guides(fill = FALSE, color = FALSE)
