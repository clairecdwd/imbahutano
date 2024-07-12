

colourscheme_cont <- c("#abdbe3", "#1e81b0", "#223a74","#3fbd39", "#2b8227")

plot_agesex <- prevalence_results %>%
    mutate(Condition = case_when(
               Condition == "BMI_Underweight" ~ "Underweight",
               Condition == "Diabetes_Status" ~ "Elevated HbA1c",
               Condition == "ImpLungFn_Status" ~ "Chronic lung disease",
               Condition == "HIV_Status" ~ "HIV",
               Condition == "Anaemia_Status" ~ "Anaemia",
               Condition == "TB_Status" ~ "TB",
               ),
           Condition = factor(Condition, levels = c("TB", "HIV", "Underweight", 
                                                    "Anaemia",
                                                    "Chronic lung disease",
                                                    "Elevated HbA1c")),
           Code = case_when(
               Stratum %in% c("Female", "10-17 years") ~ 1,
               Stratum %in% c("Male", "18-39 years") ~ 2,
               Stratum %in% c("40+ years") ~ 3),
           Code = as.character(Code),
           Estimate = Estimate*100,
           LowerBound = LowerBound*100,
           UpperBound = UpperBound*100
           ) %>%
    filter(complete.cases(Condition)) %>%
    filter(Stratum %in% c("Female", "Male", "10-17 years", "18-39 years", "40+ years")) %>%
    mutate(Cat = case_when(
        Stratum %in% c("Female", "Male") ~ "Sex",
        Stratum %in% c("10-17 years", "18-39 years", "40+ years") ~ "Age")) %>%
    filter(Level == "Yes") %>%
    ggplot(aes(y = fct_rev(Condition), x=Estimate, xmin=LowerBound, xmax=UpperBound, col = Stratum, fill = Stratum, shape = Code)) + 
    geom_linerange(size=8, position=position_dodge(width = 0.5), alpha = 0.3) +
    geom_point(size=4, position=position_dodge(width = 0.5)) +
    scale_fill_manual(name = NULL, values=colourscheme_cont) +
    scale_color_manual(name = NULL, values=colourscheme_cont) +
    scale_shape_discrete(NULL, guide = "none") +
    scale_x_continuous(name="Prevalence", breaks = c(0,10, 20,30, 40), labels = c("0%", "10%", "20%", "30%", "40%")) +
    scale_y_discrete(name="Condition", drop = FALSE) +
    theme_pubr(border = T, margin = F) +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.spacing.x = unit(0.05, "cm")) +
    ylab(NULL) +
    facet_wrap(vars(Cat))