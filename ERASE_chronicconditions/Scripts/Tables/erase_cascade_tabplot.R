# Create a summary table and plot for the cascade of care
# Colliding Epidemics paper
# December 2023
# Claire Calderwood

temp <- cascade_results %>%
    group_by(Condition, Stratum) %>%
    mutate(Max = max(TotalCount)) %>%
    rowwise() %>%
    mutate(N_Estimate = Estimate*TotalCount,
           N_LowerBound = LowerBound*TotalCount,
           N_UpperBound = UpperBound*TotalCount,
           P_Estimate = Estimate*TotalCount/Max,
           P_LowerBound = LowerBound*TotalCount/Max,
           P_UpperBound = UpperBound*TotalCount/Max) %>%
    filter(Level == "Yes")

# Table 

add_col <- cascade_results %>%
    filter(Status == "Known" & Level == "Yes") %>%
    select(Condition, Stratum, N = TotalCount)

table_cascade <- cascade_results %>%
    filter(Level == "Yes") %>%
    select(Stratum, Condition, Status, Prevalence = Estimate_CI, N = NWithCondition) %>%
    pivot_wider(id_cols = c(Stratum, Condition), names_from = Status, values_from = c("N", "Prevalence"), names_glue = "{Status}_{.value}") %>%
    left_join(add_col) %>%
    select(Stratum, Condition, N, starts_with("Known"), starts_with("Treated"), starts_with("Controlled"))


table_cascade

df_analysis %>%
    tabyl(Comorb_HIV)

# Plot

hiv_label = paste0("HIV (N = ", df_analysis %>% filter(HIV_Status == "Yes") %>% filter(Age_Baseline >= 18) %>% nrow(), ")")
diabetes_label = paste0("Diabetes (N = ", df_analysis %>% filter(Diabetes_Status == "Yes") %>% nrow(), ")")
htn_label = paste0("Hypertension (N = ", df_analysis %>% filter(Hypertension_Status == "Yes") %>% nrow(), ")")

labels = c(hiv_label, diabetes_label, htn_label)

plot_cascade <- temp %>%
    filter(Stratum %in% c("Women", "Men")) %>%
    mutate(Condition = case_when(
               Condition == "HIV" ~ hiv_label,
               Condition == "Diabetes" ~ diabetes_label,
               Condition == "Hypertension" ~ htn_label),
           Condition = factor(Condition, levels = labels)) %>%
    ggplot(aes(x = Status)) +
    geom_col(aes(y = P_Estimate, fill = Stratum), width = 0.7, position=position_dodge(width = 0.8)) +
    geom_linerange(aes(ymin = P_LowerBound, ymax = P_UpperBound, fill = Stratum),
        color = "black", size=0.5, position=position_dodge(width = 0.8), alpha = 0.3) +
    facet_wrap(vars(Condition)) +
    scale_fill_manual(values = colourscheme_cont, name = "Sex") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"),
                       name = "% of people with the condition") +
    scale_x_discrete(name = NULL) +
    theme_pubr(border = T) +
    theme(text = element_text(size = plot_fontsize))

plot_cascade

ggsave(plot = plot_cascade, file = paste0(outputdir, "plot_cascade.svg"), width=11, height=5)

ggsave(plot = plot_cascade, file = paste0(outputdir, "plot_cascade.jpeg"), width=7, height=5)

plot_cascade_site <- temp %>%
    filter(Stratum %in% c("Mozambique", "Tanzania", "Zimbabwe")) %>%
    mutate(Condition = case_when(
               Condition == "HIV" ~ hiv_label,
               Condition == "Diabetes" ~ diabetes_label,
               Condition == "Hypertension" ~ htn_label),
           Condition = factor(Condition, levels = labels)) %>%
    ggplot(aes(x = Status)) +
    geom_col(aes(y = P_Estimate, fill = Stratum), width = 0.7, position=position_dodge(width = 0.8)) +
    geom_linerange(aes(ymin = P_LowerBound, ymax = P_UpperBound, fill = Stratum),
        color = "black", size=0.5, position=position_dodge(width = 0.8), alpha = 0.3) +
    facet_wrap(vars(Condition)) +
    scale_fill_manual(values = colourscheme, name = "Sex") +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0%", "25%", "50%", "75%", "100%"),
                       name = "% of people with the condition") +
    scale_x_discrete(name = NULL) +
    theme_pubr(border = T) +
    theme(text = element_text(size = plot_fontsize))

plot_cascade_site

