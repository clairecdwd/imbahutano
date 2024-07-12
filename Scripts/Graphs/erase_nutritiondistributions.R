
library(zscorer)
library(ggpubr)
library(janitor)

temp <- df_analysis %>%
    ungroup() %>%
    select(PersID, Sex_Use, Age_Baseline, Age_Baseline_Cat_CJC, Weight, Height, MUAC_Left_Overall,
           BMI_Grade, HbA1c, Diabetes_Status, Hypertension_Status,
           BP_Sys_Overall, BP_Dia_Overall) %>%
    mutate(sex = case_when(
        Sex_Use == "Men" ~ 1,
        Sex_Use == "Women" ~ 2),
        Age = ifelse(Age_Baseline > 18.9, 18.9, Age_Baseline)) %>% # This is the format needed for the command to work
    mutate(Age_days = Age*365.25) %>%
    mutate(muac = MUAC_Left_Overall) %>%
    addWGSR(sex = "sex", firstPart = "Weight",
            secondPart = "Height", thirdPart = "Age_days",
            index = "bfa", output = "ZFA_BMI") %>%
    mutate(ZFA_BMI_Cat = case_when(
        ZFA_BMI >= -1 & ZFA_BMI <= 1 ~ "+-1SD",
        ZFA_BMI < -1 & ZFA_BMI >= -2 ~ "<-1SD & ≥-2SD",
        ZFA_BMI < -2 ~ "<-2SD",
        ZFA_BMI > 1 & ZFA_BMI <= 2 ~ ">1SD ≤2SD",
        ZFA_BMI > 2 ~ ">2SD"),
        ZFA_BMI_Cat = factor(ZFA_BMI_Cat, levels = c("<-2SD", "<-1SD & ≥-2SD", "+-1SD", ">1SD & ≤2SD", ">2SD"))) %>%
    addWGSR(sex = "sex", firstPart = "Height",
            secondPart = "Age_days",
            index = "hfa", output = "ZFA_Height") %>%
    mutate(ZFA_Height_Cat = case_when(
        ZFA_Height >= -1 & ZFA_Height <= 1 ~ "+-1SD",
        ZFA_Height < -1 & ZFA_Height >= -2 ~ "<-1SD & ≥-2SD",
        ZFA_Height < -2 ~ "<-2SD",
        ZFA_Height > 1 & ZFA_Height <= 2 ~ ">1SD & ≤2SD",
        ZFA_Height > 2 ~ ">2SD"),
        ZFA_Height_Cat = factor(ZFA_Height_Cat, levels = c("<-2SD", "<-1SD & ≥-2SD", "+-1SD", ">1SD & ≤2SD", ">2SD"))) %>%
    addWGSR(sex = "sex", firstPart = "muac",
            secondPart = "Age_days",
            index = "mfa", output = "ZFA_MUAC") %>%
    mutate(ZFA_MUAC_Cat = case_when(
        ZFA_MUAC >= -1 & ZFA_MUAC <= 1 ~ "+-1SD",
        ZFA_MUAC < -1 & ZFA_MUAC >= -2 ~ "<-1SD & ≥-2SD",
        ZFA_MUAC < -2 ~ "<-2SD",
        ZFA_MUAC > 1 & ZFA_MUAC <= 2 ~ ">1SD & ≤2SD",
        ZFA_MUAC > 2 ~ ">2SD"),
        ZFA_MUAC_Cat = factor(ZFA_MUAC_Cat, levels = c("<-2SD", "<-1SD & ≥-2SD", "+-1SD", ">1SD & ≤2SD", ">2SD"))) %>%
    select(-sex)

plot_bmibyagecat <- temp %>%
    ggplot() +
    geom_rect(xmin = -2, xmax = 2, ymin = 0, ymax = Inf, fill = "#ededed") +
    geom_rect(xmin = -1, xmax = 1, ymin = 0, ymax = Inf, fill = "#d3d3d3") +
    geom_density(aes(ZFA_BMI, col = Age_Baseline_Cat_CJC, fill = Age_Baseline_Cat_CJC), alpha = 0.5) +
    facet_wrap(vars(Sex_Use)) +
    scale_x_continuous(name = "BMI for age Z score", breaks = c(-6, -4, -2 , 0, 2, 4, 6)) +
    scale_y_continuous(name = "Density") +
    scale_fill_manual(name = "Age category", values=colourscheme_blues) +
    scale_color_manual(name = "Age category", values=colourscheme_blues) +
    theme_pubr(border = TRUE) +
    theme(text = element_text(size = plot_fontsize),
          legend.key.size = unit(0.2, "cm"))

plot_bmibyagecat
    
temp %>%
    filter(Age_Baseline >=18) %>%
    tabyl(BMI_Grade, ZFA_BMI_Cat)

plot_bmibyage <- temp %>%
    ggplot(aes(x = Age_Baseline, y = ZFA_BMI, col = Sex_Use, fill = Sex_Use)) +
    geom_point(size = dotsize) +
    geom_smooth(linewidth = dotsize) +
    scale_x_continuous(name = "Age (years)") +
    scale_y_continuous(name = "BMI for age Z score") +
    scale_fill_manual(name = "Sex", values=colourscheme_greens) +
    scale_color_manual(name = "Sex", values=colourscheme_greens) +
    theme_pubr(border = T) +
    theme(text = element_text(size = plot_fontsize),
          legend.key.size = unit(0.2, "cm"))

plot_bmibyage

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot() +
    geom_rect(xmin = -2, xmax = 2, ymin = 0, ymax = Inf, fill = "#ededed") +
    geom_rect(xmin = -1, xmax = 1, ymin = 0, ymax = Inf, fill = "#d3d3d3") +
    geom_density(aes(ZFA_MUAC, col = Age_Baseline_Cat_CJC, fill = Age_Baseline_Cat_CJC), alpha = 0.5) +
    facet_wrap(vars(Sex_Use)) +
    scale_x_continuous(breaks = c(-6, -4, -2 , 0, 2, 4, 6)) +
    theme_pubr(border = TRUE)

temp %>%
    filter(Age_Baseline >=18) %>%
    tabyl(ZFA_MUAC_Cat, ZFA_BMI_Cat)

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot(aes(x = Age_Baseline, y = ZFA_MUAC, col = Sex_Use)) +
    geom_point() +
    geom_smooth() +
    theme_pubr(border = T)

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot(aes(x = ZFA_BMI, y = ZFA_MUAC, col = Sex_Use)) +
    geom_point() +
    geom_smooth() +
    theme_pubr(border = T)

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot(aes(x = ZFA_BMI, y = ZFA_MUAC, col = Age_Baseline_Cat_CJC)) +
    geom_point() +
    geom_smooth() +
    theme_pubr(border = T)
    
temp %>%
    ggplot() +
    geom_rect(xmin = -2, xmax = 2, ymin = 0, ymax = Inf, fill = "#ededed") +
    geom_rect(xmin = -1, xmax = 1, ymin = 0, ymax = Inf, fill = "#d3d3d3") +
    geom_density(aes(ZFA_Height, col = Age_Baseline_Cat_CJC, fill = Age_Baseline_Cat_CJC), alpha = 0.5) +
    facet_wrap(vars(Sex_Use)) +
    scale_x_continuous(breaks = c(-6, -4, -2 , 0, 2, 4, 6)) +
    theme_pubr(border = TRUE)

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot(aes(x = ZFA_BMI, y = ZFA_Height, col = Sex_Use)) +
    geom_point() +
    geom_smooth() +
    theme_pubr(border = T)  

temp %>%
    filter(MUAC_Left_Overall > 10) %>%
    ggplot(aes(x = ZFA_MUAC, y = ZFA_Height, col = Age_Baseline_Cat_CJC)) +
    geom_point() +
    geom_smooth() +
    theme_pubr(border = T)  

temp %>%
    ggplot(aes(x = ZFA_BMI, y = HbA1c, col = Sex_Use)) +
    geom_point() +
    geom_smooth(method = "lm")

temp %>%
    ggplot(aes(y = BMI_Grade, x = log(HbA1c), col = Sex_Use)) +
    geom_point(position = "jitter") +
    geom_boxplot(outlier.size = 0) +
    facet_wrap(vars(Sex_Use))

temp %>%
    ggplot(aes(y = BMI_Grade, x = BP_Sys_Overall, col = Sex_Use)) +
    geom_point(position = "jitter") +
    geom_boxplot(outlier.size = 0) +
    facet_wrap(vars(Sex_Use))

fig_htndmbybmi <- temp %>%
    select(Sex_Use, BMI_Grade, HbA1c, `Systolic BP` = BP_Sys_Overall,
           `Diastolic BP` = BP_Dia_Overall) %>%
    pivot_longer(c(HbA1c, `Systolic BP`, `Diastolic BP`)) %>%
    mutate(name = factor(name, levels = c("HbA1c", "Systolic BP", "Diastolic BP"))) %>%
    ggplot(aes(y = BMI_Grade, x = value, col = Sex_Use)) +
    geom_point(position = "jitter", size = dotsize) +
    geom_boxplot(outlier.size = 0) +
    facet_grid(rows = vars(Sex_Use), cols = vars(name),
               scales = "free_x") +
    scale_color_manual(name = "Sex", values = colourscheme_greens[2:3]) +
    scale_x_continuous(trans='log10') +
    theme_pubr(border = T, legend = "top") +
    theme(text = element_text(size = plot_fontsize),
          legend.key.size = unit(0.2, "cm"),
          axis.title.y = element_blank())

table_diabetesbybmi <- temp %>%
    filter(Age_Baseline >= 18) %>%
    tabyl(BMI_Grade, Diabetes_Status) %>%
    adorn_percentages() %>%
    adorn_pct_formatting() %>%
    adorn_ns("front")

table_htnbybmi <- temp %>%
    filter(Age_Baseline >= 18) %>%
    tabyl(BMI_Grade, Hypertension_Status) %>%
    adorn_percentages() %>%
    adorn_pct_formatting() %>%
    adorn_ns("front")

erase_bmizscores <- temp

