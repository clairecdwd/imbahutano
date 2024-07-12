# Intersection plots
# 
# https://cran.r-project.org/web/packages/ComplexUpset/vignettes/Examples_R.html

library(ggplot2)
library(ComplexUpset)

temp <- df_analysis %>%
    filter(Age_Baseline >= 18) %>%
    select(`Smoking status` = Smoking_Cat_Bin, Hypertension = Hypertension_Status,
           Diabetes = Diabetes_Status, HIV = HIV_Status, Underweight = BMI_Underweight, Obesity = BMI_Obese,
           Age_Baseline, Sex_Use, BMI_Grade, SES_quintiles, Anaemia = Anaemia_Status,
           `Chronic lung disease` = ImpLungFn_Status, `Previous TB` = Comorb_TBEver) %>%
    mutate(`Smoking status` = ifelse(`Smoking status` == "Non smoker", "No", "Yes")) %>%
    filter(Age_Baseline >= 18)

temp %>%
    tbl_summary()


df_analysis %>%
    tabyl(Anaemia_Status)

conditions = c("Hypertension", "Diabetes",
               "HIV", "Anaemia", "Chronic lung disease",
               "Previous TB", "Underweight", "Obesity")

conditions

temp[conditions] = temp[conditions] == "Yes"
t(head(temp[conditions], 3))

upset(temp, conditions, name='conditions', width_ratio=0.1,
      min_size=5, keep_empty_groups=TRUE)

upset(temp, conditions, name='conditions', width_ratio=0.1,
      min_size=5, keep_empty_groups=TRUE,
      min_degree = 2)

size = get_size_mode('exclusive_intersection')

plot_incint_simple <- upset(temp, conditions, name='Conditions', width_ratio=0.1,
      min_size=10, keep_empty_groups=TRUE,
    base_annotations=list(
        'N (%) participants'=intersection_size(
            text_mapping=aes(
                label=paste0(!!size
                    ,
                    '\n(', !!upset_text_percentage(), ')'
                ),
                colour=ifelse(!!size > 200, 'on_bar', 'on_background'),
                y=ifelse(!!size > 200, !!size - 100, !!size)
            ),
            text=list(size=3)
        )
    ),
    set_sizes=(
        upset_set_size()
        + theme(axis.text.x=element_text(angle=90))
        + ylab('N with condition')
    )
)

plot_incint_simple

plot_intersectioninc <- upset(temp, conditions, name='conditions', width_ratio=0.25,
      min_size=20, keep_empty_groups=TRUE,
      mode='inclusive_intersection',
      #sort_intersections_by='ratio',
      base_annotations = list(
        'Intersection size'=(
            intersection_size()
            + ylab('N in intersection')
        )
    ),
      set_sizes=(
        upset_set_size(
            geom=geom_bar(),
            position='right'
        )
    ), 
    guides='over',
    themes=upset_default_themes(text=element_text(size=plot_fontsize)
    ),
      annotations = list(
          'Age'= (
            # note that aes(x=intersection) is supplied by default and can be skipped
            ggplot(mapping = aes(y=Age_Baseline))
            + geom_jitter(size = 0.2, col = "grey")
            + geom_boxplot(alpha=0., na.rm=TRUE, outlier.size = 0)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'Gender' = (
              ggplot(mapping=aes(fill=Sex_Use))
            + geom_bar(stat='count', position='fill')
            + ylab('Gender')
            + scale_fill_manual(values = c("#1A5222", "#34984F"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'BMI category' = (
              ggplot(mapping=aes(fill=BMI_Grade))
            + geom_bar(stat='count', position='fill')
            + ylab('BMI category') 
            + scale_fill_manual(values = c("#9B071B", "#e89a82", "#FBD7C0", "#e67963", "#c64b35"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'SES quintile' = (
              ggplot(mapping=aes(fill=SES_quintiles))
            + geom_bar(stat='count', position='fill')
            + ylab('SES quintile') 
            + scale_fill_manual(values = c("#0D47A1", "#2980B9", "#42A5F5", "#64B5F6", "#A7D6F8"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
          )
      ))




plot_intersectionexc <- upset(temp, conditions, name='conditions', width_ratio=0.25,
      min_size=10, keep_empty_groups=TRUE,
      mode='exclusive_intersection',
      #sort_intersections_by='ratio',
      base_annotations = list(
        'Intersection size'=(
            intersection_size()
            + ylab('N observations in intersection')
            + theme(text = element_text(size = plot_fontsize))
        )
    ),
      set_sizes=(
        upset_set_size(
            geom=geom_bar(),
            position='right'
        )
    ), 
    guides='over',
    themes=upset_default_themes(text=element_text(size=plot_fontsize)
    ),
      annotations = list(
          'Age'= (
            # note that aes(x=intersection) is supplied by default and can be skipped
            ggplot(mapping = aes(y=Age_Baseline))
            + geom_jitter(size = 0.2, col = "grey")
            + geom_boxplot(alpha=0., na.rm=TRUE, outlier.size = 0)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'Gender' = (
              ggplot(mapping=aes(fill=Sex_Use))
            + geom_bar(stat='count', position='fill')
            + ylab('Gender')
            + scale_fill_manual(values = c("#1A5222", "#34984F"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'BMI category' = (
              ggplot(mapping=aes(fill=BMI_Grade))
            + geom_bar(stat='count', position='fill')
            + ylab('BMI category') 
            + scale_fill_manual(values = c("#9B071B", "#e89a82", "#FBD7C0", "#e67963", "#c64b35"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
            ),
          'SES quintile' = (
              ggplot(mapping=aes(fill=SES_quintiles))
            + geom_bar(stat='count', position='fill')
            + ylab('SES quintile') 
            + scale_fill_manual(values = c("#0D47A1", "#2980B9", "#42A5F5", "#64B5F6", "#A7D6F8"), name = NULL)
            + theme(text = element_text(size = plot_fontsize))
          )
      ))


