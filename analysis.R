## @knitr analysis-setup
library(ggplot2)
library(magrittr)
individual <- readr::read_csv(here::here("data", "individual.csv")) %>% 
  dplyr::select(stemDiameter, height, growthForm)

## @knitr analysis-filter-data
analysis_df <- individual %>% 
  dplyr::filter(!is.na(growthForm), growthForm != "liana")

## @knitr analysis-set-factor-levels
gf_levels <- table(analysis_df$growthForm) %>% 
  sort() %>% 
  names()

analysis_df %<>% 
  dplyr::mutate(growthForm = factor(growthForm,
                                    levels = gf_levels))

## @knitr analysis-fig1-barplot
analysis_df %>% 
  ggplot(aes(y = growthForm, colour = growthForm, fill = growthForm)) +
  geom_bar(alpha = 0.5, show.legend = FALSE)

## @knitr analysis-fig2-violinplots
analysis_df %>% 
  tidyr::pivot_longer(cols = c(stemDiameter, height),
                      values_to = "value", 
                      names_to = "var") %>% 
  ggplot(aes(x = log(value), y = growthForm, colour = growthForm, fill = growthForm)) +
  geom_violin(alpha = 0.5, trim = T, show.legend = F) +
  geom_boxplot(alpha = 0.7, show.legend = F) +
  facet_grid(~var)

## @knitr analysis-lm-overall

lm_overall <- lm(log(stemDiameter)~log(height), analysis_df)

lm_overall %>% 
  broom::glance()
lm_overall %>% 
  broom::tidy()


## @knitr analysis-lm-fg3-overall
analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stemDiameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")


## @knitr analysis-lm-growth
lm_growth <- lm(log(stemDiameter) ~ log(height) * growthForm, analysis_df)
lm_growth %>% 
  broom::glance()
lm_growth %>% 
  broom::tidy()

## @knitr analysis-lm-fig4-growth
analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stemDiameter), colour = growthForm)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

