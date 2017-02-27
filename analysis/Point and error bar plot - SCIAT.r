# title: Simple point and error bar plot of mean D-IRAP scores
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+


# dependencies ------------------------------------------------------------


library(tidyverse)
library(lme4)


# Data acquisition and processing -----------------------------------------


setwd("/Users/Ian/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1/")

data <- 
  read.csv("data/processed data/long SCIAT data.csv") %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)


# control rt for self reported racism -------------------------------------


# model rt controlling for modern racism scale total score with participant as random factor
model_1 <- lmer(rt ~ modern_racism_scale_total + (1 | participant), 
                data = data)

# add these residuals to the original data frame
model_1_residuals <- 
  as.data.frame(residuals(model_1, type = "response")) %>%
  rename(rt_residuals = `residuals(model_1, type = "response")`) %>%
  rownames_to_column()

data <- 
  data %>%
  rownames_to_column() %>%
  left_join(model_1_residuals, by = "rowname")


# reshape data for plotting -----------------------------------------------


data_to_plot_1 <-
  data %>%
  group_by(IAT_condition, block) %>%
  summarize(m_rt_residuals = mean(rt_residuals)) %>%
  rownames_to_column()

data_to_plot_2 <-
  data %>%
  group_by(IAT_condition, block) %>%
  summarize(error_rt_residuals = 1.96*sd(rt_residuals)/sqrt(length(rt_residuals))) %>%  # returns 95% CI interval width. Remove "1.96*" for standard error
  rownames_to_column() %>%
  ungroup() %>%
  select(-IAT_condition, -block)

data_to_plot <-
  left_join(data_to_plot_1, data_to_plot_2, by = "rowname") %>%
  select(-rowname)


# plot --------------------------------------------------------------------


# define error bar limits
limits <- aes(ymax = m_rt_residuals + error_rt_residuals, ymin = m_rt_residuals - error_rt_residuals)

# "dodge" the error bars so they don't overlap
dodge <- position_dodge(width = 0.2)  

# create plot
ggplot(data_to_plot, 
       aes(colour = IAT_condition, 
           y = m_rt_residuals, 
           x = block)) + 
  #ylim(-.5, 1) +  # set limits of Y axis here
  geom_point(shape = 15,  # squares for points
             position = dodge) + 
  geom_errorbar(limits, 
                width = 0.3, 
                position = dodge) +
  theme_classic() +  # employs a sans serif font. If you want stict APA format, comment out this line and uncomment apatheme below.
  scale_color_grey() + # greyscale colours
  ylab("Mean RT residuals")  # use some expressions magic to include subscript and italics
  
  
# save plot to disk
ggsave("analysis/residuals plot - SCIAT.pdf",
       device = "pdf",  # saves a PDF that is sized to fit Elsevier's 1.5 column width standard, ready for submission.
       width = 14,  
       height = 8,
       units = "cm")

