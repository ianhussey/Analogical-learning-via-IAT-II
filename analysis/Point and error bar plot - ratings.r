# title: Simple point and error bar plot of mean D-IRAP scores
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+


# dependencies ------------------------------------------------------------


library(tidyverse)
library(lme4)


# Data acquisition and processing -----------------------------------------


setwd("/Users/Ian/Dropbox/Work/Projects/Analogy/1 analogical learning via IAT with known stimuli/Experiment 1/")

data <- read.csv("data/processed data/long ratings data.csv")


# control ratings for self reported racism --------------------------------


# model ratings controlling for modern racism scale total score with participant as random factor
model_1 <- lmer(rating ~ modern_racism_scale_total + (1 | participant), 
                data = data)

# add these residuals to the original data frame
model_1_residuals <- 
  as.data.frame(residuals(model_1, type = "response")) %>%
  rename(ratings_residuals = `residuals(model_1, type = "response")`) %>%
  rownames_to_column()

data <- 
  data %>%
  rownames_to_column() %>%
  left_join(model_1_residuals, by = "rowname")


# reshape data for plotting -----------------------------------------------


data_to_plot_1 <-
  data %>%
  group_by(IAT_condition) %>%
  summarize(m_ratings_residuals = mean(ratings_residuals)) %>%
  rownames_to_column()

data_to_plot_2 <-
  data %>%
  group_by(IAT_condition) %>%
  summarize(error_ratings_residuals = 1.96*sd(ratings_residuals)/sqrt(length(ratings_residuals))) %>%  # returns 95% CI interval width. Remove "1.96*" for standard error
  select(-IAT_condition) %>%
  rownames_to_column()

data_to_plot <-
  left_join(data_to_plot_1, data_to_plot_2, by = "rowname") %>%
  select(-rowname)


# plot --------------------------------------------------------------------


# define error bar limits
limits <- aes(ymax = m_ratings_residuals + error_ratings_residuals, ymin = m_ratings_residuals - error_ratings_residuals)

# "dodge" the error bars so they don't overlap
dodge <- position_dodge(width = 0.05)  

# create plot
ggplot(data_to_plot, 
       aes(colour = IAT_condition, 
           y = m_ratings_residuals, 
           x = 1)) + 
  #ylim(-.5, 1) +  # set limits of Y axis here
  geom_point(shape = 15,  # squares for points
             position = dodge) + 
  geom_errorbar(limits, 
                width = 0.3, 
                position = dodge) +
  theme_classic() +  # employs a sans serif font. If you want stict APA format, comment out this line and uncomment apatheme below.
  #apatheme +
  scale_color_grey() + # greyscale colours
  ylab("Mean ratings residuals")  # use some expressions magic to include subscript and italics
  # scale_x_discrete("Trial type",
  #                  labels=c("1" = "men are\nmaculine",  # Name trial types here. NB "\n" creates a new line so that captions are readable
  #                           "2" = "men are\nfeminine",
  #                           "3" = "women are\nmasculine",
  #                           "4" = "women are\nfeminine"))
  
# save plot to disk
ggsave("analysis/residuals plot - ratings.pdf",
       #path = "c:/mydocuments/Desktop/"  # windows
       device = "pdf",  # saves a PDF that is sized to fit Elsevier's 1.5 column width standard, ready for submission.
       width = 14,  
       height = 8,
       units = "cm")

