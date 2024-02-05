## ================================================================================================================
##                                 
## ================================================================================================================
##                                          DATA ANALYSIS | Firm Asymmetry               
## ================================================================================================================
## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
library(ggpubr)
library(dplyr)
library(sjstats)
library(ggpubr)
library(grid)
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyr',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'emmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'filesstrings',    # create and move files
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',        # get Cramer's V
               'rstatix',
               'effects',
               'effsize'
)
## ================================================================================================================
##                                                  EXCLUSIONS                
## ================================================================================================================

## read in data: 
# set working directory to current directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d <- read.csv('data.csv') 

d |>
  filter(Finished == 1, att1 == 2, att2 == 2) -> d

n_original <- nrow(d); n_original

d |>
  filter(comp1 == 2) -> d

n <- nrow(d) 

num_excluded <- n_original - n

## ================================================================================================================
##                                            PARTICIPANT CHARACTERISTICS                 
## ================================================================================================================

mean(as.numeric(d$age))

prop_male <- prop.table(table(d$gender))[[1]]; prop_male

## ================================================================================================================
##                                                PRE-PROCESSING          
## ================================================================================================================

d |>
  select(elon_musk_first_10, warren_buffet_second_10) |>
  gather(key = "CEO", value = "Rating", elon_musk_first_10, warren_buffet_second_10) |>
  mutate(Rating = as.numeric(Rating),
         Condition = "Musk First",
         CEO = ifelse(CEO == "elon_musk_first_10", "Musk", "Buffet")) |>
  drop_na() -> musk_first

d |>
  select(elon_musk_second_10, warren_buffet_first_10) |>
  gather(key = "CEO", value = "Rating", elon_musk_second_10, warren_buffet_first_10) |>
  mutate(Rating = as.numeric(Rating),
         Condition = "Musk Second",
         CEO = ifelse(CEO == "elon_musk_second_10", "Musk", "Buffet")) |>
  drop_na() -> musk_second

d <- rbind(musk_first, musk_second)

rm(musk_first, musk_second)

# ANOVA Analysis
a <- aov(Rating ~ as.factor(CEO) * as.factor(Condition), data = d)
summary(a)
anova_stats(a)

# t-tests

## Musk and Buffet 
t.test(d[d$CEO == "Musk",]$Rating, d[d$CEO == "Buffet",]$Rating)
cohen.d(d[d$CEO == "Musk",]$Rating, d[d$CEO == "Buffet",]$Rating)

## Musk and Buffet are conservative
t.test(d[d$CEO == "Musk",]$Rating, mu = 50)
t.test(d[d$CEO == "Buffet",]$Rating, mu = 50)
