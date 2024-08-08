## Corporate Essence Analysis - Twitter Acquisition


# Clear working directory
remove(list = ls())

# Set working directory to current file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import packages 
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('openxlsx', #open Excel spreadsheets 
               'ggplot2', #plot stuff 
               'ggimage', #add images to ggplot legend 
               'pbkrtest', 
               'lme4', #linear regression
               'data.table', #get percentage of values in a column 
               'dplyr', #data manipulation 
               'jtools', 
               'psych', 
               'plotrix', #used for std.error() to get standard error 
               'cowplot', #reads image files into R; add images as x-axis labels
               'grid', #used for rasterGrob()
               'png', #read PNG files
               'filesstrings', #move files around 
               'effsize', #effect size package
               'effectsize',
               'ggpubr'
)


##================================================================================================================
##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

# Read data
d_raw <- read.csv("data_raw.csv")

d_raw <- d_raw[2:nrow(d_raw), ] # Subset the last 320 responses (the first response was a test)
d_raw <- d_raw[!(d_raw$ResponseId %in% c("R_3KT2dJa9JNaGXoA", "R_AM9X1caZ4FVC5DX", "R_rkwlVeAVc5OTGmZ", "R_wZT3PBDk0xTY0NP")), ] # Exclude "accidental" participants
n_ss <- dim(d_raw)[1]; n_ss

## -- Pre-process  

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 3))
colnames(d_subset) <- c('cond_name', 'deactivate', 'essence')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_name <- d_raw[i,40][!is.na(d_raw[i,40])] #condition name
    d_subset[i,1] <- cond_name #condition name 
    curr <- d_raw[i,24:27][!is.na(d_raw[i,24:27])] #for a given row, get only the non NA values
    d_subset[i,2:3] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
}

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,18:23], d_subset, d_raw[,28:40])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$condition <- ifelse(d$cond_name=='buffett', -1, 1)


## -- Exclusions 

# Renumber exclusion checks 
for(i in 1:n_ss) {
  
  # Comprehension checks 
  if(d[i,"cond_name"]=='musk') { 
    if((d[i,"comp_check_1"]==1) & (d[i,"comp_check_2"]==4)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp_check_1"]!=1) & (d[i,"comp_check_2"]==4)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp_check_1"]==1) & (d[i,"comp_check_2"]!=4)) {
      d[i,"comp.errors"] <- 1 
    } else {
      d[i,"comp.errors"] <- 2 
    }
  } else if(d[i,"cond_name"]=='buffett') {
    if((d[i,"comp_check_1"]==2) & (d[i,"comp_check_2"]==1)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp_check_1"]!=2) & (d[i,"comp_check_2"]==1)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp_check_1"]==2) & (d[i,"comp_check_2"]!=1)) {
      d[i,"comp.errors"] <- 1 
    } else { 
      d[i,"comp.errors"] <- 2 
    }
  }
  
  # Attention checks 
  
  # Perform first round of attention checks
  d[i, "chk.errors"] <- ifelse(((d[i, "attn_check_1"] == 2) & (d[i, "attn_check_2"] == 2)), 0, 1) 
  
  # Perform second round of attention checks, if they failed the first
  if(d[i, "chk.errors"] != 0) {
    if(((d[i,"attn_check_3_1"]==15) & (d[i,"attn_check_3_2"] > d[i,"attn_check_3_1"]) & 
        (d[i,"attn_check_3_3"] > d[i,"attn_check_3_2"]) & (d[i,"attn_check_3_2"]%%10 == 0)) & 
       (d[i,"attn_check_4"]==1)) { 
      d[i,"chk.errors"] <- 0 
    } else if(((d[i,"attn_check_3_1"]==15) & (d[i,"attn_check_3_2"] > d[i,"attn_check_3_1"]) & 
               (d[i,"attn_check_3_3"] > d[i,"attn_check_3_2"]) & (d[i,"attn_check_3_2"]%%10 == 0)) & 
              (d[i,"attn_check_4"]!=1)) {
      d[i,"chk.errors"] <- 1 
    } else if(((d[i,"attn_check_3_1"]!=15) | (d[i,"attn_check_3_2"] <= d[i,"attn_check_3_1"]) | 
               (d[i,"attn_check_3_3"] <= d[i,"attn_check_3_2"]) | (d[i,"attn_check_3_2"]%%10 != 0)) & 
              (d[i,"attn_check_4"]==1)) { 
      d[i,"chk.errors"] <- 1 
    } else {
      d[i,"chk.errors"] <- 2 
    }
  }
} 

# Perform attention exclusions 
d <- subset(d, d$chk.errors==0)
n_before_exclusions <- dim(d)[1]; n_before_exclusions

# Perform comprehension exclusions 
final_data <- subset(d, d$comp.errors<=1) 
n_after_exclusions <- dim(final_data)[1]; n_after_exclusions
exclusion_percent <- ((n_ss - n_after_exclusions)/n_after_exclusions)*100

## -- Demographics 
d_age <- mean(as.numeric(final_data$age), na.rm = TRUE); d_age 
setDT(final_data)[, 100 * .N/nrow(final_data), by = gender] #female: 47.63% (coded as 2 in raw data) 

##=============================================================================================================
                                               ## ANALYSES ##
##=============================================================================================================

# Liberal or conservative leaning? 
pol_mean <- mean(final_data$politics); pol_mean
pol_sd <- sd(final_data$politics); pol_sd
pol_median <- median(final_data$politics); pol_median

### --- Identity Anova
mod <- aov(deactivate~condition*politics_1, data=final_data)
summary(mod)
eta_squared(mod)


# T-tests
t.test(subset(final_data,condition==-1)$deactivate,subset(final_data,condition==1)$deactivate,paired=F,var.equal=T)
cohen.d(subset(final_data,condition==-1)$deactivate,subset(final_data,condition==1)$deactivate,var.equal=TRUE, paired=FALSE)

t.test(subset(final_data,condition==-1)$essence,subset(final_data,condition==1)$essence,paired=F,var.equal=T)
cohen.d(subset(final_data,condition==-1)$essence,subset(final_data,condition==1)$essence,var.equal=TRUE, paired=FALSE)

# Regressions 

# "Change toward liberal" condition (buffett) 
lm_lib_change <- lm(deactivate ~ politics_1, data = subset(final_data, condition==-1))
summary(lm_lib_change) 

# "Change toward conservative" condition (musk)
lm_con_change <- lm(deactivate ~ politics_1, data = subset(final_data, condition==1))
summary(lm_con_change) 


### -- Essence Anova
mod <- aov(essence~condition*politics_1, data=final_data)
summary(mod)
eta_squared(mod)

# "Change toward liberal" condition (buffett) 
lm_lib_change <- lm(essence ~ politics_1, data = subset(final_data, condition==-1))
summary(lm_lib_change) 

# "Change toward conservative" condition (musk)
lm_con_change <- lm(essence ~ politics_1, data = subset(final_data, condition==1))
summary(lm_con_change) 

##=============================================================================================================
                                  ## MODERATION & MEDIATION ANALYSES ##
##=============================================================================================================

if(mediation) {
  
  source("process.R") #import process function for mediation analysis 
  final_df <- as.data.frame(final_data)
  
  process(data=final_df,
          y="deactivate", x="politics_1",
          w="condition",
          model = 1, center = 2, moments = 1, jn = 1, 
          modelbt = 1, boot = 10000, seed = 654321)
  
  
  final_data$politics.cent <- final_data$politics_1 - mean(final_data$politics_1) #centralizing politics around the mean
  final_data$politics.mid <- final_data$politics_1 - 50 #around midpoint=50
  
  process(data=final_df,
          y="deactivate", x="politics.cent",
          w="condition",
          model = 1, center = 2, moments = 1, jn = 1, 
          modelbt = 1, boot = 10000, seed = 654321)
  
  process(data=final_df,
          y="deactivate", x="politics.mid",
          w="condition",
          model = 1, center = 2, moments = 1, jn = 1, 
          modelbt = 1, boot = 10000, seed = 654321)
  
  process(data = final_df,
          y = "deactivate", x = "condition", 
          m = "essence",
          model = 4,
          center = 2,
          moments = 1, modelbt = 1,
          boot = 10000, seed = 654321)
  
  process(data = final_df,
          y = "deactivate", x = "condition", 
          m = "essence", w = "politics_1",
          model = 7,
          center = 2,
          moments = 1, modelbt = 1,
          boot = 10000, seed = 654321)
  
  process(data = final_df,
          y = "deactivate", x = "impDet", 
          m = "essence",
          model = 4,
          center = 2,
          moments = 1, modelbt = 1,
          boot = 10000, seed = 654321)
  
}


##=============================================================================================================
                                            ## PLOTTING ##
##=============================================================================================================

# Recode Political Values (Scale: 0-100 is 101 values, so midpoint of scale is 50) 
pol_midpoint <- 50 #get the midpoint of the scale 

for(i in 1:nrow(final_data)) {
  if(final_data$politics_1[[i]] < pol_midpoint) { #if liberal-leaning, code as 1 
    final_data$politics_cat[[i]] <- 1
  } else if(final_data$politics_1[[i]] >= pol_midpoint) { #if conservative leaning, code as 2 
    final_data$politics_cat[[i]] <- 2
  }
}


x_labels <- c("Change to Liberal", "Change to Conservative")
legend_labels <- c("Liberal Participants", "Conservative Participants")
x_label <- "Political Change"
y_label <- "Would Deactivate Account"

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

p1 <- ggplot(final_data, aes(x = factor(condition), y = deactivate, fill = factor(politics_cat))) + 
  theme_bw() + coord_cartesian(ylim = c(1, 100)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") +
  scale_x_discrete(labels = x_labels) +
  scale_fill_manual(values = c("#333333", "#cccccc"), name = "", labels = legend_labels, guide = guide_legend(reverse = FALSE)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(text = element_text(size = 20), axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        plot.title = element_text(size = 25),
        legend.position = "top") + 
  xlab(x_label) +
  ylab(y_label) +
  geom_bar(position = "dodge", stat = "summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))

p1

ggsave(paste0('e5b', ".png"), last_plot(), dpi = 300, width = 7, height = 6)

# Move files 
dir.create("analysis_plots") 
plot_files <- c("e5b.png") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

##=============================================================================================================
                                                ## END ##
##=============================================================================================================


