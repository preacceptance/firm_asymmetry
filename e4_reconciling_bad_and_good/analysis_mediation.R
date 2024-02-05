## Corporate Essence Analysis - Causal Essence 


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
               'sentimentr', #sentiment analysis
               'filesstrings', #move files around 
               'effsize', #effect size package
               'effectsize',
               'ggpubr',
               'sjstats',
               'interactions'
)

mediation <- TRUE 

##================================================================================================================
                                ##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

# Read data
d_raw <- read.csv("data_mediation.csv")
d_raw <- subset(d_raw, d_raw$Finished==1)
n_ss <- dim(d_raw)[1]; n_ss

## -- Pre-process 

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 10))
colnames(d_subset) <- c('vignette', 'cond', 'quest', 'dv', 'essence', 'teleology', 
                        'category', 'comp1', 'comp2', 'comp3')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_names <- d_raw[i,c(220:235)][!is.na(d_raw[i,c(220:235)])] 
    cond_names <- cond_names[cond_names!= ""]
    d_subset[i,1] <- strsplit(cond_names, ",")[[1]][1]
    d_subset[i,2] <- strsplit(cond_names, ",")[[1]][2]
    d_subset[i,3] <- strsplit(cond_names, ",")[[1]][3]
    curr <- d_raw[i,c(18:193)][!is.na(d_raw[i,c(18:193)])]   
    if(d_subset[i,3] == "PredictionMeasures") {
        d_subset[i,4:10] <- curr
    }
    else {
      d_subset[i, 4:5] <- curr[1:2]
      d_subset[i, 6] <- curr[4]
      d_subset[i, 7] <- curr[3]
      d_subset[i, 8:10] <- curr[5:7]
    }
}

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,12:17], d_subset, d_raw[,194:215])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$cond_num <- ifelse(d$cond=='Deteriorate', -1, 1)
d$quest_num <- ifelse(d$quest=='PredictionMeasures', -1, 1)

## -- Exclusions 

# Renumber exclusion checks 
for(i in 1:n_ss) {
  
  # Comprehension checks: valence name  
  if(d[i,"cond"]=='Deteriorate') { 
    if((d[i,"comp2"]==2) & (d[i,"comp3"]==5)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp2"]!=2) & (d[i,"comp3"]==5)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp2"]==2) & (d[i,"comp3"]!=5)) {
      d[i,"comp.errors"] <- 1 
    } else {
      d[i,"comp.errors"] <- 2 
    }
  } else if(d[i,"cond"]=='Improve') {
    if((d[i,"comp2"]==5) & (d[i,"comp3"]==2)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp2"]!=5) & (d[i,"comp3"]==2)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp2"]==5) & (d[i,"comp3"]!=2)) {
      d[i,"comp.errors"] <- 1 
    } else { 
      d[i,"comp.errors"] <- 2 
    }
  }
  
  # Comprehension checks: question name
  if(d[i,"quest"]=="IdentityMeasures") {
    if(d[i,"comp1"]==1) {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 0
    } else {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 1
    }
  } else if(d[i,"quest"]=="PredictionMeasures") {
    if(d[i,"comp1"]==2) {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 0
    } else {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 1
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

exclusion_percent <- ((n_before_exclusions - n_after_exclusions)/n_after_exclusions)*100

## -- Demographics 
d_age <- mean(as.numeric(final_data$age), na.rm = TRUE); d_age 
setDT(d)[, 100 * .N/nrow(d), by = gender] 

## Psychopathy scale 
psych <- cbind(final_data[, c(17:31)]) 
psych::alpha(psych) 
mean(unlist(psych)) 

final_data$psychopathy <- rowMeans(psych)

## Create low and high psychopathy categories, for later plots
final_data$psych_cat <- ifelse(final_data$psychopathy>50, "High Psychopathy", "Low Psychopathy")
final_data$psych_cat_n <- ifelse(final_data$psychopathy=="High Psychopathy",2,1)

## check even assignment
table(final_data$cond, final_data$quest)

# set to true if you want to run mediation analyses
mediation <- FALSE

## moderation?
final_data$int_term <- final_data$cond_num*final_data$quest_num
if (mediation) {
  source("../common_functions/process.R") #import process function for mediation analysis 
  process(data = final_data, 
          y = "dv", x = "int_term", w = "psychopathy",
          model = 1, effsize = 1, total = 1, stand = 1, 
          boot = 10000 , modelbt = 1, seed = 654321) 
}
  
##=============================================================================================================
## IDENTITY ANALYSES ##
##================================================================================================================

identity_data <- as.data.frame(subset(final_data,quest=="IdentityMeasures"))

# Check for interactions
mod_identity <- aov(dv ~ as.factor(vignette)*as.factor(cond), data=identity_data)
summary(mod_identity) # no interaction

## T-tests

# identity
t.test(subset(identity_data,cond_num==-1)$dv,subset(identity_data,cond_num==1)$dv,paired=F,var.equal=T, na.rm = TRUE)
cohen.d(subset(identity_data,cond_num==-1)$dv,subset(identity_data,cond_num==1)$dv,paired=F,var.equal=T, na.rm = TRUE)

# essence
t.test(subset(identity_data,cond_num==-1)$essence,subset(identity_data,cond_num==1)$essence,paired=F,var.equal=T, na.rm = TRUE)
cohen.d(subset(identity_data,cond_num==-1)$essence,subset(identity_data,cond_num==1)$essence,paired=F,var.equal=T, na.rm = TRUE)

# category
t.test(subset(identity_data,cond_num==-1)$category,subset(identity_data,cond_num==1)$category,paired=F,var.equal=T, na.rm = TRUE)
cohen.d(subset(identity_data,cond_num==-1)$category,subset(identity_data,cond_num==1)$category,paired=F,var.equal=T, na.rm = TRUE)

# teleology
t.test(subset(identity_data,cond_num==-1)$teleology,subset(identity_data,cond_num==1)$teleology,paired=F,var.equal=T, na.rm = TRUE)
cohen.d(subset(identity_data,cond_num==-1)$teleology,subset(identity_data,cond_num==1)$teleology,paired=F,var.equal=T, na.rm = TRUE)

if (mediation) {
  source("../common_functions/process.R") #import process function for mediation analysis 
  process(data = identity_data, 
          y = "dv", x = "cond_num", m = c("essence", "category", "teleology"),
          model = 4, effsize = 1, total = 1, stand = 1, 
          boot = 10000 , modelbt = 1, seed = 654321) # Indirect effect of X on Y using R: b = -7.39, SE = 1.86, 95% CI [-11.338, -4.077] 
}

# anova version
mean(identity_data$psychopathy)
mod_psych <- aov(dv ~ psychopathy*cond, data = identity_data)
summary(mod_psych) 
anova_stats(mod_psych)

##=============================================================================================================
## PREDICTION ANALYSES ##
##================================================================================================================

predict_data <- as.data.frame(subset(final_data,quest=="PredictionMeasures"))

# Check for interactions
mod_prediction <- aov(dv ~ as.factor(vignette)*as.factor(cond), data=predict_data)
summary(mod_prediction) # no interaction

## T-tests

# prediction
t.test(subset(predict_data,cond_num==-1)$dv,subset(predict_data,cond_num==1)$dv,paired=F,var.equal=T, na.rm = TRUE)
cohen.d(subset(predict_data,cond_num==-1)$dv,subset(predict_data,cond_num==1)$dv,paired=F,var.equal=T, na.rm = TRUE)

## mediation

if(mediation) {
  process(data = predict_data, 
          y = "dv", x = "cond_num", m = c("essence", "category", "teleology"),
          model = 4, effsize = 1, total = 1, stand = 1, 
          boot = 10000 , modelbt = 1, seed = 654321) # Indirect effect of X on Y using R: b = -7.39, SE = 1.86, 95% CI [-11.338, -4.077] 
}

## moderation
fit <- lm(dv ~ psychopathy * cond_num, data = predict_data)
summary(fit)

# perform the floodlight analysis using the Johnson-Neyman technique
library(jtools)
library(ggplot2)

p0 <- johnson_neyman(fit, pred = "cond_num", modx = "psychopathy")

# Set the colors for the lines and error regions
line_colors <- c("light gray", "black")
error_colors <- c("lightgray", "black")

# Modify the plot with the customized colors
p0[["plot"]] +
  labs(x = "Psychopathic Ascription", y = "Valence Condition", title = "") +
  scale_color_manual(values = line_colors) +
  #geom_ribbon(aes(fill = factor(term)), alpha = 0.5, color = NA) +
  scale_fill_manual(values = error_colors)+
  geom_vline(aes(xintercept = y$jn_point), linetype = "dotted", color = 'black')+
  theme(text = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.position="top") 

ggsave(paste0('e2_jn', "png"), last_plot(), dpi = 300, width = 7, height = 8)

# Move files 
dir.create("analysis_plots") 
plot_files <- c("e2_jn.png")
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

mod_psych <- aov(dv ~ psychopathy*cond, data = predict_data)
summary(mod_psych) 
anova_stats(mod_psych)

# followup tests
## 1. Improvement condition (negative predictions, or "reverting to bad majority") 
lm_imp_psych <- lm(dv ~ psychopathy, data = subset(predict_data, cond_num==1))
summary(lm_imp_psych) 

pred_imp <- subset(predict_data, cond_num==1)
plot(pred_imp$psychopathy, pred_imp$dv)
abline(lm(pred_imp$dv ~ pred_imp$psychopathy))


## 2. Deterioration condition (positive predictions, or "reverting to good majority") 

lm_det_psych <- lm(dv ~ psychopathy, data = subset(predict_data, cond_num==-1))
summary(lm_det_psych) 

pred_det <- subset(predict_data, cond_num==-1)
plot(pred_det$psychopathy, pred_det$dv)
abline(lm(pred_det$dv ~ pred_det$psychopathy))

##=============================================================================================================
## PLOTTING ##
##================================================================================================================

# Define variables 
x_labels <- c("Deterioration", "Improvement")

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

p1 <- ggplot(identity_data,aes(x=factor(cond),y=dv)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#cccccc", "#333333"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 20),
        legend.position="top") + 
  xlab("Condition") +
  ylab("Identity Change") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p1


x_labels <- c("Clothing", "Cosmetics", "Apparel", "Mining", 
              "Technology", "Petroleum", "Tobacco", "Makeup") #set category/industry names
legend_labels <- c("Deterioration", "Improvement")

p2 <- ggplot(identity_data,aes(x=factor(vignette),y=dv, fill=factor(cond)),color=factor(cond)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333","#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1.0, hjust = 1),
        legend.position="top") + 
  xlab("Firm Type") +
  ylab("") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p2


x_labels <- c("Deterioration", "Improvement")
figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, vjust = 1.0, hjust = 0.5)
ggsave(paste0('e2', ".png"), last_plot(), dpi = 300, width = 12, height = 6)

p3 <- ggplot(predict_data,aes(x=factor(cond),y=dv)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p3 <- p3 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#cccccc", "#333333"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 20),
        legend.position="top") + 
  xlab("Condition") +
  ylab("Will Revert to Past Behavior") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p3


x_labels <- c("Clothing", "Cosmetics", "Apparel", "Mining", 
              "Technology", "Petroleum", "Tobacco", "Makeup") #set category/industry names
legend_labels <- c("Deterioration", "Improvement")

p4 <- ggplot(predict_data,aes(x=factor(vignette),y=dv, fill=factor(cond)),color=factor(cond)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p4 <- p4 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333","#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1.0, hjust = 1),
        legend.position="top") + 
  xlab("Firm Type") +
  ylab("") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p4

figure <- ggarrange(p3, p4, nrow = 1, ncol = 2, vjust = 1.0, hjust = 0.5)
ggsave(paste0('e2_2', "png"), last_plot(), dpi = 300, width = 12, height = 6)

# Move files 
dir.create("analysis_plots") 
plot_files <- c("e2.png", "e2_2.png")
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

##=============================================================================================================
## END ##
##=============================================================================================================

