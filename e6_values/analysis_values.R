## Corporate Essence Analysis - Values  


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
               'BayesFactor', #get the Bayes factor, a likelihood ratio 
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
n_ss <- dim(d_raw)[1] # Initial count

## -- Pre-process  

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 9))
colnames(d_subset) <- c('vig_name', 'cond_name', 'identity', 'wtp', 'politics',
                        'comp.1', 'comp.2', 'comp.errors', 'chk.errors')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_names <- d_raw[i,54][!is.na(d_raw[i,54])] #condition and vignette names 
    d_subset[i,1] <- strsplit(cond_names[[1]], ",")[[1]][1] #vignette name 
    d_subset[i,2] <- strsplit(cond_names[[1]], ",")[[1]][2] #condition name 
    curr <- d_raw[i,25:44][!is.na(d_raw[i,25:44])] #for a given row, get only the non NA values
    d_subset[i,3:7] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
}

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,19:24], d_subset, d_raw[,45:54])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$condition <- ifelse(d$cond_name=='Liberal', -1, 1)
vignette_list <- list("YoungScouting" = 1, "SciTech" = 2) 

d$vignette <- NA 
for(i in 1:dim(d)[1]) { 
  d$vignette[i] <- as.numeric(vignette_list[[d$vig_name[i]]]) 
} 

## -- Exclusions 

# Renumber exclusion checks 
for(i in 1:n_ss) {
  
  # Comprehension checks 
  if(d[i,"cond_name"]=='Liberal') { 
    if((d[i,"comp.1"]==2) & (d[i,"comp.2"]==3)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp.1"]!=2) & (d[i,"comp.2"]==3)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp.1"]==2) & (d[i,"comp.2"]!=3)) {
      d[i,"comp.errors"] <- 1 
    } else {
      d[i,"comp.errors"] <- 2 
    }
  } else if(d[i,"cond_name"]=='Conservative') {
    if((d[i,"comp.1"]==2) & (d[i,"comp.2"]==1)) {
      d[i,"comp.errors"] <- 0 
    } else if((d[i,"comp.1"]!=2) & (d[i,"comp.2"]==1)) {
      d[i,"comp.errors"] <- 1 
    } else if((d[i,"comp.1"]==2) & (d[i,"comp.2"]!=1)) {
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

# Exclusions 
final_data <- subset(d, d$comp.errors<=1) 
n_after_exclusions <- dim(final_data)[1]; n_after_exclusions
exclusion_percent <- ((n_ss - n_after_exclusions)/n_after_exclusions)*100

## -- Demographics 

d_age <- mean(as.numeric(final_data$age), na.rm = TRUE); d_age 
setDT(final_data)[, 100 * .N/nrow(final_data), by = gender] #female: 62.33% (coded as 1) 

##=============================================================================================================
                                          ## MAIN ANALYSES ##
##================================================================================================================

# Liberal or conservative leaning? 
pol_mean <- mean(final_data$politics); pol_mean 
pol_sd <- sd(final_data$politics); pol_sd 
pol_median <- median(final_data$politics); pol_median


# Anova
mod <- aov(identity ~ condition*politics, data = final_data)
summary(mod)
eta_squared(mod) #0.05


# Regressions 

# "Change toward liberal" condition 
lm_lib_change <- lm(identity ~ politics, data = subset(final_data, condition==-1))
summary(lm_lib_change)

lm_lib_wtp <- lm(wtp ~ politics, data = subset(final_data, condition==-1))
summary(lm_lib_wtp) 

# "Change toward conservative" condition 
lm_con_change <- lm(identity ~ politics, data = subset(final_data, condition==1))
summary(lm_con_change) 

lm_con_wtp <- lm(wtp ~ politics, data = subset(final_data, condition==1))
summary(lm_con_wtp) 


##=============================================================================================================
                                      ## PLOTTING ##
##================================================================================================================

# Recode Political Values (Scale: 0-100 is 101 values, so midpoint of scale is 50) 
pol_midpoint <- 50 #get the midpoint of the scale 

for(i in 1:nrow(final_data)) {
  if(final_data$politics[[i]] < pol_midpoint) { #if liberal-leaning, code as 1 
      final_data$politics_cat[[i]] <- 1
  } else if(final_data$politics[[i]] >= pol_midpoint) { #if conservative leaning, code as 2 
     final_data$politics_cat[[i]] <- 2
  }
}

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

x_labels <- c("Change to Conservative", "Change to Liberal")
legend_labels <- c("Liberal Participants", "Conservative Participants")
x_label <- "Political Change"
y_label_1 <- "No Longer the Same Company"
title1 <- "Identity Change"

p1 <- ggplot(final_data,aes(x=factor(cond_name),y=identity, fill=factor(politics_cat)),color=factor(politics_cat)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333", "#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        plot.title = element_text(size=25),
        legend.position="top") + 
  xlab("") +
  ylab("") +
  ggtitle(title1) +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p1


y_label_2 <- "Willingness to Purchase"
title2 <- "Willingness to Purchase"

p2 <- ggplot(final_data,aes(x=factor(cond_name),y=wtp, fill=factor(politics_cat)),color=factor(politics_cat)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333", "#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        plot.title = element_text(size=25),
        legend.position="top") + 
  xlab("") +
  ylab("") +
  ggtitle(title2) +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p2

figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5)
annotate_figure(figure, left = text_grob("Mean", color="black", face ="plain",size=25, rot=90),
                bottom = text_grob(x_label, color="black", face ="plain",size=25, vjust=-0.4))

ggsave(paste0('e4', ".png"), last_plot(), dpi = 300, width = 12, height = 6)


# Move files 
dir.create("analysis_plots") 
plot_files <- c("e4.png") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 


##=============================================================================================================
                                              ## END ##
##=============================================================================================================

