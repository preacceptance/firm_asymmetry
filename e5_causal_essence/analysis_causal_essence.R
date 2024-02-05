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
               'sjstats'
)

mediation <- FALSE

##================================================================================================================
                                ##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

# Read data
d_raw <- read.csv("data_raw.csv")
d_raw <- d_raw[2:nrow(d_raw), ] # Subset the last 320 responses (the first response was a test
d_raw <- d_raw[!(d_raw$ResponseId %in% c("R_22yIVBTyfkyawvN", "R_OIgZrYx8loxEjkZ")), ] # Exclude "accidental" participants: response IDs R_22yIVBTyfkyawvN and R_OIgZrYx8loxEjkZ

n_ss <- dim(d_raw)[1]; n_ss # Initial count

## -- Pre-process 

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 5))
colnames(d_subset) <- c('essence', 'condition', 'identity', 
                        'comp.errors', 'chk.errors')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_names <- d_raw[i,c(44, 46)][!is.na(d_raw[i,c(44, 46)])] #condition and vignette names 
    d_subset[i,1] <- strsplit(cond_names[[1]], "_")[[1]][1] 
    d_subset[i,2] <- strsplit(cond_names[[2]], "_")[[1]][1] 
    curr <- d_raw[i,c(26:27)][!is.na(d_raw[i,c(26:27)])] #for a given row, get only the non NA values
    d_subset[i,3] <- mean(as.numeric(curr[curr!= ""])) #get mean change value 
}

# Merge good data with first and last halves of the original data
d <- cbind(d_subset, d_raw[,19:ncol(d_raw)])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$essence_num <- ifelse(d$essence=='bad', -1, 1)
d$cond_num <- ifelse(d$condition=='deteriorate', -1, 1)

## -- Exclusions 

# Renumber exclusion checks 
for(i in 1:n_ss) {
  
  # Comprehension checks: before 
  if(d[i,"comp_before"]==1) {
    d[i,"comp.errors"] <- 0 
  } else {
    d[i,"comp.errors"] <- 1 
  }
  
  # Comprehension checks: after 
  if(d[i,"condition"]=="deteriorate") {
    if(d[i,"comp_after"]==3) {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 0 
    } else {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 1 
    }} 
  
  if(d[i,"condition"]=="improve") {
    if(d[i,"comp_after"]==2) {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 0 
    } else {
      d[i,"comp.errors"] <- d[i,"comp.errors"] + 1 
    }} 
  
  
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

# Perform attention 
d <- subset(d, d$chk.errors==0)
n_before_exclusions <- dim(d)[1]; n_before_exclusions

# Perform comprehension exclusions
final_data <- subset(d, d$comp.errors<=1) 
n_after_exclusions <- dim(final_data)[1]; n_after_exclusions
exclusion_percent <- ((n_ss - n_after_exclusions)/n_after_exclusions)*100

## -- Demographics 
d_age <- mean(as.numeric(final_data$age), na.rm = TRUE); d_age 
setDT(d)[, 100 * .N/nrow(d), by = gender] #female: 59.62% (coded as 2 in raw data)  

##=============================================================================================================
                                          ## MAIN ANALYSES ##
##================================================================================================================

# Essence manipulation check
t.test(subset(final_data,essence_num==-1)$original_essence_1,subset(final_data,essence_num==1)$original_essence_1,paired=F,var.equal=T) 
cohen.d(subset(final_data,essence_num==-1)$original_essence_1,subset(final_data,essence_num==1)$original_essence_1,paired=F,var.equal=T) 

sd(subset(final_data,essence_num==1)$original_essence_1)
sd(subset(final_data,essence_num==-1)$original_essence_1)


# Get correlation for the two identity measures 
cor.test(final_data$same_company_1, final_data$agree_person_1)
#change_total <- cbind(final_data$same_company_1, final_data$agree_person_1)
#psych::alpha(change_total)$total$raw_alpha #0.8713332 


# Anova, identity effect
mod <- aov(identity ~ essence*condition, data = final_data)
summary(mod)
anova_stats(mod)


# Good essence (before) 
t.test(subset(final_data, (essence_num==1 & cond_num==-1))$identity,
       subset(final_data, (essence_num==1 & cond_num==1))$identity,paired=F,var.equal=T) 

cohen.d(subset(final_data, (essence_num==1 & cond_num==-1))$identity,
       subset(final_data, (essence_num==1 & cond_num==1))$identity,paired=F,var.equal=T) 

sd(subset(final_data, (essence_num==1 & cond_num==-1))$identity) 
sd(subset(final_data, (essence_num==1 & cond_num==1))$identity)


# Bad essence (before) 
t.test(subset(final_data, (essence_num==-1 & cond_num==-1))$identity,
       subset(final_data, (essence_num==-1 & cond_num==1))$identity,paired=F,var.equal=T) 

cohen.d(subset(final_data, (essence_num==-1 & cond_num==-1))$identity,
        subset(final_data, (essence_num==-1 & cond_num==1))$identity,paired=F,var.equal=T) 

sd(subset(final_data, (essence_num==-1 & cond_num==-1))$identity) 
sd(subset(final_data, (essence_num==-1 & cond_num==1))$identity)


# Correlation, amount of positive and negative traits mediator
cor.test(final_data$remain_good_essence_1, final_data$remain_bad_essence_1)

# Combine measures into single average measure
combined_traits <- cbind(final_data$remain_good_essence_1, (100 - final_data$remain_bad_essence_1))
final_data$avg_traits <- rowMeans(combined_traits)

##=============================================================================================================
                                    ## MODERATION MEDIATION ANALYSIS ##
##================================================================================================================

if(mediation) {
    source("../common_functions/process.R") #import process function for mediation analysis 
    
    # Moderated Mediation: PROCESS Model 7  
    process(data = final_data,
            y = "identity", x = "essence_num", 
            m = c("reflect_essence_1", "avg_traits"),
            w = "cond_num", model = 7,
            center = 2,
            moments = 1, modelbt = 1,
            boot = 10000, seed = 654321) 
}



##=============================================================================================================
                                          ## PLOTTING ##
##================================================================================================================

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

legend_labels <- c("Moral Deterioration", "Moral Improvement")
x_labels <- c("Bad Essence", "Good Essence")

p2 <- ggplot(final_data,aes(x=factor(essence),y=identity, fill=factor(condition)),color=factor(condition)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p2 <- p2 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333", "#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.text.y = element_text(size=14)) +
  theme(plot.title = element_text(size=16, hjust=0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        legend.position="top") + 
  xlab("Firm Type") +
  ylab("Identity Change") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p2

ggsave(paste0('e3', ".png"), last_plot(), dpi = 300, width = 7, height = 6)


# Move files 
dir.create("analysis_plots") 
plot_files <- c("e3.png") #list.files(pattern = ".pdf") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

##=============================================================================================================
                                              ## END ##
##=============================================================================================================

