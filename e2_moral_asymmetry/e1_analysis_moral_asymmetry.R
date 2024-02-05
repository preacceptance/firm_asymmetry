## Corporate Essence Analysis - Moral Asymmetry 


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
               'ggpubr'
)

##================================================================================================================
                                ##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

pwr.anova.test(k = 2,f = 6.35, sig.level = 0.001, power = 80)

# Read data
d_raw <- read.csv("data_raw.csv")

# Initial count
n_ss <- dim(d_raw)[1]; n_ss

## -- Pre-process  

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss, 9))
colnames(d_subset) <- c('vig_name', 'cond_name', 'change.1', 'change.2', 
                        'comp.1', 'comp.2', 'change.mean', 'comp.errors', 'chk.errors')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_names <- d_raw[i,97][!is.na(d_raw[i,97])] #condition and vignette names 
    d_subset[i,1] <- strsplit(cond_names[[1]], ",")[[1]][1] #vignette name 
    d_subset[i,2] <- strsplit(cond_names[[1]], ",")[[1]][2] #condition name 
    curr <- d_raw[i,23:86][!is.na(d_raw[i,23:86])] #for a given row, get only the non NA values
    d_subset[i,3:6] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    d_subset[i,7] <- mean(c(d_subset[i,3], d_subset[i,4])) #get mean change value 
}

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,19:22], d_subset, d_raw[,87:97])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$condition <- ifelse(d$cond_name=='Deteriorate', -1, 1)
vignette_list <- list("WestWinter" = 1, "BrighterLab" = 2, "WonderlandClothing" = 3, "AccessMine" = 4,
                      "Tecigo" = 5, "RoyalOil" = 6, "BluewaterTobacco" = 7, "DocLips" = 8)
d$vignette <- NA 
for(i in 1:dim(d)[1]) { 
    d$vignette[i] <- as.numeric(vignette_list[[d$vig_name[i]]]) 
}

## -- Exclusions 

# Renumber exclusion checks 

for(i in 1:n_ss) {
    
    # Comprehension checks 
    if(d[i,"cond_name"]=='Deteriorate') { 
        if((d[i,"comp.1"]==2) & (d[i,"comp.2"]==4)) {
            d[i,"comp.errors"] <- 0 
        } else if((d[i,"comp.1"]!=2) & (d[i,"comp.2"]==4)) {
            d[i,"comp.errors"] <- 1 
        } else if((d[i,"comp.1"]==2) & (d[i,"comp.2"]!=4)) {
            d[i,"comp.errors"] <- 1 
        } else {
            d[i,"comp.errors"] <- 2 
        }
    } else if(d[i,"cond_name"]=='Improve') {
        if((d[i,"comp.1"]==4) & (d[i,"comp.2"]==2)) {
            d[i,"comp.errors"] <- 0 
        } else if((d[i,"comp.1"]!=4) & (d[i,"comp.2"]==2)) {
            d[i,"comp.errors"] <- 1 
        } else if((d[i,"comp.1"]==4) & (d[i,"comp.2"]!=2)) {
            d[i,"comp.errors"] <- 1 
        } else { 
            d[i,"comp.errors"] <- 2 
        }
    }
    
    # Attention checks 
    if((d[i,"attn_check_1_1"]==15) & (d[i,"attn_check_1_2"] > d[i,"attn_check_1_1"]) & 
       (d[i,"attn_check_1_3"] > d[i,"attn_check_1_2"]) & (d[i,"attn_check_1_2"]%%10 == 0) & 
       (d[i,"attn_check_2"]==1)) { 
        d[i,"chk.errors"] <- 0 
    } else if((d[i,"attn_check_1_1"]==15) & (d[i,"attn_check_1_2"] > d[i,"attn_check_1_1"]) & 
              (d[i,"attn_check_1_3"] > d[i,"attn_check_1_2"]) & (d[i,"attn_check_1_2"]%%10 == 0) & 
              (d[i,"attn_check_2"]!=1)) {
        d[i,"chk.errors"] <- 1 
    } else if((d[i,"attn_check_1_1"]!=15) | (d[i,"attn_check_1_2"] <= d[i,"attn_check_1_1"]) | 
              (d[i,"attn_check_1_3"] <= d[i,"attn_check_1_2"]) | (d[i,"attn_check_1_2"]%%10 != 0) & 
              (d[i,"attn_check_2"]==1)) { 
        d[i,"chk.errors"] <- 1 
    } else {
        d[i,"chk.errors"] <- 2 
    }
}

# Perform attention exclusions 
d <- subset(d, d$chk.errors==0)
n_before_exclusions <- dim(d)[1]; n_before_exclusions

# Perform comprehension exclusions 
final_data <- subset(d, d$comp.errors<=1)
n_after_exclusions <- dim(final_data)[1]; n_after_exclusions

exclusion_percent <- ((n_before_exclusions - n_after_exclusions)/n_after_exclusions)*100; exclusion_percent

## -- Demographics 

# Age 
d_age <- mean(final_data$age, na.rm = TRUE); d_age 

# Gender: female
setDT(final_data)[, 100 * .N/nrow(final_data), by = gender] #female: 49.16% (coded as 2) 

##=============================================================================================================
                                            ## ANALYSES ##
##================================================================================================================

# correlation for two identity measures
#change_total_2 <- cbind(final_data$change.1, final_data$change.2)
#psych::alpha(change_total_2)$total$raw_alpha #0.9184 
cor.test(final_data$change.1, final_data$change.2)

# T-tests
t.test(subset(final_data,condition==-1)$change.mean,subset(final_data,condition==1)$change.mean,paired=F,var.equal=T)
cohen.d(subset(final_data,condition==-1)$change.mean,subset(final_data,condition==1)$change.mean,var.equal=TRUE, paired=FALSE)

## Check for interactions 

# Anova
mod <- aov(change.mean ~ as.factor(vignette)*as.factor(condition), data=final_data)
summary(mod) # no interaction

## Potential interaction between condition and product/other type of change 

# Product changes: WestWinter (1), BrighterLab (2), DocLips (8) 
# Non-product changes: WonderlandClothing (3), AccessMine (4), Tecigo (5), RoyalOil (6), BluewaterTobacco (7) 
prod_list <- c(1, 2, 8) #see vignette_list 

# Dummy code type of change as 1 = product-related change and 0 = other types of change 
for(i in 1:nrow(final_data)) {
    if(final_data$vignette[i] %in% prod_list) {
        final_data$change_type[i] <- 1 
    } else {
        final_data$change_type[i] <- 0 
    }
}

# Anova
prod_int <- aov(change.mean ~ as.factor(change_type)*as.factor(condition), data=final_data)
summary(prod_int) #no interaction (p = 0.94275) #(F(1, 234) = 0.005, p = .943)

##=============================================================================================================
                                            ## PLOTTING ##
##================================================================================================================

# Define variables 
x_labels <- c("Clothing", "Cosmetics", "Apparel", "Mining", 
                     "Technology", "Petroleum", "Tobacco", "Makeup") #set category/industry names
legend_labels <- c("Deterioration", "Improvement")

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

p1 <- ggplot(final_data,aes(x=factor(condition),y=change.mean)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p1 <- p1 + theme(text = element_text(size=18),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=legend_labels) +
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
  ylab("Identity Change") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p1


p2 <- ggplot(final_data,aes(x=factor(vignette),y=change.mean, fill=factor(condition)),color=factor(condition)) +  
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
        axis.text.x = element_text(size = 14, angle = 45, vjust = 1.0, hjust = 1),
        legend.position="top") + 
  xlab("Firm Type") +
  ylab("") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p2

figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, vjust = 1.0, hjust = 0.5)
ggsave(paste0('e1', ".png"), last_plot(), dpi = 300, width = 12, height = 6)


# Move files 
dir.create("analysis_plots") 
plot_files <- c("e1.png") #list.files(pattern = ".pdf") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 


##=============================================================================================================
                                              ## END ##
##================================================================================================================

