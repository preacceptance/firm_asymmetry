## Corporate Essence Analysis - Firm Size
## ,  

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
               'sjstats',
               "semTools",
               "patchwork"
)

mediation <- FALSE

##================================================================================================================
##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

# Read data
d_raw <- read.csv("data_firmSize.csv")

# Initial count
n_ss <- dim(d_raw)[1]; n_ss

## -- Pre-process  

# Define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(n_ss,10))
colnames(d_subset) <- c('vig_name', 'cond_name', 'size_name', 'change.1', 'change.2', 
                        'essence', 'comp.1', 'comp.2', 'comp.3', 'change_mean')

d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

# Extract the good data from the middle part of the raw data
for(i in 1:n_ss) {
    cond_names <- d_raw[i,223][!is.na(d_raw[i,223])] #condition and vignette names 
    d_subset[i,1] <- strsplit(cond_names[[1]], ",")[[1]][1] #vignette name 
    d_subset[i,2] <- strsplit(strsplit(cond_names[[1]], ",")[[1]][2], "\\(")[[1]][1] #condition name
    d_subset[i,3] <- strsplit(strsplit(cond_names[[1]], ",")[[1]][2], "\\(")[[1]][2] 
    d_subset[i,3] <- substr(d_subset[i,3], 1, nchar(d_subset[i,3][[1]])-1) #firm name
    
    curr <- d_raw[i,21:212][!is.na(d_raw[i,21:212])] #for a given row, get only the non NA values
    d_subset[i,4:9] <- as.numeric(curr[curr!= ""]) #and only the non-empty values
    d_subset[i,10] <- mean(c(d_subset[i,4], d_subset[i,5])) #get mean change value 
}

d_subset$size_name[is.na(d_subset$size_name)] <- "Small"

# Merge good data with first and last halves of the original data
d <- cbind(d_raw[,1:20], d_subset, d_raw[,213:219])
d$ss <- 1:dim(d)[1]

# Get numeric conditions
d$condition <- ifelse(d$cond_name=='Improve', 1, 2)
d$condition_reverse <- ifelse(d$cond_name=='Improve', 2, 1)
vignette_list <- list("WestWinter" = 1, "BrighterLab" = 2, "WonderlandClothing" = 3, "AccessMart" = 4,
                      "Tecigo" = 5, "RoyalOlor" = 6, "BluewaterTobacco" = 7, "DocLips" = 8)
d$vignette <- NA 
for(i in 1:dim(d)[1]) { 
  d$vignette[i] <- as.numeric(vignette_list[[d$vig_name[i]]]) 
}

d$size <- ifelse(d$size_name=='Big', 1, 2)

## perform attention exclusions: 
d <- subset(d, d$Finished=="1")
d <- subset(d, (d$att_1 == 4 & d$att_2 == 1))
n_before <- dim(d)[1] # number of participants should decrease after attention exclusions

d$checks <- rep(0, nrow(d))
## perform comprehension exclusions
for(i in 1:dim(d)[1]) {
    if(d$comp.1[i] != d$condition_reverse[i]*2) {
      d$checks[i] <- d$checks[i] + 1
    }
    if(d$comp.2[i] != d$condition[i]*2) {
      d$checks[i] <- d$checks[i] + 1
    }
    if(d$comp.3[i] != d$size[i]) {
      d$checks[i] <- d$checks[i] + 1
    }
}

## perform comprehension exclusions: 
d <- subset(d, d$checks <= 1)
n_after <- dim(d)[1]

n_after/n_before

##=============================================================================================================
## ANALYSES ##
##================================================================================================================

# Discriminant Validity
model <- ' change   =~ change.1 + change.2
           essence  =~ essence'

htmt(model, d)

## Correlation Matrix
cor_matrix <- cor(d[, c("change.1", "change.2", "essence")])

cor_matrix


######## Change ##########
cor.test(d$change.1, d$change.2)

######## T-Tests ##########
t.test(change_mean~condition, var.equal=TRUE, data=d)
cohen.d(change_mean~condition, var.equal=TRUE, data=d)
t.test(essence~condition, var.equal=TRUE, data=d)
cohen.d(essence~condition, var.equal=TRUE, data=d)

######### Interactions #########
summary(aov(change_mean~cond_name*size_name*vig_name, data=d)) # no interaction with scenario on identity
summary(aov(essence~cond_name*size_name*vig_name, data=d)) # no interaction with scenario on essence

summary(aov(change_mean~cond_name*size_name, data=d)) # no interaction
summary(aov(essence~cond_name*size_name, data=d)) # no interaction

######### Mediation ###########
source("process.R")
d$condition <- ifelse(d$cond_name=='Improve', 1, -1) #contrast-coding for mediation
process(data = d, y = "change_mean", x = "condition", m = "essence", 
        model = 4, effsize = 1, total = 1, stand = 1, 
        boot = 10000 , modelbt = 1, seed = 654321)

######### Graphs #############
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

p1 <- ggplot(d,aes(x=factor(condition),y=change_mean)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p1 <- p1 + theme(text = element_text(size=10),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#cccccc", "#333333"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(plot.title = element_text(size=10, hjust=0.5)) +
  theme(text = element_text(size = 10),
        axis.title.y = element_text(size = 15), 
        axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 10),
        legend.position="top") + 
  xlab("Condition") +
  ylab("Identity Change") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p1


x_labels <- c("Clothing", "Cosmetics", "Apparel", "Store", 
              "Technology", "Laundry", "Tobacco", "Makeup") #set category/industry names
legend_labels <- c("Deterioration", "Improvement")

p2 <- ggplot(d,aes(x=factor(vignette),y=change_mean, fill=factor(condition)),color=factor(condition)) +  
  theme_bw() + coord_cartesian(ylim=c(1,100))+scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) 

p2 <- p2 + theme(text = element_text(size=10),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
  scale_x_discrete(labels=x_labels) +
  scale_fill_manual(values = c("#333333", "#cccccc"),name= "",
                    labels=legend_labels, guide = guide_legend(reverse = FALSE))+
  xlab ("") + ylab ("") +
  theme_classic() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(plot.title = element_text(size=10, hjust=0.5)) +
  theme(text = element_text(size = 10),
        axis.title.y = element_text(size = 15), 
        axis.title.x = element_text(size = 15, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 10, angle = 45, vjust = 1.0, hjust = 1),
        legend.position="top") + 
  xlab("Company Type") +
  ylab("") +
  geom_bar(position="dodge", stat="summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
p2

p1 | p2

ggsave("study2.png", device="png", width = 10, height = 4)
