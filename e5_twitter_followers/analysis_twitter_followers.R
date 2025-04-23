## Corporate Essence Analysis - Twitter Followers 
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
               'ggpubr' #combine plots 
)


##================================================================================================================
                                          ##IMPORT DATA##
##================================================================================================================

# Read data
d_raw <- read.csv("senators_wt_followers.csv")

# calculate percent change
d_raw$percent_follower_change = (d_raw$diff_followers/d_raw$date1_followers) * 100
d_raw$percent_tweet_change = (d_raw$diff_tweets/d_raw$date1_tweets) * 100
d_raw$percent_tweet_change[57] <- 0 #this senator never tweeted
d_raw$party_num <- ifelse(d_raw$Party=='D', 1, 2)

# Initial count
n_ss <- dim(d_raw)[1]; n_ss

## Group Independent senators with Democrats 
d_grouped <- d_raw 
d_grouped$Party = as.character(d_grouped$Party)
for(i in 1:nrow(d_grouped)) {
  if(d_grouped$Party[i] %in% c("D", "I")) {  
    d_grouped$Party[i] <- "L" #if Democrat or Independent, group as liberal 
  } else { 
    d_grouped$Party[i] <- "C" #if Republican, rename to conservative 
  }
} 
n_senators <- nrow(d_grouped)

## Exclude Independent senators
d_excluded <- d_raw[!(d_raw$Party == "I"), ]  
d_excluded$Party = as.character(d_excluded$Party)

for(i in 1:nrow(d_excluded)) {
  if(d_excluded$Party[i]=="D") {  
    d_excluded$Party[i] <- "L" #if Democrat, rename to liberal 
  } else { 
    d_excluded$Party[i] <- "C" #if Republican, rename to conservative 
  }
} 
n_senators_new <- nrow(d_excluded) 


##=============================================================================================================
                                          ## MAIN ANALYSES ##
##=============================================================================================================

## Version 1: Include all 100 senators, grouping Bernie Sanders and Angus King (Independents) with Democrats --------------------------------------------------------------------------

t.test(subset(d_grouped,Party=="C")$percent_follower_change,subset(d_grouped,Party=="L")$percent_follower_change,paired=F,var.equal=T)
cohen.d(subset(d_grouped,Party=="C")$percent_follower_change,subset(d_grouped,Party=="L")$percent_follower_change,paired=F,var.equal=T)

t.test(subset(d_grouped,Party=="C")$percent_tweet_change,subset(d_grouped,Party=="L")$percent_tweet_change,paired=F,var.equal=T)
cohen.d(subset(d_grouped,Party=="C")$percent_tweet_change,subset(d_grouped,Party=="L")$percent_tweet_change,paired=F,var.equal=T)

## Are the means any different when controlling for # tweets?

followers_lm <- aov(percent_follower_change ~ Party + percent_tweet_change, data = d_grouped)
summary(followers_lm)

library(emmeans)

# Calculating adjusted means
adjusted_means <- emmeans(followers_lm, ~ Party)

# Viewing the adjusted means
print(adjusted_means)

## Version 2: Exclude Independent senators --------------------------------------------------------------------------

t.test(subset(d_excluded,Party=="C")$percent_follower_change,subset(d_excluded,Party=="L")$percent_follower_change,paired=F,var.equal=T)
cohen.d(subset(d_excluded,Party=="C")$percent_follower_change,subset(d_excluded,Party=="L")$percent_follower_change,paired=F,var.equal=T)

t.test(subset(d_excluded,Party=="C")$percent_tweet_change,subset(d_excluded,Party=="L")$percent_tweet_change,paired=F,var.equal=T)
cohen.d(subset(d_excluded,Party=="C")$percent_tweet_change,subset(d_excluded,Party=="L")$percent_tweet_change,paired=F,var.equal=T)

## Percentage of liberal senators with negative tweet changes and vice versa for conservatives.

# Filtering for liberal senators
liberal_data <- d_excluded[d_excluded$Party == "L", ]
conservative_data <- d_excluded[d_excluded$Party == "C", ]

# Function to calculate percentages
calculate_percentages <- function(df) {
  negative <- sum(df$percent_follower_change < 0) / nrow(df) * 100
  zero <- sum(df$percent_follower_change == 0) / nrow(df) * 100
  positive <- sum(df$percent_follower_change > 0) / nrow(df) * 100
  return(c(Negative = negative, Zero = zero, Positive = positive))
}

# Calculating percentages for liberal and conservative senators
liberal_percentages <- calculate_percentages(liberal_data)
conservative_percentages <- calculate_percentages(conservative_data)

# Printing the results
liberal_percentages
conservative_percentages


##=============================================================================================================
                                          ## PLOTTING ##
##================================================================================================================

## Save plots --------------------------------------------------------------------------


plot1 <- GetMeanDifferences(follower_new, "Party affiliation (Independents excluded)", "Percent Follower difference", 
                     party_breaks, party_labels)
plot1

plot2 <- GetMeanDifferences(tweet_new, "Party affiliation (Independents excluded)", "Tweet difference", 
                     party_breaks, party_labels)


x_labels <- c("Conservative", "Liberal")
x_label <- "Party Affiliation"
title1 <- "Change in Followers"

# Function for bootstrapping and computing confidence intervals
bootstrap_ci <- function(x, alpha = 0.05, n_bootstrap = 1000) {
  boot_samples <- replicate(n_bootstrap, sample(x, replace = TRUE))
  boot_means <- apply(boot_samples, 2, mean)
  ci_lower <- quantile(boot_means, alpha / 2)
  ci_upper <- quantile(boot_means, 1 - alpha / 2)
  data.frame(y = mean(x), ymin = ci_lower, ymax = ci_upper)
}

p1 <- ggplot(d_excluded, aes(x = factor(Party), y = percent_follower_change)) + 
  theme_bw() + coord_cartesian(ylim = c(-0.6, 0.6)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") +
  scale_x_discrete(labels = x_labels) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(text = element_text(size = 20), axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        plot.title = element_text(size = 25),
        legend.position = "top") + 
  xlab("") +
  ylab("") +
  ggtitle(title1) +
  geom_bar(position = "dodge", stat = "summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))

p1

title2 <- "Change in Tweets"

p2 <- ggplot(d_excluded, aes(x = factor(Party), y = percent_tweet_change)) + 
  theme_bw() + coord_cartesian(ylim = c(0.0, 0.6)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") +
  scale_x_discrete(labels = x_labels) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(text = element_text(size = 20), axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14),
        plot.title = element_text(size = 25),
        legend.position = "top") + 
  xlab("") +
  ylab("") +
  ggtitle(title2) +
  geom_bar(position = "dodge", stat = "summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))

p2

figure <- ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5)
annotate_figure(figure, left = text_grob("Mean Percentage Change", color="black", face ="plain",size=25, rot=90),
                bottom = text_grob(x_label, color="black", face ="plain",size=25, vjust=-0.4))

ggsave(paste0('e5a', ".png"), last_plot(), dpi = 300, width = 11, height = 6)


x_label3 <- "Senators"
y_label3 <- "Percent Follower Difference"
legend_label3 <- c("Conservative", "Liberal")  # Corrected order of elements

d_excluded$Name <- factor(d_excluded$Name, levels = d_excluded$Name[order(d_excluded$percent_follower_change)])

p3 <- ggplot(d_excluded, aes(x = factor(Name), y = percent_follower_change, fill = factor(Party)), color = factor(Pary)) +  
  theme_bw() + coord_cartesian(ylim = c(-1, 3)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(text = element_text(size = 18), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#999999", "#222222"), name = "", labels = legend_label3, guide = guide_legend(reverse = FALSE)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5)) +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1),
        plot.title = element_text(size = 25),
        legend.position = "top") + 
  xlab(x_label3) +
  ylab(y_label3) +
  geom_bar(position = "dodge", stat = "summary", width = 0.9, alpha = 0.38, size = 0.75) +
  stat_summary(fun.data = function(x) bootstrap_ci(x, alpha = 0.05, n_bootstrap = 1000),
               color = "black", size = 0.4, 
               geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))

p3

ggsave(paste0('e5a_2', ".png"), last_plot(), dpi = 300, width = 11, height = 6)


# Move files 
dir.create("analysis_plots") 
plot_files <- list.files(pattern = ".png") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 


##=============================================================================================================
                                              ## END ##
##=============================================================================================================


