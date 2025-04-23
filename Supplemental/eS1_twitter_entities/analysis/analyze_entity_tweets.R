## clear workspace - this prevents old variables from slowing down your session
rm(list = ls()) 

options(download.file.method="libcurl")

## install packages
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('ggplot2',         # plotting
               'ggsignif',        # plotting significance bars
               'lme4',            # functions for fitting linear regression models
               'ggforce',         # make ggplot even fancier
               'ggpubr',          # arrange plots in a grid, if needed
               'ltm',             # probably not using..
               'tidyverse',           # tools for cleaning messy data
               'stringr',         # perform string substitutions easily
               'assertthat',      # allows me to check whether a variable is a string, with is.string
               'lsmeans',         # contrast analysis for regression models
               'stats',           # use function to adjust for multiple comparisons
               'simr',            # power analysis for mixed models
               'compute.es',      # effect size package
               'effsize',         # another effect size package
               'pwr',             # package for power calculation
               'nlme',            # get p values for mixed effect model
               'DescTools',       # get Cramer's V
               'readxl' ,          # read excel
               'plotrix',
               'tidytext',
               'tm',
               'vader',
               'ldatuning',
               'topicmodels',
               'ggsignif',
               'filesstrings', #move files around 
)

## in this code we explore the tweets that we scraped. we decide 
### (1) which form of tweets to use (lemmatized, stemmed, or none)
### (2) whether or not to include duplicates across entities
### (3) and perform a series of statistical tests on the tweets to determine whether or not
###             they are significantly different by entity type

### we ultimately chose to use lemmatized tweets and remove duplicate entities
### other options are available but commented out. you can uncomment to explore

## ================================================================================================================
##                                         CLEANING               
## ================================================================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

## read in data: 
d = read.csv("full_dataframe_with_sentiment.csv")

# remove unnecessary columns 
d = subset(d, select = -c(X, Unnamed..0))
d = filter(d, d$entity_name!="bigfirm") ## these are duplicates that I accidentally included. 948k

d_unique_tweets = d %>% distinct(id, .keep_all = TRUE) 
d = d_unique_tweets

#write_rds(d_unique_tweets, "full_dataframe_with_sentiment.rds")


## find number of entity exclusions. exclude duplicate organizations and duplicate tweets
d_grouped <-
  d %>% 
  dplyr::group_by(entity_type,entity_name) %>% 
  dplyr::summarise(count = n())

d_grouped$is_dupe = duplicated(d_grouped$entity_name)
dupe_entities = filter(ungroup(d_grouped), d_grouped$is_dupe == TRUE) 


## remove duplicates
d = filter(d, !(d$entity_name %in% dupe_entities$entity_name))

d_grouped <-
  d %>% 
  dplyr::group_by(entity_type,entity_name) %>% 
  dplyr::summarise(count = n())

## mean number of tweets per entity type
mean(d_grouped$count)
sd(d_grouped$count)

d_grouped$is_dupe = duplicated(d_grouped$entity_name)


## sometimes grouping will mess things up so we ungroup
d = d %>% ungroup()

# remove odd entities
d <- filter(d, (d$entity_type!="police"))
d <- filter(d, (d$entity_type!="fire_dept")) 
d <- filter(d, (d$entity_type!="bank"))
d <- filter(d, (d$entity_type!="smallfirm"))

print("total number of tweets by entity type:")
table(d$entity_type)

# get a count of tweets per entity
d = d %>% dplyr::group_by(entity_name) %>%
  dplyr::mutate(n_tweets = n())

mean(d$n_tweets)
sd(d$n_tweets)

#write_rds(d, "full_dataframe_with_sentiment_no_dupes_no_police_no_fire.rds")


## order d based on sentiment
d <- d[order(d$sentiment_nostop_lemma),]


d_no_duplicates = d

## produce figure in appendix

d_no_duplicates$valence = ifelse(d_no_duplicates$sentiment_nostop_lemma==0,"neutral",ifelse(d_no_duplicates$sentiment_nostop_lemma<0,"negative","positive"))

table(d_no_duplicates$valence)
t = d_no_duplicates %>% dplyr::group_by(entity_type, valence) %>%
  dplyr::summarize(value = n()) %>% dplyr::group_by(entity_type) %>% mutate(total = sum(value))
t$valence = as.factor(t$valence)
t$perc = round(t$value/t$total,digits=3)


relevel(t$valence, 'negative')
t$valence

df3 <- t %>% 
  filter(valence=="negative") %>% 
  arrange(desc(perc))

t$entity_type <- factor(t$entity_type, levels=df3$entity_type)

ggplot(t, aes(fill=valence, y=perc, x=entity_type)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0(perc*100,"%")), 
            position = position_stack(vjust = 0.5), size = 6) + 
  theme_classic() +
  theme(text = element_text(size = 15),
        axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size = 20, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 12, angle=0)) + 
  scale_fill_grey(start=0.90, end=0.6) + 
  scale_x_discrete(name ="Entity Type", labels=c("Bank","Big Firm","Sports Franchise", "University", "Hospital","Grantmaker","Small Firm","Singer","Church","Performing Arts", "Art Museum")) +
  labs(y = "Percentage of tweets") 

filter(d_no_duplicates, d_no_duplicates$sentiment<0)


## we've chosen to exclude duplicates and used lemmatized tweets to calculate sentiment
d_entity_means_no_duplicates <- d_no_duplicates %>% group_by(entity_type,entity_name) %>% 
  dplyr::summarise(##mean_entity_sentiment = mean(sentiment),
    mean_entity_sentiment_lemma = mean(sentiment_nostop_lemma),
    # mean_entity_sentiment_stem = mean(sentiment_nostop_stem),
    # mean_entity_sentiment = mean(sentiment_nostop),
    count_tweets = n())

# over_all_sentiment_mean = mean(d_entity_means$mean_entity_sentiment)
# overall_sentiment_error = sd(d_entity_means$mean_entity_sentiment)/sqrt(nrow(d_entity_means))
# nrow(d_entity_means)


d_entity_means_no_duplicates = d_entity_means_no_duplicates %>% ungroup()


## d_type_means contains each entity type along with its mean sentiment across all tweets (10 rows)
d_type_means_no_duplicates <- d_entity_means_no_duplicates %>% group_by(entity_type) %>% 
  dplyr::summarise(
  # mean_sentiment = mean(mean_entity_sentiment),
  #                  type_se = sd(mean_entity_sentiment)/sqrt(n()),
                   mean_sentiment_lemma = mean(mean_entity_sentiment_lemma),
                   sd_lemma = sd(mean_entity_sentiment_lemma),
                   type_se_lemma = sd(mean_entity_sentiment_lemma)/sqrt(n()),
                   # mean_sentiment_stem = mean(mean_entity_sentiment_stem),
                   # type_se_stem = sd(mean_entity_sentiment_stem)/sqrt(n()),
                   count_entity = n()) %>%
  mutate(se.sent = sd_lemma / sqrt(n()),
         lower.ci.mpg = mean_sentiment_lemma - qt(1 - (0.05 / 2), n() - 1) * se.sent,
         upper.ci.mpg = mean_sentiment_lemma + qt(1 - (0.05 / 2), n() - 1) * se.sent)


## what is the effect of entity type on mean sentiment of tweets?
one.way <- aov(mean_entity_sentiment_lemma ~ entity_type, data = d_entity_means_no_duplicates)

summary(one.way)

library(lsr)
etaSquared(one.way)

## ================================================================================================================
##                              t.test comparing firms to each type of entity        
## ================================================================================================================


d_no_duplicates = d_no_duplicates %>% ungroup()
for (i in c("performing_arts", "art_museums", "hospital", "grantmaker", "uni", "singer", "sport")){
  a = "bigfirm"
  b = i
  print(paste(a, " vs ", b))
  
  print(filter(d_entity_means_no_duplicates, d_entity_means_no_duplicates$entity_type==a) %>% get_summary_stats(mean_entity_sentiment_lemma , type = "mean_sd"))
  print(filter(d_entity_means_no_duplicates, d_entity_means_no_duplicates$entity_type==b) %>% get_summary_stats(mean_entity_sentiment_lemma, type = "mean_sd"))
  
  #print(t.test(mean_entity_sentiment_lemma ~ entity_type, data = filter(d_entity_means_no_duplicates, d_entity_means_no_duplicates$entity_type==a |d_entity_means_no_duplicates$entity_type==b), alternative = "less"))
  
  i = filter(d_entity_means_no_duplicates, d_entity_means_no_duplicates$entity_type==a)
  j = filter(d_entity_means_no_duplicates, d_entity_means_no_duplicates$entity_type==b)
  
  print(t.test(i$mean_entity_sentiment_lemma, j$mean_entity_sentiment_lemma, paired=FALSE,alternative = "less"))
  print(cohen.d(i$mean_entity_sentiment_lemma, j$mean_entity_sentiment_lemma))
  
}

## ================================================================================================================
##                                         PLOT ALL MEASURES               
## ================================================================================================================

## entity count
bar_plot_count_entities <- ggplot(d_type_means_no_duplicates, aes(x = reorder(entity_type, mean_sentiment_lemma), y=count_entity)) + 
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray65") + 
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() +  
  theme(text = element_text(size = 25),
        axis.title.y = element_text(size = 25), 
        axis.title.x = element_text(size = 25), 
        axis.text.x = element_text(size = 10, angle=90),
        legend.position="top") + 
  labs(x = "Entity type", y = "Number of entities", fill = "") + 
  scale_fill_manual(##labels = firm_labels, 
    values = c("gray40", "gray85"))

bar_plot_count_entities


## sentiment with 95 CI bars

f5 <- ggplot(d_type_means_no_duplicates, aes(x = reorder(entity_type, mean_sentiment_lemma), y=mean_sentiment_lemma)) + 
  geom_bar(position="dodge", stat="identity",fill="grey65") + 
  geom_errorbar(aes(ymin=mean_sentiment_lemma-type_se_lemma, ymax=mean_sentiment_lemma+type_se_lemma), #add error bars 
                width=.2, position=position_dodge(.9)) + 
  theme_classic() +  
  theme(text = element_text(size = 15),
        axis.title.y = element_text(size = 23, margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        axis.title.x = element_text(size = 23, margin = margin(t = -20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 14, angle=45, margin = margin(t = 40, r = 0, b = 0, l = 0), color="black"),
        legend.position="top") + 
  labs(x = "Entity Type", y = "Average Sentiment Score", fill = "") + 
  ylim(-0.2, 0.7) + 
  scale_fill_manual(##labels = firm_labels, 
    values = c("black"))+ 
  scale_x_discrete(name ="Entity Type", labels=c("Firms","Sports Franchises", "Singers", "Universities","Hospitals","Grantmakers","Art Museums","Performing Arts", "Churches"))+
  geom_signif(comparisons = list(c('bigfirm', 'sport')),annotation = c("+"),y_position = 0.25)+
  geom_signif(comparisons = list(c('bigfirm', 'singer')),annotation = c("+"),y_position = 0.3)+
  geom_signif(comparisons = list(c('bigfirm', 'uni')),annotation = c("***"),y_position = 0.35, tip_length = 0, map_signif_level = TRUE)+
  geom_signif(comparisons = list(c('bigfirm', 'hospital')),annotation = c("***"),y_position = 0.4, tip_length = 0)+
  geom_signif(comparisons = list(c('bigfirm', 'grantmaker')),annotation = c("***"),y_position = 0.450, tip_length = 0)+
  geom_signif(comparisons = list(c('bigfirm', 'art_museums')),annotation = c("***"),y_position = 0.5, tip_length = 0)+
  geom_signif(comparisons = list(c('bigfirm', 'performing_arts')),annotation = c("***"),y_position = 0.55, tip_length = 0)+
  geom_signif(comparisons = list(c('bigfirm', 'church')),annotation = c("***"),y_position = 0.6, tip_length = 0)
f5

ggsave(paste0('e0', ".png"), last_plot(), dpi = 300, width = 7, height = 8)

# Move files 
dir.create("analysis_plots") 
plot_files <- c("e0.png") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

##=============================================================================================================
                                        ## PREPARE DATA FOR FREQUENCY PLOTS ##
##================================================================================================================

#THIS CODE TAKES VERY LONG TO RUN

for (i in c("bigfirm","singer", "sport", "grantmaker","hospital", "art_museums", "performing_arts", "uni")){
  
    print(paste("words that drive sentiment for: ", i))
    
    df = dplyr::filter(d_no_duplicates %>% ungroup(),d_no_duplicates$entity_type==i)
    df$text_nostop_lemma = as.character(df$text_nostop_lemma)
    df$total_wc_lemma = str_count(df$text_nostop_lemma, "\\S+")
    new_df <- df %>% unnest_tokens(word, text_nostop_lemma) #%>% anti_join(stopwords)
    frequency_dataframe = new_df %>% dplyr::count(word) %>% arrange(desc(n))
    freq_sent = vader_df(frequency_dataframe$word)
    
    frequency_dataframe$text = frequency_dataframe$word
    frequency_dataframe$count_freq = frequency_dataframe$n/sum(df$total_wc_lemma)
    
    frequency_sent_dataframe = left_join(freq_sent,frequency_dataframe)
    frequency_sent_dataframe_pos = filter(frequency_sent_dataframe, frequency_sent_dataframe$pos==1) %>% arrange(desc(n))
    frequency_sent_dataframe_pos = subset(frequency_sent_dataframe_pos, select = -c(word_scores, compound, but_count, word) )
    
    print(paste("top ten positive words for: ", i))
    print(head(frequency_sent_dataframe_pos, 10))
    
    frequency_sent_dataframe_neg = filter(frequency_sent_dataframe, frequency_sent_dataframe$neg==1) %>% arrange(desc(n))
    frequency_sent_dataframe_neg = subset(frequency_sent_dataframe_neg, select = -c(word_scores, compound, but_count, word) )
    
    print(paste("top ten negative words for: ", i))
    print(head(frequency_sent_dataframe_neg, 10))
    
    csv_name_pos = paste(i, "_pos.csv")
    write.csv(frequency_sent_dataframe_pos, csv_name_pos)
    
    csv_name_neg = paste(i, "_neg.csv")
    write.csv(frequency_sent_dataframe_neg, csv_name_neg)
    
}

##=============================================================================================================
                                        ## MAKE FREQUENCY PLOTS ##
##================================================================================================================

bank_neg = read.csv("frequency_counts/bank _neg.csv")
bigfirm_neg = read.csv("frequency_counts/bigfirm _neg.csv")
sport_neg = read.csv("frequency_counts/sport _neg.csv")
singer_neg = read.csv("frequency_counts/singer _neg.csv")

f2 = bigfirm_neg %>% 
    arrange(desc(count_freq)) %>%
    slice(1:10) %>%
    ggplot(., aes(y=reorder(text,count_freq*100), x=count_freq*100))+
    geom_bar(stat='identity') + ggtitle("Firms")+
    labs(x = "", y = "", fill = "")  + theme_classic()+  
    theme(text = element_text(size = 29),
          legend.position="top")+
    theme(axis.text.x = element_text(size=24)) +
    theme(axis.text.y = element_text(size=24)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.5), limits = c(0, 1))

f3 = sport_neg %>% 
    arrange(desc(count_freq)) %>%
    slice(1:10) %>%
    ggplot(., aes(y=reorder(text,count_freq*100), x=count_freq*100))+
    geom_bar(stat='identity')+ggtitle("Sports Franchises")+
    labs(x = "", y = "", fill = "")  + theme_classic()+  
    theme(text = element_text(size = 29),
          legend.position="top")+
    theme(axis.text.x = element_text(size=24)) +
    theme(axis.text.y = element_text(size=24)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.5), limits = c(0, 1))

f4 = singer_neg %>% 
    arrange(desc(count_freq)) %>%
    slice(1:10) %>%
    ggplot(., aes(y=reorder(text,count_freq*100), x=count_freq*100))+
    geom_bar(stat='identity') +ggtitle("Singers")+
    labs(x = "", y = "", fill = "")  + theme_classic()+  
    theme(text = element_text(size = 29),
          legend.position="top")+
    theme(axis.text.x = element_text(size=24)) +
    theme(axis.text.y = element_text(size=24)) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.5), limits = c(0, 1))


figure<- ggarrange(f2,f3,f4, nrow=1,ncol=3, vjust = 1.0, hjust = 0.5) 
annotate_figure(figure, 
                bottom = text_grob("Percentage Occurrence", color = "black", hjust = 0.5, vjust = 0.3, size = 32),
                left = text_grob("Word", color = "black", hjust = 0.5, vjust = 1, size = 32, rot=90)
                )

ggsave(paste0('e0_2', ".png"), last_plot(), dpi = 300, width = 18, height = 12)

# Move files 
plot_files <- c("e0_2.png") 
file.move(plot_files, "analysis_plots", overwrite = TRUE) 

##=============================================================================================================
## END ##
##================================================================================================================


