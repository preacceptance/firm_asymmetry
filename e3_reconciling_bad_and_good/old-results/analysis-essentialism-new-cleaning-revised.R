## Corporate Essence Analysis - Moral Asymmetry 


# Clear working directory
remove(list = ls())

# Set working directory to current file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
               'plotrix'
               
)

# Call in scripts  
source("../common_functions/process.R") #import process function for mediation analysis 

############################################################## begin cleaning

## read in data: 
d = read.csv("ce-numerical-data.csv")

## filter based on conditions

## consent
d = filter(d, d$Finished == 1)

## consent
d = filter(d, d$consent == 1)

## passed attention checks
d = filter(d, d$attn_check_3_2!=18 & d$attn_check_3_2!=23 & d$attn_check_3_2!=25 & d$attn_check_3_2!=27 & d$attn_check_3_2!=53 & d$attn_check_3_1!=14 | (d$attn_check_1==2&d$attn_check_2==2))

### extract firm name
d$firm.name = ifelse(!is.na(d$WW_det_comp_1)|!is.na(d$WW_imp_comp_1), "WW",
                     ifelse(!is.na(d$BL_det_comp_1)|!is.na(d$BL_imp_comp_1),"BL",
                            ifelse(!is.na(d$WC_det_comp_1)|!is.na(d$WC_imp_comp_1),"WC",
                                   ifelse(!is.na(d$AM_det_comp_1)|!is.na(d$AM_imp_comp_1),"AM",
                                          ifelse(!is.na(d$TC_det_comp_1)|!is.na(d$TC_imp_comp_1),"TC",
                                                 ifelse(!is.na(d$RO_det_comp_1)|!is.na(d$RO_imp_comp_1),"RO",
                                                        ifelse(!is.na(d$BT_det_comp_1)|!is.na(d$BT_imp_comp_1),"BT",
                                                               ifelse(!is.na(d$DL_det_comp_1)|!is.na(d$DL_imp_comp_1),"DL","NONE"))))))))



## comprehension checks


## pull all columns with comp_1 in name (before the changes)
comp_1_det = d[ , grepl(paste(c("ResponseId","det_comp_1"), collapse='|'), names( d ) ) ]

## concatenate all of the items in the comp_1 columns together
comp_1_det$all_comp_1_det <- apply(comp_1_det[,!names(comp_1_det) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))

comp_1_det$imp_det = ifelse(comp_1_det$all_comp_1=="","imp","det")


comp_1_imp = d[ , grepl(paste(c("ResponseId","imp_comp_1"), collapse='|'), names( d ) ) ]

## concatenate all of the items in the comp_1 columns together
comp_1_imp$all_comp_1_imp <- apply(comp_1_imp[,!names(comp_1_imp) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))


comp_1 = merge(comp_1_imp,comp_1_det)

comp_1$c1_det_pass = ifelse(comp_1$all_comp_1_det == 2, "c1-det-pass", "")
comp_1$c1_imp_pass = ifelse(comp_1$all_comp_1_imp == 5, "c1-imp-pass", "")

comp_1$c1_pass = ifelse(comp_1$c1_det_pass==""&comp_1$c1_imp_pass=="",0,1)


##### now comp_2
comp_2_det = d[ , grepl(paste(c("ResponseId","det_comp_2"), collapse='|'), names( d ) ) ]

## concatenate all of the items in the comp_2 columns together
comp_2_det$all_comp_2_det <- apply(comp_2_det[,!names(comp_2_det) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))

comp_2_imp = d[ , grepl(paste(c("ResponseId","imp_comp_2"), collapse='|'), names( d ) ) ]

## concatenate all of the items in the comp_2 columns together
comp_2_imp$all_comp_2_imp <- apply(comp_2_imp[,!names(comp_2_imp) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))

comp_2 = merge(comp_2_imp,comp_2_det)


comp_2$c2_det_pass = ifelse(comp_2$all_comp_2_det == 5, "c2-det-pass", "")


comp_2$c2_imp_pass = ifelse(comp_2$all_comp_2_imp == 2, "c2-imp-pass", "")

comp_2$c2_pass = ifelse(comp_2$c2_det_pass==""&comp_2$c2_imp_pass=="",0,1)

d <- d %>% dplyr::select(-contains("comp_1"))
d <- d %>% dplyr::select(-contains("comp_2"))

comps = merge(comp_1,comp_2)

## get order for the final comprehension check

## pull all columns with prediction
prediction_first = d[ , grepl(paste(c("ResponseId","prediction"), collapse='|'), names( d ) ) ]

## concatenate all of the items in the prediction_first columns together
prediction_first$order_placeholder <- apply(prediction_first[,!names(prediction_first) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))


## if there are items, then it is prediction first; if there are no items then it is identity first
prediction_first$order = ifelse(prediction_first$order_placeholder=="", "identity-first","prediction-first")

d$order = prediction_first$order
d$order = as.factor(d$order)

comp_prev_all = d[ , grepl(paste(c("ResponseId","comp_prev"), collapse='|'), names( d ) ) ]
comp_prev_all$all_comp_prev <- apply(comp_prev_all[,!names(comp_prev_all) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
comp_prev_all$order = d$order


comp_prev_all$comp_prev_identity_pass = ifelse(comp_prev_all$order == "identity-first"&comp_prev_all$all_comp_prev==1,"identity-pass","")

comp_prev_all$comp_prev_prediction_pass = ifelse(comp_prev_all$order == "prediction-first"&comp_prev_all$all_comp_prev==2,"prediction-pass","")

comp_prev_all$comp_prev_pass = ifelse(comp_prev_all$comp_prev_identity_pass==""&comp_prev_all$comp_prev_prediction_pass=="",0,1)
d <- d %>% dplyr::select(-contains("comp_prev"))

comps = merge(comps,comp_prev_all)

d = merge(d, comps)

d$all_checks = rowSums(d[,c("c1_pass", "c2_pass", "comp_prev_pass")])

d = filter(d, d$all_checks>=2)

########################## Collapse each variable across conditions into one variable

## collapse identity_1 variable into one overall variable
m =  d[ , grepl(paste(c("ResponseId","identity_1"), collapse='|'), names( d ) ) ]

m$identity_1_all <- apply(m[,!names(m) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,m)

## collapse identity_2 variable into one overall variable
n = d[ , grepl(paste(c("ResponseId","identity_2"), collapse='|'), names( d ) ) ]


n$identity_2_all <- apply(n[,!names(n) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,n)

## collapse det_identity_3 variable into one overall variable
o = d[ , grepl(paste(c("ResponseId","identity_3"), collapse='|'), names( d ) ) ]

o$identity_3_all <- apply(o[,!names(o) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,o)

## collapse identity_telo variable into one overall variable
p = d[ , grepl(paste(c("ResponseId","identity_telo"), collapse='|'), names( d ) ) ]

p$identity_telo <- apply(p[,!names(p) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,p)

## collapse prediction_1 variable into one overall variable
q = d[ , grepl(paste(c("ResponseId","prediction_1"), collapse='|'), names( d ) ) ]

q$prediction_1 <- apply(q[,!names(q) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,q)

## collapse prediction_es variable into one overall variable
r = d[ , grepl(paste(c("ResponseId","prediction_es"), collapse='|'), names( d ) ) ]

r$prediction_es <- apply(r[,!names(r) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,r)

## collapse prediction_te variable into one overall variable
s = d[ , grepl(paste(c("ResponseId","prediction_te"), collapse='|'), names( d ) ) ]

s$prediction_te <- apply(s[,!names(s) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,s)

## collapse prediction_ca variable into one overall variable
t = d[ , grepl(paste(c("ResponseId","prediction_ca"), collapse='|'), names( d ) ) ]

t$prediction_ca <- apply(t[,!names(t) %in% c("ResponseId")] , 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ""))
d = merge(d,t)


## make everything numeric in case we need it
d = d %>%
  mutate_at(vars(identity_1_all, identity_2_all, identity_3_all, identity_telo,
                 prediction_1,prediction_es,prediction_te,prediction_ca), as.numeric)


d$essentialism = ifelse(is.na(d$identity_2_all),d$prediction_es,d$identity_2_all)
d$categorical = ifelse(is.na(d$identity_3_all),d$prediction_ca,d$identity_3_all)
d$telos = ifelse(is.na(d$identity_telo),d$prediction_te,d$identity_telo)


table(d$order)
table(d$imp_det)
table(d$firm.name)


################# 2x2x8 ANOVA to confirm no interaction with scenario 

d$imp_det = as.factor(d$imp_det)
d$order = as.factor(d$order)

print(summary(aov(prediction_1 ~ imp_det*firm.name, d)))  # no interaction
print(summary(aov(identity_1_all ~ imp_det*firm.name, d))) # no interaction

################################## t-test items with interaction significance against imp vs det

print(t.test(identity_1_all ~ imp_det, data = d))
cohen.d(identity_1_all ~ imp_det, data = d)

print(t.test(essentialism ~ imp_det, data = d))
cohen.d(essentialism ~ imp_det, data = d)

print(t.test(categorical ~ imp_det, data = d))
cohen.d(categorical ~ imp_det, data = d)

print(t.test(telos ~ imp_det, data = d))
cohen.d(telos ~ imp_det, data = d)

print(t.test(prediction_1 ~ imp_det, data = d))
cohen.d(prediction_1  ~ imp_det, data = d)



bar_plot4 <- ggplot(d, aes(x = firm.name, y=identity_1_all, fill=imp_det )) + 
  ggtitle("The [company] after the changes is not really the same company as before the changes. ") +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") + theme_bw()

bar_plot5 <- ggplot(d, aes(x = firm.name, y=prediction_1, fill=imp_det )) + 
  scale_fill_manual(values = c("gray40", "gray85")) +
  ggtitle("") +
  stat_summary(geom = "bar", fun = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") + theme_classic()+ 
  scale_x_discrete(name ="Firm Type", labels=c("Mining","Cosmetics", "Tobacco", "Makeup","Petroleum","Technology","Apparel", "Clothing"))+labs(y = "Will return to original",fill=c("Moral change")) +theme(text = element_text(size = 25),
                                                                                                                                                                                                            axis.title.y = element_text(size = 25), 
                                                                                                                                                                                                            axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
                                                                                                                                                                                                            axis.text.x = element_text(size = 20),
                                                                                                                                                                                                            legend.position="top") +coord_cartesian(ylim=c(1,100)) 



bar_plot4
bar_plot5

####################### mediation


d$imp_det_num = ifelse(d$imp_det == "det", -1, 1)

process(data = d, y = "identity_1_all", x = "imp_det_num", m =c("essentialism", "telos", "categorical"), model = 4, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)


process(data = d, y = "prediction_1", x = "imp_det_num", m =c("essentialism", "telos", "categorical"), model = 4, effsize =1, total =1, stand =1, contrast =1, boot = 10000 , modelbt = 1, seed = 654321)

################################################## Psychopathy scale 
## collapse prediction_ca variable into one overall variable
u = d[ , grepl(paste(c("ResponseId","psych_"), collapse='|'), names( d ) ) ]
u$psych <- apply(u[,!names(u) %in% c("ResponseId")], 1, function(x) mean(x))

d = merge(d,u)
d$psych = as.numeric(d$psych)

## Create low and high psychopathy categories, for later plots
d$psych_cat <- ifelse(d$psych>50, "High Psychopathy", "Low Psychopathy")
d$psych_cat_n <- ifelse(d$psych=="High Psychopathy",2,1)

## moderation
mean(d$psych)

process(data = d, y = "identity_1_all", x = "psych", w="imp_det_num", model = 1)

process(data = d, y = "prediction_1", x = "psych", w="imp_det_num", model = 1)

# followup tests
## 1. Improvement condition (negative predictions, or "reverting to bad majority") 

lm_imp_psych <- lm(prediction_1 ~ psych, data = subset(d, imp_det=="imp"))
summary(lm_imp_psych) 

pred_imp <- subset(d, imp_det=="imp")
plot(pred_imp$psych, pred_imp$prediction_1_all)
abline(lm(pred_imp$prediction_1 ~ pred_imp$psych))


## 2. Deterioration condition (positive predictions, or "reverting to good majority") 
lm_det_psych <- lm(prediction_1 ~ psych, data = subset(d, imp_det=="det"))
summary(lm_det_psych) 

pred_det <- subset(d, imp_det=="det")
plot(pred_det$psych, pred_det$prediction_1)
abline(lm(pred_det$prediction_1 ~ pred_det$psych))