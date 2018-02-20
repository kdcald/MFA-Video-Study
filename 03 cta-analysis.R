

# ------------------------------------------------------------------------------------------------------------
# ----- SETS WORKING DIRECTORY ------
# ------------------------------------------------------------------------------------------------------------

# set working directory
kcPath = "/Users/Krystal/Google Drive/research/active-projects/video study"
setwd(kcPath)
getwd()

library(compute.es)
library(ggplot2)

# ------------------------------------------------------------------------------------------------------------
# ----- IMPORTS DATA ------
# ------------------------------------------------------------------------------------------------------------

df <- read.csv("data_final/cleanVideoDataForAnalysis.csv", stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------------------------------------
# ----- REMOVES OBSERVATIONS WITH CONTROL VIDEO ------
#------ the justification for this is so that i'm only analyzing cta's in pro-veg videos. 
# ----- we don't really care about cta's at the end of non pro-veg videos
# ------------------------------------------------------------------------------------------------------------

df = df[!df$videoTreatment=="control",]


# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ----- REGRESSIONS ----
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

df$ctaTreatment = as.factor(df$ctaTreatment)
df <- within(df, ctaTreatment <- relevel(ctaTreatment, ref = "noCta"))

summary(lm(meatConsumpReduce ~ ctaTreatment, data=df))

summary(lm(vsgBinary ~ ctaTreatment, data=df))

summary(lm(attitude1Num ~ ctaTreatment, data=df))
summary(lm(attitude2Num ~ ctaTreatment, data=df))
summary(lm(attitude3Num ~ ctaTreatment, data=df))
summary(lm(attitude4Num ~ ctaTreatment, data=df))
summary(lm(attitude5Num ~ ctaTreatment, data=df))
summary(lm(attitude6Num ~ ctaTreatment, data=df))

summary(lm(combinedFoodAttitudes.std ~ ctaTreatment, data=df))
summary(lm(combinedAnimalAttitudes.std ~ ctaTreatment, data=df))
summary(lm(combinedAllAttitudes.std ~ ctaTreatment, data=df))

summary(lm(combinedOutcome1.std ~ ctaTreatment, data=df))
summary(lm(combinedOutcome2.std ~ ctaTreatment, data=df))
summary(lm(combinedOutcome3.std ~ ctaTreatment, data=df))


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES
# MEAT CONSUMPTION
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#cta1 vs nocta
meat1 = mes(mean(df$meatConsumpReduce[df$ctaTreatment=="cta1"], na.rm=TRUE), 
    mean(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="cta1"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sum(df$ctaTreatment=="cta1"), 
    sum(df$ctaTreatment=="noCta"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta2 vs nocta
meat2 = mes(mean(df$meatConsumpReduce[df$ctaTreatment=="cta2"], na.rm=TRUE), 
    mean(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="cta2"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sum(df$ctaTreatment=="cta2"), 
    sum(df$ctaTreatment=="noCta"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta3 vs nocta
meat3 = mes(mean(df$meatConsumpReduce[df$ctaTreatment=="cta3"], na.rm=TRUE), 
    mean(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="cta3"], na.rm=TRUE), 
    sd(df$meatConsumpReduce[df$ctaTreatment=="noCta"], na.rm=TRUE), 
    sum(df$ctaTreatment=="cta3"), 
    sum(df$ctaTreatment=="noCta"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

cta = c("Please leave animals off \nyour plate", "Please cut out or cut back\non animal products",
          "Please choose vegan")
  
d = c(meat1$d, meat2$d, meat3$d)
p = c(meat1$pval.d, meat2$pval.d, meat3$pval.d)
ci = c((meat1$u.d-meat1$d),(meat2$u.d-meat2$d),(meat3$u.d-meat3$d))
names = c(rep("ctaMeatConsump", 3))

cta.meat = as.data.frame(cta)
cta.meat$names = names
cta.meat$d = d
cta.meat$ci = ci
cta.meat$p = p
cta.meat


# ---------------------------------------------------------------------------------------------------------------------
# MEAT CONSUMPTION PLOT
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(xmax = cta.meat$d + cta.meat$ci, xmin=cta.meat$d - cta.meat$ci)

p = ggplot(data=cta.meat, aes(x=d, y=reorder(cta, +d))) +
  scale_x_continuous(limits = c(-0.1,0.25), labels = c("-0.1", "No call\nto action", "0.1", "0.2", "0.3")) +
  geom_errorbarh(limits, height=0.1, size =0.3, colour="grey48") + 
  geom_point(size=5, colour='grey48') +
  labs(y='', x='\nStandardized effect size (d)', title='Standardized effect sizes for call to action on intention \nto reduce meat consumption (with 95% CIs)\n') +
  geom_vline(xintercept=0) 
p <-  p + theme(axis.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 23, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 30, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,4,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-meat-consumption-cta.png', plot=p, width=40, height=25, dpi=300, units='cm')



# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES
# VSG
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#cta1 vs nocta
vsg1 = mes(mean(df$vsgBinary[df$ctaTreatment=="cta1"], na.rm=TRUE), 
            mean(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="cta1"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sum(df$ctaTreatment=="cta1"), 
            sum(df$ctaTreatment=="noCta"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta2 vs nocta
vsg2 = mes(mean(df$vsgBinary[df$ctaTreatment=="cta2"], na.rm=TRUE), 
            mean(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="cta2"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sum(df$ctaTreatment=="cta2"), 
            sum(df$ctaTreatment=="noCta"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta3 vs nocta
vsg3 = mes(mean(df$vsgBinary[df$ctaTreatment=="cta3"], na.rm=TRUE), 
            mean(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="cta3"], na.rm=TRUE), 
            sd(df$vsgBinary[df$ctaTreatment=="noCta"], na.rm=TRUE), 
            sum(df$ctaTreatment=="cta3"), 
            sum(df$ctaTreatment=="noCta"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

cta = c("Please leave animals off \nyour plate", "Please cut out or cut back\non animal products",
        "Please choose vegan")

d = c(vsg1$d, vsg2$d, vsg3$d)
ci = c((vsg1$u.d-vsg1$d),(vsg2$u.d-vsg2$d),(vsg3$u.d-vsg3$d)) 
p = c(vsg1$pval.d, vsg2$pval.d, vsg3$pval.d)
names = c(rep("ctaVSG", 3))

cta.vsg = as.data.frame(cta)
cta.vsg$names = names
cta.vsg$d = d
cta.vsg$ci = ci
cta.vsg$p = p
cta.vsg

# ---------------------------------------------------------------------------------------------------------------------
# VSG  PLOT
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(xmax = cta.vsg$d + cta.vsg$ci, xmin=cta.vsg$d - cta.vsg$ci)

p = ggplot(data=cta.vsg, aes(x=d, y=reorder(cta, +d))) +
  scale_x_continuous(limits = c(-0.2,0.2), labels = c("-0.2", "-0.1", "No call\nto action", "0.1", "0.2")) +
  geom_errorbarh(limits, height=0.1, size =0.3, colour="grey48") + 
  geom_point(size=5, colour='grey48') +
  labs(y='', x='\nStandardized effect size (d)', title='Standardized effect sizes for call to action on Vegetarian \nStarter Guide requests (with 95% CIs)\n') +
  geom_vline(xintercept=0) 
p <-  p + theme(axis.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 23, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 30, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,4,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-vsg-cta.png', plot=p, width=40, height=25, dpi=300, units='cm')



# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES
# COMBINED OUTCOME
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#cta1 vs nocta
all1 = mes(mean(df$combinedOutcome3.std[df$ctaTreatment=="cta1"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="cta1"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sum(df$ctaTreatment=="cta1"), 
           sum(df$ctaTreatment=="noCta"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta2 vs nocta
all2 = mes(mean(df$combinedOutcome3.std[df$ctaTreatment=="cta2"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="cta2"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sum(df$ctaTreatment=="cta2"), 
           sum(df$ctaTreatment=="noCta"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cta3 vs nocta
all3 = mes(mean(df$combinedOutcome3.std[df$ctaTreatment=="cta3"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="cta3"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$ctaTreatment=="noCta"], na.rm=TRUE), 
           sum(df$ctaTreatment=="cta3"), 
           sum(df$ctaTreatment=="noCta"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

cta = c("Please leave animals off \nyour plate", "Please cut out or cut back\non animal products",
        "Please choose vegan")

d = c(all1$d, all2$d, all3$d)
ci = c((all1$u.d-all1$d),(all2$u.d-all2$d),(all3$u.d-all3$d)) 
p = c(all1$pval.d, all2$pval.d, all3$pval.d)
names = c(rep("ctaCombinedOutcome", 3))

cta.all = as.data.frame(cta)
cta.all$names = names
cta.all$d = d
cta.all$ci = ci
cta.all$p = p
cta.all


# ---------------------------------------------------------------------------------------------------------------------
# COMBINED OUTCOME  PLOT
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(xmax = cta.all$d + cta.all$ci, xmin=cta.all$d - cta.all$ci)

p = ggplot(data=cta.all, aes(x=d, y=reorder(cta, +d))) +
  scale_x_continuous(limits = c(-0.2,0.2), labels = c("-0.2", "-0.1", "No call\nto action", "0.1", "0.2")) +
  geom_errorbarh(limits, height=0.1, size =0.3, colour="grey48") + 
  geom_point(size=5, colour='grey48') +
  labs(y='', x='\nStandardized effect size (d)', title='Standardized effect sizes for call to action on \ncombined outcome (with 95% CIs)\n') +
  geom_vline(xintercept=0) 
#theme(axis.text.y = element_text(angle = 60, hjust = 1))
p <-  p + theme(axis.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 23, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 30, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,4,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-all-outcomes-cta.png', plot=p, width=40, height=25, dpi=300, units='cm')



# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# MULTIPLE HYPOTHESIS TEST CORRECTION
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------
# COMBINES TABLES FOR HYPOTHESIS TEST CORRECTION
# ---------------------------------------------------------------------------------------------------------------------

# combines tables
all.tests = rbind(cta.meat,cta.vsg, cta.all)

# ---------------------------------------------------------------------------------------------------------------------
# CORRECTION
# ---------------------------------------------------------------------------------------------------------------------

#changes working directory for function
setwd("/Users/Krystal/Google Drive/research/active-projects/video study/r_code/functions/")
source('wfdr.R')

# function
wfdr_wrapper <- function(data, pvalue_col, alpha=0.05, w=NULL, sort=TRUE) {
  wfdr_result <- wfdr(data[,pvalue_col], alpha, w)
  result <- cbind(data, wfdr_result)
  colnames(result) <- c(colnames(data), 'rejected')
  if (sort) {
    result <- result[order(-result$rejected, result[, pvalue_col]),]
  } 
  return(result)
}

get_weights <- function(data, n_important, important_total_prop) {
  n_tests <- nrow(data)
  important_weight <- n_tests*important_total_prop/n_important
  important_weights <- rep(important_weight, n_important)
  sum_important <- sum(important_weights)
  n_unimportant <- nrow(data) - n_important
  unimportant_weight <- (n_tests)*(1-important_total_prop)/n_unimportant
  unimportant_weights <- rep(unimportant_weight, n_unimportant)
  weights <- c(important_weights, unimportant_weights)
  # print(weights)
  if (sum(weights)!=n_tests) stop('Sum of weights must equal number of tests.')
  return(weights)
}


sort <- FALSE
weights <- get_weights(all.tests, 3, 0.5) # places 50% of the weight on the meat consumption outcome 


# correction at the 85% level
# I reported results from this correction
corrected15 <- wfdr_wrapper(all.tests, 'p',alpha=0.15, sort=sort, w=weights)
corrected15

# correction at the 95% level
corrected05 <- wfdr_wrapper(all.tests, 'p',alpha=0.05, sort=sort, w=weights)
corrected05















