

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

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ----- VIDEO TYPE ANALYSIS ----
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------
# ----- REGRESSIONS - VIDEO TYPE ----
# ---------------------------------------------------------------------------------------------------------

df$videoTreatment = as.factor(df$videoTreatment)
df <- within(df, videoTreatment <- relevel(videoTreatment, ref = "control"))

summary(lm(meatConsumpReduce ~ videoTreatment, data=df))

summary(lm(vsgBinary ~ videoTreatment, data=df))

summary(lm(attitude1Num ~ videoTreatment, data=df))
summary(lm(attitude2Num ~ videoTreatment, data=df))
summary(lm(attitude3Num ~ videoTreatment, data=df))
summary(lm(attitude4Num ~ videoTreatment, data=df))
summary(lm(attitude5Num ~ videoTreatment, data=df))
summary(lm(attitude6Num ~ videoTreatment, data=df))

summary(lm(combinedFoodAttitudes.std ~ videoTreatment, data=df))
summary(lm(combinedAnimalAttitudes.std ~ videoTreatment, data=df))
summary(lm(combinedAllAttitudes.std ~ videoTreatment, data=df))

summary(lm(combinedOutcome1.std ~ videoTreatment, data=df))
summary(lm(combinedOutcome2.std ~ videoTreatment, data=df))
summary(lm(combinedOutcome3.std ~ videoTreatment, data=df))


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES - VIDEO TYPE
# MEAT CONSUMPTION
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#compare vs control
meat1 = mes(mean(df$meatConsumpReduce[df$videoTreatment=="compare"], na.rm=TRUE), 
        mean(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="compare"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sum(df$videoTreatment=="compare"), 
        sum(df$videoTreatment=="control"),
        level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
meat2 = mes(mean(df$meatConsumpReduce[df$videoTreatment=="cruel"], na.rm=TRUE), 
        mean(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="cruel"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sum(df$videoTreatment=="cruel"), 
        sum(df$videoTreatment=="control"),
        level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
meat3 = mes(mean(df$meatConsumpReduce[df$videoTreatment=="cute"], na.rm=TRUE), 
        mean(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="cute"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sum(df$videoTreatment=="cute"), 
        sum(df$videoTreatment=="control"),
        level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
meat4 = mes(mean(df$meatConsumpReduce[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
        mean(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
        sd(df$meatConsumpReduce[df$videoTreatment=="control"], na.rm=TRUE), 
        sum(df$videoTreatment=="lifestyle"), 
        sum(df$videoTreatment=="control"),
        level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(meat1$d, meat2$d, meat3$d, meat4$d)
p = c(meat1$pval.d, meat2$pval.d, meat3$pval.d, meat4$pval.d)
ci = c((meat1$u.d-meat1$d),(meat2$u.d-meat2$d),(meat3$u.d-meat3$d),(meat4$u.d-meat4$d)) 
name = c(rep("videoTypeMeatConsump", 4))

videoType.meat = as.data.frame(videoType)
videoType.meat$name = name
videoType.meat$d = d
videoType.meat$ci = ci
videoType.meat$p = p
videoType.meat


# ---------------------------------------------------------------------------------------------------------------------
# MEAT CONSUMPTION PLOT - VIDEO TYPE
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(ymax = videoType.meat$d + videoType.meat$ci, ymin=videoType.meat$d - videoType.meat$ci)

p = ggplot(data=videoType.meat, aes(x=videoType, y=d)) +
  scale_y_continuous(limits = c(-0.08,0.35),labels = c("-0.1", "Control\nvideo", "0.1", "0.2", "0.3", "0.4")) +
  geom_errorbar(limits, position='dodge', size = .75, width=0.05, colour="grey48") +
  geom_point(size=5, colour='grey48') +
  labs(x='\nType of video', y='Standardized effect size (d)\n', title='Standardized effect sizes for video type on intention \nto reduce meat consumption (with 95% CIs)\n') +
  geom_hline(yintercept=0)
p <-  p + theme(axis.title = element_text(size = 20, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,2,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-meat-consump.png', plot=p, width=25, height=25, dpi=300, units='cm')



# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES - VIDEO TYPE
# VSG
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#compare vs control
vsg1 = mes(mean(df$vsgBinary[df$videoTreatment=="compare"], na.rm=TRUE), 
            mean(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="compare"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sum(df$videoTreatment=="compare"), 
            sum(df$videoTreatment=="control"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
vsg2 = mes(mean(df$vsgBinary[df$videoTreatment=="cruel"], na.rm=TRUE), 
            mean(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="cruel"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sum(df$videoTreatment=="cruel"), 
            sum(df$videoTreatment=="control"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
vsg3 = mes(mean(df$vsgBinary[df$videoTreatment=="cute"], na.rm=TRUE), 
            mean(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="cute"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sum(df$videoTreatment=="cute"), 
            sum(df$videoTreatment=="control"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
vsg4 = mes(mean(df$vsgBinary[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
            mean(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
            sd(df$vsgBinary[df$videoTreatment=="control"], na.rm=TRUE), 
            sum(df$videoTreatment=="lifestyle"), 
            sum(df$videoTreatment=="control"),
            level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(vsg1$d, vsg2$d, vsg3$d, vsg4$d)
p = c(vsg1$pval.d, vsg2$pval.d, vsg3$pval.d, vsg4$pval.d)
ci = c((vsg1$u.d-vsg1$d),(vsg2$u.d-vsg2$d),(vsg3$u.d-vsg3$d),(vsg4$u.d-vsg4$d)) 
name = c(rep("videoTypeVSG", 4))

videoType.vsg = as.data.frame(videoType)
videoType.vsg$name = name
videoType.vsg$d = d
videoType.vsg$ci = ci
videoType.vsg$p = p
videoType.vsg

# ---------------------------------------------------------------------------------------------------------------------
# VSG  PLOT - VIDEO TYPE
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(ymax = videoType.vsg$d + videoType.vsg$ci, ymin=videoType.vsg$d - videoType.vsg$ci)

p = ggplot(data=videoType.vsg, aes(x=reorder(videoType, -d), y=d)) +
  scale_y_continuous(limits = c(-0.05,0.25),labels = c("-0.1", "Control\nvideo", "0.1", "0.2", "0.3")) +
  geom_errorbar(limits, position='dodge', size = .75, width=0.05, colour="grey48") +
  geom_point(size=5, colour='grey48') +
  labs(x='\nType of video', y='Standardized effect size (d)\n', title='Standardized effect sizes for video type on Vegetarian \nStarter Guide requests (with 95% CIs)\n') +
  geom_hline(yintercept=0)
p <-  p + theme(axis.title = element_text(size = 20, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,3,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-vsg.png', plot=p, width=25, height=25, dpi=300, units='cm')



# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES - VIDEO TYPE
# COMBINED OUTCOME
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#compare vs control
all1 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="compare"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="compare"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="compare"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
all2 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="cruel"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
all3 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="cute"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="cute"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="cute"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
all4 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="lifestyle"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(all1$d, all2$d, all3$d, all4$d)
p = c(all1$pval.d, all2$pval.d, all3$pval.d, all4$pval.d)
ci = c((all1$u.d-all1$d),(all2$u.d-all2$d),(all3$u.d-all3$d),(all4$u.d-all4$d)) 
name = c(rep("videoTypeCombined", 4))

videoType.all = as.data.frame(videoType)
videoType.all$name = name
videoType.all$d = d
videoType.all$ci = ci
videoType.all$p = p
videoType.all

# ---------------------------------------------------------------------------------------------------------------------
# COMBINED OUTCOME  PLOT - VIDEO TYPE
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(ymax = videoType.all$d + videoType.all$ci, ymin=videoType.all$d - videoType.all$ci)

p = ggplot(data=videoType.all, aes(x=videoType, y=d)) +
  scale_y_continuous(limits = c(-0.08,0.35),labels = c("-0.1", "Control\nvideo", "0.1", "0.2", "0.3", "0.4")) +
  geom_errorbar(limits, position='dodge', size = .75, width=0.05, colour="grey48") +
  geom_point(size=5, colour='grey48') +
  labs(x='\nType of video', y='Standardized effect size (d)\n', title='Standardized effect sizes for video type on \ncombined outcome\n') +
  geom_hline(yintercept=0)
p <-  p + theme(axis.title = element_text(size = 20, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,2,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-all-outcomes.png', plot=p, width=25, height=25, dpi=300, units='cm')




# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------
# ----- SPECIFIC VIDEOS ANALYSIS ----
# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------------
# ----- REGRESSIONS - SPECIFIC VIDEO ----
# ---------------------------------------------------------------------------------------------------------

df$video = as.factor(df$video)
df <- within(df, video <- relevel(video, ref = "control"))

summary(lm(meatConsumpReduce ~ video, data=df))

summary(lm(vsgBinary ~ video, data=df))

summary(lm(attitude1Num ~ video, data=df))
summary(lm(attitude2Num ~ video, data=df))
summary(lm(attitude3Num ~ video, data=df))
summary(lm(attitude4Num ~ video, data=df))
summary(lm(attitude5Num ~ video, data=df))
summary(lm(attitude6Num ~ video, data=df))

summary(lm(combinedFoodAttitudes.std ~ video, data=df))
summary(lm(combinedAnimalAttitudes.std ~ video, data=df))
summary(lm(combinedAllAttitudes.std ~ video, data=df))

summary(lm(combinedOutcome1.std ~ video, data=df))
summary(lm(combinedOutcome2.std ~ video, data=df))
summary(lm(combinedOutcome3.std ~ video, data=df))


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# STANDARDIZED EFFECT SIZES - SPECIFIC VIDEO
# COMBINED OUTCOME
# computes std effect sizes and puts them in a dataframe for plotting
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

video1 = mes(mean(df$combinedOutcome3.std[df$video=="babyAnimals"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="babyAnimals"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="babyAnimals"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video2 = mes(mean(df$combinedOutcome3.std[df$video=="benny"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="benny"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="benny"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video3 = mes(mean(df$combinedOutcome3.std[df$video=="choiceYours"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="choiceYours"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="choiceYours"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video4 = mes(mean(df$combinedOutcome3.std[df$video=="happyFree"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="happyFree"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="happyFree"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video5 = mes(mean(df$combinedOutcome3.std[df$video=="henHell"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="henHell"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="henHell"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video6 = mes(mean(df$combinedOutcome3.std[df$video=="kitchen"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="kitchen"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="kitchen"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video7 = mes(mean(df$combinedOutcome3.std[df$video=="loseAppetite"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="loseAppetite"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="loseAppetite"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video8 = mes(mean(df$combinedOutcome3.std[df$video=="loveDogsPigs"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="loveDogsPigs"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="loveDogsPigs"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video9 = mes(mean(df$combinedOutcome3.std[df$video=="sleepyAnimals"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="sleepyAnimals"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="sleepyAnimals"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video10 = mes(mean(df$combinedOutcome3.std[df$video=="stuffEat"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="stuffEat"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="stuffEat"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video11 = mes(mean(df$combinedOutcome3.std[df$video=="veganSwap"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="veganSwap"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="veganSwap"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

video12 = mes(mean(df$combinedOutcome3.std[df$video=="weSame"], na.rm=TRUE), 
    mean(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="weSame"], na.rm=TRUE), 
    sd(df$combinedOutcome3.std[df$video=="control"], na.rm=TRUE), 
    sum(df$video=="weSame"), 
    sum(df$video=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)


video = c("Baby animals", "Benny's life", "Choice is yours", "Happy & free", "Hen hell", "Kitchen makeover", 
              "Lose your appetite", "Love dogs, pigs", "Sleepy animals", "Stuff I eat", "Vegan swaps", "We are same")

d = c(video1$d, video2$d, video3$d, video4$d, video5$d, video6$d, video7$d, video8$d, video9$d, video10$d, video11$d, video12$d)
p = c(video1$pval.d, video2$pval.d, video3$pval.d, video4$pval.d, video5$pval.d, video6$pval.d, video7$pval.d, video8$pval.d, video9$pval.d, video10$pval.d, video11$pval.d, video12$pval.d)
ci = c((video1$u.d-video1$d),(video2$u.d-video2$d),(video3$u.d-video3$d),(video4$u.d-video4$d),
        (video5$u.d-video5$d),(video6$u.d-video6$d),(video7$u.d-video7$d),(video8$u.d-video8$d),
        (video9$u.d-video9$d),(video10$u.d-video10$d),(video11$u.d-video11$d),(video12$u.d-video12$d)) 
name = c(rep("specVideoCombined",12))

video.all = as.data.frame(video)
video.all$name = name
video.all$d = d
video.all$ci = ci
video.all$p = p
video.all

# ---------------------------------------------------------------------------------------------------------------------
# COMBINED OUTCOME  PLOT - SPECIFIC VIDEO
# ---------------------------------------------------------------------------------------------------------------------

limits <- aes(xmax = video.all$d + video.all$ci, xmin=video.all$d - video.all$ci)

p = ggplot(data=video.all, aes(x=d, y=reorder(video, +d))) +
  scale_x_continuous(limits = c(-0.2,0.5), labels = c("-0.2", "Control\nvideo", "0.1", "0.2", "0.3")) +
  geom_errorbarh(limits, height=0.2, size =0.3, colour="grey48") + 
  geom_point(size=5, colour='grey48') +
  labs(y='', x='\nStandardized effect size (d)', title='Standardized effect sizes for specific video on \ncombined outcome (with 95% CIs)\n') +
  geom_vline(xintercept=0) 
#theme(axis.text.y = element_text(angle = 60, hjust = 1))
p <-  p + theme(axis.title = element_text(size = 20, face = "bold", family = "Trebuchet MS"),
                axis.text = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
                plot.title = element_text(size = 25, face = "bold", family = "Trebuchet MS"))
p <- p + theme(plot.margin = unit(c(1,3,1,1), "cm"))
p  

# writes plot
ggsave(filename='analysis/final-figures/se-specific-video-combined-outcome.png', plot=p, width=25, height=25, dpi=300, units='cm')


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

# changes name of specific video table for combining
colnames(video.all) = c("videoType", "name", "d", "ci", "p")

# combines tables
all.tests = rbind(videoType.meat,videoType.vsg, videoType.all, video.all)

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
weights <- get_weights(all.tests, 4, 0.5) # places 50% of the weight on the meat consumption outcome 


# correction at the 85% level
# I reported results from this correction
corrected15 <- wfdr_wrapper(all.tests, 'p',alpha=0.15, sort=sort, w=weights)
corrected15

# correction at the 95% level
corrected05 <- wfdr_wrapper(all.tests, 'p',alpha=0.05, sort=sort, w=weights)
corrected05






