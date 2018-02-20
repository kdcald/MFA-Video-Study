

# ------------------------------------------------------------------------------------------------------------
# ----- SETS WORKING DIRECTORY ------
# ------------------------------------------------------------------------------------------------------------

# set working directory
kcPath = "/Users/Krystal/Google Drive/research/active-projects/video study"
setwd(kcPath)
getwd()

library(compute.es)

# ------------------------------------------------------------------------------------------------------------
# ----- IMPORTS DATA ------
# ------------------------------------------------------------------------------------------------------------

df <- read.csv("data_final/cleanVideoDataForAnalysis.csv", stringsAsFactors = FALSE)


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# METHOD 1
# average across 6 attitudes, meat consump, vsg 
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------


#compare vs control
method1.1 = mes(mean(df$combinedOutcome1.std[df$videoTreatment=="compare"], na.rm=TRUE), 
                mean(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="compare"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="compare"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
method1.2 = mes(mean(df$combinedOutcome1.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
                mean(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="cruel"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
method1.3 = mes(mean(df$combinedOutcome1.std[df$videoTreatment=="cute"], na.rm=TRUE), 
                mean(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="cute"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="cute"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
method1.4 = mes(mean(df$combinedOutcome1.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
                mean(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
                sd(df$combinedOutcome1.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="lifestyle"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(method1.1$d, method1.2$d, method1.3$d, method1.4$d)
ci = c((method1.1$u.d-method1.1$d),(method1.2$u.d-method1.2$d),(method1.3$u.d-method1.3$d),(method1.4$u.d-method1.4$d)) 
p = c(method1.1$pval.d, method1.2$pval.d, method1.3$pval.d, method1.4$pval.d)

videoType.method1 = as.data.frame(videoType)
videoType.method1$d = d
videoType.method1$ci = ci
videoType.method1$p = p
videoType.method1


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# METHOD 2
# average across food attitudes, animal attitudes, meat consump, vsg 
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------


#compare vs control
method2.1 = mes(mean(df$combinedOutcome2.std[df$videoTreatment=="compare"], na.rm=TRUE), 
                mean(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="compare"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="compare"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
method2.2 = mes(mean(df$combinedOutcome2.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
                mean(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="cruel"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
method2.3 = mes(mean(df$combinedOutcome2.std[df$videoTreatment=="cute"], na.rm=TRUE), 
                mean(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="cute"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="cute"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
method2.4 = mes(mean(df$combinedOutcome2.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
                mean(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
                sd(df$combinedOutcome2.std[df$videoTreatment=="control"], na.rm=TRUE), 
                sum(df$videoTreatment=="lifestyle"), 
                sum(df$videoTreatment=="control"),
                level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(method2.1$d, method2.2$d, method2.3$d, method2.4$d)
ci = c((method2.1$u.d-method2.1$d),(method2.2$u.d-method2.2$d),(method2.3$u.d-method2.3$d),(method2.4$u.d-method2.4$d)) 
p = c(method2.1$pval.d, method2.2$pval.d, method2.3$pval.d, method2.4$pval.d)

videoType.method2 = as.data.frame(videoType)
videoType.method2$d = d
videoType.method2$ci = ci
videoType.method2$p = p
videoType.method2

# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# METHOD 3
# average across combined "all attitudes", meat consumption, and vsg
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

#compare vs control
method3.1 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="compare"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="compare"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="compare"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
method3.2 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="cruel"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="cruel"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
method3.3 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="cute"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="cute"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="cute"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
method3.4 = mes(mean(df$combinedOutcome3.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
           mean(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="lifestyle"], na.rm=TRUE), 
           sd(df$combinedOutcome3.std[df$videoTreatment=="control"], na.rm=TRUE), 
           sum(df$videoTreatment=="lifestyle"), 
           sum(df$videoTreatment=="control"),
           level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(method3.1$d, method3.2$d, method3.3$d, method3.4$d)
ci = c((method3.1$u.d-method3.1$d),(method3.2$u.d-method3.2$d),(method3.3$u.d-method3.3$d),(method3.4$u.d-method3.4$d)) 
p = c(method3.1$pval.d, method3.2$pval.d, method3.3$pval.d, method3.4$pval.d)

videoType.method3 = as.data.frame(videoType)
videoType.method3$d = d
videoType.method3$ci = ci
videoType.method3$p = p
videoType.method3


# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------
# METHOD 3
# principal components analysis
# ---------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------
# ----- TAKES ONLY COMPLETE CASES FOR OUTCOME VARIABLES/REMOVES NAs ------
# ------------------------------------------------------------------------------------------------------------

df.complete = df[complete.cases(df[,c(50:57)]),]

# ------------------------------------------------------------------------------------------------------------
# ----- PRINCIPAL COMPONENTS ANALYSIS ------
# ------------------------------------------------------------------------------------------------------------

colnames(df)

fit <- princomp(df.complete[50:57], cor=TRUE)

summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 

plot(fit,type="lines") # scree plot 

fit$scores # the principal components

# ------------------------------------------------------------------------------------------------------------
# ----- CREATES DATAFRAME OF PCA SCORES ------
# ----- multiplied by -1 to make scores positive 
# ------------------------------------------------------------------------------------------------------------

pcScores = as.data.frame(fit$scores*-1)

#------------------------------------------------------------------------------------------------------------
# ----- STANDARDIZED EFFECT SIZES - PCA COMPONENT 1------
# ------------------------------------------------------------------------------------------------------------

#compare vs control
pca1 = mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="compare"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="compare"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="compare"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
pca2 = mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="cruel"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="cruel"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="cruel"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
pca3 = mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="cute"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="cute"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="cute"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
pca4 = mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="lifestyle"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="lifestyle"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="lifestyle"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

videoType = c("Comparison", "Cruel", "Cute", "Lifestyle")

d = c(pca1$d, pca2$d, pca3$d, pca4$d)
ci = c((pca1$u.d-pca1$d),(pca2$u.d-pca2$d),(pca3$u.d-pca3$d),(pca4$u.d-pca4$d)) 
p = c(pca1$pval.d, pca2$pval.d, pca3$pval.d, pca4$pval.d)

pca = as.data.frame(videoType)
pca$d = d
pca$ci = ci
pca$p = p
pca

