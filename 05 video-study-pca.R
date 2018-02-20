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


# ------------------------------------------------------------------------------------------------------------
# ----- REGRESSION WITH PCA COMPONENT 1------
# ------------------------------------------------------------------------------------------------------------

df.complete$videoTreatment = as.factor(df.complete$videoTreatment)
df.complete <- within(df.complete, videoTreatment <- relevel(videoTreatment, ref = "control"))

summary(lm(pcScores$Comp.1 ~ df.complete$videoTreatment))


#------------------------------------------------------------------------------------------------------------
# ----- STANDARDIZED EFFECT SIZES - PCA COMPONENT 1------
# ------------------------------------------------------------------------------------------------------------

#compare vs control
mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="compare"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="compare"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="compare"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cruel vs control
mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="cruel"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="cruel"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="cruel"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#cute vs control
mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="cute"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="cute"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="cute"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)

#lifestyle vs control
mes(mean(pcScores$Comp.1[df.complete$videoTreatment=="lifestyle"], na.rm=TRUE), 
    mean(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="lifestyle"], na.rm=TRUE), 
    sd(pcScores$Comp.1[df.complete$videoTreatment=="control"], na.rm=TRUE), 
    sum(df.complete$videoTreatment=="lifestyle"), 
    sum(df.complete$videoTreatment=="control"),
    level = 95, cer = 0.2, dig = 10, verbose = TRUE, id=NULL, data=NULL)


