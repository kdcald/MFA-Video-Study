
# ------------------------------------------------------------------------------------------------------------
# ----- SETS WORKING DIRECTORY ------
# ------------------------------------------------------------------------------------------------------------

# set working directory
kcPath = "/Users/Krystal/Google Drive/research/active-projects/video study/data_final"
setwd(kcPath)
getwd()

# ------------------------------------------------------------------------------------------------------------
# ----- IMPORTS DATA ------
# ------------------------------------------------------------------------------------------------------------

# imports data from Part 1 of video survey
videoSurvey_wave1 <- read.csv("videoSurvey_wave1.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))
videoSurvey_wave2 <- read.csv("videoSurvey_wave2.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))

# imports data from Part 2 of video survey
videoSurvey_wave1_PART2 <- read.csv("videoSurvey_wave1_PART2.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))
videoSurvey_wave2_PART2 <- read.csv("videoSurvey_wave2_PART2.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))

# control only data
controlOnly_wave1 <- read.csv("videoSurvey_controlOnly_wave1.csv",  stringsAsFactors = FALSE)
controlOnly_wave2 <- read.csv("videoSurvey_controlOnly_wave2.csv",  stringsAsFactors = FALSE)


# ------------------------------------------------------------------------------------------------------------
# ----- CREATES VARIABLE TO IDENTIFY WHICH STUDY PART THE DATA CAME FROM
# ------------------------------------------------------------------------------------------------------------

videoSurvey_wave1$study = "part1"
videoSurvey_wave2$study = "part1"

videoSurvey_wave1_PART2$study = "part2"
videoSurvey_wave2_PART2$study = "part2"

controlOnly_wave1$study = "controlOnly"
controlOnly_wave2$study = "controlOnly"


# ------------------------------------------------------------------------------------------------------------
# ----- REMOVES "USER" COLUMN
# ------------------------------------------------------------------------------------------------------------

controlOnly_wave1_minusUser = controlOnly_wave1[ , -which(names(controlOnly_wave1) %in% c("User"))]

controlOnly_wave2_minusUser = controlOnly_wave2[ , -which(names(controlOnly_wave2) %in% c("User"))]

videoSurvey_wave2_minusUser = videoSurvey_wave2[ , -which(names(videoSurvey_wave2) %in% c("User"))]

videoSurvey_wave2_PART2_minusUser = videoSurvey_wave2_PART2[ , -which(names(videoSurvey_wave2_PART2) %in% c("User"))]

# ------------------------------------------------------------------------------------------------------------
# ----- COMBINES ALL DATA ------
# ----- results in two data frames: one for for wave 1 and one for wave 2 ------
# ------------------------------------------------------------------------------------------------------------

# combines data for wave 1
combined.wave1 = rbind(videoSurvey_wave1,videoSurvey_wave1_PART2, controlOnly_wave1_minusUser)
#combined.wave1.count = combined.wave1[!duplicated(combined.wave1$MTurk.ID..a7i4jfe.),]

# combines data for wave 2 
combined.wave2 = rbind(videoSurvey_wave2_minusUser,videoSurvey_wave2_PART2_minusUser, controlOnly_wave2_minusUser)
combined.wave2.count = combined.wave2[!duplicated(combined.wave2$MTurk.ID..a7i4jfe.),]


# ------------------------------------------------------------------------------------------------------------
# ----- MERGES WAVE 1 AND WAVE 2 DATA  ------
# ----- by MTurk ID (by var "MTurk.ID..a7i4jfe.") ----
# ------------------------------------------------------------------------------------------------------------

# merges wave 1 and wave 2 data by MTurk ID
both.waves = merge(combined.wave1,combined.wave2, by = "MTurk.ID..a7i4jfe.")

# only takes complete cases from merge
both.waves = both.waves[complete.cases(both.waves),]


# ------------------------------------------------------------------------------------------------------------
# ----- CREATES DATA FRAME OF ONLY UNIQUE MTURK IDs  ------
# ------------------------------------------------------------------------------------------------------------
# ----- note: there was an issue of a few people taking the survey several times ----
# ----- note: I *think* that "duplicated" is taking the first instance of the repeated variables. 
# -----       i checked this for several variables, but could should be confirmed further
# ------------------------------------------------------------------------------------------------------------

# creates a dataframe of only unique MTurk IDs
both.waves.unique <- both.waves[!duplicated(both.waves$MTurk.ID..a7i4jfe.),]

# creates dataframe of the repeated and discarded MTurk IDs
both.waves.duplicates = both.waves[duplicated(both.waves$MTurk.ID..a7i4jfe.),]

# ----
# # checks for duplicates 
# anyDuplicated(both.waves$MTurk.ID..a7i4jfe.)

# unique(both.waves$MTurk.ID..a7i4jfe.)


# ------------------------------------------------------------------------------------------------------------
# ----- VARIABLE CLEANING AND CREATION ------
# ----- cleans dataframe "both.waves.unique"
# ------------------------------------------------------------------------------------------------------------

# CREATES VARIABLES FOR VIDEO TREATMENT GROUPS
# -----------------------------

unique(both.waves.unique$Experiment..videos., na.rm=FALSE)

cute = c("babyAnimalsCTA1", "babyAnimalsCTA2", "babyAnimalsCTA3", "babyAnimalsCTA4",
          "sleepyAnimalsCTA1","sleepyAnimalsCTA2","sleepyAnimalsCTA3","sleepyAnimalsCTA4",
          "loveDogsPigsCTA1","loveDogsPigsCTA2","loveDogsPigsCTA3","loveDogsPigsCTA4")

compare = c("choiceYoursCTA1","choiceYoursCTA2","choiceYoursCTA3","choiceYoursCTA4",
            "happyFreeCTA1","happyFreeCTA2","happyFreeCTA3","happyFreeCTA4",
            "weSameCTA1","weSameCTA2","weSameCTA3","weSameCTA4")

cruel = c("henHellCTA1","henHellCTA2","henHellCTA3","henHellCTA4",
          "loseAppetiteCTA1","loseAppetiteCTA2","loseAppetiteCTA3","loseAppetiteCTA4",
          "bennyCTA1","bennyCTA2","bennyCTA3","bennyCTA4")

lifestyle = c("kitchenCTA1","kitchenCTA2","kitchenCTA3","kitchenCTA4",
              "veganSwapCTA1","veganSwapCTA2","veganSwapCTA3","veganSwapCTA4",
              "stuffEatCTA1","stuffEatCTA2","stuffEatCTA3","stuffEatCTA4")

control = c("controlCTA1","controlCTA2","controlCTA3","controlCTA4")

both.waves.unique$videoTreatment = NA

both.waves.unique$videoTreatment[which(both.waves.unique$Experiment..videos. %in% cute)] = "cute"
both.waves.unique$videoTreatment[which(both.waves.unique$Experiment..videos. %in% compare)] = "compare"
both.waves.unique$videoTreatment[which(both.waves.unique$Experiment..videos. %in% cruel)] = "cruel"
both.waves.unique$videoTreatment[which(both.waves.unique$Experiment..videos. %in% lifestyle)] = "lifestyle"
both.waves.unique$videoTreatment[which(both.waves.unique$Experiment..videos. %in% control)] = "control"

table(both.waves.unique$videoTreatment, exclude = NULL)


# CREATES VARIABLES FOR CTA TREATMENT GROUPS
# -----------------------------

cta1 = c("babyAnimalsCTA1", "sleepyAnimalsCTA1", "loveDogsPigsCTA1",
         "choiceYoursCTA1","happyFreeCTA1","weSameCTA1",
         "henHellCTA1","loseAppetiteCTA1","bennyCTA1",
         "kitchenCTA1","veganSwapCTA1","stuffEatCTA1", "controlCTA1")

cta2 = c("babyAnimalsCTA2", "sleepyAnimalsCTA2", "loveDogsPigsCTA2",
         "choiceYoursCTA2","happyFreeCTA2","weSameCTA2",
         "henHellCTA2","loseAppetiteCTA2","bennyCTA2",
         "kitchenCTA2","veganSwapCTA2","stuffEatCTA2", "controlCTA2")


cta3 = c("babyAnimalsCTA3", "sleepyAnimalsCTA3", "loveDogsPigsCTA3",
         "choiceYoursCTA3","happyFreeCTA3","weSameCTA3",
         "henHellCTA3","loseAppetiteCTA3","bennyCTA3",
         "kitchenCTA3","veganSwapCTA3","stuffEatCTA3", "controlCTA3")

no.cta = c("babyAnimalsCTA4", "sleepyAnimalsCTA4", "loveDogsPigsCTA4",
         "choiceYoursCTA4","happyFreeCTA4","weSameCTA4",
         "henHellCTA4","loseAppetiteCTA4","bennyCTA4",
         "kitchenCTA4","veganSwapCTA4","stuffEatCTA4", "controlCTA4")


both.waves.unique$ctaTreatment = NA

both.waves.unique$ctaTreatment[which(both.waves.unique$Experiment..videos. %in% cta1)] = "cta1"
both.waves.unique$ctaTreatment[which(both.waves.unique$Experiment..videos. %in% cta2)] = "cta2"
both.waves.unique$ctaTreatment[which(both.waves.unique$Experiment..videos. %in% cta3)] = "cta3"
both.waves.unique$ctaTreatment[which(both.waves.unique$Experiment..videos. %in% no.cta)] = "noCta"

table(both.waves.unique$ctaTreatment, exclude = NULL)


# VIDEO EMOTIONS
# -----------------------------

aLittle = c("A litte ", "A litte  | A litte ", "A litte  | A litte  | A litte ")
extremely = c("Extremely ", "Extremely  | Extremely ", "Extremely  | Extremely  | Extremely ")
moderately = c("Moderately ", "Moderately  | Moderately ", "Moderately  | Moderately  | Moderately ")
quiteAbit = c("Quite a bit ", "Quite a bit  | Quite a bit ", "Quite a bit  | Quite a bit  | Quite a bit ")
slightlyNotAtAll = c("Slightly/Not at all ", "Slightly/Not at all  | Slightly/Not at all ",
                     "Slightly/Not at all  | Slightly/Not at all  | Slightly/Not at all  | Slightly/Not at all  | Slightly/Not at all  | Slightly/Not at all ")

# happy character factor
unique(both.waves.unique$X.Happy...ol5jdah., exclude = NULL)

both.waves.unique$vidEmotionHappyChar = NA

both.waves.unique$vidEmotionHappyChar[which(both.waves.unique$X.Happy...ol5jdah. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionHappyChar[which(both.waves.unique$X.Happy...ol5jdah. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionHappyChar[which(both.waves.unique$X.Happy...ol5jdah. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionHappyChar[which(both.waves.unique$X.Happy...ol5jdah. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionHappyChar[which(both.waves.unique$X.Happy...ol5jdah. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionHappyChar, exclude = NULL)


# happy numeric factor
both.waves.unique$vidEmotionHappyNum = NA

both.waves.unique$vidEmotionHappyNum[which(both.waves.unique$X.Happy...ol5jdah. %in% aLittle)] = 2
both.waves.unique$vidEmotionHappyNum[which(both.waves.unique$X.Happy...ol5jdah. %in% extremely)] = 5
both.waves.unique$vidEmotionHappyNum[which(both.waves.unique$X.Happy...ol5jdah. %in% moderately)] = 3
both.waves.unique$vidEmotionHappyNum[which(both.waves.unique$X.Happy...ol5jdah. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionHappyNum[which(both.waves.unique$X.Happy...ol5jdah. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionHappyNum, exclude = NULL)


# Sad character factor
unique(both.waves.unique$X.Sad...otwren4., exclude = NULL)

both.waves.unique$vidEmotionSadChar = NA

both.waves.unique$vidEmotionSadChar[which(both.waves.unique$X.Sad...otwren4. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionSadChar[which(both.waves.unique$X.Sad...otwren4. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionSadChar[which(both.waves.unique$X.Sad...otwren4. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionSadChar[which(both.waves.unique$X.Sad...otwren4. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionSadChar[which(both.waves.unique$X.Sad...otwren4. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionSadChar, exclude = NULL)


# Sad numeric factor
both.waves.unique$vidEmotionSadNum = NA

both.waves.unique$vidEmotionSadNum[which(both.waves.unique$X.Sad...otwren4. %in% aLittle)] = 2
both.waves.unique$vidEmotionSadNum[which(both.waves.unique$X.Sad...otwren4. %in% extremely)] = 5
both.waves.unique$vidEmotionSadNum[which(both.waves.unique$X.Sad...otwren4. %in% moderately)] = 3
both.waves.unique$vidEmotionSadNum[which(both.waves.unique$X.Sad...otwren4. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionSadNum[which(both.waves.unique$X.Sad...otwren4. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionSadNum, exclude = NULL)


# Interested character factor
unique(both.waves.unique$X.Interested...zfy5wxt., exclude = NULL)

both.waves.unique$vidEmotionInterestedChar = NA

both.waves.unique$vidEmotionInterestedChar[which(both.waves.unique$X.Interested...zfy5wxt. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionInterestedChar[which(both.waves.unique$X.Interested...zfy5wxt. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionInterestedChar[which(both.waves.unique$X.Interested...zfy5wxt. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionInterestedChar[which(both.waves.unique$X.Interested...zfy5wxt. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionInterestedChar[which(both.waves.unique$X.Interested...zfy5wxt. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionInterestedChar, exclude = NULL)


# Interested numeric factor
both.waves.unique$vidEmotionInterestedNum = NA

both.waves.unique$vidEmotionInterestedNum[which(both.waves.unique$X.Interested...zfy5wxt. %in% aLittle)] = 2
both.waves.unique$vidEmotionInterestedNum[which(both.waves.unique$X.Interested...zfy5wxt. %in% extremely)] = 5
both.waves.unique$vidEmotionInterestedNum[which(both.waves.unique$X.Interested...zfy5wxt. %in% moderately)] = 3
both.waves.unique$vidEmotionInterestedNum[which(both.waves.unique$X.Interested...zfy5wxt. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionInterestedNum[which(both.waves.unique$X.Interested...zfy5wxt. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionInterestedNum, exclude = NULL)


# Inspired character factor
unique(both.waves.unique$X.Inspired...qa9poaq., exclude = NULL)

both.waves.unique$vidEmotionInspiredChar = NA

both.waves.unique$vidEmotionInspiredChar[which(both.waves.unique$X.Inspired...qa9poaq. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionInspiredChar[which(both.waves.unique$X.Inspired...qa9poaq. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionInspiredChar[which(both.waves.unique$X.Inspired...qa9poaq. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionInspiredChar[which(both.waves.unique$X.Inspired...qa9poaq. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionInspiredChar[which(both.waves.unique$X.Inspired...qa9poaq. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionInspiredChar, exclude = NULL)


# Inspired numeric factor
both.waves.unique$vidEmotionInspiredNum = NA

both.waves.unique$vidEmotionInspiredNum[which(both.waves.unique$X.Inspired...qa9poaq. %in% aLittle)] = 2
both.waves.unique$vidEmotionInspiredNum[which(both.waves.unique$X.Inspired...qa9poaq. %in% extremely)] = 5
both.waves.unique$vidEmotionInspiredNum[which(both.waves.unique$X.Inspired...qa9poaq. %in% moderately)] = 3
both.waves.unique$vidEmotionInspiredNum[which(both.waves.unique$X.Inspired...qa9poaq. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionInspiredNum[which(both.waves.unique$X.Inspired...qa9poaq. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionInspiredNum, exclude = NULL)


# Angry character factor
unique(both.waves.unique$X.Angry...zsqjc9d., exclude = NULL)

both.waves.unique$vidEmotionAngryChar = NA

both.waves.unique$vidEmotionAngryChar[which(both.waves.unique$X.Angry...zsqjc9d. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionAngryChar[which(both.waves.unique$X.Angry...zsqjc9d. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionAngryChar[which(both.waves.unique$X.Angry...zsqjc9d. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionAngryChar[which(both.waves.unique$X.Angry...zsqjc9d. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionAngryChar[which(both.waves.unique$X.Angry...zsqjc9d. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionAngryChar, exclude = NULL)


# Angry numeric factor
both.waves.unique$vidEmotionAngryNum = NA

both.waves.unique$vidEmotionAngryNum[which(both.waves.unique$X.Angry...zsqjc9d. %in% aLittle)] = 2
both.waves.unique$vidEmotionAngryNum[which(both.waves.unique$X.Angry...zsqjc9d. %in% extremely)] = 5
both.waves.unique$vidEmotionAngryNum[which(both.waves.unique$X.Angry...zsqjc9d. %in% moderately)] = 3
both.waves.unique$vidEmotionAngryNum[which(both.waves.unique$X.Angry...zsqjc9d. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionAngryNum[which(both.waves.unique$X.Angry...zsqjc9d. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionAngryNum, exclude = NULL)


# Guilty character factor
unique(both.waves.unique$X.Guilty...fnd0qef., exclude = NULL)

both.waves.unique$vidEmotionGuiltyChar = NA

both.waves.unique$vidEmotionGuiltyChar[which(both.waves.unique$X.Guilty...fnd0qef. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionGuiltyChar[which(both.waves.unique$X.Guilty...fnd0qef. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionGuiltyChar[which(both.waves.unique$X.Guilty...fnd0qef. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionGuiltyChar[which(both.waves.unique$X.Guilty...fnd0qef. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionGuiltyChar[which(both.waves.unique$X.Guilty...fnd0qef. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionGuiltyChar, exclude = NULL)


# Guilty numeric factor
both.waves.unique$vidEmotionGuiltyNum = NA

both.waves.unique$vidEmotionGuiltyNum[which(both.waves.unique$X.Guilty...fnd0qef. %in% aLittle)] = 2
both.waves.unique$vidEmotionGuiltyNum[which(both.waves.unique$X.Guilty...fnd0qef. %in% extremely)] = 5
both.waves.unique$vidEmotionGuiltyNum[which(both.waves.unique$X.Guilty...fnd0qef. %in% moderately)] = 3
both.waves.unique$vidEmotionGuiltyNum[which(both.waves.unique$X.Guilty...fnd0qef. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionGuiltyNum[which(both.waves.unique$X.Guilty...fnd0qef. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionGuiltyNum, exclude = NULL)


# Disgusted character factor
unique(both.waves.unique$X.Disgusted...rkxxu40., exclude = NULL)

both.waves.unique$vidEmotionDisgustedChar = NA

both.waves.unique$vidEmotionDisgustedChar[which(both.waves.unique$X.Disgusted...rkxxu40. %in% aLittle)] = "aLittle"
both.waves.unique$vidEmotionDisgustedChar[which(both.waves.unique$X.Disgusted...rkxxu40. %in% extremely)] = "extremely"
both.waves.unique$vidEmotionDisgustedChar[which(both.waves.unique$X.Disgusted...rkxxu40. %in% moderately)] = "moderately"
both.waves.unique$vidEmotionDisgustedChar[which(both.waves.unique$X.Disgusted...rkxxu40. %in% quiteAbit)] = "quiteAbit"
both.waves.unique$vidEmotionDisgustedChar[which(both.waves.unique$X.Disgusted...rkxxu40. %in% slightlyNotAtAll)] = "slightlyNotAtAll"

table(both.waves.unique$vidEmotionDisgustedChar, exclude = NULL)


# Disgusted numeric factor
both.waves.unique$vidEmotionDisgustedNum = NA

both.waves.unique$vidEmotionDisgustedNum[which(both.waves.unique$X.Disgusted...rkxxu40. %in% aLittle)] = 2
both.waves.unique$vidEmotionDisgustedNum[which(both.waves.unique$X.Disgusted...rkxxu40. %in% extremely)] = 5
both.waves.unique$vidEmotionDisgustedNum[which(both.waves.unique$X.Disgusted...rkxxu40. %in% moderately)] = 3
both.waves.unique$vidEmotionDisgustedNum[which(both.waves.unique$X.Disgusted...rkxxu40. %in% quiteAbit)] = 4
both.waves.unique$vidEmotionDisgustedNum[which(both.waves.unique$X.Disgusted...rkxxu40. %in% slightlyNotAtAll)] = 1

table(both.waves.unique$vidEmotionDisgustedNum, exclude = NULL)



# GENDER
# -----------------------------

table(both.waves.unique$What.is.your.gender...g2bio4y., exclude=FALSE)

f = c( "Female", "Female | Female", "Female | Female | Female")
m = c("Male", "Male | Male")
other = c("Other")

both.waves.unique$female = NA

both.waves.unique$female[which(both.waves.unique$What.is.your.gender...g2bio4y. %in% f)] = 1
both.waves.unique$female[which(both.waves.unique$What.is.your.gender...g2bio4y. %in% m)] = 0
both.waves.unique$female[which(both.waves.unique$What.is.your.gender...g2bio4y. %in% other)] = NA

table(both.waves.unique$female, exclude=FALSE)

unique(both.waves.unique$female)


# AGE CONTINUOUS
# -----------------------------

table(both.waves.unique$What.is.your.age...aoeq8vn., exclude = NULL)
unique(both.waves.unique$What.is.your.age...aoeq8vn., exclude = NULL)

# replaces the "2" with NA
both.waves.unique$What.is.your.age...aoeq8vn.[both.waves.unique$What.is.your.age...aoeq8vn.=="2"] = NA
both.waves.unique$What.is.your.age...aoeq8vn.[both.waves.unique$What.is.your.age...aoeq8vn.=="20-"] = NA
#both.waves.unique[both.waves.unique$What.is.your.age...aoeq8vn.<=18,]

# creates continuous, numeric age var
both.waves.unique$ageCont = as.numeric(both.waves.unique$What.is.your.age...aoeq8vn.)

table(both.waves.unique$ageCont, exclude=NULL)

# AGE GROUPS (segmented groups so that they were roughly equal)
# -----------------------------

both.waves.unique$ageGroup = NA

both.waves.unique$ageGroup[both.waves.unique$ageCont<=21] = "21orYounger"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=22 & both.waves.unique$What.is.your.age...aoeq8vn.<=25] = "22to25"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=26 & both.waves.unique$What.is.your.age...aoeq8vn.<=30] = "26to30"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=31 & both.waves.unique$What.is.your.age...aoeq8vn.<=35] = "31to35"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=36 & both.waves.unique$What.is.your.age...aoeq8vn.<=40] = "36to40"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=41 & both.waves.unique$What.is.your.age...aoeq8vn.<=45] = "41to45"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=46 & both.waves.unique$What.is.your.age...aoeq8vn.<=50] = "46to50"
both.waves.unique$ageGroup[both.waves.unique$ageCont>=51] = "51orOlder"

table(both.waves.unique$ageGroup, exclude=NULL)


# MEAT CONSUMPTION
# -----------------------------

table(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2., exclude = NULL)
unique(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2., na.rm=FALSE)

maintain = c("Maintain current levels ", "Maintain current levels  | Maintain current levels ", 
             "Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels  | Maintain current levels " )
eliminate = c("Completely eliminate ")
greatlyIncrease = c("Greatly increase ")
somewhatDecrease = c("Somewhat decrease ", "Somewhat decrease  | Somewhat decrease ")
somewhatIncrease = c("Somewhat increase ", "Somewhat increase  | Somewhat increase ")
greatlyDecrease = c("Greatly decrease ","Greatly decrease  | Greatly decrease ")


# creates meat consumption character factor var
both.waves.unique$meatConsumpChar = NA

both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = "maintain"
both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = "eliminate"
both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = "greatlyIncrease"
both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = "somewhatDecrease"
both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = "somewhatIncrease"
both.waves.unique$meatConsumpChar[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = "greatlyDecrease"

table(both.waves.unique$meatConsumpChar, exclude=NULL)


# creates meat consumption numeric factor var
both.waves.unique$meatConsumpNum = NA

both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = 4
both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = 1
both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = 6
both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = 3
both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = 5
both.waves.unique$meatConsumpNum[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = 2

table(both.waves.unique$meatConsumpNum, exclude=NULL)

# creates meat consumption binary var - reduce meat

both.waves.unique$meatConsumpReduce = NA

both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = 0
both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = 1
both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = 0
both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = 1
both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = 0
both.waves.unique$meatConsumpReduce[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = 1

table(both.waves.unique$meatConsumpReduce, exclude=NULL)

# creates meat consumption binary var - increase meat

both.waves.unique$meatConsumpIncrease = NA

both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = 0
both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = 0
both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = 1
both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = 0
both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = 1
both.waves.unique$meatConsumpIncrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = 0

table(both.waves.unique$meatConsumpIncrease, exclude=NULL)

# creates meat consumption binary var - eliminate 

both.waves.unique$meatConsumpEliminate = NA

both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = 0
both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = 1
both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = 0
both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = 0
both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = 0
both.waves.unique$meatConsumpEliminate[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = 0

table(both.waves.unique$meatConsumpEliminate, exclude=NULL)

# creates meat consumption binary var - eliminate and greatly decrease

both.waves.unique$meatConsumpEliminateGreatlyDecrease = NA

both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% maintain)] = 0
both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% eliminate)] = 1
both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyIncrease)] = 0
both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatDecrease)] = 0
both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% somewhatIncrease)] = 0
both.waves.unique$meatConsumpEliminateGreatlyDecrease[which(both.waves.unique$X.I.intend.to.____________.my.meat.intake.over.the.next.month....va4kyn2. %in% greatlyDecrease)] = 1

table(both.waves.unique$meatConsumpEliminateGreatlyDecrease, exclude=NULL)


# ATTITUDES
# -----------------------------

agree = c("Agree ", "Agree  | Agree ", "Agree  | Agree  | Agree  | Agree  | Agree  | Agree  | Agree  | Agree  | Agree  | Agree  | Agree ")
disagree = c("Disagree ",  "Disagree  | Disagree ")
neutral = c("Neutral ", "Neutral  | Neutral ")
stronglyAgree = c("Strongly Agree ", "Strongly Agree  | Strongly Agree ")
stronglyDisagree = c("Strongly Disagree ", "Strongly Disagree  | Strongly Disagree ")


# attitude 1 character factor
table(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1., exclude = NULL)
unique(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1., na.rm=FALSE)

both.waves.unique$attitude1Char = NA

both.waves.unique$attitude1Char[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% agree)] = "agree"
both.waves.unique$attitude1Char[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% disagree)] = "disagree"
both.waves.unique$attitude1Char[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% neutral)] = "neutral"
both.waves.unique$attitude1Char[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude1Char[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude1Char, exclude=NULL)

# attitude 1 numeric factor
both.waves.unique$attitude1Num = NA

both.waves.unique$attitude1Num[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% agree)] = 4
both.waves.unique$attitude1Num[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% disagree)] = 2
both.waves.unique$attitude1Num[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% neutral)] = 3
both.waves.unique$attitude1Num[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyAgree)] = 5
both.waves.unique$attitude1Num[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude1Num, exclude=NULL)

# attitude 1 binary
both.waves.unique$attitude1Binary= NA

both.waves.unique$attitude1Binary[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% agree)] = 1
both.waves.unique$attitude1Binary[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% disagree)] = 0
both.waves.unique$attitude1Binary[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% neutral)] = 0
both.waves.unique$attitude1Binary[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyAgree)] = 1
both.waves.unique$attitude1Binary[which(both.waves.unique$Pigs..cows..and.chickens.are.intelligent.and.smart..just.like.dogs.and.cats...h59toj1. %in% stronglyDisagree)] = 0

table(both.waves.unique$attitude1Binary, exclude=NULL)

# ------

# attitude 2 character factor
table(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr., exclude = NULL)
unique(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr., na.rm=FALSE)

both.waves.unique$attitude2Char = NA

both.waves.unique$attitude2Char[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% agree)] = "agree"
both.waves.unique$attitude2Char[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% disagree)] = "disagree"
both.waves.unique$attitude2Char[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% neutral)] = "neutral"
both.waves.unique$attitude2Char[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude2Char[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude2Char, exclude=NULL)

# attitude 2 numeric factor
both.waves.unique$attitude2Num = NA

both.waves.unique$attitude2Num[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% agree)] = 4
both.waves.unique$attitude2Num[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% disagree)] = 2
both.waves.unique$attitude2Num[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% neutral)] = 3
both.waves.unique$attitude2Num[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyAgree)] = 5
both.waves.unique$attitude2Num[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude2Num, exclude=NULL)

# attitude 2 binary
both.waves.unique$attitude2Binary= NA

both.waves.unique$attitude2Binary[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% agree)] = 1
both.waves.unique$attitude2Binary[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% disagree)] = 0
both.waves.unique$attitude2Binary[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% neutral)] = 0
both.waves.unique$attitude2Binary[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyAgree)] = 1
both.waves.unique$attitude2Binary[which(both.waves.unique$The.food.that.I.eat.contributes.to.animal.suffering..6ubtalr. %in% stronglyDisagree)] = 0

table(both.waves.unique$attitude2Binary, exclude=NULL)

# ------

# attitude 3 character factor
table(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i., exclude = NULL)
unique(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i., na.rm=FALSE)

both.waves.unique$attitude3Char = NA

both.waves.unique$attitude3Char[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% agree)] = "agree"
both.waves.unique$attitude3Char[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% disagree)] = "disagree"
both.waves.unique$attitude3Char[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% neutral)] = "neutral"
both.waves.unique$attitude3Char[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude3Char[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude3Char, exclude=NULL)

# attitude 3 numeric factor
both.waves.unique$attitude3Num = NA

both.waves.unique$attitude3Num[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% agree)] = 4
both.waves.unique$attitude3Num[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% disagree)] = 2
both.waves.unique$attitude3Num[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% neutral)] = 3
both.waves.unique$attitude3Num[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyAgree)] = 5
both.waves.unique$attitude3Num[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude3Num, exclude=NULL)

# attitude 3 binary
both.waves.unique$attitude3Binary= NA

both.waves.unique$attitude3Binary[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% agree)] = 1
both.waves.unique$attitude3Binary[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% disagree)] = 0
both.waves.unique$attitude3Binary[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% neutral)] = 0
both.waves.unique$attitude3Binary[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyAgree)] = 1
both.waves.unique$attitude3Binary[which(both.waves.unique$Meals.without.red.meat.or.chicken.are.delicious...pwl9u6i. %in% stronglyDisagree)] = 0

table(both.waves.unique$attitude3Binary, exclude=NULL)

# ------

# attitude 4 character factor
table(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1., exclude = NULL)
unique(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1., na.rm=FALSE)

both.waves.unique$attitude4Char = NA

both.waves.unique$attitude4Char[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% agree)] = "agree"
both.waves.unique$attitude4Char[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% disagree)] = "disagree"
both.waves.unique$attitude4Char[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% neutral)] = "neutral"
both.waves.unique$attitude4Char[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude4Char[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude4Char, exclude=NULL)

# attitude 4 numeric factor
both.waves.unique$attitude4Num = NA

both.waves.unique$attitude4Num[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% agree)] = 4
both.waves.unique$attitude4Num[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% disagree)] = 2
both.waves.unique$attitude4Num[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% neutral)] = 3
both.waves.unique$attitude4Num[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyAgree)] = 5
both.waves.unique$attitude4Num[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude4Num, exclude=NULL)

# attitude 4 binary
both.waves.unique$attitude4Binary= NA

both.waves.unique$attitude4Binary[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% agree)] = 1
both.waves.unique$attitude4Binary[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% disagree)] = 0
both.waves.unique$attitude4Binary[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% neutral)] = 0
both.waves.unique$attitude4Binary[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyAgree)] = 1
both.waves.unique$attitude4Binary[which(both.waves.unique$Eating.meals.without.red.meat.or.chicken.is.easy...26tfuc1. %in% stronglyDisagree)] = 0

table(both.waves.unique$attitude4Binary, exclude=NULL)

# ------

# attitude 5 character factor
table(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba., exclude = NULL)
unique(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba., na.rm=FALSE)

both.waves.unique$attitude5Char = NA

both.waves.unique$attitude5Char[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% agree)] = "agree"
both.waves.unique$attitude5Char[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% disagree)] = "disagree"
both.waves.unique$attitude5Char[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% neutral)] = "neutral"
both.waves.unique$attitude5Char[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude5Char[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude5Char, exclude=NULL)

# attitude 5 numeric factor
both.waves.unique$attitude5Num = NA

both.waves.unique$attitude5Num[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% agree)] = 4
both.waves.unique$attitude5Num[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% disagree)] = 2
both.waves.unique$attitude5Num[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% neutral)] = 3
both.waves.unique$attitude5Num[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyAgree)] = 5
both.waves.unique$attitude5Num[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude5Num, exclude=NULL)

# attitude 5 binary
both.waves.unique$attitude5Binary= NA

both.waves.unique$attitude5Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% agree)] = 1
both.waves.unique$attitude5Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% disagree)] = 0
both.waves.unique$attitude5Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% neutral)] = 0
both.waves.unique$attitude5Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyAgree)] = 1
both.waves.unique$attitude5Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.rich.emotional.lives..just.like.dogs.and.cats...yrvi4ba. %in% stronglyDisagree)] = 0

table(both.waves.unique$attitude5Binary, exclude=NULL)

# ------

# attitude 6 character factor
table(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4., exclude = NULL)
unique(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4., na.rm=FALSE)

both.waves.unique$attitude6Char = NA

both.waves.unique$attitude6Char[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% agree)] = "agree"
both.waves.unique$attitude6Char[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% disagree)] = "disagree"
both.waves.unique$attitude6Char[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% neutral)] = "neutral"
both.waves.unique$attitude6Char[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyAgree)] = "stronglyAgree"
both.waves.unique$attitude6Char[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyDisagree)] = "stronglyDisagree"

table(both.waves.unique$attitude6Char, exclude=NULL)

# attitude 6 numeric factor
both.waves.unique$attitude6Num = NA

both.waves.unique$attitude6Num[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% agree)] = 4
both.waves.unique$attitude6Num[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% disagree)] = 2
both.waves.unique$attitude6Num[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% neutral)] = 3
both.waves.unique$attitude6Num[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyAgree)] = 5
both.waves.unique$attitude6Num[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyDisagree)] = 1

table(both.waves.unique$attitude6Num, exclude=NULL)

# attitude 6 binary
both.waves.unique$attitude6Binary= NA

both.waves.unique$attitude6Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% agree)] = 1
both.waves.unique$attitude6Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% disagree)] = 0
both.waves.unique$attitude6Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% neutral)] = 0
both.waves.unique$attitude6Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyAgree)] = 1
both.waves.unique$attitude6Binary[which(both.waves.unique$Pigs..cows..and.chickens.have.the.ability.to.suffer.and.feel.pain...h8i94x4. %in% stronglyDisagree)] =0

table(both.waves.unique$attitude6Binary, exclude=NULL)


# VSG REQUEST
# -----------------------------

#table(both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3., exclude=NULL)

both.waves.unique$vsgBinary = NA

# yes to vsg = 1
both.waves.unique$vsgBinary[grep('@', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 1
both.waves.unique$vsgBinary[grep('^Yes', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 1

#no to vsg = 0
both.waves.unique$vsgBinary[grep('^no', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 0
both.waves.unique$vsgBinary[grep('^No', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 0
both.waves.unique$vsgBinary[grep('^NO', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 0
both.waves.unique$vsgBinary[grep('^hell', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 0
both.waves.unique$vsgBinary[grep('^"no"', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 0

# yes to vsg, specific cases
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="noeljolly13@yahoo.com"] = 1
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="gtajoe81"] = 1
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="A1QP2WFF00OKVZ"] = 1
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="laurenm007"] = 1
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="cjsaltz7"] = 1
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="nosamtthc"] = 1


# no to vsg, specific cases
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="np"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="NoNoNoNoNo@Nope.com"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="n/a"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="Mo"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="I've got this!"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="o"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="Not sure"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="Nope!"] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="Cats and dogs are not separate from pigs and cows. In fact I like cows far more than stupid cats and annoying dogs."] = 0
both.waves.unique$vsgBinary[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="\"no\"."] = 0


table(both.waves.unique$vsgBinary, exclude=NULL)


# VEGETARIAN/PESCATARIAN/REDUCERS
# self-reported vegetarians in the vsg request open-ended response
# -----------------------------

both.waves.unique$vegetarianSelfReport = NA

# assigns people who indiicate that the were veg or pesc at 1
both.waves.unique$vegetarianSelfReport[grep('veg', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 1
both.waves.unique$vegetarianSelfReport[grep('pescatarian', both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.)] = 1
both.waves.unique$vegetarianSelfReport[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="No. Our relatively non-existent consumption comes through barter with our neighbors. We all care deeply."] = 1


# replaces someone who said they would be veg one day to an NA
both.waves.unique$vegetarianSelfReport[both.waves.unique$Would.you.like.us.to.send.you.a._free.40.page.Vegetarian.Starter.Guide_..If.so..please.enter.your.email.below..If.not..please.type..no....mjvvry3.=="no, but thanks for the offer and I may one day become vegetarian, just not now"] = NA


table(both.waves.unique$vegetarianSelfReport, exclude=NULL)

# VIDEO VARIABLE

unique(both.waves.unique$Experiment..videos.)

both.waves.unique$video = NA
both.waves.unique$video[grep('^benny', both.waves.unique$Experiment..videos.)] = "benny"
both.waves.unique$video[grep('^choiceYours', both.waves.unique$Experiment..videos.)] = "choiceYours"
both.waves.unique$video[grep('^babyAnimals', both.waves.unique$Experiment..videos.)] = "babyAnimals"
both.waves.unique$video[grep('^sleepyAnimals', both.waves.unique$Experiment..videos.)] = "sleepyAnimals"
both.waves.unique$video[grep('^control', both.waves.unique$Experiment..videos.)] = "control"
both.waves.unique$video[grep('^happyFree', both.waves.unique$Experiment..videos.)] = "happyFree"
both.waves.unique$video[grep('^loseAppetite', both.waves.unique$Experiment..videos.)] = "loseAppetite"
both.waves.unique$video[grep('^stuffEat', both.waves.unique$Experiment..videos.)] = "stuffEat"
both.waves.unique$video[grep('^weSame', both.waves.unique$Experiment..videos.)] = "weSame"
both.waves.unique$video[grep('^loveDogsPigs', both.waves.unique$Experiment..videos.)] = "loveDogsPigs"
both.waves.unique$video[grep('^henHell', both.waves.unique$Experiment..videos.)] = "henHell"
both.waves.unique$video[grep('^veganSwap', both.waves.unique$Experiment..videos.)] = "veganSwap"
both.waves.unique$video[grep('^kitchen', both.waves.unique$Experiment..videos.)] = "kitchen"



# ------------------------------------------------------------------------------------------------------------
# ----- CREATES DATAFRAME WITH CLEANED VARIABLES ------
# ------------------------------------------------------------------------------------------------------------

#colnames(both.waves.unique)

# variables for cleaned data set
cleanVars = c("MTurk.ID..a7i4jfe.", "Experiment..videos.", "study.x", "videoTreatment", "ctaTreatment", "video",
          "vidEmotionHappyChar","vidEmotionHappyNum","vidEmotionSadChar","vidEmotionSadNum",
          "vidEmotionInterestedChar", "vidEmotionInterestedNum" ,"vidEmotionInspiredChar",  
          "vidEmotionInspiredNum", "vidEmotionAngryChar", "vidEmotionAngryNum", "vidEmotionGuiltyChar",
          "vidEmotionGuiltyNum", "vidEmotionDisgustedChar", "vidEmotionDisgustedNum", "female", "ageCont",
          "ageGroup", "meatConsumpChar", "meatConsumpNum", "meatConsumpReduce", "meatConsumpIncrease",
          "meatConsumpEliminate", "meatConsumpEliminateGreatlyDecrease", "attitude1Char", "attitude1Num",
          "attitude1Binary", "attitude2Char", "attitude2Num", "attitude2Binary", "attitude3Char", "attitude3Num",
          "attitude3Binary", "attitude4Char", "attitude4Num", "attitude4Binary", "attitude5Char", "attitude5Num", 
          "attitude5Binary", "attitude6Char", "attitude6Num", "attitude6Binary", "vsgBinary", "vegetarianSelfReport")

# colnames
colNames = c("mturkID", "experimentGroup", "study", "videoTreatment", "ctaTreatment", "video",
              "vidEmotionHappyChar","vidEmotionHappyNum","vidEmotionSadChar","vidEmotionSadNum",
              "vidEmotionInterestedChar", "vidEmotionInterestedNum" ,"vidEmotionInspiredChar",  
              "vidEmotionInspiredNum", "vidEmotionAngryChar", "vidEmotionAngryNum", "vidEmotionGuiltyChar",
              "vidEmotionGuiltyNum", "vidEmotionDisgustedChar", "vidEmotionDisgustedNum", "female", "ageCont",
              "ageGroup", "meatConsumpChar", "meatConsumpNum", "meatConsumpReduce", "meatConsumpIncrease",
             "meatConsumpEliminate", "meatConsumpEliminateGreatlyDecrease", "attitude1Char", "attitude1Num",
              "attitude1Binary", "attitude2Char", "attitude2Num", "attitude2Binary", "attitude3Char", "attitude3Num",
              "attitude3Binary", "attitude4Char", "attitude4Num", "attitude4Binary", "attitude5Char", "attitude5Num", 
             "attitude5Binary", "attitude6Char", "attitude6Num", "attitude6Binary", "vsgBinary", "vegetarianSelfReport")


# cleaned data frame
dC = both.waves.unique[,cleanVars]
# gives dC new names
colnames(dC) = colNames


# ------------------------------------------------------------------------------------------------------------
# ----- ADDING STANDARDIZED OUTCOMES TO CLEANED DATAFRAME  ------
# ------------------------------------------------------------------------------------------------------------

colnames(dC)

dC$attitude1Num.std <- apply(dC[31], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$attitude2Num.std <- apply(dC[34], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$attitude3Num.std <- apply(dC[37], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$attitude4Num.std <- apply(dC[40], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$attitude5Num.std <- apply(dC[43], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$attitude6Num.std <- apply(dC[46], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$vsgBinary.std <- apply(dC[48], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
dC$meatConsumpReduce.std <- apply(dC[26], MARGIN=2, FUN=function(x) (x - mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))


# ------------------------------------------------------------------------------------------------------------
# ----- ADDING COMBINED ATTITUDES TO CLEANED DATAFRAME  ------
# ------------------------------------------------------------------------------------------------------------

dC$combinedFoodAttitudes.std = rowMeans(dC[,c(51,52,53)])

dC$combinedAnimalAttitudes.std = rowMeans(dC[,c(50,54,55)])

dC$combinedAllAttitudes.std = rowMeans(dC[,c(50,51,52,53,54,55)])


# ------------------------------------------------------------------------------------------------------------
# ----- ADDING COMBINED OUTCOMES TO CLEANED DATAFRAME  ------
# ------------------------------------------------------------------------------------------------------------
colnames(dC)

dC$combinedOutcome1.std = rowMeans(dC[,c(50,51,52,53,54,55,56,57)])

dC$combinedOutcome2.std = rowMeans(dC[,c(56,57,58,59)])

dC$combinedOutcome3.std = rowMeans(dC[,c(56,57,60)])


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# 			  	EXPORTS THE FINAL DATAFRAME FOR ANALYSIS  			 #
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

write.csv(dC, "cleanVideoDataForAnalysis.csv", row.names=FALSE)








