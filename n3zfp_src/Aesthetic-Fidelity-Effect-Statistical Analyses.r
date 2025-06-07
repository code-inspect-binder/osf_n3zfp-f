################################################################################################################################
#R-code for all analyses reported in the following paper:
#"The Aesthetic Fidelity Effect" by Annika Wiecek, Daniel Wentzel, & Jan R. Landwehr
#International Journal of Research in Marketing
#
#Please note: The datasets for Studies 1A-1C contain confidential information we are not allowed to share publicly.
#Please contact the corresponding author if you would like us to run additional analyses on these datasets.
################################################################################################################################



################################################################################################################################
#Setting R
library(readxl)
library(psych)
library(car)

setwd("C:/Users/jlandweh/DATEN/Publications/Product Design lock in - RWTH Aachen/Finale Auswertung/")

#Function to z-standardize within groups
z_std <- function(variable, gruppe){
    newname <- variable
    for(i in 1:length(levels(gruppe))){
      mittel <- mean(variable[gruppe == levels(gruppe)[i]], na.rm=T)
      standard <- sd(variable[gruppe == levels(gruppe)[i]], na.rm=T)
      newname[gruppe == levels(gruppe)[i]] <- (variable[gruppe == levels(gruppe)[i]]-mittel)/standard
    }
    return(newname)
}


################################################################################################################################
###Study 1A) Cars

#Load data
R <- as.data.frame(read_excel("1a-Datensatz Autos.xlsx",sheet=1))
M <- as.data.frame(read_excel("1a-Stern-Marken-12.xlsx",sheet=1))
names(R) <- tolower(names(R))
R1 <- subset(R, R$angebote>=10 & R$km!=0)
R2 <- merge(R1, M)

summary(R2)

#z-standardization within each segment
R2$z_km <- z_std(R2$km, as.factor(R2$kategorie))
R2$z_zustimmung <- z_std(R2$zustimmung, as.factor(R2$kategorie))
R2$z_ps <- z_std(R2$ps, as.factor(R2$kategorie))
R2$z_beschleunigung <- z_std(R2$beschleunigung, as.factor(R2$kategorie))
R2$z_verbrauch <- z_std(R2$verbrauch, as.factor(R2$kategorie))
R2$z_preis <- z_std(R2$preis, as.factor(R2$kategorie))
R2$z_bekanntheit <- z_std(R2$bekanntheit, as.factor(R2$kategorie))

#OLS-Regression models
summary(lm(z_km ~ z_zustimmung, data=R2))
summary(lm(z_km ~ z_ps + z_beschleunigung + z_verbrauch + z_preis + z_bekanntheit, data=R2))
summary(lm(z_km ~ z_zustimmung + z_ps + z_beschleunigung + z_verbrauch + z_preis + z_bekanntheit, data=R2))

round(coefficients(lm(z_km ~ z_zustimmung, data=R2)), 2)
round(coefficients(lm(z_km ~ z_ps + z_beschleunigung + z_verbrauch + z_preis + z_bekanntheit, data=R2)), 2)
round(coefficients(lm(z_km ~ z_zustimmung + z_ps + z_beschleunigung + z_verbrauch + z_preis + z_bekanntheit, data=R2)), 2)


################################################################################################################################
###Study 1B) Maternity Wear

K <- as.data.frame(read_excel("1b-maternity.xlsx",sheet=1))
names(K) <- tolower(names(K))
summary(K)

#ICC: We use ICC(2,k) because our judges were a random sample of possible judges and we intend to aggregate the scores
ICC(K[,2:4])

#Recoding of the aesthetics ratings such that higher values indicate higher levels of aesthetic evaluation
K$aesthetics <- (K$ästhetik-8)*(-1)

#z-standardization within each segment
K$z_mietzeit <- z_std(K$mietdauer, as.factor(K$kategorie))
K$z_aesthetics <- z_std(K$aesthetics, as.factor(K$kategorie))
K$z_price <- z_std(K$preis, as.factor(K$kategorie))

#OLS-Regressions and ANOVA for the dummy-coded brand variable
mod.1b.1 <- lm(z_mietzeit ~ z_aesthetics, data=K)
mod.1b.2 <- lm(z_mietzeit ~ z_price + as.factor(marke), data=K)
mod.1b.3 <- lm(z_mietzeit ~ z_aesthetics + z_price + as.factor(marke), data=K)

summary(mod.1b.1)
summary(mod.1b.2)
summary(mod.1b.3)

round(coefficients(mod.1b.1), 2)
round(coefficients(mod.1b.2), 2)
round(coefficients(mod.1b.3), 2)

summary(aov(mod.1b.2))
summary(aov(mod.1b.3))


################################################################################################################################
###Study 1C) Children Clothes

RS <- as.data.frame(read_excel("1c-children.xlsx",sheet=1))
names(RS) <- tolower(names(RS))
summary(RS)

#ICC: We use ICC(2,k) because our judges were a random sample of possible judges and we intend to aggregate the scores
ICC(RS[,2:4])

#Recoding of the aesthetics ratings such that higher values indicate higher levels of aesthetic evaluation
RS$aesthetics <- (RS$ästhetik-8)*(-1)

#z-standardization within each segment
RS$z_mietzeit <- z_std(RS$mietzeit, as.factor(RS$kategorie))
RS$z_aesthetics <- z_std(RS$aesthetics, as.factor(RS$kategorie))
RS$z_price <- z_std(RS$preis, as.factor(RS$kategorie))

#OLS-Regressions and ANOVA for the dummy-coded brand variable
mod.1c.1 <- lm(z_mietzeit ~ z_aesthetics, data=RS)
mod.1c.2 <- lm(z_mietzeit ~ z_price + as.factor(marke), data=RS)
mod.1c.3 <- lm(z_mietzeit ~ z_aesthetics + z_price + as.factor(marke), data=RS)

summary(mod.1c.1)
summary(mod.1c.2)
summary(mod.1c.3)

round(coefficients(mod.1c.1), 2)
round(coefficients(mod.1c.2), 2)
round(coefficients(mod.1c.3), 2)

summary(aov(mod.1c.2))
summary(aov(mod.1c.3))


################################################################################################################################
###Study 2) Aesthetic fidelity effect

#######################################################
###Pre-Test
PT2 <- read_excel("2-Pretest Study 2.xlsx",sheet=1)
summary(PT2)

PT2$aest <- as.factor(ifelse(PT2$GroupAesthetics==1, "low", "high"))

#Cronbach's alpha
PT2aest1 <- c(PT2$AestheticsPhone1_1, PT2$AestheticsPhone2_1)
PT2aest2 <- c(PT2$AestheticsPhone1_2, PT2$AestheticsPhone2_2)
PT2aest3 <- c(PT2$AestheticsPhone1_3, PT2$AestheticsPhone2_3)
alpha(cbind(PT2aest1, PT2aest2, PT2aest3))

#Mean comparison
by(PT2$AestheticsPhone1, PT2$aest, FUN=mean)
by(PT2$AestheticsPhone2, PT2$aest, FUN=mean)
t.test(PT2$AestheticsPhone1[PT2$aest=="high"], PT2$AestheticsPhone2[PT2$aest=="high"], paired = TRUE, var.equal = TRUE)
t.test(PT2$AestheticsPhone1[PT2$aest=="low"], PT2$AestheticsPhone2[PT2$aest=="low"], paired = TRUE, var.equal = TRUE)

by(PT2$MeanAS1AS2, PT2$aest, FUN=mean)
summary(aov(MeanAS1AS2 ~ aest, data=PT2))

#######################################################
###Main Study
#Read data (Note: One participant who guessed the purpose of the study was already removed from this dataset)
Stu2 <- read_excel("2-Experiment Aesthetic Fidelity.xlsx",sheet=1)
summary(Stu2)
dim(Stu2)

#Sample descriptives
table(Stu2$Gender)[2]/nrow(Stu2)
mean(Stu2$Age)

#Recoding: not-aesthetic=-1; aesthetic=1
Stu2$aest <- ifelse(Stu2$GroupAesthetics==1, 1, -1)
#Recoding: Choosing status quo 1; choosing new smartphone =0
Stu2$choice <- ifelse(Stu2$SmartphoneChoice==0, 1, 0)
#Recoding (Speed new phone - Speed old phone) * (-1)
Stu2$speed <- Stu2$DifferenceEnteringSpeedBetweenSmartphones*(-1)

#Cronbach's alpha
Stu2aest1 <- c(Stu2$AestheticsSmartphone1_1, Stu2$AestheticsSmartphone2_1)
Stu2aest2 <- c(Stu2$AestheticsSmartphone1_2, Stu2$AestheticsSmartphone2_2)
Stu2aest3 <- c(Stu2$AestheticsSmartphone1_3, Stu2$AestheticsSmartphone2_3)
alpha(cbind(Stu2aest1, Stu2aest2, Stu2aest3))

alpha(Stu2[,4:7])

#Manipulation checks
by(Stu2$MeanAestheticsS1S2, Stu2$aest, FUN=mean)
summary(aov(MeanAestheticsS1S2 ~ aest, data=Stu2))

by(Stu2$AestheticsSmartphone1, Stu2$aest, FUN=mean)
by(Stu2$AestheticsSmartphone2, Stu2$aest, FUN=mean)
t.test(Stu2$AestheticsSmartphone1[Stu2$aest==1], Stu2$AestheticsSmartphone2[Stu2$aest==1], paired = TRUE, var.equal = TRUE)
t.test(Stu2$AestheticsSmartphone1[Stu2$aest==-1], Stu2$AestheticsSmartphone2[Stu2$aest==-1], paired = TRUE, var.equal = TRUE)

by(Stu2$UsageEnjoyment, Stu2$aest, FUN=mean)
summary(aov(UsageEnjoyment ~ aest, data=Stu2))

#ANOVA Models
by(Stu2$UsageDurationSeconds, Stu2$aest, FUN=mean)
summary(aov(UsageDurationSeconds ~ aest, data=Stu2))

by(Stu2$speed, Stu2$aest, FUN=mean)
summary(aov(speed ~ aest, data=Stu2))

#Regression Models
summary(glm(choice ~ aest, family=binomial, data=Stu2))

addmargins(table(Stu2$choice, Stu2$aest))
16/35
26/34

#Add ID
Stu2$id <- 1:69

#Number of random draws
straps_no <- 5000

#Build the bootstrapped datasets
#tempdat denotes the list of bootstrapped observation IDs that is matched with the corresponding data from the original dataset
datlist <- list()
for(i in 1:straps_no){
  tempdat <- as.data.frame(matrix(nrow=69,ncol=1))
  names(tempdat) <- c("id")
  tempdat[,1] <- sample(1:69, size=69, replace=T)
  datlist[[i]] <- merge(tempdat, Stu2)
  print(i)
}

#Estimation of the distribution of key parameters of interest
boot_para <- as.data.frame(matrix(nrow=straps_no,ncol=3))
names(boot_para) <- c("aest_usage", "usage_speed", "speed_lock")
for(i in 1:straps_no){
  boot_para[i,1] <- lm(UsageDurationSeconds ~ aest, data=datlist[[i]])$coefficients[2]
  boot_para[i,2] <- lm(speed ~ UsageDurationSeconds + aest, data=datlist[[i]])$coefficients[2]
  boot_para[i,3] <- glm(choice ~ speed + UsageDurationSeconds + aest, family=binomial, data=datlist[[i]])$coefficients[2]
  print(i)
}

boot_para$ind_effect <- boot_para$aest_usage*boot_para$usage_speed*boot_para$speed_lock

#Estimate of the indirect effect and 95%-confidence interval
mean(boot_para$ind_effect)
sort(boot_para$ind_effect)[126]
sort(boot_para$ind_effect)[4875]


################################################################################################################################
###Study 3)
Stu3 <- read_excel("3-Experiment Usage Intensity.xlsx",sheet=1)
summary(Stu3)
dim(Stu3)

#Sample descriptives
table(Stu3$Gender)[1]/nrow(Stu3)
mean(Stu3$Age)

#Recoding: not-aesthetic=-1; aesthetic=1
Stu3$aest <- ifelse(Stu3$GroupAesthetics==1, 1, -1)
#Recoding: no practice=-1; practice=1
Stu3$pract <- ifelse(Stu3$GroupPractice==1, 1, -1)
#Recoding: Choosing status quo 1; choosing the new smartphone =0
Stu3$choice <- ifelse(Stu3$SmartphoneChoice==0, 1, 0)
#Recoding (Speed new phone - Speed old phone) * (-1)
Stu3$speed <- Stu3$DifferenceEnteringSpeedBetweenSmartphones*(-1)

#Cronbach's alpha
Stu3aest1 <- c(Stu3$AestheticsSmartphone1_1, Stu3$AestheticsSmartphone2_1)
Stu3aest2 <- c(Stu3$AestheticsSmartphone1_2, Stu3$AestheticsSmartphone2_2)
Stu3aest3 <- c(Stu3$AestheticsSmartphone1_3, Stu3$AestheticsSmartphone2_3)
alpha(cbind(Stu3aest1, Stu3aest2, Stu3aest3))

#Manipulation checks
Stu3$mean_aest <- (Stu3$AestheticsSmartphone1+Stu3$AestheticsSmartphone2)/2
by(Stu3$mean_aest, Stu3$aest, FUN=mean)
summary(aov(mean_aest ~ aest, data=Stu3))

by(Stu3$AestheticsSmartphone1, Stu3$aest, FUN=mean)
by(Stu3$AestheticsSmartphone2, Stu3$aest, FUN=mean)
t.test(Stu3$AestheticsSmartphone1[Stu3$aest==1], Stu3$AestheticsSmartphone2[Stu3$aest==1], paired = TRUE, var.equal = TRUE)
t.test(Stu3$AestheticsSmartphone1[Stu3$aest==-1], Stu3$AestheticsSmartphone2[Stu3$aest==-1], paired = TRUE, var.equal = TRUE)

#ANOVA Models
Anova(aov(speed ~ aest*pract, data=Stu3), type="III")
by(Stu3$speed, Stu3$pract, FUN=mean)

###Mediation
summary(glm(choice ~ aest*pract, family=binomial, data=Stu3))

#Frequencies
addmargins(table(Stu3$choice, Stu3$pract))
42/57
30/60

#Add ID
Stu3$id <- 1:117

#Number of random draws
straps_no <- 5000

#Build the bootstrapped datasets
#tempdat denotes the list of bootstrapped observation IDs that is matched with the corresponding data from the original dataset
datlist2 <- list()
for(i in 1:straps_no){
  tempdat2 <- as.data.frame(matrix(nrow=117,ncol=1))
  names(tempdat2) <- c("id")
  tempdat2[,1] <- sample(1:117, size=117, replace=T)
  datlist2[[i]] <- merge(tempdat2, Stu3)
  print(i)
}

#Estimation of the distribution of key parameters of interest
boot_para2 <- as.data.frame(matrix(nrow=straps_no,ncol=2))
names(boot_para2) <- c("practice_speed", "speed_lockin")
for(i in 1:straps_no){
  boot_para2[i,1] <- lm(speed ~ aest*pract, data=datlist2[[i]])$coefficients[3]
  boot_para2[i,2] <- glm(choice ~ speed + aest*pract, family=binomial, data=datlist2[[i]])$coefficients[2]
  print(i)
}

boot_para2$ind_effect <- boot_para2$practice_speed*boot_para2$speed_lockin

#Estimate of the indirect effect and 95%-confidence interval
mean(boot_para2$ind_effect)
sort(boot_para2$ind_effect)[126]
sort(boot_para2$ind_effect)[4875]


################################################################################################################################
###Study 4)

############################################################################
###Pre Test
PT4 <- read_excel("4-Pretest Study 4.xlsx",sheet=1)

summary(PT4)

t.test(PT4$ÄsTürkis, PT4$ÄsDunkBl, paired = TRUE, var.equal = TRUE)


############################################################################
###Main Study
Stu4 <- read_excel("4-Experiment Durable vs Non-Durable.xlsx",sheet=1)
summary(Stu4)
dim(Stu4)

#Sample descriptives
table(Stu4$Gender)[1]/nrow(Stu4)
mean(Stu4$Age, na.rm=T)

#Recoding: not-aesthetic=-1; aesthetic=1
Stu4$aest <- ifelse(Stu4$Ä==2, 1, -1)
#Recoding: no practice=-1; practice=1
Stu4$durable <- ifelse(Stu4$D==2, 1, -1)

#Cronbach's alpha
alpha(Stu4[,5:7])
alpha(Stu4[,9:10])

#Manipulation checks
by(Stu4$Aesthetic, Stu4$aest, FUN=mean)
summary(aov(Aesthetic ~ aest, data=Stu4))

by(Stu4$Durability, Stu4$durable, FUN=mean)
summary(aov(Durability ~ durable, data=Stu4))

#ANOVA Models
Anova(aov(Writing.time ~ aest*durable, data=Stu4), type="III")
by(Stu4$Writing.time, list(Stu4$aest, Stu4$durable), FUN=mean)

####################################################################
###Adding simple effects for "Aesthetics" conditional on "Durability"

#Extracting the relevant Sum of Squares and degrees of freedom from the ANOVA models
SS_aest_durable <- Anova(aov(Writing.time ~ aest, data=subset(Stu4, Stu4$durable==1)))$"Sum Sq"[1]
Df_aest_durable <- Anova(aov(Writing.time ~ aest, data=subset(Stu4, Stu4$durable==1)))$"Df"[1]

SS_aest_nond <- Anova(aov(Writing.time ~ aest, data=subset(Stu4, Stu4$durable==-1)))$"Sum Sq"[1]
Df_aest_nond <- Anova(aov(Writing.time ~ aest, data=subset(Stu4, Stu4$durable==-1)))$"Df"[1]

SS_residual <- Anova(aov(Writing.time ~ aest*durable, data=Stu4), type="III")$"Sum Sq"[5]
Df_residual <- Anova(aov(Writing.time ~ aest*durable, data=Stu4), type="III")$"Df"[5]

#Computing Mean Sum of Squares
MS_aest_durable <- SS_aest_durable/Df_aest_durable
MS_aest_nond <- SS_aest_nond/Df_aest_nond
MS_residual <- SS_residual/Df_residual

#Computing empirical F-values
Femp_durable <- MS_aest_durable/MS_residual
Femp_nond <- MS_aest_nond/MS_residual

#Computing p-values based on the F-distribution
p_durable <- pf(q=Femp_durable, df1=Df_aest_durable, df2=Df_residual, lower.tail = F)
p_nond <- pf(q=Femp_nond, df1=Df_aest_nond, df2=Df_residual, lower.tail = F)

#Printing the simple effect of font for the business brand
Femp_durable
Df_aest_durable
Df_residual
p_durable

#Printing the simple effect of font for the leisure brand
Femp_nond
Df_aest_nond
Df_residual
p_nond
