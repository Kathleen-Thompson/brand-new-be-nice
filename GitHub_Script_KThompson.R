#Getting Started~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#install necessary packages
install.packages(lme4)
install.packages(lmerTest)
install.packages(ggplot2)

#load packages into your library
library(lme4)
library(lmerTest)
library(lsmeans)
library(ggplot2)

#set working directory (tell R where to find your files)
setwd("~/Desktop/P. deltoides Greenhouse")
AMFdata = read.csv("pdelEMFAMF_ESA2022.csv")
#above we've established that we will refer to our dataframe as "AMFdata"

#Data Clean Up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#summary command shows us what the data looks like and how R is reading/interpreting it
summary(AMFdata)

#some of these were read as continuous/numerical variables when they are actually categorical; 
#let's fix that
AMFdata$Plot.. <- as.factor(AMFdata$Plot..)
AMFdata$Seedling.. <- as.factor(AMFdata$Seedling..)
AMFdata$Soil.Moisture <- as.numeric(AMFdata$Soil.Moisture)
AMFdata$Plot.. <- as.factor(AMFdata$Plot..)
AMFdata$Seedling.. <- as.factor(AMFdata$Seedling..)
AMFdata$Planting.Cohort.. <- as.factor(AMFdata$Planting.Cohort..)
AMFdata$Intra.cohort.ID <- as.factor(AMFdata$Intra.cohort.ID)

#revisit summary; all look good?
summary(AMFdata)

#Exploring Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AMF colonization model
mod_AM = lmer(Total_AMF_Presence~Nitrogen*Light*Moisture + Width..cm. +  Highest.Order.Branching + (1|Plot..), data=AMFdata)
anova(mod_AM)

###Incorporating EM Data
#looking for trends between the two different ways EM data was collected; rough correlation
plot(AMFdata$Colperrtlngth,AMFdata$PercEM_new)

#Define our new dependent variable of interest: EM to AM Ratio
EMAMratio = AMFdata$AMF_EM_Mantles/AMFdata$Total_AMF_Presence

#Full model with all variables
mod_EMAM_Full = lmer(EMAMratio~Nitrogen*Light*Moisture + Julian_Date + Width..cm. + X..Green.Leaves + X..Open.Buds + X..Total.Roots + Highest.Order.Branching + (1|Plot..), data=AMFdata)
AIC(mod_EMAM_Full)
#AIC = 565.2627
anova(mod_EMAM_Full)

#Reduced full model using AIC
mod_EMAM = lmer(EMAMratio~Nitrogen*Light*Moisture + Width..cm. +  Highest.Order.Branching + (1|Plot..), data=AMFdata)
AIC(mod_EMAM)
#AIC = 549.76
anova(mod_EMAM)
#Nitrogen p = .00067 ; N:L p =.0025 ; (N:M p = .068)

#Individual pairwise comparison not useful due to interactions;
#avoid this section and move ahead
emmeans(mod_EMAM, list(pairwise ~ Moisture), adjust = "tukey")
emmeans(mod_EMAM, list(pairwise ~ Light), adjust = "tukey")
emmeans(mod_EMAM, list(pairwise ~ Nitrogen), adjust = "tukey")

#This considers interactions and is useful for generating pairwise p-values
emmeans(mod_EMAM, list(pairwise ~ Nitrogen|Light|Moisture), adjust = "tukey")

#Generating LS means for each group
emmeans(mod_EMAM, ~Moisture|Light|Nitrogen)
sum_means_error <- summary(emmeans(mod_EMAM, ~Moisture|Light|Nitrogen))

#Plotting Data
#reorder categorial variables
AMFdata$Moisture <- factor(AMFdata$Moisture, levels=c("Low", "Medium", "High"))
AMFdata$Nitrogen <- factor(AMFdata$Nitrogen, levels=c("Control", "NPK", "Leaves"))
AMFdata$Light <- factor(AMFdata$Light, levels=c("Low", "Medium", "High"))

#Plot
ggplot(data=sum_means_error, aes(Moisture, emmean)) + ylab("EM/AM Ratio") + labs(title = "") +
  geom_bar(aes(fill = Light), position = "dodge", stat="identity") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE, group = factor(Light)), width=.2,
                position=position_dodge(.9))



#Exploring Additional Dependent Variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#total mycorrhizal colonization
EMAMsum = AMFdata$Total_AMF_Presence+AMFdata$AMF_EM_Mantles
modC_totcol = lmer(EMAMsum~Nitrogen*Light*Moisture + Width..cm. + Highest.Order.Branching + (1|Plot..), data=AMFdata)
anova(modC_totcol)
#moisture p = .00722

emmeans(modC_totcol, ~Moisture)
emmeans(modC_totcol, list(pairwise ~ Moisture), adjust = "tukey")

#total symbiont presence
Symbiosum = AMFdata$Total_AMF_Presence+AMFdata$AMF_EM_Mantles+AMFdata$AMF_DSEs+AMFdata$AMF_Oomycetes+AMFdata$AMF_Septate_Hyphae
modC_totcolSymbio = lmer(Symbiosum~Nitrogen*Light*Moisture + Width..cm. + Highest.Order.Branching + (1|Plot..), data=AMFdata)
anova(modC_totcolSymbio)
#took out: +AMFdata$AMF_Septate_Hyphae in model above but could include
#without septate: moisture p = .0032
#with septate: moisture p = .00011 ; Nitrogen p = .014 ; N:M p = .028 