# set appropriate path
read.csv("~/Kemp_Cheirogaleus_leaping_data.csv", header=T) -> d
#data prep
d$Individual <- factor(d$Individual, levels= c("A", "B", "C", "D", "E"))
d$Condition <- factor(d$Condition, levels= c("Control", "Blinder"))
d$adjustNum <- as.factor(d$adjustNum)
d$OutcomeBinary <- as.factor(d$OutcomeBinary)
d$OutcomeSevere <- as.factor(d$OutcomeSevere)
d$OutcomeType <- as.factor(d$OutcomeType)
BCcol <- c("#0070C0", "#C00000")
d$OutcomeType <- factor(d$OutcomeType,levels=c("Normal", "Strike", "Spin", "Drop", "Fall"))
d$shortest <- as.factor(d$shortest)

library(lsmeans); library(lmerTest); library(emmeans)
library(dplyr); library(ggplot2)
library(lme4)

#ADVERSE LANDINGS
#models 
outcomeModel1 <- glmer(OutcomeBinary ~ Condition + logDistance.cm. + logSpeed.m.s. + LeapSincePoleChange + Parallax + (1|Individual), data = d, family = binomial) 
summary(outcomeModel1)
BIC(outcomeModel1)
outcomeModel2 <- glmer(OutcomeBinary ~ Condition + logDistance.cm. + logSpeed.m.s. + LeapSincePoleChange + (1|Individual), data = d, family = binomial) 
summary(outcomeModel2)
BIC(outcomeModel2)
outcomeModel3<- glmer(OutcomeBinary ~ Condition + logDistance.cm. + logSpeed.m.s. + (1|Individual), data = d, family = binomial)
summary(outcomeModel3)
BIC(outcomeModel3)
outcomeInteraction <- glmer(OutcomeBinary ~ Condition + logDistance.cm. + logSpeed.m.s. + logDistance.cm.:Condition + (1|Individual), data = d, family = binomial)
summary(outcomeInteraction)
BIC(outcomeInteraction)

#Adverse landing plots 
outcomeDF <- data.frame(d%>%group_by(Condition, Individual, OutcomeType)%>%summarise(n =n())%>% mutate(prop = 100*n / sum(n))) 
outcomeDF$prop <- as.numeric(outcomeDF$prop)
outcomeDF$n <- as.integer(outcomeDF$n)
plotOutcomeDF <- data.frame(d%>%group_by(Condition, OutcomeType)%>%summarise(n =n())%>% mutate(prop = 100*n / sum(n)))
plotOutcomeDF$prop <- as.numeric(plotOutcomeDF$prop)
plotOutcomeDF$n <- as.integer(plotOutcomeDF$n)
outcomeDF$OutcomeType <- factor(outcomeDF$OutcomeType, levels = c("Normal","Strike", "Spin", "Drop", "Fall"))
outcomeDF2 <- filter(outcomeDF, OutcomeType != "Normal")
#Filling in lines for condition/landing type combinations some individuals did 
#not ehxhibit manually to have a complete dataframe
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "A", "Strike", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "B", "Strike", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "C", "Strike", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "E", "Strike", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "D", "Strike", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "A", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "B", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "C", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "D", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "E", "Fall", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Control", "D", "Fall", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "A", "Fall", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "C", "Fall", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "C", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "D", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "E", "Drop", 0,  0)
outcomeDF2[nrow(outcomeDF2) + 1,] <- list("Blinder", "E", "Fall", 0,  0)
outcomeDF2$Individual <- as.factor(outcomeDF2$Individual)

#Landing type frequency plots for all landing types
ggplot(plotOutcomeDF, aes(x=OutcomeType, y=prop, fill= Condition)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  theme_bw(base_size=25) + 
  xlab(NULL) + ylab("Percentage of leaps") +
  scale_y_continuous(position = "right")+
  scale_fill_manual(values = BCcol)

#Landing type frequency plots foronly adverse landings
ggplot(outcomeDF2, aes(x=OutcomeType, y=prop, fill= Condition)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  theme_bw(base_size=22) + 
  xlab(NULL) + ylab("Percentage of leaps") +
  ylim(0,7) +
  scale_y_continuous(position = "right")+
  scale_fill_manual(values = BCcol)+ 
  facet_wrap(.~Individual, ncol = 1, strip.position = "left") +
  theme(strip.text.y = element_text(angle = 0))


#LANDING TYPE
#models
library(nnet)
d$Outcome2 <- relevel(d$OutcomeType, ref = "Normal")
TypeAll <- multinom(Outcome2 ~ Condition + logDistance.cm. + Leap + 
                      logSpeed.m.s. + LeapSincePoleChange+ Parallax, 
                    data = d)
summary(TypeAll)
BIC(TypeAll)
Type2 <- multinom(Outcome2 ~ Condition + logDistance.cm. + logSpeed.m.s. + 
                    LeapSincePoleChange+ Parallax, 
                  data = d)
summary(Type2)
BIC(Type2)
Type3 <- multinom(Outcome2 ~ Condition + logDistance.cm. + logSpeed.m.s. + 
                    LeapSincePoleChange, 
                  data = d)
summary(Type3)
BIC(Type3)
Type4 <- multinom(Outcome2 ~ Condition + logDistance.cm. + logSpeed.m.s.,
                  data = d)
summary(Type4)
BIC(Type4)
Type4INT <- multinom(Outcome2 ~ Condition + logDistance.cm. + logSpeed.m.s. + 
                       Condition:logDistance.cm.,
                     data = d)
summary(Type4INT)
BIC(Type4INT)
Type5 <- multinom(Outcome2 ~ Condition + logDistance.cm.,
                  data = d)
summary(Type5)
BIC(Type5)
Type6 <- multinom(Outcome2 ~ Condition +  logSpeed.m.s.,
                  data = d)
summary(Type6)
BIC(Type6)
Type7 <- multinom(Outcome2 ~  logDistance.cm. + logSpeed.m.s.,
                  data = d)
summary(Type7)
BIC(Type7)

#Predicted probabilities of landing types under restricted and 
#control conditions based on best-fit model (Type4)
z <- summary(Type4)$coefficients/summary(Type4)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
exp(coef(Type4))
dCond <- data.frame(Condition = c("Control", "Blinder"), 
                    logSpeed.m.s. = mean(d$logSpeed.m.s.), 
                    logDistance.cm. = mean(d$logDistance.cm.))
predict(Type4, newdata = dCond, "probs")

#ADJUSTMENTS
library(dplyr)
adjustDF <- data.frame(d%>%filter(adjustNum!= "NA")%>%group_by(Condition, Individual, adjustNum)%>%summarise(n =n())%>% mutate(prop = 100*n / sum(n)))

ggplot(adjustDF, aes(x=adjustNum, y=prop, fill=Condition)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+ 
  theme_bw(base_size = 25)  + ylab("Percentage of leaps")+ 
  xlab("Grasp adjustments")+
  scale_fill_manual(values =  BCcol) +
  theme(legend.position = "none")

library(lme4)
adjModel1 <- glmer(as.numeric(adjustNum)~Condition + logDistance.cm.  + logSpeed.m.s. + Leap  + LeapSincePoleChange + Parallax + (1|Individual),family= poisson, data = d)
summary(adjModel1)
BIC(adjModel1)

adjModel2 <- glmer(as.numeric(adjustNum)~Condition + logDistance.cm.  + 
                     logSpeed.m.s. + Leap  + Parallax + (1|Individual),
                   family= poisson, data = d)
summary(adjModel2)
BIC(adjModel2)

adjModel3 <- glmer(as.numeric(adjustNum)~Condition + logDistance.cm.  + 
                     logSpeed.m.s. + logDistance.cm.:logSpeed.m.s.+  Leap  + 
                     (1|Individual),family= poisson, data = d)
summary(adjModel3)
BIC(adjModel3)

adjModel4 <- glmer(as.numeric(adjustNum)~Condition + logDistance.cm.  + Leap  +
                     (1|Individual),family= poisson, data = d)
summary(adjModel4)
BIC(adjModel4)

adjModeltest <- lmer(as.numeric(adjustNum)~Condition + logDistance.cm. + Leap +
                       (1|Individual),data = d, REML = FALSE)
adjnull <- lmer(as.numeric(adjustNum)~ logDistance.cm. + Leap + 
                  LeapSincePoleChange+ (1|Individual),data = d, REML = FALSE)
predict(adjModeltest) -> out
mean(out[which(d$Condition== "Blinder")], na.rm=T)
anova(adjnull, adjModeltest)
summary(adjModel4)
summary(adjModeltest)

#SPEED
#models
speedModel1 <- lmer(logSpeed.m.s. ~Condition + logDistance.cm.  + Leap + 
                      LeapSincePoleChange+ Parallax+  (1|Individual), data = d)
summary(speedModel1)
BIC(speedModel1)
speedModel2 <- lmer(logSpeed.m.s. ~Condition + logDistance.cm.  + Leap + 
                      LeapSincePoleChange+  (1|Individual), data = d)
summary(speedModel2)
BIC(speedModel2)
speedModel3 <- lmer(logSpeed.m.s. ~Condition + logDistance.cm.  + Leap + 
                      LeapSincePoleChange+ logDistance.cm. :Condition + 
                      (1|Individual), data = d)
summary(speedModel3)
BIC(speedModel3)

predict(speedModel3) -> out
1/mean(out[which(d$Condition== "Blinder")], na.rm=T)

#plot
ggplot(d, aes(Distancebin, logSpeed.m.s. , fill = Condition)) +geom_boxplot() +
  theme_bw(base_size = 20)+
  ylab("Log10 speed (m/s)")+ xlab("Distance")+
  scale_fill_manual(values = BCcol) + theme(legend.position="none") 

#PATH
#models
d$shortest <- relevel(d$shortest, ref = "Other")

pathModel <- glmer(shortest ~ Condition + Leap + LeapSincePoleChange +
                     (1|Individual), data = d, family = binomial)
summary(pathModel) 
BIC(pathModel)

lsmeans(pathModel, pairwise~Condition, type="response")

m2 <- summary(lsmeans(pathModel, pairwise~Condition, type="response")$lsmeans)
m2_plot <- m2[c("Condition", "lsmean", "SE")]
m2_plot$lsmean <- m2_plot$lsmean*100
m2_plot$SE <- m2_plot$SE*100

#plot
ggplot(m2_plot, aes(x = Condition, y = lsmean)) +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), width = 0.5, linewidth = 2, colour = BCcol) +
  geom_point(colour = BCcol, size = 4) +
  labs(x = "Condition", y = "Predicted probability of \n choosing shortest path (%)") +
  theme_bw(base_size = 25)


d$shortest <- factor(d$shortest, levels=c("Shortest", "Other"))
data.frame(d%>%group_by(Condition, Individual, shortest)%>%summarise(n =n())%>% 
      mutate(prop = 100*n / sum(n)))%>%filter(shortest=="Shortest") -> shortIndDF
#Individual B never took shortest path under control conditions, adding that 
#line separately
shortIndDF[10, ] <- list("Control", "B", "Shortest", 0, 0)
shortIndDF$prop <- round(shortIndDF$prop)

ggplot(shortIndDF, aes(x=Individual, y=prop, fill= Condition)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  theme_bw(base_size=20) + xlab("Individual") + 
  ylab("Shortest path chosen\n(% of leaps)") + 
  scale_fill_manual(values = BCcol) +theme(legend.position="none")

#PARALLAX
#model
d$Parallax <- as.factor(d$Parallax)
paraModel1 <- glmer(Parallax ~ Condition + logDistance.cm. + Leap  + LeapSincePoleChange  + (1|Individual), data = d, family = binomial) 
summary(paraModel1)
BIC(paraModel1)

paraModel2 <- glmer(Parallax ~ Condition + logDistance.cm. + Leap  + (1|Individual), data = d, family = binomial) 
summary(paraModel2)
BIC(paraModel2)


#plot
#Predicted probabilities of parallax use under restricted and 
#control conditions based on best-fit model (paraModel1)
m2 <- summary(lsmeans(paraModel1, pairwise~Condition, type="response")$lsmeans)
m2_plot <- m2[c("Condition", "lsmean", "SE")]
m2_plot$lsmean <- m2_plot$lsmean*100
m2_plot$SE<- m2_plot$SE*100

ggplot(m2_plot, aes(x = Condition, y = lsmean)) +
  geom_errorbar(aes(ymin = lsmean - SE, ymax = lsmean + SE), width = 0.5, linewidth = 2, colour = BCcol) +
  geom_point(colour = BCcol, size = 4) +
  labs(x = "Condition", y = "Predicted probability of\n using motion parallax") +
  theme_bw(base_size = 25) +ylim(-.05, 2.05)



