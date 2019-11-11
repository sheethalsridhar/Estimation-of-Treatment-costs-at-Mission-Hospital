
# data prep ---------------------------------------------------------------


library(readxl)
mission <- read_excel("Downloads/mission.xlsx")
summary(mission)
str(mission)
summary(mission$ALERT)
mission$AGE<- ifelse(mission$AGE <= 10, 1, ifelse(mission$AGE <= 25, 2, ifelse(mission$AGE<=50,3,4)))
mission$AGE<-as.factor(mission$AGE)
mission$GENDER<-as.factor(mission$GENDER)
mission$`MARITAL STATUS`<-as.factor(mission$`MARITAL STATUS`)
mission$`KEY COMPLAINTS -CODE`<-as.factor(mission$`KEY COMPLAINTS -CODE`)
mission$ACHD<-as.factor(mission$ACHD)
mission$`CAD-DVD`<-as.factor(mission$`CAD-DVD`)
mission$`CAD-SVD`<-as.factor(mission$`CAD-SVD`)
mission$`CAD-TVD`<-as.factor(mission$`CAD-TVD`)
mission$`CAD-VSD`<-as.factor(mission$`CAD-VSD`)
mission$`OS-ASD`<-as.factor(mission$`OS-ASD`)
mission$`other- heart`<-as.factor(mission$`other- heart`)
mission$`other- respiratory`<-as.factor(mission$`other- respiratory`)
mission$`other-general`<-as.factor(mission$`other-general`)
mission$`other-nervous`<-as.factor(mission$`other-nervous`)
mission$`other-tertalogy`<-as.factor(mission$`other-tertalogy`)
mission$`PM-VSD`<-as.factor(mission$`PM-VSD`)
mission$RHD<-as.factor(mission$RHD)
BMI = function(height,weight){
  return(0.45455*weight/(.0254*height)^2)}
mission$BMI <- BMI(mission$`BODY HEIGHT`,mission$`BODY WEIGHT`)
#mission$`BODY HEIGHT`<-NULL
#mission$`BODY WEIGHT`<-NULL
mission$`BP-LOW`<-as.numeric(mission$`BP-LOW`)
mission$Diabetes1<-as.factor(mission$Diabetes1)
mission$Diabetes2<-as.factor(mission$Diabetes2)
mission$hypertension1<-as.factor(mission$hypertension1)
mission$hypertension2<-as.factor(mission$hypertension2)
mission$hypertension3<-as.factor(mission$hypertension3)
mission$other<-as.factor(mission$other)
mission$`MODE OF ARRIVAL`<-as.factor(mission$`MODE OF ARRIVAL`)
mission$`TYPE OF ADMSN`<-as.factor(mission$`TYPE OF ADMSN`)
mission$IMPLANT<-as.factor(mission$IMPLANT)
is.na(mission)
summary(mission)
mission1<-mission
mission<-mission[!is.na(mission$`BP-LOW`),]
mission<-mission[!is.na(mission$HB),]
mission<-mission[!is.na(mission$UREA),]
mission<-mission[!is.na(mission$CREATININE),]

mission<-na.omit(mission)
str(mission)


# anova -------------------------------------------------------------------

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$AGE, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$GENDER, data=cancer)
mission.aov
summary(mission.aov) # ....not that imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`MARITAL STATUS`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`KEY COMPLAINTS -CODE`, data=mission)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.tk<-TukeyHSD(mission.aov)
round(mission.tk$`TOTAL COST TO HOSPITAL`,3)

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$ACHD, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`CAD-DVD`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`CAD-SVD`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`CAD-TVD`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`CAD-VSD`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`OS-ASD`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`other- heart`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`other- respiratory`, data=cancer)
mission.aov
summary(mission.aov) #not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`other-general`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`other-nervous`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`other-tertalogy`, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`PM-VSD`, data=cancer)
mission.aov
summary(mission.aov) # Not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$RHD, data=cancer)
mission.aov
summary(mission.aov) # 0.0532 . imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$Diabetes1, data=cancer)
mission.aov
summary(mission.aov) # Not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$Diabetes2, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$hypertension1, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$hypertension2, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$hypertension3, data=cancer)
mission.aov
summary(mission.aov) # not imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$other, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`MODE OF ARRIVAL`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$`TYPE OF ADMSN`, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$IMPLANT, data=cancer)
mission.aov
summary(mission.aov) # Null rejected....its imp

mission.aov <- aov(mission$`TOTAL COST TO HOSPITAL`~mission$BMI, data=mission)
mission.aov
summary(mission.aov) # Null rejected....its imp


# model -------------------------------------------------------------------


model <- lm(`Ln(Total Cost)`~., data = mission)
summary(model)
