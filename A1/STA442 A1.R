#Question1
library(faraway)
data('fruitfly', package='faraway')
# glimpse the data
fruitfly
summary(fruitfly)

#change the factor levels
levels(fruitfly$activity) <- c("Solitary", "1 Preg Fly", "1 Vig Fly", "8 Preg Flies", "8 Vig Flies") 

#create the table1 "Mean longevity of each fruitfly group"
aggdata = aggregate(longevity~activity, fruitfly, mean )
aggdata = round(aggdata[,2])
aggdata = as.matrix(aggdata,5)
colnames(aggdata)<-c("Longevity (Days)")
rownames(aggdata)<- c("Isolated","With 1 Pregnant Fly","With 1 Virgin Fly","With 8 Pregnant Flies","With 8 Virgin Flies")
knitr::kable(aggdata, cap="Marginal means of each fruit fly group")

#normalize thorax
c = mean(fruitfly$thorax)
d = var(fruitfly$thorax)
new_thorax = ((fruitfly$thorax)-c)/sqrt(d)

#fit a Gamma Regression Model with log link fuction
fitmod=glm(longevity~new_thorax + activity,family=Gamma(link='log'), data=fruitfly)
#create the table 2 "Estimated parameters from the Gamma regression model of the fruitflies"
coeffdata = round(summary(fitmod)$coef,3)
coeffdata[,1] = round(exp(coeffdata[,1]),3) #to use a more natural Scale
colnames(coeffdata) <- c("Exp. Estimate", "Std. Error", "t value", "P-Value") 
rownames(coeffdata) <- c("Intercept" ,"Thorax Length", "With 1 Pregnant Fly","With 1 Virgin Fly","With 8 Pregnant Flies",
                        "With 8 Virgin Flies")
knitr::kable(coeffdata, cap="Estimated parameters from the Gamma regression model of the fruitflies")


#Question 2
smokeUrl = 'http://pbrown.ca/teaching/appliedstats/data/smoke.RData'
(smokeFile = tempfile(fileext='.RData'))
download.file(smokeUrl, smokeFile, mode='wb')
(load(smokeFile))

#glimpse the data
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'chewing_tobacco_snuff_or')]
smoke[1:5,c('Age','Sex','Grade','RuralUrban','Race', 'ever_tobacco_hookah_or_wa')]

#reponse of two models
smoke$Reg_chew_tob = factor(smoke$chewing_tobacco_snuff_or, levels=c('TRUE','FALSE'), labels=c('yes','no'))
smoke$Ever_hookah = factor(smoke$ever_tobacco_hookah_or_wa, levels=c('TRUE','FALSE'), labels=c('yes','no'))

#' nine year olds look suspicious
#' get rid of missings(NA) and age 9
#+ smokeSub
smokeSub1 = smoke[smoke$Age != 9 & !is.na(smoke$Race) &
                   !is.na(smoke$Reg_chew_tob), ]
smokeSub2 = smoke[smoke$Age != 9 & !is.na(smoke$Race) &
                    !is.na(smoke$Ever_hookah), ]
smokeAgg1 = reshape2::dcast(smokeSub1,
                           Age + Sex + Race + RuralUrban ~ Reg_chew_tob,
                           length) 
smokeAgg2 = reshape2::dcast(smokeSub2,
                            Age + Sex + Race + RuralUrban ~ Ever_hookah,
                            length)
# data collection finished
smokeAgg1 = na.omit(smokeAgg1)
smokeAgg2 = na.omit(smokeAgg2)

# center Age so intercept is age 15
smokeAgg1$ageC = smokeAgg1$Age - 15 
smokeAgg2$ageC = smokeAgg2$Age - 15 
# fit two binomial models
smokeAgg1$y = cbind(smokeAgg1$yes, smokeAgg1$no)
smokeAgg2$y = cbind(smokeAgg2$yes, smokeAgg2$no)
smokeFit1 = glm(y ~ ageC + Sex + Race + RuralUrban, 
               family=binomial(link='logit'), data=smokeAgg1)
smokeFit2 = glm(y ~ ageC + Sex + Race + RuralUrban, 
                family=binomial(link='logit'), data=smokeAgg2)
knitr::kable(anova(aov(smokeFit1)), digits=3, 
             cap="ANOVA summary table for modelling odds of regular use of chewing tobacco")

#convert to odds 
smokeTable1 = summary(smokeFit1)$coef
smokeTable1[,1] = round(exp(smokeTable1[,1]),3)
smokeTable2 = summary(smokeFit2)$coef
smokeTable2[,1] = round(exp(smokeTable2[,1]),3)

# make row names nicer
rownames(smokeTable1) <- c("Intercept" ,"Age", "Female","Black","Hispanic","Asian","Native","Pacific",
                        "Rural")
rownames(smokeTable2) <- c("Intercept" ,"Age", "Female","Black","Hispanic","Asian","Native","Pacific",
                           "Rural")
#make column names nicer
colnames(smokeTable1) <- c("Exp. Estimate", "Std. Error", "z value", "P-Value")
colnames(smokeTable2) <- c("Exp. Estimate", "Std. Error", "z value", "P-Value")
#create the table1
knitr::kable(smokeTable1, cap="Estimated odds of regular use of chewing tobacco",digits = 3)
#create the table2
knitr::kable(smokeTable2, cap="Estimated odds of ever using hookah or waterpipe",digits = 3)

