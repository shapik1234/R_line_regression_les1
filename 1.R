#
require('corrplot')
#
require('MASS')
require('car')
require('lmtest')

#
source(file.choose())
#
obj<- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])

#построение корреляционной матрицы
(obj.cor <- cor(obj, method = 'pearson'))
#строим корелограмму
col4 <- colorRampPalette(c("#7F0000","purple","#FF7F00",
                           "yellow","#7FFF7F","cyan",
                           "#007FFF","blue","#00007F"))
corrplot(obj.cor,method="circle",col=col4(8),cl.lenght=6,
         order="AOE",addCoef.col="red")
#
fit <- lm(Murder~.,data = obj)

summary(fit)
confint(fit)
stepAIC(lm(Murder~Population+Illiteracy+Income+Frost,data = obj))

#
fit1 <- lm(formula = Murder ~ Population + Illiteracy, data = obj)
summary(fit1)
confint(fit1)
#
obj <- cbind.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost","Life Exp","HS Grad")])
#Мультиколениарность
sqrt(car::vif(fit))>=2
#
fit2 <- lm(Murder ~ Population + Illiteracy+Population+Life Exp, data = obj)
summary(fit2)
confint(fit2)
#
display.resid(fit2)
#
normality3.extend(fit2$residuals)
#
ncvTest(fit2)
#
durbinWatsonTest(fit2)
#
bgtest(fit2)
#
white.test(fit2)
sqrt(vif(fit2))