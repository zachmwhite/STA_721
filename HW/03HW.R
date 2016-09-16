# HW 3
library(lasso2)
library(ggplot2)
library(ggfortify)


## 2
data("Prostate")

prostate.lm = lm(lcavol~., data = Prostate)
summary.pros = summary(prostate.lm)
pros.coefs = summary.pros$coefficients
cbind(exp(pros.coefs[,"Estimate"]),exp(confint(prostate.lm)))
cbind(pros.coefs[,"Estimate"],confint(prostate.lm))

prostate.sv = lm(lcavol ~ svi + lpsa, data = Prostate)
summary.sv = summary(prostate.sv)
coefs = summary.sv$coefficients
cbind(coefs[,"Estimate"],confint(prostate.sv))
exp(cbind(coefs[,"Estimate"],confint(prostate.sv)))
coefs.est = coefs[,"Estimate"]

lpsa = Prostate$lpsa
data1 <- data.frame(cbind(seq(min(lpsa)-1,max(lpsa)+1,
                              length.out=100),rep(0,100)))
colnames(data1) <- c("lpsa","svi")

data2 <- data1
data2$svi <- rep(1,100)

pred.svi.not = predict.lm(prostate.sv,newdata = data1, interval = "c", level = .95 )
pred.svi = predict.lm(prostate.sv,newdata = data2,interval = "c", level = .95) 

data.svi.not = cbind(data1,pred.svi.not)
data.svi = cbind(data2, pred.svi)

prostate.plot = ggplot(data = Prostate, aes(x = lpsa, y = lcavol, color = factor(svi))) + geom_point()
prostate.plot + geom_abline(intercept = coefs.est[1] + coefs.est[2], slope = coefs.est[3], color = "red") +
  geom_abline(intercept = coefs.est[1], slope = coefs.est[3], color = "blue") +
  geom_line(data = data.svi, aes(x = lpsa, y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = data.svi, aes(x = lpsa, y = upr), color = "red", linetype = "dashed") +
  geom_line(data = data.svi.not, aes(x = lpsa, y = lwr), color = "blue", linetype = "dashed") +
  geom_line(data = data.svi.not, aes(x = lpsa, y = upr), color = "blue", linetype = "dashed")
  

