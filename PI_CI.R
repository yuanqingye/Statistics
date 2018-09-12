set.seed(123)
hemoglobin<-rnorm(400, mean = 139, sd = 14.75)
df<-data.frame(hemoglobin)

CI<-predict(lm(df$hemoglobin~ 1), interval="confidence") 
CI[1,]

PI<-predict(lm(df$hemoglobin~ 1), interval="predict")
PI[1,]

library(ggplot2)
limits_CI <- aes(x=1.3  , ymin=CI[1,2], ymax =CI[1,3])
limits_PI <- aes(x=0.7 , ymin=PI[1,2], ymax =PI[1,3])
PI_CI<-ggplot(df, aes(x=1, y=hemoglobin)) + 
  geom_jitter(width=0.1, pch=21, fill="grey", alpha=0.5) + 
  geom_errorbar (limits_PI, width=0.1, col="#1A425C") +
  geom_point (aes(x=0.7, y=PI[1,1]), col="#1A425C", size=2) +
  geom_errorbar (limits_CI, width=0.1, col="#8AB63F") +
  geom_point (aes(x=1.3, y=CI[1,1]), col="#8AB63F", size=2) +
  
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(95,190))+
  theme_bw()+ylab("Hemoglobin concentration (g/L)") + 
  xlab(NULL)+
  geom_text(aes(x=0.6, y=160, label="Prediction\ninterval", 
                hjust="right", cex=2), col="#1A425C")+
  geom_text(aes(x=1.4, y=140, label="Confidence\ninterval", 
                hjust="left", cex=2), col="#8AB63F")+
  theme(legend.position="none", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
PI_CI


Hb<- read.table("http://rforbiostatistics.colmanstatistics.be/wp-content/uploads/2018/06/Hb.txt", 
                header = TRUE)
library(knitr)
kable(head(Hb))

plot(Hb$New, Hb$Reference, 
     xlab="Hemoglobin concentration (g/L) - new method", 
     ylab="Hemoglobin concentration (g/L) - reference method")

fit.lm <- lm(Hb$Reference ~ Hb$New)
plot(Hb$New, Hb$Reference,  
     xlab="Hemoglobin concentration (g/L) - new method", 
     ylab="Hemoglobin concentration (g/L) - reference method")
cat ("Adding the regression line:")

abline (a=fit.lm$coefficients[1], b=fit.lm$coefficients[2] )
cat ("Adding the identity line:")
abline (a=0, b=1, lty=2)

CI_ex <- predict(fit.lm, interval="confidence")
colnames(CI_ex)<- c("fit_CI", "lwr_CI", "upr_CI")

PI_ex <- predict(fit.lm, interval="prediction")
## Warning in predict.lm(fit.lm, interval = "prediction"): predictions on current data refer to _future_ responses
colnames(PI_ex)<- c("fit_PI", "lwr_PI", "upr_PI")

Hb_results<-cbind(Hb, CI_ex, PI_ex)
kable(head(round(Hb_results),1))

plot(Hb$New, Hb$Reference, 
     xlab="Hemoglobin concentration (g/L) - new method", 
     ylab="Hemoglobin concentration (g/L) - reference method")
Hb_results_s <- Hb_results[order(Hb_results$New),]
lines (x=Hb_results_s$New, y=Hb_results_s$fit_CI)
lines (x=Hb_results_s$New, y=Hb_results_s$lwr_CI, 
       col="#8AB63F", lwd=1.2)
lines (x=Hb_results_s$New, y=Hb_results_s$upr_CI, 
       col="#8AB63F", lwd=1.2)
lines (x=Hb_results_s$New, y=Hb_results_s$lwr_PI, 
       col="#1A425C", lwd=1.2)
lines (x=Hb_results_s$New, y=Hb_results_s$upr_PI,
       col="#1A425C", lwd=1.2)
abline (a=0, b=1, lty=2)

library (BivRegBLS)
Hb.BLS = BLS (data = Hb, xcol = c("New"), 
              ycol = c("Reference"), var.y=10, var.x=8, conf.level=0.95)
XY.plot (Hb.BLS,
         yname = "Hemoglobin concentration (g/L) - reference method",
         xname = "Hemoglobin concentration (g/L) - new method",
         graph.int = c("CI","PI"))