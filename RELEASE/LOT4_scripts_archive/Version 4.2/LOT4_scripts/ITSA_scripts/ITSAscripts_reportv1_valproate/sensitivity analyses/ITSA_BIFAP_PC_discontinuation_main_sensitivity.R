#ITSA and plot - report v1.0
#Date = 23.03.22 
DAP = "BIFAP_PC"
analysis = "discontinuation_sensitivity" 
#Analyst = RPajouheshnia
########################################
# PARAMETERS AND DATA
library(lmtest)
library(car)
library(nlme)

setwd("Y:/research-valproate-pooled/Report v1.0/Objective 1/Counts tables/main analyses")

datao <- read.csv("BIFAP_PC_Valproate_discontinued_counts_final_Pooled.csv")
datao<-datao[61:122,]
datao$time <- 1:62

yvar <- datao$percent  #outcome variable
min(yvar) 
max(yvar)
m_t <- dim(datao)[1]   # number months
m <- table(datao$level)[1]#month number before intervention
ymin <- 0
ymax <- 20
y_lab <- "Percentage discontinued valproate"
int.date <- m + 0.5 # moment just before intervention
plot.name <- paste0("ITSA_", DAP, "_", analysis, ".pdf") 
#include<-c(1:43,46:62) # date range truncation to remove dates with no/poor data quality; already done manually in results files
#------------------------------------------------------------
# Plot outcome variable versus time
plot(datao$time,
     yvar,
     ylab=y_lab,
     ylim=c(ymin,ymax),
     xlab="Date",
     type="l",
     col="red",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:m_t, labels=datao$date)

# Add in the points for the figure
points(datao$time,yvar,
       col="red",
       pch=20)

# Label the intervention
abline(v=int.date,lty=2)

#--------------------------------------------------
#ITSA
#data_pi<-datao[include,] #Phase-in - not necessary as done manually
data_pi <- datao

#Modelling-preliminary OLS regression#
model_ols <- lm(yvar~time + level + trend, data = data_pi)

#Autocorrelation#
#Durbin-watson test#
dwtest(model_ols, alternative="two.sided")
durbinWatsonTest(model_ols, max.lag = 12, alternative = "two.sided")

acf(residuals(model_ols))
acf(residuals(model_ols),type="partial")

#Final model#
#model_final<-gls(yvar~time+level+trend,data = data_pi)
#model_final<-gls(yvar~time+level+trend,data = data_pi, correlation = corARMA(p=2,form =~time),method ="ML")
model_final<-gls(yvar~time+level+trend,data = data_pi,method ="ML")
summary(model_final)

#Checking the model               
model_p11<-update(model_final,correlation=corARMA(p=9,form = ~time))
anova(model_p11,model_final)

#Plot results#
setwd("Y:/research-valproate-pooled/Report v1.0/Objective 1/ITSA_plots/sensitivity analyses")

pdf(file = plot.name,   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 3) # The height of the plot in inches

plot(datao$time,
     yvar,
     ylim=c(ymin,ymax),
     ylab=y_lab,
     xlab="Date",
     pch=20,
     col="pink",
     xaxt="n",
     cex.axis = 0.8,
     cex.lab = 0.8)
axis(1, at=1:m_t, labels=datao$date, cex.axis=0.8)
abline(v=int.date,lty=2)

# Plot the first line segment
lines(data_pi$time[1:m], fitted(model_final)[1:m], col="red",lwd=2)

# Plot the second line segment
lines(data_pi$time[(m+1):m_t], fitted(model_final)[(m+1):m_t], col="red",lwd=2)

# And the counterfactual
segments(m+3,
         model_final$coef[1]+model_final$coef[2]*(m+3),
         m_t,
         model_final$coef[1]+model_final$coef[2]*m_t,
         lty=2,
         lwd=2,
         col='red')

rect((m+0.5),-2,(m+2.5),20,border = NA, col = "#00000011")

dev.off()
