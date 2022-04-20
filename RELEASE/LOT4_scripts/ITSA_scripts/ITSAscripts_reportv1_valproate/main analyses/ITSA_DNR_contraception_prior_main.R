#ITSA and plot - report v1.0
#Date = 23.03.22 
DAP = "DNR"
analysis = "contraception_prior_main" 
#Analyst = RPajouheshnia
########################################
# PARAMETERS AND DATA
library(lmtest)
library(car)
library(nlme)

setwd("Y:/research-valproate-pooled/Report v1.0/Objective 2/Counts tables/main analyses")

datao <- read.csv("DNR_ALL_Valproate_contraception_prior_counts.csv")
datao <- datao[4:108,]
yvar <- datao$percent  #outcome variable
min(yvar) 
max(yvar)
m_t <- dim(datao)[1]   # number months
m <- table(datao$level)[1]#month number before intervention
ymin <- 0
ymax <- 30
y_lab <- "Percent prescriptions with prior contraception" 
int.date <- m + 0.5 # moment just before intervention
plot.name <- paste0("ITSA_", DAP, "_", analysis, ".pdf") 
#include<-c(1:43,46:62) # date range truncation to remove dates with no/poor data quality; already done manually in results files
#------------------------------------------------------------
# Plot outcome variable versus time
setwd("Y:/research-valproate-pooled/Report v1.0/Objective 2/ITSA_plots/main analyses")

pdf(file = plot.name,   # The directory you want to save the file in
    width = 6, # The width of the plot in inches
    height = 3) # The height of the plot in inches

plot(datao$time,
     yvar,
     ylab=y_lab,
     ylim=c(ymin,ymax),
     xlab="Date",
     type="l",
     col="red",
     xaxt="n",
     cex.lab = 0.7,
     cex.axis = 0.7)

# Add x-axis year labels
axis(1, at=1:m_t, labels=datao$date, cex.axis = 0.7)

# Add in the points for the figure
points(datao$time,yvar,
       col="red",
       pch=20)

# Label the intervention
abline(v=int.date,lty=2)

dev.off()