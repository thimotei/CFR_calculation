dev.off()

layout(matrix(c(1, 2),
              nrow=1, byrow=TRUE))


xSamplesDays <- seq(0,40, 1)
xSamplesCurve <- seq(0,40, 0.1)
samplingFromDelayDistDays <- onset_to_death(xSamplesDays)
samplingFromDelayDistCurve <- onset_to_death(xSamplesCurve)
plot(xSamplesDays, samplingFromDelayDistDays, 
     xlab = "Days after onset",
     pch = 19, 
     ylab = "P(death on a given day | death)",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(xSamplesCurve, samplingFromDelayDistCurve)


xSamplesDays <- seq(0,40, 1)
xSamplesCurve <- seq(0,40, 0.1)
samplingFromDelayDistDays <- confirmation_to_death(xSamplesDays)
samplingFromDelayDistCurve <- confirmation_to_death(xSamplesCurve)
plot(xSamplesDays, samplingFromDelayDistDays, 
     xlab = "Days after onset",
     pch = 19, 
     ylab = "P(death on a given day | death)",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(xSamplesCurve, samplingFromDelayDistCurve)

# plotting Wuhan CFR estimates using inferred case data and real death data

dev.off()

layout(matrix(c(1,
                2,
                3),
              nrow=3, byrow=TRUE))


end2 <- 33 # 
y1lim <- 0.28


start2 <- 50

par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
plotCI(x=nCFRInferredCIs$date[1:end2],
       y=nCFRInferredCIs$ci_mid[1:end2],
       li=nCFRInferredCIs$ci_low[1:end2],
       ui=nCFRInferredCIs$ci_high[1:end2],
       xlab = "Date",
       ylab = "CFR (%)", 
       yaxt='n',
       ylim=c(0,y1lim),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = nCFRInferredCIs$date[1:end2], nCFRInferredCIs$ci_mid[1:end2], col = rgb(0.8, 0.2, 0.6))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")

par(new=T)

par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
plotCI(x=cCFRInferredCIs$date[1:end2],
       y=cCFRInferredCIs$ci_mid[1:end2],
       li=cCFRInferredCIs$ci_low[1:end2],
       ui=cCFRInferredCIs$ci_high[1:end2],
       xlab = "Date",
       ylab = "CFR (%)",
       ylim=c(0,y1lim),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = cCFRInferredCIs$date[1:end2], cCFRInferredCIs$ci_mid[1:end2], col = rgb(0.2, 0.8, 0.6))
#grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(cCFRInferredCIs$date[2], y1lim, 
       legend=c("nCFR using inferred Wuhan data",
                "cCFR using inferred Wuhan data"), 
       col=c(rgb(0.8, 0.2, 0.6), rgb(0.2, 0.8, 0.6)),
       lty=1:1, 
       cex=1.3)
par(xpd=TRUE)
text(cCFRInferredCIs$date[1] - 2,0.3, "A")

par(xpd=FALSE)
plot(x = allTogetherInferred$date[start2:82],
     allTogetherInferred$new_cases[start2:82], 
     xlab = "Date",
     ylab = "Incidence (inferred)", 
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(x = allTogetherInferred$date[start2:82], allTogetherInferred$new_cases[start2:82], col = "green")
par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
par(new=T)
plot(x = allTogetherReal$date, 
     allTogetherReal$new_cases, 
     xlab = "",
     ylab = "", 
     axes = FALSE,
     cex = 1.3,
     cex.axis = 1.4)
legend(allTogetherReal$date[1], 13000, 
       legend=c("new inferred cases", 
                "new reported cases"),
       col=c("green", "blue"),
       lty = 1,
       cex=1.3,)
lines(x = allTogetherReal$date, allTogetherReal$new_cases, col = "blue")
axis(side = 4, cex.axis = 1.4)
par(mar=c(3,4,3,3))
par(xpd=TRUE)
text(allTogetherInferred$date[1] - 5, 14000, "B")
#mtext(side = 4, line = 3, 'Incidence (real)')


par(xpd=FALSE)
plot(x = allTogetherReal$date[start2:82], 
     allTogetherReal$new_deaths[start2:82],
     xlab = "Date",
     ylab="Incidence of deaths",
     cex.lab = 1.4,
     cex.axis = 1.4)
#title(ylab=, line=-1)
lines(x = allTogetherReal$date[start2:82], allTogetherReal$new_deaths[start2:82], col = "red")
par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(allTogetherReal$date[1], 32500, 
       legend=c("new cases (in Wuhan)", 
                "new deaths (in Wuhan)"),
       col=c("red", "blue"), lty=1:2,
       cex=1.3)
par(xpd=TRUE)
text(allTogetherReal$date[start2] - 2, 78, "C")

###### plotting international data

#pdf("international_CFR_estimates.pdf", width = 800, height = 600)

dev.off()
layout(matrix(c(1, 2,
                3, 4
                ),
              nrow=2, byrow=TRUE))

startInt <- 5
endInt <- nrow(nCFRInternationalCIs)

par(xpd=FALSE)
par(mar = c(3,4,1,1),mgp = c(2,0.6,0))
plotCI(x = nCFRInternationalCIs$date[startInt:endInt],
       y = nCFRInternationalCIs$ci_mid[startInt:endInt],
       li = nCFRInternationalCIs$ci_low[startInt:endInt],
       ui = nCFRInternationalCIs$ci_high[startInt:endInt],
       xlab = "Date", 
       ylab = "nCFR (%)",
       ylim = c(0,4),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = nCFRInternationalCIs$date[startInt:endInt], nCFRInternationalCIs$ci_mid[startInt:endInt], col = rgb(0.8, 0.2, 0.6))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
par(xpd=TRUE)
text(nCFRInternationalCIs$date[startInt] + 0.6 , 3.8, "A", cex = 1.5)

#par(new=T)

par(xpd=FALSE)
par(mar = c(3,4,1,1),mgp = c(2,0.6,0))
plotCI(x = cCFRInternationalCIs$date[startInt:endInt],
       y = cCFRInternationalCIs$ci_mid[startInt:endInt],
       li = cCFRInternationalCIs$ci_low[startInt:endInt],
       ui = cCFRInternationalCIs$ci_high[startInt:endInt], 
       xlab = "Date",
       ylab = "cCFR (%)",
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = cCFRInternationalCIs$date[startInt:endInt], cCFRInternationalCIs$ci_mid[startInt:endInt], col = rgb(0.2, 0.8, 0.6))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
par(xpd=TRUE)
text(nCFRInternationalCIs$date[startInt] + 1 , 19, "B", cex = 1.5)


par(xpd=FALSE)
plot(x = internationalDataRaw$date,
     internationalDataRaw$new_cases,
     xlab = "Date",
     ylab = "Incidence of confirmed cases",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(x = internationalDataRaw$date, internationalDataRaw$new_cases, col = "green")
par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
par(xpd=TRUE)
text(internationalDataRaw$date[1] + 1 , 115, "C", cex = 1.5)


par(xpd=FALSE)
plot(x = internationalDataRaw$date, 
     internationalDataRaw$new_deaths,
     xlab = "Date",
     ylab = "Incidence of deaths",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(x = internationalDataRaw$date, internationalDataRaw$new_deaths, col = "red")
par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(internationalDataRaw$date[1], 
       10, 
       legend=c("new international cases", "new international deaths"), 
       col=c("red", "blue"), 
       lty=1:2, 
       cex=0.8)
par(xpd=TRUE)
text(internationalDataRaw$date[1] + 1 , 1.95, "D", cex = 1.5)

#dev.off()

###### plotting international + cruise data


#png("international_cruise_CFR_estimates.png", width = 800, height = 800)

layout(matrix(c(1, 2,
                3, 4
),
nrow=2, byrow=TRUE))

startInt <- 1
endInt <- nrow(nCFRInternationalCruiseCIs)

par(xpd=FALSE)
par(mar = c(3,4,1,1),mgp = c(2,0.6,0))
plotCI(x = nCFRInternationalCruiseCIs$date[startInt:endInt],
       y = nCFRInternationalCruiseCIs$ci_mid[startInt:endInt],
       li = nCFRInternationalCruiseCIs$ci_low[startInt:endInt],
       ui = nCFRInternationalCruiseCIs$ci_high[startInt:endInt],
       xlab = "Date", 
       ylab = "nCFR (%)",
       ylim = c(0,4),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = nCFRInternationalCruiseCIs$date[startInt:endInt],
      nCFRInternationalCruiseCIs$ci_mid[startInt:endInt],
      col = rgb(0.8, 0.2, 0.6))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
par(xpd=TRUE)
text(nCFRInternationalCruiseCIs$date[startInt] + 0.3 , 3.8, "A", cex = 1.5)



par(xpd=FALSE)
par(mar = c(3,4,1,1),mgp = c(2,0.6,0))
plotCI(x = cCFRInternationalCruiseCIs$date[startInt:endInt],
       y = cCFRInternationalCruiseCIs$ci_mid[startInt:endInt],
       li = cCFRInternationalCruiseCIs$ci_low[startInt:endInt],
       ui = cCFRInternationalCruiseCIs$ci_high[startInt:endInt],
       xlab = "Date",
       ylab = "cCFR (%)",
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = cCFRInternationalCruiseCIs$date[startInt:endInt],
      cCFRInternationalCruiseCIs$ci_mid[startInt:endInt], col = rgb(0.2, 0.8, 0.6))
grid(ny = NULL,
     nx = 0,
     col = rgb(0.9,0.9,0.9),
     lty = "solid")
par(xpd=TRUE)
text(cCFRInternationalCruiseCIs$date[startInt] + 0.4 , 29, "B", cex = 1.5)



par(xpd=FALSE)
plot(x = internationalCruiseData$date, 
     internationalCruiseData$new_cases,
     xlab = "Date", 
     ylab = "Incidence of confirmed cases",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(x = internationalCruiseData$date, internationalCruiseData$new_cases, col = "green")
par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
grid(ny = NULL, 
     nx = 0, 
     col = rgb(0.9,0.9,0.9),
     lty = "solid")
par(xpd=TRUE)
text(internationalCruiseData$date[1] + 0.45, 200, "C", cex = 1.5)


par(xpd=FALSE)
plot(x = internationalCruiseData$date, 
     internationalCruiseData$new_deaths, 
     xlab = "Date", 
     ylab = "Incidence of deaths",
     cex.lab = 1.4,
     cex.axis = 1.4)
lines(x = internationalCruiseData$date, internationalCruiseData$new_deaths, col = "red")
par(mar=c(3,4,1,1),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(internationalCruiseData$date[1], 10,
       legend=c("new international cases", "new international deaths"), 
       col=c("red", "blue"), 
       lty=1:2,
       cex=1.4)
par(xpd=TRUE)
text(internationalCruiseData$date[1] + 0.45 , 3.8, "D", cex = 1.5)



#### plotting Christian's data

dev.off()

layout(matrix(c(1,
                2,
                3),
              nrow=3, byrow=TRUE))




end4 <- length(nCFRChristianCIs$date) # 
y4lim <- 20


#start4 <- 50

par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
plotCI(x=nCFRChristianCIs$date[1:end4],
       y=nCFRChristianCIs$ci_mid[1:end4],
       li=nCFRChristianCIs$ci_low[1:end4],
       ui=nCFRChristianCIs$ci_high[1:end4],
       xlab = "Date",
       ylab = "CFR (%)", 
       #yaxt='n',
       ylim=c(0, y4lim),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = nCFRChristianCIs$date[1:end4], nCFRChristianCIs$ci_mid[1:end4], col = rgb(0.8, 0.2, 0.6))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")

par(new=T)

par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
plotCI(x=cCFRChristianCIs$date[1:end4],
       y=cCFRChristianCIs$ci_mid[1:end4],
       li=cCFRChristianCIs$ci_low[1:end4],
       ui=cCFRChristianCIs$ci_high[1:end4],
       xlab = "Date",
       ylab = "CFR (%)",
       ylim=c(0, y4lim),
       cex.lab = 1.4,
       cex.axis = 1.4)
lines(x = cCFRChristianCIs$date[1:end4], cCFRChristianCIs$ci_mid[1:end4], col = rgb(0.2, 0.8, 0.6))
#grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(cCFRChristianCIs$date[11], y4lim, 
       legend=c("nCFR using Christian's method",
                "cCFR using Christian's method"), 
       col=c(rgb(0.8, 0.2, 0.6), rgb(0.2, 0.8, 0.6)),
       lty=1:1, 
       cex=1.3)
par(xpd=TRUE)
text(cCFRChristianCIs$date[1],18, cex = 2 ,"A")

par(xpd=FALSE)
plot(x = christiansData$date,
     christiansData$new_cases, 
     xlab = "Date",
     ylab = "Incidence (inferred)", 
     cex.lab = 1.4,
     cex.axis = 1.4,
     ylim = c(0,20))
lines(x = christiansData$date, christiansData$new_cases, col = "green")
par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
text(allTogetherInferred$date[1], 18, "B")

#mtext(side = 4, line = 3, 'Incidence (real)')


par(xpd=FALSE)
plot(x = christiansData$date, 
     christiansData$new_deaths,
     xlab = "Date",
     ylab="Incidence of deaths",
     cex.lab = 1.4,
     cex.axis = 1.4,
     ylim = c(0,2))
#title(ylab=, line=-1)
lines(x = christiansData$date, christiansData$new_deaths, col = "red")
par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend(christiansData$date[1], 32500, 
       legend=c("new cases (in Wuhan)", 
                "new deaths (in Wuhan)"),
       col=c("red", "blue"), lty=1:2,
       cex=1.3)
par(xpd=TRUE)
text(christiansData$date[1] , 78, "C")



#dev.off()





