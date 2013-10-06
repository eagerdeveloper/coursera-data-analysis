loansData <- read.csv("../../data/loansData.csv")
save(loansRaw, dateDownloaded, file = "../../data/loansData.rda")

# Clean up the data
ld <- data.frame(loansData)
ld$Interest.Rate <- as.numeric(gsub("%", "", loansData$Interest.Rate))
ld$Debt.To.Income.Ratio <- as.numeric(gsub("%", "", loansData$Debt.To.Income.Ratio))

as.factor(ld$FICO.Range)


### P VALUE DETERMINATION ######################################################
# Calculate linear model for each vs interest rate and find p-value

lm <- lm(ld$Interest.Rate ~ ld$Amount.Requested)  
summary(lm)  # P-value <2e-16 - CONSIDER
smoothScatter(ld$Interest.Rate, log10(ld$Amount.Requested))
hist(ld$Amount.Requested, col="blue", breaks=100)
hist(log10(ld$Amount.Requested), col="blue", breaks=100)

lm <- lm(ld$Interest.Rate ~ ld$Amount.Funded.By.Investors)
summary(lm)  # P-value <2e-16 - CONSIDER

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$Loan.Length))
summary(lm)  # P-value < 2.2e-16 - CONSIDER

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$Loan.Purpose))
summary(lm)  # P-value 0.0019 - CONSIDER

lm <- lm(ld$Interest.Rate ~ ld$Debt.To.Income.Ratio)
summary(lm)  # P-value <2e-16 - CONSIDER

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$State))
summary(lm)  # P-value 0.679 - NO

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$Home.Ownership), na.action=na.exclude)
summary(lm)  # P-value 0.000174 - CONSIDER
         
lm <- lm(ld$Interest.Rate ~ ld$Monthly.Income)
summary(lm)  # P-value 0.5396  - NO

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$FICO.Range))
summary(lm)  # P-value <2e-16 - CONSIDER

lm <- lm(ld$Interest.Rate ~ ld$Open.CREDIT.Lines)
summary(lm)  # P-value 6.17e-06 - CONSIDER

lm <- lm(ld$Interest.Rate ~ ld$Revolving.CREDIT.Balance)
summary(lm)  # P-value 0.00225 - NO
anova(lm)

lm <- lm(ld$Interest.Rate ~ ld$Inquiries.in.the.Last.6.Months)
summary(lm)  # P-value <2e-16 - CONSIDER
anova(lm)

lm <- lm(ld$Interest.Rate ~ as.numeric(ld$Employment.Length))
summary(lm)  # P-value 0.5303 - NO



# Round 2 - Find the confounders

lm <- lm(as.numeric(ld$FICO.Range) ~ ld$Amount.Requested)
summary(lm)  # P-value 3.69e-05 - CONFOUNDER

lm <- lm(as.numeric(ld$FICO.Range) ~ ld$Amount.Funded.By.Investors)
summary(lm)  # P-value 0.00024 - CONFOUNDER

lm <- lm(as.numeric(ld$FICO.Range) ~ ld$Loan.Length)
summary(lm)  # P-value 0.54 - NO

lm <- lm(as.numeric(ld$FICO.Range) ~ ld$Debt.To.Income.Ratio)
summary(lm)  # P-value <2e-16 - CONFOUNDER

lm <- lm(as.numeric(ld$FICO.Range) ~ as.numeric(ld$Home.Ownership))
summary(lm)  # P-value 2.7e-15 - CONFOUNDER

lm <- lm(as.numeric(ld$FICO.Range) ~ as.numeric(ld$Open.CREDIT.Lines))
summary(lm)  # P-value 7.47e-06 - CONFOUNDER

lm <- lm(as.numeric(ld$FICO.Range) ~ as.numeric(ld$Inquiries.in.the.Last.6.Months))
summary(lm)  # P-value 3.97e-06 - CONFOUNDER



### P VALUE DETERMINATION ######################################################

plot(as.numeric(ld$FICO.Range) ~ ld$Interest.Rate, col="blue")
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Amount.Requested)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Amount.Funded.By.Investors)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Loan.Length)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Loan.Purpose)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Debt.To.Income.Ratio)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$State)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Home.Ownership)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Monthly.Income)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$FICO.Range)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Open.CREDIT.Lines)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Revolving.CREDIT.Balance)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Inquiries.in.the.Last.6.Months)))
anova(lm(ld$Interest.Rate ~ as.numeric(ld$Employment.Length)))


par(mfrow=c(1,1))
plot(ld$Interest.Rate, as.factor(ld$FICO.Range), col="blue")

lmNoAdjust <- lm(ld$Interest.Rate ~ as.numeric(ld$FICO.Range), na.action=na.exclude)
par(mfrow=c(1,3))
plot(as.numeric(ld$FICO.Range), lmNoAdjust$residuals, col=ld$Amount.Requested)
plot(as.numeric(ld$FICO.Range), lmNoAdjust$residuals, col=ld$Amount.Funded.By.Investors)
plot(as.numeric(ld$FICO.Range), lmNoAdjust$residuals, col=ld$Loan.Length)

abline(lmFinal$coeff[1], lmFinal$coeff[2], col="red")
summary(lmNoAdjust)


lmFinal <- lm(ld$Interest.Rate 
              ~   
                as.numeric(ld$FICO.Range)
              + ld$Amount.Requested
              + ld$Amount.Funded.By.Investors
              + as.numeric(ld$Loan.Length)
              + ld$Debt.To.Income.Ratio
              + as.numeric(ld$Home.Ownership)
              + as.numeric(ld$Open.CREDIT.Lines)
              + as.numeric(ld$Inquiries.in.the.Last.6.Months) 
)

summary(lmFinal)
confint(lmFinal)



library(RColorBrewer)

## Set up a function that makes colors prettier
mypar <- function(a = 1, b = 1, brewer.n = 8, brewer.name = "Dark2", ...) {
  par(mar = c(2.5, 2.5, 1.6, 1.1), mgp = c(1.5, 0.5, 0))
  par(mfrow = c(a, b), ...)
  palette(brewer.pal(brewer.n, brewer.name))
}

## Set size of axes
cx = 1.3


## Save figure to pdf file
pdf(file = "../../figures/finalLoanfigure.pdf", height = 4, width = 3 * 4)

mypar(mfrow = c(1, 3))

hist(as.numeric(ld$Interest.Rate), breaks = 100, col = 1, xlab = "FICO Range", ylab = "Frequency", 
     main = "", cex.axis = cx, cex.lab = cx)
plot(as.numeric(ld$FICO.Range), lmNoAdjust$residuals, col = ld$Home.Ownership, pch = 19, 
     xlab = "FICO Range", ylab = "No Adjustment Residuals", cex.axis = cx, 
     cex.lab = cx, type="p")
plot(as.numeric(ld$FICO.Range), c(lmFinal$residuals,NA,NA), col = ld$Home.Ownership, pch = 19, xlab = "FICO Range", 
     ylab = "Full Model Residuals", cex.axis = cx, cex.lab = cx)

?plot

dev.off()



summary(lmFinal)
anova(lmFinal)


hist(as.numeric(ld$FICO.Range))
plot(as.factor(ld$FICO.Range), col="blue")
hist(as.numeric(ld$Interest.Rate))

as.numeric(ld$FICO.Range)

residuals
plot(ld$Interest.Rate, c(lmFinal$residuals,NA,NA), col="blue")


length(ld$Interest.Rate)

length(lmFinal$residuals)

lmFinal <- lm(ld$Interest.Rate 
                      ~   
                as.numeric(ld$FICO.Range)
                     + ld$Amount.Requested * as.numeric(ld$FICO.Range)
                     + ld$Amount.Funded.By.Investors * as.numeric(ld$FICO.Range)
                       + as.numeric(ld$Loan.Length) * as.numeric(ld$FICO.Range)
                        + ld$Debt.To.Income.Ratio*as.numeric(ld$FICO.Range)
                       + as.numeric(ld$Home.Ownership)*as.numeric(ld$FICO.Range)
                       + ld$Open.CREDIT.Lines*as.numeric(ld$FICO.Range)
                       + ld$Inquiries.in.the.Last.6.Months*as.numeric(ld$FICO.Range)
                        , na.action=na.omit)

anova(lmFinal)

summary(lmFinal)$coeff

confint(lmFinal, level=0.95)
abline(lmFinal, col="red")


abline(lmFinal$coeff[1], lmFinal$coeff[2], col="red")


ld[is.na(ld$Amount.Requested),c("Amount.Requested")] #None
ld[is.na(ld$Amount.Funded.By.Investors),c("Amount.Funded.By.Investors")] #None
ld[is.na(ld$Debt.To.Income.Ratio),c("Debt.To.Income.Ratio")] #None
ld[is.na(ld$Home.Ownership),c("Amount.Funded.By.Investors")] #None
ld[is.na(ld$FICO.Range),c("FICO.Range")] #None
ld[is.na(ld$Open.CREDIT.Lines),c("Monthly.Income")] # 1
ld[is.na(ld$Inquiries.in.the.Last.6.Months),
   c("Inquiries.in.the.Last.6.Months")] # 1
              
              
par(mfrow=c(1,1))
plot(lmFinal)
lines(ld$Interest.Rate, lmFinal$fitted, col="red", lwd=3)
plot(ld$Interest.Rate, lmFinal$residuals, col="blue", pch=19)
abline(lmFinal, col="red")


InterestRate <- lmFinal$coeff[1] +  lmFinal$coeff[2]*as.numeric(ld$FICO.Range)
smoothScatter(ld$FICO.Range, InterestRate, col="blue")



full.model <- lm(ld$Interest.Rate ~ as.numeric(ld$FICO.Range) + ld$Amount.Requested 
                 + ld$Amount.Funded.By.Investors 
                 + as.numeric(ld$Loan.Length))
reduced.model <- step(full.model, direction="backward")


start.model <- lm(ld$Interest.Rate ~ 1)

forward.model <- step(start.model, direction ="forward", 
                      scope = (~as.numeric(ld$FICO.Range) + ld$Amount.Requested 
                               + ld$Amount.Funded.By.Investors 
                               + as.numeric(ld$Loan.Length)))

lmFinal <- lm(ld$Interest.Rate ~   as.numeric(ld$FICO.Range))
