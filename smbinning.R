
# Load package and its data
library(smbinning) 
data(chileancredit)
str(chileancredit) # Quick description of the data
View(chileancredit)

# Delete indeterminates
chileancredit=subset(chileancredit,(FlagGB==1 | FlagGB==0))

# table(chileancredit$FlagGB) # Tabulate target variable
# table(chileancredit$FlagSample) # 2 random samples (1:75%, 0:25%) 

# Missing Data
library(VIM)
aggr(chileancredit,col=c("white","gray"),numbers=TRUE, prop=FALSE) # Plot pattern (Numbers)

# Outliers
boxplot(chileancredit$TOB,
        horizontal=T, frame=F, col="lightgray",main="Distribution")

boxplot(chileancredit$TOB~chileancredit$FlagGB,
        horizontal=T, frame=F, col="lightgray",main="Distribution")


# Training and testing samples
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Optimal Binning ----------------------------------------------------------
result1=
  smbinning(df=chileancredit.train,y="FlagGB",x="TOB",p=0.05) # Run and save
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree from partykit

# Relevant plots (2x2 Page)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
boxplot(chileancredit.train$TOB~chileancredit.train$FlagGB,
        horizontal=T, frame=F, col="lightgray",main="Distribution")
mtext("Time on Books (Months)",3)
smbinning.plot(result,option="dist",sub="Time on Books (Months)")
smbinning.plot(result,option="badrate",sub="Time on Books (Months)")
smbinning.plot(result,option="WoE",sub="Time on Books (Months)")

# SQL Code after binning a numeric variable ---------------------------------
smbinning.sql(result)

# Generate variable after binning -------------------------------------------
chileancredit=smbinning.gen(chileancredit, result1, chrname = "gTOB") # Its a factor variable
View(chileancredit)

# Customized Binning --------------------------------------------------------
# Remove exclusions from chileancredit dataset
TOB.train=
  subset(chileancredit,(FlagSample==1 & (FlagGB==1 | FlagGB==0)), select=TOB)
# Percentiles of 20%
TOB.Pct20=quantile(TOB.train, probs=seq(0,1,0.2), na.rm=T)
TOB.Pct20.Breaks=as.vector(quantile(TOB.train, probs=seq(0,1,0.2), na.rm=T))
Cuts.TOB.Pct20=TOB.Pct20.Breaks[2:(length(TOB.Pct20.Breaks)-1)]


# Package application and results
result1=
  smbinning.custom(df=chileancredit.train,
                   y="FlagGB",x="TOB",cuts=Cuts.TOB.Pct20) # Run and save
result$ivtable # Tabulation and Information Value

# Factor Variable Application -----------------------------------------------
result1=
  smbinning.factor(df=chileancredit.train,y="FlagGB",x="IncomeLevel")
result$ivtable

# SQL Code after binning a factor variable ----------------------------------
smbinning.sql(result)

# Generate variable after binning factor ------------------------------------
chileancredit=smbinning.factor.gen(chileancredit, result1, chrname = "gInc")
View(chileancredit)

# Exploratory Data Analysis -------------------------------------------------
smbinning.eda(df=chileancredit.train)$eda # Table with basic statistics
smbinning.eda(df=chileancredit.train)$edapct # Table with basic percentages

# Information Value for all variables in one step ---------------------------
smbinning.sumiv(df=chileancredit.train,y="FlagGB") # IV for eache variable

# Plot IV for all variables -------------------------------------------------
sumivt=smbinning.sumiv(chileancredit.train,y="FlagGB")
sumivt # Display table with IV by characteristic
par(mfrow=c(1,1))
smbinning.sumiv.plot(sumivt,cex=1) # Plot IV summary table

# Another vable
result1=
  smbinning.factor(df=chileancredit.train,y="FlagGB",x="MaxDqBin01")
result$ivtable
chileancredit=smbinning.factor.gen(chileancredit, result1, chrname = "gDq")

# Training and testing samples (Again)
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

View(chileancredit)

model=glm(FlagGB~gTOB+gInc+gDq,chileancredit.train,family="binomial")
summary(model)

# Remove Income
model=glm(FlagGB~gTOB+gDq,chileancredit.train,family="binomial")
summary(model)

# Training and testing samples (Again)
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Add prediction
chileancredit.test=cbind(chileancredit.test,pgood=predict(model,chileancredit.test,"response"))
# chileancredit=cbind(chileancredit,pgood=predict(model,df,"response"))

# Model evaluation AUC
library(pROC)
auc(chileancredit.test$FlagGB,chileancredit.test$pgood)

predictions1 <- ifelse(chileancredit.test$pgood >= 0.93 ,1,0)
table(predictions1, chileancredit.test$FlagGB)