library(readxl)
xl_data <- "/Users/nchapm742/Documents/MSAS/MAT 8452/Final Project/MAT_8452_DataSets.xlsx"
excel_sheets(path = xl_data)


two_sample_data <- read_excel(path = xl_data, sheet = "Two-Sample")
k_sample_data <- read_excel(path = xl_data, sheet = "Robust Regression")
association_data <- read_excel(path = xl_data, sheet = "Association")
bootstrap_data <-read_excel(path = xl_data, sheet = "Bootstrap")
contingency_data <-read_excel(path=xl_data, sheet='Contingency')
robust_data <- read_excel(path=xl_data, sheet="Robust Regression")



##########################################
############# Two Sample #################
##########################################
library(snpar)
library(clinfun)
library(jmuOutlier) 

set.seed(1)
x <- two_sample_data$`More than One Language`
x<-x[!is.na(x)]
x <- sample(x,15)
y <- two_sample_data$`One Language`
y<-y[!is.na(y)]
y <- sample(y,15)

group = gl(2,15)




wilcox.test(x,y)$p.value
ns.test(c(x,y),group)$p.value
ansari.test(x,y)$p.value
ks.test(x,y)$p.value
t.test(x,y,paired = FALSE)$p.value


values=c(x,y)
type=rep(c('More than One Language','One Language'),c(length(x),length(y)))

boxplot(values~type,xlab='Type',ylab='Revenue')



##########################################
############# K-Sample ###################
##########################################
set.seed(0)
action <- k_sample_data[k_sample_data$main_genre == 'Action',]$ROI
comedy <- k_sample_data[k_sample_data$main_genre == 'Comedy',]$ROI
drama <- k_sample_data[k_sample_data$main_genre == 'Drama',]$ROI
fantasy <- k_sample_data[k_sample_data$main_genre == 'Fantasy',]$ROI
horror <- k_sample_data[k_sample_data$main_genre == 'Horror',]$ROI
mystery <- k_sample_data[k_sample_data$main_genre == 'Mystery',]$ROI
romance <- k_sample_data[k_sample_data$main_genre == 'Romance',]$ROI
thriller <- k_sample_data[k_sample_data$main_genre == 'Thriller',]$ROI

action_samp <- sample(action,15)
comedy_samp <- sample(comedy,15)
drama_samp <- sample(drama,15)
fantasy_samp <-sample(fantasy,15)
horror_samp <- sample(horror,15)
romance_samp <-sample(romance,15)
thriller_samp <- sample(thriller ,15)
  


ROI=c(action_samp,comedy_samp,drama_samp,fantasy_samp,horror_samp,romance_samp,
       thriller_samp)
Genres=rep(c('Action','Comedy','Drama','Fantasy','Horror','Romance','Thriller')
           ,c(15,15,15,15,15,15,15))
groups=rep(1:7,c(15,15,15,15,15,15,15))

boxplot(ROI~Genres) #Not consistent with a shift model as the 
                    #spread appears to vary widely among the genres.


kruskal.test(ROI,groups) ## May not be appropriate here as the shapes vary widely. 
jonckheere.test(ROI, groups,alternative = 'two.sided') ## Not Appropriate Here
perm.f.test(ROI,groups)
test = aov(ROI~factor(groups))
summary(test)



##########################################
############# Association ################
##########################################

library(jmuOutlier) 


as.numeric(association_data$rev_sample)

revenue <- as.numeric(association_data$rev_sample)
runtime <- as.numeric(association_data$runtime)

cor.test(runtime,revenue,method='spearman')
cor.test(runtime,revenue,method='kendall')
perm.cor.test(runtime,revenue,method='pearson') 
cor.test(runtime,revenue,method='pearson')


hist(robust_data$revenue)

plot(revenue,runtime)

cor.test(robust_data$revenue, robust_data$runtime,method='kendall')
cor.test(robust_data$revenue, robust_data$runtime,method='pearson')
cor.test(robust_data$revenue, robust_data$runtime,method='spearman')

cor(robust_data$revenue, robust_data$runtime)

plot(robust_data$revenue, robust_data$runtime)


par(mfrow=c(1,2))

hist(revenue)
hist(runtime)

hist(robust_data$revenue)
hist(robust_data$runtime)



##########################################
############# Bootstrap ##################
##########################################


set.seed(0)
samp <- sample(bootstrap_data$revenue,15)
n=length(samp)
B=1000 #Number of bootstrap samples to draw.
sdvec=double(B) #Saving the bootstrap standard deviations.
meanvec=double(B) #Saving the bootstrap sample means.
varcoefvec=double(B) #Saving the bootstrap sample medians.
sd=sd(samp) #Observed standard deviation.
varcoef = sd(samp) / mean(samp)
for (brun in 1:B){
  bsamp=sample(samp,n,replace=T) #Sample with replacement from the sample.
  meanvec[brun]=mean(bsamp)
  sdvec[brun]=sd(bsamp)
  varcoefvec[brun] = sd(bsamp) / mean(bsamp)
}

hist(meanvec) #Plotting the bootstrap means
abline(v=mean(samp))

hist(sdvec) #Plotting the bootstrap standard deviations.
abline(v=sd)

hist(varcoefvec) #Plotting the bootstrap variance coefficients
abline(v=varcoef)



#Now studying the mean

ehat=mean(meanvec) #Mean of the bootstrap values.
varhat=var(meanvec) #Estimated variance.
msehat=mean((meanvec-mean(samp))^2) #Estimated MSE.
se=sqrt(msehat) #Bootstrap SE.
bhat=ehat-mean(samp) #Estimated bias.
ehat
varhat
msehat
se
bhat

#Now studying the standard deviation.

ehat=mean(sdvec) #Mean of the bootstrap values.
varhat=var(sdvec) #Estimated variance.
msehat=mean((sdvec-sd)^2) #Estimated MSE.
se=sqrt(msehat) #Bootstrap SE.
bhat=ehat-sd #Estimated bias.
ehat
varhat
msehat
se
bhat



#Now studying the variance coefficient

ehat=mean(varcoefvec) #Mean of the bootstrap values.
varhat=var(varcoefvec) #Estimated variance.
msehat=mean((varcoefvec-varcoef)^2) #Estimated MSE.
se=sqrt(msehat) #Bootstrap SE.
bhat=ehat-varcoef #Estimated bias.
ehat
varhat
msehat
se
bhat

#95% CI for the population mean.

quantile(meanvec,c(0.025,0.975))

hist(meanvec)
abline(v=quantile(meanvec,0.025))
abline(v=quantile(meanvec,0.975))

#95% for the population standard deviation.

quantile(sdvec,c(0.025,0.975))
hist(sdvec)
abline(v=quantile(sdvec,0.025))
abline(v=quantile(sdvec,0.975))

#95% for the variance coefficient

quantile(varcoefvec,c(0.025,0.975))
hist(varcoefvec)
abline(v=quantile(varcoefvec,0.025))
abline(v=quantile(varcoefvec,0.975))


##########################################
############# Contingency ################
##########################################
set.seed(0)

cont_df<-as.data.frame(contingency_data)


x <- contingency_data$white
x<-x[!is.na(x)]
x <- sample(x,65)
y <- contingency_data$non_white
y<-y[!is.na(y)]
y <- sample(y,25)


x_freq<-as.data.frame(table(x))
y_freq<-as.data.frame(table(y))

colnames(x_freq)<-c('genre','white_leads')
colnames(y_freq)<-c('genre','nonwhite_leads')

full_freq<-merge(x_freq,y_freq,by='genre',all.x=TRUE,all.y=TRUE)
full_freq$white_leads<-ifelse((is.na(full_freq$white_leads)),0,full_freq$white_leads)
full_freq$nonwhite_leads<-ifelse((is.na(full_freq$nonwhite_leads)),0,full_freq$nonwhite_leads)
                                                            

chisq.test(full_freq$white_leads, full_freq$nonwhite_leads) #Using the asymptotic approximation.

chisq.test(full_freq$white_leads, full_freq$nonwhite_leads,simulate.p.value=T) #Doing the permutation test.


##########################################
############# Regression #################
##########################################
library(MASS)

robust_data <- as.data.frame(robust_data)
attach(robust_data)
robust_data$rev_scaled <- robust_data$revenue / 1000000000
robust_data$bud_scaled <- robust_data$budget / 1000000000

plot(robust_data$budget, robust_data$revenue)
fit_lm <- lm(robust_data$rev_scaled ~ robust_data$bud_scaled + factor(robust_data$animated) +
                robust_data$runtime +
               robust_data$language_count + factor(robust_data$main_genre))

fit_log_lm <- lm(log(robust_data$rev_scaled) ~ log(robust_data$bud_scaled)+
                   robust_data$runtime +
                   robust_data$language_count + factor(robust_data$main_genre))

summary(fit_lm)
summary(fit_log_lm)

plot(fit_log_lm,1)


fit_rlm <- rlm(robust_data$rev_scaled ~ robust_data$bud_scaled +
                robust_data$runtime + robust_data$language_count)

fit_log_rlm <- rlm(log(robust_data$rev_scaled) ~ log(robust_data$bud_scaled) +
                 log(robust_data$runtime) + robust_data$language_count + factor(robust_data$main_genre))

fit_log_rlm <- rlm(log(robust_data$rev_scaled) ~ robust_data$bud_scaled +
                     robust_data$runtime + robust_data$language_count )

summary(fit_rlm)
summary(fit_log_rlm)

plot(fit_log_lm)




hweights <- data.frame(resid = fit_log_rlm$resid, weight = fit_log_rlm$w)
hweights2 <- hweights[order(fit_log_rlm$w), ]
hweights2[1:5, ]


