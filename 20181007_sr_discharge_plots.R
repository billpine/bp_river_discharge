
#Run it line by line. Note in the read and prepare data section it 
#will take a few seconds for each line to load the data from the server. 
#Don't run the code for temperature as your gauge doesn't have temperature. 
#It might have rainfall however.




################################################################################
#
# this code reads daily water guage data from a USGS site, computes the weekly
# composite mean discharge, the weekly climatology over the entire time frame,
# and anomolies based on the the time span of climatoloty.  Requires the
# waterData package.  Visit the following pages for site numbers and metadata
#
# sites:       http://maps.waterdata.usgs.gov/mapper/
# stat codes:  http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table
# code codes:  http://nwis.waterdata.usgs.gov/usa/nwis/pmcodes     (choose physical)
#
# Wilcox Suwannee  gauge 02323500
#
################################################################################
#SETUP
rm(list=ls());gc();
graphics.off();windows(record=T)
library("waterData")
library("hydroTSM")
library("zoo")
#setwd("~/Git/transect/data")

#station to analyze
station = '02323500'   

################################################################################ 
#
#                     READ AND PREPARE DATA
#
################################################################################ 
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1941-10-01") 
#ok looks like temp code is wrong for this station, maybe rainfall is available?

#get some date components
dis$year    = as.numeric(strftime(dis$dates,format="%Y"))
dis$month   = as.numeric(strftime(dis$dates,format="%m")) 

#make dataset from epochs, 
disE  = dis[dis$dates>='1941-10-01' & dis$dates<='2019-07-31',]  

 

#get monthly sum, mean, sd, and var
#discharge
disE.mo  = aggregate(val~month+year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE.mo  = do.call('data.frame',disE.mo)
names(disE.mo)[3:6] = c('avg','sd','var','sumflow') 
disE.mo$yrmo = disE.mo$year+(disE.mo$month-0.5)/12       


#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE.yr  = aggregate(val~year,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE.yr  = do.call('data.frame',disE.yr)
names(disE.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE.all  = aggregate(val~staid,data=disE,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE.all  = do.call('data.frame',disE.all)
names(disE.all)[2:4] = c('avg','sd','var')  




#make some time series objects
disE.zoo    = zoo(disE$val,disE$dates)  

disE.mo.ts  = ts(disE.mo$avg,start=c(1941,1),end=c(2019,5),frequency=12)
disE.mo.sum.ts  = ts(disE.mo$sumflow,start=c(1941,1),end=c(2019,5),frequency=12)
disE.yr.ts  = ts(disE.yr$avg,start=1941,end=2019,frequency=1)

# #fill the gaps in monthly temperature time series - need this for some of the time series analysis
# tempE.mo.zoo = na.approx(tempE.mo.ts)                     #this doeas linear interpolates over gaps
# tempE.seasonal = decompose(tempE.mo.zoo)$seasonal         #this gets the seasonal component
# tempE.mo.filled = ifelse(is.na(tempE.mo.ts),tempE.mo.zoo+tempE.seasonal,tempE.mo.ts)  #now add the seasonal component to interpolated
# tempE.mo.filled  = ts(tempE.mo.filled,start=c(1950,1),end=c(2018,12),frequency=12)
# plot(tempE.mo.filled,col='green',main='Filled Temperature Trend',ylab='Temperature (deg C)')
# lines(tempE.mo.zoo,col='red')  
# lines(tempE.mo.ts,col='black')
# legend('topleft',legend=c('available data','linear interpolation','interpolation + seasonal component'),bty='n',
#        text.col=c('black','red','green'))
# plot(tempE.mo.filled)
# 





################################################################################ 
#
#                     PLOT DATA
#
################################################################################ 
#---------------------------- basic time series plots --------------------------------

#plot all available daily data, discharge and temp
par(mfrow=c(2,1),oma=c(0,0,1,0),mar=c(4,4,4,1))
plot(dis$dates,dis$val,type='l',main='Discharge',xlab='Date',ylab='cf/s')


title(stinfo[1,2],outer=T)

#plot daily data from epochs, discharge and temp
par(mfrow=c(2,1),oma=c(0,0,1,0),mar=c(4,4,4,1))
plot(disE$dates,disE$val,type='l',main='Discharge',xlab='Date',ylab='cf/s')
title(stinfo[1,2],outer=T)

#--------------------summary plots from hydroTSM package------------------------
#discharge data, 1941-2018
hydroplot(disE.zoo,FUN=mean,ylab='Q',var.unit='cf/s')
title(paste(stinfo[1,2],': 1941-2019',sep=''),outer=T)

#--------------------------- duration curves -----------------------------------
#simply converting each vector to 'ts' allows to cbind without recycling values in shorter vector

dis.fdc = as.data.frame(cbind(ts(disE$val[disE$year<2010]),ts(disE$val[disE$year>=2010])))
par(mfrow=c(1,1))
fdc(dis.fdc,ylab='Discharge (cf/s)',leg.txt=c('1941-2009','2010-2019'),cex.axis=1)
title(stinfo[1,2],outer=T)


#---------------------------- quartile plots -----------------------------------

################################################################################ 
#
#                     LOESS - local regression, smoothing
#
################################################################################ 
#this is used to graphically evaluate whether the time series data show monotonic
#or piecewise trends

#Discharge data - cf/s
par(mfrow=c(2,2),mar=c(4,4,2,2),oma=c(0,0,3,0))
plot(disE$dates,disE$val,type='l',xlab='Date',ylab='Discharge (cf/s)',main='daily data')
lines(lowess(disE$dates,disE$val),col='red')  
plot(disE.mo.ts,xlab='Date',ylab='Discharge (cf/s)',main='monthly means')
lines(lowess(disE.mo.ts),col='red')   
plot(disE.mo.sum.ts,xlab='Date',ylab='Discharge (cf/s)',main='monthly sum')
lines(lowess(disE.mo.sum.ts),col='red')   
plot(disE.yr$year,disE.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',main='annual means')
lines(lowess(disE.yr$year,disE.yr$avg),col='red')
abline(lm(disE.yr$avg~seq(1950,2018,1)))
title(stinfo[1,2],outer=T)



########
####BELOW ARE THE STANDARD 4 panel plots you make
####The first group is for water epoch 1, 1941-2019
####The second group is for water epoch 2,2010-2019 

##change the xlim to whatever years you want.

disE.yr$CV  = (disE.yr$sd/disE.yr$avg)*100

windows(record=T)

tiff(filename="data_output/big_bend_trends/Figure1.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)

#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

par(mfrow=c(2,2))
plot(disE.yr$year,disE.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Mean daily discharge by year', ylim=c(0,21000), xlim=c(1941,2019))
lines(lowess(disE.yr$year,disE.yr$avg),col='red', lwd=4)
abline(h=disE.all[2], col="blue", lwd=4, lty=2)
#abline(lm(disE.yr$avg~seq(1950,2018,1)))
title(stinfo[1,2],outer=T, line = -1.0)

plot(disE.yr$year,disE.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge variance by year', ylim=c(0,135000000), xlim=c(1941,2019))
lines(lowess(disE.yr$year,disE.yr$var),col='red', lwd=4)
abline(h=disE.all[4], col="blue", lwd=4, lty=2)
#abline(lm(disE.yr$avg~seq(1950,2018,1)))
title(outer=T)

plot(disE.yr$year,disE.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge CV by year', ylim=c(0,100), xlim=c(1941,2019))
lines(lowess(disE.yr$year,disE.yr$CV),col='red', lwd=4)
abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
title(outer=T)

plot(disE.yr$year,disE.yr$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Total annual discharge by year', ylim=c(0,8000000), xlim=c(1941,2019))
lines(lowess(disE.yr$year,disE.yr$sum),col='red', lwd=4)
abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)

title(outer=T)

# write.table(disE.yr,file="data_output/discharge.txt", sep = ",", quote = FALSE, row.names = T)




########
####REMAKE PLOTS for just using data up until 2018 ONLY
####REMOVE TITLES - add in letters for each panel
####The first group is for water epoch 1, 1941-2018
####The second group is for water epoch 2,2010-2018

#remove 2019
library(ggplot2)
disE.yr2 <- disE.yr[-79,]

disE.yr2$CV  = (disE.yr2$sd/disE.yr2$avg)*100

tiff( file= "data_output/big_bend_trends/newly_updated_figures/Fig4Discharge19412018.tiff", res=300, width= 3200, height= 3200, units= "px")
par(mfrow=c(3,1), mar= c(5.1, 4.7,4.1,2.1))

plot(disE.yr2$year,disE.yr2$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,21000), xlim=c(1941,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE.yr2$year,disE.yr2$avg),col='red', lwd=4)
abline(h=mean(disE.yr2$avg), col="blue", lwd=4, lty=2)
#abline(h=disE.all[2], col="blue", lwd=4, lty=2)
mtext("A", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)

plot(disE.yr2$year,disE.yr2$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,135000000), xlim=c(1941,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE.yr2$year,disE.yr2$var),col='red', lwd=4)
#abline(h=disE.all[4], col="blue", lwd=4, lty=2)
abline(h=mean(disE.yr2$var), col="blue", lwd=4, lty=2)
mtext("B", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)

plot(disE.yr2$year,disE.yr2$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,100), xlim=c(1941,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE.yr2$year,disE.yr2$CV),col='red', lwd=4)
#abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
abline(h=mean(disE.yr2$CV), col="blue", lwd=4, lty=2)
mtext("C", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)
dev.off()



#plot(disE.yr2$year,disE.yr2$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,8000000), xlim=c(1941,2018))
#lines(lowess(disE.yr2$year,disE.yr2$sum),col='red', lwd=4)
#abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)
#mtext("D", side = 3, line = 1, adj = -0.05, font = 2)



###########
#now work with water epoch 2
#get monthly sum, mean, sd, and var
#discharge

windows(record=T)

disE2 = dis[dis$dates>='2010-01-01' & dis$dates<='2019-07-31',]

disE2.mo  = aggregate(val~month+year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE2.mo  = do.call('data.frame',disE2.mo)
names(disE2.mo)[3:6] = c('avg','sd','var','sumflow') 
disE2.mo$yrmo = disE2.mo$year+(disE2.mo$month-0.5)/12       


#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE2.yr  = aggregate(val~year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE2.yr  = do.call('data.frame',disE2.yr)
names(disE2.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE2.all  = aggregate(val~staid,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE2.all  = do.call('data.frame',disE2.all)
names(disE2.all)[2:4] = c('avg','sd','var')  

disE2.yr$CV  = (disE2.yr$sd/disE2.yr$avg)*100


#make some time series objects
disE2.zoo    = zoo(disE2$val,disE2$dates)  

disE2.mo.ts  = ts(disE2.mo$avg,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.mo.sum.ts  = ts(disE2.mo$sumflow,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.yr.ts  = ts(disE2.yr$avg,start=2010,end=2019,frequency=1)


par(mfrow=c(2,2))

tiff(filename="Figure1.tiff", height = 22.7, width = 17.3, units = 'cm', compression = "lzw", res = 500)


windows(record=T)

#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

plot(disE2.yr$year,disE2.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Mean daily discharge by year', ylim=c(0,21000), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$avg),col='red', lwd=4)
abline(h=disE.all[2], col="blue", lwd=4, lty=2) 
#this calls disE because you want the average from full time series

#abline(lm(disE2.yr$avg~seq(1950,2019,1)))
title(stinfo[1,2],outer=T, line = -1.0)

plot(disE2.yr$year,disE2.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge variance by year', ylim=c(0,135000000), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$var),col='red', lwd=4)
abline(h=disE.all[4], col="blue", lwd=4, lty=2)
#abline(lm(disE2.yr$avg~seq(1950,2019,1)))
title(outer=T)

plot(disE2.yr$year,disE2.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Daily discharge CV by year', ylim=c(0,100), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$CV),col='red', lwd=4)
abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
title(outer=T)

plot(disE2.yr$year,disE2.yr$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5,main='Total annual discharge by year', ylim=c(0,8000000), xlim=c(2010,2019))
lines(lowess(disE2.yr$year,disE2.yr$sum),col='red', lwd=4)
abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)

title(outer=T)




###########
# REMAKE PLOTS FOR JUST 2010 - 2018
# take out titles add in letter labels for each panel
#now work with water epoch 2
#get monthly sum, mean, sd, and var
#discharge

disE2 = dis[dis$dates>='2010-01-01' & dis$dates<='2018-12-31',]

disE2.mo  = aggregate(val~month+year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x)))
disE2.mo  = do.call('data.frame',disE2.mo)
names(disE2.mo)[3:6] = c('avg','sd','var','sumflow') 
disE2.mo$yrmo = disE2.mo$year+(disE2.mo$month-0.5)/12       

#get yearly mean, sd, and var and annual sum of discharge
#discharge
disE2.yr  = aggregate(val~year,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T),sum(x,na.rm=T)))
disE2.yr  = do.call('data.frame',disE2.yr)
names(disE2.yr)[2:5] = c('avg','sd','var','sum')                      

#overall mean, sd, var
disE2.all  = aggregate(val~staid,data=disE2,FUN = function(x) c(mean(x,na.rm=T),sd(x,na.rm=T),var(x,na.rm=T)))
disE2.all  = do.call('data.frame',disE2.all)
names(disE2.all)[2:4] = c('avg','sd','var')  

disE2.yr$CV  = (disE2.yr$sd/disE2.yr$avg)*100

#make some time series objects
disE2.zoo    = zoo(disE2$val,disE2$dates)  

disE2.mo.ts  = ts(disE2.mo$avg,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.mo.sum.ts  = ts(disE2.mo$sumflow,start=c(2010,1),end=c(2019,5),frequency=12)
disE2.yr.ts  = ts(disE2.yr$avg,start=2010,end=2019,frequency=1)


tiff( file= "data_output/big_bend_trends/newly_updated_figures/Fig5Discharge20102018.tiff", res=300, width= 3200, height= 3200, units= "px")
par(mfrow=c(3,1), mar= c(5.1, 4.7,4.1,2.1))

plot(disE2.yr$year,disE2.yr$avg,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,21000), xlim=c(2010,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE2.yr$year,disE2.yr$avg),col='red', lwd=4)
abline(h=mean(disE2.yr$avg), col="blue", lwd=4, lty=2)
#abline(h=disE.all[2], col="blue", lwd=4, lty=2) 
#this calls disE because you want the average from full time series
mtext("A", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)

plot(disE2.yr$year,disE2.yr$var,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,135000000), xlim=c(2010,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE2.yr$year,disE2.yr$var),col='red', lwd=4)
abline(h=mean(disE2.yr$var), col="blue", lwd=4, lty=2)
#abline(h=disE.all[4], col="blue", lwd=4, lty=2)
mtext("B", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)


plot(disE2.yr$year,disE2.yr$CV,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c(0,100), xlim=c(2010,2018),cex.lab=2.2, cex.axis=1.5)
lines(lowess(disE2.yr$year,disE2.yr$CV),col='red', lwd=4)
abline(h=mean(disE2.yr$CV), col="blue", lwd=4, lty=2)
#abline(h=((disE.all[3]/disE.all[2])*100), col="blue", lwd=4, lty=2)
mtext("C", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)
dev.off()

#plot(disE2.yr$year,disE2.yr$sum,type='p',xlab='Date',ylab='Discharge (cf/s)',pch=19,cex=1.5, ylim=c#(0,8000000), xlim=c(2010,2018))
#lines(lowess(disE2.yr$year,disE2.yr$sum),col='red', lwd=4)
#abline(h=mean(disE.yr$sum), col="blue", lwd=4, lty=2)
#mtext("D", side = 3, line = 1, adj = -0.05, font = 2, cex = 1.5)



# 
# 
# 
# ################################################################################ 
# #
# #                     PIECEWISE REGRESSION
# #
# ################################################################################ 
# #------------------- manually looping through breakpoints ----------------------
# #this assumes that the segments are discontinuous
# par(mfrow=c(2,2))
# #Annual Mean Discharge..........................................................
# Break = disE.yr$year
# rse   = numeric(length(Break))
# for(i in 1:length(Break)){
#   mod = lm(avg ~ year*(year<Break[i]) + year*(year>=Break[i]),data=disE.yr)
#   rse[i] = summary(mod)[[6]]
# }
# #model with optimum breakpoint
# optbk   = Break[which.min(rse)]
# mod.opt = lm(avg ~ year*(year<optbk) + year*(year>=optbk),data=disE.yr)
# coefs   = coef(mod.opt)
# mod.lm  = lm(avg~year,data=disE.yr)
# anova(mod.lm,mod.opt)
# #plot
# plot(avg~year,data=disE.yr,type='b',main='annual',xlab='Year',ylab='Mean Discharge (cf/s)')
# curve(coefs[1]+coefs[2]*x,add=T,from=optbk,to=tail(Break,1),col='red')
# curve((coefs[1]+coefs[3])+(coefs[2]+coefs[5])*x,add=T,from=head(Break,1),to=optbk,col='red')
# abline(v=optbk,col='gray')
# minor.tick(5)
# plot(Break,rse,type='l',main='annual',xlab='Breakpoint')
# minor.tick(5)    
# 
# #Monthly Mean Discharge......................................................... 
# Break = disE.mo$yrmo
# rse   = numeric(length(Break))
# for(i in 1:length(Break)){
#   mod = lm(avg ~ yrmo*(yrmo<Break[i]) + yrmo*(yrmo>=Break[i]),data=disE.mo)
#   rse[i] = summary(mod)[[6]]
# }
# #model with optimum breakpoint
# optbk   = Break[which.min(rse)]
# mod.opt = lm(avg ~ yrmo*(yrmo<optbk) + yrmo*(yrmo>=optbk),data=disE.mo)
# coefs   = coef(mod.opt)
# mod.lm  = lm(avg~yrmo,data=disE.mo)
# anova(mod.lm,mod.opt)
# #plot
# plot(avg~yrmo,data=disE.mo,type='b',main='monthly',xlab='Month',ylab='Mean Discharge (cf/s)')
# curve(coefs[1]+coefs[2]*x,add=T,from=optbk,to=tail(Break,1),col='red')
# curve((coefs[1]+coefs[3])+(coefs[2]+coefs[5])*x,add=T,from=head(Break,1),to=optbk,col='red')
# abline(v=optbk,col='gray')  
# minor.tick(5)
# plot(Break,rse,type='l',main='monthly',xlab='Breakpoint')
# minor.tick(5)  
# title(stinfo[1,2],outer=T)
# 
# 
# #---------------------- using segmented package --------------------------------
# #this method assumes that segments are continuous
# library(segmented)
# par(mfrow=c(2,1))
# 
# #Annual Mean Discharge..........................................................
# mod.lm   = lm(avg~year,data=disE.yr)
# mod.seg  = segmented(mod.lm,seg.Z=~year,psi=1950)
# summary(mod.seg)
# plot(avg~year,data=disE.yr,type='b',main='annual',xlab='Year',ylab='Mean Discharge (cf/s)')
# plot(mod.seg,add=T,col='red')
# lines(mod.seg,col='blue') 
# minor.tick(5)
# anova(mod.lm,mod.seg)   
# 
# #Monthly Mean Discharge.........................................................
# mod.lm   = lm(avg~yrmo,data=disE.mo)
# mod.seg  = segmented(mod.lm,seg.Z=~yrmo,psi=1950)
# summary(mod.seg)
# plot(avg~yrmo,data=disE.mo,type='b',main='monthly',xlab='Month',ylab='Mean Discharge (cf/s)')
# plot(mod.seg,add=T,col='red')
# lines(mod.seg,col='blue')
# minor.tick(5)
# anova(mod.lm,mod.seg)
# 
# title(stinfo[1,2],outer=T)
# 
# 
# 
# ################################################################################ 
# #
# #                     TIME SERIES DECOMPOSITION & ANALYSIS
# #
# ################################################################################ 
# #--------------------------- DISCHARGE -----------------------------------------
# par(mfrow=c(1,1))
# plot(disE.mo.ts)
# frequency(disE.mo.ts)
# 
# disE.mo.stl = stl(disE.mo.ts,"periodic")
# plot(disE.mo.stl,main='Monthly Mean Discharge Data')
# ts.plot(disE.mo.stl$time.series,col=c('black','blue','red'))
# legend('bottomright',legend=attributes(disE.mo.stl$time.series)$dimnames[[2]],
#        bty='n',text.col=c('black','blue','red'))
# 
# plot(diff(disE.mo.ts))
# 
