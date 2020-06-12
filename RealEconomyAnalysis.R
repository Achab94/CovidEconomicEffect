#------------------------------------------------------------------------------------------#
# ---- REAL ECONOMY ITALY -----
#------------------------------------------------------------------------------------------#

library(dlm)
library(xts)

#------------------------------------------------------------------------------------------#
# ---- download GDP -----
#------------------------------------------------------------------------------------------#

### full data
url.GDP <- "https://www.dropbox.com/s/2gygtthbfdtwl3f/QuarterlyGDP.csv?dl=1"
GDP.full <- read_csv(url.GDP)
GDP.full.adj <- GDP.full[, c("LOCATION", "TIME", "Value")]

### GDP in ITALY
GDP.ita <- as.data.frame(GDP.full.adj[GDP.full.adj$LOCATION == "ITA",]) 
GDP.ita.ts <- xts(GDP.ita$Value, order.by = as.yearqtr(GDP.ita$TIME, "%Y-Q%q"))

# nominal quarterly GDP
GDP.nom.ita = rep(0,length(GDP.ita.ts)+1)
GDP.nom.ita[1] = 10
for(i in 1:length(GDP.ita.ts)){
  GDP.nom.ita[i+1]=GDP.nom.ita[i]*(1+GDP.ita$Value[i]/100)
}

# monthly nominal quarter GDP
GDP.nom.ita.M = qtr2month(ts(c(GDP.nom.ita[-1]), start = c(2000), frequency = 4))
GDP.nom.ita.M.ts <- xts(c(GDP.nom.ita.M, rep(NA, length(dates)-length(GDP.nom.ita.M))),
                        order.by = as.yearmon(substr(as.Date(dates), start=1, stop=7),
                                              format="%Y-%m"))


#------------------------------------------------------------------------------------------#
# ---- download Unemployment rate -----
#------------------------------------------------------------------------------------------#

### Employment in ITALY
url.ita.emp <- "https://www.dropbox.com/s/a1qrlt0uoraz7mk/italy.unemployment-rate.csv?dl=1"
emp.ita.dat <- read.csv(url.ita.emp, sep = "\t")

## change direction of time
emp.ita = emp.ita.dat$ActualValue
emp.ita <- emp.ita[length(emp.ita):1]

## monthly unemploument from 2000
emp.ita.val <- c(rep(NA, length(dates) - length(emp.ita) - 1), emp.ita, NA )
emp.ita.ts <- xts(emp.ita.val, order.by = as.yearmon(substr(as.Date(dates), start=1, stop=7),
                                                     format="%Y-%m"))


#------------------------------------------------------------------------------------------#
# ---- download PMI manufacturing -----
#------------------------------------------------------------------------------------------#

pmi.date <- seq(as.Date("2000-01-01"), as.Date("2020-05-01"), by = "month")


### PMI MANUFACTURING in ITALY
url.ita.man <- "https://www.dropbox.com/s/kco0vvagtbzb5zm/italy.markit-manufacturing-pmi.csv?dl=1"
pmi.ita.man.dat <- read.csv(url.ita.man, sep = "\t")

# nominal version of pmi
pmi.ita.man = rep(0, nrow(pmi.ita.man.dat)+1)
pmi.ita.man[1] <- 10
for(i in 1:nrow(pmi.ita.man.dat)){
  pmi.ita.man[i+1]=pmi.ita.man[i]*(1+(pmi.ita.man.dat[nrow(pmi.ita.man.dat)+1-i,2]-50)/100)
}

# monthly nominal version of pmi from Jan. 2000
pmi.ita.man.val <- c(rep(NA, length(dates)-length(pmi.ita.man)), pmi.ita.man)
pmi.ita.man.ts <- xts(pmi.ita.man.val, order.by = as.yearmon(substr(as.Date(dates),start=1, stop=7), 
                                                             format="%Y-%m"))


#------------------------------------------------------------------------------------------#
# ---- download PMI service -----
#------------------------------------------------------------------------------------------#


### PMI SERVICE in ITALY
url.ita.ser <- "https://www.dropbox.com/s/2ypwwc0w53nuy1w/italy.markit-services-pmi.csv?dl=1"
pmi.ita.ser.dat <- read.csv(url.ita.ser, sep = "\t")

# nominal version of pmi
pmi.ita.ser = rep(0, nrow(pmi.ita.ser.dat)+1)
pmi.ita.ser[1] <- 10
for(i in 1:nrow(pmi.ita.ser.dat)){
  pmi.ita.ser[i+1]=pmi.ita.ser[i]*(1+(pmi.ita.ser.dat[nrow(pmi.ita.ser.dat)+1-i,2]-50)/100)
}


# monthly nominal version of pmi from Jan. 2000
pmi.ita.ser.val <- c(rep(NA, length(dates)-length(pmi.ita.ser)), pmi.ita.ser)
pmi.ita.ser.ts <- xts(pmi.ita.ser.val, order.by = as.yearmon(substr(as.Date(dates),
                                                                    start=1, stop=7), format="%Y-%m"))




#------------------------------------------------------------------------------------------#
# ---- STRINGENCY INDEX and  ECONOMICAL SUPPORT  -----
#------------------------------------------------------------------------------------------#


## ECONOMICAL SUPPORT
# E1_Income support
# E2_Debt/contract relief
# E3_Fiscal measures 

## COUNTRY CODES
# ITA: Italy

## load data from github
url.git <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
full.git <- read_csv(url.git)


## full data in 6 countries
countries1 <- c("Italy", "Germany", "France", "United Kingdom", "United States", "China")
dat.git <- as.data.frame(full.git[full.git$CountryName %in% countries1,])

## names for policy columns
names.pol <- c("E1","E2","E3", "StringencyIndex")

## STRINGENCY INDEX DATA FOR 6 COUNTRIES
pol.dat <- subset(dat.git, select = c("Date", "CountryCode", 
                                      "E1_Income support", "E2_Debt/contract relief",
                                      "E3_Fiscal measures", "StringencyIndex"))
colnames(pol.dat) <- c("Date", "CountryCode", names.pol)
pol.dat$Date <- strptime(as.character(pol.dat$Date), format = "%Y%m%d")


# average monthly
pol.dat.M <- aggregate(pol.dat[,names.pol], 
                       list(format(pol.dat$Date, "%Y/%m"), pol.dat$CountryCode), mean)[,-1] 


# Covariates matrix
x.tmp = as.matrix(pol.dat.M[which(pol.dat.M$Group.2=="ITA"),c("StringencyIndex","E1")])
x = rbind(matrix(0,dim(pmi.ita.man.ts)[1]-4,2), x.tmp[-nrow(x.tmp),])
x[,1]= x[,1]/100
# E2 corresponds to E1
# stringency index measured between 0 and 1

# x[,3]= x[,3]/10^(9)
# investment measured in bln of dollars. to ignore


#------------------------------------------------------------------------------------------#
# ---- Time series -----
#------------------------------------------------------------------------------------------#


# look at the time series
plot(emp.ita.ts)
plot(pmi.ita.man.ts)
plot(pmi.ita.ser.ts)
plot(na.locf(GDP.nom.ita.M.ts, na.rm = T, fromLast = T))
plot(na.approx(GDP.nom.ita.M.ts))
lines(emp.ita.ts, col="red", lwd=2)


#------------------------------------------------------------------------------------------#
# ---- Smoothing GDP -----
#------------------------------------------------------------------------------------------#

# smooth GDP (infer the month measure under the constraint of exact quarter mean)

# the quarter GDP is explain by the two series of quarter PMIs and a quarter smooth series
# GDPq = beta0 + beta1*(sum(pmi.man) ) + beta2*(sum(pmi.ser) ) + beta3*(sum(smooth) ) + eps
# where the last two terms forms residuals
# we will smooth the residuals and we will weight each month according to the smooth residuals

# after 2011
GDP.mean.q = 3*as.numeric(GDP.nom.ita.M.ts)[137:(length(GDP.nom.ita.M.ts)-1)]
pmi.ita.man.q = drop(as.matrix(aggregate(as.numeric(pmi.ita.man.ts[137:(length(GDP.nom.ita.M.ts)-1)]),
                          by=list(ceiling(seq(1,length(GDP.nom.ita.M.ts)-137)/3) ), FUN=sum )[2]))
pmi.ita.ser.q = drop(as.matrix(aggregate(as.numeric(pmi.ita.ser.ts[137:(length(GDP.nom.ita.M.ts)-1)]),
                          by=list(ceiling(seq(1,length(GDP.nom.ita.M.ts)-137)/3) ), FUN=sum )[2]))
lm.mod = lm( GDP.mean.q[!is.na(GDP.mean.q)] ~ pmi.ita.man.q )
lm.mod2 = lm( GDP.mean.q[!is.na(GDP.mean.q)] ~ pmi.ita.ser.q )
summary(lm.mod); summary(lm.mod2)
# ser explain slightly better

# make monthly residuals and smooth excluding the last three months
res.m = residuals(lm.mod2)[ceiling(seq(1,length(GDP.nom.ita.M.ts)-137)/3)]/3
lo = loess(res.m[seq(1,length(res.m)-3)] ~ seq(1, length(res.m)-3), span = 0.1 )
# since the last quarter is completely different with a shock within the month
# we do not smooth the residuals in the last quarter, 
# leaving to the state space model the smoothing part
plot(res.m)
lines(predict(lo), col="red", lwd=2)
res.smooth.q = drop(as.matrix(aggregate(residuals(lo), 
                      by=list(ceiling(seq(1,length(res.m)-3)/3)), FUN=sum)[2]))

# gdp month
gdp.month = coef(lm.mod2)[1]/3 + coef(lm.mod2)[2]*as.numeric(pmi.ita.ser.ts[137:(length(GDP.nom.ita.M.ts)-1)])+
  c(predict(lo),0,0,0) +
  c(res.smooth.q[ceiling(seq(1,length(res.m)-3)/3)]/3, res.m[(length(res.m)-2):length(res.m)])
plot(gdp.month)
 
#
# before 2011: just the smoothing
GDP.month.fix = as.numeric(na.locf(GDP.nom.ita.M.ts, fromLast = T)[1:136])
lo11 = loess(GDP.month.fix ~ seq(1, 136), span = 0.1 )
# since the last quarter is completely different with a shock within the month
# we do not smooth the residuals in the last quarter, 
# leaving to the state space model the smoothing part
plot(GDP.month.fix)
lines(predict(lo11), col="red", lwd=2)
res.smooth.q = drop(as.matrix(aggregate(residuals(lo11), 
                                        by=list(c(0,ceiling(seq(1,length(GDP.month.fix)-1)/3))), FUN=sum)[2]))

# gdp month
gdp.month2 = predict(lo11) + c(res.smooth.q[1],
                               res.smooth.q[ceiling(seq(1,length(GDP.month.fix)-1)/3)+1]/3)
plot(gdp.month2)


# New spline Time series for GDP
# quarter GDP
GDP.nom.ita.Q.ts= na.locf(GDP.nom.ita.M.ts, fromLast = T)*3
GDP.nom.ita.M.ts = xts(c(gdp.month2,gdp.month,NA), order.by = index(GDP.nom.ita.Q.ts))
plot(GDP.nom.ita.Q.ts)
plot(GDP.nom.ita.M.ts)

rm(gdp.month, gdp.month2, GDP.mean.q, GDP.month.fix, res.m, res.smooth.q, res,
   lo, lo11, lm.mod, lm.mod2)

#------------------------------------------------------------------------------------------#
# ---- Delay of the Unemployement -----
#------------------------------------------------------------------------------------------#


# correlation between unemployement or lag
corrplot(cor( cbind( as.numeric(emp.ita.ts[135:228]),
                     as.numeric(emp.ita.ts[137:230]),
                     as.numeric(emp.ita.ts[139:232]),
                     as.numeric(emp.ita.ts[141:234]),
                     as.numeric(emp.ita.ts[143:236]),
                     as.numeric(emp.ita.ts[145:238]),
                     as.numeric(emp.ita.ts[147:240]),
                     as.numeric(pmi.ita.man.ts[135:228]),
                     as.numeric(pmi.ita.ser.ts[135:228]) )  ))
# lag ? di almeno 5
corrplot(cor( cbind( as.numeric(emp.ita.ts[135:228]),
                     as.numeric(emp.ita.ts[139:232]),
                     as.numeric(emp.ita.ts[140:233]),
                     as.numeric(emp.ita.ts[141:234]),
                     as.numeric(emp.ita.ts[142:235]),
                     as.numeric(emp.ita.ts[143:236]),
                     as.numeric(emp.ita.ts[144:237]),
                     as.numeric(emp.ita.ts[145:238]),
                     as.numeric(emp.ita.ts[146:239]),
                     as.numeric(emp.ita.ts[147:240]),
                     as.numeric(pmi.ita.man.ts[135:228]),
                     as.numeric(pmi.ita.ser.ts[135:228]) )  ))

cor(cbind(as.numeric(emp.ita.ts[144:237]),
          as.numeric(emp.ita.ts[145:238]),
          as.numeric(emp.ita.ts[146:239]),
          as.numeric(emp.ita.ts[147:240]),
          as.numeric(pmi.ita.man.ts[135:228]),
          as.numeric(pmi.ita.ser.ts[135:228])) )

plot( as.numeric(pmi.ita.man.ts[135:228]), type="l", lwd=2 )
lines( as.numeric(pmi.ita.ser.ts[135:228]), lwd=2 , col="blue")
lines( as.numeric(45-3*emp.ita.ts[135:228]), lwd=2 , col="red")
lines( as.numeric(45-3*emp.ita.ts[138:231]), lwd=2 , col="green")
lines( as.numeric(45-3*emp.ita.ts[144:237]), lwd=2 , col="green")

# 3 quarters late
cor(cbind(as.numeric(emp.ita.ts[135:235]),
          as.numeric(emp.ita.ts[138:238]),
          as.numeric(emp.ita.ts[141:241]),
          as.numeric(pmi.ita.man.ts[135:235]),
          as.numeric(pmi.ita.ser.ts[135:235])) )


#------------------------------------------------------------------------------------------#
# ---- Scale of timeseries and  covariates -----
#------------------------------------------------------------------------------------------#


# scale of the series
sd(GDP.nom.ita.M.ts, na.rm = T)
sd(emp.ita.ts, na.rm = T)
sd(pmi.ita.man.ts, na.rm = T)
sd(pmi.ita.ser.ts, na.rm = T)
# that means that global cycles should be scaled in F of
# 1 - 5 - 30 - 14

#------------------------------------------------------------------------------------------#
# ---- Trend and cycles -----
#------------------------------------------------------------------------------------------#

# Linear trend
tr.scale = 10^(-1)
Tr = dlmModPoly(order = 2, dV= tr.scale, dW=c(0,tr.scale), C0=tr.scale*4*diag(2))

# big and small

# Business cycle Huge: just for GDP
bc.scale1 = 10^(-1)
Bc0 = dlmModTrig(s=16*12, q=1, dV= bc.scale1 , C0=bc.scale1*4*diag(2)) 


# Business Cycles big: last for 8 years more or less (reference). 
# quasi-random walk model
bc.scale1 = 10^(-1)
Bc1 = dlmModTrig(s=8*12, q=1, dV= bc.scale1 , C0=bc.scale1*4*diag(2)) 
# used 3 times: shifted for unemployement and PMI (but highly correlated) 


# Business Cycle small, crisis: last for 4 years more or less. 
bc.scale2 = 10^(-1)
Bc2 = dlmModTrig(s=4*12, q=1, dV= bc.scale2 , C0=bc.scale2*4*diag(2)) 
W(Bc2)[1,1] = bc.scale2

# pay attention to unemployement rate: countercyclical (change F matrix) 
# and lagged of 9 months (change the series the G)



# Univariate
# 10 States:
  # 2 for trend, 4 for business cycle, 2 for indirect, 2 for direct
FF.uni = cbind(FF(Tr),FF(Bc1),FF(Bc2), matrix(c(0,0,1,1),1,4) )
JFF.uni = matrix(c(rep(0,8),1,2),1,10)
V.uni = 0.4
GG.uni = matrix(0,10,10)
GG.uni[1:2,1:2]=GG(Tr)     # trend evolution
GG.uni[3:4,3:4]=GG(Bc1)     # business cycle evolution
GG.uni[5:6,5:6]=GG(Bc2)
GG.uni[7:10, 7:10]=diag(4)  # coeff evolution
GG.uni[5,7:8]=c(1,1)  # impact of the indirect coefficient on smaller business cycle 
JGG.uni = matrix(0,10,10)
JGG.uni[5,7:8]=c(1,2)

# RW only trend and on BC
W.uni = matrix(0,10,10)
W.uni[1:2,1:2] = W(Tr) 
W.uni[3:4,3:4] = W(Bc1)
W.uni[5:6,5:6] = W(Bc2)

# mean of latent state, forcing positive and negative effect
m0.uni=c(rep(0,6),  -2, 1, -2, 1)

# variance of latent state
C0.uni=matrix(0,10,10)
C0.uni[1:2,1:2] = C0(Tr)
C0.uni[3:4, 3:4] = C0(Bc1)
C0.uni[5:6, 5:6] = C0(Bc2)
C0.uni[7:10, 7:10] = diag(c(1, 1, 1, 1))     
# Economic support has smaller scale (so bigger coeff)

# We expect stringency and economics parameters are negative correlated
C0.uni[7,8]=C0.uni[8,7]=C0.uni[9,10]=C0.uni[10,9]= -0.4
# and the direct effect and the indirect effect are positive correlated
C0.uni[7,9]=C0.uni[9,7]= 0.4
C0.uni[8,10]=C0.uni[10,8]= 0.4
C0.uni

# Variable coefficients
X.uni = x

# Univariate model
mod.uni =dlm( list(FF=FF.uni, V=V.uni, GG=GG.uni, W=W.uni, m0=m0.uni, C0=C0.uni,
                   JFF=JFF.uni, JGG=JGG.uni, X=X.uni) )





### Quatervariate

# shift the unemployement series
shift.emp.ita.ts = xts(c(as.numeric(emp.ita.ts[10:length(emp.ita.ts)]),rep(NA,9)),
                       order.by = index(emp.ita.ts))
plot(shift.emp.ita.ts)


# we assume different trend but same business cycles.
# different direct effect of the lockdown
  # 4 common states + 2 for 3 shifted cycles +1 covariates effect for each series + 2 trend states each series

# c(0, -1, 0.5, 1.8 ,1, -3,-1.5,-0.5,-1,-3)
buildMod = function(par, ...){
  
  # par1: qlogis of dumping, 
  dump= plogis(par[1])
  # par 4-7 : V matr
  v.vec = exp(par[2:5])+rep(0.0001,4)
  # v.vec = exp(c(-1, 0.5, 1.8 ,1))
  # par 8-12: W matr
  w.vec = exp(par[6:10])+rep(0.0001,5)
  #w.vec = c(-3,-1.5,-0.5,-1,-3)
  
  # Linear trend
  tr.scale = 10^(-1)
  Tr = dlmModPoly(order = 2, dV= tr.scale, dW=c(0,tr.scale), C0=tr.scale*4*diag(2))
  
  # big and small
  # Business Cycles big: last for 8 years more or less (reference). 
  # quasi-random walk model
  bc.scale1 = 10^(-1)
  Bc1 = dlmModTrig(s=f1*12, q=1, dV= bc.scale1 , C0=bc.scale1*4*diag(2)) 
  # used 3 times: shifted for unemployement and PMI (but highly correlated) 
  
  # Business Cycle small, crisis: last for 4 years more or less. 
  bc.scale2 = 10^(-1)
  Bc2 = dlmModTrig(s=f2*12, q=1, dV= bc.scale2 , C0=bc.scale2*4*diag(2)) 

  # FF
  FF.quad = rbind(c(1,0,rep(0,2),rep(0,2),rep(0,2), FF(Bc1),rep(0,2),rep(0,2), FF(Bc2), rep(0,2), c(1,0,0,0) ),
                  c(rep(0,2),1,0,rep(0,2),rep(0,2), rep(0,2), -5*FF(Bc1),rep(0,2), -5*FF(Bc2), rep(0,2),c(0,1,0,0)),
                  c(rep(0,2),rep(0,2),1,0,rep(0,2), rep(0,2), rep(0,2), 30*FF(Bc1), 30*FF(Bc2), rep(0,2),c(0,0,1,0) ),
                  c(rep(0,2),rep(0,2),rep(0,2),1,0,  rep(0,2), rep(0,2), 15*FF(Bc1), 15*FF(Bc2), rep(0,2), c(0,0,0,1) ) )
  JFF.quad = matrix(0,4,22)
  JFF.quad[1,19]=JFF.quad[2,20]=JFF.quad[3,21]=JFF.quad[4,22]=1
  
  #GG
  GG.quad = matrix(0,22,22)
  GG.quad[1:2,1:2] = GG.quad[3:4,3:4] =GG.quad[5:6,5:6] = GG.quad[7:8,7:8] = GG(Tr)
  GG.quad[9:10,9:10] = GG.quad[11:12,11:12] = GG.quad[13:14,13:14] =  GG(Bc1)
  GG.quad[15:16,15:16] = dump*GG(Bc2)
  GG.quad[15,17:18]  = c(1,1)
  GG.quad[16,17:18] = c(1,1)
  GG.quad[17:22,17:22]=diag(6)
  JGG.quad = matrix(0,22,22)
  JGG.quad[15,17:18] = JGG.quad[16,17:18] = c(1,2)

  
  # V covariance matrix
  # we assume zero correlation on V (verified for PMI)
  # 0.5, 1.5, 6, 3
  v.vec[is.infinite(v.vec)]=10^6
  V.quad = diag(v.vec)
  
  # W covariance
  # 0.05, 0.2, 0.6, 0.3 - 0.05
  w.vec[is.infinite(w.vec)]=10^6
  W.quad = matrix(0,22,22)
  W.quad[1,1] = w.vec[1]
  W.quad[3,3] = w.vec[2]
  W.quad[5,5] = w.vec[3]
  W.quad[7,7] = w.vec[4]
  W.quad[15,15] = w.vec[5]

  # opposite direct effect for unemployement
  m0.quad = c(10,0,rep(0,6), rep(0,8), c(-2, 1, -2, 2, -2,-2))

  C0.quad = matrix(0,22,22)
  # correlation between beta
  C0.quad[1:2,1:2] =C0.quad[3:4,3:4] =C0.quad[5:6,5:6] =C0.quad[7:8,7:8] = C0(Tr)
  # correlation between different 8 year cycle
  C0.quad[c(9,11,13),c(9,11,13)]=0.4*C0(Bc1)[1,1]
  C0.quad[c(10,12,14),c(10,12,14)]=0.4*C0(Bc1)[1,1]
  C0.quad[9:10,9:10]=C0.quad[11:12,11:12]=C0.quad[13:14,13:14]=C0(Bc1)
  # indirect effect
  C0.quad[17:18,17:18]=matrix(c(1,-0.4,-0.4,1),2,2)
  # direct effect
  C0.quad[19:22,19:22]=0.4
  C0.quad[19,19]=C0.quad[20,20]=C0.quad[21,21]=C0.quad[22,22]=1
  # correlation between direct effects and positive indirect
  C0.quad[19:22,17]=C0.quad[17,19:22]=0.4
  C0.quad[19:22,18]=C0.quad[18,19:22]=0.4
  

  mod.quad =dlm( list(FF=FF.quad, V=V.quad, GG=GG.quad, W=W.quad, m0=m0.quad, C0=C0.quad,
                      JFF=JFF.quad, JGG=JGG.quad, X=x) )
  return(mod.quad)
}


# estimation
# Order: GDP, unemployement, pmi.man, pmi.ser
multivariate.ts = xts(cbind(GDP.nom.ita.M.ts, emp.ita.ts, pmi.ita.man.ts, pmi.ita.ser.ts),
                      order.by = index(GDP.nom.ita.M.ts))

# it cannot estimate directly the different measure of the cicles.
# I'll do by hand
ll=matrix(NA, 12,6)
for(f1 in c(5,6,7,8,9,10,11,12)){
  for(f2 in c(2,3,4,5,6)){
    fitMod = dlmMLE(multivariate.ts, parm = c(0, -1, 0.5, 1.8 ,1, -3,-1.5,-0.5,-1,-3),
                    build = buildMod, hessian = TRUE, debug=T, method = "BFGS", f1=f2, f2=f2)
    ll[f1,f2] = fitMod$value
  }
}
ll
# f1=10, f2=6. But also good  (f1=12, f2=3), (f1=8, f2=2), (f1=10, f2=4)

# Take a look to f1=10, f2=6
fitMod = dlmMLE(multivariate.ts, parm = c(0, -1, 0.5, 1.8 ,1, -3,-1.5,-0.5,-1,-3),
                build = buildMod, hessian = TRUE, debug=T, method = "BFGS", f1=10, f2=6)
fitMod$par

# try to see 
mod.quad=buildMod(fitMod$par, f1=10, f2=6)


# filtering and smoothing
filtred = dlmFilter(y=multivariate.ts, mod = mod.quad)
smoothed = dlmSmooth(y=filtred)

theta.filter = filtred$m[-1,]
theta.smooth = smoothed$s[-1,]

# smoothed trend GDP
smooth.tr.gdp = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.tr.gdp[i] = theta.smooth[i,1]
}
plot(smooth.tr.gdp, type="l", lwd=2, col="dodgerblue3")

# smoothed BC  GDP
smooth.bc1.gdp = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.bc1.gdp[i] = sum(theta.smooth[i,9])
}
plot(smooth.bc1.gdp, type="l", lwd=2, col="dodgerblue3")
plot(smooth.tr.gdp+smooth.bc1.gdp, type="l", lwd=2, col="dodgerblue3")

# smoothed recession cycle
smooth.bc2.gdp = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.bc2.gdp[i] = sum(theta.smooth[i,15])
}
plot(smooth.bc2.gdp, type="l", lwd=2, col="dodgerblue3")

# smoothed tr+bc
plot(smooth.tr.gdp+smooth.bc1.gdp+smooth.bc2.gdp, type="l", lwd=2, col="dodgerblue3")
lines(as.numeric(GDP.nom.ita.M.ts), col="red", lwd=2)


# direct and indirect effect of covariates
str.direff.gdp = theta.smooth[1,19]
str.indireff = theta.smooth[1,17:18]
smooth.dir.str.gdp = x[,1]*str.direff.gdp

plot(smooth.tr.gdp+smooth.bc1.gdp+smooth.bc2.gdp+smooth.dir.str.gdp,
     type="l", lwd=2, col="dodgerblue3")
lines(as.numeric(as.numeric(GDP.nom.ita.M.ts)), col="red", lwd=2)


# smoothed trend unempl
smooth.tr.emp = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.tr.emp[i] = theta.smooth[i,3]
}
plot(smooth.tr.emp, type="l", lwd=2, col="dodgerblue3")

# smoothed BC  GDP
smooth.bc1.emp = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.bc1.emp[i] = -5*theta.smooth[i,11]
}
plot(smooth.bc1.emp, type="l", lwd=2, col="dodgerblue3")
plot(smooth.tr.emp+smooth.bc1.emp, type="l", lwd=2, col="dodgerblue3")


smooth.bc2.emp = -5*smooth.bc2.gdp
plot(smooth.bc2.emp, type="l", lwd=2, col="dodgerblue3")

# smoothed tr+bc
plot(smooth.tr.emp+smooth.bc1.emp+smooth.bc2.emp, type="l", lwd=2, col="dodgerblue3")
lines(as.numeric(emp.ita.ts), col="red", lwd=2)


# direct and indirect effect of covariates
str.direff.emp = theta.smooth[1,20]
smooth.dir.str.emp = x[,1]*str.direff.emp

plot(smooth.tr.emp+smooth.bc1.emp+smooth.bc2.emp+smooth.dir.str.emp,
     type="l", lwd=2, col="dodgerblue3",
     xlim=c(135,length(smooth.tr.emp)), ylim=c(0,15) )
lines(as.numeric(emp.ita.ts), col="red", lwd=2)

# smoothed trend pmi man
smooth.tr.pmi.man = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.tr.pmi.man[i] = theta.smooth[i,5]
}
plot(smooth.tr.pmi.man, type="l", lwd=2, col="dodgerblue3")

# smoothed BC  pmi
smooth.bc1.pmi.man = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.bc1.pmi.man[i] = 30*theta.smooth[i,13]
}
plot(smooth.bc1.pmi.man, type="l", lwd=2, col="dodgerblue3")
plot(smooth.tr.pmi.man+smooth.bc1.pmi.man, type="l", lwd=2, col="dodgerblue3")

smooth.bc2.pmi.man = 30*smooth.bc2.gdp
plot(smooth.bc2.pmi.man, type="l", lwd=2, col="dodgerblue3")

# smoothed tr+bc
plot(smooth.tr.pmi.man+smooth.bc1.pmi.man+smooth.bc2.pmi.man, type="l", lwd=2, col="dodgerblue3")
lines(as.numeric(pmi.ita.man.ts), col="red", lwd=2)

# direct and indirect effect of covariates
str.direff.pmi.man = theta.smooth[1,21]
smooth.dir.str.pmi.man = x[,1]*str.direff.pmi.man

plot(smooth.tr.pmi.man+smooth.bc1.pmi.man+smooth.bc2.pmi.man+smooth.dir.str.pmi.man,
     type="l", lwd=2, col="dodgerblue3",
     xlim=c(135,length(smooth.tr.pmi.man)), ylim=c(0,30))
lines(as.numeric(pmi.ita.man.ts), col="red", lwd=2)



# smoothed trend pmi ser
smooth.tr.pmi.ser = rep(0,nrow(theta.smooth))
for(i in 1:nrow(theta.smooth)){
  smooth.tr.pmi.ser[i] = theta.smooth[i,7]
}
plot(smooth.tr.pmi.ser, type="l", lwd=2, col="dodgerblue3")

# smoothed BC  pmi
smooth.bc1.pmi.ser = smooth.bc1.pmi.man/2
plot(smooth.bc1.pmi.ser, type="l", lwd=2, col="dodgerblue3")
plot(smooth.tr.pmi.ser+smooth.bc1.pmi.ser, type="l", lwd=2, col="dodgerblue3")

smooth.bc2.pmi.ser = 15*smooth.bc2.gdp
plot(smooth.bc2.pmi.ser, type="l", lwd=2, col="dodgerblue3")

# smoothed tr+bc
plot(smooth.tr.pmi.ser+smooth.bc1.pmi.ser+smooth.bc2.pmi.ser, type="l", lwd=2, col="dodgerblue3",
     xlim=c(135,length(smooth.tr.pmi.ser)), ylim=c(0,20))
lines(as.numeric(pmi.ita.ser.ts), col="red", lwd=2)

# direct and indirect effect of covariates
str.direff.pmi.ser = theta.smooth[1,22]
smooth.dir.str.pmi.ser = x[,1]*str.direff.pmi.ser

plot(smooth.tr.pmi.ser+smooth.bc1.pmi.ser+smooth.bc2.pmi.ser+smooth.dir.str.pmi.ser,
     type="l", lwd=2, col="dodgerblue3",
     xlim=c(135,length(smooth.tr.pmi.ser)), ylim=c(0,20))
lines(as.numeric(pmi.ita.ser.ts), col="red", lwd=2)





#-------------------------------------------------------------------------------------------------#
##---- FORECAST ----
#-------------------------------------------------------------------------------------------------#

# forecast (current level of lockdown and economical support)

### forecast with different scenarios
?dlmForecast

# stopping the lockdown and the economical support
mod.quad1 = mod.quad
GG(mod.quad1)[15,c(17,18)] = GG(mod.quad1)[16,c(17,18)] = c(0,0)
JGG(mod.quad1)[15,c(17,18)] = JGG(mod.quad1)[16,c(17,18)] = 0
FF(mod.quad1)[1:4, 19:22] = JFF(mod.quad1)[1:4, 19:22] = 0

filtred$mod=list(FF=FF(mod.quad1), GG=GG(mod.quad1), V=V(mod.quad1),
                 W=W(mod.quad1), m0=m0(mod.quad1), C0=C0(mod.quad1))

forecast1 = dlmForecast(mod=filtred, nAhead = 12 , sampleNew = 1000)

# states
plot(rbind(theta.smooth, forecast1$a)[,15], type="l")

# GDP
smooth.gdp = smooth.tr.gdp+smooth.bc1.gdp+smooth.bc2.gdp+smooth.dir.str.gdp
plot(as.numeric(GDP.nom.ita.M.ts), type="l", lwd=2, 
     xlim = c(0,260), ylim=c(9,11.5))
lines(smooth.gdp, lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.gdp),length(smooth.bc1.gdp)+12), 
       c(smooth.gdp[length(smooth.gdp)],forecast1$f[1:12,1]),
       col="dodgerblue", lwd=2)

# PMI
plot(as.numeric(pmi.ita.man.ts), type="l", lwd=2, 
     xlim = c(130,260), ylim=c(4,30))
lines(smooth.tr.pmi.man+smooth.bc1.pmi.man+smooth.bc2.pmi.man+smooth.dir.str.pmi.man,
      lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.pmi.man)+1,length(smooth.bc1.pmi.man)+12), forecast1$f[,3],
       col="dodgerblue", lwd=2)


# keep actual situation
# half lockdown and economical support 
mod.quad2 = mod.quad
GG(mod.quad2)[15,c(17,18)] = GG(mod.quad2)[16,c(17,18)] = c(0.5,1)
JGG(mod.quad2)[15,c(17,18)] = JGG(mod.quad2)[16,c(17,18)] = 0
FF(mod.quad2)[1:4, 19:22] = diag(4)*0.5
JFF(mod.quad2)[1:4, 19:22] = 0

filtred$mod=list(FF=FF(mod.quad2), GG=GG(mod.quad2), V=V(mod.quad2),
                 W=W(mod.quad2), m0=m0(mod.quad2), C0=C0(mod.quad2))

forecast2 = dlmForecast(mod=filtred, nAhead = 24 , sampleNew = 1000)

# states
plot(rbind(theta.smooth, forecast2$a)[,15], type="l")

# GDP
smooth.gdp = smooth.tr.gdp+smooth.bc1.gdp+smooth.bc2.gdp+smooth.dir.str.gdp
plot(as.numeric(GDP.nom.ita.M.ts), type="l", lwd=2, 
     xlim = c(0,260), ylim=c(9,11.5))
lines(smooth.gdp, lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.gdp),length(smooth.bc1.gdp)+12), 
       c(smooth.gdp[length(smooth.gdp)],forecast2$f[1:12,1]),
       col="dodgerblue", lwd=2)



# keep a small level of lockdown and no economical support
mod.quad3 = mod.quad
GG(mod.quad3)[15,c(17,18)] = GG(mod.quad2)[16,c(17,18)] = c(0.25,0.75)
JGG(mod.quad3)[15,c(17,18)] = JGG(mod.quad2)[16,c(17,18)] = 0
FF(mod.quad3)[1:4, 19:22] = diag(4)*0.25
JFF(mod.quad3)[1:4, 19:22] = 0

filtred$mod=list(FF=FF(mod.quad3), GG=GG(mod.quad3), V=V(mod.quad3),
                 W=W(mod.quad3), m0=m0(mod.quad3), C0=C0(mod.quad3))

forecast3 = dlmForecast(mod=filtred, nAhead = 12 , sampleNew = 1000)

# states
plot(rbind(theta.smooth, forecast3$a)[,15], type="l")

# GDP
plot(as.numeric(GDP.nom.ita.M.ts), type="l", lwd=2, 
     xlim = c(0,260), ylim=c(9,11.5))
lines(smooth.gdp, lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.gdp),length(smooth.bc1.gdp)+12), 
       c(smooth.gdp[length(smooth.gdp)],forecast3$f[1:12,1]),
       col="dodgerblue", lwd=2)



# New wave of contagion: high lockdown and economical support
mod.quad4 = mod.quad
GG(mod.quad4)[15,c(17,18)] = GG(mod.quad2)[16,c(17,18)] = c(0.75,1)
JGG(mod.quad4)[15,c(17,18)] = JGG(mod.quad2)[16,c(17,18)] = 0
FF(mod.quad4)[1:4, 19:22] = diag(4)*0.75
JFF(mod.quad4)[1:4, 19:22] = 0

filtred$mod=list(FF=FF(mod.quad4), GG=GG(mod.quad4), V=V(mod.quad4),
                 W=W(mod.quad4), m0=m0(mod.quad4), C0=C0(mod.quad4))

forecast4 = dlmForecast(mod=filtred, nAhead = 24 , sampleNew = 1000)

# states
plot(rbind(theta.smooth, forecast4$a)[,15], type="l")

# GDP
plot(as.numeric(GDP.nom.ita.M.ts), type="l", lwd=2, 
     xlim = c(0,260), ylim=c(9,11.5))
lines(smooth.gdp, lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.gdp),length(smooth.bc1.gdp)+24), 
       c(smooth.gdp[length(smooth.gdp)],forecast4$f[1:24,1]),
       col="dodgerblue", lwd=2)



# stop the economic support after june but a bit of lockdown ?s kept
mod.quad5 = mod.quad
GG(mod.quad5)[15,c(17,18)] = GG(mod.quad2)[16,c(17,18)] = c(0.25,0.25)
JGG(mod.quad5)[15,c(17,18)] = JGG(mod.quad2)[16,c(17,18)] = 0
FF(mod.quad5)[1:4, 19:22] = diag(4)*0.25
JFF(mod.quad5)[1:4, 19:22] = 0

filtred$mod=list(FF=FF(mod.quad5), GG=GG(mod.quad5), V=V(mod.quad5),
                 W=W(mod.quad5), m0=m0(mod.quad5), C0=C0(mod.quad5))

forecast5 = dlmForecast(mod=filtred, nAhead = 24 , sampleNew = 1000)

# states
plot(rbind(theta.smooth, forecast4$a)[,15], type="l")

# GDP
plot(as.numeric(GDP.nom.ita.M.ts), type="l", lwd=2, 
     xlim = c(0,260), ylim=c(9,11.5))
lines(smooth.gdp, lwd=2, col="dodgerblue3")
lines( seq(length(smooth.bc1.gdp),length(smooth.bc1.gdp)+24), 
       c(smooth.gdp[length(smooth.gdp)],forecast4$f[1:24,1]),
       col="dodgerblue", lwd=2)
