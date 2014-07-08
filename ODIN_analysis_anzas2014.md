# ODIN data analysis
Gustavo Olivares  
08/07/2014  


```r
# Load required packages
require(openair)
```

```
## Loading required package: openair
```

```r
# Load ODIN data from figshare
odin_01 <- read.table("http://files.figshare.com/1582978/odin_01.data",
                      header=T,
                      quote="")
# force GMT as the time zone to avoid openair issues with daylight saving switches
# The actual time zone is 'NZST'
odin_01$date=as.POSIXct(paste(odin_01$Date,odin_01$Time),tz='GMT')
odin_01$Time<-NULL
odin_01$Date<-NULL
odin_01$Battery<-5*odin_01$Battery/1024

# Load ECan data from the data catalogue
download.file(url = "http://data.ecan.govt.nz/data/29/Air/Air%20quality%20data%20for%20a%20monitored%20site/CSV?SiteId=1&StartDate=15%2F05%2F2014&EndDate=04%2F06%2F2014",destfile = "ecan_data.csv",method = "curl")
# Note that the "system" calls require "sed" installed and have only been tested
# in Linux (see system information below)
system("sed -i 's/a.m./AM/g' ecan_data.csv")
system("sed -i 's/p.m./PM/g' ecan_data.csv")
ecan_data_raw <- read.csv("ecan_data.csv",stringsAsFactors=FALSE)
ecan_data_raw$date<-as.POSIXct(ecan_data_raw$DateTime,format = "%d/%m/%Y %I:%M:%S %p",tz='GMT')
ecan_data<-as.data.frame(ecan_data_raw[,c('date','FDMS.PM10','FDMS.PM25','Temp_2m_CR','Temp_10_HMP')])

# Merging the data
odin_01.10min<-timeAverage(odin_01,avg.time='10 min')
all_merged.10min_FULL<-merge(odin_01.10min,ecan_data,by='date',all=TRUE)
all_merged.10min<-selectByDate(all_merged.10min_FULL,start="15/5/2014",end="3/6/2014")
names(all_merged.10min)<-c('date','ODIN.raw','Humidity.01','Temperature.01',
                           'Battery.01','PM10','PM2.5',
                           'Temperature_1m','Temperature_10m')

# Time sync
# Check the time difference, correct the data and re-merge.
lag_test=ccf(all_merged.10min$Temperature.01,
             all_merged.10min$Temperature_1m,
             na.action=na.pass,
             lag.max=100,
             type='correlation',
             ylab='Correlation',
             main='Temperature correlation as function of clock lag')
```

![plot of chunk unnamed-chunk-1](./ODIN_analysis_anzas2014_files/figure-html/unnamed-chunk-1.png) 

```r
odin01_lag=lag_test$lag[which.max(lag_test$acf)]
odin_01$date=odin_01$date-odin01_lag*10*60
odin_01.10min<-timeAverage(odin_01,avg.time='10 min')
all_merged.10min_FULL<-merge(odin_01.10min,ecan_data,by='date',all=TRUE)
all_merged.10min<-selectByDate(all_merged.10min_FULL,start="15/5/2014",end="3/6/2014")
names(all_merged.10min)<-c('date','ODIN.raw','Humidity.01','Temperature.01',
                           'Battery.01','PM10','PM2.5',
                           'Temperature_1m','Temperature_10m')
all_merged.1hr<-timeAverage(all_merged.10min,avg.time='1 hour')


# Dust performance using ECan data for calibration
# Calibration expression:
#  $Dust_{calibrated}=A*Dust_{raw}+B*Temperature_{ODIN}+C*RH_{ODIN}+D$

# Full dataset 1 hour  
# PM$_{2.5}$ fdms
summary(odin1.lm.full.1hr.pm2.5<-
          lm(data=all_merged.1hr,PM2.5~
               ODIN.raw+Temperature.01+Humidity.01))
```

```
## 
## Call:
## lm(formula = PM2.5 ~ ODIN.raw + Temperature.01 + Humidity.01, 
##     data = all_merged.1hr)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -33.44  -7.89  -0.77   6.88  38.98 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -271.5035    12.4022   -21.9   <2e-16 ***
## ODIN.raw          0.8788     0.0257    34.2   <2e-16 ***
## Temperature.01   -5.4308     0.1322   -41.1   <2e-16 ***
## Humidity.01      -1.0504     0.0651   -16.1   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.3 on 414 degrees of freedom
##   (62 observations deleted due to missingness)
## Multiple R-squared:  0.839,	Adjusted R-squared:  0.837 
## F-statistic:  717 on 3 and 414 DF,  p-value: <2e-16
```

```r
### Daytime dataset 1 hour
daytime_merged.1hr<-selectByDate(all_merged.1hr,hour=9:18)

#### PM$_{2.5}$_fdms
summary(odin1.lm.daytime.1hr.pm2.5<-
          lm(data=daytime_merged.1hr,PM2.5~
               ODIN.raw+Temperature.01+Humidity.01))
```

```
## 
## Call:
## lm(formula = PM2.5 ~ ODIN.raw + Temperature.01 + Humidity.01, 
##     data = daytime_merged.1hr)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -19.36  -6.94  -2.15   4.51  35.36 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -76.9846    20.2176   -3.81  0.00019 ***
## ODIN.raw         0.2866     0.0553    5.18  6.1e-07 ***
## Temperature.01  -2.0862     0.3457   -6.03  9.4e-09 ***
## Humidity.01     -0.2684     0.1131   -2.37  0.01869 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.2 on 174 degrees of freedom
##   (22 observations deleted due to missingness)
## Multiple R-squared:  0.294,	Adjusted R-squared:  0.282 
## F-statistic: 24.1 on 3 and 174 DF,  p-value: 4.12e-13
```

```r
### Nighttime dataset 1 hour
nighttime_merged.1hr<-selectByDate(all_merged.1hr,hour=c(0:8,19:23))

#### PM$_{2.5}$ fdms
summary(odin1.lm.nighttime.1hr.pm2.5<-
          lm(data=nighttime_merged.1hr,PM2.5~
               ODIN.raw+Temperature.01+Humidity.01))
```

```
## 
## Call:
## lm(formula = PM2.5 ~ ODIN.raw + Temperature.01 + Humidity.01, 
##     data = nighttime_merged.1hr)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -36.26  -7.74  -0.50   6.24  36.35 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -318.4230    15.1557   -21.0   <2e-16 ***
## ODIN.raw          0.9838     0.0282    34.9   <2e-16 ***
## Temperature.01   -5.5724     0.1506   -37.0   <2e-16 ***
## Humidity.01      -1.0555     0.0933   -11.3   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.4 on 236 degrees of freedom
##   (40 observations deleted due to missingness)
## Multiple R-squared:  0.902,	Adjusted R-squared:   0.9 
## F-statistic:  722 on 3 and 236 DF,  p-value: <2e-16
```

```r
### Plot the change in calibration for PM$_{2.5}$
all_merged.1hr$ODIN<-predict(odin1.lm.full.1hr.pm2.5,
                             newdata = all_merged.1hr)
all_merged.1hr$ODIN.day<-predict(odin1.lm.daytime.1hr.pm2.5,
                                 newdata = all_merged.1hr)
all_merged.1hr$ODIN.night<-predict(odin1.lm.nighttime.1hr.pm2.5,
                                   newdata = all_merged.1hr)
pdf('raw_odin_fdms.pdf',width = 12,height = 6)
timePlot(all_merged.1hr,
         pollutant = c('PM2.5','ODIN.raw'),
         avg.time='1 hour',group=FALSE,xlab='',ylab='Dust',
         main='Raw ODIN output')
dev.off()
```

```
## pdf 
##   2
```

```r
pdf('cal_odin_fdms_1hr.pdf',width = 12,height = 3)
timePlot(all_merged.1hr,
         pollutant = c('PM2.5','ODIN','ODIN.day','ODIN.night'),
         avg.time='1 hour',group=TRUE,xlab='',ylab='PM2.5 [ug/m3]',
         main='Calibration Comparison')
dev.off()
```

```
## pdf 
##   2
```

```r
pdf('cal_raw_odin_fdms_1dy.pdf',width = 12,height = 3)
timePlot(all_merged.1hr,
         pollutant = c('PM2.5','ODIN','ODIN.day','ODIN.night'),
         avg.time='1 day',group=TRUE,xlab='',ylab='PM2.5 [ug/m3]',
         main='')
dev.off()
```

```
## pdf 
##   2
```

```r
# System information
sessionInfo()
```

```
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-redhat-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_NZ.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_NZ.UTF-8        LC_COLLATE=en_NZ.UTF-8    
##  [5] LC_MONETARY=en_NZ.UTF-8    LC_MESSAGES=en_NZ.UTF-8   
##  [7] LC_PAPER=en_NZ.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_NZ.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] openair_1.0
## 
## loaded via a namespace (and not attached):
##  [1] cluster_1.15.2      digest_0.6.4        evaluate_0.5.5     
##  [4] formatR_0.10        grid_3.1.0          htmltools_0.2.4    
##  [7] knitr_1.6           lattice_0.20-29     latticeExtra_0.6-26
## [10] Matrix_1.1-3        mgcv_1.7-29         nlme_3.1-117       
## [13] plyr_1.8.1          RColorBrewer_1.0-5  Rcpp_0.11.1        
## [16] reshape2_1.4        rmarkdown_0.2.49    stringr_0.6.2      
## [19] tools_3.1.0         yaml_2.1.13
```
