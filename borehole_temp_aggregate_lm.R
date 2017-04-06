#################################
#
# read and summarize soil temp
# from AK borehole sites for 
# McCulloch analyses
#
# MML 03/30/17
#################################

rm(list=ls())
require('lubridate')
require('plyr')

setwd('/Users/mloranty/Google Drive/Documents/Research/NSF_VIPER_2015-18/borehole_data/viper_stations_15_16/')
# ############ first aggregate Healy data to daily ############
# ## data are from http://www.lter.uaf.edu/data/data-detail/id/517 ###
# hly <- read.csv('original_daily/HLY_30min_2013_2015.csv',header=T)
# hly.d <- aggregate(hly[,c(3:6,8:22)],by=list(paste0(hly$year,hly$doy)),FUN=mean)
# ## aggregate replicate data ##
# hly.d$d0.05.m <- rowMeans(hly.d[,c(6,11,16)])
# hly.d$d0.10.m <- rowMeans(hly.d[,c(7,12,17)])
# hly.d$d0.20.m <- rowMeans(hly.d[,c(8,13,18)])
# hly.d$d0.30.m <- rowMeans(hly.d[,c(9,14,19)])
# hly.d$d0.40.m <- rowMeans(hly.d[,c(10,15,20)])
# write.csv(hly.d[,c(2:5,21:25)],file="csv/HLY_daily_13_15.csv",row.names=F)
######### now read in all data from Kholodov #############
f.name <- unlist(lapply(strsplit(list.files(path='csv'),'_'),`[[`,1))
s.name <- lapply(strsplit(list.files(path='csv'),'_'),`[[`,1)
f.path <- list.files(path='csv',full.name=T)

## read file with basic site sampling info  
site <- read.csv('site_info.csv',header=T)  

# vector match site data with borehole temp file names
ref <- c(20,21,8,14,15,15,15,16,17,9,24,19,12,11,1,4,3,2)

## create a list with all of the temp data 
dat <- list()
site.info <- list()
msmt.info <-   list()
for(i in 1:length(f.path))
{
  dat[[i]] <- read.csv(f.path[i],header=T,skip=12)
  site.info[[i]] <- read.csv(f.path[i],header=F,skip=12,nrow=1,colClasses="character")
  msmt.info[[i]] <- read.csv(f.path[i],header=F,skip=11,nrow=1,colClasses="character")
}

### get rid of unwanted data ###
surf.id <- list()
air.id <- list()
shit <- list()
final <- list()
use.me <- list()
depth <- list()

for(i in 1:length(f.path))
{
  surf.id[[i]] <- grepl("-",site.info[[i]])
  air.id[[i]] <- gsub("[m]","",site.info[[i]])
  shit[[i]] <- grepl("\\d",air.id[[i]])
  final[[i]] <- ifelse(surf.id[[i]]==FALSE&shit[[i]]==TRUE,1,0)
  use.me[[i]] <- dat[[i]][,final[[i]]==1]
  depth[[i]] <- as.numeric(air.id[[i]][final[[i]]==1])
}

# subset data from less than 40 cm depth
#head(use.me[[1]][,depth[[1]]<0.30&depth[[1]]>0.05])
dat40 <- list()
depth40 <- list()
for(i in 1:length(f.path))
{
  dat40[[i]] <- use.me[[i]][,depth[[i]]<0.40]
  depth40[[i]] <- depth[[i]][depth[[i]]<0.40]
}


###################################################################
######## now aggregate and calculate metrics for analysis #########
## identify subset of data pre-sampling in 2015
j <- list() # identify rec for 01/01/2015 for TDD calcs
s <- list() # identify date rec for date of sampling
t1 <- list()
t2 <-  list()
for(i in 1:length(f.path))
{
  j[[i]] <- which(dat[[i]]$year==2015 & dat[[i]]$month==1 & dat[[i]]$day==1)
  s[[i]] <- which(dat[[i]]$year==2015 & dat[[i]]$month==site$Month[ref[i]] & dat[[i]]$day==site$Day[ref[i]])
  t1[[i]] <- which(dat[[i]]$year==2014 & dat[[i]]$month==6 & dat[[i]]$day==1)
  t2[[i]] <- which(dat[[i]]$year==2015 & dat[[i]]$month==5 & dat[[i]]$day==31)
}

#functions to calculate number of thaw days, and thawing degree days
thaw.f <- function(x){length(which(x>0))}
tdd.f <- function(x){y <- which(x>0)
return(sum(x[y]))}

## calculate stats
m.temp <- list()
m.temp.na <- list()
thaw <- list()
tdd <- list()
## calculate thawing degree/ number of thawed days for period preceding root sampling
## and mean temp for 6/2014 - 5/2015
for(i in 1:length(dat))
{
  if(length(t1[[i]])==1)
    {m.temp[[i]] <- colMeans(dat40[[i]][t1[[i]]:t2[[i]],],na.rm=T)}
  else{m.temp[[i]] <- rep(NA,length(depth40[[i]]))}
  
  if(length(t1[[i]])==1)
  {m.temp.na[[i]] <- colMeans(dat40[[i]][t1[[i]]:t2[[i]],],na.rm=F)}
  else{m.temp.na[[i]] <- rep(NA,length(depth40[[i]]))}
  
  thaw[[i]] <- apply(dat40[[i]][j[[i]]:s[[i]],],2,thaw.f)
  tdd[[i]] <- apply(dat40[[i]][j[[i]]:s[[i]],],2,tdd.f)
}

## make a list of site names
site.out <- list()
for(i in 1:length(dat))
{
  site.out[[i]] <- rep(s.name[[i]],length(depth40[[i]]))
}
## make a data frame with all of this data ##
dat.out <- data.frame(unlist(site.out))
dat.out$depth <- unlist(depth40)
dat.out$tdd <- unlist(tdd)
dat.out$thaw <- unlist(thaw)
dat.out$temp <- unlist(m.temp)
dat.out$temp.na <- unlist(m.temp.na)



#### JUNK CODE #####
## calculate mean annual temp for all depths for 2yrs of data
mat <- list(colMeans(dat[[1]][,2:ncol(dat[[1]])]))
for(i in 2:length(dat))
{
  mat[[i]] <- colMeans(dat[[i]][,2:ncol(dat[[i]])])
}
