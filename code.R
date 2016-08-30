#' playing with yorkr (cricket data)
#' goal: predict win prob in odi or 20/20
#' 
library(yorkr)
library(dplyr)
library(tidyr)
library(ggplot2)
#convertYaml2RDataframe("225171.yaml","./source","./data")
getMatchDetails("Australia","Afghanistan","2015-03-04")
load("Australia-Afghanistan-2015-03-04.RData")
d1=overs
tbl_df(d1)
teamBattingScorecardMatch(d1,"Australia")
teamBattingScorecardMatch(d1,"Afghanistan")
glimpse(d1)
d1 %>% select(ball,noballs,wides,totalRuns,wicketKind) %>% 
  separate(ball,into=c("inns","over","ball")) %>% 
  mutate(over=as.numeric(over),
         ball=as.numeric(ball)) %>% 
  mutate(isWkt=as.numeric(wicketKind!="not-out")) %>% 
  group_by(inns) %>% 
  mutate(runs=cumsum(totalRuns),wkts=cumsum(isWkt),bls=6*over+ball) %>% 
  select(inns,runs,wkts,bls) -> d2
d2
d2 %>% summarize(runTotal=max(runs),
                 wicketTotal=max(wkts)) -> d3
# next what I want is: 
# number of balls left; number of wickets left; number more runs
left_join(d3,d2) %>% 
  mutate(moreBalls=300-bls,
              moreWickets=10-wkts,
              moreRuns=runTotal-runs) %>% 
  select(inns,moreBalls,moreWickets,moreRuns) -> d4

d4

# this will do for predicting runs scored in 1st innings
# for 2nd innings, want more balls, more wickets, win
# also have to look at only matches that lasted all 50 overs

# this may also work for carter-guthrie with (maybe) less processing: balls left, wickets left, # runs, wicket

# amass into function to handle one match


getMatch=function(fname) {
  load(fname)
  overs %>% select(ball,noballs,wides,totalRuns,wicketKind) %>% 
    separate(ball,into=c("inns","over","ball")) %>% 
    mutate(over=as.numeric(over),
           ball=as.numeric(ball)) %>% 
    mutate(isWkt=as.numeric(wicketKind!="not-out")) %>% 
    group_by(inns) %>% 
    mutate(runs=cumsum(totalRuns),wkts=cumsum(isWkt),bls=6*over+ball) %>% 
    select(inns,runs,wkts,bls) -> d2
  d2 %>% summarize(runTotal=max(runs),
                   wicketTotal=max(wkts)) -> d3
  # next what I want is: 
  # number of balls left; number of wickets left; number more runs
  left_join(d3,d2) %>% 
    mutate(moreBalls=300-bls,
           moreWickets=10-wkts,
           moreRuns=runTotal-runs) %>% 
    select(inns,moreBalls,moreWickets,moreRuns) -> d4
  
  d4
}

fname="Australia-Afghanistan-2015-03-04.RData"
getMatch(fname)

## get all the files I have

files=list.files(pattern = "[0-9].RData$")

# get the type of a match

getMatchType=function(fname) {
  load(fname) # loads into df called "overs"
  data.frame(fname=fname,type=as.character(overs$matchType[1]))
}

# test

getMatchType(fname)

# get data frame with all match types ODI

odis=bind_rows(lapply(files,getMatchType)) %>% filter(type=="ODI")

x=lapply(odis$fname,getMatch) # list of data frames one per match
y=bind_rows(x) # 600,000 rows!

y %>% filter(inns=="1st")


# need to find out if match complete
# this is d3 from getMatch

# pull out as matchSummary

matchSummary=function(fname) { # this is copy-paste programming :-(
  load(fname)
  overs %>% select(ball,noballs,wides,totalRuns,wicketKind) %>% 
    separate(ball,into=c("inns","over","ball")) %>% 
    mutate(over=as.numeric(over),
           ball=as.numeric(ball)) %>% 
    mutate(isWkt=as.numeric(wicketKind!="not-out")) %>% 
    group_by(inns) %>% 
    mutate(runs=cumsum(totalRuns),wkts=cumsum(isWkt),bls=6*over+ball) %>% 
    select(inns,runs,wkts,bls) -> d2
  # need to summarize overs also
  d2 %>% summarize(runTotal=max(runs),
                   wicketTotal=max(wkts),
                   ballTotal=max(bls)) -> d3
  data.frame(fname,cbind(d3[1,],d3[2,]))
}

b=matchSummary(files[1])
b

xx=lapply(odis$fname,matchSummary)
yy=bind_rows(xx)
yy
names(yy)

# check to see whether match complete:
# (a) if wickets for 1st team<10, then ballTotal>=300
# (b) if wickets for 2nd team<10 and runs (2nd) < runs (1st), then ballTotal(2nd)>=300
# anything else?

yy[1,]

isComplete=function(v) {
  firstComplete=ifelse(v["wicketTotal"]<10,ifelse(v["ballTotal"]>=300,T,F),T)
  secondComplete=ifelse(v["wicketTotal.1"]<10,ifelse(v["runTotal.1"]<=v["runTotal"],ifelse(v["ballTotal.1"]>=300,T,F),T),T)
  firstComplete & secondComplete
}

complete=apply(yy,1,isComplete)
yy %>% mutate(complete=complete) %>% filter(complete) -> yy1
tbl_df(yy1)

# get matches again

odis=bind_rows(lapply(yy1$fname,getMatchType)) %>% filter(type=="ODI")

x=lapply(odis$fname,getMatch) # list of data frames one per match
y=bind_rows(x) # 600,000 rows, nearly!
y %>% filter(inns=="1st") %>% gather(xname,x,moreBalls:moreWickets) %>% 
  ggplot(aes(x=x,y=moreRuns))+geom_point()+geom_smooth(se=F)+
  facet_wrap(~xname,scales='free') # this takes time

# moreRuns increases faster than linear with more wickets left
# moreRuns more or less linear in moreBalls

y %>% ggplot(aes(x=moreWickets,y=moreBalls))+geom_point()

y %>% lm(moreRuns~moreWickets+I(moreWickets^2)+moreBalls,data=.) -> y.1

new=expand.grid(moreWickets=0:10,moreBalls=seq(0,300,50))
p=predict(y.1,new)
data.frame(new,p)

# doesn't make too much sense at edges. Try Carter-Guthrie idea.

############################################################

memoization

########################################################

library(memoise)

bico=function(x) {
  # binomial coefficient x[1] choose x[2]
  if (x[2]==0) return(1)
  if (x[2]==x[1]) return(1)
  bico(c(x[1]-1,x[2]-1))+bico(c(x[1]-1,x[2]))
}
system.time(bico(c(30,8)))
mem.bico=memoise(bico)
system.time(mem.bico(c(20,8)))
system.time(mem.bico(c(30,8)))
system.time(mem.bico(c(33,10)))
system.time(mem.bico(c(20,6)))

##########################################################

environment

env=new.env(parent=emptyenv())
env$x=1:12
env$y=letters[1:10]

changenth=function(i=4) {
  tmp=get("x",envir=env)
  tmp[i]=99
  assign("x",value=tmp,envir=env)
  tmp2=get("y",envir=env)
  tmp2[i]="xx"
  assign("y",value=tmp2,envir=env)
}

changenth(4)
?get
env$x
env$y
changenth(6)

################### array

a=array(0,dim=c(2,3,4))
a
a[2,3,1]=99
a[2,1,2]=999
a
as.data.frame(a)
?array
dimnames(a)=list(1:2,letters[1:3],LETTERS[1:4])
a
as.data.frame(a)
a %>% separate()