# as of Aug 4
# actually run this for good, changing name of global env to env2

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
#library(MASS)
detach("package:MASS",unload=T)
library(yorkr)

## ------------------------------------------------------------------------
readMatch=function(fname) {
  load(fname)
  overs
}

## ------------------------------------------------------------------------
fname="Afghanistan-Bangladesh-2015-02-18.RData"
d=readMatch(fname)
glimpse(d)

## ------------------------------------------------------------------------
matchInfo=function(fname) {
  d=readMatch(fname)
  data.frame(fname=fname,date=d$date[1],type=d$matchType[1])
}

## ------------------------------------------------------------------------
matchInfo(fname)

## ------------------------------------------------------------------------
processMatch=function(fname) {
  readMatch(fname) %>% select(ball,noballs,wides,totalRuns,wicketKind) %>% 
    separate(ball,into=c("inns","over","ball")) %>% 
    group_by(inns) %>% 
    mutate(over=as.numeric(over),
           ball=as.numeric(ball),
           isWkt=(wicketKind!="not-out"),
           wktDown=cumsum(isWkt),
           isExtra=(noballs+wides>0),
           totalBalls=6*over+ball) %>%
    select(inns,totalBalls,isExtra,isWkt,wktDown,totalRuns)
}

## ------------------------------------------------------------------------
d=processMatch(fname)
d

## ------------------------------------------------------------------------
summarizeMatch=function(fname) {
  processMatch(fname) %>% summarize(
                                 runTotal=max(cumsum(totalRuns)),
                                 wktTotal=max(wktDown),
                                 ballTotal=max(totalBalls)
                                 ) 
}

## ------------------------------------------------------------------------
summarizeMatch(fname)

## ------------------------------------------------------------------------
firstComplete=function(fname) {
  summarizeMatch(fname) %>% 
    mutate(complete=ifelse(wktTotal[1]==10,T,ifelse(ballTotal[1]>=300,T,F))) %>% 
    mutate(fname=fname) %>% 
    select(c(fname,complete)) %>% 
    slice(1)
}
firstComplete(fname)

## ----listfiles-----------------------------------------------------------
files=list.files(pattern = "[0-9].RData$")
head(files)

## ------------------------------------------------------------------------
atop=function(v,f) {
  xx=lapply(v,f)
  bind_rows(xx)
}

## ----firstcomplete-------------------------------------------------------
isComplete=atop(files,firstComplete)
isComplete

## ------------------------------------------------------------------------
isComplete %>% slice(3:9) -> tmp
atop(tmp$fname,summarizeMatch)

## ------------------------------------------------------------------------
v=atop(files,matchInfo)
head(v)
table(v$type)

## ----twoatop-------------------------------------------------------------
atop(files,matchInfo) %>% filter(type=="ODI") -> odis
atop(odis$fname,firstComplete) %>% filter(complete) -> complete.odis
complete.odis

## ----slow----------------------------------------------------------------
atop(complete.odis$fname,processMatch) %>% 
  filter(inns=="1st") -> d
d

## ------------------------------------------------------------------------
d %>% summarize(pEx=sum(isExtra)/nrow(d)) %>% select(pEx) -> pExtra
pExtra

## ------------------------------------------------------------------------
d %>% mutate(ballsLeft=300-totalBalls,
            wktsLeft=10-wktDown) %>%
  filter(!isExtra) -> d1
print(paste("Looking up",r,b,w))

## ----wicketModel---------------------------------------------------------
wModel=glm(isWkt~ballsLeft+wktsLeft+I(ballsLeft^2),data=d1,family="binomial") 
summary(wModel)

## ------------------------------------------------------------------------
new=expand.grid(wktsLeft=c(1,5,10),ballsLeft=c(1,100,200,300))
p=predict(wModel,new,type="response")
data.frame(new,p)

## ------------------------------------------------------------------------
d1 %>% mutate(rf=ordered(totalRuns)) %>% 
  filter(!isWkt) -> d2

## ------------------------------------------------------------------------
library(MASS)

## ----rModel--------------------------------------------------------------
rModel=polr(rf~ballsLeft+wktsLeft+I(ballsLeft^2),data=d2)

## ------------------------------------------------------------------------
p=predict(rModel,new,type="probs")
data.frame(new,round(p,3))

## ------------------------------------------------------------------------
new=data.frame(ballsLeft=3,wktsLeft=2)
pExtra
predict(wModel,new,type="response")
predict(rModel,new,type="probs")

## ------------------------------------------------------------------------
max.runs=400
max.balls=300
max.wickets=10
env2=new.env(parent=emptyenv())
# or make a a data frame and search it (see if data frameness preserved)
aa=data.frame(rr=integer(),bb=integer(),ww=integer(),F=double())
env2$lookupTable=aa
str(env2$lookupTable)

## ----bigF def------------------------------------------------------------
bigF=function(r,b,w) {
  # base cases
  if (r<0) return(0)
  if (b==0) return(1)
  if (w==0) return(1)
  # return value if in lookup table
  tab=get("lookupTable",envir=env2)
  tab %>% filter(rr==r, bb==b, ww==w) -> x
  if (nrow(x)>0) return(x[1,4])
  # recursion
  new=data.frame(ballsLeft=b,wktsLeft=w)
  pW=predict(wModel,new,type="response")
  p=predict(rModel,new,type="probs")
  pE=as.numeric(pExtra[1,1]) # global variable
  sum=0
  for (j in 0:6) {
    sum=sum+p[j+1]*bigF(r-j,b-1,w)
  }
  ans=pE*bigF(r-1,b,w)+(1-pE)*pW*bigF(r,b-1,w-1)+(1-pE)*(1-pW)*sum
  names(ans)=NULL
  # lookup table might have changed since earlier, so get again before altering
  tab=get("lookupTable",envir=env2)
  tab=rbind(tab,data.frame(rr=r,bb=b,ww=w,F=ans))
  assign("lookupTable",value=tab,envir=env2)
  return(ans)
}

## ----calcbigF------------------------------------------------------------
system.time(ans <- bigF(4,3,2))
ans

## ----showlookuptable-----------------------------------------------------
env2$lookupTable %>% arrange(bb,ww,rr)

## ----calcbigF2-----------------------------------------------------------
system.time(ans <- bigF(4,3,2))
ans

## ------------------------------------------------------------------------
system.time(bigF(10,10,10))

## ------------------------------------------------------------------------
env2$lookupTable
env2$lookupTable %>% filter(bb==8) %>% arrange(rr)
# 8 balls with eg 8, 9, 10 wickets

# table of what's been calculated
env2$lookupTable %>% dplyr::select(-F) %>% table()

# plot? Assume have all wickets:
env2$lookupTable %>% ggplot(aes(x=bb,y=rr))+geom_point()
# not very illuminating

# just get maxima

env2$lookupTable %>% summarize( mr=max(rr), mb=max(bb), mw=max(ww))


# view of some results
env2$lookupTable %>% filter(bb==40,ww==5) %>% arrange(rr)

system.time(bigF(10,10,8))
system.time(bigF(20,15,10))
system.time(bigF(30,20,10))
system.time(bigF(40,30,10))
system.time(bigF(60,45,10))
system.time(bigF(90,60,10))
system.time(bigF(120,90,10))
system.time(bigF(150,120,10))
system.time(bigF(170,130,10))
system.time(bigF(190,145,10))
system.time(bigF(210,165,10))
system.time(bigF(220,175,10))
system.time(bigF(240,200,10))
system.time(bigF(250,200,10))
system.time(bigF(260,210,10))
system.time(bigF(300,240,10))

for (i in 257:300) {
  ww=4
  bigF(i+60,i,ww)
  print(paste("Done",i+60,i,ww,"at",Sys.time()))
}

all=expand.grid(rr=0:400,bb=1:300,ww=1:10)
left_join(all,env2$lookupTable) %>% filter(is.na(F)) %>% filter(ww==min(ww)) %>% 
  filter(rr==min(rr)) %>% filter(bb==min(bb))
str(env2$lookupTable)

for (w in 10:10) {
  for (r in 400:444) {
    for (b in 1:300) {
      bigF(r,b,w)
    }
    print(paste("Done",r,w,"at",Sys.time()))
  }
  
}

# done!

lookup=function(r,b,w) {
  tab=get("lookupTable",envir=env2)
  tab %>% filter(rr==r,bb==b,ww==w) %>% select(F) %>% as.numeric()
}
lookup(250,300,10)

cdf=function(b,w) {
  tab=get("lookupTable",envir=env2)
  tab %>% filter(bb==b,ww==w) %>% arrange(rr) %>% select(rr,F) # %>% filter(F>1e-6,F<1-(1e-6))
}

prob=function(b,w) {
  tab=get("lookupTable",envir=env2)
  tab %>% filter(bb==b,ww==w) %>% arrange(rr) %>% select(rr,F) %>% # filter(F>1e-6,F<1-(1e-6)) %>%
    mutate(FF=lag(F,default=0),p=F-FF) %>% select(c(rr,p))
}
prob(10,3)

tab %>% filter(bb==10,ww==2) %>% arrange(rr) %>% select(rr,F) %>% # filter(F>1e-6,F<1-(1e-6)) %>% 
  mutate(FF=lag(F,default=0),p=F-FF)


prob(4,3,2)
sapply(0:10,prob,3,2)

greater=function(d1,d2) {
  gtr=outer(d1$x,d2$x,">")
  eql=outer(d1$x,d2$x,"==")
  less=outer(d1$x,d2$x,"<")
  p=outer(d1$p,d2$p,"*")
  c(sum(p*gtr),sum(p*eql),sum(p*less))
}

d1=data.frame(x=1:2,    p=c(0.6,0.4))
d2=data.frame(x=0:2,p=c(0.1,0.5,0.4))
greater(d1,d2)

prob.final=function(runs.now,balls.now,wickets.now) {
  balls.left=300-balls.now
  wickets.left=10-wickets.now
  runs=0:400
  p=sapply(runs,prob,balls.left,wickets.left)
  data.frame(rr=runs+runs.now,p=p)
}
prob.final(250,40*6,7)



cdf.final=function(runs.now,balls.now,wickets.now) {
  balls.left=300-balls.now
  wickets.left=10-wickets.now
  cdf(balls.left,wickets.left) %>% mutate(rr=rr+runs.now)
}

v=cdf.final(240,48*6,7)
v

mass=function(cumul) {
  c(cumul[1],diff(cumul))
}

prob.greater=function(F1,F2) {
  print(paste("dimensions of F1",dim(F1),"F2",dim(F2)))
  isg=outer(F1[,1],F2[,1],">")
  ise=outer(F1[,1],F2[,1],"==")
  isl=outer(F1[,1],F2[,1],"<")
  this=outer(mass(F1[,2]),mass(F2[,2]),"*")
  print(paste("dimensions of isg",dim(isg),"of this",dim(this)))
  c(sum(isg*this),sum(ise*this),sum(isl*this))
}


first.wins=function(runs.now,balls.now,wickets.now) {
  if (balls.now>=300 | wickets.now==10) {
    first.cdf=data.frame(rr=(runs.now-1):runs.now,F=0:1)
  } else 
  {
    first.cdf=cdf.final(runs.now,balls.now,wickets.now)
  }
  second.cdf=cdf(300,10)
  v=prob.greater(first.cdf,second.cdf)
  v[1]+0.5*v[2]
}

first.wins(250,40*6,3)
first.wins(280,50*6,8)

second.wins=function(first.runs,runs.now,balls.now,wickets.now) {
  P=cdf(300-balls.now,10-wickets.now)
  runs.to.tie=first.runs-runs.now
  if (runs.to.tie<0) return(1)
  P %>% filter(rr==runs.to.tie) %>% select(F) -> p
  if (nrow(p)>=1) {
    return(1-p)
  } else
  {
    return(0)
  }
}

second.wins(320,150,40*6,3)
# returns nothing?

first.runs=320
runs.now=150
balls.now=40*6
wickets.now=3



sapply(240:250,second.wins,150,40*6,3)

#### D&L examples

# sa 211(50); pak 74-2 (21); rain, 15 overs lost
second.wins(211,74,21*6,2) # 0.9085
second.wins(211,99,36*6,2) # 0.9067
# reduce target by 25 to 212-25=187 (higher d/l)
# or, they resume at 99-2 after 36 overs

# sa 236(50); eng 63-0 (12); 9 overs lost
second.wins(235,63,12*6,0) # 0.9850
second.wins(235,31,21*6,0) # 0.9846 odd:32 *more* runs needed
# so England resume at 31-0 after 21 overs!

# sl 127-1 (18.1), 8 overs lost
first.wins(127,18*6+1,1) # 0.8603
first.wins(155,(18+8)*6+1,1) # 0.8618
# sl should gain 28 runs to bring them to 50 overs (305+28=333)
# sl resume at 155-1 after 26.1 overs
second.wins(333,0,0,0) # 0.3120
second.wins(333,-3,6*8,0) # 0.3120 target 336????

# recent d-l examples?
# scotland hongkong
# no play, 20 over match possible (30overs/side lost)
# think of Scotland resuming after 30 overs with 0 wickets down, 50% chance of winning
first.wins(76,30*6,0) # 0.5001, 76-0
# scotland actually scored 153-6 in last 20 overs, total 229-6
second.wins(229,0,0,0) # 0.6759 is chance of HK winning at this point
# preserve this chance with 0 wkts down after 30 overs
second.wins(229,22,30*6,0) # 0.6771, HK start at 22-0, with a lot fewer free runs than Scotland had
# thus they need 208 runs in 20 overs!
# problem is the 0.5 to start, I think
# treat as 20 over match on ODI tables
second.wins(153,136,48*6,4) # 0.6672 with 2 overs left, so HK should have been declared winners


# auswi.1 and asuwi.2 from elsewhere
auswi.1

all.first=function(d) {
  n=nrow(d)
  prob=numeric(n)
  for (i in 1:n) {
    balls=as.numeric(d$Over[i])*6
    runs=as.numeric(d$runs[i])
    wickets=as.numeric(d$wickets[i])
    prob[i]=first.wins(runs,balls,wickets)
  }
  d$probability=prob
  d
}

all.first(auswi.1)

auswi.2

all.second=function(d1,d2) {
  tobeat=max(as.numeric(d1$runs))
  n=nrow(d2)
  prob=numeric(n)
  d2$wickets[is.na(d2$wickets)]=10
  d2$runs[d2$runs==""]=0
  for (i in 1:n) {
    balls=as.numeric(d2$Over[i])*6
    rruns=as.numeric(d2$runs[i])
    wickets=as.numeric(d2$wickets[i])
    if (is.na(tobeat)) tobeat=999
    if (rruns>tobeat) {
      prob[i]=1
    } else {
      prob[i]=second.wins(tobeat,rruns,balls,wickets)
    }
  }
  d2$probability=unlist(prob)
  d2
}

ww=all.second(auswi.1,auswi.2)

auswi.1$Over[4]
nrow(auswi.1)
auswi.2$wickets
max(as.numeric(auswi.1$runs))

############################################

convertYaml2RDataframe("913631.yaml") # this is England-Sri Lanka-2016-07-03.RData
# would like to be able to store the filename going with this match

d=processMatch("New Zealand-Pakistan-2014-12-08.RData")
glimpse(d)
info=matchInfo("New Zealand-Pakistan-2014-12-08.RData")
info
matchSummary("New Zealand-Pakistan-2014-12-08.RData") # team batting 2nd won by 3 wickets with 3 balls left
# convert into right form to calculate probs
# needs Over possibly decimal, runs, wickets for each innings
d


makeProbsDF=function(d) {
  # input is output from ProcessMatch
  d %>% group_by(inns) %>% mutate(Over=totalBalls/6,
                                  runs=cumsum(totalRuns),
                                  wickets=wktDown) %>% 
    select(Over:wickets) -> d0
  d0 %>% filter(inns=="1st") -> d1
  d0 %>% filter(inns=="2nd") -> d2
  d11=all.first(d1)
  d22=all.second(d1,d2)
  dd=bind_rows(d11,d22)
  dd
  #dd %>% ggplot(aes(x=Over,y=probability,colour=inns))+geom_point()+geom_line() -> g
  #g
}

dd=makeProbsDF(d)

makePlot=function(dd) {
  dd %>% ggplot(aes(x=Over,y=probability,colour=inns))+geom_point()+geom_line()+facet_wrap(~inns)
}

makePlot(dd)

makeExtrema=function(dd) {
  dd %>% mutate(probdiff=probability-lag(probability),line=row_number()) %>% 
    group_by(inns) %>% filter(probability==min(probability) |                                
                                probability==max(probability) |
                                probdiff==min(probdiff,na.rm=T) |
                                probdiff==max(probdiff,na.rm=T))
                                                      
}

print(makeExtrema(dd),n=50)


convertYaml2RDataframe("238170.yaml") # this is England-Sri Lanka-2016-07-03.RData
# would like to be able to store the filename going with this match

d=processMatch("Bangladesh-Sri Lanka-2006-02-25.RData")
glimpse(d)
info=matchInfo("Bangladesh-Sri Lanka-2006-02-25.RData")
info
matchSummary("Bangladesh-Sri Lanka-2006-02-25.RData") # lost by 78 runs
dd=makeProbsDF(d) 


convertYaml2RDataframe("913631.yaml") 
d=processMatch("England-Sri Lanka-2016-07-02.RData")
dd=makeProbsDF(d)
dd
makePlot(dd)
makeExtrema(dd)
dd %>% print(n=700)
