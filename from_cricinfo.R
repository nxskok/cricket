library(tidyr)
library(dplyr)
library(ggplot2)
# run perl getTable.pl to get over comparison table on url like
#   http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913655.html?view=comparison
url="http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913653.html" # eng paskistan #1 (eng won by DL)
url="http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913655.html" # eng pakistan #2
url="http://www.espncricinfo.com/sri-lanka-v-australia-2016/engine/match/995457.html" # sri lanka australia
url="http://www.espncricinfo.com/sri-lanka-v-australia-2016/engine/match/995459.html" # sri lanka australia
url="http://www.espncricinfo.com/india-v-south-africa-2015-16/engine/match/903593.html" # s africa india, thrown away by india
url="http://www.espncricinfo.com/ci/engine/match/995461.html" # sri lanka australia
url="http://www.espncricinfo.com/ci/engine/match/947511.html" # warks somerset
url="http://www.espncricinfo.com/ci/engine/match/947509.html" # surrey yorks
url="http://www.espncricinfo.com/ci/engine/match/947505.html" # northants surrey: impossible win!
url="http://www.espncricinfo.com/ci/engine/match/947503.html" # warks essex
url="http://www.espncricinfo.com/ci/engine/match/947501.html" # worcs somerset
url="http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913657.html" # eng pak 3rd odi
url="http://www.espncricinfo.com/sri-lanka-v-australia-2016/engine/match/995463.html" # sl aus 4th odi
url="http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913659.html" # eng pak 4th odi
url="http://www.espncricinfo.com/sri-lanka-v-australia-2016/engine/match/995465.html" # aus sl 5th odi 
url="http://www.espncricinfo.com/england-v-pakistan-2016/engine/match/913661.html" # eng pak 5th odi

ans=makeGraph(url)
ans$g
ans$match %>% filter(runs>0 | wickets<10)


makeGraph=function(url) {
  urlvc=paste0(url,"?view=comparison")
  system(paste("perl getTable.pl",url," > match.csv"))
  # read teams
  teams=read.table("teams.txt",header=T,sep="\t")
  team=as.character(teams$teams)
  match=read.csv("match.csv",header=T,skip=2)
  match %>% dplyr::select(c(Over,Score,Score.1)) %>% gather(which,score,-Over) %>% 
    separate(score,into=c("runs","wickets"),sep="/") %>% filter(!is.na(wickets)) -> match.all
  match.all %>% filter(which=="Score") -> match.1
  match.all %>% filter(which=="Score.1") -> match.2
  d1=all.first(match.1)
  if (any(!is.na(match.2$runs))) {
    d2=all.second(match.1,match.2)
    match=dplyr::bind_rows(d1,d2)
  } else {
    match=d1
  }
  myBreaks=seq(0,1,0.10)
  match %>% filter(runs>0) %>% 
    ggplot(aes(x=Over,y=probability,colour=which))+geom_point()+geom_line()+
    geom_hline(yintercept = 0.5,col="black") +
    geom_hline(yintercept = 0.0,col="black") +
    geom_hline(yintercept = 1.0,col="black") +
    scale_color_manual("Teams",values=c("red","blue"),labels=team) +
    scale_y_continuous(breaks=myBreaks,labels=myBreaks) -> g
  list(g=g,match=match)
}

