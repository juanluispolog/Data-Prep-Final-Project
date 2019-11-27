suppressMessages(library(dplyr))
library(ggplot2)

###################################################################################################################################


# load game result data into season-by-season dfs and into one "master" df with all seasons

for (i in c('0910','1011','1112','1213','1314','1415','1516','1617','1718','1819','1920'))
{
  assign(paste('s',i,sep=''), select(read.csv(paste("~/school/CSP 571/Project/EngPrimLg/data/season-",i,"_csv.csv",sep=''),
                                              header=TRUE),Date:AY,-c(Referee,HTHG,HTAG,HTR)))
}

Past.Master<-rbind(s0910,s1011,s1112,s1213,s1314,s1415,s1516,s1617,s1718,s1819) #df with all seasons in time-decending order


#load the betting data for full time results into season-by-season dfs for use later on
for (i in c('1516','1617','1718','1819','1920'))
{
  assign(paste('bets',i,sep=''), select(read.csv(paste("~/school/CSP 571/Project/EngPrimLg/data/season-",i,"_csv.csv",sep=''),
                                                 header=TRUE),Date,B365H:VCA))
}


# Example of how to get data for each hteam (2015-16 season)
for (i in levels(s1516$HomeTeam))
{
  assign(paste('s1516.',i,sep=''),filter(s1516,HomeTeam==i|AwayTeam==i))
}

###################################################################################################################################


# check out the match result precentages over time graphically

homeWins<-c(sum(s0910$FTR=='H')/380,sum(s1011$FTR=='H')/380,sum(s1112$FTR=='H')/380,sum(s1213$FTR=='H')/380,sum(s1314$FTR=='H')/380,
            sum(s1415$FTR=='H')/380,sum(s1516$FTR=='H')/380,sum(s1617$FTR=='H')/380,sum(s1718$FTR=='H')/380,sum(s1819$FTR=='H')/380)
homeWins[11]<-sum(s1920$FTR=='H')/120

awayWins<-c(sum(s0910$FTR=='A')/380,sum(s1011$FTR=='A')/380,sum(s1112$FTR=='A')/380,sum(s1213$FTR=='A')/380,sum(s1314$FTR=='A')/380,
            sum(s1415$FTR=='A')/380,sum(s1516$FTR=='A')/380,sum(s1617$FTR=='A')/380,sum(s1718$FTR=='A')/380,sum(s1819$FTR=='A')/380)
awayWins[11]<-sum(s1920$FTR=='A')/120

draws<-c(sum(s0910$FTR=='D')/380,sum(s1011$FTR=='D')/380,sum(s1112$FTR=='D')/380,sum(s1213$FTR=='D')/380,sum(s1314$FTR=='D')/380,
         sum(s1415$FTR=='D')/380,sum(s1516$FTR=='D')/380,sum(s1617$FTR=='D')/380,sum(s1718$FTR=='D')/380,sum(s1819$FTR=='D')/380)
draws[11]<-sum(s1920$FTR=='D')/120

#check that they sum to 1
homeWins+awayWins+draws

season<-c('2009-10','2010-11','2011-12','2012-13','2013-14','2014-15','2015-16','2016-17','2017-18','2018-19','2019-20')
hadPercentages<-data.frame(season,homeWins,awayWins,draws,1:11)

# plot the values over time
ggplot(hadPercentages, aes(season, color = variable)) + 
  geom_line(aes(y = homeWins, color = "home wins", group = "home wins"),size=1.3) +
  geom_line(aes(y = awayWins, color = "away wins", group = "away wins"),size=1.3) +
  geom_line(aes(y = draws, color = "draws", group = "draws"),size=1.3) +
  geom_point(aes(y = homeWins, col = "home wins"),size=2) + 
  geom_point(aes(y = awayWins, col = "away wins"),size=2) +
  geom_point(aes(y = draws, col = "draws"),size=2)+labs(title="Match Outcome Percentages 2009-10 -- 2019-20")+xlab("Season")+ylab('Percentage')


###################################################################################################################################


#function for counting the counts of each number of occurrences for a feature (G,Y,C,,ST,F)
#counts up and plots a histogram of the desired game event for home, away, and total

featCount<-function(data,feature){
  # MUST HAVE DATA READY IN A DATA FRAME TO USE THIS FUNCTION
  # data is the data frame with the league or team data you want to count counts for
  # feature is a string with single capital letter: 'Y' yellow cards, "C" corners, "F" fouls, "G" goals,"S" shots,"ST" shots on target

  n=dim(data)[1]
  #event
  if(feature=="G"){event="Goals"}
  else if(feature=="Y"){event="Yellow Cards"}
  else if(feature=="C"){event="Corner Kicks"}
  else if(feature=="S"){event= 'Shots'}
  else if(feature=="ST"){event="Shots on Target"}
  else{event="Fouls"}
  
  
  #types
  if (feature=="G"){
    homeFeat<-paste('FTH',feature,sep='')
    awayFeat<-paste('FTA',feature,sep='')   
  }
  else{
    homeFeat<-paste('H',feature,sep='')
    awayFeat<-paste('A',feature,sep='')
  }
  
  #raw
  homeNumbs<- data[,homeFeat]
  awayNumbs<-data[,awayFeat]
  totalNumbs<-c(homeNumbs,awayNumbs)
  
  #means
  lamHome <- mean(homeNumbs)
  lamAway <- mean(awayNumbs)
  lamTotal<- mean(totalNumbs)
  
  
  #poisson points
  x.H=0:max(homeNumbs)
  x.A=0:max(awayNumbs)
  x.T=0:max(totalNumbs)
  
  y.H<-dpois(x.H,lamHome)
  y.A<-dpois(x.A,lamAway)
  y.T<-dpois(x.T,lamTotal)
  
  
  #proportional counts (pts)
  
  H<-c() #counts for home feature
  for (i in 0:max(homeNumbs)) {
    H[i+1]<-sum(homeNumbs==i)/n
    names(H)[i+1]=i
  }
  A<-c()
  for (i in 0:max(awayNumbs)) {
    A[i+1]<-sum(awayNumbs==i)/n
    names(A)[i+1]=i
  }
  Tot<-c()
  for (i in 0:max(totalNumbs)) {
    Tot[i+1]<-sum(totalNumbs==i)/(2*n)
    names(Tot)[i+1]=i
  }
  
  outputs<-list(lambdas=c(l.H=lamHome,l.A=lamAway,l.T=lamTotal),h=homeNumbs,a=awayNumbs,t=totalNumbs,hr=H,ar=A,tr=Tot)
  
  hist(homeNumbs,breaks=c(0,seq(from=0.99999,to=(max(homeNumbs)+.99999),by=1)),ylim=c(0,max(c(H,y.H))+0.005),xlab=event,main=paste('Home',event))
  points(x.H,H,lwd=2)
  lines(x.H,y.H,col=2,type='o',lwd=2)
  legend('topright',legend = c('Actual','Poisson'),lty=c(NA,1),pch=c(1,1),col=c(1,2))
  
  hist(awayNumbs,breaks=c(0,seq(from=0.99999,to=(max(awayNumbs)+.99999),by=1)),ylim=c(0,max(c(A,y.A))+0.005),xlab=event,main=paste('Away',event))
  points(x.A,A,lwd=2)
  lines(x.A,y.A,col=2,type='o',lwd=2)
  legend('topright',legend = c('Actual','Poisson'),lty=c(NA,1),pch=c(1,1),col=c(1,2))
  
  hist(totalNumbs,breaks=c(0,seq(from=0.99999,to=(max(totalNumbs)+.99999),by=1)),ylim=c(0,max(c(Tot,y.T))+0.005),xlab=event,main=paste('Total',event))
  points(x.T,Tot,lwd=2)
  lines(x.T,y.T,col=2,type='o',lwd=2)
  legend('topright',legend = c('Actual','Poisson'),lty=c(NA,1),pch=c(1,1),col=c(1,2))
  
}


#test it out
featCount(s1516,'ST')
featCount(s1516,'G')
featCount(s1516,'C')
featCount(s1516,'Y')
featCount(s1516,'S')
featCount(s1516,'F')


###################################################################################################################################

# histograms of proportional goal count with Poisonnian curve (EXAMPLE)

x=0:6
H<-c() #counts for home feature
for (i in 0:6) {
  H[i+1]<-sum(s1516$FTHG==i)/380
  names(H)[i+1]=i
}
pois.pts.df=data.frame(x=x,y=H)

x.pts=seq(from=0,to=6,length=10000)
lamh=mean(s1516$FTHG)
y.pts=exp(-lamh)*(lamh)^x.pts/gamma(1+x.pts)

pois.df=data.frame(x=x.pts,y=y.pts)

ggplot()+geom_histogram(data=s1516,colour='black',binwidth = 1,aes(x=FTHG,y=..density..,fill=..count..))+
  geom_point(data=pois.df,aes(x=x,y=y),size=0.5,col=2)+
  labs(title="2015-16 EPL Home Goals", y='Density', x="Full Time Home Goals")+
  geom_point(data=pois.pts.df,aes(x=x,y=y),col='black')


####################################################################################################################################

# modified dataframes for poisson regression


# previous 10 completed seasons

for (i in c('0910','1011','1112','1213','1314','1415','1516','1617','1718','1819'))
{
  assign(paste('s',i,'.mod',sep=''), as.data.frame(matrix(0,nrow=760,ncol=4)))
}

#function to set up modified dfs for poisson regression for each season

modDFs<-function(data1,data2){
  names(data2)<-c('Team','Opponent','Goals','Home')
  for (i in 1:380){
    
    #assign teams and opponents for each game from the point of view of each participating team      
    data2$Team[(2*i)-1]<-as.character(data1$HomeTeam)[i]  
    data2$Opponent[(2*i)-1]<-as.character(data1$AwayTeam)[i]
    data2$Team[2*i]<-as.character(data1$AwayTeam)[i]
    data2$Opponent[2*i]<-as.character(data1$HomeTeam)[i]
    
    #record goals scored by each team in each game
    data2$Goals[(2*i)-1]<-data1$FTHG[i]
    data2$Goals[(2*i)]<-data1$FTAG[i]
    
    #assign home team indicator variable
    data2$Home[(2*i)-1]=1
  }
  return(data2)
}


# Apply function

s0910.mod<-modDFs(s0910,s0910.mod)
s1011.mod<-modDFs(s1011,s1011.mod)
s1112.mod<-modDFs(s1112,s1112.mod)
s1213.mod<-modDFs(s1213,s1213.mod)
s1314.mod<-modDFs(s1314,s1314.mod)
s1415.mod<-modDFs(s1415,s1415.mod)
s1516.mod<-modDFs(s1516,s1516.mod)
s1617.mod<-modDFs(s1617,s1617.mod)
s1718.mod<-modDFs(s1718,s1718.mod)
s1819.mod<-modDFs(s1819,s1819.mod)


# do season 19-20 separately (because it is still ongoing)

s1920.mod<-as.data.frame(matrix(0,nrow=240,ncol=4))
names(s1920.mod)<-c('Team','Opponent','Goals','Home')

for (i in 1:120){
  
  #assign teams and opponents for each game from the point of view of each participating team      
  s1920.mod$Team[(2*i)-1]<-as.character(s1920$HomeTeam)[i]  
  s1920.mod$Opponent[(2*i)-1]<-as.character(s1920$AwayTeam)[i]
  s1920.mod$Team[2*i]<-as.character(s1920$AwayTeam)[i]
  s1920.mod$Opponent[2*i]<-as.character(s1920$HomeTeam)[i]
  
  #record goals scored by each team in each game
  s1920.mod$Goals[(2*i)-1]<-s1920$FTHG[i]
  s1920.mod$Goals[(2*i)]<-s1920$FTAG[i]
  
  #assign home team indicator variable
  s1920.mod$Home[(2*i)-1]=1
}



####################################################################################################################################


# look at variation in fitted parameters for teams, home advantage, avg score etc when a WHOLE SEASON's data is used

#storage
ParamHome<-c()
manuAttacks<-c()
manuDefs<-c()
liverpoolAttacks<-c()
liverpoolDefs<-c()
chelseaAttacks<-c()
chelseaDefs<-c()

# list of data to loop over
whole.seasons<-list(s0910.mod,s1011.mod,s1112.mod,s1213.mod,s1314.mod,s1415.mod,s1516.mod,s1617.mod,s1718.mod,s1819.mod)

seasons<-c("0910",'1011','1112','1213','1314','1415','1516','1617','1718','1819')

#loop through each season, model using whole season data, then take the desired param and store it with the right vector
for (i in 1:10){
  model<-glm(Goals~Team+Opponent+Home,data=whole.seasons[[i]],family=poisson(link='log'))
  ParamHome[i]=model$coefficients['Home']
  manuAttacks[i]=model$coefficients['TeamMan United']
  manuDefs[i]=model$coefficients['OpponentMan United']
  liverpoolAttacks[i]=model$coefficients['TeamLiverpool']
  liverpoolDefs[i]=model$coefficients["OpponentLiverpool"]
  chelseaAttacks[i]=model$coefficients['TeamChelsea']
  chelseaDefs[i]=model$coefficients['OpponentChelsea']
}


# store data in a df for ggplotting
teams<-data.frame(seasons, manuAttacks,manuDefs,liverpoolAttacks,liverpoolDefs,chelseaAttacks,chelseaDefs)

#ggplots
ggplot(teams, aes(seasons, color = variable)) + 
  geom_line(aes(y = manuAttacks, color = "Man United Atk", group = "Man United Atk"),size=1) +
  geom_line(aes(y = manuDefs, color = "Man United Def", group = "Man United Def"),size=1) +
  ggtitle('Manchester United Parameters Over Time')+xlab("Season")+ylab("Parameter Value")


ggplot(teams, aes(seasons, color = variable)) + 
  geom_line(aes(y = liverpoolAttacks, color = "Liverpool Atk", group = "Liverpool Atk"),size=1) +
  geom_line(aes(y = liverpoolDefs, color = "Liverpool Def", group = "Liverpool Def"),size=1) + scale_colour_brewer(palette = "Set2") 


####################################################################################################################################

# Predictions! This code was easily modified to check different seasons and look back windows and is meant as an example

# Predictions over the course of the 2018-19 season using last 10 weeks starting at week 11

lambda.list=list()
acc<-c()
mod.acc<-c()
manuAttacks<-c()
manuDefs<-c()
matchweek=11:38
#season to date starting at week 11
for (i in 11:38){
  #train model
  training.model.start <- glm(Goals~Team+Opponent+Home,data=s1819.mod[((20*(i-1))-199):(20*(i-1)),],family=poisson(link='log'))
  manuAttacks[i-10]=training.model.start$coefficients["TeamMan United"]
  manuDefs[i-10]=training.model.start$coefficients['OpponentMan United']
  #redict lambdas
  lambda.start<-c()
  for (j in ((20*i)-19):(20*i)){
    lambda.start[j-(20*(i-1))]=predict(training.model.start, data.frame(Home=s1819.mod$Home[j], Team=s1819.mod$Team[j], Opponent=s1819.mod$Opponent[j]), type="response")
    
  }
  lambda.list[[i-10]]=lambda.start
  
  #matrix of outcome probabilities
  probs.matrix<-c()
  for(k in 1:10){
    #storage matrix
    Z=matrix(0,nrow = 7,ncol=7)
    row.names(Z)<-0:6
    colnames(Z)<-0:6
    
    #lambdas for game i (home and away respectively)
    lamH=lambda.start[2*k-1]
    lamA=lambda.start[2*k]
    
    #Probabilities from the poisson distribution
    range=0:6
    pHome=dpois(0:6,lamH)
    pAway=dpois(0:6,lamA)
    
    #fill in z matrix with probabilities (home in the rows, away in the columns)
    for (l in 1:7){
      for (m in 1:7){
        Z[l,m]=pHome[l]*pAway[m]  }}
    
    #use the matrix of probabilities above to calculate probabilities
    ph<-c()
    for(b in 1:6){
      
      ph[b]= sum(Z[(b+1):7,b])
      
    }
    p.homewin=sum(ph)
    
    pa<-c()
    for(b in 1:6){
      
      pa[b]= sum(Z[b,(b+1):7])
      
    }
    p.awaywin=sum(pa)
    
    p.draw=1-p.homewin-p.awaywin
    
    probs<- c(p.homewin,p.awaywin,p.draw)
    
    #row bind this vector to the bottom of the probability matrix
    
    probs.matrix=rbind(probs.matrix,probs)
  }
  
  # predictions with base model
  predictions<-c()
  for (c in 1:10){
    
    if (which.max(probs.matrix[c,])==1){predictions[c]='H'}
    else if (which.max(probs.matrix[c,])==2){predictions[c]='A'}
    else{predictions[c]='D'}
  }
  
  #modified predictions
  mod.pred<-predictions
  for (d in 1:10)
  {
    if(max(probs.matrix[d,])<0.66)
    {
      mod.pred[d]='D'
    }
  }
  keystone<-cbind(probs.matrix, predictions,mod.pred, as.character(s1819$FTR[(10*i-9):(10*i)]))
  colnames(keystone)[6]='actual'
  
  acc[i-10]=sum(keystone[,'predictions']==keystone[,'actual'])/10
  mod.acc[i-10]=sum(keystone[,'mod.pred']==keystone[,'actual'])/10
  
}

manuAttacks

manu<-data.frame(matchweek,manuAttacks,manuDefs)


ggplot(manu, aes(matchweek, color = variable)) + 
  geom_line(aes(y = manuAttacks, color = "Man United Atk", group = "Man United Atk"),size=1) +
  geom_line(aes(y = manuDefs, color = "Man United Def", group = "Man United Def"),size=1)+ggtitle("Manchester United Parameters Across the 2018-19 Season",subtitle = 'Model fit using prior 10 matchweeks')+xlab("Matchweek")+ylab("Parameter Value")

acc
mod.acc


acc.frame=data.frame(11:38,acc)
acc.frame
mod.acc.frame=data.frame(11:38,mod.acc)
mod.acc.frame
mean(acc)
mean(mod.acc)

accuracy<-data.frame(11:38,acc,mod.acc)
accuracy.melted<-data.frame(rep(11:38,2),c(acc,mod.acc),c(rep('base',28),rep('modified',28)))
names(accuracy.melted)<-c("matchweek",'accuracy','model')
accuracy.melted

ggplot(data=accuracy.melted,aes(x=matchweek, y=accuracy,fill=model))+geom_bar(position='identity',stat='identity',alpha=0.7)+
  ggtitle("Accuracy of Base and Modified Prediction Models")+xlab('Matchweek')+ylab('Accuracy (%)')



par(mfrow=c(1,1))
ggplot(data=acc.frame,aes(x=X11.38,y=acc))+geom_point(col='firebrick',size=2)+geom_line(col='firebrick',size=1.2)+
  xlab('matchweek')+ylab('prediction accuracy')+ geom_hline(yintercept=1/3, linetype="dashed", size=1.2)

ggplot(data=mod.acc.frame,aes(x=X11.38,y=mod.acc))+geom_point(col='steelblue',size=2)+geom_line(col='steelblue',size=1.2)+
  xlab('matchweek')+ylab('prediction accuracy')+ geom_hline(yintercept=1/3, linetype="dashed", size=1.2)