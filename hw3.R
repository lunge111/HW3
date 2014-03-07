#data processing
setwd("/media/Windows8_OS/linuxdl")
data=read.csv("train-sample.csv",stringsAsFactors=F)
names(data)
options( java.parameters = "-Xmx4g" )
library(openNLP)
library(NLP)
library(markdown)
library(XML)
invmarkdown<-sapply(data$BodyMarkdown,function(x) 
  markdownToHTML(text=x,fragment.only=TRUE))
toText<-function(h){
  doc=htmlParse(h)
  node=getNodeSet(doc,path="//p")
  k=sapply(node,xmlValue)
  paste(k,collapse=' ')
}
postday=as.Date(data$PostClosedDate,format='%m/%d/%Y')-as.Date(data$PostCreationDate,format='%m/%d/%Y')
head(postday)
userage=as.Date(data$PostCreationDate,format='%m/%d/%Y')-as.Date(data$OwnerCreationDate,format='%m/%d/%Y')
bodytexts=sapply(invmarkdown,toText)
bodyWordcount=sapply(strsplit(data$BodyMarkdown," "),length)
tileWordcout=sapply(strsplit(data$Title," "),length)
tagFreq<-function(v){
tt1=table(v)
stt1=sort(tt1,decreasing=T)
stt1[v]
}
names(bodytexts)
length(userage)
dat=data.frame(POSDAYS=postday,USERAGE=userage,REPUTANTION=as.numeric(data$ReputationAtPostCreation),
              UNDELETTIME=as.numeric(data$OwnerUndeletedAnswerCountAtPostTime),
              TITLEWORDS=tileWordcout,BODYWORDS=bodyWordcount,TAG1FREQ=tagFreq(data$Tag1),
              TAG2FREQ=tagFreq(data$Tag2),TAG3FREQ=tagFreq(data$Tag3),
              TAG4FREQ=tagFreq(data$Tag4),TAG5FREQ=tagFreq(data$Tag5),
              ISOPEN=(data$OpenStatus=="open"),stringsAsFactors=F)
names(dat)
dat$STATUS=rep(NA,140272)
dat$STATUS[dat$ISOPEN]="open"
dat$STATUS[!dat$ISOPEN]="closed"
dat=dat[,-12]
dat=dat[,-1]
dat[,1]=as.integer(dat[,1])
dat[is.na(dat)]=0
save(dat,file="dat.rda")
i=sample(1:140272)
dat=dat[i,]
train=dat[14028:140272,]
test=dat[1:14027,]

setwd("/media/Windows8_OS/linuxdl")
load("dat.rda")
library(rpart)
classtree=rpart(STATUS~., data=train,method='class')
pred=predict(classtree,test[,-11], type = 'class')

table(truth = test$STATUS, predict= pred)

logreg=glm(I(STATUS=="open")~., data=train,family="binomial")
summary(logreg)
a=apply(test[,-11],1,function(x) sum(x*logreg$coefficients[-1])-0.43345)
pred1=ifelse(a>0.5,"open","closed")
table(truth = test$STATUS, predict= pred1)




boost<-function(dat,j){
  #initialization
  n=nrow(dat)
 wei=rep(1/n,n)
 a=rep(0,j)
 fitlist=list()
 #looping
 for(i in 1:j){
   fit=rpart(STATUS~., data=dat,weights = wei ,method = 'class')
   pred=predict(fit,type='class')
   mis=(pred!=dat$STATUS)
   err=sum(wei[mis])/sum(wei)
   alpha=log((1-err)/err)
   wei=wei*exp(alpha*mis)
   fitlist[[i]]=fit
 a[i]=alpha
 }
return(list(fit = fitlist, alpha = a))
}

train$STATUS=ifelse(train$STATUS=="open",1,-1)
test$STATUS=ifelse(test$STATUS=="open",1,-1)
l=boost(train,100)

fitlist=l$fit
alpha=l$alpha
predmatrix=matrix(rep(0,100*nrow(test)),ncol=100)
for(i in 1:100){pred=predict(fitlist[[i]],test[,-11],type='class')
  predmatrix[, i] = alpha[i] * ifelse(pred == 1, 1, -1)                        
}
pred1=apply(predmatrix,1,sum)

result=ifelse(pred1>0,1,-1)
table(result,test$STATUS)



#NLP (fail, not run)
options( java.parameters = "-Xmx4g" )
library(openNLP)
library(NLP)
library(markdown)
library(XML)
invmarkdown<-sapply(data$BodyMarkdown,function(x) 
  markdownToHTML(text=x,fragment.only=TRUE))
toText<-function(h){
  doc=htmlParse(h)
  node=getNodeSet(doc,path="//p")
  k=sapply(node,xmlValue)
paste(k,collapse=' ')
}
library(parallel)
cl <- makeCluster(detectCores(),"FORK")

bodytexts=sapply(invmarkdown,toText)
te
posTitles =
  function(txt)
  {
    str =as.String(txt)
    sent_ann = Maxent_Sent_Token_Annotator()
    s = sent_ann(str)
    word_ann = Maxent_Word_Token_Annotator()
    w = annotate(str, list(sent_ann, word_ann))
    
    pos_ann = Maxent_POS_Tag_Annotator()
    pos = annotate(str, pos_ann, w)
    
    i = pos$type == 'word'
    ww = str[pos][i]
    cbind(ww, unlist(pos$features[i]))
  }

getdf<-function(v){
  a=sapply(data$v,posTitles)
  for(i in 1:length(a)){
    j=a[[i]][,2] %in% c("JJ", "JJR", "JJS", "LS", "MD", "NN",
                        "NNS", "NNP", "NNPS", "RBR", "RB", "VB", 
                        "VBD", "VBG", "VBN", "VBP", "VBZ" )
    a[[i]]=a[[i]][j,]
  }
  for(i in 1:length(a)) a[[i]]= paste(a[[i]][,1],a[[i]][,2]) 
  b=sapply(a,unique)
  b=unlist(b,use.names = F)
  m <- as.data.frame(setNames(replicate(length(b),numeric(0), simplify = F), b))
  for(i in 1:length(a)){
    tt=table(a[[i]])
    k=tt[b]
    m=rbind(m,k)
  }
  m[is.na(m)]=0
  m
}

