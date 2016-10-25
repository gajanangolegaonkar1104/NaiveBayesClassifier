#!/usr/bin/env Rscript
library(e1071)
args <- commandArgs(trailingOnly = TRUE);

trainis <- args[1];
testis <-args[2];
training<-read.csv(file=trainis,header = TRUE,sep = "\t",quote = "\"")
testing<-read.csv(file=testis,header = TRUE,sep = "\t",quote = "\"")
#training<-read.csv("C:\\Users\\admin\\Downloads\\train.csv")
#testing<-read.csv("C:\\Users\\admin\\Downloads\\test.csv")
rows<-nrow(training)
cols<-ncol(training)

rowst<-nrow(testing)
colst<-ncol(testing)


classvalues<-unique(training[,cols])
classvalues<-sort(classvalues)
numberofclasses<-length(classvalues)
print(classvalues)

for(i in classvalues)
{
  outputval0<-0
  bar<-subset(training,training[,cols]==i)
  brow<-nrow(bar)
  outputval0 <- paste("P(",names(training)[cols],"=",i,")=",brow/rows," ",sep = "")
  
  attributename <- names(training[,-cols]);
  vari<-1
  for(j in attributename){
    
    attributevalues <- unique(training[,vari],incomparables = FALSE);
    attributevalues<- sort(attributevalues);
    count<-1
    for(k in attributevalues){
      l1<-colSums(bar[,-cols]==k)
      names(l1)<-NULL
      outputval0 <- paste(outputval0,"P(",j,"=",k,"|",i,")=", l1[vari]/brow," ",sep = "")
      count<-count+1
    }
    vari<-vari+1
  }
  cat(outputval0,"\n\n\n")
}

model <- naiveBayes(as.factor(class)~., data=training,threshold = 0.000001)

prediction <- predict(model, training[,-cols])
dtt<-table(pred=prediction, true=training$class)
accuracy<-mean(prediction==training$class)
cat("Accuracy on training set(",rows,"instances):",accuracy*100,"\n\n")
prediction <- predict(model, testing[,-colst])
dtt<-table(pred=prediction, true=testing$class)
accuracyt<-mean(prediction==testing$class)
cat("Accuracy on testing set(",rowst,"instances):",accuracyt*100,"\n\n")

