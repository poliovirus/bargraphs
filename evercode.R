
****************************************************************
  #function to get your data prepared
  #just in case you don't know how to run a function
  #mag.fun(data.frame)
  
  setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Everardo/Rwork/bargraphs")

  mag.fun<-function(x){
    se <- function(y){ 
      sqrt(var(y)/length(y))
    }
    x$quantity<-log10(x$quantity)
    df.qiagen<-x[x$Kit == "qiagen",]
    df.zymo<-x[x$Kit == "zymo",]
    
    df.qiagen.1<-as.data.frame(as.matrix(by(df.qiagen$quantity, df.qiagen$id, mean)))
    df.qiagen.1$id<-row.names(df.qiagen.1)
    names(df.qiagen.1)[1]<-"mean"
    row.names(df.qiagen.1)<-NULL
    df.qiagen.2<-as.data.frame(as.matrix(by(df.qiagen$quantity, df.qiagen$id, se)))
    df.qiagen.1$se<-df.qiagen.2$V1
    
    df.zymo.1<-as.data.frame(as.matrix(by(df.zymo$quantity, df.zymo$id, mean)))
    df.zymo.1$id<-row.names(df.zymo.1)
    names(df.zymo.1)[1]<-"mean"
    row.names(df.zymo.1)<-NULL
    df.zymo.2<-as.data.frame(as.matrix(by(df.zymo$quantity, df.zymo$id, se)))
    df.zymo.1$se<-df.zymo.2$V1
    df.qiagen.1$kit<-rep("qiagen", nrow(df.qiagen.1))
    df.zymo.1$kit<-rep("zymo", nrow(df.zymo.1))
    
    ready.df<-rbind(df.qiagen.1, df.zymo.1)
    
    return(ready.df)
    
  }
  
  ***************************************************************
    #this will work
    BS.setup<-ggplot(BS, aes(id, mean, ymin = mean+se, ymax = mean-se, fill = kit))
    BS.graph<-BS.setup+geom_bar(position=position_dodge(), stat = "identity", color = "black")
    BS.graph.1<-BS.graph+geom_errorbar(position=position_dodge(0.9), width=0.50)
    BS.graph.1
    ****************************************************************
      #below is my testing code
      
      
      BS.graph.2<-BS.graph.1+scale_y_discrete(expand = c(0.05,-1))
    BS.graph.3<-BS.graph.2+annotation_logticks(sides = "l")
    BS.graph.3
    
    
    geom_bar(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=len-se, ymax=len+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9))
    