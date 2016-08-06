library(data.table)



DT <- as.data.table(read.csv('http://doitprofiler.com/wp-content/uploads/2016/06/Competition2Maths.csv
                             ', stringsAsFactors = F))

Sa_cols <- c(paste0('An0', 1:9), paste0('An', 10:48))
Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:48))
Sd_cols <- c(paste0('Du0', 1:9), paste0('Du', 10:48))

# removing commans added
DT <- DT[, lapply(.SD, gsub, pattern  = ',', replacement = '.'), .SDcols = Sa_cols]

unique(DT$Group)
unique(DT$Score)

sum(DT$Sc01==0,na.rm = T)
head_counts=colSums(!is.na(DT[,Sc_cols,with=FALSE]))
false_rate=((head_counts-apply(DT[,Sc_cols, with=FALSE], 2, sum, na.rm=TRUE)))/head_counts

head_counts
sum(DT$Sc01==1,na.rm = T)

plot(false_rate)

xDT=DT[,Sa_cols,with=FALSE]

who_commas=apply(xDT,1,function(x){sum(grepl(",",x))>0})

apply(DT[,Sc_cols, with=FALSE], 2, sum, na.rm=TRUE)

#inferring correct answers


#x=c("ara","bera,")
#grepl(",",x)


#geniuses<-DT[DT$Score>30,]

#genius<-geniuses[Sc45==1,]
#geniuses[1,"An01" ,with=FALSE]
#genius[1,Sa_cols ,with=FALSE]

goodanswer=rep("WTF",48)
#goodanswer_count=rep(0,48)


#for(i in 1:10){
#goodanswer<-ifelse(geniuses[i,Sc_cols ,with=FALSE],geniuses[i,Sa_cols ,with=FALSE],goodanswer)
#goodanswer_count<-ifelse(geniuses[i,Sc_cols ,with=FALSE],1,goodanswer_count)
#}
#which(goodanswer_count!=1)
# those one need to be added 

goodanswer[[22]]=4.5
goodanswer[[27]]=17.5
goodanswer[[36]]=89
goodanswer[[42]]="tangent"
goodanswer[[44]]=-1.5
goodanswer[[45]]=1
goodanswer[[46]]=5
goodanswer[[47]]=157
goodanswer[[48]]=12
goodanswer_final=unlist(goodanswer)

#answers that need to be checked for commas
ans_to_check<-c(11,16,17,22,25,26,27,28,31,33,35)
goodans_to_check<-c("3.75" ,"0.25", "14.29","4.5","1.73","97.723", "17.5", "3.456","57.5", "154.18633","88.226") 



# the answers were corrected in an XLX file
copyDT<-copy(DT)
for(i in 1:11){
  #going trough the columns to check whether somons answer is now correct.
  
  column_nr=ans_to_check[i]
  column_Sc=Sc_cols[column_nr]
  column_An=Sa_cols[column_nr]
  correct_ans=goodans_to_check[i]
  
print(c("Question",column_nr))
 
 new_score<-ifelse(DT[,column_Sc,with=FALSE]==1,
           1,
            ifelse(DT[,column_An,with=FALSE]==correct_ans,1,0)
         )
 print(c("new score=",sum(new_score,na.rm=T), "old score=", sum(DT[,column_Sc,with=FALSE],na.rm=T)))
     
DT[,column_Sc:=new_score]
    
}


temp=geniuses[1,Sa_cols ,with=FALSE]==goodanswer_final
which(temp!=geniuses[1,Sc_cols ,with=FALSE])
goodanswer_final[36]
# geniuses[1,"An36", ,with=FALSE]
# geniuses[1,"Sc36" ,with=FALSE]
# geniuses[1,"Score",with=FALSE]




# transforming the data
  apply(DT[,Sc_cols, with=FALSE], 2, function(x){cumsum(x)/1:48})

apply(DT[who_commas,Sc_cols, with=FALSE], 2, sum, na.rm=TRUE)


