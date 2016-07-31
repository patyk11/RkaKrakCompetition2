
#Prepering sum sum data


# Filled Density Plot
rolling_average<-apply(DT[,Sc_cols, with=FALSE], 1, function(x){cumsum(x)/1:48})
rolling_average<-as.data.table(t(rolling_average))

rolling_average_average<-apply( rolling_average,2,sum, na.rm=T)/head_counts

avg_score=DT$Score/48
rolling_average[, "Score":=avg_score]


treshold=0


plotData<-matrix(0, 49,20)
for(i in 1:10 ){
  treshold=i*0.6/10
  plotData[,i]=apply( rolling_average[Score<treshold],2,mean, na.rm=T)
  plotData[,10+i]=apply( rolling_average[Score>treshold],2,mean, na.rm=T)
}
matplot(plotData, type = c("b"),pch=1,col = 1:20, main="Path of average score in treshold subsamples") #plot
legend("topleft", legend = 1:20, col=1:20, pch=1) # optional legend




rolling_sum<-apply(DT[,Sc_cols, with=FALSE], 1, function(x){cumsum(x)})
rolling_sum<-as.data.table(t(rolling_sum))

sum_score=DT$Score
rolling_sum[, "Score":=sum_score]
plotData<-matrix(0, 49,20)

for(i in 1:10 ){
  treshold=i*30/10
  plotData[,i]=apply( rolling_sum[Score<treshold],2,mean, na.rm=T)
  plotData[,10+i]=apply( rolling_sum[Score>treshold],2,mean, na.rm=T)
}
matplot(plotData, type = c("b"),pch=1,col = 1:20, main="Sum of the results in treshold subsamples") #plot
legend("topleft", legend = 1:20, col=1:20, pch=1) # optional legend

