library(data.table)



DT <- as.data.table(read.csv('http://doitprofiler.com/wp-content/uploads/2016/06/Competition2Maths.csv
                             ', stringsAsFactors = F))


Sa_cols <- c(paste0('An0', 1:9), paste0('An', 10:48))
Sc_cols <- c(paste0('Sc0', 1:9), paste0('Sc', 10:48))
Sd_cols <- c(paste0('Du0', 1:9), paste0('Du', 10:48))



unique(DT$Group)
unique(DT$Score)

sum(DT$Sc01==0,na.rm = T)
colSums(!is.na(DT[,Sc_cols,with=FALSE]))-apply(DT[,Sc_cols, with=FALSE], 2, sum, na.rm=TRUE)


sum(DT$Sc01==1,na.rm = T)

