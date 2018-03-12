## input data
df<-read.csv('F:/Pembelajaran/Mata Kuliah/PASCA sem 2/MOKLAS/Pertemuan 3/white_wine.csv')
df$label<-ifelse(df$quality>6,'Bagus','Kurang')
data<-df[,c('density','alcohol','label')]
nrow(data)
k.knn=10

#########fungsi kfold with knn ##########################

knn.cv<-function(data,kelas,k=5,k.knn=1){
library(class)
indeks<-sample(1:nrow(data),nrow(data))
df_temp<-data[indeks,]
range<-round(nrow(df_temp)/k)
y_temp<-NULL
for(i in 1:k)
{
	if(i!=k)
	{
		test_indeks<-((i-1)*range+1):(i*range)
		y_temp<-factor(append(as.character(y_temp),as.character(knn(df_temp[-test_indeks,-kelas],
												df_temp[test_indeks,-kelas],df_temp[-test_indeks,kelas],k=k.knn))))
	}else{
		test_indeks<-((i-1)*range+1):nrow(df_temp)
		y_temp<-factor(append(as.character(y_temp),as.character(knn(df_temp[-test_indeks,-kelas],
												df_temp[test_indeks,-kelas],df_temp[-test_indeks,kelas],k=k.knn))))
	}
}
akurasi<-mean((df_temp[,kelas]==y_temp)+0)
return (akurasi)
}

dulsearchcv<-function(data, kelas,k=9,k.knn=1){
tune_hyperparameter<-k.knn
akurasi<-NULL
for(i in tune_hyperparameter){
akurasi<-c(akurasi,knn.cv(data,kelas=kelas,k=k,k.knn=i))
}
return(akurasi)
}
################################# fungsi beres ######
k.knn<-1:100
akurasi_knn<-dulsearchcv(data,kelas = 3,k=10,k.knn=k.knn)
plot(k.knn,akurasi_knn,type='o')
