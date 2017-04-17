#��ȡ����
mydata = read.table('C:/Users/Vio/Desktop/DataMining/homework1/mycode/horsedata.txt',
col.names=c('surgery','Age','Hospital Number','rectal temperature',
'pulse','respiratory rate','temperature of extremities','peripheral pulse',
'mucous membranes','capillary refill time','pain','peristalsis','abdominal distension',
'nasogastric tube','nasogastric reflux','nasogastric reflux PH','rectal examination',
'abdomen','packed cell volume','total protein','abdominocentesis appearance',
'abdomcentesis total protein','outcome','surgical lesion','type of lesion_1',
'type of lesion_2','type of lesion_3','cp_data'),
colClasses = c("factor","factor","factor","numeric","numeric","numeric",
"factor","factor","factor","factor","factor","factor","factor",
"factor","factor","numeric","factor","factor","numeric","numeric",
"factor","numeric","factor","factor","factor","factor","factor","factor"),
na.string=c('?'))

#����ժҪ
#summary(mydata)

#����ֱ��ͼ,��rectal.temperatureΪ����������������
hist(mydata$rectal.temperature)

#����car��
library(car)

#����QQͼ,��rectal.temperatureΪ����������������
qqPlot(mydata$rectal.temperature,main='Norm QQ Plot of rectal.temperature')

#��������ͼ,��rectal.temperatureΪ����������������
boxplot(mydata$rectal.temperature,ylab='rectal.temperature')
rug(mydata$rectal.temperature,side=4)
abline(h=mean(mydata$arectal.temperature,na.rm=T),lty=2)

#����pulse
hist(mydata$pulse)
qqPlot(mydata$pulse,main='Norm QQ Plot of pulse')
boxplot(mydata$pulse,ylab='pulse')
rug(mydata$pulse,side=4)
abline(h=mean(mydata$pulse,na.rm=T),lty=2)

#����respiratory.rate
hist(mydata$respiratory.rate)
qqPlot(mydata$respiratory.rate,main='Norm QQ Plot of respiratory.rate')
boxplot(mydata$respiratory.rate,ylab='respiratory.rate')
rug(mydata$respiratory.rate,side=4)
abline(h=mean(mydata$respiratory.rate,na.rm=T),lty=2)

#����nasogastric.reflux.PH
hist(mydata$nasogastric.reflux.PH)
qqPlot(mydata$nasogastric.reflux.PH,main='Norm QQ Plot of nasogastric.reflux.PH')
boxplot(mydata$nasogastric.reflux.PH,ylab='nasogastric.reflux.PH')
rug(mydata$nasogastric.reflux.PH,side=4)
abline(h=mean(mydata$nasogastric.reflux.PH,na.rm=T),lty=2)

#����packed.cell.volume
hist(mydata$packed.cell.volume)
qqPlot(mydata$packed.cell.volume,main='Norm QQ Plot of packed.cell.volume')
boxplot(mydata$packed.cell.volume,ylab='packed.cell.volume')
rug(mydata$packed.cell.volume,side=4)
abline(h=mean(mydata$packed.cell.volume,na.rm=T),lty=2)

#����total.protein
hist(mydata$total.protein)
qqPlot(mydata$total.protein,main='Norm QQ Plot of total.protein')
boxplot(mydata$total.protein,ylab='total.protein')
rug(mydata$total.protein,side=4)
abline(h=mean(mydata$total.protein,na.rm=T),lty=2)

#����abdomcentesis.total.protein
hist(mydata$abdomcentesis.total.protein)
qqPlot(mydata$abdomcentesis.total.protein,main='Norm QQ Plot of abdomcentesis.total.protein')
boxplot(mydata$abdomcentesis.total.protein,ylab='abdomcentesis.total.protein')
rug(mydata$abdomcentesis.total.protein,side=4)
abline(h=mean(mydata$abdomcentesis.total.protein,na.rm=T),lty=2)

#ȱʧ���ݴ���
#�޳�ȱʧ����
omitdata = na.omit(mydata)
write.table(omitdata,'C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/OmitedData.txt',col.names = F,row.names = F, quote = F)

#ʹ�ø�Ƶ�����滻
library(DMwR)
preprocess2 = mydata[-manyNAs(mydata),]
preprocess2 = centralImputation(preprocess2)
write.table(preprocess2,'C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/CentralImputationData.txt',
col.names = F,row.names = F, quote = F)

#���ӻ��Ա�,��respiratory.rateΪ��
dataPre = read.table('C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/CentralImputationData.txt',
col.names=c('surgery','Age','Hospital Number','rectal temperature',
'pulse','respiratory rate','temperature of extremities','peripheral pulse',
'mucous membranes','capillary refill time','pain','peristalsis','abdominal distension',
'nasogastric tube','nasogastric reflux','nasogastric reflux PH','rectal examination',
'abdomen','packed cell volume','total protein','abdominocentesis appearance',
'abdomcentesis total protein','outcome','surgical lesion','type of lesion_1',
'type of lesion_2','type of lesion_3','cp_data'),
colClasses = c("factor","factor","factor","numeric","numeric","numeric",
"factor","factor","factor","factor","factor","factor","factor",
"factor","factor","numeric","factor","factor","numeric","numeric",
"factor","numeric","factor","factor","factor","factor","factor","factor"),
)
hist(dataPre$respiratory.rate)
qqPlot(dataPre$respiratory.rate,main='Norm QQ Plot of respiratory.rate')
boxplot(dataPre$respiratory.rate,ylab='respiratory.rate')
rug(dataPre$respiratory.rate,side=4)
abline(h=mean(dataPre$respiratory.rate,na.rm=T),lty=2)

#ͨ������������ȱʧֵ
symnum(cor(mydata[,c(4,5,6,16,19,20,22)],use='complete.obs'))
lm(formula=respiratory.rate~pulse, data=mydata)
preprocess3 = mydata[-manyNAs(mydata),]
fillpulse <- function(respiratory.rate){
	if(is.na(respiratory.rate ))
		return(NA)
	else return (11.1244 + 0.2801*respiratory.rate)
}
preprocess3[is.na(preprocess3$pulse),'pulse'] <- sapply(preprocess3[is.na(preprocess3$pulse),'respiratory.rate'],fillpulse)
write.table(preprocess3,'C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/linearDefaultData.txt',
col.names = F,row.names = F, quote = F)

#���ӻ��Ա�,��respiratory.rateΪ��
dataPre = read.table('C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/linearDefaultData.txt',
col.names=c('surgery','Age','Hospital Number','rectal temperature',
'pulse','respiratory rate','temperature of extremities','peripheral pulse',
'mucous membranes','capillary refill time','pain','peristalsis','abdominal distension',
'nasogastric tube','nasogastric reflux','nasogastric reflux PH','rectal examination',
'abdomen','packed cell volume','total protein','abdominocentesis appearance',
'abdomcentesis total protein','outcome','surgical lesion','type of lesion_1',
'type of lesion_2','type of lesion_3','cp_data'),
colClasses = c("factor","factor","factor","numeric","numeric","numeric",
"factor","factor","factor","factor","factor","factor","factor",
"factor","factor","numeric","factor","factor","numeric","numeric",
"factor","numeric","factor","factor","factor","factor","factor","factor"),
)
hist(dataPre$respiratory.rate)
qqPlot(dataPre$respiratory.rate,main='Norm QQ Plot of respiratory.rate')
boxplot(dataPre$respiratory.rate,ylab='respiratory.rate')
rug(dataPre$respiratory.rate,side=4)
abline(h=mean(dataPre$respiratory.rate,na.rm=T),lty=2)

#ͨ��������������ȱʧֵ
preprocess4 = knnImputation(mydata,k=10)
write.table(preprocess4,'C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/knnImputationData.txt',
col.names = F,row.names = F, quote = F)

#���ӻ��Ա�,��respiratory.rateΪ��
dataPre = read.table('C:/Users/Vio/Desktop/DataMining/homework1/mycode/PreprocessedData/knnImputationData.txt',
col.names=c('surgery','Age','Hospital Number','rectal temperature',
'pulse','respiratory rate','temperature of extremities','peripheral pulse',
'mucous membranes','capillary refill time','pain','peristalsis','abdominal distension',
'nasogastric tube','nasogastric reflux','nasogastric reflux PH','rectal examination',
'abdomen','packed cell volume','total protein','abdominocentesis appearance',
'abdomcentesis total protein','outcome','surgical lesion','type of lesion_1',
'type of lesion_2','type of lesion_3','cp_data'),
colClasses = c("factor","factor","factor","numeric","numeric","numeric",
"factor","factor","factor","factor","factor","factor","factor",
"factor","factor","numeric","factor","factor","numeric","numeric",
"factor","numeric","factor","factor","factor","factor","factor","factor"),
)
hist(dataPre$respiratory.rate)
qqPlot(dataPre$respiratory.rate,main='Norm QQ Plot of respiratory.rate')
boxplot(dataPre$respiratory.rate,ylab='respiratory.rate')
rug(dataPre$respiratory.rate,side=4)
abline(h=mean(dataPre$respiratory.rate,na.rm=T),lty=2)