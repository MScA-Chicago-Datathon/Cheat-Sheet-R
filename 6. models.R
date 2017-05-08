library(nnet)
library(ggplot2)
#删除outliers
par(mfrow=c(2,6))
for(i in 2:12)
    boxplot(redwine[,i],xlab=str(redwine[0,i]),col="steelblue")
redwinelq<-redwine
for(i in 2:12)
    redwinelq<-redwinelq[which(redwinelq[,i]<mean(redwinelq[,i])
                               +3*IQR(redwinelq[,i])&redwinelq[,i]>mean(redwinelq[,i])-3*IQR(redwinelq[,i])),]
redwine<-redwinelq
#EFA建⽴立新的数据框
redwineefa<-scale(redwinelq)#删离群点后
options(max.print=1000000)
redwineefa<-data.frame(redwineefa)
attach(redwineefa)
acid<-(fixed.acidity+citric.acid-pH)/3
sulfur.dioxide<-(free.sulfur.dioxide+total.sulfur.dioxide)/2
alcodensity<-(-alcohol+density)/2
redwineefa<-redwineefa[,-12]
redwineefa<-cbind(acid,sulfur.dioxide,alcodensity,redwineefa,redwinelq$quality)
names(redwineefa)[15]<-"quality"

#multinom logistic
test <- multinom(quality~acid+volatile.acidity+residual.sugar+chlorides+sulfur.dioxide
                 +alcodensity+sulphates,data=redwineefatrain)
summary(test)
#显著性检验
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))# 2-tailed z test
p
#预测
predict=predict(test,newdata=redwineefa)
predict1<-predict(test,newdata=redwineefatrain1)
predict2<-predict(test,newdata=redwineefatest1)


#拟合精度
accuracy<-0
for(i in 1:946){
    if(redwineefatrain$quality[i]==predict1[i]){accuracy<-accuracy+1}
    else{accuracy<-accuracy}}
accuracy/946
table(predict1,redwineefatrain$quality)
#预测精度

#计算精度.大循环
for(j in 1:10){
    accuracy2<-0
    for(i in 1:472){
        if(redwineefatest$quality[i]==predict2[i]){accuracy2<-accuracy2+1}
        else{accuracy2<-accuracy2}}
    accuracy2/472
    table(predict2,redwineefatest$quality)
    a[j]<-accuracy/946
    a2[j]<-accuracy2/472
}
mean(a)
mean(a2)
#选择测试集
index<-sample(1:dim(redwineefa)[1])#随机打乱数据集的行指标，将打乱后的数据#集的行指标放在index里
redwineefatrain<-redwineefa[index[1:2*floor(dim(redwineefa)[1]/3)],]#将index元素个数的前2/3作为行指标，提取出的数据集作为训练集
redwineefatest<-redwineefa[index[((2*ceiling(dim(redwineefa)[1]/3))+1):dim(redwineefa)[1]],]#将剩下的作为待测集
G<-as.factor(redwineefatrain[,dim(redwineefa)[2]]) #训练集分类情况：因子变量
redwineefatrain1<-redwineefatrain[,-dim(redwineefa)[2]]
G1<-redwineefatest[,dim(redwineefa)[2]]#待测集分类情况
redwineefatest1<-redwineefatest[,-dim(redwineefa)[2]]

#Decision Tree
library(rpart)
library(rpart.plot)
red.decision<-rpart(quality~.,data=redtrain,method="class")
rpart.plot(red.decision,type=3,extra=2,main="Decision Tree\nRed quality by 11 parameters",cex =0.8) ##回代，检验误判率
redtrain.decision<-predict(red.decision,redtrain,type="class")
red2<-table(redtrain.decision,G)
red2
##计算误判率
len<-length(which(G!=redtrain.decision))
len/length(G)
##预测待测集
red.predict.decision<-predict(red.decision,redtest,type="class")
redtest[,12]<-red.predict.decision
red21<-table(red.predict.decision,G1)
red21
len<-length(which(G1!=red.predict.decision))
len/length(G1)

#K-NN
library(class)
##回代，检验误判率
red.knn<-knn(redtrain1,redtrain1,G,k=5,prob=T)
red1<-table(red.knn,G)
red1
##计算误判率
len<-length(which(G!=red.knn))
len/length(G)
##预测待测集
red.predict.knn<-knn(redtrain1,redtest1,G,k=5,prob=T)
redtest[,12]<-red.predict.knn
red11<-table(red.predict.knn,G1)
red11
len<-length(which(G1!=red.predict.knn))
len/length(G1)

##SVM
library(kernlab)
red.SVM<-
    ksvm(as.factor(quality)~.,data=redtrain,kernel="rbfdot",C=5,cross=3,prob.model=T)
red.SVM
##回代，得到分类结果
redtrain.check<-fitted(red.SVM)
##检验误判率
red0<-table(redtrain.check,G)
red0
##计算误判率
len<-length(which(G!=redtrain.check))
len/length(G)
##预测待测集
red.predict.SVM<-predict(red.SVM,redtest)
redtest[,12]<-red.predict.SVM
red01<-table(red.predict.SVM,G1)
red01
len<-length(which(G1!=red.predict.SVM))
len/length(G1)
