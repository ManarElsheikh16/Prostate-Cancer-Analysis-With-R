install.packages("read_xlsx")
install.packages("readxl")
install.packages("xlsx")
library("xlsx")
library("readxl")

my_data <-readxl::read_xlsx("GSE180239_After_Cleaning.xlsx")
data=data.frame(my_data)
control=data[,c(2,3,4,5)] 
treatment=data[,c(6,7,8,9)]
index<-c()  #use it for storing index of cells which pvalue<0.05
pvalues<-c() #use it for storing pvalues of cells
t<-c()        #use it for storing t values of cells
i=1
counter=1
while(counter<=100) {
  x<-t.test(control[i,],treatment[i,],var.equal = TRUE)
  if(x$p.value<=0.05&!is.nan(x$p.value)){
    index[counter]=i
    pvalues[counter]=x$p.value
    t[counter]=x$statistic
    counter=counter+1}
  i=i+1}
rownames=c(1:100)
colnames=c("gene name", "P-value","t")
output<-matrix(c(data$Gene_Name[index],pvalues,t),nrow = 100,byrow = FALSE,dimnames =list(rownames,colnames))
output # show  t and p-value of first 100 gene which p-value <0.05

boxplot(data[index,2:9],boxwex=0.7, xlab = "Number of Samples",ylab = "Number of Genes ",
        main="Boxplot of gene Expression",ylim=c(0,10000),
        col= c("blue","blue","blue","blue","green","green","green","green"),las=2)


hist( data[index,3],main="Histogram for comparing between 2 groups ", xlab = "Sample(Control) ",col="black")
hist( data[index,7],main="Histogram for one Sample (treatment) ", xlab = "Sample(treatment) ",
      col = "green")

hist( data[index,7],add=T,col = "green")


