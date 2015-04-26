#Extracting and merging data
datatrain<-read.table("./train/X_train.txt")
datatest <- read.table("./test/X_test.txt")


data <- merge(datatrain, datatest,all=TRUE)



labeldata <- read.table ("./features.txt")
names(data) <- labeldata[,2]


splitNames <- strsplit(names(data),"\\-")

varn<-""
for (i in 1:length(splitNames)) 
{
   varn[i-1]<- splitNames[[i]][1]  
} 

varn<-unique(varn)

enumvec1 <- c("mean()","std()")
enumvec2 <- c("X","Y","Z")
v<-0
g<-""
for (i in varn) {
        for (j in enumvec1) {
                for (z in enumvec2) {
                        g[v]<-paste(i,j,z,sep="-")
                        v=v+1
                }
        }
}
#extract only measurements of mean ans stds
data2<-data.frame()
for (i in g) {
        data2[i]<-data[i]
        
        
}

almostdone <- matrix(unlist(data2), ncol = 29, byrow = TRUE)

dataactiv<- read.table("./train/y_train.txt")
dataactiv2<- read.table("./test/y_test.txt")
dataativmerged <- c(dataactiv[,1],dataactiv2[,1])
datastate<-dataativmerged


namesx <- c("WALKING",
"WALKING_UPSTAIRS",
"WALKING_DOWNSTAIRS",
"SITTING",
"STANDING",
"LAYING")

for (i in 1:length(namesx)) {
        dataativmerged[dataativmerged==i]<-namesx[i]
}
gun<-names(data2)
colnames(almostdone)<-gun
dataframedone<-as.data.frame(data2)

rownames(almostdone)<-dataativmerged

#Subjects screening
sub1<- read.table("./train/subject_train.txt")
sub2<- read.table("./test/subject_test.txt")
submerged <- c(sub1[,1],sub2[,1])

data3<-cbind(almostdone,submerged)
data3<-cbind(data3,datastate)

dataframedone$subjs <- submerged
dataframedone$state <- dataativmerged

datastorer<-data.frame()
for (j in 1:30){
        
datastorer<-rbind(datastorer,sapply(filter(dataframedone, subjs ==j)[1:29],mean ))
}

colnames(datastorer)<-gun

for (z in namesx){
        
        datastorer<-rbind(datastorer,sapply(filter(dataframedone, state ==z)[1:29],function(x) mean(x, na.rm = TRUE) ))
}

rownames(datastorer)<-c(1:30,c("WALKING",
                               "WALKING_UPSTAIRS",
                               "WALKING_DOWNSTAIRS",
                               "SITTING",
                               "STANDING",
                               "LAYING"))
write.table(datastorer,"./table.txt",row.name=FALSE)
