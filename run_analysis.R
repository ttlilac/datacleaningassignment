run_analysis<-function(){
  
  ## Read in both test and train data
  test<-read.table("./test/X_test.txt", sep = "")
  testlabel<-read.table("./test/y_test.txt", sep = "")
  testsub<-read.table("./test/subject_test.txt", sep = "")
  
  train<-read.table("./train/X_train.txt", sep = "")
  trainlabel<-read.table("./train/y_train.txt", sep = "")
  trainsub<-read.table("./train/subject_train.txt", sep = "")
  
  ## Merge test and train datasets
  final<-rbind(test,train)
  label<-rbind(testlabel,trainlabel)
  subject<-rbind(testsub,trainsub)
  
  ## Add column names into final dataset
  feature<-read.table("./features.txt", sep = "")
  colnames(final)<-feature[,2]
  
  ## Extract mean and standrad deviation for each measurement
  final<-final[, colnames(final)[grep("mean()|std()", colnames(final))]]
  
  ## Merge activity names into the dataset
  activity<-character(nrow(label))
  for (i in 1:nrow(label)){
    if(label[i,]==1)  activity[i]<-"WALKING"
    if(label[i,]==2)  activity[i]<-"WALKING_UPSTAIRS"
    if(label[i,]==3)  activity[i]<-"WALKING_DOWNSTAIRS"
    if(label[i,]==4)  activity[i]<-"SITTING"
    if(label[i,]==5)  activity[i]<-"STANDING"
    if(label[i,]==6)  activity[i]<-"LAYING"
  }
  final$activity<-activity
  
  ## Merge subject into the dataset
  final$subject<-subject$V1
  
  ## Calculate average of each variable for each activity and each subject
  tidy<-data.frame()
  
  for (sub in 1:30){
    for (act in c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")){
      tidy<-rbind(tidy,colMeans(final[final$activity==act & final$subject == sub, colnames(final)[grep("mean()|std()", colnames(final))]]))
    }
  }
  
  colnames(tidy)<-colnames(final)[grep("mean()|std()", colnames(final))]
  tidy$activity<-rep(c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"),30)
  tidy$subject<-rep(c(1:30),each=6, times=1)
  
  
  ## Write the average value dataset into file
  write.table(tidy, file="tidy.txt", row.names = FALSE)
}