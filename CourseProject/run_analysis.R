#Course Assignment
#testing Git hub from rstudio
packages <- c("data.table","plyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)



wd0 = getwd()

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "getdata-projectfiles-UCI HAR Dataset.zip"

if (!file.exists(wd0)) {dir.create(wd0)}
if (!file.exists(zipfile)){download.file(url, file.path(wd0, zipfile))}
if (!file.exists("UCI HAR Dataset")){ unzip (zipfile, exdir = "./")}

wd1 = paste(wd0,"UCI HAR Dataset",sep = "/")
setwd(wd1)



MyActivityLabels = fread("activity_labels.txt", header = FALSE, sep = "auto", stringsAsFactors= F)
setnames(MyActivityLabels, names(MyActivityLabels), c("ActivityNum", "ActivityName"))

#print(MyActivityLabels)

wd1 = paste(wd0,"UCI HAR Dataset",sep = "/")

MyFeatures = as.data.table(fread("features.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F ))
MyFeatures = MyFeatures[order(MyFeatures$V1),] 

setkey(MyFeatures,V2)

setnames(MyFeatures, names(MyFeatures), c("Num", "Name"))
MyFeatures = MyFeatures[order(MyFeatures$Num),] 
MyFeatures$Code <- MyFeatures[, paste0("V", Num)]
MyFeatures$Code


wd2 = paste(wd0,"UCI HAR Dataset/test",sep = "/")
setwd(wd2)

MyXTest = as.data.table(fread("X_test.txt", header = FALSE, sep = "auto", stringsAsFactors= F))
MySubjectTest = as.data.table(fread("subject_test.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F))
#write.csv(MyXTest, file = "MyXTest.csv")


MyYTest = as.data.table(fread("Y_test.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F))
colnames(MyYTest) = c(c("ActivityNum")) 

#write.csv(MyYTest, file = "MyYTest.csv")

wd3 = paste(wd0,"UCI HAR Dataset/train",sep = "/")
setwd(wd3)

MyXTrain = as.data.table(fread("X_train.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F))
MySubjectTrain = as.data.table(fread("subject_train.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F))
#write.csv(MyXTrain, file = "MyXTrain.csv")

MyYTrain = as.data.table(fread("Y_train.txt", header = FALSE, sep = "auto" ,stringsAsFactors= F))
colnames(MyYTrain) = c(c("ActivityNum")) 
#write.csv(MyYTrain, file = "MyYTrain.csv")

MySubjects <- rbind(MySubjectTrain, MySubjectTest)
setnames(MySubjects, "V1", "subject")

setwd(wd0)

#Merge Test X and Y files together
  
  # Create a key & merge
    MergeKey = 1:nrow(MyXTest)
    MyXTest = cbind(MergeKey=MergeKey, MyXTest)
    
    MergeKey = 1:nrow(MyXTrain)
    MyXTrain = cbind(MergeKey=MergeKey, MyXTrain)
    
    MergeKey = 1:nrow(MyYTest)
    MyYTest = cbind(MergeKey=MergeKey, MyYTest)
    
    MergeKey =  1:nrow(MyYTrain)
    MyYTrain = cbind(MergeKey=MergeKey, MyYTrain)
  
    setkey(MyXTest, MergeKey); setkey(MyYTest, MergeKey)
    setkey(MyXTrain, MergeKey); setkey(MyYTrain, MergeKey)
    
    # merge
    MergedXYTestData = merge(MyXTest, MyYTest)
    MergedXYTrainData = merge(MyXTrain, MyYTrain)
    
    #merge
    MergedAllData = rbind (MergedXYTrainData,MergedXYTestData)
    MergedAllData = cbind (MySubjects,MergedAllData)
    
    MergedAllData <- merge(MergedAllData, MyActivityLabels, by="ActivityNum", all.x=TRUE)
  
    setkey(MyFeatures,Name)
    
    
    
    #create final dataset with ActivityNum, Subject, & Activity names
    FinalDataSet= MergedAllData[,1:2,with = FALSE]
    FinalDataSet= c(FinalDataSet,MergedAllData[,565,with = FALSE])
   
    
    # add feature names to MergedAllData
    for(i in 1:nrow(MyFeatures)) 
   {
       
            b = subset(MyFeatures, Num==i)
            MyFeaturesSubset = as.data.table( b[,Name])
            x= 3+i
            names(MergedAllData)[x] = as.character( MyFeaturesSubset) 
      
    }
    
    
    # Subset data with mean or std observations
    for(i in 1:ncol(MergedAllData)) 
    {
      
      
      g = grep("\\b-mean()\\b",names(MergedAllData)[i] ,value=F)
      g1 =grep("\\b-std()\\b",names(MergedAllData)[i] ,value=F)
      
      if (length(g) == 1  ){
        #print(names(MergedAllData)[i])
        FinalDataSet= c(FinalDataSet,MergedAllData[,i,with = FALSE])
        
      }else if ( length(g1) == 1){
        #print(names(MergedAllData)[i])
        FinalDataSet= c(FinalDataSet,MergedAllData[,i,with = FALSE])
        
      }
      
    }
    
    #clean up column names to make them reabable
    HumanReadableColumnNames <- function(columnnames) {
      
      columnnames = gsub(x = columnnames, pattern = "[:():]", replacement = "")
      columnnames = gsub(x = columnnames, pattern = "^t", replacement = "Time")
      columnnames = gsub(x = columnnames, pattern = "^f", replacement = "Frequency")
      columnnames = gsub(x = columnnames, pattern = "Mag", replacement = "Magnitude")
      columnnames = gsub(x = columnnames, pattern = "BodyBody", replacement = "Body")}
    
    
    FinalDataSet = as.data.table(FinalDataSet)
    colnames(FinalDataSet) <- HumanReadableColumnNames(colnames(FinalDataSet))
    
    
    
    
    write.table(as.data.table(FinalDataSet), file = "FinalDataSet.txt",row.names = FALSE,sep = "\t")
    
    #mean each variable by activity by subject 
    FinalMeanDataset = ddply(as.data.frame(FinalDataSet), .(ActivityNum, subject,ActivityName), function(x) colMeans(x[,-c(1:3)]))
    
    write.table(as.data.table(FinalMeanDataset), file = "FinalMeanDataSet.txt",row.names = FALSE,sep = "\t")
    
    print(paste("FinalDataSet.txt written to ",getwd(),sep = ""))
    print(paste("FinalMergedDataSet.txt written to ",getwd(),sep = ""))
     