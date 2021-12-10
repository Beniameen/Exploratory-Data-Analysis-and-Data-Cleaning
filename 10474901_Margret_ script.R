#You may need to change/include the path of your working directory
#Import the dataset into R Studio.
#Margret Beniameen
dat <- read.csv("MLDATASET-200000-1612938401.csv",
                na.strings="", stringsAsFactors=TRUE)
set.seed(10474901)
#Randomly select 150,000 rows
selected.rows <- sample(1:nrow(dat),size=500,replace=FALSE)
#Your sub-sample of 150,000 observations and excluding the 1st and last column
mydata <- dat[selected.rows,2:16]
dim(mydata)#check the dimension of your sub-sample
install.packages("dplyr")
install.packages("moments")

library(tidyverse)
library(ggpubr)
library(factoextra)
library(dplyr)
library(moments)
#library(scales) 
library(ggplot2)
library(grid)

#Part 1 (i) 
#Categorical Feature
#1. Download.source
Download.freq <- table(mydata$Download.Source, useNA = "ifany"); Download.freq
Download.prop <- prop.table(Download.freq); Download.prop
Download.percent <- Download.prop*100; Download.percent

#2. TLD
TLD.freq <- table(mydata$TLD , useNA = "ifany"); TLD.freq
TLD.prop <- prop.table(TLD.freq); TLD.prop
TLD.percent <- TLD.prop*100; TLD.percent
#3.Download.Speed
DownloadS.freq <- table(mydata$Download.Speed, useNA = "ifany"); DownloadS.freq
DownloadS.prop <- prop.table(DownloadS.freq); DownloadS.prop
DownloadS.percent <- DownloadS.prop*100; DownloadS.percent

#Binary
#1.Executable Code Maybe Present in Headers
ExecutableCode.freg <- table(mydata$Executable.Code.Maybe.Present.in.Headers, useNA = "ifany"); ExecutableCode.freg
ExecutableCode.prop <- prop.table(ExecutableCode.freg); ExecutableCode.prop
ExecutableCode.percent <- ExecutableCode.prop*100; ExecutableCode.percent
#2.No Executable Code Found in Headers
NoExecutable.freg <- table(mydata$No.Executable.Code.Found.In.Headers, useNA = "ifany"); NoExecutable.freg
NoExecutable.prop <- prop.table(NoExecutable.freg); NoExecutable.prop
NoExecutable.percent <- NoExecutable.prop*100; NoExecutable.percent
#3. Evidence of Code Obfuscation
evidence.freg <- table(mydata$Evidence.of.Code.Obfuscation, useNA = "ifany");evidence.freg
evidence.prop <- prop.table(evidence.freg); evidence.prop
evidence.percent <- evidence.prop*100; evidence.percent
#4.Actually Malicious
ActuallyMalicious.freg <- table(mydata$Actually.Malicious, useNA = "ifany") ; ActuallyMalicious.freg
ActuallyMalicious.prop <- prop.table(ActuallyMalicious.freg); ActuallyMalicious.prop
ActuallyMalicious.percent <- ActuallyMalicious.prop*100; ActuallyMalicious.percent

#Part.1(ii)
#Finding missing Ping Time to Server
missing.pingTime <- sum(is.na(mydata$Ping.Time.To.Server)); missing.pingTime
pingTime.N <- missing.pingTime/500*100; pingTime.N

#Finding minimum Ping Time to Server
minimum.pingTime <- min(mydata$Ping.Time.To.Server, na.rm = TRUE);minimum.pingTime

#Finding maximum Ping Time to Server
maximum.pingTime <- max(mydata$Ping.Time.To.Server, na.rm = TRUE);maximum.pingTime

#Finding mean Ping Time to Server
mean.pingTime <- mean(mydata$Ping.Time.To.Server, na.rm = TRUE);mean.pingTime 

#Finding median ping Time to Server
median.pingTime <- median(mydata$Ping.Time.To.Server, na.rm = TRUE); median.pingTime

#Finding skewnwess ping Time to Server
skewness.pingTime <- skewness(mydata$Ping.Time.To.Server, na.rm = TRUE); skewness.pingTime

#///////////////////////////////////////////////////#

#Finding missing File Size (Bytes)
missing.fileSize <- sum(is.na(mydata$File.Size..Bytes.)); missing.fileSize
fileSize.N <- missing.fileSize/500*100; fileSize.N

#Finding minimum File Size (Bytes)
minimum.fileSize <- min(mydata$File.Size..Bytes.);minimum.fileSize

#Finding maximum File Size (Bytes)
maximum.fileSize <- max(mydata$File.Size..Bytes.);maximum.fileSize

#Finding mean File Size (Bytes)
mean.fileSize <- mean(mydata$File.Size..Bytes.);mean.fileSize

#Finding median File Size (Bytes)
median.fileSize <- median(mydata$File.Size..Bytes.); median.fileSize

#Finding skewnwess File Size (Bytes)
skewness.fileSize <- skewness(mydata$File.Size..Bytes.); skewness.fileSize

#//////////////////////////////////////////#
#Finding missing How Many Times File Seen
missing.fileSeen <- sum(is.na(mydata$How.Many.Times.File.Seen)); missing.fileSeen
fileSeen.N <- missing.fileSeen/500*100; fileSeen.N

#Finding minimum How Many Times File Seen
minimum.fileSeen <- min(mydata$How.Many.Times.File.Seen);minimum.fileSeen

#Finding maximum How Many Times File Seen
maximum.fileSeen <- max(mydata$How.Many.Times.File.Seen);maximum.fileSeen

#Finding mean How Many Times File Seen
mean.fileSeen <- mean(mydata$How.Many.Times.File.Seen);mean.fileSeen

#Finding median How Many Times File Seen
median.fileSeen <- median(mydata$How.Many.Times.File.Seen); median.fileSeen

#Finding skewnwess How Many Times File Seen
skewness.fileSeen <- skewness(mydata$How.Many.Times.File.Seen); skewness.fileSeen

#//////////////////////////////////////////////////#
#Finding missing Calls to Low-Level System Libraries
missing.SystemLibraries <- sum(is.na(mydata$Calls.To.Low.level.System.Libraries)); missing.SystemLibraries
SystemLibraries.N <- missing.SystemLibraries/500*100; SystemLibraries.N

#Finding minimum Calls to Low-Level System Libraries
minimum.SystemLibraries <- min(mydata$Calls.To.Low.level.System.Libraries, na.rm = TRUE);minimum.SystemLibraries

#Finding maximum Calls to Low-Level System Libraries
maximum.SystemLibraries <- max(mydata$Calls.To.Low.level.System.Libraries, na.rm = TRUE);maximum.SystemLibraries

#Finding mean Calls to Low-Level System Libraries
mean.SystemLibraries <- mean(mydata$Calls.To.Low.level.System.Libraries, na.rm = TRUE);mean.SystemLibraries

#Finding median Calls to Low-Level System Libraries
median.SystemLibraries <- median(mydata$Calls.To.Low.level.System.Libraries, na.rm = TRUE); median.SystemLibraries

#Finding skewnwess Calls to Low-Level System Libraries
skewness.SystemLibraries <- skewness(mydata$Calls.To.Low.level.System.Libraries, na.rm = TRUE); skewness.SystemLibraries

#//////////////////////////////////////////////////#
#Finding missing Threads Started
missing.ThreadsStarted <- sum(is.na(mydata$Threads.Started)); missing.ThreadsStarted
ThreadsStarted.N <- missing.ThreadsStarted/500*100; ThreadsStarted.N

#Finding minimum Threads Started
minimum.ThreadsStarted <- min(mydata$Threads.Started, na.rm = TRUE);minimum.ThreadsStarted

#Finding maximum Threads Started
maximum.ThreadsStarted <- max(mydata$Threads.Started, na.rm = TRUE);maximum.ThreadsStarted

#Finding mean Threads Started
mean.ThreadsStarted <- mean(mydata$Threads.Started, na.rm = TRUE);mean.ThreadsStarted

#Finding median Threads Started
median.ThreadsStarted <- median(mydata$Threads.Started, na.rm = TRUE); median.ThreadsStarted

#Finding skewnwess Threads Started
skewness.ThreadsStarted <- skewness(mydata$Threads.Started, na.rm = TRUE); skewness.ThreadsStarted

#//////////////////////////////////////////////////#
#Finding missing Mean Word Length of Extracted Strings
missing.ExtractedStrings <- sum(is.na(mydata$Mean.Word.Length.of.Extracted.Strings)); missing.ExtractedStrings
ExtractedStrings.N <- missing.ExtractedStrings/500*100; ExtractedStrings.N

#Finding minimum Mean Word Length of Extracted Strings
minimum.ExtractedStrings <- min(mydata$Mean.Word.Length.of.Extracted.Strings, na.rm = TRUE);minimum.ExtractedStrings

#Finding maximum Mean Word Length of Extracted Strings
maximum.ExtractedStrings <- max(mydata$Mean.Word.Length.of.Extracted.Strings, na.rm = TRUE);maximum.ExtractedStrings

#Finding mean Mean Word Length of Extracted Strings
mean.ExtractedStrings <- mean(mydata$Mean.Word.Length.of.Extracted.Strings, na.rm = TRUE);mean.ExtractedStrings

#Finding median Mean Word Length of Extracted Strings
median.ExtractedStrings <- median(mydata$Mean.Word.Length.of.Extracted.Strings, na.rm = TRUE); median.ExtractedStrings

#Finding skewnwess Mean Word Length of Extracted Strings
skewness.ExtractedStrings <- skewness(mydata$Mean.Word.Length.of.Extracted.Strings, na.rm = TRUE); skewness.ExtractedStrings

#/////////////////////////////////////////////////#
#Finding missing Similarity Score
missing.SimilarityScore <- sum(is.na(mydata$Similarity.Score)); missing.SimilarityScore
SimilarityScore.N <- missing.SimilarityScore/500*100; SimilarityScore.N

#Finding minimum Similarity Score
minimum.SimilarityScore <- min(mydata$Similarity.Score, na.rm = TRUE);minimum.SimilarityScore

#Finding maximum Similarity Score
maximum.SimilarityScore <- max(mydata$Similarity.Score, na.rm = TRUE);maximum.SimilarityScore

#Finding mean Similarity Score
mean.SimilarityScore <- mean(mydata$Similarity.Score, na.rm = TRUE);mean.SimilarityScore

#Finding median Similarity Score
median.SimilarityScore <- median(mydata$Similarity.Score, na.rm = TRUE); median.SimilarityScore

#Finding skewnwess Similarity Score
skewness.SimilarityScore <- skewness(mydata$Similarity.Score, na.rm = TRUE); skewness.SimilarityScore

#///////////////////////////////////////////////////#
#Finding missing Characters in URL
missing.CharactersinURL <- sum(is.na(mydata$Characters.in.URL)); missing.CharactersinURL
CharactersinURL.N <- missing.CharactersinURL/500*100; CharactersinURL.N

#Finding minimum Characters in URL
minimum.CharactersinURL <- min(mydata$Characters.in.URL, na.rm = TRUE);minimum.CharactersinURL

#Finding maximum Characters in URL
maximum.CharactersinURL <- max(mydata$Characters.in.URL, na.rm = TRUE);maximum.CharactersinURL

#Finding mean Characters in URL
mean.CharactersinURL <- mean(mydata$Characters.in.URL, na.rm = TRUE);mean.CharactersinURL

#Finding median Characters in URL
median.CharactersinURL <- median(mydata$Characters.in.URL, na.rm = TRUE); median.CharactersinURL

#Finding skewnwess Characters in URL
skewness.CharactersinURL <- skewness(mydata$Characters.in.URL, na.rm = TRUE); skewness.CharactersinURL

#///////////////////////////////////////////////////#
#A point x is an outlier if it falls outside of the range given by Q1 ??? 1.5 × IQR ??? x ??? Q3 + 1.5 × IQR.
fivenum(mydata$Characters.in.URL)
IQR(mydata$Characters.in.URL)
#
fivenum(mydata$Similarity.Score)
IQR(mydata$Similarity.Score)
#
fivenum(mydata$Mean.Word.Length.of.Extracted.Strings)
IQR(mydata$Mean.Word.Length.of.Extracted.Strings)

#using boxplot.stats() to find all the outliers for the numeric variables
boxplot.stats(mydata$Ping.Time.To.Server)$out
boxplot.stats(mydata$File.Size..Bytes.)$out
boxplot.stats(mydata$How.Many.Times.File.Seen)$out
boxplot.stats(mydata$Calls.To.Low.level.System.Libraries)$out
boxplot.stats(mydata$Threads.Started)$out
boxplot.stats(mydata$Mean.Word.Length.of.Extracted.Strings)$out
boxplot.stats(mydata$Similarity.Score)$out
boxplot.stats(mydata$Characters.in.URL)$out
#
sum(is.na(mydata))
# clean up Data

#Drop -1 the unused level from Download Speed Category
levels(mydata$Download.Speed)
mydata$Download.Speed <- droplevels(mydata$Download.Speed)
mydata$Download.Speed
#
mydata$Download.Source[is.na(mydata$Download.Source)] <- "Email"
sapply(mydata, function(x) sum(is.na(x)))
#
#Replacing na with 0 for my numeric variables 
mydata$Ping.Time.To.Server[is.na(mydata$Ping.Time.To.Server)] <- 0
mydata$Calls.To.Low.level.System.Libraries[is.na(mydata$Calls.To.Low.level.System.Libraries)] <- 0
#
#log-transformation to handle outliers
hist(mydata$Calls.To.Low.level.System.Libraries, col='steelblue', main='Original')
log_y <- log10(mydata$Calls.To.Low.level.System.Libraries)
hist(log_y, col='coral2', main='Log Transformed')
#
hist(mydata$Ping.Time.To.Server, col='steelblue', main='Original')
log_y <- log10(mydata$Ping.Time.To.Server)
hist(log_y, col='coral2', main='Log Transformed')
#
hist(mydata$File.Size..Bytes., col='steelblue', main='Original')
log_y <- log10(mydata$File.Size..Bytes.)
hist(log_y, col='coral2', main='Log Transformed')
#
hist(mydata$How.Many.Times.File.Seen, col='steelblue', main='Original')
log_y <- log10(mydata$How.Many.Times.File.Seen)
hist(log_y, col='coral2', main='Log Transformed')
#
hist(mydata$Threads.Started, col='steelblue', main='Original')
log_y <- log10(mydata$Calls.To.Low.level.System.Libraries)
hist(log_y, col='coral2', main='Log Transformed')
#
hist(mydata$Characters.in.URL, col='steelblue', main='Original')
log_y <- log10(mydata$Characters.in.URL)
hist(log_y, col='coral2', main='Log Transformed')
#
write.csv(mydata,"mydata.csv") 

#Extracting Data
mydata[  ,c ("Ping.Time.To.Server","File.Size..Bytes.", 
             "How.Many.Times.File.Seen", "Calls.To.Low.level.System.Libraries",
             "Threads.Started", "Mean.Word.Length.of.Extracted.Strings",
             "Similarity.Score", "Characters.in.URL",
             "Actually.Malicious")]
tib  <- tibble(mydata[  ,c ("Ping.Time.To.Server","File.Size..Bytes.", 
                            "How.Many.Times.File.Seen", "Calls.To.Low.level.System.Libraries",
                            "Threads.Started", "Mean.Word.Length.of.Extracted.Strings",
                            "Similarity.Score", "Characters.in.URL",
                            "Actually.Malicious")])
 
View(tib)
#

#Scatter plot matrix
pairs(tib[,1:8],
      pch=23, #Shape of the points
      #Colour of the outline of the points
      col=as.numeric(tib$Actually.Malicious)+1,
      #Fill colour of the points with reduced colour intensity
      bg=alpha(as.numeric(tib$Actually.Malicious)+1,0.8),
      cex=1.5, #Size of the points
      upper.panel=NULL, #Do not display the the upper panel
      #Substitution the full stop in the feature names with a space
      labels=gsub("[[:punct:]]"," ",colnames(tib[,1:8])))

pca.tib <- prcomp(tib[,1:8], #my tib dataset excluding the last column
                     scale=TRUE) #Standardised the data
summary(pca.tib)  # To Show the individual and cumulative proportion of variance explained.

pca.tib$rotation

#Creating scree plot
plot(pca.tib, type="l", main="Scree plot - tib")
#
varexp.tib <- summary(pca.tib)$importance; varexp.tib
df <- tibble(Variance=varexp.tib[1,]^2,PC=1:length(pca.tib$sdev));
ggplot(df,aes(PC,Variance))+
   geom_line(colour="steelblue",size=1.5,linetype=2)+
   geom_point(size=5)+
   theme_minimal(base_size=14)+
   xlab("Principal Component")+
   ylab("Variance")+
   scale_x_discrete(limits=paste("PC",1:length(pca.tib$sdev),sep=""))+
   annotate("text",x=c(1:8)+0.15,y=varexp.tib[1,]^2+0.3,
            label=paste(round(varexp.tib[2,]*100,1),"%",sep=""))
#
#visualise our data in the first two dimensions, i.e. PC1 and PC2.
df <- data.frame(pca.tib$x, #PCA scores
                   Malicious=tib$Actually.Malicious);

ggplot(df,aes(x=PC1,y=PC2))+
   geom_point(aes(colour=Malicious),alpha=0.8,size=4)+
   theme_minimal(base_size=14)+
   theme(legend.position = "top")+
   xlab("PC1")+
   ylab("PC2");


#
view(pca.tib$x)
#biplot
fviz_pca_biplot(pca.tib,
                axes = c(1,2), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=tib$Actually.Malicious, #Outline colour of the shape
                fill.ind=tib$Actually.Malicious, #fill colour of the shape
                alpha=10, #transparency of the fill colour
                pointsize=2, #Size of the shape
                pointshape=18, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="Actually.Malicious",fill="Actually.Malicious",alpha="Actually.Malicious"))

#view correlation
view(cor(tib[, 1:8]))

