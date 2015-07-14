#Problem 1.1:
clinical = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

max(nchar(clinical$abstract))

#Problem 1.2:
clinical$nchar = nchar(clinical$abstract)
noabs = subset(clinical , nchar == 0)
nrow(noabs)


#Problem 1.3:
which.min(nchar(clinical$title))
clinical$title[1258]

#Problem 1.4:
library(tm)
corpusTitle = Corpus(VectorSource(clinical$title))
corpusAbstract = Corpus(VectorSource(clinical$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english")) 
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)


#problem 2.3
which.max(colSums(dtmAbstract))

#Problem: 3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#Problem 3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trials = clinical$trial
str(dtm)

#Problem 3.3
library(caTools)
set.seed(144)
spl = sample.split(dtm$trials, 0.7)
train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)
table(dtm$trials)
1043/(1043+817)


#Problem 3.4
library(rpart)
library(rpart.plot)
trialCART = rpart(trials~., data=train, method= "class")
prp(trialCART)


#Problem: 3.5
predictCART = predict(trialCART, newdata = train)[,2]
table(predictCART)

#Problem: 3.7
table(train$trials, predictCART >= 0.5 )
(631+441)/(631+99+131+441)
(441)/(131+441)
(631)/(631+99)


















