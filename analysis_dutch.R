#-----------------------------------------------------------------------
#import

load("exp_data_with_correctness.Rdata")


#---------------------------------------------------------------------
#filter to relevant data


#remove unnecessary columns
exp.data <- filtered.data[, ! names(filtered.data) %in% c("response_time", "correct")]

#remove fillers
is.experimental <- function (item) {
  substring(item, 1,4) == "item"
}

exp.data <- subset(exp.data, sapply(exp.data$item, is.experimental))

#add boolean columns to dataframe
exp.data$active <- exp.data$voice == "active"
exp.data$dislocated <- exp.data$dislocation == "yes"

#add combined column to dataframe
exp.data$condition <- sapply(1:nrow(exp.data), function(i) {paste(exp.data$voice[i], "+", exp.data$dislocation[i])})

#extract french data
nl.data <- subset(exp.data, exp.data$language == "dutch")

#----------------------------------------------------------------------
#analyse COMPREHENSION

compr.data <- subset(nl.data, nl.data$topic == 'comprehension')
compr.data$correctness <- as.logical(compr.data$correctness)

table(compr.data$dislocation, compr.data$correctness)
table(compr.data$voice, compr.data$correctness)


#----------------------------------------------------------------------
#analyse RECALL

recall.data <- subset(nl.data, nl.data$topic == 'recall')
recall.data$correctness <- as.logical(recall.data$correctness)

table(recall.data$dislocation, recall.data$correctness)
table(recall.data$voice, recall.data$correctness)


#----------------------------------------------------------------------
#analyse RECOGNITION

recog.data <- subset(nl.data, nl.data$topic == 'recognition')
recog.data$correctness <- as.logical(recog.data$correctness)

table(recog.data$dislocation, recog.data$correctness)
table(recog.data$voice, recog.data$correctness)

#----------------------------------------------------------------------
#analyse ATTITUDE


att.data <- subset(nl.data, nl.data$topic == 'attitude')
att.data$answer <- as.numeric(att.data$answer)

#get all topics
topics <- unique(att.data$question)

#per topic, analyse data

for (i in 1:length(topics)) {
  topic <- topics[i]
  topic.data <- subset(att.data, att.data$question == topics[i]) #for some reason calling "topic" directly does not work
  
  
  
  print(topic)
  print("passive mean")
  subdata <- subset(topic.data, voice == "passive")
  print(mean(subdata$answer))
  
  print("active mean")
  subdata <- subset(topic.data, voice == "active")
  print(mean(subdata$answer))
  
  print("dislocated mean")
  subdata <- subset(topic.data, dislocated == "TRUE")
  print(mean(subdata$answer))
  
  print("not dislocated mean")
  subdata <- subset(topic.data, dislocated == "FALSE")
  print(mean(subdata$answer))
  
}
