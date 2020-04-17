#-----------------------------------------------------------------------
#import

library(ggplot2)
library(lme4)

load("exp_data_with_correctness.Rdata")


#---------------------------------------------------------------------
#filter to relevant data


#remove unnecessary columns
exp.data <- filtered.data[, ! names(filtered.data) %in% c("response_time", "correct")]

#remove fillers
is.experimental <- function (item) {
  substring(item, 1,4) == "item"
}

is.filler <- function (item) {
  ! is.experimental(item)
}

filler.data <- subset(exp.data, sapply(filtered.data$item, is.filler))

exp.data <- subset(exp.data, sapply(exp.data$item, is.experimental))

#add boolean columns to dataframe
exp.data$active <- exp.data$voice == "active"
exp.data$dislocated <- exp.data$dislocation == "yes"

#add combined column to dataframe
exp.data$condition <- sapply(1:nrow(exp.data), function(i) {paste(exp.data$voice[i], "+", exp.data$dislocation[i])})

#extract french data
fr.data <- subset(exp.data, exp.data$language == "french")

#----------------------------------------------------------------------
# general functions

get.data.for.condition <- function(data, voice.value, disloc.value) {
  filtered.results <- subset(data, data$voice == voice.value & data$dislocation == disloc.value)
  
  filtered.results
}

voice.col <- rep(c('active', 'passive'), times = 2)
disloc.col <- c(rep('yes', times = 2), rep('no', times = 2))

get.stat.per.condition <- function(data, stat) {
  #input data and a function that gives a stat based on a dataframe. output is a column with the stat for each condition
  output.col <- c(stat(get.data.for.condition(data, 'active', 'yes')), stat(get.data.for.condition(data, 'passive', 'yes')), stat(get.data.for.condition(data, 'active', 'no')), stat(get.data.for.condition(data, 'passive', 'no')))
  
  output.col
}

#----------------------------------------------------------------------
#analyse COMPREHENSION

compr.data <- subset(fr.data, fr.data$topic == 'comprehension')

#make plot 

compute.percentage <- function(data) {
  total <- nrow(data)
  correct.results <- subset(data, data$correctness == TRUE)
  correct <- nrow(correct.results)
  return(correct / total)
  
}

ratios.col <- get.stat.per.condition(compr.data, compute.percentage)
plot.data <- data.frame(voice = voice.col, dislocation = disloc.col, correct = ratios.col)

plot <- ggplot(plot.data) +
  geom_col(aes(x = dislocation, y = correct, fill = voice), position = 'dodge') +
  scale_y_continuous(limits = c(0,1))
plot

#analyse using chi square
compr.data$correctness <- as.logical(compr.data$correctness)

table(compr.data$condition, compr.data$correctness)

chisq.test(x = compr.data$dislocated, y = compr.data$correctness)
chisq.test(x = compr.data$active, y = compr.data$correctness)
chisq.test(x = compr.data$condition, y = compr.data$correctness)

chisq.test(x = compr.data$product, y = compr.data$correctness)
chisq.test(x = compr.data$participant, y = compr.data$correctness)

#analyse using logistic regression

m1 <- glm(correctness ~ dislocated * active, family = "binomial", data = compr.data)


#----------------------------------------------------------------------
#analyse RECALL

recall.data <- subset(fr.data, fr.data$topic == 'recall')

#make plot 

compute.percentage <- function(data) {
  correct.results <- subset(data, data$correctness == TRUE)
  correct <- nrow(correct.results)
  
  incorrect.results  <- subset(data, data$correctness == FALSE)
  incorrect <-  nrow(incorrect.results)
  
  total <- incorrect + correct
  
  return(correct / total)
  
}


compute.percentage(subset(filtered.data, filtered.data$topic == "recall" & filtered.data$language == "french")) #overall percentage for french data
compute.percentage(subset(filler.data, filler.data$topic == "recall" & filler.data$language == "french"))
compute.percentage(recall.data)


ratios.col <- get.stat.per.condition(recall.data, compute.percentage)
plot.data <- data.frame(voice = voice.col, dislocation = disloc.col, correct = ratios.col)

plot <- ggplot(plot.data) +
  geom_col(aes(x = dislocation, y = correct, fill = voice), position = 'dodge') +
  scale_y_continuous(limits = c(0,1))
plot

#analyse using chi square
recall.data$correctness <- as.logical(recall.data$correctness)

table(recall.data$condition, recall.data$correctness)

chisq.test(x = recall.data$dislocated, y = recall.data$correctness)
chisq.test(x = recall.data$active, y = recall.data$correctness)
chisq.test(x = recall.data$condition, y = recall.data$correctness)

chisq.test(x = recall.data$product, y = recall.data$correctness)

#analyse using logistic regression
m1 <- glm(correctness ~ dislocation, family = "binomial", data = recall.data)
m2 <- glm(correctness ~ voice, family = "binomial", data = recall.data)
m3 <- glm(correctness ~ dislocation * voice, family = "binomial", data = recall.data)


#-----------------------------------------------------------------------
#analyse RECOGNITION

recog.data <- subset(fr.data, fr.data$topic == 'recognition')

#make plot 

compute.percentage <- function(data) {
  total <- nrow(data)
  correct.results <- subset(data, data$correctness == TRUE)
  correct <- nrow(correct.results)
  return(correct / total)
  
}

ratios.col <- get.stat.per.condition(recog.data, compute.percentage)
plot.data <- data.frame(voice = voice.col, dislocation = disloc.col, correct = ratios.col)

plot <- ggplot(plot.data) +
  geom_col(aes(x = dislocation, y = correct, fill = voice), position = 'dodge') +
  scale_y_continuous(limits = c(0,1))
plot

#analyse using chi square
recog.data$correctness <- as.logical(recog.data$correctness)

table(recog.data$condition, recog.data$correctness)

chisq.test(x = recog.data$dislocated, y = recog.data$correctness)
chisq.test(x = recog.data$active, y = recog.data$correctness)
chisq.test(x = recog.data$condition, y = recog.data$correctness)

chisq.test(x = recog.data$product, y = recog.data$correctness)
chisq.test(x = recog.data$participant, y = recog.data$correctness)

#analyse using logistic regression

m1 <- glm(correctness ~ dislocated, family = "binomial", data = recog.data)
m2 <- glm(correctness ~  dislocated * active, family = "binomial", data = recog.data)
anova(m2, m1, test = "Chisq")

#-----------------------------------------------------------------------
#analyse ATTITUDE

att.data <- subset(fr.data, fr.data$topic == 'attitude')
att.data$answer <- as.numeric(att.data$answer)

#get all topics
topics <- unique(att.data$question)

#per topic, analyse data

analyse.attitude <- function(dataframe) {
  
  fit.product <- aov(answer ~ product, data = dataframe)
  
  fit.interaction <- aov(answer ~ dislocation * voice + product, data = dataframe)
  print(summary(fit.interaction))
}

for (i in 1:length(topics)) {
  topic <- topics[i]
  topic.data <- subset(att.data, att.data$question == topics[i]) #for some reason calling "topic" directly does not work
  
  
  
  print(topic)
  print(mean(topic.data$answer))
  print(sd(topic.data$answer))
  #analyse.attitude(topic.data)
}

#make plots
for (i in 1:length(topics)) {
  topic <- topics[i]
  topic.data <- subset(att.data, att.data$question == topics[i]) #for some reason calling "topic" directly does not work
  
  compute.mean.answer <- function(data) {
    mean(data$answer)
  }
  
  compute.sd.answer <- function(data) {
    sd(data$answer)
  }
  
  means.col <- get.stat.per.condition(topic.data, compute.mean.answer)
  sds.col <- get.stat.per.condition(topic.data, compute.sd.answer)
  plot.data <- data.frame(voice = voice.col, dislocation = disloc.col, mean_answer = means.col, sd_answer = sds.col)

  
  plot2 <- ggplot(data <- plot.data) +
    geom_crossbar(aes(x = voice, y = mean_answer, ymin = mean_answer - sd_answer, ymax = mean_answer + sd_answer, color = dislocation), position = position_dodge2(width = 0.75, padding = 0.2)) +
    ylab("response") 
    
  show(plot2)
}