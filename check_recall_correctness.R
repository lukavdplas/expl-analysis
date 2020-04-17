#-----------------------------------------------------------------------
#import

load("exp_data.Rdata")

slogan.data <- read.csv("slogans.csv", encoding = 'UTF-8')

#-----------------------------------------------------------------------
#add column with original slogans

get.slogan <- function(item_input, voice_input, disloc_input, lang_input) {
  subdata <- subset(slogan.data, slogan.data$item == item_input & slogan.data$voice == voice_input & slogan.data$dislocation == disloc_input & slogan.data$language == lang_input)
  slogan <- as.character(subdata$slogan[1])
  return(slogan)
}

print(get.slogan("item1", "passive", "no", "french"))

get.row.slogan <- function(i) {
  item <- filtered.data$item[i]
  voice <- filtered.data$voice[i]
  disloc <- filtered.data$dislocation[i]
  lang <- filtered.data$language[i] 
  
  if (substring(item, 1,4) == "item") { 
    slogan <- get.slogan(item, voice, disloc, lang)
    return(slogan) 
    }
  else {
    return(NULL)
  }
}

slogan.col <- sapply(1:nrow(filtered.data), get.row.slogan)

filtered.data$slogan <- slogan.col

#-----------------------------------------------------------------------
#compute correctness

library(stringdist)


#give distance between phrases, with some preprocessing

clean.string <- function(phrase) {
  #clean up strings for easy comparison
  phrase <- tolower(phrase) #make lowercase
  phrase <- trimws(phrase, whitespace = "[ .]") #remove irrelevant edge characters
  
  return(phrase)
}

compare.phrases <- function(phrase.1, phrase.2, maxdist = 5) {
#strip irrelevant edge characters
  phrase.1 <- clean.string(phrase.1)
  phrase.2 <- clean.string(phrase.2)
  
  if (is.na(phrase.2)) {
    #fix for when some of the fillers dont have an original slogan filled in
    dist <- nchar(phrase.1)
  }
  else {
    dist <- stringdist(phrase.1, phrase.2)
  }
    
  if (dist <= maxdist) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

eval.response <- function(i, maxdist) {
  response <- as.character(filtered.data$answer[i])
  original <- as.character(filtered.data$slogan[i])
  
  matching <- compare.phrases(response, original, maxdist)
  
  if (matching) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#---------------------------------------------------------
#test edit distances

recall.ratio <- function(d) {
  recall.distances <- sapply(which(filtered.data$topic == "recall" & filtered.data$language == "french"), function(i) { eval.response(i, d)})
  recall.ratio <- sum(recall.distances) / length(recall.distances)
  return(recall.ratio)  
}

recog.ratio <- function(d) {
  recog.distances <- sapply(which(filtered.data$topic == "recognition" & filtered.data$language == "french"),  function(i) { eval.response(i, d)})
  recog.ratio <- sum(recog.distances) / length(recog.distances)
  return(recog.ratio)
}

X <- (1:20)
recall.curve <- sapply(X, recall.ratio)
recog.curve <- sapply(X, recog.ratio)

plot(x = X, y = recog.curve, col = "red")
lines(x = X, y = recog.curve, col = "red")
points(x = X, y = recall.curve)
lines(x = X, y = recall.curve)

#----------------------------------------------------
#calculate actual column

get.row.correctness <- function(i) {
  topic <- filtered.data$topic[i]
  if (topic == "attitude") {
    return (NULL)
  }
  if (topic == "comprehension") {
    if (filtered.data$correct[i] == 1) {
      return(TRUE)
    }
    else {
      return(FALSE)
    }
  }
  if (topic == "recall") {
    return(eval.response(i, 10))
  }
  if (topic == "recognition") {
    return(eval.response(i, 8))
  }
  
  return(NULL)
}

correctness.col <- sapply(1:nrow(filtered.data), get.row.correctness)

#---------------------------------------------------------
#export

filtered.data$correctness <- correctness.col

save(filtered.data, file = "exp_data_with_correctness.Rdata")
