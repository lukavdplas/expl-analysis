library('rvest')
library('xml2')

#---------------------------------------------------------------------
#import and parse table

process.data <- function(filename, language) {
  data <- read.csv(filename, comment.char = '#', encoding = 'UTF-8', header = FALSE, col.names = c('time_recorded', 'participant', 'controller_name', 'item_number', 'element_number', 'type', 'group', 'question', 'answer', 'correct', 'response_time'))

  #get participant IDs instead of ip strings
  all.ips <- unique(data$participant)
  id.col <- sapply(data$participant, function(ip) { which(ip == all.ips)})
  
  #add 100 or 200 depending on language
  if (language == 'french') { t = 100 } else {t = 200}
  id.col <- sapply(id.col, function (x) {x + t})
  
  data$participant <- id.col
  

  #clean up html
  clean <- function (string) { html_text(read_html(paste('<i>', string, '</i>')))  } #clean up one string. i add italics to force reading the string as html code, not a file location
  clean.col <- function (vector) { sapply(vector, function (item) { clean(as.character(item))}) }
  data$question <- clean.col(data$question)
  data$answer <- clean.col(data$answer)
  
  #clean up whitespace
  clean.ws.col <- function (col) {sapply(col, trimws)}
  data$question <- clean.ws.col(data$question)
  data$answer <- clean.ws.col(data$answer)
  
  #add language column
  data$language <- language
  
  #return
  data
}

#parse per language
fr.data <- process.data("results_fr.csv", 'french')
nl.data <- process.data("results_nl.csv", 'dutch')

#put all data together - this is the table i will be using
all.data <- rbind(fr.data, nl.data)

#---------------------------------------------------------------------
#organise data by participant (useful for demographics)

organise.by.participant <- function(data) {
  #make table
  all.ids <- unique(data$participant)
  table <- data.frame(id = all.ids)
  
  #get consent
  consents <- subset(data, question == 'consent')$answer
  table$consent <- consents
  
  #get age
  ages <- as.numeric(subset(data, question == 'age')$answer)
  table$age <- ages
  
  #get experiment language
  langs <- subset(data, question == 'consent')$language
  table$language <- langs  
  
  #return
  table
}

p.data <- organise.by.participant(all.data)

#---------------------------------------------------------------------
#add explanatory columns

#add column for item

#items are:
#distractor (dolphins)
#filler1 (blender)
#filler2 (laptop)
#item1 (camera)
#item2 (filter)
#item3 (printer)
#item4 (bike)

items <- rep('NULL', times = length(all.data$time_recorded))

keys <- c('distractor', 'filler1', 'filler2', 'item1', 'item2', 'item3', 'item4')

for(key in keys) {
  #get all matches for the key
  matches.question <- as.vector(sapply(all.data$question, function ( item ) { grepl(key, as.character(item))}))
  matches.type <- as.vector(sapply(all.data$type, function ( item ) { grepl(key, as.character(item))}))
  matches <- matches.question | matches.type
  
  #fill in the key in those positions
  items[which(matches)] <- key
}

all.data$item <- items



#add column for product

product.list <- list('dolphins', 'blender', 'laptop', 'camera', 'filter', 'printer', 'bike')
names(product.list) <- keys

products.col <- as.vector(sapply(all.data$item, function (item) {as.character(product.list[item])}))

all.data$product <- products.col



#add column for topic that we're investigating

keys <- c('att', 'compr', 'syn', 'recall')
topics.list <- list('attitude', 'comprehension', 'recognition', 'recall')
names(topics.list) <- keys

topics.col <- rep('NULL', times = length(all.data$time_recorded))


for(key in keys) {
  #get all matches for the key
  matches <- as.vector(sapply(all.data$type, function ( item ) { grepl(key, as.character(item))}))
  
  #fill in the key in those positions
  topics.col[which(matches)] <- as.character(topics.list[key])
}

all.data$topic <- topics.col



#add column for active/passive and for dislocation / no dislocation


voices <- rep('NULL', times = nrow(all.data))
dislocations <- rep('NULL', times = nrow(all.data))

exp.items <- c('item1', 'item2', 'item3', 'item4')

for (i in 1:nrow(all.data)) {
  item <- all.data$item[i]
  if (is.element(item, exp.items)) {
    p <- all.data$participant[i]
    
    #find key of original stimulus
    p.data <- subset(all.data, all.data$participant == p) #data for this participant
    exp.n <- substring(item ,nchar(item)) #number of item
    keyrow <- subset(p.data, p.data$group == exp.n)
    key <- as.character(keyrow$type[1])
    
    #extract active/passive
    if (substring(key, 5, 7) == "act") {
      voice <- 'active'
    }
    else {
      voice <- 'passive'
    }
    voices[i] <- voice
    
    #extract dislocation / no dislocation
    if (substring(key, 9,10) == "di") {
      dislocation <- "yes"
    }
    else {
      dislocation <- "no"
    }
    dislocations[i] <- dislocation
  
  }
}

all.data$voice <- voices
all.data$dislocation <- dislocations

#--------------------------------------------------------------------------------
#clean up unnecessary columns and rows

filtered.data <- subset(all.data, all.data$type != "intro")

filtered.data <- filtered.data[, ! names(all.data) %in% c("time_recorded", "controller_name", "item_number", "element_number", "type", "group" )]

#---------------------------------------------------------------------------------
#export clean table

save(filtered.data, file = "exp_data.Rdata")