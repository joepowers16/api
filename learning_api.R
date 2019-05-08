#' Overview: learning to call API from RStudio
#' Date: 2019-05-08
#' Author: JP
#' 
#' Overview: 
#' APIs can be easily accessde from RStudio using the httr package.
#' APIs will usually supply their data in json format which will need to be 
#' transformed into dataframes via jsonlite package. 



# code from https://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html

library(httr)
library(jsonlite)
library(lubridate)

options(stringsAsFactors = FALSE)

# Retrieving valid classifiers ####
url  <- "http://api.epdb.eu"
path <- "eurlex/directory_code"

raw.result <- GET(url = url, path = path)

names(raw.result)
class(raw.result)
str(raw.result)

raw.result$status_code # 200 means all is fine

head(raw.result$content)

this.raw.content <- rawToChar(raw.result$content)

nchar(this.raw.content)

substr(this.raw.content, 1, 100)

this.content <- fromJSON(this.raw.content)

this.content[[1]] 

this.content.df <- do.call(
  what = "rbind", # bind all df that follow
  args = lapply(this.content, as.data.frame) # convert ea list element to df
)

head(this.content.df)

# Extracting energy classifiers ####
headClass <- 
  substr(
    x = this.content.df[, "directory_code"],
    start = 1,
    stop  = 2
  )

head(headClass)

isEnergy <- headClass == "12"
table(isEnergy) 
  
relevant.df <- this.content.df[isEnergy, ]
  
relevant.dc <- relevant.df[, "directory_code"]

# Retrieving metadata ####
makeQuery <- function(classifier) {
  this.query <- list(classifier)
  names(this.query) <- "dc"
  return(this.query)
}

queries <- lapply(as.list(relevant.dc), makeQuery)

this.raw.result <- GET(url = url, path = path, query = queries[[1]])

this.result <- fromJSON(rawToChar(this.raw.result$content))

names(this.result[[1]])

all.results <- vector(mode = "list", length = length(relevant.dc))

for (i in 1:length(all.results)) {
  this.query       <- queries[[i]]
  this.raw.answer  <- GET(url = url, path = path, query = this.query)
  this.answer      <- fromJSON(rawToChar(this.raw.answer$content))
  all.results[[i]] <- this.answer
  message(".", appendLF = FALSE)
  Sys.sleep(time = 1)
}
  
 
parseAnswer <- function(answer) {
  this.form   <- answer$form
  this.date   <- answer$date
  this.effect <- answer$of_effect
  result <- data.frame(form   = this.form,
                       date   = this.date,
                       effect = this.effect)
  return(result)
}


parseAnswer(all.results[[1]][[2]])

parsedAnswers <- 
  lapply(all.results, function(x) do.call("rbind", lapply(x, parseAnswer)))

finalResult <- do.call("rbind", parsedAnswers)
class(finalResult) 

head(finalResult)

# Working with dates ####
date.character <- "1981-05-02"
date.POSIXct <- ymd(date.character)

class(date.character)

class(date.POSIXct) 

finalResult$date <- ymd(finalResult$date)
finalResult$effect <- ymd(finalResult$effect)

finalResult$effectDay <- wday(finalResult$effect, label = TRUE)
table(finalResult$effectDay) 
