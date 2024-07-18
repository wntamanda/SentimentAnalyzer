#' Calculate Sentiment
#'
#' This function calculates sentiment based on the presence of positive and negative words.
#' @param text A character string or vector of text to analyze.
#' @examples
#' calculate_sentiment("I love this product!")
calculate_sentiment <- function(text) {
  positive_words <- c("good", "great", "love", "excellent", "happy", "fantastic", "wonderful", "amazing", "pleased", "favorite")
  negative_words <- c("bad", "worst", "dislike", "terrible", "sad", "horrible", "disappointed", "poor", "hate", "awful")

  words <- tolower(unlist(strsplit(as.character(text), "\\s+")))
  score <- sum(words %in% positive_words) - sum(words %in% negative_words)
  return(score)
}

#' Summary of Sentiments
#'
#' This function returns a summary of sentiments for a vector of texts.
#' @param texts A character vector of texts to analyze.
#' @examples
#' summary_sentiment(c("I love this product!", "I hate this!"))
summary_sentiment <- function(texts) {
  sapply(texts, calculate_sentiment)
}

#' Classify Sentiment
#'
#' This function classifies the sentiment as positive (1) or negative (0) based on sentiment score.
#' @param score A numeric value of sentiment score.
#' @examples
#' classify_sentiment(3) # Returns 1
#' classify_sentiment(-1) # Returns 0
classify_sentiment <- function(score) {
  if(score > 0) {
    return(1) # Positive
  } else {
    return(0) # Negative
  }
}
