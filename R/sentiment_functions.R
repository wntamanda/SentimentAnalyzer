#' Calculate Sentiment
#'
#' This function calculates sentiment based on the presence of positive and negative words.
#' @param text A character string or vector of text to analyze.
#' @examples
#' calculate_sentiment("I love this product!")
calculate_sentiment <- function(text) {
  positive_words <- c("good", "great", "love", "excellent", "happy")
  negative_words <- c("bad", "worst", "dislike", "terrible", "sad")

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
