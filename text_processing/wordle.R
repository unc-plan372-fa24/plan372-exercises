# Wordle is a popular game where you try to guess a five-letter word, and the
# game will tell you which letters you have right. This is a simplified version
# of Wordle, which will only tell you which letters you have correct that are
# in the correct position. There is a list of all possible words in the words.csv
# file.

# These are functions that I wrote to play the game. The first function chooses a word
# and the second checks whether a word is correct.

library(tidyverse)

# these words are from https://github.com/hackerb9/gwordlist
# which originally came from Google Books
words = read_csv("words.csv")

WORDLEN = 5

choose_word = function () {
  word = words$word[str_length(words$word) == WORDLEN] |> sample(1)
  # use a list to obfuscate answer in data view
  chosen_word <<- list(str_to_lower(word))
}

check_word = function(candidate) {
  stopifnot(str_length(candidate) == WORDLEN)
  
  cat(paste0(candidate, "\n", paste0(vapply(1:WORDLEN, function(i) {
    letter = substr(candidate, i, i)
    if (letter == substr(chosen_word[[1]], i, i)) {
      return("^")
    } else {
      return(" ")
    }
  }, "x"), collapse=""))[[1]])
}

# choose a word
choose_word()

# choose an initial word, and use check_word to check it, until you figure out a single
# letter.
check_word("your word here")

# Then, use a regex to filter the list of words to find possible candidates
filter(words, str_detect(word, "your regex here"))

