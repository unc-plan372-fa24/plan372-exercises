# Regular expressions
# This exercise introduces regular expressions and how to use them to match and extract data from strings
# Regular expressions describe patterns of characters in a string

# Regular expression reference: https://github.com/ziishaned/learn-regex
# stringr (R library for regular expressions) cheatsheet: https://github.com/rstudio/cheatsheets/blob/main/strings.pdf

library(tidyverse)

# Basic regular expressions: all alphanumeric characters match themselves
# So, for instance, the regular expression "cat" will match "I like cats"
# but not "Dogs rule"
str_detect("I like cats", "cat")
str_detect("Dogs rule", "cat")

# Regular expressions are case sensitive, "dog" will not match "Dogs rule" because of the
# capital D
str_detect("Dogs rule", "dog")

# But we can always use str_to_lower to convert our string to lowercase first - this is often a
# good idea when processing text
str_detect(str_to_lower("Dogs rule"), "dog")

# Many other characters have special meanings in regular expressions.
# . matches any single character. "d.g" will match "dog" or "dug", but not "dang"
str_detect("I just got a dog", "d.g")
str_detect("Who dug these holes in the yard?", "d.g")
str_detect("dang what a mess", "d.g")

# Character classes are lists of characters in brackets. They match any single
# character in the list. So "d[ou]g" will match "dog" or "dug", but not "dig". Since character
# classes only match one character, it will not match "doug"
str_detect("I just got a dog", "d[ou]g")
str_detect("Who dug these holes in the yard?", "d[ou]g")
str_detect("Does anyone here like to dig?", "d[ou]g")
str_detect("I think doug did", "d[ou]g")

# Special character classes
# Within a character class, you can use special, predefined sets of characters to
# avoid writing them all out. These are enclosed in [::]. For instance,
#  [:alpha:] - all alphabetic characters (A-Z, a-z, and locale-specific letters)
#  [:lower:] - lowercase letters
#  [:upper:] - uppercase letters
#  [:digit:] - digits 0-9
#  [:space:] - whitespace (spaces, tabs, etc.)
#  [:alnum:] - Alphabetic and numeric characters
#  [:punct:] - Punctuation
str_detect("R2D2", "[:alpha:][:digit:][:alpha:][:alnum:]")
str_detect("C3PO", "[:alpha:][:digit:][:alpha:][:alnum:]")
str_detect("r2d2", "[:alpha:][:digit:][:alpha:][:alnum:]")

# Inverting character classes
# If you put a ^ as the first character within a character class, it will match
# characters _not_ in that class


# Matching multiple letters
# adding a metacharacter after a character, character class, or group
# (which we'll cover in a minute) affects how many times
# it can occur in the matched pattern
#  + will match one or more
#  * will match 0 or more
#  ? will match 0 or 1
#  {n} will match exactly n
#  {n,m} will match between n and m
# "d[ou]+g" will match dug or dog, like before, but also doug
str_detect("I just got a dog", "d[ou]+g")
str_detect("Who dug these holes in the yard?", "d[ou]+g")
str_detect("Does anyone here like to dig?", "d[ou]+g")
str_detect("I think doug did", "d[ou]+g")

# dou?g will match dog or doug, but not dug - the u is optional, everything else is required
str_detect("I just got a dog", "dou?g")
str_detect("Who dug these holes in the yard?", "dou?g")
str_detect("Does anyone here like to dig?", "dou?g")
str_detect("I think doug did", "dou?g")

# d[ou]{2}g will match doug or duog, but not dog
str_detect("doug", "d[ou]{2}g")
str_detect("dog", "d[ou]{2}g")

# d[ou]{1,2}g will match doug or dog
str_detect("doug", "d[ou]{1,2}g")
str_detect("dog", "d[ou]{1,2}g")

# a[:alpha:]*s will match any word starting with a and ending with s, including as
str_detect("as", "a[:alpha:]*s")
str_detect("ads", "a[:alpha:]*s")
str_detect("arts", "a[:alpha:]*s")

# But it will also detect words with a and s in them, because the regular expression
# just looks for patterns, not what's before or after them
str_detect("carson", "a[:alpha:]*s")

# similary, "dog" will match "boondoggle"
str_detect("boondoggle", "dog")

# Anchors
# We need to "anchor" our pattern match to occur at a particular part of the string.
# There are three common anchors:
#  ^ matches the start of the string
#  $ matches the end
#  \\b matches the boundary between words (note: in R this is written as \\b, but in some languages will be \b)
# so "\\bdog\\b" will not match boondoggle, but will match dog
str_detect("dog", "\\bdog\\b")
str_detect("boondoggle", "\bdog\b")

# "^d[ou]+g$" will match "dog", "doug", and "dug", but not "the dog ran quickly"
str_detect("dog", "^d[ou]+g$")
str_detect("the dog ran quickly", "^d[ou]+g$")
