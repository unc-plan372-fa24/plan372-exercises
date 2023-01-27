# This file demonstrates estimating a "topic model" - a model that assigns text documents
# to topics based on the words they contain (a bag-of-words approach). It also demonstrates
# loading text data, tokenizing it, removing stopwords, and stemming it. We will use the
# grants data scraped from the California Grants Portal.

# Load libraries
# In addition to tidyverse, we will be using
# - tidytext, a text-processing library that interacts well with tidyverse
# - textste, which includes functionality to stem/lemmatize words
# - topicmodels, which performs topic modelling
library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)

grants = read_csv("ca_grants_all.csv")

# First, we need to tokenize the grant description. This will split the text up
# into individual words, one word per row. We are "unnesting" the description field
# into a new field "word"
tokens = unnest_tokens(grants, word, description)

head(tokens)

# we now will likely want to remove "stopwords" - common words that will appear often
# in all topics - for instance, the, a, and, I, etc. tidytext includes a dataframe
# stop_words with common English stop words.
stop_words

# to remove stop words, we use the anti_join tidyverse command, which returns all
# rows that did _not_ match.
tokens = anti_join(tokens, stop_words, by="word")

# Now, we may or may not want to stem words. Sometimes it is helpful to stem words,
# sometimes it is not, it depends on the application, the method, and the interpretation.
# To lemmatize words, we will use the lemmatize_words function from the textstem package.
tokens = mutate(tokens, lemma=lemmatize_words(word))
View(head(tokens))

# Now we have finished our data preparation. We are almost ready to build the topic model.
# The last step is to reformat our data. We have a simple list of words by document,
# but the topicmodels package needs a matrix of number of each word per document. First,
# we create a table with word counts by document. Here we will use the stemmed
# words.
wcounts = group_by(tokens, title, lemma) %>% summarize(count=n())

# now we turn it into a matrix that can be used in a topic model.
# We tell the matrix function which column identifies the document, the word or
# stem, and the count in the document.
word_mtx = cast_dtm(wcounts, title, lemma, count)

# finally we can build the topic model
# we specify the matrix and the number of topics we would like to identify (there
# is not really a good way to determine this other than to compare models with several
# different numbers of classes),
# control=list(seed=42) controls the pseudorandom number generator. Like many computational
# statistical algorithms, LDA uses random numbers, which means each time you run an
# LDA you will get a slightly different result. Setting the seed ensures consistency.
# What you set the seed to doesn't matter as long as it's consistent.
model = LDA(word_mtx, 25, control=list(seed=42))

# There are two interesting sets of estimated parameters in an LDA, the beta and
# gamma matrix. The beta matrix tells you how often words are associated with a
# particular topic, while the gamma matrix tells you how often topics are associated
# with documents.
beta = tidy(model, matrix="beta")
View(beta)

# The beta matrix contains for each topic the probability that each word is associated
# with that topic. Of course, most words are unlikely to be associated with most topics.
# We can filter to just the top 5 or 10 words for each topic as these are likely to be
# quite relevant to the topic.
top_10 = group_by(beta, topic) %>% slice_max(beta, n=10)
top_10

# We can also plot these with ggplot. 
# this creates a ggplot with the top 10 data frame, sorts it by term and then by beta,
# plots beta on the x axis, and 
# this part of the code is closely based on https://www.tidytextmining.com/topicmodeling.html
ggplot(top_10, aes(y=reorder_within(term, beta, topic), x=beta)) +
  # this says to make a column plot (bar plot, with default stat identity)
  geom_col() +
  # This "facets" the plot into subplots by topic.
  facet_wrap(~topic, scales="free") +
  # this allows the reordered y axis to display correctly.
  scale_y_reordered()


# We can also look at the gamma matrix to see what documents are in what
# topics.
gamma = tidy(model, matrix="gamma")

# for instance, let's take a look at topic 6, which seems to have
# something to do with clean water
filter(gamma, topic==6) %>% arrange(-gamma)

# Not all categories are this well defined. Some have a hodgepodge of different
# grants.
filter(gamma, topic==20) %>% arrange(-gamma)

# adding additional stopwords
# Words like "california", "eligible" etc. are a large part of many of the
# grant descriptions, unsurprisingly. We can remove these as they are unlikely to
# be useful in differentiating categories. To do this, we will create a new stop
# words table with the words we want to remove. We may also want to remove words that
# appear in a subset of grants, such as "loan" or "requirement" that do not define
# useful categories of grant.

# since stop words are generally removed before stemming/lemmatization, it's a useful to look at
# which lemmas were associated with which words, so we can remove them all
unique(filter(tokens, lemma=="applicant")$word)
custom_stop_words = bind_rows(
  stop_words,  # we still want to keep all of the original stop words
  tibble(word=c("stop", "words", "here"))
)

# Remove the stop words and repeat the LDA. Experiment with the number of topics.

# Does removing stop words improve the quality of the results?

# How do the results differ if you stem (stem_words function) instead of lemmatizing?


