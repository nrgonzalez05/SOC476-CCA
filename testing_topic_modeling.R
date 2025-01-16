## Testing Topic Modeling for Past Paper 


# Install and load necessary libraries
install.packages("tm")
install.packages("topicmodels")
install.packages("tidyverse")
library(tm)
library(topicmodels)
library(tidyverse)

# Load your text document
# Replace "your_document.txt" with your actual document file path
doc <- readLines("/Users/nicholasrgonzalez/Documents/SOC476-CCA/2024_STOTU")

# Create a text corpus
corpus <- Corpus(VectorSource(doc))

# Preprocessing the text (convert to lowercase, remove punctuation, numbers, and stopwords)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a document-term matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms (adjust threshold as necessary)
dtm <- removeSparseTerms(dtm, 0.99)

# Fit LDA model
# Set k = number of topics you want to extract
k <- 5  # Adjust this to the number of topics you wish to extract
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# View topics
topics <- tidy(lda_model, matrix = "beta")

# Display top words for each topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta)

print(top_terms)

# To view the document-topic distribution
doc_topics <- tidy(lda_model, matrix = "gamma")
head(doc_topics)

# Visualize the topics (optional)
library(ggplot2)
ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~topic, scales = "free") +
  labs(title = "Top Terms per Topic", x = "Term", y = "Beta")