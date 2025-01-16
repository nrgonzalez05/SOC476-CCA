## Testing MLM 

# Install required packages if you haven't already
install.packages("tm")
install.packages("e1071")
install.packages("text2vec")
install.packages("slam")

# Load libraries
library(tm)
library(e1071)
library(text2vec)
library(slam)

# Sample manifestos
democratic_manifesto <- "We believe in a fair economy where all people, regardless of their background, have equal access to opportunities. We will increase healthcare funding and take action on climate change, protecting the planet for future generations. Our policy aims to strengthen social welfare systems and invest in infrastructure to create jobs."
republican_manifesto <- "Our priority is to protect individual freedoms and economic prosperity. We will lower taxes and reduce government intervention. National security and a strong military are paramount to ensure the safety of our country. We will promote traditional family values and defend the right to life."
green_party_manifesto <- "We stand for environmental justice and sustainable development. We will address climate change with bold action. Our focus is on transitioning to renewable energy, protecting biodiversity, and reducing carbon emissions. We believe in social equality, economic fairness, and a green economy that works for all."

# Combine all manifestos into a list
texts <- c(democratic_manifesto, republican_manifesto, green_party_manifesto)

# Preprocess the text: remove punctuation, convert to lowercase, remove stopwords, etc.
corpus <- Corpus(VectorSource(texts))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove common stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra spaces