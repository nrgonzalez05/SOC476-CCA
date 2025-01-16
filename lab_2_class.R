# Code developed by Oscar Stuhler, Cat Dang Ton
##################################################################
### What we'll cover today
##################################################################

### Packages: tools vs datasets
### Core quanteda objects: corpus, tokens, document-feature matrix
### Preprocessing: stopword removal, stemming & lemmatization
### Dictionaries
### Measuring similarity
### Adding weights to features with tf-idf
### Measuring readability

##################################################################
### Packages for analysis (quanteda, tidytext, SnowballC) and data (sotu, lexicon)
##################################################################

### There are a variety of packages for doing text analysis in R.
### Some are toolkits for analysis.
### The two most general of these are "quanteda" (and quanteda.textstats) and "tidytext". 
### Tidytext is a relatively new addition and well integrated with the tidyverse.
### It was developed by Julia Silge and David Robinson 
### and is used in their "Text mining with R" book.
### If this is your preferred style of coding, you should maybe go for it.

### Here, however, we'll mostly use the "quanteda" package, which is
### developed and maintained by LSE Political Scientist Ken Benoit and his group.

### Some packages only contain data. Here we'll use "sotu" and "lexicon". 

### REMEMBER: Initialize a virtual environment in your research project space.
### If the renv already exists, you can restore it and keep adding packages to it
renv::init()

# refresh cache
rm(list=ls())

### Load packages
library(pacman)
p_load(quanteda, quanteda.textstats, stringr, sotu, ggplot2, lexicon, SnowballC)

##################################################################
### State of the Union corpus
##################################################################

### The State of the Union (SOTU) is an address the President
### annually gives to Congress — since 1790.
### The "sotu" package contains:
### all State of The Union addresses (1790-2020): sotu_text
### and some metadata: sotu_meta

# Glimpse SOTU metadata
head(sotu_meta)

# Combine metadata with text
sotu_df = cbind(sotu_meta, text = sotu_text)
sotu_df$docid = paste0(sotu_df$president, sotu_df$year, sotu_df$X)
# Note this df's structure as an example for your data.
View(sotu_df)

##################################################################
### Corpus objects 
##################################################################

### "Corpus object" = object class that consists of texts and metadata.

### This object is a prerequisite for most quanteda functions.

# A dataframe can be converted into a corpus object, using corpus().
# corpus() requires a df, a text variable and a document ID variable.
# The df could look like this:
#| ID | TEXT | METADATA1 | METADATA2
#| 1234 | text 1 | beef | salt |
#| 9382 | text 2 | pork | chili |
#| 4923| text 3 | chicken | smoked paprika |
#|... |... |... |
corpus <- quanteda::corpus(sotu_df, text_field = "text", docid_field = "docid")
print(corpus)

# Each row/case in this dataframe is referred to as a "document" in a corpus.
# Get document-level variable "year"
docvars(corpus, field = "year")

### Subset a corpus
corpus_sub <- corpus_subset(corpus, year >= 1950 & sotu_type == "speech")

### corpus_reshape() splits the text into "sentences", "paragraphs" or "documents"
corpus_sub_sentences <- corpus_reshape(corpus_sub, to = "sentences")
cat("Number of text units:", ndoc(corpus_sub_sentences))
head(corpus_sub_sentences) # Note the change in document IDs.  

### We can also re-aggregate
corpus_sub <- corpus_reshape(corpus_sub_sentences, to = "documents")
cat("Number of text units:", ndoc(corpus_sub))
print(corpus_sub)

### You can also split the text based on regex patterns. Note that this
### only works if your data are very clean. You might have to build
### something more complicated if they are not.

# TOY EXAMPLE: Split a text by speaking turn to create 1 document per speaking turn
corpus_speeches <- corpus("Mrs. Smith: Question?
                         Mr. Jones: Confused mumbling.
                         Mrs. Smith: Question!
                         Mr. Smith: Evasive answer!?")
speakingturn_pattern <- "[A-Z].+\\s[A-z]+:" 
corpus_speakers <- corpus_segment(corpus_speeches, pattern = speakingturn_pattern, valuetype = "regex")
print(corpus_speakers)

# corpus_segment() didn't retain the speakers' names to help us identify who is speaking. 
# What would you do?

##################################################################
### Tokens objects 
##################################################################

### "Tokens object" = object class consisting of a list of character strings.

### A tokens object can also be made from a corpus.
### We'll keep using the SOTU corpus as an example.
### As you can see, we have a bunch of preprocessing (data cleaning) options here.
tokens <- tokens(corpus_sub, what = "word",
                    remove_punct = TRUE,
                    remove_symbols = FALSE,
                    remove_numbers = FALSE,
                    remove_url = FALSE,
                    remove_separators = TRUE,
                    split_hyphens = FALSE)
head(tokens)
head(tokens(corpus_sub, what = "sentence"))

### Lowercase all tokens
tokens = tokens_tolower(tokens, keep_acronyms = TRUE)
head(tokens)

### Locate keywords in context 
# This function takes a corpus/list of texts/tokens object and a keyword, 
# and returns all occurrences of the keyword, with context.
# Context "window" = number of tokens before and after the keyword.
keyword_regex = "troop*"
kwic(tokens, window = 5, pattern = keyword_regex)[1:6]

### Make ngrams
tokens_ngram <- tokens_ngrams(tokens("Colorless green ideas sleep furiously."), n = 1:3)
print(tokens_ngram)

# See how this would essentially double the token number (what does this imply?)
tokens_bigram <- tokens_ngrams(tokens, n = 1:2)
sum(lengths(bigrams)) / sum(lengths(tokens))


##################################################################
### Document-feature matrix
##################################################################

### The document-feature matrix (dfm) is a staple of the bag-of-words model.

### We can turn a tokens object into a dfm object.
sotu_dfm <- dfm(tokens_nostop)
cat("Dimensions of dfm:",dim(sotu_dfm))
head(sotu_dfm) # What do the numbers mean?

### This is not to be confused with the object class "matrix" in R.
### We can convert a dfm object into a matric object, though.
sotu_dfm_matrix = convert(sotu_dfm, to = "matrix")
View(sotu_dfm_matrix)

### Get count of features (looking at 10 most frequent features)
feature_counts = colSums(sotu_dfm) 
sort(feature_counts, decreasing = T)[1:10]

### Remove features from dfm with dfm_select()
sotu_dfm <- dfm_select(sotu_dfm, pattern = "can", selection = "remove")

##################################################################
### Stopword removal, lemmatization and stemming
##################################################################

### Removing stopwords from a tokens object

# This uses a default list of common English stopwords.
# You can also make your own list of stopwords with dictionaries.
cat("Total tokens:", sum(lengths(tokens)))
print(stopwords("en")) # view default stopwords
tokens_nostop <- tokens_remove(tokens, pattern = stopwords("en")) # remove stopwords
cat("Share of tokens removed:", 1-sum(lengths(tokens_nostop))/sum(lengths(tokens)))

### Now we have a good measure of document lengths in tokens
doc_lengths = lengths(tokens)
ggplot() +
  geom_point(aes(x = docvars(tokens)$year, y = doc_lengths, color = docvars(tokens)$party)) +
  scale_color_manual(values = c("red", "blue"), breaks = c("Republican", "Democratic")) +
  geom_vline(xintercept = seq(1952.5,2016.5,4), color = "grey50") +
  ylab("Number of tokens") +
  xlab("Year") +
  theme_bw()

### Lemmatization simplifies words by converting them to their lemma/base/root form.

# We rely on "lemmatizer" packages/libraries that contain lemmatization lists and/or rules.
# Most are in Python, but there are some English packages in R.

### For this example, we'll use a data table called "hash_lemmas" from the "lexicon" package,
# which lists the base form for each feature. 
View(lexicon::hash_lemmas)

# We can use quanteda's dfm_replace() or tokens_replace() with items from hash_lemmas as the input.

# Toy example
sentence = tokens("Colorless green ideas sleep furiously.")
tokens_replace(sentence, 
               pattern = lexicon::hash_lemmas$token, 
               replacement = lexicon::hash_lemmas$lemma)
# Dfm example
sotu_dfm_lemmatized = dfm_replace(sotu_dfm, 
                                  pattern = lexicon::hash_lemmas$token, 
                                  replacement = lexicon::hash_lemmas$lemma)

### Stemming removes the inflections of words.
### It is an alternative to lemmatization. Don't do both.
quanteda::tokens_wordstem(sentence, language = "en")

### This works for languages included in getStemLanguages()
# You'll need to know the language's code. See: http://www.iso.org/iso/home/standards/language_codes.htm.
getStemLanguages()

# German example
sentence = tokens("Farblose grüne Ideen schlafen wütend.")
tokens_wordstem(sentence) # language can be auto-detected...
tokens_wordstem(sentence, language = "de") # or manually specified.
# You can also use the Porter stemmer implemented in the SnowballC package
SnowballC::wordStem("Farblose grüne Ideen schlafen wütend.", language = "de")

# Let's apply this to our dfm
sotu_dfm_stemmed = dfm_wordstem(sotu_dfm)

### Compare dfm dimensions & sparsity
cat("Dimensions before stemming:", dim(sotu_dfm),
    "\nDimensions after stemming:", dim(sotu_dfm_stemmed))
cat("Sparsity before stemming:", sparsity(sotu_dfm),
    "\nSparsity after stemming:", sparsity(sotu_dfm_stemmed))

### You can see stemming is a bit more of a drastic feature reduction method.
cat("Rate of features reduced through stemming:", 1-ncol(sotu_dfm_stemmed)/ncol(sotu_dfm),
    "\n Rate of features reduced through lemmatization:", 1-ncol(sotu_dfm_lemmatized)/ncol(sotu_dfm))


##################################################################
### Working with dictionaries
##################################################################

### "Dictionary" = list of features/terms/words.
### There are many ways to use dictionaries. 
### One way is to compile features that signify some meaning you want to capture from the text.

### EXAMPLE: Create a dictionary for law & order terms, as a measure of how prevalent this topic is. 
# You see that this is a list of strings in regex form,
# i.e., they have * at the end to match multiple possible features.
# Technically, this is not necessary, because we already lemmatized the features above.
law_and_order_dict = dictionary(list(law_and_order = c("assaults", "bail", "burglar*", "constab*", "convict*", 
                                                       "court", "courts", "custod*", "dealing", "delinquen*", 
                                                       "deter", "deter*", "disorder", "drug*", "fine", "fines", 
                                                       "firmness", "force*", "fraud*", "guard*", "hooligan*", 
                                                       "illegal*", "intimidat*", "joy-ride*", "lawless*", "magistrat*", 
                                                       "offence*", "officer*", "penal*", "police", "policemen", 
                                                       "policing", "prison*", "probation", "prosecution", 
                                                       "punish*", "re-offend", "ruc", "seiz*", "sentence*", 
                                                       "shop-lifting", "squatting", "terror*", "theft*", 
                                                       "thug*", "tough*", "trafficker*", "uniformed", 
                                                       "unlawful", "vandal*", "victim*", "vigilan*")),
           file = NULL,
)
### These terms come from a dictionary created by Laver and Garry. (see https://www.jstor.org/stable/2669268)
### Michael Laver is a political scientist at NYU and was a real pioneer in bringing text analysis to social science.
### The dictionary was developed to measure UK political party positions. 
### It is highly doubtful that it is a good way to measure law and order rhetoric in U.S. politics,
### especially over such a long time period.
### Taking a dictionary that was created in one context to measure something in another context
### is something YOU SHOULD NEVER DO for any kind of serious inquiry. 
### I do this here only for illustration.

### Let's count all conservative-signaling "law and order" words in the speeches.
### dfm_lookup() does this. It requires a dfm, and a dictionary of features/terms to look for inside the dfm.
laworder_counts <- dfm_lookup(sotu_dfm_stemmed, dictionary = law_and_order_dict, levels = 1)
print(laworder_counts) # What do these numbers mean?

### Now we compute the probability that "law and order" terms appear in a given speech
sotu_wordcounts <- rowSums(sotu_dfm_lemmatized)
prob_conservative <- as.numeric(laworder_counts[,"law_and_order"]) / sotu_wordcounts

### Plot results
ggplot() +
  geom_point(aes(x = docvars(sotu_dfm_lemmatized)$year, y = prob_conservative, color = docvars(sotu_dfm_lemmatized)$party)) +
  scale_color_manual(values = c("red", "blue"), breaks = c("Republican", "Democratic")) +
  geom_vline(xintercept = seq(1952.5,2016.5,4), color = "grey50") +
  ylab("Number of tokens") +
  xlab("Year") +
  theme_bw()

##################################################################
### Similarity and distance
##################################################################

### COSINE SIMILARITY: a measure of how similar documents are, regardless of their size.

### First let's make a cosine function to learn how it works. Remember that
### cosine is the dot product of two vectors (i.e. lists of numbers),
### divided by the product of their length.
### That is to say: cos = x * y / |x||y|
calculate_cosine_similarity = function(vec1, vec2) { 
  nominator = sum(vec1 * vec2)
  denominator = sqrt(sum(vec1*vec1))*sqrt(sum(vec2*vec2))
  return(nominator/denominator)
}

### Example 1
x = c(2, 3)
y = c(2, 3)

### What should we get?
calculate_cosine_similarity(x, y)

### Example 2
a = c(2, 3)
b = c(4, 6)

### What should we get?
calculate_cosine_similarity(a, b)

### Example 3
a = c(1, 2, 3)
b = c(-1, -2, -3)

### What should we get?
calculate_cosine_similarity(a, b)

# Still don't get it? Check this out: https://tomhazledine.com/cosine-similarity/

### SCALED-UP EXAMPLE: Let's see how this works on a text corpus.

# We'll compare an unprocessed and a pre-processed dfm.
# Set dfm
dfm_raw <- sotu_dfm
dfm_pre <- sotu_dfm_lemmatized
# View document identifiers in a dfm
dfm_raw@docvars[["docname_"]]

### Calculate cosine similarity
calculate_cosine_similarity(vec1 = dfm_raw[c("Barack Obama2015235"),], 
                            vec2 = dfm_raw[c("Donald Trump2017237"),])

### Let's see what pre-processing does to this
calculate_cosine_similarity(vec1 = dfm_pre[c("Barack Obama2015235"),], 
                            vec2 = dfm_pre[c("Donald Trump2017237"),])

### This reduces the cosines, why?

### In practice, we use textstat_simil() or textstat_dist() from quanteda.textstats
# This function requires a dfm object as input. 
# This object can be a subset of a larger dfm object, as shown below.
textstat_simil(dfm_pre[c("Barack Obama2015235", "Donald Trump2017237"),], margin = "documents", method = "cosine") # Give us a matrix (cosine is a symmetric measure)

### textstat_simil() or textstat_dist() lets us use measures other than cosine.
# Similarity measures: method = c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamann", "simple matching"),
# Distance measures: method = c("euclidean", "manhattan", "maximum", "canberra", "minkowski")
textstat_simil(dfm_pre[c("Barack Obama2015235", "Donald Trump2017237"),], margin = "documents", method = "jaccard") 
textstat_dist(dfm_pre[c("Barack Obama2015235", "Donald Trump2017237"),], margin = "documents", method = "manhattan")

### We can also compare specific documents with textstat_simil)
sim_scores = textstat_simil(x = dfm_pre["Barack Obama2015235",], y = dfm_pre,
                      margin = "documents", method = "cosine")
sim_scores

### Or compare all documents (this will get cumbersome with larger corpora, however)
textstat_simil(x = dfm_pre,
               margin = "documents", method = "cosine")

### PLOT document similarity scores 
# Plotting similarity scores requires a corpus object and a sim_scores object. 
# We'll need to retrace our steps regarding the objects we created. 
# To do this, it helps to search objects' names from bottom up. 
# sim_scores <- dfm_pre <- sotu_dfm_lemmatized <- tokens_nostop <- corpus_sub.
# Readable naming practices (names that tell the reader what something is and what it does) make it easier.

p1 <- ggplot() +
  geom_point(aes(x = docvars(corpus_sub)$year, 
                 y = as.vector(sim_scores), 
                 color = docvars(corpus_sub)$party)) +
  ylab("cosine similarity between SOTUs and Obama 2015")  +
  xlab("")

p1

##################################################################
### Adding weights using term frequency-inverse document frequency (tf-idf)
##################################################################

### Recall what tf-idf is. Let's make a conceptual example.

### First make a function to calculate term frequency (TF)
# TF is the number of times a term appears in a document
calculate_tf <- function(documents, term) {
  tf <- sapply(documents, function(doc) { # for each document,
    terms <- unlist(strsplit(doc, "\\s+")) # split strings into terms, by whitespace,
    target_term_count <- sum(terms == term) # n occurences of target term
    allterms_count <- length(terms) # sum total of all terms in document
    target_term_count / allterms_count # Get TF value
  })
  return(tf)
}

### Then make a function to calculate Inverse Document Frequency (IDF)
# IDF is the natural log of the total size of the corpus
# divided by the number of documents in the corpus that contain the target term.
calculate_idf <- function(documents, term) {
  # Get sum of all documents in corpus/set of documents
  alldocs_count <- length(documents)  
  # Get number of documents containing the target term
  docs_with_term_count <- sum(sapply(documents, function(doc) { # for each document,
    terms <- unlist(strsplit(doc, "\\s+")) # split strings into terms, by whitespace,
    return(any(terms == term)) # return TRUE if the target term exists in the document, otherwise FALSE
  }))
  # Get IDF value, with Laplace smoothing
  idf <- log2(alldocs_count / (1 + docs_with_term_count)) 
  return(idf)
}

### Combine the above functions into a function to calculate TF-IDF
calculate_tfidf <- function(documents, term) {
  tf <- calculate_tf(documents, term)
  cat("term frequencies:", tf, "\n")
  idf <- calculate_idf(documents, term)
  cat("inverse document frequency:", idf, "\n")
  tfidf <- tf * idf
  return(tfidf)
}

### Apply function to example data
doc1 <- "this is a document"
doc2 <- "this is another example document"
documents <- c(doc1, doc2)
term <- "this"
calculate_tfidf(documents, term)
# Why are these values negative?

### DFM EXAMPLE: Let's see how plot p1 changes when we re-weight our SOTU dfm using tf-idf. 
### You can do that using quanteda's dfm_tfidf() function
dfm_pre_tfidf <- dfm_tfidf(dfm_pre, scheme_tf = "prop")
sims = textstat_simil(x = dfm_pre_tfidf["Barack Obama2015235",], y = dfm_pre_tfidf,
                      margin = "documents", method = "cosine")
p2 <- ggplot() +
  geom_point(aes(x = docvars(corpus_sub)$year, 
                 y = as.vector(sims), 
                 color = docvars(corpus_sub)$party)) +
  ylab("Weighted cosine similarity between SOTUs and Obama 2015")  +
  xlab("")

p_load(patchwork)
multilplot <- (p1 + p2) 
multilplot

##################################################################
### Readability
##################################################################

### Let's implement the Flesh-Kincaid readability score.
### Remember this was:
### 206.835 - 1.015(N(words)/N(sentences))-84.6(N(syllables)/N(words))

### We can find two of these elements pretty easily using quanteda
n_words <- tokens(corpus, what = "word") %>% # Number of words
  lengths() 
n_sentences <- tokens(corpus, what = "sentence") %>% # Number of sentences
  lengths() 

### It's not trivial to get at the number of syllables, but
### we can make a reasonable estimate by counting the number of vowels, i.e.:
n_vowels <- str_count(corpus,"[aeiouyAEIOUY]")

### Compute readability
FK_readability = 206.835 - 1.015*(n_words/n_sentences) - 84.6 * (n_vowels/n_words)
print(FK_readability)
### Note that this number might be "off" because 
### vowels are not an ideal measure of syllables. 
### But, assuming the relationship between syllables and vowels 
### is relatively constant across speakers,
### we can still treat this as a measure of readability.

ggplot() +
  geom_point(aes(x = docvars(corpus)$year, 
                 y = FK_readability, 
                 color = docvars(corpus)$party)) +
  ylab("Readability of SOTUs")  +
  xlab("")



