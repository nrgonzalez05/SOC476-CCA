# Load required libraries
install.packages("tm")
library(tm)        # Text Mining
install.packages("SnowballC")
library(SnowballC) # Stemming
install.packages("textstem")
library(textstem)  # Lemmatization
library(tidyverse) # Data manipulation and visualization

# Step 1: Define the text
manifesto <- "
We, as champions of democracy, affirm our unwavering commitment to equality, justice, and representation. At the heart of our vision lies the principle of democratic parity—a system where every voice matters equally, and power is distributed equitably among all people, regardless of gender, race, class, or creed. Democratic parity is more than a policy goal; it is the cornerstone of a society that thrives on fairness and inclusion. We envision a future where institutions reflect the diversity of the populations they serve. To achieve this, we pledge to dismantle systemic barriers that exclude marginalized communities from meaningful participation in governance and decision-making.  

To realize this vision, we commit to several key actions. We will implement electoral reforms, such as proportional representation and ranked-choice voting, to ensure fairer outcomes and amplify underrepresented voices. We will address wealth disparities that skew political power by enacting reforms to curb the influence of money in politics. We will equip citizens with the tools and knowledge to engage fully in civic life through robust public education and media literacy initiatives. Furthermore, we will pursue institutional reforms, including term limits, transparency measures, and anti-corruption frameworks, to restore trust in government. Finally, we will promote democratic values globally, supporting efforts to combat authoritarianism and fostering solidarity across nations.  

We call upon citizens, leaders, and communities to join us in forging a democracy that works for all—a democracy of, by, and for the people. Together, we can build a future defined by parity, equity, and opportunity for every individual.
"

# Step 2: Preprocess the text
# Create a text corpus
corpus <- Corpus(VectorSource(manifesto))

# Clean the text
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%  # Convert to lowercase
  tm_map(removePunctuation) %>%            # Remove punctuation
  tm_map(removeNumbers) %>%                # Remove numbers
  tm_map(removeWords, stopwords("en")) %>% # Remove stop words
  tm_map(stripWhitespace)                  # Remove extra spaces

# Optional: Apply lemmatization
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))

# Step 3: Create a Term-Document Matrix (TDM)
tdm <- TermDocumentMatrix(corpus)

# Convert TDM to a matrix for further analysis
tdm_matrix <- as.matrix(tdm)

# Step 4: Normalize using TF-IDF (Optional)
tdm_tfidf <- weightTfIdf(tdm)
tfidf_matrix <- as.matrix(tdm_tfidf)

# Step 5: View the vectorized representation
print("TDM Matrix:")
print(tdm_matrix)

print("TF-IDF Matrix:")
print(tfidf_matrix)

# Step 6: Visualize most frequent words (Optional)
# Sum frequencies of each term
word_freq <- rowSums(tdm_matrix)
word_freq_df <- data.frame(term = names(word_freq), freq = word_freq)

# Plot the most frequent terms
word_freq_df %>%
  arrange(desc(freq)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Most Frequent Terms", x = "Terms", y = "Frequency") +
  theme_minimal()

## above is ChatGPT Democratic Party Manifesto

## below is ChaptGPT Republican 

# Step 1: Define the text
manifesto_rnc <- "
We, as guardians of liberty, stand resolutely for the principles that make our nation strong: freedom, responsibility, and opportunity. At the core of our vision is the belief that a government’s primary role is to secure the rights of its citizens while fostering an environment where individuals and families can thrive through hard work and ingenuity.  

We are committed to a government that is limited, transparent, and accountable to the people. A government that protects personal freedoms, supports economic growth, and ensures national security. We pledge to reduce the burden of excessive regulation and taxation, empowering small businesses and entrepreneurs to create jobs and innovate.  

We believe in upholding traditional values while embracing the diversity of thought and culture that defines America. Families are the foundation of society, and we support policies that strengthen them, including parental choice in education and protecting the sanctity of life.  

We are champions of free markets, recognizing that competition and innovation drive prosperity. We will protect the right to private property and encourage responsible environmental stewardship, balancing growth with conservation.  

On the world stage, we stand for peace through strength, supporting our allies and deterring those who threaten freedom. We believe in a strong national defense and fair trade agreements that prioritize American interests.  

Together, let us reaffirm our commitment to the timeless principles that have made this nation exceptional. Together, we can ensure a future of freedom, opportunity, and prosperity for all Americans.
"

# Step 2: Preprocess the text
corpus <- Corpus(VectorSource(manifesto))
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Optional: Lemmatization
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))

# Step 3: Create Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

# Optional: TF-IDF
tdm_tfidf <- weightTfIdf(tdm)
tfidf_matrix <- as.matrix(tdm_tfidf)

# Step 4: View Vectorized Output
print("TDM Matrix:")
print(tdm_matrix)

print("TF-IDF Matrix:")
print(tfidf_matrix)

# Step 5: Visualize Most Frequent Words
word_freq <- rowSums(tdm_matrix)
word_freq_df <- data.frame(term = names(word_freq), freq = word_freq)

word_freq_df %>%
  arrange(desc(freq)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Most Frequent Terms", x = "Terms", y = "Frequency") +
  theme_minimal()

## getting cosine sim

# Load necessary library

install.packages("lsa")
library(lsa)  # For cosine similarity

# Step 1: Define the two manifestos
democratic_manifesto <- "We, as champions of democracy, affirm our unwavering commitment to equality, justice, and representation. At the heart of our vision lies the principle of democratic parity—a system where every voice matters equally, and power is distributed equitably among all people, regardless of gender, race, class, or creed. Democratic parity is more than a policy goal; it is the cornerstone of a society that thrives on fairness and inclusion. We envision a future where institutions reflect the diversity of the populations they serve. To achieve this, we pledge to dismantle systemic barriers that exclude marginalized communities from meaningful participation in governance and decision-making.  

To realize this vision, we commit to several key actions. We will implement electoral reforms, such as proportional representation and ranked-choice voting, to ensure fairer outcomes and amplify underrepresented voices. We will address wealth disparities that skew political power by enacting reforms to curb the influence of money in politics. We will equip citizens with the tools and knowledge to engage fully in civic life through robust public education and media literacy initiatives. Furthermore, we will pursue institutional reforms, including term limits, transparency measures, and anti-corruption frameworks, to restore trust in government. Finally, we will promote democratic values globally, supporting efforts to combat authoritarianism and fostering solidarity across nations.  

We call upon citizens, leaders, and communities to join us in forging a democracy that works for all—a democracy of, by, and for the people. Together, we can build a future defined by parity, equity, and opportunity for every individual.
"

republican_manifesto <- "We, as guardians of liberty, stand resolutely for the principles that make our nation strong: freedom, responsibility, and opportunity. At the core of our vision is the belief that a government’s primary role is to secure the rights of its citizens while fostering an environment where individuals and families can thrive through hard work and ingenuity.  

We are committed to a government that is limited, transparent, and accountable to the people. A government that protects personal freedoms, supports economic growth, and ensures national security. We pledge to reduce the burden of excessive regulation and taxation, empowering small businesses and entrepreneurs to create jobs and innovate.  

We believe in upholding traditional values while embracing the diversity of thought and culture that defines America. Families are the foundation of society, and we support policies that strengthen them, including parental choice in education and protecting the sanctity of life.  

We are champions of free markets, recognizing that competition and innovation drive prosperity. We will protect the right to private property and encourage responsible environmental stewardship, balancing growth with conservation.  

On the world stage, we stand for peace through strength, supporting our allies and deterring those who threaten freedom. We believe in a strong national defense and fair trade agreements that prioritize American interests.  

Together, let us reaffirm our commitment to the timeless principles that have made this nation exceptional. Together, we can ensure a future of freedom, opportunity, and prosperity for all Americans.
"

# Step 2: Create a combined corpus
library(tm)
combined_corpus <- Corpus(VectorSource(c(democratic_manifesto, republican_manifesto)))

# Step 3: Preprocess the text
combined_corpus <- combined_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Step 4: Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(combined_corpus)
dtm_matrix <- as.matrix(dtm)

# Step 5: Compute the Cosine Similarity
cosine_sim <- cosine(dtm_matrix[1, ], dtm_matrix[2, ])

# Step 6: Print the Result
cat("Cosine Similarity between the Democratic and Republican manifestos:", round(cosine_sim, 4), "\n")


# Load necessary libraries
library(lsa)  # For cosine similarity
library(ggplot2)  # For plotting

# Step 1: Define the two manifestos
democratic_manifesto <- "We, as champions of democracy, affirm our unwavering commitment to equality, justice, and representation. At the heart of our vision lies the principle of democratic parity—a system where every voice matters equally, and power is distributed equitably among all people, regardless of gender, race, class, or creed. Democratic parity is more than a policy goal; it is the cornerstone of a society that thrives on fairness and inclusion. We envision a future where institutions reflect the diversity of the populations they serve. To achieve this, we pledge to dismantle systemic barriers that exclude marginalized communities from meaningful participation in governance and decision-making.  

To realize this vision, we commit to several key actions. We will implement electoral reforms, such as proportional representation and ranked-choice voting, to ensure fairer outcomes and amplify underrepresented voices. We will address wealth disparities that skew political power by enacting reforms to curb the influence of money in politics. We will equip citizens with the tools and knowledge to engage fully in civic life through robust public education and media literacy initiatives. Furthermore, we will pursue institutional reforms, including term limits, transparency measures, and anti-corruption frameworks, to restore trust in government. Finally, we will promote democratic values globally, supporting efforts to combat authoritarianism and fostering solidarity across nations.  

We call upon citizens, leaders, and communities to join us in forging a democracy that works for all—a democracy of, by, and for the people. Together, we can build a future defined by parity, equity, and opportunity for every individual.
"

republican_manifesto <- "We, as guardians of liberty, stand resolutely for the principles that make our nation strong: freedom, responsibility, and opportunity. At the core of our vision is the belief that a government’s primary role is to secure the rights of its citizens while fostering an environment where individuals and families can thrive through hard work and ingenuity.  

We are committed to a government that is limited, transparent, and accountable to the people. A government that protects personal freedoms, supports economic growth, and ensures national security. We pledge to reduce the burden of excessive regulation and taxation, empowering small businesses and entrepreneurs to create jobs and innovate.  

We believe in upholding traditional values while embracing the diversity of thought and culture that defines America. Families are the foundation of society, and we support policies that strengthen them, including parental choice in education and protecting the sanctity of life.  

We are champions of free markets, recognizing that competition and innovation drive prosperity. We will protect the right to private property and encourage responsible environmental stewardship, balancing growth with conservation.  

On the world stage, we stand for peace through strength, supporting our allies and deterring those who threaten freedom. We believe in a strong national defense and fair trade agreements that prioritize American interests.  

Together, let us reaffirm our commitment to the timeless principles that have made this nation exceptional. Together, we can ensure a future of freedom, opportunity, and prosperity for all Americans.
"


# Step 2: Create a combined corpus
library(tm)
combined_corpus <- Corpus(VectorSource(c(democratic_manifesto, republican_manifesto)))

# Step 3: Preprocess the text
combined_corpus <- combined_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Step 4: Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(combined_corpus)
dtm_matrix <- as.matrix(dtm)

# Step 5: Compute the Cosine Similarity
cosine_sim <- cosine(dtm_matrix[1, ], dtm_matrix[2, ])

# Step 6: Prepare data for plotting
similarity_data <- data.frame(
  Comparison = c("Democratic vs Republican"),
  Similarity = c(cosine_sim)
)

# Step 7: Plot the cosine similarity
ggplot(similarity_data, aes(x = Comparison, y = Similarity, fill = Comparison)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Similarity, 4)), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Cosine Similarity Between Democratic and Republican Manifestos",
    x = "",
    y = "Cosine Similarity"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## 3 parties
# Load necessary libraries

library(tm)
library(SnowballC)
install.packages("text2vec")
library(text2vec)

# Sample manifestos
democratic_manifesto <- "We believe in a fair economy where all people, regardless of their background, have equal access to opportunities. We will increase healthcare funding and take action on climate change, protecting the planet for future generations. Our policy aims to strengthen social welfare systems and invest in infrastructure to create jobs."
republican_manifesto <- "Our priority is to protect individual freedoms and economic prosperity. We will lower taxes and reduce government intervention. National security and a strong military are paramount to ensure the safety of our country. We will promote traditional family values and defend the right to life."
green_party_manifesto <- "We stand for environmental justice and sustainable development. We will address climate change with bold action. Our focus is on transitioning to renewable energy, protecting biodiversity, and reducing carbon emissions. We believe in social equality, economic fairness, and a green economy that works for all."

# Combine all manifestos into a list
manifestos <- c(democratic_manifesto, republican_manifesto, green_party_manifesto)

# Preprocess the text data (convert to lowercase, remove stopwords, punctuation, etc.)
corpus <- Corpus(VectorSource(manifestos))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces

# Create a document-term matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Compute the cosine similarity matrix
similarity_matrix <- sim2(dtm_matrix, method = "cosine", norm = "l2")

# Create a data frame for better visualization
similarity_df <- as.data.frame(similarity_matrix)
colnames(similarity_df) <- c("Democratic", "Republican", "Green Party")
rownames(similarity_df) <- c("Democratic", "Republican", "Green Party")

# Display the similarity matrix
print(similarity_df)

# Load necessary libraries
library(tidyverse)
library(tm)
library(SnowballC)
library(text2vec)

# Sample manifestos
democratic_manifesto <- "We believe in a fair economy where all people, regardless of their background, have equal access to opportunities. We will increase healthcare funding and take action on climate change, protecting the planet for future generations. Our policy aims to strengthen social welfare systems and invest in infrastructure to create jobs."
republican_manifesto <- "Our priority is to protect individual freedoms and economic prosperity. We will lower taxes and reduce government intervention. National security and a strong military are paramount to ensure the safety of our country. We will promote traditional family values and defend the right to life."
green_party_manifesto <- "We stand for environmental justice and sustainable development. We will address climate change with bold action. Our focus is on transitioning to renewable energy, protecting biodiversity, and reducing carbon emissions. We believe in social equality, economic fairness, and a green economy that works for all."

# Combine all manifestos into a list
manifestos <- c(democratic_manifesto, republican_manifesto, green_party_manifesto)

# Preprocess the text data
corpus <- Corpus(VectorSource(manifestos))
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert text to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespaces

# Create a document-term matrix (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to a matrix
dtm_matrix <- as.matrix(dtm)

# Compute the cosine similarity matrix
similarity_matrix <- sim2(dtm_matrix, method = "cosine", norm = "l2")

# Convert the matrix into a long format
similarity_long <- as.data.frame(as.table(similarity_matrix))

# Rename columns for clarity
colnames(similarity_long) <- c("Party1", "Party2", "Similarity")

# Filter out the self-similarity (diagonal) to avoid repeating comparisons
similarity_long <- similarity_long %>%
  filter(Party1 != Party2)

# Convert Party1 and Party2 to factors with explicit levels to ensure correct order
similarity_long$Party1 <- factor(similarity_long$Party1, levels = c("1", "2", "3"))
similarity_long$Party2 <- factor(similarity_long$Party2, levels = c("1", "2", "3"))

# Assign readable party names
party_names <- c("Democratic", "Republican", "Green Party")
levels(similarity_long$Party1) <- party_names
levels(similarity_long$Party2) <- party_names

# Plot the heatmap
ggplot(similarity_long, aes(x = Party1, y = Party2, fill = Similarity)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Cosine Similarity between Political Party Manifestos",
       x = "Party", y = "Party", fill = "Similarity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1)) +  # Ensure y-axis labels are readable
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))  # Adjust font size

