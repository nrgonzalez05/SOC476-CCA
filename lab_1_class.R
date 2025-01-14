
##################################################################
### What we'll cover today
##################################################################

### Package management best practices (packages: renv, pacman)
### HTML files
### Basic web scraping: static & dynamic methods (packages: rvest,RSelenium)
### Text search/extraction using regular expressions (package: stringr)

##################################################################
### Package management best practices
##################################################################

### Each research project should have its own "virtual environment",
# to prevent version conflicts as software gets updated over time,
# to avoid breaking a project while installing packages for another project,
# and to easily transport your project to another computer.
### Virtual environment = an isolated, local directory that stores a copy of all packages/libraries 
# with the particular versions used in your project.

# Install the package "renv" to create and manage a virtual R environment.
install.packages("renv")
library(renv)

# Get working directory (the virtual environment directory will be stored wherever the working directory is)
getwd()

# Initialize a new renv 
renv::init()

### To flexibly load existing packages and install new ones if they aren't already installed,
# use pacman's p_load() instead of install.packages() and library(). 

# First, install pacman
install.packages("pacman")
library(pacman)

# Then load/install packages with p_load()
p_load(tidyverse, dplyr, rvest, RSelenium, stringr)

##################################################################
### HTML files
##################################################################

### An HTML file is structured like this:
mini_html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

### You can extract elements from HTML files with the command html_elements().
### HTML elements can be selected thanks to these types of identifiers: "CSS selector" or "XPath".
### On your web browser, find a way to "inspect" or "pick/select" an element,
browseURL("https://sociology.northwestern.edu/people/graduate-students/")

### Then copy your selected element's identifier into html_elements()
# with css selector:
html_elements(mini_html, css = "body")
# with xpath:
html_elements(mini_html, xpath = "")

##################################################################
### Static scraping
##################################################################

### SIMPLE EXAMPLE: Say we are interested in feminist fiction, and we wanna create a list of feminist fiction works.
browseURL("https://www.goodreads.com/list/show/46.Best_Feminist_Fiction")

### We can scrape full websites using read_html().
###  We retrieve the HTML like this.
fem_fic_page <- rvest::read_html("https://www.goodreads.com/list/show/46.Best_Feminist_Fiction")
# Note: An element's contents can only be scraped if visible in an .html snapshot of the webpage. 
# Dynamic elements cannot be scraped with this method.
# Example: https://www.gofundme.com/discover/volunteer-fundraiser 

# see what the HTML looks like:
print(as.character(fem_fic_page))
### Looks pretty messy! To get the titles and author names, 
# we need to first locate the right CSS selector using html_elements(), then grab the text associated with it using html_text().
fem_fic_title <- rvest::html_elements(fem_fic_page, css = ".bookTitle span") %>% html_text()
fem_fic_auth <- rvest::html_elements(fem_fic_page, css = ".authorName span")  %>% html_text()

# Let's turn the above into a function to scrape titles and authors from a given Goodreads page URL
scrape_titles_authors <- function(goodreads_url) {
  page <- read_html(goodreads_url)
  titles <- html_elements(page, css = ".bookTitle span") %>% html_text()
  glimpse(titles) # glimpse results to see if the function works
  authors <- html_elements(page, css = ".authorName span") %>% html_text()
  glimpse(authors)
  return(list(titles = titles, authors = authors))
}

# test the function on two goodreads pages
url1 <- "https://www.goodreads.com/list/show/46.Best_Feminist_Fiction"
url2 <- "https://www.goodreads.com/list/show/46.Best_Feminist_Fiction?page=2"
page1_data <- scrape_titles_authors(url1)
page2_data <- scrape_titles_authors(url2)

### SCALED-UP EXAMPLE: 
### It's tiring to go through pages 1-15 manually. But the page URLs looks pretty uniform.
### This means we can build a loop to go through each page automatically!
for(i in 2:15){
  
  cat("\n Scraping page:", i) # Print a message to indicate which page the loop is at
  
  # Build URL for each page, matching the page number to the basic URL.
  url <- paste0("https://www.goodreads.com/list/show/46.Best_Feminist_Fiction?page=", i)
  
  # Make an empty list to store the scraped output (in this case, titles and authors of feminist fiction)
  fem_fic_titles <- list()
  fem_fic_authors<-list()
  
  # Get elements and convert to text with the function we made above
  page_results <- scrape_titles_authors(url)
  result_titles <- page_results$titles
  result_authors <- page_results$authors
  
  # Append results to the list of titles and authors what we have collected from the page
  fem_fic_titles <- append(fem_fic_titles, result_titles)
  fem_fic_authors <- append(fem_fic_authors, result_authors)
  
  # Pause script for 5 seconds to not overload server (aka avoid bot detection)
  Sys.sleep(5)
  #Sys.sleep(5, sample(1:5, 1)) # Simulate "human" behavior (useful for some websites)
  stop("Hey! Didn't I tell you not to run this loop!?")
}

### Make a dataframe
fem_fic_df <- data.frame("Title" = fem_fic_titles, "Author" =  fem_fic_authors)
head(fem_fic_df)


##################################################################
### Dynamic scraping
##################################################################

### For dynamic scraping, we'll simulate active user interactions with a website to access/scrape the elements we want.

### We'll use the RSelenium package. Depending on your setup
### you'll also need a couple of other things to make this work.
### It can be quite a headache to get this to run so if you don't have any
### immediate scraping projects for which you need this and you're not super curious,
### you don't have to run this.

### We will use Docker, which allows you to run Firefox (or Chrome, if you prefer) in a "container."
### You can download docker here:
### https://www.docker.com
### You will also need to install VNC Viewer: https://www.realvnc.com/en/connect/download/viewer/ 
### You may also need Java, which you can get here:
### https://www.java.com/en/
### Running Docker and connecting it to R can be tricky.
### You can find some good turorials here:
### https://cran.r-project.org/web/packages/RSelenium/vignettes/docker.html
### https://medium.com/@fabiokeller/how-to-make-rselenium-work-on-macos-monterey-with-docker-7fde9c9de5ae

### Open Docker from the terminal. The system() command is from base R and
### it allows you to run things in the terminal from within R.
system("open -a Docker")
system("docker stop $(docker ps -q)") # Reset Docker by killing all running images to free ports

### We can download a recent firefox "image" via this code
### You can think of an image as a lightweight version of a browser
#system("sudo docker pull selenium/standalone-firefox-debug")

### Now we create a "container" in Docker (you can also do this using the application).
### This means that we start an instance of an image. It's basically like opening your browser.
### When we do that, we will have to pick "ports." We make two ports for this. 
### Ports are endpoints through which data is communicated between Docker and the environment.
system("docker run -d -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug")

### Now we access the container.
###  Enter the following address into the field "Enter a VNC Server address". "127.0.0.1:5901"
###  As a password, you can enter "secret" 
system('open -a "VNC Viewer"')

### Now we can connect the remoteDriver to the port. The remote driver is the object in R
### with which we'll be driving the browser.
remDr <- remoteDriver(port = 4445L)

### Open the browser (check the VNC viewer and see if the browser opens)
remDr$open(silent = TRUE)

### Navigate to pages
remDr$navigate("http://www.nyt.com")
remDr$navigate("https://sociology.northwestern.edu")

### You can use normal commands to navigate the web.
remDr$goBack()
remDr$goForward()

### SIMPLE EXAMPLE: Let's say we wanted to collect the wikipedia articles 
###  for all the authors on the feminist fiction list above (fem_fic_df).
# Navigate to wikipedia
remDr$navigate("https://www.wikipedia.org/")

### We need to enter something into the search field.
### So we need to identify the search field (by its CSS selector/xpath),
### Then we can lock in on this element with findElement() 
input_field <- remDr$findElement(using = "css selector", "#searchInput")

### With the element locked in, use sendKeysToElement() 
# to input some text (a search query in this case) and hit ENTER
input_field$sendKeysToElement(list("Ursula K. Le Guin", key = "enter"))

### Get current URL and extract the text with the appropriate CSS selector ("p")
url <- remDr$getCurrentUrl()[[1]]
author_page <- rvest::read_html(url)
author_wiki <- rvest::html_elements(author_page, css = "p")  %>% html_text()
print(author_wiki)

### SCALED-UP EXAMPLE: Let's systematically fill our feminist fiction dataframe with wikipedia articles.
### First make a dataframe with all the authors.
author_df <- data.frame(Author = unique(fem_fic_df$Author), Wiki = NA)

### Then write a loop, where for each unique author, we get their wikipedia page.
remDr$navigate("https://www.wikipedia.org/")
for(i in 1:nrow(author_df)){
  cat(fem_fic_df$Author[i], "\n")
  
  # Find the input field on the current page
  input_field <- remDr$findElement(using = "css selector", "#searchInput")
  
  # Send the respective name to the field and hit enter in order to navigate 
  # to the respective page.
  input_field$sendKeysToElement(list(author_df$Author[i], key = "enter"))
  
  # Then we grab the respective current URL.
  url <- remDr$getCurrentUrl()[[1]]
  
  # Read the full HTML of the page
  author_page <- rvest::read_html(url)
  
  # Extract the text
  author_wiki <- rvest::html_elements(author_page, css = "p")  %>% html_text()
  
  # And store it in our dataframe.
  author_df$Wiki[i] = paste(author_wiki, collapse = " ")
  
  # But we wait a little to not get into trouble.
  Sys.sleep(7)
}

### Inspect dataframe
View(author_df)

### Other important commands ###

### Find and click a button:
edit_hist_button <- remDr$findElement(using = 'css selector', "#ca-history span")
edit_hist_button$clickElement()
### Find multiple elements:
edit_dates <- remDr$findElements(using = 'css selector', ".mw-changeslist-date")
### Get text from a specific element (doesn't work on multiple elements):
eds <- c()
for(ed in edit_dates){
  ed <- ed$getElementText()
  eds <- c(eds, ed)
}
print(unlist(eds))

### End the session. This may seem redundant, but if you forget it, 
### you'll end up blocking your ports.
remDr$quit()

##################################################################
### Stringr
##################################################################
### Working with text requires dealing with character strings
# and patterns of strings, known as "regular expressions".

### You can work with regular expressions in either base R or with the stringr package.
### stringr is more intuitive to use.

### You can define an object as a character string in R using either quotation marks or apostrophes.
### Text that contain quotes or apostrophes can cause issues.
### You can escape a quote or an apostrophe within a string by prefacing it with a backslash (\).
### Alternatively, you can use double quotes to define a string containing single quotes and vice versa.
single <- "How much is the fish?"
double <- 'Sally: "How much is the fish? Bobby\'s mom loves fish."'
cat(single,"\n",  double)

### Manipulate case
str_to_lower(single) # lowercase
str_to_upper(single) # uppercase
str_to_title(single) # title case

### Concatenate strings
str_c(single, double)

### Count character length 
str_length(single)

### Remove excess whitespace
str_trim(" How much is the fish?        ")


##################################################################
### Regular expressions
##################################################################

### Regular expressions, also known as regex, are sequences of characters that define a search pattern. 
### They are usually used to find specific strings or patterns of text within a larger body of text. 
### Once you define the pattern you want to use, you can perform operations like:
### editing, deleting certain characters or words,
### substituting one thing for another, and extracting relevant information.
### This will be very useful if you have messy text data and you need to clean them,
### like, say, transcripts form a conversation where you need to identify speaking turns.

# Start by defining a regex pattern of interest. This should be a string object.
regex_pattern = "fish"

### Extract a string that matches the pattern you defined
str_extract(string = "How much is the fish?", pattern = regex_pattern)

### Detect a string
str_detect(string = "How much is the fish?", pattern = regex_pattern)

### Remove a string
str_remove(string = "How much is the fish?", pattern = regex_pattern)

### Extract all instances of a pattern
str_extract_all(string = "How much is the fish? What fish?", pattern = regex_pattern)

### Replace a pattern with some other pattern (similar to "find and replace" in Excel)
replacement_pattern = "crab"
str_replace(string = "How much is the fish? What fish?", pattern = regex_pattern, replacement = replacement_pattern)
str_replace_all(string = "How much is the fish? What fish?", pattern = regex_pattern, replacement = replacement_pattern)

### Now this is all fine and well, but the real magic of regex lies in using special characters.
### These characters allow us to specify relatively complex searches.
### The important special characters are these: . \ | ( ) { } [ ] ^ $ - * + ?

### E.g., using simple OR condition and wrapping the OR options in brackets
str_extract(string = "How much is the fish?", pattern = "(zebra|dog)") # "zebra OR dog"
str_extract(string = "How much is the fish?", pattern = "(fish|dog)")

### Square brackets let you specify different letters
str_detect(string = "summarize", pattern = "[s]")
str_detect(string = "summarize", pattern = "[sz]")

### The point is a wildcard
str_detect(string = "summarize", pattern = "summari.e")

### If you want to match a special character literally, you have to escape it 
### via \\ or wrap it in brackets:
str_extract(string = "The fish is $1.99", pattern = ".") # matches T (the first letter in str string since it's a wildcard)
str_extract(string = "The fish is $1.99", pattern = "[.]") # Matches the literal period
str_extract(string = "The fish is $1.99", pattern = "\\.")

### You can match patterns by their character classes. Important ones:
### [:digit:] - digits, or [0-9]
### [:alpha:] - letters, or [A-z]
### [:lower:] - lowercase letters, or [a-z]
### [:upper:] - uppercase letters, or [A-Z]
### [:alnum:] - letters and numbers
### [:punct:] - punctuation
### \s - whitespace character
### \w - word character (letters, digits and underscores)
### \n - newline character (paragraph markers)
str_extract_all(string = "The fish is $1.99", pattern = "[:digit:]") # extracts all digits
str_extract_all(string = "The fish is $1.99", pattern = "([:digit:]|[:punct:])") # extracts all digits or punctuation

### There are also "quantifiers" for matches. They help specify character length.
###  ? – zero or one
###  * – zero or more
###  + – one or more
###  {n} – exactly n
###  {n,} – n or more
###  {n,m} – between n and m
str_extract(string = "The fish is $1.99", pattern = "[:digit:]+[.][:digit:]{2}")
str_extract_all(string = "performed at, performed on Tuesday at, performed with Lady Gaga at", pattern = "performed(?:\\s+\\w+){0,3}\\s+at")

### Quantifiers are by default "greedy" (i.e. trying to match as much of the input text as possible).
str_extract("find the fish in the dish", pattern = "f.*h")
### You can make them "lazy" (i.e. trying to match as little of the input text as possible)
str_extract(text, pattern = "f.*?h")

### Specify "lookaheads" and "lookbehinds" in either positive or negative form:
### Positive lookahead: (?=)
### Negative lookahead: (?!)
### Positive lookbehind: (?<=)
### Negative lookbehind: (?<!)

### EXAMPLE: get everything before the letter c:
str_extract("abcdefg", ".+(?=c)")
### Get everything after c
str_extract("abcdefg", "(?<=c).+")
### Get b unless followed by c
str_extract("abcdefg", "b(?!c)")
str_extract("abdefg", "b(?!c)")
### Get d unless preceded by c
str_extract("abcdefg", "(?<!c)d")
str_extract("abdefg", "(?<!c)d")

### You can make these patterns complicated by nesting them
str_extract("The fish is $1.99", "\\$(.*)") # extract everything (wildcards of 0 or more characters) that comes after the dollar symbol
str_extract("The fish is $1.99", "(?<=\\$).+") # see a difference?

## NOTE ON AI USAGE: AI tools (Github Copilot, ChatGPT) can help you interpret/generate regex patterns, 
## BUT it's still your job to make sure the patterns capture what you aim to capture. 
## (PLEASE do not paste your data directly into ChatGPT or any other AI chat logs)

### REGEX CHEATSHEET: linked below. It has some more elements than what we covered,
### but we got the most important ones.
browseURL("https://github.com/rstudio/cheatsheets/blob/main/strings.pdf")

###### EXERCISES ######
### Extract all dates from the following string
t = "John's birthday is on 12/05/1992, and Jane's is on 03/08/1987. We'll have a party on 15/07/2023."
### Correct output: c(12/05/1992, 03/08/1987, 15/07/2023)

### Extract all item codes and quantities from the following string
t = "Order List: [A12B-999 x10], [C34D-567 x4], [e78F-123 x15]. Backorder: [G90H-222 x7], [J11K-333 x2]."
### Expected output: c("A12B-999", "C34D-567", "E78F-123", "G90H-222", "J11K-333")
###                  c("10", "4", "15", "7", "2")

### Extract all emails from the following string
t = "Please contact us at support@example.com or sales@my-company.org for further information. You can reach me directly at john_doe@my-company.com."
### Expected output: c("support@example.com", "sales@my-company.org", "john_doe@my-company.com")



