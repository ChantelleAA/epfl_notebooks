---
title: "R Notebook"
output: html_notebook
---

# Text Compression

import libraries

```{r}
library(readtext)
library(stringr)
```

1. Preview data

```{r}
preview <- function(filename){
  text_df <- readtext(filename)
  text <- text_df$text
  data_type <- class(text)
  num_of_chars <- str_length(text)
  size_of_file <- file.size(filename)
  print(paste("The data type of text is: ", data_type))
  print(paste("The number of chars in text is: ", num_of_chars))
  print(paste("The file size is: ", size_of_file, "bytes."))
  return(text)
}

text1 <- preview("sample_text.txt")
```

2.  Separate text string `text1` into a vector of words.

```{r}
separate_words <- function(text){
    words <- str_split(text, boundary("word"))
    return(words)
    }
word_vector <- separate_words(text1)
```

3.  Generate a table of words and their frequency

```{r}
word_freq_table <- table(word_vector)
```

4.  Filter out significantly long and frequently occurring words.

```{r}

filter_significant_words <- function(word_freq_table){
    significant_words <- c()
    max_word_length <- 4
    max_word_freq <- 3
    
    for (i in 1:length(word_freq_table)){
        word <- word_freq_table[i]
        word_name <- names(word)
        freq <- word
        if (freq >= max_word_freq & str_length(word_name) > max_word_length){
          significant_words <-  append(significant_words, word)
        }
    }
    return(significant_words)
}

filtered_words <- filter_significant_words(word_freq_table)
```


5. Generate keys for filtered words

```{r}

generate_keys <- function(filtered_words){
    word_key_df <- data.frame(word=as.character(1:length(filtered_words)-1), key = as.character(1:length(filtered_words)-1))
    words <- c()
    keys <- c()
    for (i in 1:length(filtered_words)){
        word <- filtered_words[i]
        word_name <- names(word)
        key <- paste0("@", i)
        words <- append(words, word_name)
        keys <- append(keys, key)
    }
    word_key_df$word <- words
    word_key_df$key <- keys
    return(word_key_df)
}

word_keys <- generate_keys(filtered_words)
```

6. Replace words with keys

```{r}
replace_words <- function(text1, word_keys){
    for (i in 1:length(word_keys$word)){
      text_compressed <- str_replace_all(text1, word_keys$word[i], word_keys$key[i])
    }
    text_compressed <- paste(text_compressed, as.character(word_keys))
    return(text_compressed)
}
```

7. Save into a new txt file

```{r}
save_file <- function(compressed, filename="compressed_sample_text.txt"){
  cat(compressed, file=filename)
}

```

8. Size of new file.

```{r}
num_of_chars <- str_length(text_compressed)
size_of_file <- file.size("compressed_sample_text.txt")
print(num_of_chars)
print(size_of_file)
```
# Image compression

```{r}

```



