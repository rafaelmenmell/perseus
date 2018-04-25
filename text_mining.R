#Text Mining with R
#https://www.tidytextmining.com/tidytext.html
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(igraph)

  con <- file("http://www.gutenberg.org/cache/epub/16452/pg16452.txt",open="r")
  lines <- readLines(con)
  book.start <- vector("integer",24)
  for (book in 1:24){
    search <- sprintf("BOOK %s\\.",as.roman(book))
    book.start[book] <- last(which(grepl(search,lines)==TRUE)) #porque la primera vez que aparece es el índice
  }
  final <- '                  \\*       \\*       \\*       \\*       \\*'
  libros <- vector("list",24)
  for (l in 1:24){
    if (l!=24){
      libros[[l]] <- lines[(lines.split[l]+1):(lines.split[(l+1)]-1)]
    } else {
      libros[[l]] <- lines[(lines.split[l]+1):length(lines)]
    }
    #hay final de linea
    if (TRUE %in% grepl(final,libros[[l]])){
      libros[[l]] <- libros[[l]][1:(which(grepl(final,libros[[l]])==TRUE)-1)]
    }
    #si hay ARGUMENT
    if (TRUE %in% grepl("ARGUMENT",libros[[l]])){
      libros[[l]] <- libros[[l]][1:(which(grepl("ARGUMENT",libros[[l]])==TRUE)-1)]
    }
  }
  #Vamos a limpiar todos los libros
  #Borro líneas vacías
  libros <- lapply(libros,FUN = function(x) x[x!=""])
  #Borro números
  libros <- lapply(libros,FUN = function(x) gsub('[0-9]+', '', x))
  #Borro lineas sueltas
  libros <- lapply(libros,FUN = function(x) x[!(grepl("THE ILIAD.",x))])
  libros <- lapply(libros,FUN = function(x) x[!(grepl("BOOK",x))])
  #Borro los corchetes de las notas
  libros <- lapply(libros,FUN = function(x) gsub('\\[|\\]+', '', x))
  #Borro cuando hay más de un espacio
  libros <- lapply(libros,FUN = function(x) gsub("\\s\\s+","",x))
  
  libros.df <- lapply(libros,FUN=function(x) data.frame(line=1:length(x),text=x))
  libros.df <- lapply(libros.df,FUN=function(x) x %>% unnest_tokens(word,text))
  for (i in 1:length(libros.df)){
    libros.df[[i]]$book <- i
  }
  libros.df <- bind_rows(libros.df)
  
  #frecuencia
  freq <- libros.df %>% anti_join(stop_words) %>% group_by(book) %>% count(word,sort=TRUE) %>% group_by(book)
  freq <- split(freq,freq$book)  
  #sentimientos cada 25 lineas
  sentiments <- libros.df %>% inner_join(get_sentiments("bing")) %>% count(book,index = line %/% 25, sentiment) %>% spread(sentiment, n, fill = 0) %>%  mutate(sentiment = positive - negative)

  ggplot(sentiments, aes(index, sentiment, fill = as.factor(book))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 6, scales = "free_x")
  
  #comparamos todos los metodos para todo el libro (sin dividir)
  libros.df$line2 <- 1:nrow(libros.df)
  afinn <- libros.df %>% 
    inner_join(get_sentiments("afinn")) %>% 
    group_by(index = line2 %/% 250) %>% 
    summarise(sentiment = sum(score)) %>% 
    mutate(method = "AFINN")
  
  bing_and_nrc <- bind_rows(libros.df %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            libros.df %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           dplyr::filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
    count(method, index = line2 %/% 250, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  
  bind_rows(afinn, 
            bing_and_nrc) %>%
    ggplot(aes(index, sentiment, fill = method)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~method, ncol = 1, scales = "free_y")
  
  #most common + - words
  
  bing_word_counts <- libros.df %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

  #term frequency
  
  book_words <- libros.df %>%
    count(book, word, sort = TRUE) %>%
    ungroup()
  
  total_words <- book_words %>% 
    group_by(book) %>% 
    summarize(total = sum(n))
  
  book_words <- left_join(book_words, total_words)
  
  ggplot(book_words, aes(n/total, fill = as.factor(book))) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.004) +
    facet_wrap(~book, ncol = 6, scales = "free_y")
  
  freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total)
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = as.factor(book))) + 
    geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  rank_subset <- freq_by_rank %>% 
    dplyr::filter(rank < 500,
           rank > 10)
  
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  
  book_words <- book_words %>%
    bind_tf_idf(word, book, n)
  book_words
  
  book_words %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  book_words %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(word, tf_idf, fill = as.factor(book))) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 6, scales = "free") +
    coord_flip()

  #relations between words
  libros.df_2 <- lapply(libros,FUN=function(x) data.frame(line=1:length(x),text=x))
  libros.df_2 <- lapply(libros.df_2,FUN=function(x) x %>% unnest_tokens(bigram,text,token="ngrams",n=2))
  for (i in 1:length(libros.df_2)){
    libros.df_2[[i]]$book <- i
  }
  iliad_bigrams <- bind_rows(libros.df_2)
  
  iliad_bigrams %>%
    count(bigram, sort = TRUE)
  
  bigrams_separated <- iliad_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    dplyr::filter(!word1 %in% stop_words$word) %>%
    dplyr::filter(!word2 %in% stop_words$word)
  
  # new bigram counts:
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  libros.df_3 <- lapply(libros,FUN=function(x) data.frame(line=1:length(x),text=x))
  libros.df_3 <- lapply(libros.df_3,FUN=function(x) x %>% unnest_tokens(trigram,text,token="ngrams",n=3))
  for (i in 1:length(libros.df_3)){
    libros.df_3[[i]]$book <- i
  }
  iliad_trigrams <- bind_rows(libros.df_3)
  iliad_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    dplyr::filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)
  
  bigrams_filtered %>%
    dplyr::filter(word2 == "god") %>%
    count(word1, sort = TRUE)
  
  bigrams_filtered %>%
    dplyr::filter(word2 == "achilles") %>%
    count(word1, sort = TRUE)
  
  bigram_tf_idf <- iliad_bigrams %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))
  
  bigram_tf_idf %>% arrange(desc(tf_idf)) %>%
    mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
    group_by(book) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(bigram, tf_idf, fill = as.factor(book))) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~book, ncol = 6, scales = "free") +
    coord_flip()
  
  not_words <- bigrams_separated %>%
    dplyr::filter(word1 == "not") %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    count(word2, score, sort = TRUE) %>%
    ungroup()
  
  not_words %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by \"not\"") +
    ylab("Sentiment score * number of occurrences") +
    coord_flip()
  
  negation_words <- c("not", "no", "never", "without")
  
  negated_words <- bigrams_separated %>%
    dplyr::filter(word1 %in% negation_words) %>%
    inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
    count(word1, word2, score, sort = TRUE) %>%
    ungroup()
  
  negated_words %>%
    mutate(contribution = n * score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n * score, fill = n * score > 0)) +
    geom_col(show.legend = FALSE) +
    xlab("Words preceded by ") +
    ylab("Sentiment score * number of occurrences") +
    facet_wrap(~word1, ncol = 2, scales = "free") +
    coord_flip()
  