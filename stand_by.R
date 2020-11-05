```{r}
# this code section will take long (~3 min) to run. 
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation) # remove punctuation
  corpus <- tm_map(corpus, content_transformer(tolower)) # remove punctuation
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords_en) # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords_en) # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords_first) # remove stop words 
  corpus <- tm_map(corpus, removeWords, stopwords_last) # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords_countries) # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords_state) # remove stop words
  corpus <- tm_map(corpus, removeWords, stopwords_capitals ) # remove stop words  
  corpus
}
```



#2002

```{r}
co2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/cereals2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
cs2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/corn2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
met2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/equipment2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
ext2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/extension2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
for2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/forage2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
phy2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/physiology2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
turf2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/industrial2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soil2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/soil2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soy2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/soybeans2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
hort2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/sugarbeets2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
eco2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/ecology2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
app2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/applicsymposium2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
res2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/glysymp2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
shi2002 <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/weedshiftsymp2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 



nc_2002 <- rbind(co2002, cs2002, met2002, ext2002, for2002, phy2002, soil2002, 
                 soy2002, turf2002, hort2002, eco2002, app2002, res2002, shi2002)
```


```{r warning=FALSE, include=FALSE}
titledm_02 <- data.frame() # as data frame

for (i in 1:length(nc_2002)) {
  print(i)
  titlelines <- data.frame(names = nc_2002[[i]])
  titledm_02 <- bind_rows(titledm_02, titlelines)
} 
```

```{r}
titledm_02$names <- gsub("2,4-D" , "twofourd" , titledm_02$names)
```

```{r}
title_tokens_02 <- titledm_02 %>%
  unnest_tokens(words, names)
```


```{r eval=FALSE, include=FALSE}
authors <- read_html("http://ncwss.org/proceed/2002/Proc2002/abstracts/author2.htm") %>% 
  html_nodes(xpath = "/html/body/p[.]") %>%
  html_text() 
authors <- c(authors)
authors <- data_frame(authors)
#tokerization
authors_tokens <- authors %>%
  unnest_tokens(words, authors)
```


```{r }
title_tokens_02 <- anti_join(title_tokens_02, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_02 <- anti_join(title_tokens_02, stopwords_tokens, by = "words") 
title_tokens_02 <- anti_join(title_tokens_02, authors_tokens, by = "words") 
```


```{r}
title_source_02 <- VectorSource(title_tokens_02$words)
title_corpus_02 <- VCorpus(title_source_02)
```

```{r}
title_corpus_02 <- clean_corpus(title_corpus_02)
```

```{r}
title_dtm_02 <- TermDocumentMatrix(title_corpus_02)
title_matrix_02 <- as.matrix(title_dtm_02)
title_v_02 <- sort(rowSums(title_matrix_02), decreasing=TRUE)
title_dt_02 <- data.frame(word = names(title_v_02),freq=title_v_02)
```

```{r}
title_dt_02$word <- gsub("twofourd" , "2,4-D" , title_dt_02$word)
```

```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_02 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words02 <- title_dt_02  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2002")
```




```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_02$word, freq = title_dt_02$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```













#2003

```{r}
co2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Cereals%20and%20Oilseeds.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
cs2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Corn%20and%20Sorghum.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
met2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Equipment%20and%20Application.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
ext2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Extension.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
for2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Forages%20and%20Range.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
phy2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Herbicide%20Phy.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soil2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Soil%20and%20Environment.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soy2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Soybean.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
hort2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/Sugarbeet,%20hort,%20ornamental.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
eco2003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/weed%20ecology.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
sym22003 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/symposia.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 



nc_2003 <- rbind(co2003, cs2003, met2003, ext2003, for2003, phy2003, soil2003, 
                 soy2003, hort2003, eco2003, sym22003)
```

```{r warning=FALSE, include=FALSE}
titledm_03 <- data.frame() # as data frame

for (i in 1:length(nc_2003)) {
  print(i)
  titlelines <- data.frame(names = nc_2003[[i]])
  titledm_03 <- bind_rows(titledm_03, titlelines)
} 
```

```{r}
titledm_03$names <- gsub("2,4-D" , "twofourd" , titledm_03$names)
```

```{r}
title_tokens_03 <- titledm_03 %>%
  unnest_tokens(words, names)
```


```{r eval=FALSE, include=FALSE}
authors_03 <- read_html("http://ncwss.org/proceed/2003/Proc03/abstracts/author%20index.htm") %>% 
  html_nodes(xpath = "/html/body/ilayer/table[2]") %>%
  html_text() 
authors_03 <- c(authors_03)
authors_03 <- data_frame(authors_03)
#tokerization
authors_tokens_03 <- authors_03 %>%
  unnest_tokens(words, authors_03)
```



```{r }
title_tokens_03 <- anti_join(title_tokens_03, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_03 <- anti_join(title_tokens_03, stopwords_tokens, by = "words") 
title_tokens_03 <- anti_join(title_tokens_03, authors_tokens, by = "words") 
```




```{r}
title_source_03 <- VectorSource(title_tokens_03$words)
title_corpus_03 <- VCorpus(title_source_03)
```


```{r}
title_corpus_03 <- clean_corpus(title_corpus_03)
```

```{r}
title_dtm_03 <- TermDocumentMatrix(title_corpus_03)
title_matrix_03 <- as.matrix(title_dtm_03)
title_v_03 <- sort(rowSums(title_matrix_03), decreasing=TRUE)
title_dt_03 <- data.frame(word = names(title_v_03),freq=title_v_03)
```

```{r}
title_dt_03$word <- gsub("twofourd" , "2,4-D" , title_dt_03$word)
```

```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_03 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words03 <- title_dt_03  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2003")
```


```{r message=FALSE, warning=FALSE}
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_03$word, freq = title_dt_03$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```



# 2004


```{r}
co2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/cereals2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
cs2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/corn2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
met2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/equipment2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
ext2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/extension2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
for2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/forage2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
phy2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/physiology2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
tur2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/industrial2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
inv2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/invasives2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soy2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/soybean2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
hort2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/sugarbeets2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
eco2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/ecology2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
tur22004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/herbtolturf2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
iv22004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/invasives2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
seed2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/seedbankdynam2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
sind2004 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/seedindustry2.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 


nc_2004 <- rbind(co2004, cs2004, met2004, ext2004, for2004, phy2004, tur2004, inv2004, 
                 soy2004, hort2004, eco2004, tur22004, iv22004, seed2004, sind2004)
```

```{r warning=FALSE, include=FALSE}
titledm_04 <- data.frame() # as data frame

for (i in 1:length(nc_2004)) {
  print(i)
  titlelines <- data.frame(names = nc_2004[[i]])
  titledm_04 <- bind_rows(titledm_04, titlelines)
} 
```

```{r}
titledm_04$names <- gsub("2,4-D" , "twofourd" , titledm_04$names)
```


```{r}
title_tokens_04 <- titledm_04 %>%
  unnest_tokens(words, names)
```


```{r eval=FALSE, include=FALSE}
authors_04 <- read_html("http://ncwss.org/proceed/2004/proc04/abstracts/authors.htm") %>% 
  html_nodes(xpath = "//table[2]") %>%
  html_text() 
authors_04 <- c(authors_04)
authors_04 <- data_frame(authors_04)
#tokerization
authors_tokens_04 <- authors_04 %>%
  unnest_tokens(words, authors_04)
```


```{r }
title_tokens_04 <- anti_join(title_tokens_04, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_04 <- anti_join(title_tokens_04, stopwords_tokens, by = "words") 
title_tokens_04 <- anti_join(title_tokens_04, authors_tokens, by = "words") 
```


```{r}
title_source_04 <- VectorSource(title_tokens_04$words)
title_corpus_04 <- VCorpus(title_source_04)
```



```{r}
title_corpus_04 <- clean_corpus(title_corpus_04)
```



```{r}
title_dtm_04 <- TermDocumentMatrix(title_corpus_04)
title_matrix_04 <- as.matrix(title_dtm_04)
title_v_04 <- sort(rowSums(title_matrix_04), decreasing=TRUE)
title_dt_04 <- data.frame(word = names(title_v_04),freq=title_v_04)
```


```{r}
title_dt_04$word <- gsub("twofourd" , "2,4-D" , title_dt_04$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_04 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words04 <- title_dt_04  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2004")
```


```{r warning = FALSE} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_04$word, freq = title_dt_04$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```




# 2005




```{r}
co2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/cereals.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
cs2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/corn.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
met2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/equipment.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
ext2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/extension.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
for2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/forage.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
phy2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/physiology.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
tur2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/industrial.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
inv2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/invasive.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soil2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/soil.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
soy2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/soybean.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
hort2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/sugarbeets.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
eco2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/ecolbiol.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 
man2005 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/ecolmanage.htm") %>% 
  html_nodes(xpath = "//p[.]") %>%
  html_text() 



nc_2005 <- rbind(co2005, cs2005, met2005, ext2005, for2005, phy2005, tur2005, inv2005, soil2005,
                 soy2005, hort2005, eco2005, man2005)
```

```{r warning=FALSE, include=FALSE}
titledm_05 <- data.frame() # as data frame

for (i in 1:length(nc_2005)) {
  print(i)
  titlelines <- data.frame(names = nc_2005[[i]])
  titledm_05 <- bind_rows(titledm_05, titlelines)
} 
```

```{r}
titledm_05$names <- gsub("2,4-D" , "twofourd" , titledm_05$names)
```

```{r}
title_tokens_05 <- titledm_05 %>%
  unnest_tokens(words, names)
```


```{r eval=FALSE, include=FALSE}
authors_05 <- read_html("http://ncwss.org/proceed/2005/proc05/abstracts/author.htm") %>% 
  html_nodes(xpath = "//table[2]") %>%
  html_text() 
authors_05 <- c(authors_05)
authors_05 <- data_frame(authors_05)
#tokerization
authors_tokens_05 <- authors_05 %>%
  unnest_tokens(words, authors_05)
```


```{r }
title_tokens_05 <- anti_join(title_tokens_05, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_05 <- anti_join(title_tokens_05, stopwords_tokens, by = "words") 
title_tokens_05 <- anti_join(title_tokens_05, authors_tokens, by = "words") 
```


```{r}
title_source_05 <- VectorSource(title_tokens_05$words)
title_corpus_05 <- VCorpus(title_source_05)
```



```{r}
title_corpus_05 <- clean_corpus(title_corpus_05)
```



```{r}
title_dtm_05 <- TermDocumentMatrix(title_corpus_05)
title_matrix_05 <- as.matrix(title_dtm_05)
title_v_05 <- sort(rowSums(title_matrix_05), decreasing=TRUE)
title_dt_05 <- data.frame(word = names(title_v_05),freq=title_v_05)
```


```{r}
title_dt_05$word <- gsub("twofourd" , "2,4-D" , title_dt_05$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_05 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words05 <- title_dt_05  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2005")
```


```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_05$word, freq = title_dt_05$freq, min.freq=1, scale=c(5,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```






# 2006

```{r}
pdf_06 <- pdf_text("docs/nc2006.pdf") %>% 
  strsplit(split = "\n")
```

```{r}
title <- pdf_06[1:20] 
```


```{r warning=FALSE, include=FALSE}
titledm_06 <- data.frame() # as data frame

for (i in 1:length(title)) {
  print(i)
  titlelines_06 <- data.frame(names = title[[i]])
  titledm_06 <- bind_rows(titledm_06, titlelines_06)
} 
```

```{r}
titledm_06$names <- gsub("2,4-D" , "twofourd" , titledm_06$names)
```


```{r}
title_tokens_06 <- titledm_06 %>%
  unnest_tokens(words, names)
```


```{r warning=FALSE, include=FALSE}
authors_06 <- pdf_06[21:28]

authorsdm_06 <- data.frame()

for (i in 1:length(authors)) {
  print(i)
  authorslines_06 <- data.frame(names = authors_06[[i]])
  authorsdm_06 <- bind_rows(authorsdm_06, authorslines_06)
} 

#tokerization
authors_tokens_06 <- authorsdm_06 %>%
  unnest_tokens(words, names)
```



```{r warning=FALSE}
title_tokens_06 <- anti_join(title_tokens_06, authors_tokens_06, by = "words")
```



```{r include=FALSE}
rm_author_06 <- c(1:12) 
rm_authors_number_06 <-c()
for (i in 1:length(rm_author)) {
  print(i)
  rm_authors_numbers_06 <- c(paste0(authors_tokens_06$words, i))
  rm_authors_number_06 <- c(rm_authors_number_06, rm_authors_numbers_06)
}
```


```{r}
rm_authors_06 <- data.frame(rm_authors_number_06) 
rm_authors_06 <- rename(rm_authors_06, words = rm_authors_number_06) 
rm_authors_06$words <- as.character(rm_authors_06$words)
```


```{r }
title_tokens_06 <- anti_join(title_tokens_06, rm_authors_06, by = "words") 
```


```{r }
title_tokens_06 <- anti_join(title_tokens_06, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_06 <- anti_join(title_tokens_06, stopwords_tokens, by = "words") 
title_tokens_06 <- anti_join(title_tokens_06, authors_tokens_06, by = "words") 
```


```{r}
title_source_06 <- VectorSource(title_tokens_06$words)
title_corpus_06 <- VCorpus(title_source_06)
```



```{r}
title_corpus_06 <- clean_corpus(title_corpus_06)
```



```{r}
title_dtm_06 <- TermDocumentMatrix(title_corpus_06)
title_matrix_06 <- as.matrix(title_dtm_06)
title_v_06 <- sort(rowSums(title_matrix_06), decreasing=TRUE)
title_dt_06 <- data.frame(word = names(title_v_06),freq=title_v_06)
```


```{r}
title_dt_06$word <- gsub("twofourd" , "2,4-D" , title_dt_06$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_06 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words06 <- title_dt_06  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2006")
```


```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_06$word, freq = title_dt_06$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```




# 2007





```{r}
ag2007 <- read_html("http://ncwss.org/proceed/2007/agronomic.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
ex2007 <- read_html("http://ncwss.org/proceed/2007/extension.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
phy2007 <- read_html("http://ncwss.org/proceed/2007/physiology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
hort2007 <- read_html("http://ncwss.org/proceed/2007/horticulture.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
inv2007 <- read_html("http://ncwss.org/proceed/2007/invasiveweeds.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
bem2007 <- read_html("http://ncwss.org/proceed/2007/biology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
cm2007 <- read_html("http://ncwss.org/proceed/2007/communication.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
gf2007 <- read_html("http://ncwss.org/proceed/2007/geneflow.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 

nc_2007 <- rbind(ag2007, ex2007, phy2007, hort2007, inv2007, bem2007,  cm2007, gf2007)
```

```{r warning=FALSE, include=FALSE}
titledm_07 <- data.frame() # as data frame

for (i in 1:length(nc_2007)) {
  print(i)
  titlelines_07 <- data.frame(names = nc_2007[[i]])
  titledm_07 <- bind_rows(titledm_07, titlelines_07)
} 
```

```{r}
titledm_07$names <- gsub("2,4-D" , "twofourd" , titledm_07$names)
```


```{r}
title_tokens_07 <- titledm_07 %>%
  unnest_tokens(words, names)
```


```{r}
authors_07 <- read_html("http://ncwss.org/proceed/2007/authorindex.html") %>% 
  html_nodes(xpath = "//table[.]") %>%
  html_text() 
authors_07 <- c(authors_07)
authors_07 <- data_frame(authors_07)
#tokerization
authors_tokens_07 <- authors_07 %>%
  unnest_tokens(words, authors_07)
```



```{r }
title_tokens_07 <- anti_join(title_tokens_07, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_07 <- anti_join(title_tokens_07, stopwords_tokens, by = "words") 
title_tokens_07 <- anti_join(title_tokens_07, authors_tokens_07, by = "words") 
```


```{r}
title_source_07 <- VectorSource(title_tokens_07$words)
title_corpus_07 <- VCorpus(title_source_07)
```



```{r}
title_corpus_07 <- clean_corpus(title_corpus_07)
```



```{r}
title_dtm_07 <- TermDocumentMatrix(title_corpus_07)
title_matrix_07 <- as.matrix(title_dtm_07)
title_v_07 <- sort(rowSums(title_matrix_07), decreasing=TRUE)
title_dt_07 <- data.frame(word = names(title_v_07),freq=title_v_07)
```


```{r}
title_dt_07$word <- gsub("twofourd" , "2,4-D" , title_dt_07$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_07 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words07 <- title_dt_07  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2007")
```


```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_07$word, freq = title_dt_07$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```









# 2008


```{r}
ag2008 <- read_html("http://ncwss.org/proceed/2008/agronomic.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
ex2008 <- read_html("http://ncwss.org/proceed/2008/extension.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
phy2008 <- read_html("http://ncwss.org/proceed/2008/physiology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
hort2008 <- read_html("http://ncwss.org/proceed/2008/horticulture.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
inv2008 <- read_html("http://ncwss.org/proceed/2008/invasiveweeds.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
bem2008 <- read_html("http://ncwss.org/proceed/2008/biology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
swr2008 <- read_html("http://ncwss.org/proceed/2008/HRC%20Symposium.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
siw2008 <- read_html("http://ncwss.org/proceed/2008/IWM%20symposium.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
siv2008 <- read_html("http://ncwss.org/proceed/2008/MIPN%20Symposium.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 

nc_2008 <- rbind(ag2008, ex2008, phy2008, hort2008, inv2008, bem2008, swr2008, siw2008, siv2008)
```


```{r warning=FALSE, include=FALSE}
titledm_08 <- data.frame() # as data frame

for (i in 1:length(nc_2008)) {
  print(i)
  titlelines_08 <- data.frame(names = nc_2008[[i]])
  titledm_08 <- bind_rows(titledm_08, titlelines_08)
} 
```

```{r}
titledm_08$names <- gsub("2,4-D" , "twofourd" , titledm_08$names)
```

```{r}
title_tokens_08 <- titledm_08 %>%
  unnest_tokens(words, names)
```


```{r}
# Authors 2008
authors_08 <- read_html("http://ncwss.org/proceed/2008/authorindex.html") %>% 
  html_nodes(xpath = "//table[.]") %>%
  html_text() 
authors_08 <- c(authors_08)
authors_08 <- data_frame(authors_08)
#tokerization
authors_tokens_08 <- authors_08 %>%
  unnest_tokens(words, authors_08)
```



```{r }
title_tokens_08 <- anti_join(title_tokens_08, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_08 <- anti_join(title_tokens_08, stopwords_tokens, by = "words") 
title_tokens_08 <- anti_join(title_tokens_08, authors_tokens_08, by = "words") 
```




```{r}
title_source_08 <- VectorSource(title_tokens_08$words)
title_corpus_08 <- VCorpus(title_source_08)
```




```{r}
title_corpus_08 <- clean_corpus(title_corpus_08)
```



```{r}
title_dtm_08 <- TermDocumentMatrix(title_corpus_08)
title_matrix_08 <- as.matrix(title_dtm_08)
title_v_08 <- sort(rowSums(title_matrix_08), decreasing=TRUE)
title_dt_08 <- data.frame(word = names(title_v_08),freq=title_v_08)
```


```{r}
title_dt_08$word <- gsub("twofourd" , "2,4-D" , title_dt_08$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_08 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words08 <- title_dt_08  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2008")
```


```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_08$word, freq = title_dt_08$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```





# 2009

```{r}
ag2009 <- read_html("http://ncwss.org/proceed/2009/agronomic.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
ex2009 <- read_html("http://ncwss.org/proceed/2009/extension.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
phy2009 <- read_html("http://ncwss.org/proceed/2009/physiology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
hort2009 <- read_html("http://ncwss.org/proceed/2009/horticulture.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
inv2009 <- read_html("http://ncwss.org/proceed/2009/invasiveweeds.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
bem2009 <- read_html("http://ncwss.org/proceed/2009/biology.html") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
lea2009 <- read_html("http://ncwss.org/proceed/2009/Learning%20store%20symposium.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 
st2009 <- read_html("http://ncwss.org/proceed/2009/statistics%20symposium.htm") %>% 
  html_nodes(xpath = "//strong") %>%
  html_text() 


nc_2009 <- rbind(ag2009, ex2009, phy2009, hort2009, inv2009, bem2009, lea2009, st2009)
```

```{r warning=FALSE, include=FALSE}
titledm_09 <- data.frame() # as data frame

for (i in 1:length(nc_2009)) {
  print(i)
  titlelines_09 <- data.frame(names = nc_2009[[i]])
  titledm_09 <- bind_rows(titledm_09, titlelines_09)
} 
```

```{r}
titledm_09$names <- gsub("2,4-D" , "twofourd" , titledm_09$names)
```

```{r}
title_tokens_09 <- titledm_09 %>%
  unnest_tokens(words, names)
```



```{r}
# Authors 2009
authors_09 <- read_html("http://ncwss.org/proceed/2009/authorindex.html") %>% 
  html_nodes(xpath = "//table[.]") %>%
  html_text() 
authors_09 <- c(authors_09)
authors_09 <- data_frame(authors_09)
#tokerization
authors_tokens_09 <- authors_09 %>%
  unnest_tokens(words, authors_09)
```



```{r }
title_tokens_09 <- anti_join(title_tokens_09, stop_wssaprogram_words_tokens, by = "words") 
title_tokens_09 <- anti_join(title_tokens_09, stopwords_tokens, by = "words") 
title_tokens_09 <- anti_join(title_tokens_09, authors_tokens_09, by = "words") 
```





```{r}
title_source_09 <- VectorSource(title_tokens_09$words)
title_corpus_09 <- VCorpus(title_source_09)
```



```{r}
title_corpus_09 <- clean_corpus(title_corpus_09)
```



```{r}
title_dtm_09 <- TermDocumentMatrix(title_corpus_09)
title_matrix_09 <- as.matrix(title_dtm_09)
title_v_09 <- sort(rowSums(title_matrix_09), decreasing=TRUE)
title_dt_09 <- data.frame(word = names(title_v_09),freq=title_v_09)
```


```{r}
title_dt_09$word <- gsub("twofourd" , "2,4-D" , title_dt_09$word)
```


```{r fig.cap= "Most frequent words (n=30+) that appeared in the WSSA/WSWS oral and poster presentation titles."}
title_dt_09 %>% 
  filter(freq >= 10) %>% 
  ggplot(aes(x=reorder(word,freq), y=freq, fill=freq)) +
  scale_fill_continuous(low = "#FFF6F6", high = "#9b0000") +
  geom_col(show.legend=FALSE) + coord_flip() + theme_classic() +
  labs(y="Frequence (n)", x="Words") + #ylim(0,150) +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=13)) 
```

```{r}
words09 <- title_dt_09  %>% 
  select(word, freq) %>% 
  #group_by(word) %>% 
  add_tally(freq, name="nn") %>% 
  mutate(Perc=(freq/nn)*100) %>% 
  mutate_at(4, round, 1) %>% 
  mutate(year = "2009")
```


```{r} 
# need to stop footnotes' word

pal <- brewer.pal(8,"Dark2") #selecting the color palette

set.seed(1234)
wordcloud(words = title_dt_09$word, freq = title_dt_09$freq, min.freq=1, scale=c(4,.1),
          random.order=FALSE, rot.per=0.35, colors=pal)
```
