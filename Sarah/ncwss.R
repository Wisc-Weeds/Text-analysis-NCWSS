library(readr)
library(dplyr)
library(rvest)
library(tidyverse)
library(data.table)
library(xml2)
library(pdftools)
library(tm)
library(glue)
library(pipeR)

#Download dos documentos

url_base <- "http://ncwss.org/publications/"


urls <- url_base %>%
    read_html() %>%
    html_nodes(xpath = "//a[@href]") %>%
    html_attr(name = "href")

urls

hrefs = urls[urls %like% ".pdf"]

hrefs

dfs <- hrefs
dfs <- gsub("http://ncwss.org/wp-content/uploads/", "", df)

dfs

for (i in 1:length(hrefs)){
  download.file(hrefs[i], df[i])
}

urls

hrefs = urls[urls %like% "proceed/"]
hrefs

#download de um documento único com o material das conferências de 2010 a 2017 
#e do 2007 Gene Flow Symposium

#Abstracts da conferência de 2009

url_base <- "http://ncwss.org/proceed/2009/grid.html"

urls_2009 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")


urls_2009

hrefs_2009 = urls_2009[urls_2009 %like% ".pdf"]
df_2009 = hrefs_2009

df_2009

hrefs_2009 <- paste0("http://ncwss.org/proceed/2009/", hrefs_2009)
df_2009 <- gsub("proceed/2009/Abstracts/", "", df_2009)

hrefs_2009

for (i in 1:length(hrefs_2009)){
  download.file(hrefs_2009[i], df_2009[i])
}

#2008

url_base <- "http://ncwss.org/proceed/2008/grid.html"

urls_2008 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2008

hrefs_2008 = urls_2008[urls_2008 %like% ".pdf"]
df_2008 = hrefs_2008

df_2008

hrefs_2008 <- paste0("http://ncwss.org/proceed/2008/", hrefs_2008)
df_2008 <- gsub("abstracts/", "", df_2008)

hrefs_2008
df_2008

for (i in 1:length(hrefs_2008)){
  download.file(hrefs_2008[i], df_2008[i])
}

#2007

hrefs

url_base <- "http://ncwss.org/proceed/2007/grid.htm"

urls_2007 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2007

hrefs_2007 = urls_2007[urls_2007 %like% ".pdf"]
df_2007 = hrefs_2007

df_2007

hrefs_2007 <- paste0("http://ncwss.org/proceed/2007/", hrefs_2007)
df_2007 <- gsub("Abstracts/", "2007", df_2007)

hrefs_2007
df_2007

for (i in 1:length(hrefs_2007)){
  download.file(hrefs_2007[i], df_2007[i])
}

#2006

hrefs

url_base <- "http://ncwss.org/proceed/2006/grid.html"

urls_2006 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2006

hrefs_2006 = urls_2006[urls_2006 %like% ".pdf"]
df_2006 = hrefs_2006

df_2006

hrefs_2006 <- paste0("http://ncwss.org/proceed/2006/", hrefs_2006)
df_2006 <- gsub("abstracts/", "", df_2006)

hrefs_2006
df_2006

for (i in 1:length(hrefs_2006)){
  download.file(hrefs_2006[i], df_2006[i])
}

#2005

hrefs

url_base <- "http://ncwss.org/proceed/2005/proc05/abstracts/grid05.htm"

urls_2005 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2005

hrefs_2005 = urls_2005[urls_2005 %like% ".pdf"]
df_2005 = hrefs_2005

df_2005

hrefs_2005 <- paste0("http://ncwss.org/proceed/2005/proc05/abstracts/", hrefs_2005)

hrefs_2005

for (i in 1:length(hrefs_2005)){
  download.file(hrefs_2005[i], df_2005[i])
}

#2004

hrefs

url_base <- "http://ncwss.org/proceed/2004/proc04/abstracts/04grid.htm"

urls_2004 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2004

hrefs_2004 = urls_2004[urls_2004 %like% ".pdf"]
df_2004 = hrefs_2004

df_2004

hrefs_2004 <- paste0("http://ncwss.org/proceed/2004/proc04/abstracts/", hrefs_2004)

hrefs_2004

for (i in 1:length(hrefs_2004)){
  download.file(hrefs_2004[i], df_2004[i])
}

#2003

hrefs

url_base <- "http://ncwss.org/proceed/2003/Proc03/abstracts/03abstracts.htm"

urls_2003 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2003

hrefs_2003 = urls_2003[urls_2003 %like% ".pdf"]
df_2003 = hrefs_2003

df_2003
hrefs_2003

hrefs_2003 <- gsub("../../", "http://ncwss.org/proceed/2003/", hrefs_2003)
df_2003 <- gsub("../../2003%20NCWSS%20Proceedings/abstracts/", "", df_2003)

hrefs_2003
df_2003

for (i in 1:length(hrefs_2003)){
  download.file(hrefs_2003[i], df_2003[i])
}

#2002

hrefs

url_base <- "http://ncwss.org/proceed/2002/Proc2002/abstracts/paperno2.htm"

urls_2002 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2002

hrefs_2002 = urls_2002[urls_2002 %like% ".pdf"]
df_2002 = hrefs_2002

df_2002

hrefs_2002 <- paste0("http://ncwss.org/proceed/2002/Proc2002/abstracts/", hrefs_2002)

hrefs_2002

for (i in 1:length(hrefs_2002)){
  download.file(hrefs_2002[i], df_2002[i])
}

#2001

hrefs

url_base <- "http://ncwss.org/proceed/2001/indexes/paperno.htm"

urls_2001 <- url_base %>%
  read_html() %>%
  html_nodes(xpath = "//a[@href]") %>%
  html_attr(name = "href")

urls_2001

hrefs_2001 = urls_2001[urls_2001 %like% ".pdf"]
hrefs_2001

df_2001 = hrefs_2001

hrefs_2001 <- gsub("../abstracts/", "http://ncwss.org/proceed/2001/abstracts/", hrefs_2001)
df_2001 <- gsub("../abstracts/", "", df_2001)

hrefs_2001
df_2001

for (i in 1:length(hrefs_2001)){
  download.file(hrefs_2001[i], df_2001[i])
}

file <- list.files (pattern = "pdf$")

ncwss_2007<- lapply(file, pdf_text)


ncwss_2007[1:207] <- gsub("\n", "", ncwss_2007[1:207])

ncwss_2007[[100]]

#text mining

library(ggplot2)
library(igraph)
library(ggraph)
library(topicmodels)
library(wordcloud)

ncwss_2007_source <- VectorSource(ncwss_2007)
ncwss_2007_corpus <- VCorpus(ncwss_2007_source)

ncwss_2007_corpus
class(ncwss_2007_corpus)

str(ncwss_2007_corpus[[4]])

limpa_corpus <- function(corpus){
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords())
  
  corpus
}

ncwss_2007_corpus <- limpa_corpus(ncwss_2007_corpus)

dtm_ncwss_2007 <- DocumentTermMatrix(ncwss_2007_corpus)
findFreqTerms(dtm_ncwss_2007, 60)

word
