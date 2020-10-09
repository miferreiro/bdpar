library("bdpar")
library("tm")
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
set.seed(1234)

runPipeline(path = system.file(file.path("example"),
                               package = "bdpar"))

all <- read.csv(file = "example.csv", header = TRUE, sep = ";", dec = ".",
                fill = FALSE, stringsAsFactors = FALSE)

sms <- all[all$extension == "tsms",]
eml <- all[all$extension == "eml",]

#Return the words and frequencies
word.frec <- function(data){
  corpus <- VCorpus(VectorSource(data))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stemDocument)
  m <- as.matrix(TermDocumentMatrix(corpus))
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  return(d)}

sms.words <- word.frec(sms$data)
eml.words <- word.frec(eml$data)
all.words <- word.frec(all$data)

# wordcloud for SMS and e-mail
op <- par(mfrow = c(1, 1), mai = rep(0, 4), mar = rep(0, 4))
wordcloud(words = sms.words$word, freq = sms.words$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = .5, colors = brewer.pal(8, "Dark2"))
text(x = 0.52, y = 0.15, labels = "Frecuency", vfont = c("serif","bold"))
legend(x = 0.23, y = 0.13, legend = c("1-3", 4, 10, 28), fill = brewer.pal(8, "Dark2")[c(1, 3, 2, 8)], bty = "n", y.intersp = 0.75, horiz = TRUE)

wordcloud(words = eml.words$word, freq = eml.words$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = .4, colors = brewer.pal(8, "Dark2"))
text(x = 0.52, y = 0.15, labels = "Frecuency", vfont = c("serif","bold"))
legend(x = 0.15, y = 0.13, legend = c("1-13", "13-25", 27, 46, 101), fill = brewer.pal(8, "Dark2")[c(1, 2, 3, 4, 8)], bty = "n", x.intersp = 0.25, y.intersp = 0.75, horiz = TRUE)
par(op)
#join wordcloud
op <- par(mai = rep(0,4), mar = rep(0,4))
wordcloud(words = all.words$word, freq = all.words$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = .5, colors = brewer.pal(8, "Dark2"))
text(x = 0.5, y = 0.15, labels = "Frecuency", vfont = c("serif","bold"))
legend(x = 0.19, y = 0.125, legend = c("1-16", "17-29", 56, 129), fill = brewer.pal(8, "Dark2")[c(1, 2, 3, 8)], bty = "n",  y.intersp = 0.75, horiz = TRUE)
par(op)

