#install.packages("tm")
#install.packages("stringi")
#install.packages("stringr")
#install.packages("qdap")
library(stringi)
library(stringr)
library(qdap)
library(tm)

# Ucitavanje dataseta, od 100.000 komentara napravljena su 2 dataseta, prvi sa komentarima
# klasifikovanim na pozitivne i negativne i drugi sa komentarima bez labele, ukoliko bude potrebno za kasniju analizu

movie_reviews <- read.csv(file = "imdb_master.csv", stringsAsFactors = FALSE)
movie_reviews$X <- NULL
movie_reviews$type <- NULL
movie_reviews$file <- NULL
movie_reviews_unsup<- subset(movie_reviews, movie_reviews$label == "unsup")
movie_reviews <- movie_reviews[-c(50001:100000),]

table(movie.reviews.train$label)

movie.reviews.train <- movie_reviews[-c(2501:47500),]
row.names(movie.reviews.train) <- seq(1:length(movie.reviews.train$review))
movie.reviews.train$label <- as.factor(movie.reviews.train$label)

str(movie.reviews.train)
class(movie.reviews.train$label)

# Broj karaktera u prvih 6 komentara. Obratiti paznju na to da nchar funkcija
# racuna i razmak odnosno space kao jedan karakter
nchar(head(movie.reviews.train$review))

# Primenom navedene nchar funkcije na celokupnu text kolonu, a potom pozivanjem
# mean funkcije dobijamo prosecnu duzinu komentara izrazenu brojem karaktera
mean(nchar(movie.reviews.train$review))

# Jos jedna odlicna primena nchar funkcije je uklanjanje dokumenata koji su
# prazni odnosno imaju 0 karaktera
movie_reviews <- subset(movie_reviews, nchar(movie_reviews$review)>0)

# Kreiranje korpusa
movies.corpus <- VCorpus(VectorSource(movie.reviews.train$review))
movies.corpus[[629]][1]
# Funkacija grep proverava da li se trazeni string nalazi u dokumentima
# BAR JEDNOM i vraca poziciju dokumenta odnosno id
grep("five", movie.reviews.train$review)

# Funkcija stri_count vraca vektor duzine broja dokumenata i za svaki dokument ukupan broj
# pojavljivanja trazenog termina
sum(stri_count(movie.reviews.train$review, fixed='http'))

# Ukoliko zelimo da saznamo koji procenat komentara sadrzi neki odredjeni string,
# mozemo iskoristiti funkciju grepl koja vraca logicki vektor T ili F u zavisnosti
# od toga da li dokument sadrzi trazeni string ili ne
brojac <- grepl("movie", movie.reviews.train$review, ignore.case = T)
sum(brojac)/nrow(movie.reviews.train)

# Jedan od najlaksih nacina da se uvidi zastupljenost pojedinacnih termina
# u celokupnom datasetu odnosno korpusu je freq_terms funcija iz qdap paketa i
# jednostavnom plot funkcijom se prikaze raspodela. Ovo pomaze da se uvide termini
# koji su veoma zastupljeni a znamo da nece uticati na sentiment i kandidati su 
# za stopwords. U nasem slucaju su to movie, br i film.
freq.terms <- freq_terms(movie.reviews.train$review, 20)
plot(freq.terms)

# Uklanjanje URL-a uz pomoc regularnih izraza. Nakon http ili www trazi se bilo koji simbol
# ukljucujuci i znakove interpunkcije i menja ih stringom url.
removeHTTP <- function(x) gsub("http[[:alnum:][:punct:]]*", "url", x)
movies.corpus <- tm_map(movies.corpus, content_transformer(removeHTTP))


# Qdap paket sadrzi dataframe - recnik sa 81 emotikonom i njegovim znacenjem. 
data(emoticon)
head(emoticon)

# Takodje mozemo da dodamo i nove emotikone i njihovo znacenje
meaning<-c('angry','smile')
emoticon<-c('>-:(',':D')
new.emoticons<-data.frame(meaning,emoticon)

emoticon <- rbind(emoticon,new.emoticons)


# Koristeci mgsub funkciju iz istog paketa mozemo da zamenimo emotikon sa njegovim znacenjem.
emoticons <- function(x) mgsub(emoticon[,2],emoticon[,1],x)
movies.corpus <- tm_map(movies.corpus, content_transformer(emoticons))



# Za potrebe sledecih funkcija koje rade sa stringovima odmah cemo pozvati funkciju tolower
# koja zamenjuje velika slova malim, radi smanjenja broja varijacija.
movies.corpus <- tm_map(movies.corpus, content_transformer(tolower))

removeCustom <- function(x) {
  patterns<-c('&amp','&lt','&gt','@','#','$','*','<br /><br />','%','Ã')
  replacements<-c(' ',' ',' ',' ',' ',' ',' ',' ',' ',' ')
  mgsub(patterns,replacements,x)  }
movies.corpus <- tm_map(movies.corpus, content_transformer(removeCustom))

changeCustom <- function(x) {
  patterns<-c('pls','thx','&','thanks')
  replacements<-c('please','thank you','and','thank you')
  mgsub(patterns,replacements,x)  }
movies.corpus <- tm_map(movies.corpus, content_transformer(changeCustom))

changeNumbers <- function(x) {
  patterns<-c('1st','2nd','3rd','4th','5th','6th','7th','8th','9th')
  replacements<-c('first','second','third','fourth','fifth','sixth','seventh','eighth','ninth')
  mgsub(patterns,replacements,x)  }
movies.corpus <- tm_map(movies.corpus, content_transformer(changeNumbers))

changeCustomNumbers <- function(x) {
  patterns<-c('one','two','three','four','five','six','seven','eighth','nine','ten')
  replacements<-c(1:10)
  mgsub(patterns,replacements,x)  }
movies.corpus <- tm_map(movies.corpus, content_transformer(changeCustomNumbers))

# U ovom trenutku je dobro primeniti funkciju koja uklanja prazne karaktere koji mogu 
# nastati kao rezultat prethodnog rada sa stringovima, odnosno njihovim brisanjem
movies.corpus <- tm_map(movies.corpus, stripWhitespace)

#Veoma je bitno ispratiti redosled pozivanja sledecih funkacija. Prva funkcija koja mora biti
# pozvana je tolower, kao priprema za funkciju koja uklanja i one stop words koje smo mi sami
# ukljucili a napisane su malim slovom. Posto stop words sadrze i one sa apostrofima koje je
# potrebno ukloniti, mora biti pozvana pre funkcije removePunctuation da bi se znaci interpunkcije
# zadrzali. Nakon uklanjanja znakova interpunkcije mogu se pojaviti dodatni prazni karakteri
# koje uklanjamo funkcijom stripWhitespace

movies.corpus <- tm_map(movies.corpus, tolower)

custom.stopwords <- c(stopwords('english'),
                      'film', 'movie')


movies.corpus <- tm_map(movies.corpus, removeWords, custom.stopwords)

movies.corpus <- tm_map(movies.corpus, removePunctuation, 
                        preserve_intra_word_contractions = TRUE,
                        preserve_intra_word_dashes = TRUE)

movies.corpus <- tm_map(movies.corpus, stripWhitespace)


movies.corpus <- tm_map(movies.corpus, removeNumbers)

# Pre stemovanja sacuvati kopiju starog korpusa ukoliko kasnije bude potrebno.
nostem <- movies.corpus


# stemovanje
movies.corpus <- tm_map(movies.corpus, stemDocument, language = "english")
movies.corpus <- tm_map(movies.corpus, stemCompletion, dictionary = no.stemming.corpus)

# moze se primeniti samo na manje korpus rucno. Funkcija koja bi se napravila ne bi imala
# smila jer je recnik koji koristi qdap veoma skroman i mnoge reci koje su zapravo pravilno 
# napisane bi bile zamenjene prvim pogresnim izborom koji funkcija nudi.
which_misspelled(movie_reviews$review[100])
check_spelling_interactive(movie_reviews$review[100])


# Kreiranje DTM matrice uz dodatne parametre za duzinu atributa
min.freq <- round(0.005*length(movies.corpus))
max.freq <- round(0.95*length(movies.corpus))
dtm <- DocumentTermMatrix(movies.corpus, 
                          control = list(bounds = list(global = c(min.freq,max.freq)),
                                         wordLengths = c(2,16),
                                         weighting = weightTf))
inspect(dtm)


# Podesavanje sparse tresholda, uklanjanje termina koji se retko pojavljuju

dtm.trimmed <- removeSparseTerms(dtm, sparse = 0.98)
inspect(dtm.trimmed)

names(term.freq)[1:10]
term.freq[1:10]
str(term.freq)

# Vizuelizacije
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("rlang")
library(tm)
library(ggplot2)
library(ggthemes)
library(rlang)

# Kreiranja dataframa od DTM matrice, koji ima 2 kolone gde je prva termin, a druga kolona
# je frekventnost pojavljivanja termina sto se dobija sumiranjem kolona koje su ustvari termini
dataframe <- as.matrix(dtm.trimmed.nostem)
term.freq <- colSums(dataframe)
freq.df <- data.frame( word = names(term.freq), frequency = term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]


freq.df$word<-factor(freq.df$word, levels = unique(as.character(freq.df$word)))
ggplot(freq.df[1:20,], aes(x=word, #ggplot f-ji prosledjujemo dataframe sa prvih 20 najucestalijih termina
                           y=frequency))+geom_bar(stat="identity",
                                                  fill='darkred')+coord_flip()+theme_gdocs()+
  geom_text(aes(label=frequency),
            colour="white",hjust=1.25, size=5.0)

# Word Associations
# Slicno korelaciji. Kada se termin x pojavi, trazi se termin y koji se pojavljuje uz njega.
# Ne zavisi direktno od frekvencije, vec sklapa termine u parove i za razliku od korelacije
# ne krece se izmedju -1 i 1 vec je vrednost izmedju 0 i 1.
associations <- findAssocs(dtm.trimmed, 'like', 0.15)
associations <- as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms,
                           levels=associations$terms)

ggplot(associations, aes(y=terms)) +
  geom_point(aes(x=like), data=associations,
             size=2)+
  theme_gdocs()+ geom_text(aes(x=like,
                               label=like),
                           colour="darkred",hjust=-.25,size=3)+
  theme(text=element_text(size=9),
        axis.title.y=element_blank())

# Word Clouds
# Bazirani su na frekventnosti. Sto je vizuelno rec veca u oblaku, to je njena frekventnost 
# veca u korpusu.
#install.packages("wordcloud")
library(wordcloud)
head(freq.df)
wordcloud(freq.df$word,freq.df$frequency, max.words = 70, colors=c('black','darkred'))

# MODEL 1 - stablo odlucivanja
#install.packages('rpart')
#install.packages('caret')
#install.packages('rattle')
source("confusion.matrix.R")
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

dataframe1 <- as.data.frame(as.matrix(dtm.trimmed))
str(dataframe1)
dataframe1$lbl <- movie.reviews.train$label

# Iz jedinstvenog dataframe-a pravimo dva koji predstavljaju test i train

set.seed(10)
train.indices <- createDataPartition(dataframe1$lbl, 
                                     
                                     p = .80,
                                     
                                     list = FALSE)

train.data <- dataframe1[train.indices,]
test.data <- dataframe1[-train.indices,]
table(test.data$lbl)

# Stablo odlucivanja
tree <- rpart(lbl ~ ., data = train.data, method = "class")
print(tree)

# predikcije nad test podacima
tree.pred <- predict(object = tree, newdata = test.data, type = "class")

# rezultati
cm1 <- table(true=test.data$lbl, predicted=tree.pred)
cm1
compute.eval.metrics(cm1)


# copy of test data for polarity

movie.reviews.test.polarity <- movie.reviews.test

movie.reviews.test.polarity$review <- removePunctuation(movie.reviews.test.polarity$review)
movie.reviews.test.polarity$review <- removeNumbers(movie.reviews.test.polarity$review)
movie.reviews.test.polarity$review <- tolower(movie.reviews.test.polarity$review)

movies.polarity <- polarity(movie.reviews.test.polarity$review)
movie.reviews.test.polarity$polarity <- movies.polarity$all$polarity

movie.reviews.test.polarity$polarity.scaled <- scale(movies.polarity$all$polarity)

# Sa plota se moze videti da rezultati nisu centrirani u nuli. Skaliranje ce ga pomeriti ka nuli
ggplot(movies.polarity$all, aes(x=polarity,
                                y=..density..)) + theme_gdocs() +
  geom_histogram(binwidth=.25,
                 fill="darkred",colour="grey60", size=.2) +
  geom_density(size=.75)

# Koristimo klasifikaciju na pozitivne i negativne, nula je odabrana kao treshold
movie.reviews.test.polarity$predicted_lbl <- ifelse(movie.reviews.test.polarity$polarity>0, "pos", "neg")
table(movie.reviews.test.polarity$predicted_lbl)

polarity.cm <- table(true=movie.reviews.test.polarity$label, predicted=movie.reviews.test.polarity$predicted_lbl)
polarity.cm
compute.eval.metrics(polarity.cm)



# Tokenizacija, kreiranje bigrama
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

# Kreiranje DTM matrice sa bigramima
dtm2 <- DocumentTermMatrix(movies.corpus, control=list(tokenize=BigramTokenizer))
inspect(dtm2)
# uklanjamo bigrame koji se pojavljuju retko
dtm2.trimmed <- removeSparseTerms(dtm2, sparse = 0.999)
inspect(dtm2.trimmed)

dataframe2 <- as.data.frame(as.matrix(dtm2.trimmed))
str(m1)
dataframe2$lbl <- movie.reviews.train$label

# train i test
library(caret)
set.seed(10)
train.indices <- createDataPartition(dataframe2$lbl, 
                                     
                                     p = .80,
                                     
                                     list = FALSE)

train.data1 <- dataframe2[train.indices,]
test.data1 <- dataframe2[-train.indices,]
table(test.data1$lbl)


# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))


#install.packages("ranger")
library(ranger)



# Random forest sa 50 stabala

rf3 <- ranger(dependent.variable.name = "lbl", data = train.data, num.trees = 50, write.forest = TRUE)
rf3
# predikcije koristeci kreirani rf3 model nad test podacima
rf3.pred <- predict(rf3, data = test.data)
rf3.pred$predictions
# evaluacije
cmm3 <- table(true = test.data$lbl, predicted = rf3.pred$predictions)
compute.eval.metrics(cmm3)

### Random forest sa 500 stabala

rf4 <- ranger(dependent.variable.name = "lbl", data = train.data, num.trees = 500, importance = "impurity", write.forest = TRUE)
rf4
# predikcije koristeci kreirani rf4 model nad test podacima
rf4.pred <- predict(rf4, data = test.data)
rf4.pred$predictions
# evaluacije
cmm4 <- table(true = test.data$lbl, predicted = rf4.pred$predictions)
compute.eval.metrics(cmm4)


### Random forest bigrami sa 500 stabala
rf.bigram <- ranger(dependent.variable.name = "lbl", data = train.data1, num.trees = 500, importance = "impurity", write.forest = TRUE)
rf.bigram
# predikcije koristeci kreirani rf.bigram model nad test podacima
rf.bigram.pred <- predict(rf.bigram, data = test.data1)
rf.bigram.pred$predictions
# evaluacije
cm.rf.bigrams <- table(true = test.data1$lbl, predicted = rf.bigram.pred$predictions)
compute.eval.metrics(cm.rf.bigrams)


# najvazniji atributi (bigrami) po proceni random forest algoritma
importance(rf.bigram)
i2 <- as.vector(rf.bigram$variable.importance)
t2 <- as.vector(colnames(train.data1))
importance.df2 <- as.data.frame(cbind(t2,i2))
importance.df2$i2 <- as.numeric(as.character(importance.df2$i2))
importance.df2<-importance.df2[order(importance.df2[,2], decreasing=T),]
str(importance.df2$i2)

# # najvazniji atributi (unigrami) po proceni random forest algoritma
importance(rf4)
i <- as.vector(rf4$variable.importance)
t <- as.vector(colnames(train.data))
importance.df <- as.data.frame(cbind(t,i))
importance.df$i <- as.numeric(as.character(importance.df$i))
importance.df<-importance.df[order(importance.df[,2], decreasing=T),]
importance.df$i <- as.numeric(as.character(importance.df$i))
str(importance.df$i)
# decimalni zapisi
options(digits = 4)

# grafik sa 30 najznacajnijih bigrama rf.bigram modela
importance2.top30 <- importance.df2[1:30,]

ggplot(importance2.top30, aes(x=reorder(t2,i2), y=i2,fill=i2))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  scale_fill_gradient(low="red", high="blue")

# cuvanje slike u razlicitim formatima
ggsave("bigram.png",width = 6.19 , height = 4 ,dpi = 600)
ggsave("bigram2.png",width = 6.19 , height = 4 ,dpi = 300)
ggsave("bigram.jpeg",width = 6.19 , height = 4 ,dpi = 600)


# grafik sa 40 najznacajnijih unigrama rf4 modela
importance.top40 <- importance.df[1:40,]

plot <- ggplot(importance.top40, aes(x=reorder(t,i), y=i,fill=i))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  scale_fill_gradient(low="red", high="blue")
ggsave("plot.png",width = 6.19 , height = 4.78 ,dpi = 600)
ggsave("plot2.png",width = 6.19 , height = 4.78 ,dpi = 300)
ggsave("plot.jpeg",width = 6.19 , height = 4.78 ,dpi = 600)


# Primena polarity funkcije na test podacima
test.vector <- row.names(test.data)
testt <- movie.reviews.train[test.vector,]
testt$predicted <- rf4.pred$predictions
row.names(testt) <- seq(1:length(testt$review))

# Sredjivanje teksta za polarity funkciju
test.polarity <- testt
test.polarity$review <- removePunctuation(test.polarity$review)
test.polarity$review <- removeNumbers(test.polarity$review)
test.polarity$review <- tolower(test.polarity$review)

polarity.score <- polarity(test.polarity$review)
test.polarity$polarity <- polarity.score$all$polarity



# Klasifikacija na pozitivne i negativne, nula je odabrana kao treshold
test.polarity$predicted_lbl <- ifelse(test.polarity$polarity>0 , "pos", "neg")
table(test.polarity$predicted_lbl)

## evaluacije
test.polarity.cm <- table(true=test.polarity$label, predicted=test.polarity$predicted_lbl)
test.polarity.cm
compute.eval.metrics(test.polarity.cm)
