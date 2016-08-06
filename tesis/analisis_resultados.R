library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

#analisis resultados

df <- data.frame(opcion=c("BM25","TF/IDF","Ninguno"),
                 prop=c(60,25,15))
df <- arrange(df, desc(prop))
df$opcion <- factor(df$opcion,
                            levels=df$opcion)

ggplot(df, aes(x=opcion, y=prop, fill=opcion, label=prop)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  theme_bw() +
  scale_fill_brewer(palette="YlGnBu") +
  labs(fill="Opción") +
  ggtitle("% de selección de etiquetas sugeridas por BM25\n TF/IDF y ninguna en las encuestas")



ninguno <- c(22,23,55,66,83,85,88,95,97,98,99,106,109,
                        125,126,134,143,144,159,167,172,173,175,176,
                        177,179,192,196,199,205)
tfidf <- c(2,4,5,6,7,13,14,15,24,25,27,31,33,35,36,37,42,43,49,61,62,65,75,76,
           89,92,96,100,105,110,112,117,118,119,122,123,131,135,137,142,152,
           155,156,166,168,170,171,191,194,197,204)
bm25 <- seq(1:205)
bm25 <- which(!(bm25 %in% c(ninguno, tfidf)))
bm25 <- bm25[bm25 != 69]

load("~/Documents/itam/itam/tesis/docs_formulario.RData")

##ninguno
docs_ninguno <- documentos_humanos[ninguno, ]

docs_nin <- docs_ninguno %>% group_by(seccion_new) %>%
  summarise(docs=n())

all_docs <- documentos_humanos %>% group_by(seccion_new) %>%
  summarise(docs=n())

df_ninguno <- inner_join(all_docs, docs_nin, by=c("seccion_new"="seccion_new"))
names(df_ninguno) <- c("seccion","all","ninguno")
df_ninguno$prop <- round(df_ninguno$ninguno/df_ninguno$all*100,2)

ggplot(df_ninguno, aes(x=seccion, y=prop, fill=seccion, label=prop)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = sum(df_ninguno$ninguno)/sum(df_ninguno$all)*100, color="black") +
  theme_bw() +
  ggtitle("% de selección de etiquetas sugeridas\npor BM25 en las encuestas") +
  scale_fill_brewer(palette="Spectral") +
  ylab("%") +
  scale_y_continuous(limits=c(0, (max(df_ninguno$prop)*.10 + max(df_ninguno$prop)))) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

  


##bm25
docs_bm25 <- documentos_humanos[bm25,]

docs_bm25_df <- docs_bm25 %>% group_by(seccion_new) %>%
  summarise(docs=n())

df_bm25 <- inner_join(all_docs, docs_bm25_df, by=c("seccion_new"="seccion_new"))
names(df_bm25) <- c("seccion","all","bm25")
df_bm25$prop <- round(df_bm25$bm25/df_bm25$all*100,2)

ggplot(df_bm25, aes(x=seccion, y=prop, fill=seccion, label=prop)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = sum(df_bm25$bm25)/sum(df_bm25$all)*100, color="black") +
  theme_bw() +
  ggtitle("% de selección de etiquetas sugeridas\npor BM25 en las encuestas") +
  scale_fill_brewer(palette="Spectral") +
  ylab("%") +
  scale_y_continuous(limits=c(0, (max(df_bm25$prop)*.10 + max(df_bm25$prop)))) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

##tf/idf
docs_tfidf <- documentos_humanos[tfidf,]

docs_tfidf_df <- docs_tfidf %>% group_by(seccion_new) %>%
  summarise(docs=n())

df_tfidf <- inner_join(all_docs, docs_tfidf_df, by=c("seccion_new"="seccion_new"))
names(df_tfidf) <- c("seccion","all","tfidf")
df_tfidf$prop <- round(df_tfidf$tfidf/df_tfidf$all*100,2)

ggplot(df_tfidf, aes(x=seccion, y=prop, fill=seccion, label=prop)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = sum(df_tfidf$tfidf)/sum(df_tfidf$all)*100, color="black") +
  theme_bw() +
  ggtitle("% de selección de etiquetas sugeridas\npor TF/IDF en las encuestas") +
  scale_fill_brewer(palette="Spectral") +
  ylab("%") +
  scale_y_continuous(limits=c(0, (max(df_tfidf$prop)*.10 + max(df_tfidf$prop)))) +
  theme(axis.text.x=element_text(angle=90, hjust=1))


##all in one
recomendaciones <- data.frame(seccion=df_bm25$seccion,
                              all=df_bm25$all,
                              bm25=df_bm25$prop,
                              tfidf=df_tfidf$prop,
                              ninguno=df_ninguno$prop)

recom <- gather(recomendaciones, modelo, prop, -seccion, -all)

ggplot(recom, aes(x=seccion, y=prop, fill=modelo)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_brewer(palette="YlGnBu") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("% recomendaciones seleccionadas por sección")


###analisis por longitud de documentos

