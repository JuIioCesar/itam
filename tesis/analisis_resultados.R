library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(corrplot)

#analisis resultados

df <- data.frame(opcion=c("BM25","TF/IDF","Ninguno"),
                 prop=c(60,25,15))
df <- arrange(df, desc(prop))
df$opcion <- factor(df$opcion,
                            levels=df$opcion)
df$modelo <- "opcion"

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
docs_bm25$longitud <- sapply(docs_bm25$content, function(x) str_split(x, " ") %>% 
                               unlist() %>% length())

lon_bm25 <- group_by(docs_bm25, seccion_new) %>%
  summarise(mean_len=round(mean(longitud)))

ggplot(lon_bm25, aes(x=seccion_new, y=mean_len, fill=seccion_new, label=mean_len)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = mean(lon_bm25$mean_len)) + 
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  xlab("seccion") +
  ylab("promedio longitud") + 
  labs(fill="seccion") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Promedio de longitud de documentos recomendación BM25")

## tfidf
docs_tfidf$longitud <- sapply(docs_tfidf$content, function(x) str_split(x, " ") %>%
                                unlist() %>% length())

lon_tfidf <- group_by(docs_tfidf, seccion_new) %>%
  summarise(mean_len=round(mean(longitud)))

ggplot(lon_tfidf, aes(x=seccion_new, y=mean_len, fill=seccion_new, label=mean_len)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = mean(lon_tfidf$mean_len)) + 
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  xlab("seccion") +
  ylab("promedio longitud") + 
  labs(fill="seccion") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Promedio de longitud de documentos recomendación TF/IDF")

##ninguna
docs_ninguno$longitud <- sapply(docs_ninguno$content, function(x) str_split(x, " ") %>%
                                unlist() %>% length())

lon_ninguno <- group_by(docs_ninguno, seccion_new) %>%
  summarise(mean_len=round(mean(longitud)))

ggplot(lon_ninguno, aes(x=seccion_new, y=mean_len, fill=seccion_new, label=mean_len)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  geom_hline(yintercept = mean(lon_ninguno$mean_len)) + 
  scale_fill_brewer(palette="Spectral") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  xlab("seccion") +
  ylab("promedio longitud") + 
  labs(fill="seccion") +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Promedio de longitud de documentos recomendación Ninguno")


#all
longitudes_all <- lon_bm25
names(longitudes_all)[grep("mean_len", names(longitudes_all))] <- "bm25"
longitudes_all$tfidf <- lon_tfidf$mean_len
longitudes_all$ninguno <- lon_ninguno$mean_len

longitudes_df <- gather(longitudes_all, modelo, mean_len, -seccion_new)

# ggplot(longitudes, aes(x=seccion_new, y=mean_len, fill=modelo, label=mean_len)) +
#   geom_bar(stat="identity", position="dodge") +
#   #geom_hline(yintercept=mean(longitudes$mean_len)) +
#   scale_fill_brewer(palette="YlGnBu") +
#   scale_y_continuous(labels=comma) +
#   theme_bw() +
#   labs(fill="seccion") +
#   xlab("seccion") +
#   ylab("longitud promedio") +
#   theme(axis.text.x=element_text(angle=90, hjust=1)) +
#   ggtitle("Longitud promedio por sección/modelo")

load("longitudes_docs.RData")


ggplot(longitudes_df, aes(x=modelo, y=mean_len, fill=modelo, 
                       label=format(mean_len, big.mark=","))) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(vjust=0, size=3) +
  facet_grid(seccion_new ~ .) +
  geom_hline(data=longitudes, aes(yintercept=mean_len), color="black", size=1) + 
  scale_fill_brewer(palette="YlGnBu") +
  scale_y_continuous(labels=comma, 
                     limits=c(0, max(longitudes_df$mean_len)*.10 + 
                                max(longitudes_df$mean_len))) +
  theme_bw() +
  labs(fill="seccion") +
  xlab("seccion") +
  ylab("longitud promedio") +
  #theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Longitud promedio por sección/modelo")
  
###correlacion 
load("~/Documents/itam/itam/tesis/first_terms.RData")

recomendaciones$seccion <- as.character(recomendaciones$seccion)
names(lon_bm25)[grep("mean_len", names(lon_bm25))] <- "lon_bm25"
names(lon_tfidf)[grep("mean_len", names(lon_tfidf))] <- "lon_tfidf"
names(lon_ninguno)[grep("mean_len", names(lon_ninguno))] <- "lon_ninguno"
prop_tags <- data.frame(seccion=c("estados-unidos","internacional","nacional","salud","tecnologia"),
                        prop_tags=c(47.61,47.61,47.61,1.01,2.99), 
                        stringsAsFactors = F)

cor_data <- inner_join(recomendaciones, lon_bm25, by=c("seccion"="seccion_new")) %>%
  inner_join(lon_tfidf, by=c("seccion"="seccion_new")) %>%
  inner_join(lon_ninguno, by=c("seccion"="seccion_new"))

M <- cor(cor_data[3:dim(cor_data)[2]])
corrplot.mixed(M, lower="number", upper="circle", diag='l', tl.pos="lt")

cor_data <- inner_join(recomendaciones, prop_tags)

M <- cor(cor_data[3:dim(cor_data)[2]])
corrplot.mixed(M, lower="number", upper="circle", diag='l', tl.pos="lt")


###top 10 tags por modelo
#bm25
top_10_bm25 <- group_by(docs_bm25, tag_tfidf) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(desc(prop)) %>%
  head(10)

top_10_tfidf <- group_by(docs_tfidf, tag_tfidf) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(desc(prop)) %>%
  head(10)

top_10_ninguno_bm25 <- group_by(docs_ninguno, tag_bm25) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100,2)) %>%
  arrange(desc(prop)) %>%
  head(10)

top_10_ninguno_tfidf <- group_by(docs_ninguno, tag_tfidf) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100,2)) %>%
  arrange(desc(prop)) %>%
  head(10)


top_10_ninguno_bm25_tfidf <- group_by(docs_ninguno, tag_tfidf, tag_bm25) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100,2)) %>%
  arrange(desc(prop)) %>%
  head(10)

least_10_bm25 <- group_by(docs_bm25, tag_bm25) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(prop) %>%
  head(10)

least_10_tfidf <- group_by(docs_tfidf, tag_tfidf) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(prop) %>%
  head(10)

least_10_bm25_ninguno <- group_by(docs_ninguno, tag_bm25) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100,2)) %>%
  arrange(prop) %>%
  head(10)

least_10_tfidf_ninguno <- group_by(docs_ninguno, tag_tfidf) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(prop) %>%
  head(10)

salud_ninguno <- docs_ninguno[grep("salud",docs_ninguno$tag_bm25),]
 
#### nivel de abstraccion por cada modelo 
bm25_na <- docs_bm25
bm25_na$nivel_abstraccion <- sapply(bm25_na$tag_bm25, function(x) 
  str_split(x, "\\.") %>% unlist() %>% length())

mean_nivel_abstraccion_seccion <- bm25_na %>% group_by(seccion_new) %>%
  summarise(mean_nivel_abstraccion=round(mean(nivel_abstraccion),2))



ggplot(mean_nivel_abstraccion_seccion, aes(x=seccion_new, y=mean_nivel_abstraccion, 
                                           label=mean_nivel_abstraccion, 
                                           fill=seccion_new)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  scale_fill_brewer(palette="Spectral", name="sección") +
  theme_bw() +
  xlab("sección") +
  ylab("nivel de abstracción promedio") +
  ggtitle("Nivel de abstracción promedio por sección - BM25") +
  theme(axis.text.x=element_text(angle=90)) 

##tfidf 
tfidf_na <- docs_tfidf
tfidf_na$nivel_abstraccion <- sapply(tfidf_na$tag_tfidf, function(x) 
  str_split(x, "\\.") %>% unlist() %>% length())

mean_nivel_abstraccion_seccion <- tfidf_na %>% group_by(seccion_new) %>%
  summarise(mean_nivel_abstraccion=round(mean(nivel_abstraccion),2))



ggplot(mean_nivel_abstraccion_seccion, aes(x=seccion_new, y=mean_nivel_abstraccion, 
                                           label=mean_nivel_abstraccion, 
                                           fill=seccion_new)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  scale_fill_brewer(palette="Spectral", name="sección") +
  theme_bw() +
  xlab("sección") +
  ylab("nivel de abstracción promedio") +
  ggtitle("Nivel de abstracción promedio por sección - TF/IDF") +
  theme(axis.text.x=element_text(angle=90)) 

##ninguno 
ninguno_na <- docs_ninguno
ninguno_na$nivel_abstraccion_bm25 <- sapply(ninguno_na$tag_bm25, function(x) 
  str_split(x, "\\.") %>% unlist() %>% length())
ninguno_na$nivel_abstraccion_tfidf <- sapply(ninguno_na$tag_tfidf, function(x) 
  str_split(x, "\\.") %>% unlist() %>% length())

mean_nivel_abstraccion_seccion <- ninguno_na %>% group_by(seccion_new) %>%
  summarise(bm25=round(mean(nivel_abstraccion_bm25),2),
            tfidf=round(mean(nivel_abstraccion_tfidf),2))


mean_niv_abs_seccion <- gather(mean_nivel_abstraccion_seccion, model, nivel_abstraccion, -seccion_new)


ggplot(mean_niv_abs_seccion, aes(x=seccion_new, y=nivel_abstraccion, 
                                           label=nivel_abstraccion, 
                                           fill=seccion_new)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  scale_fill_brewer(palette="Spectral", name="sección") +
  facet_grid(model~.) +
  theme_bw() +
  xlab("sección") +
  ylab("nivel de abstracción promedio") +
  ggtitle("Nivel de abstracción promedio por sección - Ninguno") +
  theme(axis.text.x=element_text(angle=90)) 


###documentos con misma seleccion
load("docs_formulario.RData")