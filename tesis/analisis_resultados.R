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

load("~/Documents/itam/itam/tesis/docs_formulario.RData")

docs_ninguno <- documentos_humanos[ninguno, ]

docs_nin <- docs_ninguno %>% group_by(seccion) %>%
  summarise(docs=n())

all_docs <- documentos_humanos %>% group_by(seccion) %>%
  summarise(docs=n())

df <- inner_join(all_docs, docs_nin, by=c("seccion"="seccion"))
names(df) <- c("seccion","all","ninguno")
df$prop <- round(df$ninguno/df$all*100,2)