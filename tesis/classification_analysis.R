library(dplyr)
library(ggplot2)

my_db <- src_postgres(dbname="itamtesis", host="localhost", user="Lili")
clasificaciones <- tbl(my_db, "clasificaciones")

clasificaciones_df <- collect(clasificaciones)

clasificaciones <- group_by(clasificaciones_df, content) %>%
  summarise(bm25=max(bm25, na.rm=T),
            tfidf=max(tfidf, na.rm=T))

s <- inner_join(clasificaciones, clasificaciones_df, by=c("content"="content",
                                                          "bm25"="bm25")) 
t <- distinct(s, content, bm25, .keep_all = T) 
t <- select(t, content, bm25, tfidf.x, tag_bm25, tag_tfidf, seccion, url)
names(t)[grep("^tfidf", names(t))] <- "tfidf" 
clasificaciones <- t


##modificar las de america latina que tienen mexico a editorial nyt
america_latina <- filter(clasificaciones, seccion=='america-latina') 
america_latina[grep("mexico", america_latina$url),"seccion"] <- "editorial"


###modificar las de editorial que son de america-latina y otros
editorial <- filter(clasificaciones, seccion=="editorial")
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/03/03/la-presion-para-que-renuncie-nicolas-maduro/",
                             clasificaciones$url)] <- "america-latina"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/02/19/obama-debe-promover-la-democracia-en-su-visita-a-cuba/",
                             clasificaciones$url)] <- "america-latina"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/03/12/los-ninos-inmigrantes-merecen-una-voz-en-los-tribunales-de-estados-unidos/",
                             clasificaciones$url)] <- "estados-unidos"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/04/19/dilma-rousseff-ya-no-puede-esquivar-la-corrupcion-que-acecha-a-su-presidencia/", 
                             clasificaciones$url)] <- "america-latina"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/05/01/el-laberinto-politico-de-espana/", 
                             clasificaciones$url)] <- "europa"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/04/25/es-hora-de-replantear-la-guerra-contra-las-drogas/",
                             clasificaciones$url)] <- "america-latina"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/04/14/europa-debe-solucionar-sus-problemas-de-seguridad-de-manera-urgente/",
                             clasificaciones$url)] <- "europa"
clasificaciones$seccion[grep("http://www.nytimes.com/es/2016/01/25/editorial-las-naciones-unidas-pueden-sellar-la-paz-en-colombia/",
                             clasificaciones$url)] <- "america-latina"


secciones <- group_by(clasificaciones, seccion) %>%
  summarise(n=n()) %>% arrange(desc(n)) %>%
  mutate(prop=round(n/sum(n)*100,2))
secciones$seccion <- factor(secciones$seccion, levels=secciones$seccion)


ggplot(secciones, aes(x=seccion, y=prop, fill=seccion)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_fill_brewer(palette="Spectral") +
  xlab("sección") +
  ylab("%") +
  ggtitle("% secciones en el contenido clasificado")


clasificaciones$seccion_new <- ifelse(clasificaciones$seccion == "editorial", "nacional",
                                      ifelse(clasificaciones$seccion =="europa","internacional",
                                             ifelse(clasificaciones$seccion == "america-latina","internacional",
                                                    ifelse(clasificaciones$seccion == "financial_times",
                                                           "internacional",
                                                           ifelse(clasificaciones$seccion == "tech",
                                                                  "tecnologia", 
                                                                  ifelse(clasificaciones$seccion == "mundo",
                                                                         "internacional", 
                                                                         clasificaciones$seccion))))))

secciones_nuevas <- group_by(clasificaciones, seccion_new) %>%
  summarise(n=n()) %>%
  mutate(prop=round(n/sum(n)*100, 2)) %>%
  arrange(desc(prop))
secciones_nuevas$seccion_new <- factor(secciones_nuevas$seccion_new, 
                                       levels=secciones_nuevas$seccion_new)

ggplot(secciones_nuevas, aes(x=seccion_new, y=prop, fill=seccion_new,
                             label=n)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0, size=3) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  scale_fill_brewer(palette="Spectral") +
  xlab("sección") +
  ylab("%") +
  scale_y_continuous(limits=c(0,50)) +
  ggtitle("% contenidos por sección")

###documentos clasificados igual
equal_tags <- filter(clasificaciones, tag_bm25==tag_tfidf)
equal_tags_agg <- group_by(equal_tags, seccion_new) %>%
  summarise(n=n()) 

secciones_nuevas_copy <- secciones_nuevas
secciones_nuevas_copy$seccion_new <- as.character(secciones_nuevas_copy$seccion_new)

totals_secciones <- inner_join(equal_tags_agg, secciones_nuevas_copy, 
                               by="seccion_new")
names(totals_secciones) <- c("seccion_new","iguales","totales","prop_totales")
totals_secciones$prop_iguales <- round(totals_secciones$iguales/
                                         totals_secciones$totales*100, 2)


  

###get sample 
without_equals <- clasificaciones[which(!(clasificaciones$url %in% 
                                            equal_tags$url)),]
  
set.seed(54903)
aux <- filter(without_equals, seccion_new == "internacional")
#internacional_sample <- sample_n(aux, size=17, replace=F)
internacional_sample <- sample_n(aux, size=82, replace=F)

aux <- filter(without_equals, seccion_new == "nacional")
#nacional_sample <- sample_n(aux, size=14, replace=F)
nacional_sample <- sample_n(aux, size=67, replace=F)

aux <- filter(without_equals, seccion_new == "tecnologia")
#tec_sample <- sample_n(aux, size=7, replace=F)
tec_sample <- sample_n(aux, size=32, replace=F)


aux <- filter(without_equals, seccion_new == "estados-unidos")
#estados_unidos_sample <- sample_n(aux, size=3, replace=F)
estados_unidos_sample <- sample_n(aux, size=13, replace=F)

aux <- filter(without_equals, seccion_new == "salud")
#salud_sample <- sample_n(aux, size=2, replace=F)
salud_sample <- sample_n(aux, size=11, replace=F)


#documentos 
documentos_humanos <- internacional_sample
documentos_humanos <- rbind(documentos_humanos, nacional_sample)
documentos_humanos <- rbind(documentos_humanos, tec_sample)
documentos_humanos <- rbind(documentos_humanos, estados_unidos_sample)
documentos_humanos <- rbind(documentos_humanos, salud_sample)

save(documentos_humanos, file="docs_formulario.RData")


### promedio de longitud de documentos
len_docs <- sapply(documentos_humanos$content, function(x) str_split(x, " ") %>%
         unlist() %>% length())
names(len_docs) <- NULL
len_docs_df <- data.frame(len_doc=len_docs,
                       seccion=documentos_humanos$seccion_new)
len_docs_df$seccion <- as.character(len_docs_df$seccion)

mean_len_section <- group_by(len_docs_df, seccion) %>%
  summarise(mean_len= mean(len_doc)) %>%
  arrange(desc(mean_len))
mean_len_section$seccion <- factor(mean_len_section$seccion,
                                    levels=mean_len_section$seccion)

ggplot(mean_len_section, aes(x=seccion, y=mean_len, fill=seccion, 
                             label=format(round(mean_len, 2), big.mark=","))) +
  geom_bar(stat="identity") + 
  geom_text(vjust=0, size=4) +
  geom_hline(yintercept=mean(mean_len_section$mean_len), color="black", size=1) +
  scale_fill_brewer(palette="Spectral") +
  theme_bw() +
  labs(fill="Sección") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  ggtitle("Longitud promedio de documentos por sección")
