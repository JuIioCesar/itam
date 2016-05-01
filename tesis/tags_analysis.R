s <- sapply(tags.df$tags, function(x)  
  str_split(x,"\\.") %>% unlist() %>% length())

s_df <- as.data.frame(s) 
names(s_df)<- c("abstraction_level")

s_df_agg <- group_by(s_df, abstraction_level) %>%
  summarise(n=n()) %>%
  mutate(prop=round(prop.table(n)*100,2))


ggplot(s_df_agg, aes(x=abstraction_level, y=prop, 
                     label=prop)) +
  geom_bar(stat="identity") +
  geom_text(check_overlap = TRUE, vjust=0) +
  scale_fill_discrete(name="# of abstraction\nlevels") +
  theme_bw() +
  xlab("number of levels") +
  ylab("%") +
  ggtitle("Niveles de abstracción en etiquetas de Grupo Expansión")
  

mean(s_df$abstraction_level)



first_terms <- sapply(tags.df$tags, function(x)  
  unlist(str_split(x,"\\."))[1])

first_terms_df <- as.data.frame(first_terms)
names(first_terms_df) <- "first_terms"

first_terms_unique <- distinct(first_terms_df)
first_terms_unique <- first_terms_unique[with(first_terms_unique, order(first_terms)),]

first_terms_df_agg <- group_by(first_terms_df, first_terms) %>%
  summarise(n=n()) %>%
  mutate(prop=round(prop.table(n)*100,2))


ggplot(first_terms_df_agg, aes(x=first_terms, y=prop,
                               label=prop)) +
  geom_bar(stat="identity") +
  geom_text(vjust=0) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  guides(fill=F) +
  xlab("temas generales (1 nivel abstracción)")+
  ylab("%")+
  ggtitle("# etiquetas pertenecientes a cada tema general")

## second terms 
second_terms <- sapply(tags.df$tags, function(x)
  unlist(str_split(x,"\\."))[2])

second_terms_df <- as.data.frame(second_terms)
names(second_terms_df) <- "second_terms"

second_terms_s <- as.data.frame(second_terms_df[-which(is.na(second_terms_df$second_terms)),])

second_terms_unique <- distinct(second_terms_s)
