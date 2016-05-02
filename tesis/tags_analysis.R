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


##### visualization
library(googleVis)

algo <- sapply(tags.df$tags, function(x) 
 str_split(x, "\\.")
) 

tags_df <- data.frame(level1=character(), level2=character(), 
                      level3=character(), level4=character(),
                      level5=character(), level6=character(),
                      level7=character(), level8=character())

for(i in 1:length(algo)){
  b <- algo[[i]]
  row <- character()
  for (j in 1:8){
    row <- cbind(row, b[j])
  }
  tags_df <- rbind(tags_df, row)
}


tags_df <- as.data.frame(tags_df)
names(tags_df) <- c("level1","level2","level3","level4","level5","level6",
                    "level7","level8")

a_0 <- c("root",NA,19)

a_1 <- group_by(tags_df, level1) %>%
  summarise(n=n())
names(a_1) <- c("node","n")
a_1$parent <- "root"

tree_df <- select(a_1, node, parent, n)

a_2 <- group_by(tags_df, level1, level2) %>%
  summarise(n=n())
a_2 <- a_2[-which(is.na(a_2$level2)),]
names(a_2) <- c("parent","node","n")
a_2$node <- as.character(a_2$node)
a_2[which(a_2$node == "union europea" &
            a_2$parent == "organizacion"), "node"] <- "union europea organizacion"
tree_df <- rbind(tree_df, select(as.data.frame(a_2), node, parent, n))

a_3 <- group_by(tags_df, level1, level2, level3) %>%
  summarise(n=n())
a_3 <- a_3[-which(is.na(a_3$level3)),]
names(a_3)[grep("level3", names(a_3))] <- "node"
names(a_3)[grep("level2", names(a_3))] <- "parent"
a_3$node <- as.character(a_3$node)
a_3$node[which(a_3$parent == "valores sociales" & a_3$node == "corrupcion")] <- "corrupcion social"
tree_df <- rbind(tree_df, select(as.data.frame(a_3), node, parent, n))


a_4 <- group_by(tags_df, level1, level2, level3, level4) %>%
  summarise(n=n())
a_4 <- a_4[-which(is.na(a_4$level4)),]
names(a_4)[grep("level4", names(a_4))] <- "node"
names(a_4)[grep("level3", names(a_4))] <- "parent"
a_4$node <- as.character(a_4$node)
a_4[which(a_4$node == "adidas" & 
            a_4$parent == "marcas de moda y estilo de vida"),"node"] <- "adidas estilo vida"
a_4[which(a_4$node == "australia" & a_4$parent == "paises australianos"), "node"] <- "australia pais"
a_4[which(a_4$node == "contabilidad y auditoria" & 
            a_4$parent == "servicios financieros y comerciales"), "node"] <- "contabilidad y auditoria servicios"
a_4[which(a_4$node == "internet" & 
            a_4$parent == "software e internet") , "node"] <- "internet software"
a_4[which(a_4$node == "navidad" & 
            a_4$parent == "festividades"), "node"] <- "navidad festividad"
a_4[which(a_4$node == "nike" & 
            a_4$parent == "marcas de moda y estilo de vida"), "node"] <- "nike marca"
a_4[which(a_4$node == "oceanografia" & 
            a_4$parent == "empresas"), "node"] <- "oceanografia empresa"
a_4[which(a_4$node == "software" &
            a_4$parent == "computacion e informatica"), "node"] <- "software computacion"
a_4[which(a_4$node == "alexander mcqueen" &
            a_4$parent == "marcas de moda y estilo de vida"), "node"] <- "alexander mcqueen moda"
a_4$node[grep("cafe", a_4$node)] <- "cafe ocio"
tree_df <- rbind(tree_df, select(as.data.frame(a_4), node, parent, n))


a_5 <- group_by(tags_df, level1, level2, level3, level4, level5) %>%
  summarise(n=n())
a_5 <- a_5[-which(is.na(a_5$level5)),]
names(a_5)[grep("level5", names(a_5))] <- "node"
names(a_5)[grep("level4", names(a_5))] <- "parent"
a_5$node <- as.character(a_5$node)
a_5[which(a_5$node == "bellas artes" &
            a_5$parent == "otros"), "node"] <- "bellas artes sitio"
a_5[grep("bicicletas", a_5$node), "node"] <- "bicicletas chilango"
a_5$node[grep("calvin klein", a_5$node)] <- "calvin klein disenador"
a_5$node[grep("carolina herrera", a_5$node)] <- "carolina herrera disenador"
a_5$node[grep("christian louboutin", a_5$node)] <- "christian louboutin disenador"
a_5$node[grep("elie saab", a_5$node)] <- "elie saab disenador"
a_5$node[grep("giorgio armani", a_5$node)] <- "giorgio armani disenador"
a_5$node[grep("marc jacobs", a_5$node)] <- "marc jacobs disenador"
to_change <- a_5[grep("estados", a_5$node), c("parent","node")]
to_change$aux <- paste(to_change$node, to_change$parent, sep=" ")
a_5$node[grep("estados", a_5$node)] <- to_change$aux
a_5[which(a_5$node == "granada" &
            a_5$parent == "zonas del df"), "node"] <- "granada zona df"
a_5$node[grep("oscar de la renta", a_5$node)] <- "oscar de la renta disenador"
a_5$node[grep("ralph lauren", a_5$node)] <- "ralph lauren disenador"
a_5$node[grep("roberto cavalli", a_5$node)] <- "roberto cavalli disenador"
a_5$node[grep("stella mccartney", a_5$node)] <- "stella mccartney disenador"
a_5$node[grep("tom ford", a_5$node)] <- "tom ford disenador"
to_change <- a_5[grep("regiones", a_5$node), c("parent","node")]
to_change$aux <- paste(to_change$node, to_change$parent, sep=" ")
a_5$node[grep("regiones", a_5$node)] <- to_change$aux
to_change <- a_5[grep("aplicaciones y servicios de internet", a_5$parent), "node"]
to_change$aux <- paste(to_change$node, "servicio", sep=" ")
a_5$node[grep("aplicaciones y servicios de internet", a_5$parent)] <- to_change$aux
a_5$node[grep("tommy hilfiger", a_5$node)] <- "tommy hilfiger actor"
a_5[which(a_5$node == "uber" &
            a_5$parent == "transporte chilango"), "node"] <- "uber transporte"
a_5[which(a_5$node == "valentino" &
            a_5$parent == "cantantes"), "node"] <- "valentino cantantes"
a_5$node[grep("yves saint laurent", a_5$node)] <- "yves saint laurent actriz"
tree_df <- rbind(tree_df, select(as.data.frame(a_5), node, parent, n))


a_6 <- group_by(tags_df, level1, level2, level3, level4, level5,
                level6) %>%
  summarise(n=n())
a_6 <- a_6[-which(is.na(a_6$level6)),]
names(a_6)[grep("level6", names(a_6))] <- "node"
names(a_6)[grep("level5", names(a_6))] <- "parent"
a_6$node <- as.character(a_6$node)
a_6$node[grep("florida", a_6$node)] <- "florida estado"
a_6$node[grep("georgia", a_6$node)] <- "georgia estado"
a_6$node[grep("luxemburgo", a_6$node)] <- "luxemburgo estado"
a_6[which(a_6$node == "monaco" &
            a_6$parent == "estados"),"node"] <- "monaco estado"
a_6$node[grep("singapur", a_6$node)] <- "singapur estado"
a_6$node[grep("tunez", a_6$node)] <- "tunez estado"
a_6$node[grep("yibuti", a_6$node)] <- "yibuti estado"
tree_df <- rbind(tree_df, select(as.data.frame(a_6), node, parent, n))



a_7 <- group_by(tags_df, level1, level2, level3, level4, level5,
                level6, level7) %>%
  summarise(n=n())
a_7 <- a_7[-which(is.na(a_7$level7)),]
names(a_7)[grep("level7", names(a_7))] <- "node"
names(a_7)[grep("level6", names(a_7))] <- "parent"
tree_df <- rbind(tree_df, select(as.data.frame(a_7), node, parent, n))

a_8 <- group_by(tags_df, level1, level2, level3, level4, level5,
                level6, level7, level8) %>%
  summarise(n=n())
a_8 <- a_8[-which(is.na(a_8$level8)),]
names(a_8)[grep("level8", names(a_8))] <- "node"
names(a_8)[grep("level7", names(a_8))] <- "parent"
tree_df <- rbind(tree_df, select(as.data.frame(a_8), node, parent, n))

tree_df$node <- as.character(tree_df$node)
tree_df <- rbind(tree_df, a_0)

tree_df_unique <- group_by(tree_df, node, parent) %>%
  summarise(n=sum(as.integer(n))) 
tree_df_unique$color <- sample(1:100, dim(tree_df_unique)[1], replace=T)

#test <- group_by(tree_df_unique, node) %>% summarise(n=n())
#filter(test, n > 3)

p1 <- gvisTreeMap(tree_df_unique,  idvar="node", parentvar="parent",
                    sizevar="n", colorvar="color")
plot(p1)





