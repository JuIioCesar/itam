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
a_4[which(a_4$node == "adidas" & a_4$parent == "marcas de moda y estilo de vida"),"node"] <- "adidas estilo vida"
tree_df <- rbind(tree_df, select(as.data.frame(a_4), node, parent, n))

a_5 <- group_by(tags_df, level1, level2, level3, level4, level5) %>%
  summarise(n=n())
a_5 <- a_5[-which(is.na(a_5$level5)),]
names(a_5)[grep("level5", names(a_5))] <- "node"
names(a_5)[grep("level4", names(a_5))] <- "parent"
tree_df <- rbind(tree_df, select(as.data.frame(a_5), node, parent, n))

a_6 <- group_by(tags_df, level1, level2, level3, level4, level5,
                level6) %>%
  summarise(n=n())
a_6 <- a_6[-which(is.na(a_6$level6)),]
names(a_6)[grep("level6", names(a_6))] <- "node"
names(a_6)[grep("level5", names(a_6))] <- "parent"
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


p1 <- gvisTreeMap(tree_df_unique,  idvar="node", parentvar="parent",
                    sizevar="n", colorvar="n")
plot(p1)



