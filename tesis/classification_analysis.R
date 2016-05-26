library(dplyr)

my_db <- src_postgres(dbname="itamtesis", host="localhost", user="liliana.millan")
clasificaciones <- tbl(my_db, "clasificaciones")

clasificaciones_df <- collect(clasificaciones)

clasificaciones <- distinct(clasificaciones_df, content)

secciones <- group_by(clasificaciones, seccion) %>%
  summarise(n=n()) %>% arrange(desc(n))

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


http://www.nytimes.com/es/2016/03/03/la-presion-para-que-renuncie-nicolas-maduro/