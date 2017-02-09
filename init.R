library(dplyr)
library(antaresRead)

### chargement des données depuis une étude ANTARES==== 
# Cette étape peut ne pas être présentée
setSimulationPath()
areas <- readAntares() # equivalent to readAntares(areas="all")
links <- readAntares(links="all")
save(areas,links, file="TD_RTE.Rdata")


### chargement des données depuis un Rdata ==== 
load("TD_RTE.Rdata")

# visualiser la base, en afficher les statistiques descriptives ====
View(areas)
summary(areas)

# afficher le nom des variables ====
names(areas)

# combien y a til de lignes ? =====
nrow(areas)
dim(areas)
areas[,.N]



# calculer pour chaque area le MRG price moyen ====
areas %>% group_by(area) %>% summarise(prix_moyen=mean(`MRG. PRICE`))

areas[,mean(`MRG. PRICE`),by=area]
areas[,.(prix_moyen = mean(`MRG. PRICE`)),by=area]


# calculer pour chaque area a chaque tranche horaire le MRG prix moyen ====

areas %>% group_by(area,hour) %>% summarise(prix_moyen=mean(`MRG. PRICE`))
areas[,mean(`MRG. PRICE`),by=.(area,hour)]
areas[,.(prix_moyen = mean(`MRG. PRICE`)),by=.(area,hour)]


# calculer pour chaque area a chaque heure la moyenne de tous les maximums (exemple de maximum : BALANCE_max) ====
areas %>% group_by(area,hour) %>% summarise_each(funs(mean),ends_with("max"))
areas %>% group_by(area,hour) %>% summarise_each(funs(mean),contains("max"))
areas %>% group_by(area,hour) %>% 
  select(contains("max")) %>% summarise_each(funs(mean))

areas[,lapply(.SD,mean),
      .SDcols= names(areas) %like% "max",
      by=.(area,hour)]

areas[,lapply(.SD,mean),
      .SDcols= select_vars(names(areas), contains("max")),
      by=.(area,hour)]


areas[,c("area","hour",select_vars(names(areas), contains("max"))),with=FALSE][,lapply(.SD,mean),by=.(area,hour)]
areas[,c("area","hour",select_vars(names(areas), contains("max"))),with=TRUE]

## effectuer un benchemark sur les traitements précédents
microbenchmark::microbenchmark(areas[,lapply(.SD,mean),
                                     .SDcols= select_vars(names(areas), contains("max")),
                                     by=.(area,hour)],areas[,c("area","hour",select_vars(names(areas), contains("max"))),with=FALSE][,lapply(.SD,mean),by=.(area,hour)]
)

names(areas)

areas[,.(NODU)]
areas[,c(a="NODU",b="NODU_std"),with=TRUE]
# construire le tableau qui reprÃ©sente la moyenne des prix avec une ligne par ====
#area et une colonne par horaire ====

library(reshape2)
areas %>% 
  dcast(area ~ hour,value.var = "MRG. PRICE",fun.aggregate=mean)

areas %>% group_by(area,hour) %>% 
  summarise(MOY= mean(`MRG. PRICE`)) %>% 
  dcast(area~hour)


# ne selectionner que l'area a dans areas====
areas2 <- areas %>% filter(area=="a")

areas2b <- areas[area=="a",]


# supprimer la colonne area ====

areas3 <- areas2 %>% select(-area)
areas3b <- areas2b[,area:=NULL]  #la colonne area est aussi supprimé dans areas2b

##à voir si vous voulez aborder ce point ici
is.null(areas3b$area)  #TRUE
is.null(areas2b$area)  #TRUE

areas2b <- areas[area=="a",]
areas4b <- copy(areas2b)
areas4b<-areas4b[,area:=NULL]

is.null(areas4b$area)  #TRUE
is.null(areas2b$area)  #FALSE

# ne garder que les colonnes de consommation et d'éolien ====

areas4 <- areas3 %>% select(contains("LOAD"),contains("WIND"), `OV. COST`)

areas4b <- areas3b[,
                   select_vars(names(areas3b),contains("LOAD"),contains("WIND")),
                   with=FALSE]

# rajouter une colonne qui correspond au ratio de `WIND` par LOAD ====
# Arrondir le resulat a 2 chiffres apres la virgule ====

areas5 <- areas4 %>% mutate(ratio=WIND/LOAD,
                            ratio=round(ratio,2))
                            

areas4b[,ratio := round(WIND/LOAD,2)]
areas4b[,ratio := WIND/LOAD
        ][,ratio := round(ratio,2)]


# changer le nom de la colonne OV. COST par "cout_total ====
setnames(areas4,'OV. COST',"cout_total")


# exporter le resultat en CSV====

# tout faire en une seule instruction=====
areas %>% 
  filter(area=="a") %>% 
  select(-area) %>% 
  select(contains("LOAD"),contains("WIND"),`OV. COST`) %>% 
  mutate(ratio=WIND/LOAD,
        ratio=round(ratio,2)) %>% 
  arrange(ratio) %>%
  rename(cout_total = `OV. COST`) %>% 
  write.csv2(file="out.csv",row.names=FALSE)

browseURL("out.csv")

### Créer un package R contenant cette dernière instruction ==== 
## documenté (roxygen2) et testé (testhat)

areas[area=="a",
      select_vars(names(areas),contains("LOAD"),contains("WIND"),`OV. COST`),
      with=FALSE][,ratio := round(WIND/LOAD,2)][order(ratio)]

browseURL("out.csv")

### Créer un package R contenant cette dernière instruction ==== 
## documenté (roxygen2) et testé (testhat)


  
# changer le nom de la colonne OV. COST par "cout_total ====


# exporter le rÃ©sultat dans un fichier csv


# tout faire en une seule instruction

# exporter le resultat en CSV

# tout faire en une seule instruction


# et en data.table

areas %>% 
  filter(area=="a") %>% 
  select(-area) %>% 
  select(contains("LOAD"),contains("WIND"),`OV. COST`) %>% 
  mutate(ratio =WIND/LOAD,
         ratio=round(ratio,2)) %>% 
  arrange(ratio) %>%
  rename(cout_total = `OV. COST`) %>% 
  write.csv2(file="out.csv",row.names=FALSE)

browseURL("out.csv")

areas


### Créer un package R contenant cette dernière instruction ==== 
## documenté (roxygen2) et testé (testhat)

