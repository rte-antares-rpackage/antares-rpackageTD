library(dplyr)
library(antaresRead)
setSimulationPath()

areas <- readAntares() # equivalent to readAntares(areas="all")
links <- readAntares(links="all")
save(areas,links, file="TD_RTE.Rdata")

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


# calculer pour chaque area a chauqe tranche horainre le MRG price moyen ====

areas %>% group_by(area,hour) %>% summarise(prix_moyen=mean(`MRG. PRICE`))
areas[,mean(`MRG. PRICE`),by=.(area,hour)]
areas[,.(prix_moyen = mean(`MRG. PRICE`)),by=.(area,hour)]



# calculer pour chaque area a chaque heure la moyenne de tous les maximum ====
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
microbenchmark::microbenchmark(areas[,lapply(.SD,mean),
                                     .SDcols= select_vars(names(areas), contains("max")),
                                     by=.(area,hour)],areas[,c("area","hour",select_vars(names(areas), contains("max"))),with=FALSE][,lapply(.SD,mean),by=.(area,hour)]
)

names(areas)

areas[,.(NODU)]
areas[,c(a="NODU",b="NODU_std"),with=TRUE]
# construire le tableau qui représente la moyenne des prix avec une ligne par ====
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
areas3b <- areas2b[,area:=NULL]

# ne garder que les colonnes de cout et de prix et d'emission de CO2 ====

areas4 <- areas3 %>% select(contains("COST"),contains("PRICE"),`CO2 EMIS.`)

areas4b <- areas3b[,
                   select_vars(names(areas3b),contains("COST"),contains("PRICE"),`CO2 EMIS.`),
                   with=FALSE]

# rajouter une colonne qui correspon au ratio  de CO2 EMIS. par PRICE MRG.====
# Arrondir le résulat a 2 chiffres apres la virgule ====

areas5 <- areas4 %>% mutate(ratio =`CO2 EMIS.`/`MRG. PRICE`,
                            ratio=round(ratio,2))
                            

areas4b[,ratio := round(`CO2 EMIS.`/`MRG. PRICE`,2)]
areas4b[,ratio := `CO2 EMIS.`/`MRG. PRICE`
        ][,ratio := round(ratio,2)]


# changer le nom de la colonne CO2 EMIS. par "emission_co2 ====
setnames(areas4b,'CO2 EMIS.',"emission_co2")


# exporter le resultat en CSV====

# tout faire en une seule instruction=====
areas %>% 
  filter(area=="a") %>% 
  select(-area) %>% 
  select(contains("COST"),contains("PRICE"),`CO2 EMIS.`) %>% 
  mutate(ratio =`CO2 EMIS.`/`MRG. PRICE`,
        ratio=round(ratio,2)) %>% 
  arrange(ratio) %>%
  rename(emission_co2 = `CO2 EMIS.`) %>% 
  write.csv2(file="out.csv",row.names=FALSE)


areas[area=="a",
      select_vars(names(areas),contains("COST"),contains("PRICE"),`CO2 EMIS.`),
      with=FALSE][,ratio := round(`CO2 EMIS.`/`MRG. PRICE`,2)][order(ratio)]



browseURL("out.csv")
  
# changer le nom de la colonne CO2 EMIS. par "emission_co2 ===


# exporter le résultat dans un fichier csv


# tout faire en une seule instruction

# exporter le resultat en CSV

# tout faire en une seule instruction


# et en data.table

areas %>% 
  filter(area=="a") %>% 
  select(-area) %>% 
  select(contains("COST"),contains("PRICE"),`CO2 EMIS.`) %>% 
  mutate(ratio =`CO2 EMIS.`/`MRG. PRICE`,
         ratio=round(ratio,2)) %>% 
  arrange(ratio) %>%
  rename(emission_co2 = `CO2 EMIS.`) %>% 
  write.csv2(file="out.csv",row.names=FALSE)

areas


