library(dplyr)
library(antaresRead)
setSimulationPath()

areas <- readAntares() # equivalent to readAntares(areas="all")
links <- readAntares(links="all")
save(areas,links, file="TD_RTE.Rdata")

load("TD_RTE.Rdata")

base %>% select(`ma variable`)


# visualiser la base, en afficher les statistiques descriptives ====
View(areas)
summary(areas)

# afficher le nom des variables ====
names(areas)

# combien y a til de lignes ? =====
nrow(areas)
dim(areas)



# calculer pour chaque area le MRG price moyen ====
areas %>% 
  group_by(area) %>% 
  summarise(prix_moyen=mean(`MRG. PRICE`,na.rm=TRUE))


# calculer pour chaque area a chauqe tranche horainre le MRG price moyen ====

areas %>% 
  group_by(area,hour) %>%
  summarise(prix_moyen=mean(`MRG. PRICE`))



# calculer pour chaque area a chaque heure la moyenne de tous les maximum ====
areas %>% group_by(area,hour) %>% 
  summarise_each(funs(mean),ends_with("max"))

areas %>% group_by(area,hour) %>% 
  summarise_each(funs(mean),contains("max",ignore.case = TRUE))

areas %>% 
  group_by(area,hour) %>% 
  select(contains("max")) %>% 
  summarise_each(funs(mean))

# construire le tableau qui représente la moyenne des prix avec une ligne par ====
#area et une colonne par horaire ====

library(reshape2)
areas %>% 
  dcast(area ~ hour,value.var = "MRG. PRICE",fun.aggregate=mean)

areas %>% group_by(area,hour) %>% 
  summarise(MOY= mean(`MRG. PRICE`)) %>% 
  dcast(area~hour)

areas %>% group_by(area,hour) %>% 
  summarise(MOY= mean(`MRG. PRICE`)) %>% 
  spread(area,hour)# a verifier.


# ne selectionner que l'area a dans areas====
areas2 <- areas %>% filter(area=="a")
areas2b <- areas[area=="a",]



# supprimer la colonne area ====

areas3 <- areas2 %>% select(-area)

# ne garder que les colonnes de cout et de prix et d'emission de CO2 ====

areas4 <- areas3 %>% select(contains("COST",ignore.case=TRUE),
                            contains("PRICE",ignore.case=TRUE),`CO2 EMIS.`)

# rajouter une colonne qui correspon au ratio  de CO2 EMIS. par PRICE MRG.====
# Arrondir le résulat a 2 chiffres apres la virgule ====

areas5 <- areas4 %>% mutate(ratio =`CO2 EMIS.`/`MRG. PRICE`,
                            ratio=round(ratio,2))

# changer le nom de la colonne CO2 EMIS. par "emission_co2 ====
areas6 <-rename(areas5,emission_co2=`CO2 EMIS.`)

# exporter le resultat en CSV====

write.csv2(areas6,file="out.csv",row.names = FALSE)
browseURL("out.csv")

# tout faire en une seule instruction=====
areas %>% 
  filter(area=="a") %>% 
  select(-area) %>% 
  select(contains("COST",ignore.case=TRUE),
         contains("PRICE",ignore.case=TRUE),
         `CO2 EMIS.`) %>% 
  mutate(ratio =`CO2 EMIS.`/`MRG. PRICE`,
         ratio=round(ratio,2)) %>% 
  arrange(ratio) %>%
  rename(emission_co2 = `CO2 EMIS.`) %>% 
  write.csv2(file="out.csv",row.names=FALSE)

  browseURL("out.csv")

areas %>% group_by(area) %>% summarise(nombre=n())
areas %>% group_by(area) %>% tally()
areas %>% count(area)
areas %>% count()

# microbenchmark::microbenchmark(nrow(areas),count(areas))

areas %>% distinct(area,.keep_all = TRUE)
areas %>% distinct(area,.keep_all = FALSE)
areas %>% distinct(area)
areas
areas %>% sample_n(10)

base[1:70,]
areas %>% slice(1:10)

apprentissage <- areas %>% sample_frac(0.7)
areas %>% anti_join(apprentissage) -> validation

dim(apprentissage)+dim(validation)
dim(areas)

#areas %>% group_by(area) %>% mutate() %>% ungroup() %>% arrange





