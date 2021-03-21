# Duomenų importavimas

library(readr)
library(tidyverse)
library(ggplot2)

duomenys <- read_csv("data/lab_sodra.csv")
duomenys <- as.data.frame(duomenys)
summary(duomenys)

# 1 užduotis

subset_duomenys <- duomenys %>%
  filter(ecoActCode == 451100) 

h <- subset_duomenys %>%
  ggplot(aes(x = avgWage)) +
  geom_histogram(bins = 50, fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Vidutinio atlyginimo pasiskirstymas") +
  theme_minimal() +
  theme(plot.title = element_text(size=11)) 

ggsave("1 uzduotis.png")

# 2 užduotis

top_imones <- subset_duomenys %>%
  select(name, avgWage) %>%
  group_by(name) %>%
  summarise(avgWage = mean(avgWage)) %>%
  arrange(desc(avgWage)) %>%
  top_n(5) 

# Tam, kad būtų paprasčiau atvaizduoti 

subset_duomenys <- subset_duomenys %>%
  mutate(name=replace(name, name == "BENDRA LIETUVOS-ČEKIJOS ĮMONĖ UŽDAROJI AKCINĖ BENDROVĖ \"ARX\"", "UAB \"ARX\"")) %>%
  as.data.frame()


#reiktu sugalvoti, kaip pakeisti i data
subset_duomenys$month <- as.character(subset_duomenys$month)

p <- subset_duomenys %>%
  filter(name %in% c("UAB BALTIJOS AUTONUOMA", 
         "UAB \"ARX\"", "UAB PRIME AUTO",
         "UAB AUTOVISTA", "UAB TOKVILA")) %>%
  ggplot(aes(x = month, y = avgWage, group = name, colour = name)) +
  geom_line() +
  geom_point() +
  ggtitle("Top 5 įmonių atlyginimo kitimo dinamika") +
  theme_minimal() +
  ylim(0, 5000)

p + theme(
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 7),
  legend.position ="bottom"
)

ggsave("2 uzduotis.png")

#library(lubridate)
#duomenys$month <- as.month(duomenys$month, format = "%Y%m")
#class(duomenys$month)
#str(duomenys)
# year month day without spaces
#ugh <- c("20180205", "20170303")
#as.Date(ugh, format = "%Y%m%d")
## [1] "2018-02-05" "2017-03-03"
#as_date(x, origin = lubridate::origin)
#newestMoH$awarded.issued <- as.Date(newestMoH$awarded.issued, format = "%m/%d/%Y")
# Kintamojo sukurimas aw.day:
# newestMoH$aw.day <- as.numeric(format(newestMoH$awarded.issued,"%d")) 01/04/1945

# 3 užduotis

apdraustieji <- subset_duomenys %>%
  select(name, ecoActCode, numInsured) %>%
  group_by(name) %>%
  filter(name %in% c("UAB BALTIJOS AUTONUOMA", "UAB \"ARX\"", "UAB PRIME AUTO",
                     "UAB AUTOVISTA", "UAB TOKVILA")) %>%
  summarise(MaxnumInsured  = max(numInsured)) %>%
  arrange(desc(MaxnumInsured)) %>%
  as.data.frame()

m <- ggplot(apdraustieji, aes(x=reorder(name, -MaxnumInsured), y=MaxnumInsured, fill=name)) +
  geom_bar(stat="identity") +
  ggtitle("Maksimalus apdraustų darbuotojų skaičius") +
  theme_minimal() +
  xlab("") +
  scale_fill_brewer(palette = "Dark2", breaks=c("UAB TOKVILA", "UAB PRIME AUTO", "UAB \"ARX\"",
                                                "UAB BALTIJOS AUTONUOMA", "UAB AUTOVISTA")) 
 

m + theme(
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 6.5),
  legend.position =c(0.9, 0.9)
)
  
 
ggsave("3 uzduotis.png") 

write.csv(subset_duomenys, 'sodra_subset.csv')
