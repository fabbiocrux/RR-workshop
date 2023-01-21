# Loading the Package
library(tidyverse)
library(here)


## Data sur l'Energie: https://www.data.gouv.fr/fr/datasets/evolution-des-prix-domestiques-du-gaz-et-de-lelectricite/
# Estructure des données
# Année: annee[date]
# Semestre: semestre[text]
# U.E. – Gaz Naturel (€/MWh): u_e_gaz_naturel[int] €/MWh
# France – Gaz Naturel (€/MWh): france_gaz_naturel[int] €/MWh
# U.E. – Electricité (€/MWh): u_e_electricite[int] €/MWh
# France – Electricité (€/MWh): france_electricite[int] €/MWh


# load the data set
data <- read_csv2("data/2023/Motivation/evolution-des-prix-domestiques-du-gaz-et-de-lelectricite.csv")


## Comparaison de l'Electricité FR et EU
### Obtention des moyens par annee
Electricite <- 
   data %>% 
   group_by(annee) %>% summarise(Fr_elec_mean = mean(france_electricite),
                                 EU_elec_mean = mean(u_e_electricite))

### Preparation pour graphique

Electricite2 <- 
   Electricite %>% 
   pivot_longer(cols = Fr_elec_mean : EU_elec_mean)
   

# Visualitation des donnes

Electricite2 %>% 
ggplot( aes(x = annee,
            y = value,
            color = name)
        ) +
   geom_point() +
   geom_line() +
   #geom_bar(stat = "identity") +
   labs(
      title = "Comparaison de Coût Electricité",
      subtitle = "Le cas de la France et l'EU",
      x = "Anne",
      y = "Electricité (€/MWh)",
      caption = "Data Source: https://www.data.gouv.fr",
      #tag = "test"
      ) +
   theme_minimal()


ggsave("Prix Electricité.jpg", path = "Figures", width = 6, height = 4)

