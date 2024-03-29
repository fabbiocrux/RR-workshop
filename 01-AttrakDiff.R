# ================ AttrakDiff Script for data analysis ================
# Create a reproductible Script that enables the analysis of the AttrakDiff Methodology

# CONTENTS
#   1. Reading the Excel/CSV Data
#   2. Treating Excel/CSV Data
#   3. Making the Tables and the Graphics
#   4. Introduce the results in Rmarkdown table.


# KEY PACKAGES
## Install those now if you do not have the latest versions. ----
#install.packages("tidyverse")  # Tidyverse approach for Data science
#install.packages("readxl")


## Load the packages ----
library(tidyverse) # Data Science Tools
library(readxl)  # Read a Excel File


#  1. Reading the CSV Data ----
Attrakdiff <- read_csv('Groupe-C.csv')


# delete Test

Attrakdiff <- Attrakdiff[-c(2,3), ]

## Quantity of participants
total <- nrow(Attrakdiff)

## Obtaining the names of the columns
names(Attrakdiff)



#  2. Treating CSV Data ----
## 2.1 Change the Participants ----
Attrakdiff <- Attrakdiff %>% mutate( Participant = paste(LETTERS[1 : total])) # Approach 1
Attrakdiff$Participant <- paste(LETTERS[1 : total]) # Approach 2


## Pivot_longer() to arrange each variable
data <-
   Attrakdiff %>% pivot_longer(cols = c(QP1:ATT7),
                         names_to = "Variables",
                         values_to = "Answers")


## 2.2 Changing the scale of the answers ----
## See:https://carinelallemand.files.wordpress.com/2015/09/version-franc3a7aise-attrakdiff_lallemand_2015.pdf

### Approach 1 
data <-
   data %>% mutate(
      Final_value =
         case_when(
            Variables == "QP1"  ~ Answers*(-1),
            #Variables == "QP2"  ~ as.numeric(Answers*-1),
            #Variables == "QP3"  ~ as.numeric(Answers*-1),
            #Variables == "QP5"  ~ as.numeric(Answers*-1),
            #Variables == "ATT1"  ~ as.numeric(Answers*-1),
            #Variables == "ATT3"  ~ as.numeric(Answers*-1),
            #Variables == "ATT5"  ~ as.numeric(Answers*-1),
            #Variables == "ATT7"  ~ as.numeric(Answers*-1),
            #Variables == "QHS1"  ~ as.numeric(Answers*-1),
            #Variables == "QHS3"  ~ as.numeric(Answers*-1),
            #Variables == "QHS4"  ~ as.numeric(Answers*-1),
            #Variables == "QHS7"  ~ as.numeric(Answers*-1),
            #Variables == "QHS1"  ~ as.numeric(Answers*-1),
            #Variables == "QHS3"  ~ as.numeric(Answers*-1),
            #Variables == "QHS4"  ~ as.numeric(Answers*-1),
            #Variables == "QHS7"  ~ as.numeric(Answers*-1),
            TRUE ~ Answers
         )
      )




View(data)
## Inversing the Scale
toInvert <- c("QP1", "QP2", "QP3", "QP5", 
              "ATT1", "ATT3", "ATT5", "ATT7", 
              "QHS1", "QHS3", "QHS4", "QHS7", 
              "QHI2", "QHI3", "QHI6")


### Changing the sign of the answers : Approach Explicit
data <-
   data %>% mutate(
      Final_value =
         case_when(
            Variables %in% toInvert ~ as.numeric(Answers)*(-1),
            TRUE ~ Answers
         )
   )

#View(data)


## 2.3 Groups of the Dimmensions ----
data <-
   data %>% mutate(
      Factors =
         case_when(
            str_detect(Variables, "QP") ~ "Qualité Pragmatique (QP)",
            str_detect(Variables, "QHS") ~ "Qualité Hédonique - Stimulation (QH-S)",
            str_detect(Variables, "QHI") ~ "Qualité Hédonique - Identité (QH-I)",
            str_detect(Variables, "ATT") ~ "Attractivité Globale (ATT)",
            TRUE ~ "ATTENTION"
         ))





#  3. Graphic I ----
## 3.1 Calculating the mean values and the error
Table_I <-
   data %>%
   group_by(Factors) %>%
   summarise(Moyenne = mean(Final_value),
             Std = sd(Final_value),
             Se = Std / sqrt(length(Final_value)))
Table_I
# More Info: # https://www.r-graph-gallery.com/4-barplot-with-error-bar.html

Graph_I <-
   Table_I %>%
   ggplot() +
   aes(x= Factors, y=Moyenne , fill = Factors ) +
   geom_bar(stat = "identity") +
   geom_errorbar( aes(x=Factors, ymin = Moyenne - Se,
                      ymax = Moyenne + Se ),
                  width=0.1, colour="orange", alpha=0.9, size=0.5) +
   geom_point() +
   coord_flip() +
   scale_x_discrete( name = "AttrakDiff Resutls") +
   scale_y_continuous(limits = c(-3,3)) +
   labs(x = "",
        y = "Level ",
        title = "AtracDiff Profile for XXX",
        subtitle = paste("Total of answers:" , total)
   ) +
   theme_minimal(base_size = 12, base_family = "Palatino")



# Saving the File
#ggsave("Figures/AttrakDiff-GroupeC.jpg", width = 10, height = 5, dpi="print" )


## 4. Graphic II ----

Table_II <- 
   data %>% 
   group_by( Variables ) %>%
   summarise( Moyenne = mean(Final_value))


Graph_II <-
   Table_II %>%
   ggplot() + 
   aes(x = Variables, y=Moyenne, group =1) + 
   geom_line( color="grey" ) +
   geom_point() +
   scale_y_continuous(name="Moyenne", breaks=seq(-3,3,1), limits=c(-3, 3))+
   scale_x_discrete(name="Items", 
                    labels=c("ATT1" = "Plaisant - Déplaisant", 
                             "ATT2" = "Laid - Beau", 
                             "ATT3" = "Agréable - Désagréable", 
                             "ATT4" = "Rebutant - Attirant", 
                             "ATT5" = "Bon - Mauvais", 
                             "ATT6" = "Repoussant - Attrayant", 
                             "ATT7" = "Motivant - Décourageant", 
                             "QHI1" = "M'isole - Me socialise", 
                             "QHI2" = "Professionel - Amateur", 
                             "QHI3" = "De bon goût - De mauvais goût", 
                             "QHI4" = "Bas de gamme - Haut de gamme", 
                             "QHI5" = "M'exclut - M'intègre", 
                             "QHI6" = "Me rapproche des autres - Me sépare des autres",
                             "QHI7" = "Non présenatble - Présentable", 
                             "QHS1" = "Original - Conventionnel", 
                             "QHS2" = "Sans imagination - Créatif" , 
                             "QHS3" = "Audacieux - Prudent", 
                             "QHS4" ="Novateur - Conservateur", 
                             "QHS5" = "Ennuyeux - Captivant", 
                             "QHS6" = "Peu exigeant - Challeging", 
                             "QHS7" = "Nouveau - Commun" ,
                             "QP1" = "Humain - Technique", 
                             "QP2"= "Simple - Compliqué", 
                             "QP3" = "Pratique - Pas pratique", 
                             "QP4" = "Fastidieux - Efficace", 
                             "QP5" = "Prévisible - Imprévisible", 
                             "QP6" = "Confus - Clair", 
                             "QP7" = "Incontrôlable - Maîtrisable"))+
   coord_flip() +
   annotate("rect", xmin=c(1,8,15,22), xmax=c(7,14,21,28),
            ymin=rep(-3,4), ymax=rep(3, 4),
            alpha = .1 , fill = c("blue", "red", "grey","green")) +
   ggplot2::annotate("text",
                     y = c(2, 2, 2, 2),
                     x = c(4, 11, 19, 26),
                     label = c("Attractivité \n globale",
                               "Qualité \n hédonique - identification",
                               "Qualité \n hédonique - stimulation",
                               "Qualité \n pragmatique"),
                     family = "Palatino", fontface = 3, size=3) +
   theme_minimal(base_size = 12, base_family = "Palatino") +
   labs(x = "",
        y = "Level ",
        title = "AtrakDiff Profile",
        subtitle = paste("Total of answers:" , total ) ) +
   theme(
      legend.position = "right",
      panel.border = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 18, family = "Palatino")
   )

# Saving the File
#ggsave("Figures/AttrakDiff-2-GroupeC.jpg", width = 5, height = 7, dpi="print" )



# 5. Graphic III ----
QH <-
   data %>%
   filter(Factors == "Qualité Hédonique - Stimulation (QH-S)" | 
             Factors == "Qualité Hédonique - Identité (QH-I)") %>%
   summarise(QH = mean(Final_value),
             QH_sd = sd(Final_value),
             QH_IC_min = t.test(Final_value)$conf.int[1], # see https://larmarange.github.io/analyse-R/intervalles-de-confiance.html
             QH_IC_max = t.test(Final_value)$conf.int[2]
   )
   
QP <-
   data %>%
   filter(Factors == "Qualité Pragmatique (QP)") %>%
   summarise(QP = mean(Final_value),
             QP_sd = sd(Final_value),
             QP_IC_min = t.test(Final_value)$conf.int[1], # see https://larmarange.github.io/analyse-R/intervalles-de-confiance.html
             QP_IC_max = t.test(Final_value)$conf.int[2]
   )


Table_III <- tibble(QH, QP)
names(Table_III)


Graph_III <-
   
   Table_III %>%
   ggplot() +
   aes(x=QP, y=QH) +
   geom_point()+
   ylim(-3,3)+ xlim(-3,3) +
   geom_hline(yintercept=c(-1,1))+
   geom_vline(xintercept=c(-1,1)) +
   annotate("rect", xmin = Table_III$QP_IC_min, xmax = Table_III$QP_IC_max,
            ymin = Table_III$QH_IC_min, ymax = Table_III$QH_IC_max,
            alpha = .5 , fill = c("blue")) +
   ggplot2::annotate("rect", xmin=c(-1), xmax=c(1),
                     ymin=c(-1), ymax=c(1),
                     alpha = .1 , fill = c("#009999")) +
   ggplot2::annotate("text",
                     y = c(0.5),
                     x = c(0),
                     label = c("Neutre"),
                     family = "Palatino", fontface = 3, size=4) +
   labs(title = "Global AttrakDiff ",
        subtitle = paste("Total of answers:" , total),
        x = "Qualité Pragmatique",
        y = "Qualité Hedonique ") +
   theme_minimal(base_size = 10, base_family = "Palatino")


#ggsave("Figures/AttrakDiff-2-Groupe-C.jpg", width = 5, height = 7, dpi="print" )


# Total of the Results ----
Results <- list(Table_1 = Table_I, 
                Table_2 = Table_II, 
                Table_3= Table_III,
                Fig_1 = Graph_I, 
                Fig_2 = Graph_II, 
                Fig_3 = Graph_III)



# Remove all data
objects <- ls() 
rm(list =  objects[ !objects == 'Results'] )


#  Reading the Excel for the Parameters  ----
# Identify the onglets
onglets <- excel_sheets("data/Attrakdiff.xlsx")

# Reading all table
Parameters_AttrakDiff <- 
   read_excel(path = "data/Attrakdiff.xlsx", sheet = "Parameters" )

# Reading all table but considering as title the second row
data <- read_excel(path = "data/Data.xlsx" , skip = 1)

# Obtaining the names of the columns
names(data)





# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
   name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
   value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
data %>%
   ggplot( aes(x=name, y=value, fill=name)) +
   geom_boxplot() +
   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
   geom_jitter(color="black", size=0.4, alpha=0.9) +
   theme_ipsum() +
   theme(
      legend.position="none",
      plot.title = element_text(size=11)
   ) +
   ggtitle("A boxplot with jitter") +
   xlab("")

