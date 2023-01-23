#  Purpose of this tutorial
# Understand the Manipulation of Tables with Tidyverse tools

# - `select()` et `filter()`, qui permettent de selectionner des colonnes et des lignes d'un jeu de données
# - `arrange()`, qui vous permet de réordonner les lignes d'un jeu de données
# - `%>%`, qui organise votre code sous la forme de "pipes" faciles à déchiffer (un pipe peut être vu comme un flux dans lequel divers traitements sur les données sont enchaînés)
# - `mutate()`, `group_by()`, et `summarize()`, servent à calculer des statistiques descriptives


# Load the packages
library(tidyverse)

# Load the Data.
data <- read_csv2('data/taille.csv')
data

# Select function  ----
## Select the different columns of the data



## Exclusion of the columns


## Selection of range of columns


## Columns that contains a string


## Columns that starts with a string


## Columns that ends with a string



# Filter function ----
?filter

## Logical operators
#   == (Equal to)
#   != (Not equal to)
#   < (Less than)
#   <= (Less than or equal to)
#   > (Greater than)
#   >= (Greater than or equal to)

filter(data, nom == "Benjamin")
filter(data, nom != "Benjamin")

## Combination of logical tests

# - & (logical AND) --> Est-ce que les conditionsA et B sont toutes les deux vraies ?
# - | (logical OR) --> Est-ce que l’ une ou les deux conditions  A et B sont vraies ?
# - ! (logical NOT) --> Est-ce que A n’est pas vraie ?
# - %in%  --> Est-ce que x est dans l’ensemble a, b, et c ?

test <- filter(data, name == "Fabio" & year == 2018)


# Arrange function ----
?arrange

# Slice ----
slice(data, 1)



## Selection of Man,  taille > 170, 

test <- filter(data, taille > 160, sexe == "H")
test <- arrange(test, grade)
test <- slice(test, 1:3)

ggplot(test) +
   aes(x = nom, y = poids) +
   geom_bar(stat = 'identity')+
   coord_flip()

# Pipe operator ----
## Shorcut: Ctr/Cmd + Shift + M


data %>% 
   filter(taille > 160, sexe == "H") %>%
   arrange(desc(grade)) %>%
   slice(1:3) %>%
   ggplot() +
   aes(x = nom, y = poids) +
   geom_bar(stat = 'identity')+
   coord_flip()


# Derivate Information
## `mutate()`, `group_by()`, et `summarise()`, qui permettent d'utiliser vos données pour calculer de nouvelles variables et des statistiques récapitulatives

# Summarise() fonctions ----
?summarise
data %>%
   filter(sexe == "H") %>%
   summarise(total = n(),
             max = max(taille),
             moyenne = mean(taille))

data %>%
   filter(sexe == "F") %>%
   summarise(total = n(),
             max = max(taille),
             moyenne = mean(taille))


# group_by() -----
data %>%
   group_by(sexe) %>%
   summarise(total = n())


data %>%
   group_by(year, sex) %>%
   summarise(total = sum(n)) %>%
   summarise(total = sum(total))



# Mutate() ---
data %>%
   mutate(IMC = poids/taille^2)




# Exercise: Plotting your Name 

library(prenoms)

prenoms <- prenoms_france


## Exercise: Determinez les 10 prenomz masculins le plus utilisés de l'annee 2018


## Exercise: Pouvez-vous faire une graphique avec la quantite de personnes par annee 
##             avec votre prenom?


prenoms %>%
   filter(name == "Fabio", sex == "M") %>%
   ggplot() +
   aes(x = year, y = n) +
   geom_line() +
   labs(title = "Popularité du prénom Fabio en France") +
   theme_minimal()

# Exercise: Compare your name with other that you want
prenoms %>%
   filter(name == "Fabio" | name == "Mauricio") %>%
   ggplot() +
   aes(x = year, y = n, color = name) +
   geom_line() +
   labs(title = "Popularité du prénom Fabio en France") +
   theme_minimal()



boys_2018 <- filter(prenoms, year == 2018, sex == "M")
boys_2018 <- select(boys_2018, name, n)
boys_2018 <- arrange(boys_2018, desc(n))
boys_2018


