#A remplacer par le dossier du projet
setwd(dir = "./Bureau/arules_occurences/")
source("src/fonctions_occurences.R")
data("Groceries")
conf = 0.3
sup = 0.01
input_labels_utilisateurs = c("whole milk","pork","shoe","root vegetables","tropical fruit")

decompte_lhs = T
decompte_rhs = F
lhs_exclusif = F
rhs_exclusif = F

grocery_rules <- apriori(Groceries, parameter = list(support = sup, confidence = conf))
affiche_occurences(grocery_rules,input_labels_utilisateurs,decompte_lhs,decompte_rhs,lhs_exclusif, rhs_exclusif)
