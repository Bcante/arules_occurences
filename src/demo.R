#A remplacer par le dossier du projet
setwd(dir = "./Bureau/arules_occurences/")
source("src/main.R")
data("Groceries")
conf = 0.3
sup = 0.01
input_labels_utilisateurs = c("whole milk","pork","shoe","root vegetables","tropical fruit","shoe")
input_labels_utilisateurs = c("whole milk","pork","root vegetables","tropical fruit")
decompte_lhs = T
decompte_rhs = F
lhs_exclusif = F
rhs_exclusif = F

grocery_rules <- apriori(Groceries, parameter = list(support = sup, confidence = conf))
affiche_occurences(grocery_rules,input_labels_utilisateurs,decompte_lhs,decompte_rhs,lhs_exclusif, rhs_exclusif)

rules = grocery_rules

rules2 = rules



rules=grocery_rules
mean_rules = function(labels_utilisateurs,rules) {
  df_list = list()
  
  for (i in 1:length(labels_utilisateurs)) {
    rules_item = subset(rules, subset = lhs %in% labels_utilisateurs[i])
    if(length(rules_item) > 1) {
      rules_df = data.frame(
        lhs = labels(lhs(rules_item)),
        rhs = labels(rhs(rules_item)), 
        rules_item@quality)
      
      rules_df=rules_df %>% select(lhs,rhs,support,confidence,lift)
      
      rules_df=rules_df %>% dplyr::summarize(
        mean_support = mean(support),
        mean_confidence = mean(confidence),
        mean_lift = mean(lift)
      )
      rules_df$nom=labels_utilisateurs[i]
      
      df_list[[i]] = rules_df
    }
  }
  return(df_list)
}

res=mean_rules(input_labels_utilisateurs,grocery_rules)
bind_rows(res)
distinct(xd)

rules=res[[1]]
rules=rules %>% dplyr::summarize(
  mean_support = mean(support),
  mean_confidence = mean(confidence),
  mean_lift = mean(lift)
) 
rules$name = "shoe"
