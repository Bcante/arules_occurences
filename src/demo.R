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



affiche_occurences(grocery_rules,input_labels_utilisateurs,decompte_lhs,decompte_rhs,lhs_exclusif, rhs_exclusif)


calcule_mean <- function(label,rules_item) {
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
    rules_df$nom=label
    return(rules_df)
}

mean_rules = function(labels_utilisateurs,rules,decompte_lhs,decompte_rhs,lhs_exclusif, rhs_exclusif) {
  rules_lhs = 1
  rules_rhs = 1
  rules_item = 1

  df_list = list()
  
  for (i in 1:length(labels_utilisateurs)) {
    if (lhs_exclusif == T) {
      rules_lhs = subset(rules, subset = lhs %oin% labels_utilisateurs[i])
    } else {
      rules_lhs = subset(rules, subset = lhs %in% labels_utilisateurs[i])
    }
    
    if (rhs_exclusif == T) {
      rules_rhs = subset(rules, subset = rhs %oin% labels_utilisateurs[i])
    } else {
      rules_rhs = subset(rules, subset = rhs %in% labels_utilisateurs[i])
    }
    
    if (decompte_lhs == T & decompte_rhs == T) {
      rules_item = c(rules_lhs,rules_rhs)
      rules_item = unique(rules_item)
    } else if (decompte_lhs == T & decompte_rhs == F) {
      rules_item = rules_lhs
    } else if (decompte_lhs == F & decompte_rhs == T) {
      rules_item = rules_rhs
    } else {
      stop("decompte_lhs et decompte_rhs sont FAUX, hors au moins un des deux doit être VRAI")
    }
    
    if(length(rules_item) > 1) {
      subset=calcule_mean(labels_utilisateurs[i],rules_item)
      df_list[[i]]=subset
    }
  }
  return(bind_rows(df_list))
}

mean_rules(input_labels_utilisateurs,grocery_rules,T,F,F,F)
