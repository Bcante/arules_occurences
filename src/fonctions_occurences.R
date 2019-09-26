library(ggplot2)
library(arules)
library(dplyr)

intersecte_labels <- function(labels_utilisateurs, rules) {
  return(labels_utilisateurs %>% intersect(itemLabels(rules)))
}

#Prepare un vecteur nommé contenant en nom chaque label existant dans le jeux de données
#Si label_utilisateurs cont
prepare_named_vector <- function(rules, labels_utilisateurs) {
  labels_utilisateurs=intersecte_labels(labels_utilisateurs,rules)
  labels_utilisateurs_presents=numeric(length(labels_utilisateurs)) #Préallocation d'un vecteur nommé
  labels_utilisateurs = setNames(labels_utilisateurs_presents,labels_utilisateurs)
  return(labels_utilisateurs)
}

#Permet  d'avoir pour chaque label le nombre d'occurence en RHS ou LHS
count_occurences <- function(rules,labels_utilisateurs, decompte_lhs = F, decompte_rhs = F, lhs_exclusif = F , rhs_exclusif = F) {
  apparition_lhs = labels_utilisateurs
  apparition_rhs = labels_utilisateurs
  
  if (decompte_lhs == F & decompte_rhs == F) {
    stop("decompte_lhs et decompte_rhs sont FAUX, hors au moins un des deux doit être VRAI")
  }
  
  if (decompte_lhs == T) {
    if (lhs_exclusif == T) {
      for(i in 1:length(apparition_lhs)) {
        apparition_lhs[names(apparition_lhs[i])] = 
          length(subset(rules, subset = lhs %oin% as.character(names(apparition_lhs[i]))))
      }
    } else {
      for(i in 1:length(apparition_lhs)) {
        apparition_lhs[names(apparition_lhs[i])] = 
          length(subset(rules, subset = lhs %in% as.character(names(apparition_lhs[i]))))
      }
    }
  }
  if (decompte_rhs == T) {
    if (rhs_exclusif == T) {
      for(i in 1:length(apparition_rhs)) {
        apparition_rhs[names(apparition_rhs[i])] = 
        length(subset(rules, subset = rhs %oin% as.character(names(apparition_rhs[i]))))
      }
    } else {
      for(i in 1:length(apparition_rhs)) {
       apparition_rhs[names(apparition_rhs[i])] = 
        length(subset(rules, subset = rhs %in% as.character(names(apparition_rhs[i]))))
      }
    }
  }
  return(apparition_lhs+apparition_rhs)
}

#Permet de créer le titre du graphique selon si on fait le décompte des occurences en lhs et/ou rhs 
genere_titre <- function (decompte_lhs,decompte_rhs) {
  titre = "Apparition en "
  if (decompte_lhs == T & decompte_rhs == T) {
    titre = paste(titre ,"antécédent ou conséquence",sep = "")
  } else if (decompte_lhs == T) {
    titre = paste(titre ,"antécédent uniquement",sep = "")
  } else if (decompte_rhs == T) {
    titre = paste(titre ,"conséquence uniquement",sep = "")
  } else {
    stop("decompte_lhs et decompte_rhs sont FAUX, hors au moins un des deux doit être VRAI")
  }
  return(titre)
}

genere_plot <- function(liste_a_analyser,decompte_lhs = F, decompte_rhs = F, lhs_exclusif = F , rhs_exclusif = F,conf,sup,nb_regles) {
  titre = genere_titre(decompte_lhs,decompte_rhs)
  
  lhs_label = "exclusif"
  rhs_label = "exclusif"
  if (lhs_exclusif == T) {
    lhs_label = paste("non-",lhs_label,sep = "")
  }
  if (rhs_exclusif == T) {
    rhs_label = paste("non-",rhs_label,sep = "")
  }
  
  conf_et_sup_label = paste("confiance: ",conf,", support: ",sup,sep = "")
  sous_titre=paste("antécédent: ",lhs_label,", conséquence: ",rhs_label,", ",conf_et_sup_label,", # de règles total: ",nb_regles,sep = "")
  
  
  df=data.frame(keyName=names(liste_a_analyser), value=liste_a_analyser, row.names=NULL) %>% 
    filter (value > 0)
  
  p<-ggplot(data=df, aes(x=reorder(keyName,-value), y=value)) +
    geom_bar(stat="identity", fill="steelblue")+
    ggtitle(titre) +
    theme_minimal() +
    labs(
      subtitle = sous_titre,
      x="items",
      y=paste("nombre d'occurences")
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text=element_text(size=12))
  return(p)
  
}

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

affiche_occurences = function(rules_utilisateurs,input_labels_utilisateurs,decompte_lhs = F,decompte_rhs = T,lhs_exclusif = F,rhs_exclusif = F) {
  nb_regles=rules_utilisateurs %>% length()
  named_vector=prepare_named_vector(rules_utilisateurs,input_labels_utilisateurs)
  occurences=count_occurences(rules_utilisateurs,named_vector,decompte_lhs,decompte_rhs,lhs_exclusif,rhs_exclusif)
  genere_plot(occurences,decompte_lhs, decompte_rhs, lhs_exclusif, rhs_exclusif,conf,sup,nb_regles)
}
