sousou <-function(mise = 4000, cout_part = 25,nbr_cycle = 4, salaire_part = .1*cout_part,
                  derniereSemaineDeReinvestissement = 14,
                  benefice_initial = NULL,
                  age_part_initial = NULL,
                  duree_cycle = 14){
  #### Durée du jeu
  nbr_semaine = nbr_cycle * duree_cycle
  duree_cycle = duree_cycle -1 #Because of  the week 0
  if(is.null(derniereSemaineDeReinvestissement)){derniereSemaineDeReinvestissement = nbr_semaine}
  
  #### DEBUT DU JEU: debut de la semaine 1
  # Benefice déjà engrangés au début de la semaine 1
  if(is.null(benefice_initial)){benefice_initial=0}
  #Age des parts au début de la première semaine de jeu
  if(is.null(age_part_initial) || length(age_part_initial)<duree_cycle){
    age_part_initial = rep(0,duree_cycle)
  } else {
    age_part_initial = ifelse(test = is.na(age_part_initial),yes = 0,no = age_part_initial)
  }
  # Premiere semaine
  semaine = 1
  Argent = data.frame(
    semaine = semaine,
    portemonnaie = 0,
    benefice = -mise+benefice_initial,
    retrait = 0,
    revenu = 0 )
  
  Part = data.frame(
    semaine = semaine,
    age_0 = mise %/% cout_part # Parts achetees en debut de semaine
  )
  for(age in 1:duree_cycle){
    Part[semaine,paste0("age_",age)] = age_part_initial[age]
  }
  Part[semaine,"nombreDePart"] = Part[semaine,"age_0"]
  Part[semaine,"revenuFinDeSemaine"] = Part[semaine,"nombreDePart"] * salaire_part
  Argent[semaine,"portemonnaie"]= mise %% cout_part
  
  #### DEROULEMENT DU JEU PENDANT LA PHASE DE REINVESTISSEMENT
  # A chaque iteration, la situation au bebut de la semaine "semaine"
  #est modelisee.
  # La fin de la semaine "semaine" corrspond au debut de la semaine "semaine+1".
  for(semaine in 2:derniereSemaineDeReinvestissement){
    if(semaine<2){break}
    # Nouvelle seamine
    Argent[semaine,"semaine"] = semaine
    Part[semaine,"semaine"] = semaine
    
    # Revenu au debut de la semaine: Salaire des clics la semaine precedente
    #et remboursement des parts achetees il y a 14 semaines.
    Argent[semaine,"revenu"] = 
      sum(Part[semaine-1,paste0("age_",0:duree_cycle)]) * salaire_part 
    # Les parts mises en jeu viellissent d'une semaine, les parts vielles de
    # 14 semaines sont retirees du jeu.
    Part[semaine,paste0("age_",1:duree_cycle)] = Part[semaine-1,paste0("age_",seq(0,duree_cycle-1))]
    
    # Le revenu  et le portemonaie sont investis dans autant de nouvelles parts 
    #que possible
    Part[semaine,"age_0"] = (Argent[semaine,"revenu"] + 
                               Argent[semaine-1,"portemonnaie"]) %/% 
      cout_part
    # Le reste est mis dans le portemonaie
    Argent[semaine,"portemonnaie"] = (Argent[semaine,"revenu"] + 
                                        Argent[semaine-1,"portemonnaie"]) %% 
      cout_part
    # Aucune some n'est retirée du jeu
    Argent[semaine,"retrait"] = 0
    # Puisqu'on ne retire pas de sous, le bénéfice est le même en fin de semaine et au début de la semaine suivante
    Argent[semaine,"benefice"] = Argent[semaine-1,"benefice"]

    # Prévision pour la fin de semain
    Part[semaine,"nombreDePart"] = sum(Part[semaine,paste0("age_",0:duree_cycle)])
    Part[semaine,"revenuFinDeSemaine"] = Part[semaine,"nombreDePart"] * salaire_part
  }
  
  #### DEROULEMENT DU JEU PENDANT LA PHASE DE RETRAIT
  # A chaque iteration, la situation au bebut de la semaine "semaine"
  #est modelisee.
  # La fin de la semaine "semaine" corrspond au debut de la semaine "semaine+1".
  for(semaine in (derniereSemaineDeReinvestissement+1):nbr_semaine){
    if(semaine>nbr_semaine){break}
    # Nouvelle seamine
    Argent[semaine,"semaine"] = semaine
    Part[semaine,"semaine"] = semaine
    
    # Revenu au debut de la semaine: Salaire des clics la semaine precedente
    #et remboursement des parts achetees il y a 14 semaines.
    Argent[semaine,"revenu"] = 
      sum(Part[semaine-1,paste0("age_",0:duree_cycle)]) * salaire_part 
    # Les parts mises en jeu viellissent d'une semaine, les parts vielles de
    # 14 semaines sont retirees du jeu.
    Part[semaine,paste0("age_",1:duree_cycle)] = Part[semaine-1,paste0("age_",0:12)]
    
    # Aucune nouvelle part n'est achetée
    Part[semaine,"age_0"] = 0
    # Le revenu  et le portemonaie sont retirés du jeu`
    Argent[semaine,"retrait"] = (Argent[semaine,"revenu"] + Argent[semaine-1,"portemonnaie"]) 
    
    # Le portemonnaie est vide
    Argent[semaine,"portemonnaie"] = 0
    
    # Puisqu'on a retiré pas des sous, le bénéfice est celui en fin de semaine augmenté des sous retirés
    Argent[semaine,"benefice"] = Argent[semaine-1,"benefice"] + Argent[semaine,"retrait"] 
    
    # Prévision pour la fin de semain
    Part[semaine,"nombreDePart"] = sum(Part[semaine,paste0("age_",0:duree_cycle)])
    Part[semaine,"revenuFinDeSemaine"] = Part[semaine,"nombreDePart"] * salaire_part
  }
  return(list(Argent = Argent,Part = Part))
}