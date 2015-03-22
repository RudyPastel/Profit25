rm(list = ls())
#Nombre de cycles
nbr_cycle = 5
#Nombre de semaines de jeu
nbr_semaine = 14 * nbr_cycle
#Mise initiale
mise = 4000
# Co?t d'une part
cout_part = 25
# Salaire d'une part
salaire_part = .1 * cout_part

# Supposons pour le moment que toutes les euros disponibles sont reinvestis dans
# l'ach?t de part chaque semaine. De plus, la mise initiale est retiree du jeu 
# des que le profit en fin de cycle est superieur a la mise initiale.

#### DEBUT DU JEU: debut de la semaine 1
# Premiere semaine
semaine = 1
Argent = data.frame(
    semaine = semaine,
    portemonnaie = 0,
    benefice = -mise,
    revenu = 0 )

Part = data.frame(
    semaine = semaine,
    age_0 = mise %/% cout_part # Parts achetees en debut de semaine
)
for(age in 1:13){
    Part[semaine,paste0("age_",age)] = 0
}
Argent[semaine,"portemonnaie"]= mise %% cout_part

#### DEROULEMENT DU JEU
# A chaque iteration, la situation au bebut de la semaine "semaine"
#est modelisee.
# La fin de la semaine "semaine" corrspond au debut de la semaine "semaine+1".
for(semaine in 2:nbr_semaine){
    # Nouvelle seamine
    Argent[semaine,"semaine"] = semaine
    Part[semaine,"semaine"] = semaine
    
    # Revenu au debut de la semaine: Salaire des clics la semaine precedente
    #et remboursement des parts achetees il y a 14 semaines.
    Argent[semaine,"revenu"] = 
        sum(Part[semaine-1,paste0("age_",0:13)]) * salaire_part + 
        Part[semaine-1,"age_13"] * cout_part 
    # Les parts mises en jeu viellissent d'une semaine, les parts vielles de
    # 14 semaines sont retirees du jeu.
    Part[semaine,paste0("age_",1:13)] = Part[semaine-1,paste0("age_",0:12)]
    
    # Le revenu  et le portemonaie sont investis dans autant de nouvelles parts 
    #que possible
    Part[semaine,"age_0"] = (Argent[semaine,"revenu"] + 
                                 Argent[semaine-1,"portemonnaie"]) %/% 
        cout_part
    # Le reste est mis dans le portemonaie
    Argent[semaine,"portemonnaie"] = (Argent[semaine,"revenu"] + 
                                          Argent[semaine-1,"portemonnaie"]) %% 
        cout_part
    
    # Puisqu'on ne retire pas de sous, le bénéfice est le même en fin de semaine et au début de la semaine suivante
    Argent[semaine,"benefice"]=Argent[semaine-1,"benefice"]
}

