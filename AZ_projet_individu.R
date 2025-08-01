
## Script rendu ##

# C'est un script pour mon projet individuel sur l'estime de l'importante de la famille et des enfants
# en relation avec la confiance dans l'Etat. 

library(dplyr)
library(questionr)
library(rio)
library(forcats)

evs <- readRDS("EVS_fr_tot.rds")

# les variables d'intérêt sont celles où les personnes enquêtées expriment leur opinion pour le mariage, 
# le divorce, le fait d'avoir des enfants, l'avortement, la confiance dans l'Etat et dans l'Eglise 

# 1. Recodage des variables pour l'acm -----------------------------------------
# V69 : importance d'avoir des enfants pour un mariage réussi
freq(evs$v69)
 
## Recoding evs$v69 into evs$enfants_imp
evs$enfants_imp <- evs$v69 %>%
  as.character() %>%
  fct_recode(
    "très important" = "1",
    "plutôt important" = "2",
    "pas très important" = "3",
    NULL = "8",
    NULL = "9"
  )

evs$enfants_imp <- fct_na_value_to_level(evs$enfants_imp, "manquant")

freq(evs$enfants_imp)

# V83 : avoir des enfants est un devoir vis-à-vis de la société
freq(evs$v83)

## Recoding evs$v83 into evs$enfants_dev
evs$enfants_dev <- evs$v83 %>%
  as.character() %>%
  fct_recode(
    "Complétement d'accord" = "1",
    "D'accord" = "2",
    "Sans opinion" = "3",
    "Pas d'accord" = "4",
    "Pas du tout d'accord" = "5",
    NULL = "8",
    NULL = "9"
  )

evs$enfants_dev <- fct_na_value_to_level(evs$enfants_dev, "manquant")
freq(evs$enfants_dev)

# V71 : le mariage est une institution dépassée ?

freq(evs$v71)

## Recoding evs$v71 into evs$mariage_depas
evs$mariage_depas <- evs$v71 %>%
  as.character() %>%
  fct_recode(
    "D'accord" = "1",
    "Pas d'accord" = "2",
    NULL = "8",
    NULL = "9"
  )

evs$mariage_depas <- fct_na_value_to_level(evs$mariage_depas, "manquant")

freq(evs$mariage_depas)

# V154 : justifier l'avortement 

freq(evs$v154)

## Recoding evs$v154 into evs$avortement
evs$avortement <- evs$v154 %>%
  as.character() %>%
  fct_recode(
    "Jamais justifié" = "1",
    "Pas justifié" = "2",
    "Pas justifié" = "3",
    "Pas justifié" = "4",
    "Sans opinion" = "5",
    "Sans opinion" = "6",
    "Justifié" = "7",
    "Justifié" = "8",
    "Justifié" = "9",
    "Toujours justifié" = "10",
    NULL = "88",
    NULL = "99"
  )

## Reordering evs$avortement
evs$avortement <- evs$avortement %>%
  fct_relevel(
    "Jamais justifié", "Pas justifié", "Sans opinion", "Justifié",
    "Toujours justifié"
  )

evs$avortement <- fct_na_value_to_level(evs$avortement, "manquant")

freq(evs$avortement)


# 2. Groupe confiance ----------------------------------------------------------

# V123 : confiance dans la sécurité sociale 

freq(evs$v123)
## Recoding evs$v123 into evs$conf_securite_soc
evs$conf_securite_soc <- evs$v123 %>%
  as.character() %>%
  fct_recode(
    "Grande confiance" = "1",
    "Certaine confiance" = "2",
    "Peu de confiance" = "3",
    "Pas de confiance" = "4",
    NULL = "8",
    NULL = "9"
  )

freq(evs$conf_securite_soc)


# V131 : confiance dans le gouvernement

freq(evs$v131)
## Recoding evs$v131 into evs$conf_gouver
evs$conf_gouver <- evs$v131 %>%
  as.character() %>%
  fct_recode(
    "Confiance" = "1",
    "Confiance" = "2",
    "Peu de confiance" = "3",
    "Pas de confiance du tout" = "4",
    NULL = "8",
    NULL = "9"
  )

freq(evs$conf_gouver)

# V115 : confiance dans l'Eglise

freq(evs$v115)
## Recoding evs$v115 into evs$conf_eglise
evs$conf_eglise <- evs$v115 %>%
  as.character() %>%
  fct_recode(
    "Grande confiance" = "1",
    "Certaine confiance" = "2",
    "Peu de confiance" = "3",
    "Pas de confiance du tout" = "4",
    NULL = "8",
    NULL = "9"
  )

freq(evs$conf_eglise)


# 3. Variables de contrôle -----------------------------------------------------
# sexe, âge, statut matrimonial, nombre d'enfants

freq(evs$v225)
## Recoding evs$v225 into evs$sexe
evs$sexe <- evs$v225 %>%
  as.character() %>%
  fct_recode(
    "Homme" = "1",
    "Femme" = "2"
  )
freq(evs$sexe)


freq(evs$v234)
## Recoding evs$v234 into evs$statut_mariage
evs$statut_mariage <- evs$v234 %>%
  as.character() %>%
  fct_recode(
    "Marié(e) / Pacsé(e)" = "1",
    "Marié(e) / Pacsé(e)" = "2",
    "Veuf / veuve" = "3",
    "Divorcé(e) / Séparé(e)" = "4",
    "Divorcé(e) / Séparé(e)" = "5",
    "Jamais en couple enregistré" = "6",
    NULL = "8",
    NULL = "9"
  )

freq(evs$statut_mariage)

freq(evs$age4)
## Recoding evs$age4 into evs$age_rec
evs$age_rec <- evs$age4 %>%
  as.character() %>%
  fct_recode(
    "18-29" = "1",
    "30-44" = "2",
    "45-59" = "3",
    "60+" = "4"
  )
freq(evs$age_rec)

freq(evs$v239a)
## Recoding evs$v239a into evs$num_enf
evs$num_enf <- evs$v239a %>%
  as.character() %>%
  fct_recode(
    "Pas d'enfants" = "0",
    "Un enfant" = "1",
    "Deux et plus d'enfants" = "2",
    "Deux et plus d'enfants" = "3",
    "Deux et plus d'enfants" = "4",
    "Deux et plus d'enfants" = "5",
    "Deux et plus d'enfants" = "6"
  )

freq(evs$num_enf)

## Recoding evs$num_enf into evs$enf2
evs$enf2 <- evs$num_enf %>%
  fct_recode(
    "Un ou plusieurs enfants" = "Un enfant",
    "Un ou plusieurs enfants" = "Deux et plus d'enfants"
  )

freq(evs$v243)

## Recoding evs$v243 into evs$edu
evs$edu <- evs$v243 %>%
  as.character() %>%
  fct_recode(
    "Sans éducation ou éducation primaire" = "1",
    "Sans éducation ou éducation primaire" = "2",
    "Sans éducation ou éducation primaire" = "3",
    "Sans diplôme" = "4",
    "Sans diplôme" = "5",
    "Sans diplôme" = "6",
    "CAP, BEP" = "7",
    "CAP, BEP" = "8",
    "Bac ou équivalent" = "9",
    "Bac ou équivalent" = "10",
    "Bac ou équivalent" = "11",
    "Bac ou équivalent" = "12",
    "Bac ou équivalent" = "13",
    "Enseignement supérieur du premier cycle" = "14",
    "Enseignement supérieur du premier cycle" = "15",
    "Enseignement supérieur du premier cycle" = "16",
    "Enseignement supérieur du premier cycle" = "17",
    "Enseignement supérieur du premier cycle" = "18",
    "Master ou doctorat" = "19",
    "Master ou doctorat" = "20",
    "Master ou doctorat" = "21",
    "Master ou doctorat" = "22",
    "Master ou doctorat" = "23",
    "Master ou doctorat" = "24",
    "Master ou doctorat" = "25",
    "Master ou doctorat" = "26",
    NULL = "88",
    NULL = "99"
  )

## Reordering evs$edu
evs$edu <- evs$edu %>%
  fct_relevel(
    "Sans éducation ou éducation primaire", "Sans diplôme",
    "Bac ou équivalent", "CAP, BEP", "Enseignement supérieur du premier cycle",
    "Master ou doctorat"
  )

freq(evs$edu)

# recoding une version simple de niveau d'éducation
evs$edu4 <- evs$v243 %>%
  as.character() %>%
  fct_recode(
    "Non-diplômé" = "1",
    "Non-diplômé" = "2",
    "Non-diplômé" = "3",
    "Non-diplômé" = "4",
    "Non-diplômé" = "5",
    "Non-diplômé" = "6",
    "CAP, BEP" = "7",
    "CAP, BEP" = "8",
    "Bac ou équivalent" = "9",
    "Bac ou équivalent" = "10",
    "Bac ou équivalent" = "11",
    "Bac ou équivalent" = "12",
    "Bac ou équivalent" = "13",
    "Enseignement supérieur" = "14",
    "Enseignement supérieur" = "15",
    "Enseignement supérieur" = "16",
    "Enseignement supérieur" = "17",
    "Enseignement supérieur" = "18",
    "Enseignement supérieur" = "19",
    "Enseignement supérieur" = "20",
    "Enseignement supérieur" = "21",
    "Enseignement supérieur" = "22",
    "Enseignement supérieur" = "23",
    "Enseignement supérieur" = "24",
    "Enseignement supérieur" = "25",
    "Enseignement supérieur" = "26",
    NULL = "88",
    NULL = "99"
  )

## Reordering evs$edu4
evs$edu4 <- evs$edu4 %>%
  fct_relevel(
    "Non-diplômé", "Bac ou équivalent", "CAP, BEP", "Enseignement supérieur")


# 4. Introduction a l'acm ------------------------------------------------------

# on prend X variables en factor comportant entre 2 et 6 modalités comportant au moins 5% de l'echantillon pour chaque modalite
# les NA soit explicités sous la forme "manquant" avec 
# evs$nom de la variable <- fct_explicit_na(evs$nom de la variable, "manquant)
evs %>% colnames()
evs_acm <- evs %>% select(enfants_imp,enfants_dev,mariage_depas,avortement)
library(FactoMineR)
library(GDAtools)
library(factoextra)
library('MASS')
getindexcat(evs_acm)
mca_famille = MCA(evs_acm, excl= c(4,10,13,19), row.w = evs$poids_gl)
fviz_mca_var(mca_famille)
evs$var_latente_famille = mca_famille$ind$coord[,1]
evs$var_latente_famille_4 = quant.cut(evs$var_latente_famille, 4)

mca_famille %>% explor::explor()
evs$var_latente_famille_4 %>% freq


# 5. La var latente + le contrôle ----------------------------------------------
## Recoding evs$var_latente_famille_4 into evs$val_fam
evs$val_fam <- evs$var_latente_famille_4 %>%
  fct_recode(
    "Pas du tout familialiste" = "[-1.3995,-0.42102)",
    "Un peu familialiste" = "[-0.42102,-0.059497)",
    "Plutôt familialiste" = "[-0.059497,0.38454)",
    "Très familialiste" = "[0.38454,1.2517]"
  )

# familialiste + age 
library("survey")
evs_pon <- svydesign(ids = ~1, data = evs, weights = evs$poids_gl)
svytable(~age_rec+val_fam, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$age_rec, evs$val_fam))
print(chi2)
chi2$stdres
cramer.v(table(evs$age_rec, evs$val_fam))

# familialiste + sexe
svytable(~sexe+val_fam, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$sexe, evs$val_fam))
print(chi2) #pas significatif ! 

# familialiste + education
svytable(~val_fam + edu, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$edu))
print(chi2)
chi2$stdres
cramer.v(table(evs$val_fam, evs$edu))

# familialiste + nombre d'enfants
svytable(~enf2 + val_fam, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$enf2))
print(chi2)
chi2$stdres
cramer.v(table(evs$val_fam, evs$enf2))

# familialiste + statut marital
svytable(~val_fam + statut_mariage, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$statut_mariage))
print(chi2)
chi2$stdres
cramer.v(table(evs$val_fam, evs$statut_mariage))


library(ggplot2)

# graphique age~val_fam
df <- as.data.frame(svytable(~ age_rec + val_fam, evs_pon))

library(dplyr)
df <- df %>%
  group_by(val_fam) %>%
  mutate(percent = Freq / sum(Freq) * 100)

ggplot(df, aes(x = val_fam, y = Freq, fill = age_rec)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +  
  labs(title = NULL,
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "RdBu")

# graphique edu~val_fam
df2 <- as.data.frame(svytable(~ edu4 + val_fam, evs_pon))

df2 <- df2 %>%
  group_by(val_fam) %>%
  mutate(percent = Freq / sum(Freq) * 100)

ggplot(df2, aes(x = val_fam, y = Freq, fill = edu4)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +  
  labs(title = NULL,
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "RdBu")

# graphique statut mar~val_fam
df3 <- as.data.frame(svytable(~ statut_mariage + val_fam, evs_pon))

df3 <- df3 %>%
  group_by(val_fam) %>%
  mutate(percent = Freq / sum(Freq) * 100)

ggplot(df3, aes(x = val_fam, y = Freq, fill = statut_mariage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +  
  labs(title = NULL,
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "RdBu")



# 6. La var latente + groupe confiance -----------------------------------------

# familialiste + confiance dans le gouvernement
svytable(~val_fam + conf_gouver, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$conf_gouver))
print(chi2) # pas significatif ! 

# familialiste + confiance dans la séciruté sociale
svytable(~conf_securite_soc + val_fam, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$conf_securite_soc))
print(chi2)
chi2$stdres

# familialiste + confiance dans l'église
svytable(~conf_eglise + val_fam, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$val_fam, evs$conf_eglise))
print(chi2)
chi2$stdres


# 7. Croiser les effets : Sécurité sociale -------------------------------------

## enfants + sécurité sociale : est-ce que les parents croient plus dans l'Etat-providence ?

svytable(~num_enf+conf_securite_soc, evs_pon) %>% rprop
svytable(~conf_securite_soc+val_fam, evs_pon) %>% rprop
evs_enf <- filter(evs, num_enf == "Un enfant" | 
                    num_enf == "Deux et plus d'enfants") # on ne garde que les parents
evs_enf_p <- svydesign(ids = ~1, data = evs_enf, weights = evs_enf$poids_gl)

svytable(~enf2+conf_securite_soc, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$conf_securite_soc, evs$enf2))
chi2

##  statut marital + soc_sec: est-ce que les mariés croient plus dans le SSS ? 

svytable(~statut_mariage+conf_securite_soc, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$statut_mariage, evs$conf_securite_soc))
chi2 # significatif

## âge + sec_soc : croit-on plus en système de sécurité sociale avec l'âge ? 

svytable(~age_rec+conf_securite_soc, evs_pon) %>% rprop
# la confiance en sec_soc progresse au cours de la vie 
chi2 = chisq.test(table(evs$age_rec, evs$conf_securite_soc))
chi2 # significatif

## éducation + soc_sec : les diplômés font moins confiance au sss ? 

svytable(~edu4+conf_securite_soc, evs_pon) %>% rprop
# pas de différence prononcée entre les niveaux d'éducation par rapport à la confiance en sss
chi2 = chisq.test(table(evs$edu, evs$conf_securite_soc))
chi2 # significatif



# 8. Croiser les effets : Eglise -----------------------------------------------

## age
svytable(~age_rec+conf_eglise, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$age_rec, evs$conf_eglise))
chi2 # significatif

## éducation
svytable(~edu4+conf_eglise, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$edu4, evs$conf_eglise))
chi2 # significatif

## nombre d'enfants
svytable(~enf2+conf_eglise, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$enf2, evs$conf_eglise))
chi2 # non-significatif

## statut marital 
svytable(~statut_mariage+conf_eglise, evs_pon) %>% rprop
chi2 = chisq.test(table(evs$statut_mariage, evs$conf_eglise))
chi2 # significatif

