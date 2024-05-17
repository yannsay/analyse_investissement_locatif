calcul_mensualite <- function(capital, taux, nbr_mensualite) {
  (capital * taux / 12) / (1 - (((1 + (taux / 12)))^(-nbr_mensualite)))
}

cout_total_construction <- function(cout_construction, 
                                    cout_non_construction_inclut_CI, 
                                    cout_non_construction_hors_CI, 
                                    imprevu) {
  cout_construction + cout_non_construction_inclut_CI + cout_non_construction_hors_CI + cout_construction * imprevu
  
}
calcul_pret <- function(cout_total, apport_perso){
  cout_total - apport_perso
}

calcul_nombre_mensualite <- function(duree_annee_pret) {
  duree_annee_pret * 12
}
calcul_cout_total_credit <- function(mensualite, nombre_mensualite){
  mensualite * nombre_mensualite
}
calcul_credit_dimpot <- function(cout_construction,
                                 cout_non_construction_inclut_CI,
                                 taux_credit_impot) {
  (cout_construction + cout_non_construction_inclut_CI) * taux_credit_impot
}


calcul_tresorie <- function(annees,
                            revenu_annuel,
                            charges_annuelles_init,
                            taux_taxes_foncieres,
                            charges_courrantes_percentage,
                            inflation,
                            mensualite,
                            duree_annees_pret,
                            credit_impot,
                            cout_total_credit) {
  data.frame(annee = 1:annees) %>%
    mutate(loyer_placeholder = revenu_annuel * (1+inflation)^(1:annees),
           loyer_annuel = lag(loyer_placeholder),
           loyer_annuel = if_else(is.na(loyer_annuel),revenu_annuel,loyer_annuel),
           charge_annuelles_placeholder = charges_annuelles_init * (1+inflation)^(1:annees),
           charges_annuelles = lag(charge_annuelles_placeholder),
           charges_annuelles = if_else(is.na(charges_annuelles),charges_annuelles_init,charges_annuelles)) %>%
    select(-loyer_placeholder, -charge_annuelles_placeholder) %>%
    mutate(taxes_foncieres = loyer_annuel * taux_taxes_foncieres / 12,
           charges_courrantes = loyer_annuel * charges_courrantes_percentage,
           remboursement_pret = c(rep(mensualite * 12, duree_annees_pret), rep(0,(annees-duree_annees_pret))),
           CI = c(0,credit_impot, rep(0,(annees-2))),
           tresorie = cumsum(loyer_annuel - charges_annuelles - taxes_foncieres - charges_courrantes - remboursement_pret + CI),
           pret_restant = cout_total_credit -cumsum(remboursement_pret),
           pret_restant = if_else(pret_restant <1, 0, pret_restant),
           rembourse = cout_total_credit - pret_restant)
}



faire_graph_tresorie <- function(tresorie) {
  tresorie %>% 
    mutate(couleur = if_else(tresorie < 0, "negatif", "positif")) %>%
    ggplot() +
    geom_col(aes(x=annee, y=tresorie, fill = couleur)) + 
    theme_minimal() + 
    scale_fill_manual(values = c("negatif" = "#F4A582", "positif" = "#A6D96A"), guide = FALSE) +
    scale_y_continuous(labels=dollar_format(suffix="€",prefix="")) + 
    theme(legend.position = "none") +
    ggtitle("Tresorie par annee") +
    ylab("")
}

formatteur_euro <- scales::label_dollar(prefix = "", suffix = "€", big.mark = ".", decimal.mark = ",")