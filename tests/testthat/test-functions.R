test_that("calcul_* fonctionnent", {
  expect_equal(round(calcul_mensualite(10000, 0.05, 180),2), 79.08)
  expect_equal(round(calcul_mensualite(1320000, 0.05, 180),2), 10438.48)
  expect_equal(cout_total_construction(cout_construction = 10000, 
                                       cout_non_construction_inclut_CI = 10000, 
                                       cout_non_construction_hors_CI = 10000, 
                                       imprevu = .10), 
               31000)
  expect_equal(calcul_pret(cout_total = 10000, 
                           apport_perso = 2000), 
               8000)
  expect_equal(calcul_nombre_mensualite(5), 
               60)
  expect_equal(calcul_cout_total_credit(100, 12), 1200)
  
  expect_equal(calcul_credit_dimpot(10000, 1000, .20), 2200)
  
})

test_that("calcul_tresorie fonctionne", {
  expected_output <- data.frame(annee = 1:5,
                                loyer_annuel = c(1000, 1010, 1020.1, 1030.3, 1040.6),
                                charges_annuelles = c(100, 101, 102.01, 103.03, 104.06),
                                taxes_foncieres = c(83.33, 84.17, 85.01, 85.86, 86.72),
                                charges_courrantes = c(50, 50.5, 51.01, 51.52, 52.03),
                                remboursement_pret = rep(10800,5),
                                CI = c(0,3000,0,0,0),
                                tresorie = c(-10033.33, -17059, -27076.92, -37087.03, -47089.23),
                                pret_restant = c(89200,78400,67600,56800,46000),
                                rembourse = c(10800,21600,32400,43200,54000))
  
  actual_result <- calcul_tresorie(annees = 5,
                                   revenu_annuel = 1000,
                                   charges_annuelles_init = 100,
                                   taux_taxes_foncieres = 1,
                                   charges_courrantes_percentage = .05,
                                   inflation = 0.01,
                                   mensualite = 900,
                                   duree_annees_pret = 5,
                                   credit_impot = 3000,
                                   cout_total_credit = 100000) |> round(2)
  
  expect_equal(actual_result,expected_output)
})



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
