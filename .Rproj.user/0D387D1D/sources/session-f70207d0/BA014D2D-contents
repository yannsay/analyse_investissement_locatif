library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(DT)
library(shinyWidgets)
library(openxlsx)
library(gridlayout)
library(bslib)
library(shinythemes)

source("R/functions.R")
source("R/parameters.R")

ui <- grid_page(
  theme = shinytheme("simplex"),
  layout = c(
    "header header   download",
    "inputs navpanel navpanel"
  ),
  row_sizes = c(
    "100px",
    "1.67fr"
  ),
  col_sizes = c(
    "270px",
    "1.72fr",
    "0.28fr"
  ),
  gap_size = "1rem",
  grid_card_text(
    area = "header",
    content = "Analyse financière investissement locatif",
    alignment = "start",
    is_title = TRUE
  ),
  grid_card(
    area = "navpanel",
    card_body(
      tabsetPanel(
        nav_panel(
          title = "Graphique",
          plotlyOutput(outputId = "graph_tresorie")
        ),
        nav_panel(
          title = "Trésorie",
          DTOutput(outputId = "formated_tresorie", width = "100%")
        ),
        nav_panel(
          title = "Resumé",
          DTOutput(outputId = "resume", width = "100%")
        ),
        nav_panel(
          title = "Lisez-moi",
          DTOutput(outputId = "lisezmoi", width = "100%")
        )
      )
    )
  ),
  grid_card(
    area = "inputs",
    card_body(
      align = "right",
      actionButton(inputId = "simulate", label = "Simulation"),
      tabsetPanel(
        nav_panel(
          title = "Instruction",
          
          p("Entrez vos informations dans l'onglet \"Information\" et cliquez sur \"Simulation\".",
            style="text-align:left"),
          p("Pour télécharger votre simulation, cliquez sur le bouton en haut à droite",
            style="text-align:left")
        ),
        nav_panel(
          title = "Information",
          currencyInput("cout_construction", "Coût de construction", value = 120000, align =  "right"),
          currencyInput("cout_non_construction_inclut_CI", "Coût non construction, inclût crédit d'impôts", value = 15000, align = "right"),
          currencyInput("cout_non_construction_hors_CI", "Coût non construction, hors crédit d'impôts", value = 5000, align = "right"),
          formatNumericInput("imprevu", "Imprévu (% coût de construction)", value = .10, format = "percentageEU2dec", align = "right"),
          currencyInput("apport_perso", "Apport personel",value = 26000, align = "right"),
          formatNumericInput("interet", "Taux d'interet", value = 0.05, format = "percentageEU2dec", align = "right"),
          autonumericInput("duree_annee_pret", "Nombre d'années prêt", value = 15, align = "right"),
          formatNumericInput("taux_CI", "Taux crédit d'impôts", value = .3, format = "percentageEU2dec", align = "right"),
          currencyInput("revenu_annuel", "Revenu annuel", value = 9200, align = "right"),
          formatNumericInput("augmentation_loyer", "Pourcentage d'augmentation loyer annuel", value = .01, 
                             format = "percentageEU2dec", align = "right"),
          formatNumericInput("charges_courrantes_percentage", "Charge courrantes (en % du loyer)", 
                             value = .05, format = "percentageEU2dec", align = "right"),
          autonumericInput("taux_taxes_foncieres", "Taxes foncières (en nombre de loyer mensuel)", 
                                                                 value = 2),
          currencyInput("charges_annuelles_init", "Charge annuelle initiale", value = 600, align = "right"),
          autonumericInput("annees", "Simuler sur combien d'années?", value = 20, align = "right")
        )
      )
    )
  ),
  grid_card(
    area = "download",
    card_body(downloadButton("telecharger", ""))
  )
)


server <- function(input, output) {
  cout_total_input <- eventReactive(input$simulate, {
    cout_total_construction(cout_construction = input$cout_construction, 
                            cout_non_construction_inclut_CI = input$cout_non_construction_inclut_CI, 
                            cout_non_construction_hors_CI = input$cout_non_construction_hors_CI, 
                            imprevu = input$imprevu)
  })
  
  pret <- reactive(calcul_pret(cout_total = cout_total_input(), 
                               apport_perso = input$apport_perso))
  
  nombre_mensualite <- eventReactive(input$simulate, {
    calcul_nombre_mensualite(duree_annee_pret = input$duree_annee_pret)
  })
  
  mensualite <- reactive(calcul_mensualite(capital = pret(),
                                           nbr_mensualite = nombre_mensualite(),
                                           taux = input$interet)
  )
  
  cout_total_credit <- reactive(calcul_cout_total_credit(mensualite = mensualite(), 
                                                         nombre_mensualite = nombre_mensualite()
  )
  )
  
  CI <-eventReactive(input$simulate, 
                     {calcul_credit_dimpot(cout_construction = input$cout_construction,
                                           cout_non_construction_inclut_CI = input$cout_non_construction_inclut_CI,
                                           taux_credit_impot = input$taux_CI)
                     })
  
  tresorie <- reactive(calcul_tresorie(annees = input$annees,
                                       revenu_annuel = input$revenu_annuel,
                                       charges_annuelles_init = input$charges_annuelles_init,
                                       taux_taxes_foncieres = input$taux_taxes_foncieres,
                                       charges_courrantes_percentage = input$charges_courrantes_percentage,
                                       inflation = input$augmentation_loyer,
                                       mensualite = mensualite(),
                                       duree_annees_pret = input$duree_annee_pret,
                                       credit_impot = CI(),
                                       cout_total_credit = cout_total_credit()))
  
  formated_tresorie <- reactive(tresorie() %>%
                                  datatable() %>%
                                  formatCurrency(formatter_currency, currency = "€"))
  graph_tresorie <- reactive(faire_graph_tresorie(tresorie = tresorie())) 
  
  
  resume <- eventReactive(input$simulate,{
    rbind.data.frame(
      c("Coût de construction", formatteur_euro(input$cout_construction)),
      c("Coût non construction, inclût crédit d'impôts", formatteur_euro(input$cout_non_construction_inclut_CI)),
      c("Coût non construction, hors crédit d'impôts", formatteur_euro(input$cout_non_construction_hors_CI)),
      c("Imprévu (% coût de construction)", input$imprevu),
      c("Coût total construction",formatteur_euro(cout_total_input())),
      c("Apport personel", formatteur_euro(input$apport_perso)),
      c("Montant de la demande de prêt", formatteur_euro(pret())),
      c("Nombre d'années prêt", input$duree_annee_pret),
      c("Nombre de mensualités", nombre_mensualite()),
      c("Taux d'interet", input$interet),
      c("Mensualité des remboursements", formatteur_euro(mensualite())),
      c("Coût total prêt", formatteur_euro(cout_total_credit())),
      c("Taux crédit d'impôts", input$taux_CI),
      c("Montant crédit d'impôts", formatteur_euro(CI())),
      c("Revenu annuel", formatteur_euro(input$revenu_annuel)),
      c("Pourcentage d'augmentation loyer annuel", input$augmentation_loyer),
      c("Charge courrantes (en % du loyer)",input$charges_courrantes_percentage),
      c("Taxes foncières (en nombre de loyer mensuel)",input$taux_taxes_foncieres),
      c("Charge annuelle initiale",input$charges_annuelles_init),
      make.row.names = F)  %>%
      `names<-`(c("Item", "Valeur"))
  })
  
  output$graph_tresorie <- renderPlotly({graph_tresorie()})
  output$formated_tresorie <- DT::renderDataTable(formated_tresorie())
  output$resume <- DT::renderDataTable(resume())
  output$lisezmoi <- DT::renderDataTable(lisezmoi)
  
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("simulation",Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName = "graph")
      addWorksheet(wb, sheetName = "tresorie")
      addWorksheet(wb, sheetName = "resume")
      addWorksheet(wb, sheetName = "lisezmoi")
      writeData(wb, sheet = "resume", resume())
      writeData(wb, sheet = "tresorie", tresorie())
      writeData(wb, sheet = "lisezmoi", lisezmoi)
      plot(graph_tresorie())
      insertPlot(wb, "graph")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}

shinyApp(ui, server)
