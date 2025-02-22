# Charger les packages nécessaires
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(DT)

# Fonction pour charger les données
load_data <- function() {
  data <- read.csv("/home/hyont-nick/COURS M2S9/INTRODUCTION AU LANGAGE R (DSAD934)/Projet/dataset.csv")
  set.seed(123)
  data$Promotion <- sample(c("Oui", "Non"), nrow(data), replace = TRUE)
  
  # Utiliser mdy pour le formatage des dates (format mm/dd/yyyy)
  data$Date <- mdy(data$Date)
  
  # Ajouter les colonnes Jour, Mois et Année
  data$Jour <- day(data$Date)
  data$Mois <- month(data$Date, label = TRUE, abbr = TRUE)
  data$Annee <- year(data$Date)
  
  return(data)
}

# Charger les données initiales
data <- load_data()

# Interface utilisateur (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Analyse des Ventes Supermarché"),
  
  # Empêcher le débordement horizontal
  tags$head(
    tags$style(HTML("
      body {
        overflow-x: hidden;
      }
      .sidebar {
        max-height: 90vh; 
        overflow-y: auto; 
      }
      .main-panel {
        max-height: 90vh; 
        overflow-y: auto; 
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar", # Appliquer la classe CSS pour le style
      actionButton("reload_data", "Recharger les données", icon = icon("refresh"), 
                   style = "color: #FFFFFF; background-color: #007BFF; margin-bottom: 15px;"),
      selectInput("city", "Ville :", choices = c("Toutes" = "", unique(data$City)), selected = ""),
      selectInput("product_line", "Ligne de produits :", choices = c("Toutes" = "", unique(data$Product.line)), selected = ""),
      selectInput("branch", "Branch :", choices = c("Toutes" = "", unique(data$Branch)), selected = ""),
      selectInput("customer_type", "Type de client :", choices = c("Tous" = "", unique(data$Customer.type)), selected = ""),
      selectInput("gender", "Genre :", choices = c("Tous" = "", unique(data$Gender)), selected = ""),
      selectInput("payment", "Mode de paiement :", choices = c("Tous" = "", unique(data$Payment)), selected = ""),
      selectInput("jour", "Jour :", choices = c("Tous" = "", unique(data$Jour)), selected = ""),
      selectInput("mois", "Mois :", choices = c("Tous" = "", unique(data$Mois)), selected = ""),
      selectInput("annee", "Année :", choices = c("Toutes" = "", unique(data$Annee)), selected = ""),
      checkboxInput("promo", "Inclure uniquement les promotions", value = FALSE),
      selectInput("chart_type", "Type de graphique :", 
                  choices = c("Ventes totales par mois", "Ventes totales par branche", 
                              "Répartition des modes de paiement", "Évolution des ventes par mois"), 
                  selected = "Ventes totales par mois"),
      selectInput("forecast_unit", "Unité de prévision :", 
                  choices = c("Jour" = "day", "Mois" = "month", "Année" = "year"), 
                  selected = "month")
    ),
    
    mainPanel(
      class = "main-panel", # Appliquer la classe CSS pour le style
      tabsetPanel(
        tabPanel("Résumé", tableOutput("summary_table")),
        tabPanel("Graphiques", plotOutput("sales_plot")),
        tabPanel("Données détaillées", dataTableOutput("data_table")),
        tabPanel("Prévisions", plotOutput("forecast_plot"))
      )
    )
  )
)

""
#"Distribution des évaluations", 
#"Ventes par ligne de produit",
""
# Serveur
server <- function(input, output, session) {
  # Stocker les données dans une variable réactive
  data_reactive <- reactiveVal(data)
  
  # Observer les clics sur le bouton de rechargement
  observeEvent(input$reload_data, {
    new_data <- load_data()  # Recharger les données
    data_reactive(new_data) # Mettre à jour la variable réactive
    showNotification("Les données ont été rechargées avec succès.", type = "message")
    
    # Mettre à jour les filtres dynamiques
    updateSelectInput(session, "city", choices = c("Toutes" = "", unique(new_data$City)))
    updateSelectInput(session, "product_line", choices = c("Toutes" = "", unique(new_data$Product.line)))
    updateSelectInput(session, "branch", choices = c("Toutes" = "", unique(new_data$Branch)))
    updateSelectInput(session, "customer_type", choices = c("Tous" = "", unique(new_data$Customer.type)))
    updateSelectInput(session, "gender", choices = c("Tous" = "", unique(new_data$Gender)))
    updateSelectInput(session, "payment", choices = c("Tous" = "", unique(new_data$Payment)))
    updateSelectInput(session, "jour", choices = c("Tous" = "", unique(new_data$Jour)))
    updateSelectInput(session, "mois", choices = c("Tous" = "", unique(new_data$Mois)))
    updateSelectInput(session, "annee", choices = c("Toutes" = "", unique(new_data$Annee)))
  })
  
  # Filtrer les données en fonction des entrées utilisateur
  filtered_data <- reactive({
    df <- data_reactive()
    
    if (input$city != "") {
      df <- df %>% filter(City == input$city)
    }
    if (input$product_line != "") {
      df <- df %>% filter(Product.line == input$product_line)
    }
    if (input$branch != "") {
      df <- df %>% filter(Branch == input$branch)
    }
    if (input$customer_type != "") {
      df <- df %>% filter(Customer.type == input$customer_type)
    }
    if (input$gender != "") {
      df <- df %>% filter(Gender == input$gender)
    }
    if (input$payment != "") {
      df <- df %>% filter(Payment == input$payment)
    }
    if (input$jour != "") {
      df <- df %>% filter(Jour == as.numeric(input$jour))
    }
    if (input$mois != "") {
      df <- df %>% filter(Mois == input$mois)
    }
    if (input$annee != "") {
      df <- df %>% filter(Annee == as.numeric(input$annee))
    }
    if (input$promo) {
      df <- df %>% filter(Promotion == "Oui")
    }
    
    df
  })
  
  # Résumé des données
  output$summary_table <- renderTable({
    filtered_data() %>%
      summarise(
        Total_Ventes = sum(Total),
        Quantite_Totale = sum(Quantity),
        Revenu_Moyen = mean(Total)
      )
  })
  
  # Graphiques
  output$sales_plot <- renderPlot({
    data_to_plot <- filtered_data()
    
    if (input$chart_type == "Ventes totales par mois") {
      ggplot(data_to_plot, aes(x = Mois, y = Total, fill = Promotion)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Ventes totales par mois", x = "Mois", y = "Total des ventes") +
        theme_minimal()
      
    } else if (input$chart_type == "Ventes totales par branche") {
      ggplot(data_to_plot, aes(x = Branch, y = Total, fill = Promotion)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Ventes totales par branche", x = "Branche", y = "Total des ventes") +
        theme_minimal()
      
    } else if (input$chart_type == "Répartition des modes de paiement") {
      data_to_plot %>%
        count(Payment) %>%
        ggplot(aes(x = "", y = n, fill = Payment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "Répartition des modes de paiement", x = NULL, y = NULL) +
        theme_void()
      
    } else if (input$chart_type == "Évolution des ventes par mois") {
      data_to_plot %>%
        group_by(Mois, Annee) %>%
        summarise(Total_Ventes = sum(Total)) %>%
        ggplot(aes(x = Mois, y = Total_Ventes, group = Annee, color = as.factor(Annee))) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Évolution des ventes par mois", x = "Mois", y = "Total des ventes", color = "Année") +
        theme_minimal()
    }
  })
  
  # Prévisions
  output$forecast_plot <- renderPlot({
    sales_data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(Total_Ventes = sum(Total, na.rm = TRUE)) %>%
      filter(!is.na(Date))
    
    if (nrow(sales_data) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée disponible pour les prévisions.", cex = 1.5)
      return()
    }
    
    unit <- input$forecast_unit
    if (unit == "day") {
      sales_ts <- ts(sales_data$Total_Ventes, frequency = 365, start = c(year(min(sales_data$Date)), yday(min(sales_data$Date))))
    } else if (unit == "month") {
      sales_ts <- ts(sales_data$Total_Ventes, frequency = 12, start = c(year(min(sales_data$Date)), month(min(sales_data$Date))))
    } else if (unit == "year") {
      sales_ts <- ts(sales_data$Total_Ventes, frequency = 1, start = c(year(min(sales_data$Date))))
    }
    
    model_hw <- HoltWinters(sales_ts)
    forecasted_hw <- forecast::forecast(model_hw, h = ifelse(unit == "day", 30, ifelse(unit == "month", 12, 5)))
    
    plot(forecasted_hw, main = paste("Prévisions des ventes (", unit, ")", sep = ""))
  })
  
  # Données détaillées
  output$data_table <- renderDataTable({
    filtered_data()
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
