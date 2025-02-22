# Charger les bibliothèques nécessaires
library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)

# Charger les données
load_data <- function() {
  data <- read.csv("/home/hyont-nick/COURS M2S9/INTRODUCTION AU LANGAGE R (DSAD934)/Projet/dataset.csv")
  set.seed(123)
  data$Promotion <- sample(c("Oui", "Non"), nrow(data), replace = TRUE)
  data$Date <- mdy(data$Date)  # Corriger le format des dates
  return(data)
}

data <- load_data()

# Filtrer les données pour janvier à mars 2019
filtered_data <- data %>%
  filter(Date >= "2019-01-01" & Date <= "2019-03-31") %>%
  group_by(Date) %>%
  summarise(Total_Ventes = sum(Total, na.rm = TRUE)) %>%
  arrange(Date)

# Vérifier s'il y a des données disponibles
if (nrow(filtered_data) > 0) {
  # Transformer en série temporelle avec une fréquence hebdomadaire
  sales_ts <- ts(filtered_data$Total_Ventes, 
                 frequency = 7,  # Fréquence hebdomadaire
                 start = c(2019, yday(min(filtered_data$Date)) / 7))
  
  # Vérifier si la série temporelle est valide
  if (length(sales_ts) > 2) {
    # Modèle Holt-Winters
    model_hw <- HoltWinters(sales_ts)
    forecasted_hw <- forecast::forecast(model_hw, h = 14)  # Prévision pour les 14 prochains jours
    
    # Simuler les données réelles de test pour les 14 jours (ici c'est juste un exemple avec des valeurs fictives)
    # En pratique, vous utiliseriez les données réelles si elles étaient disponibles
    set.seed(123)  # Fixer la graine pour la reproductibilité
    real_test_data <- sales_ts[length(sales_ts)] + rnorm(14, mean = 0, sd = 10)  # Simulation d'une série aléatoire
    
    # Calcul des erreurs (différence entre les valeurs réelles et les prévisions)
    errors <- real_test_data - forecasted_hw$mean
    
    # Calcul des métriques
    MAE <- mean(abs(errors))  # Mean Absolute Error
    RMSE <- sqrt(mean(errors^2))  # Root Mean Squared Error
    MAPE <- mean(abs(errors / real_test_data)) * 100  # Mean Absolute Percentage Error
    
    # R² (coefficient de détermination)
    SS_res <- sum(errors^2)  # Somme des carrés des résidus
    SS_tot <- sum((real_test_data - mean(real_test_data))^2)  # Somme des carrés totaux
    R_squared <- 1 - (SS_res / SS_tot)
    
    # Afficher les métriques
    cat("Métriques d'évaluation :\n")
    cat("MAE :", round(MAE, 2), "\n")
    cat("RMSE :", round(RMSE, 2), "\n")
    cat("MAPE :", round(MAPE, 2), "%\n")
    cat("R² :", round(R_squared, 2), "\n")
    
    # Visualisation des prévisions
    autoplot(forecasted_hw) +
      labs(title = "Prévisions des ventes avec Holt-Winters (Hebdomadaire)",
           x = "Temps (semaines)",
           y = "Ventes prévues") +
      theme_minimal()
  } else {
    print("Pas assez de données pour effectuer une prévision.")
  }
} else {
  print("Aucune donnée disponible pour les prévisions.")
}
