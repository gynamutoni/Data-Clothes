#MODÈLE DE DONNÉES

##OBJET 1 — SAISONS & TEMPÉRATURES

saisons <- data.frame(
  saison = c("printemps", "été", "automne", "hiver"),
  temp_min = c(10, 20, 10, -5),
  temp_max = c(20, 35, 18, 10),
  stringsAsFactors = FALSE
) 

###OBJET 2 — TENUES (CŒUR DU SYSTÈME)


tenues <- data.frame(
  saison = c(
    # PRINTEMPS
    "printemps","printemps","printemps",
    # ÉTÉ
    "été","été","été",
    # AUTOMNE
    "automne","automne","automne",
    # HIVER
    "hiver","hiver","hiver"
  ),
  humeur = c(
    "professionnel","chill","stylé",
    "professionnel","chill","stylé",
    "professionnel","chill","stylé",
    "professionnel","chill","stylé"
  ),
  haut = c(
    # printemps
    "Blouse légère","Cardigan léger","Trench",
    # été
    "Robe chemise","Débardeur","Crop top chic",
    # automne
    "Chemisier soie","Pull cachemire","Col roulé",
    # hiver
    "Pull col montant","Pull oversize","Pull graphique"
  ),
  bas = c(
    # printemps
    "Pantalon clair","Robe fluide","Jean boyfriend",
    # été
    "Pantalon fluide","Short jean","Pantalon palazzo",
    # automne
    "Pantalon costume","Jean slim","Jupe cuir",
    # hiver
    "Pantalon laine","Jogging chaud","Jean foncé"
  ),
  chaussures = c(
    # printemps
    "Mocassins","Baskets","Bottines",
    # été
    "Sandales talons","Espadrilles","Sandales plateformes",
    # automne
    "Bottines","Sneakers","Bottes motardes",
    # hiver
    "Bottes","Boots","Bottes fantaisie"
  ),
  propre = TRUE,
  nb_utilisations = 0,
  stringsAsFactors = FALSE
)

#### VÉRIFICATION

table(tenues$saison, tenues$humeur)

#### Température → saisons compatibles

saisons_compatibles <- function(temp) {
  saisons$saison[temp >= saisons$temp_min & temp <= saisons$temp_max]
}
####Sélection automatique d’une tenue

selection_tenue <- function(temp, humeur, tenues) {
  
  saisons_ok <- saisons_compatibles(temp)
  
  eligibles <- tenues[
    tenues$saison %in% saisons_ok &
      tenues$humeur == humeur &
      tenues$propre,
  ]
  
  if (nrow(eligibles) == 0) {
    return(NULL)
  }
  
  eligibles[sample(nrow(eligibles), 1), ]
}


####Automatisation “porter → sale”

porter_tenue <- function(tenue, tenues) {
  idx <- with(tenues,
              saison == tenue$saison &
                humeur == tenue$humeur &
                haut == tenue$haut &
                bas == tenue$bas &
                chaussures == tenue$chaussures)
  
  tenues$propre[idx] <- FALSE
  tenues$nb_utilisations[idx] <- tenues$nb_utilisations[idx] + 1
  
  tenues
}

####Fonction de lavage (simple)

laver_tout <- function(tenues) {
  tenues$propre <- TRUE
  tenues
}

####Test complet (preuve que tout marche)

temp <- 18
humeur <- "stylé"

tenue <- selection_tenue(temp, humeur, tenues)

if (!is.null(tenue)) {
  print(tenue)
  tenues <- porter_tenue(tenue, tenues)
}

####Interface Shiny

library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Sélecteur de tenue"),
  sidebarLayout(
    sidebarPanel(
      numericInput("temp", "Température (°C)", value = 20, min = -10, max = 40),
      selectInput("humeur", "Humeur", choices = c("professionnel", "chill", "stylé")),
      actionButton("valider", "Obtenir tenue"),
      actionButton("laver", "Laver toutes les tenues")
    ),
    mainPanel(
      h3("Tenue sélectionnée"),
      verbatimTextOutput("tenue_affiche"),
      h3("État des tenues"),
      tableOutput("etat_tenues")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # stockage réactif des tenues
  tenues_react <- reactiveVal(tenues)
  
  # Affichage tenue et mise à jour de l'état
  observeEvent(input$valider, {
    t <- selection_tenue(input$temp, input$humeur, tenues_react())
    
    if (is.null(t)) {
      output$tenue_affiche <- renderText("Aucune tenue disponible pour ces critères !")
    } else {
      output$tenue_affiche <- renderText({
        paste("Saison:", t$saison, "\n",
              "Haut:", t$haut, "\n",
              "Bas:", t$bas, "\n",
              "Chaussures:", t$chaussures)
      })
      
      # mettre à jour l'état des tenues proprement
      nouvelles_tenues <- porter_tenue(t, tenues_react())
      tenues_react(nouvelles_tenues)
    }
  })
  
  # Bouton laver tout
  observeEvent(input$laver, {
    nouvelles_tenues <- laver_tout(tenues_react())
    tenues_react(nouvelles_tenues)
    output$tenue_affiche <- renderText("Toutes les tenues sont lavées et propres !")
  })
  
  # Affichage de l'état interne des tenues (tableau)
  output$etat_tenues <- renderTable({
    tenues_react()
  })
}

# Lancer l'app
shinyApp(ui, server)
