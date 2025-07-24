library(shiny)
library(worldfootballR)
library(dplyr)
library(ggplot2)
library(DT)
library(cluster)
library(dbscan)

#create function to get data with timeout included
fetch_player_stats <- function() {
  tryCatch({
    #set timeout for network request
    options(timeout = 60)
    cat("Getting data from FBref...\n")
    player_stats <- fb_big5_advanced_season_stats(season_end_year = 2025, stat_type = "standard") %>%
      filter(Comp %in% c("Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1")) %>%
      mutate(
        player.name = Player,
        competition.competition_name = Comp,
        season.season_name = "2024/2025",
        matches_played = as.numeric(MP),
        goals_per_match = as.numeric(Gls) / pmax(matches_played, 1), #avoid dividing by zero
        assists_per_match = as.numeric(Ast) / pmax(matches_played, 1),
        prog_passes_per_match = as.numeric(PrgP) / pmax(matches_played, 1),
        tackles_per_match = as.numeric(TklW) / pmax(matches_played, 1),
        dribbles_per_match = as.numeric(Succ) / pmax(matches_played, 1)
      ) %>%
      filter(matches_played >= 5, !is.na(player.name)) %>%
      select(player.name, Pos, Squad, competition.competition_name, season.season_name, 
             matches_played, goals_per_match, assists_per_match, 
             prog_passes_per_match, tackles_per_match, dribbles_per_match)
    
    cat("Data fetched successfully. Rows:", nrow(player_stats), "\n")
    return(player_stats)
  }, error = function(e) {
    cat("Error getting data:", conditionMessage(e), "\n")
    return(NULL)
  })
}

#define archetype categpries based on cluster centroids
assign_archetype <- function(centroids) {
  archetypes <- apply(centroids, 1, function(row) {
    if (is.na(row["goals_per_match"])) return("Unknown")
    if (row["goals_per_match"] > mean(row["goals_per_match"], na.rm = TRUE) + sd(row["goals_per_match"], na.rm = TRUE)) {
      "Forward"
    } else if (row["assists_per_match"] > mean(row["assists_per_match"], na.rm = TRUE) + sd(row["assists_per_match"], na.rm = TRUE) ||
               row["prog_passes_per_match"] > mean(row["prog_passes_per_match"], na.rm = TRUE) + sd(row["prog_passes_per_match"], na.rm = TRUE)) {
      "Playmaker"
    } else if (row["tackles_per_match"] > mean(row["tackles_per_match"], na.rm = TRUE) + sd(row["tackles_per_match"], na.rm = TRUE)) {
      "Defensive Midfielder"
    } else if (row["dribbles_per_match"] > mean(row["dribbles_per_match"], na.rm = TRUE) + sd(row["dribbles_per_match"], na.rm = TRUE)) {
      "Winger"
    } else {
      "General Midfielder"
    }
  })
  return(archetypes)
}

#shiny UI
ui <- fluidPage(
  titlePanel("Player Clustering: Big 5 European Leagues 2024-2025"),
  sidebarLayout(
    sidebarPanel(
      selectInput("algorithm", "Clustering Algorithm:", choices = c("K-means", "DBSCAN")),
      conditionalPanel(
        condition = "input.algorithm == 'K-means'",
        sliderInput("k", "Number of Clusters (K):", min = 2, max = 10, value = 5)
      ),
      conditionalPanel(
        condition = "input.algorithm == 'DBSCAN'",
        sliderInput("eps", "DBSCAN eps:", min = 0.1, max = 2, value = 0.5, step = 0.1),
        sliderInput("minPts", "DBSCAN minPts:", min = 3, max = 20, value = 5)
      ),
      selectInput("x_metric", "X-axis Metric:", 
                  choices = c("Goals per Match" = "goals_per_match", 
                              "Assists per Match" = "assists_per_match",
                              "Progressive Passes per Match" = "prog_passes_per_match",
                              "Tackles per Match" = "tackles_per_match",
                              "Dribbles per Match" = "dribbles_per_match")),
      selectInput("y_metric", "Y-axis Metric:", 
                  choices = c("Goals per Match" = "goals_per_match", 
                              "Assists per Match" = "assists_per_match",
                              "Progressive Passes per Match" = "prog_passes_per_match",
                              "Tackles per Match" = "tackles_per_match",
                              "Dribbles per Match" = "dribbles_per_match"),
                  selected = "assists_per_match"),
      actionButton("stopApp", "Stop App")
    ),
    mainPanel(
      textOutput("status"),
      plotOutput("cluster_plot"),
      DTOutput("cluster_table")
    )
  )
)

#shiny Server
server <- function(input, output, session) {
  #get data once
  player_data <- reactiveVal(NULL)
  observe({
    data <- fetch_player_stats()
    if (!is.null(data)) {
      metrics <- data %>% 
        select(goals_per_match, assists_per_match, prog_passes_per_match, 
               tackles_per_match, dribbles_per_match) %>%
        scale()
      player_data(list(data = data, metrics = metrics))
      output$status <- renderText("Data loaded successfully.")
    } else {
      output$status <- renderText("Failed to load data. Try manual data loading.")
    }
  })
  
  #perform clustering
  clusters <- reactive({
    req(player_data())
    metrics <- player_data()$metrics
    if (input$algorithm == "K-means") {
      set.seed(123)
      kmeans_result <- kmeans(metrics, centers = input$k, nstart = 25)
      list(
        labels = kmeans_result$cluster,
        centroids = kmeans_result$centers,
        method = "K-means"
      )
    } else {
      dbscan_result <- dbscan(metrics, eps = input$eps, minPts = input$minPts)
      list(
        labels = dbscan_result$cluster,
        centroids = aggregate(metrics, by = list(cluster = dbscan_result$cluster), mean, na.rm = TRUE)[,-1],
        method = "DBSCAN"
      )
    }
  })
  
  #assign archetypes
  archetypes <- reactive({
    centroids <- clusters()$centroids
    if (nrow(centroids) > 0) {
      assign_archetype(centroids)
    } else {
      NULL
    }
  })
  
  #visualize cluster plot
  output$cluster_plot <- renderPlot({
    req(player_data())
    data <- player_data()$data
    cluster_labels <- clusters()$labels
    data$Cluster <- as.factor(cluster_labels)
    
    ggplot(data, aes_string(x = input$x_metric, y = input$y_metric, color = "Cluster")) +
      geom_point(size = 3, alpha = 0.6) +
      theme_minimal() +
      labs(title = paste(clusters()$method, "Clustering of Players"),
           x = input$x_metric, y = input$y_metric)
  })
  
  #display results table
  output$cluster_table <- renderDT({
    req(player_data())
    data <- player_data()$data
    cluster_labels <- clusters()$labels
    data$Cluster <- as.factor(cluster_labels)
    
    arch <- archetypes()
    if (!is.null(arch)) {
      data$Archetype <- arch[cluster_labels]
      if (clusters()$method == "DBSCAN") {
        data$Archetype[cluster_labels == 0] <- "Outlier"
      }
    } else {
      data$Archetype <- "Unknown"
    }
    
    datatable(data %>% 
                select(player.name, Pos, Squad, competition.competition_name, 
                       matches_played, goals_per_match, assists_per_match, 
                       prog_passes_per_match, tackles_per_match, dribbles_per_match, 
                       Cluster, Archetype),
              options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  #stop app when button is clicked
  observeEvent(input$stopApp, {
    stopApp()
  })
}

#run shiny
shinyApp(ui, server)
