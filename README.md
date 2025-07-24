# Player Clustering Analysis: Big 5 European Leagues (2024-2025)

This repository contains an R Shiny application that clusters football players from the Big 5 European Leagues (Premier League, La Liga, Serie A, Bundesliga, Ligue 1) for the 2024-2025 season, using performance metrics from FBref’s Player Standard Stats table (https://fbref.com/en/comps/Big5/stats/players/Big-5-European-Leagues-Stats). The app applies K-means and DBSCAN clustering algorithms to identify player archetypes (e.g., playmakers, defensive midfielders) and provides interactive visualizations and tables.

## Features
  * Data Source: player stats (goals, assists, progressive passes, tackles, dribbles) using the worldfootballR package.
  * Clustering Algorithms: Supports K-means (user-defined clusters) and DBSCAN (density-based clustering with adjustable parameters).
  * Archetype Identification: Assigns roles like Forward, Playmaker, Defensive Midfielder, Winger, or General Midfielder based on cluster characteristics.
  * Interactive Interface: Shiny app with scatter plots (ggplot2) and interactive tables (DT) for exploring clusters.
  * Robust Error Handling: Includes network timeouts, error messages, and a "Stop App" button to prevent console lock.

## Data
  * Source: FBref Player Standard Stats table for the 2024-2025 season (accessed July 24, 2025).
  * Metrics Used:
    * Goals per Match (Gls / MP)
    * Assists per Match (Ast / MP)
    * Progressive Passes per Match (PrgP / MP)
    * Tackles per Match (TklW / MP)
    * Dribbles per Match (Succ / MP)

  * Filters: Players with at least 5 matches played to ensure meaningful stats.

## Archetypes
Players are assigned archetypes based on cluster centroids:
  * Forward: High goals per match.
  * Playmaker: High assists or progressive passes per match.
  * Defensive Midfielder: High tackles per match.
  * Winger: High dribbles per match.
  * General Midfielder: Balanced stats.
  * Outlier (DBSCAN): Players not assigned to any cluster.

