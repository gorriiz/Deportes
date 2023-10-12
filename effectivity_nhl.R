library(hockeyR)
library(tidyverse)
library(sportyR)
library(gt)
library(gtExtras)
#library(nflreadr)

pbp_nhl <- load_pbp("2022-23")


tbl_nhl <- pbp_nhl |> 
  group_by(event_team) |> 
  summarize(
    `goals/game` = (sum(event_type == "GOAL", na.rm = T) / 82),
    `xG/game` = (round(sum(xg, na.rm = T),1) / 82),
    `goals/shot %` = (round(sum(event_type == "GOAL", na.rm = T)) / sum(event_type == "GOAL" | event_type == "BLOCKED_SHOT" |
                                                                      event_type == "SHOT" | event_type == "MISSED_SHOT") * 100)
  ) |> 
  slice(1:32) |> 
  arrange(-`goals/game`) |> 
  gt() |> 
  data_color(
    columns = c(`goals/shot %`, `goals/game`, `xG/game`),
    fn = scales::col_numeric(
      palette = c("#59a14f", "#edc948", "#e15759"),
      domain = NULL,
      reverse = TRUE
    )
  ) |>  
  opt_align_table_header(align = "center") |> 
  cols_align("center") |> 
  opt_row_striping() |> 
  cols_label(event_team = "Team") |> 
  gt_theme_espn() |> 
  tab_header(title = md("Efectividad de Equipos NHL temporada 2022-2023"),
             subtitle = "Goles y Goles esperados por partido y Porcentaje de goles por disparo") |> 
  tab_source_note("Tabla: @gorriz_ | Data: hockeyR | Code: @arjunmenon100 | Inspiration: @Marcus_Mosher")

gtsave(tbl_nhl, "nhl_goals_2022.png")

tbl_nhl
