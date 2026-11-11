setwd("/users/fsobo15/desktop/git/nfl.fio/workspace")
required_packages <- c("tidyverse", "devtools", "RSQLite", "DBI", "lubridate", "paws", "nflreadr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
library("dplyr")
library("tidyverse")
library("nflreadr")

all_data <- c("kaggle_base_data/combine_results",
              "kaggle_base_data/combine_tracking",
              "kaggle_base_data/game_tracking_2023",
              "kaggle_base_data/game_tracking_2024",
              "kaggle_base_data/game_tracking_2025",
              "kaggle_base_data/player_career_successes",
              "kaggle_base_data/player_play",
              "kaggle_base_data/players",
              "kaggle_base_data/games")

extra_data <- c("player_weekly_snap_counts/team_snap_counts_all_teams_2023", 
                "player_weekly_snap_counts/team_snap_counts_all_teams_2024",
                "player_weekly_snap_counts/team_snap_counts_all_teams_2025")


download_all <- function(file_names, folder = "/users/fsobo15/desktop/git/NFL.FIO/db/") {
  results <- vector("list", length(file_names))
  names(results) <- basename(file_names)
 
   
  for (i in seq_along(file_names)) {
  results[[i]] <- read.csv(paste0(folder, 
                                  file_names[i],
                                  ".csv"), 
                           header = TRUE)
  }
  return(results)
}

all_tables <- download_all(all_data)
new <- download_all(extra_data)

id_bridge <- load_players() |>
  select(display_name, pfr_id, gsis_id, nfl_id) |>
  filter(!is.na(pfr_id), !is.na(gsis_id)) |>
  distinct()

need_names <- all_tables[1:7]

naming <- function(unnamed, players) {
  
  players_staging <- players |>
    distinct(nfl_id, display_name) |>
    mutate(nfl_id = as.character(nfl_id))
  
  for (i in seq_along(unnamed)) {
    unnamed[[i]] <- unnamed[[i]] |>
      mutate(nfl_id = as.character(nfl_id)) |>
      left_join(players_staging,
                by = join_by(nfl_id),
                relationship = "many-to-one") |>
      relocate(display_name, .after = nfl_id)
  }
  return(unnamed)
}
    
named_tables <- naming(need_names, all_tables[["players"]])

years <- c(2023, 2024, 2025)

snap_counter <- function(year, folder = "/users/fsobo15/desktop/git/NFL.FIO/db/",
                         recode_teams = c()) {

  out_dirs <- c("team_weekly_phase_breakdown/major_detail",
                "team_weekly_phase_breakdown/minor_detail", 
                "player_stats_and_snaps")
  
  for (d in out_dirs) {
    dir.create(paste0(folder, d), recursive = TRUE, showWarnings = FALSE)
  }
  
  sc_year <- load_snap_counts(seasons = year)
  
  if (length(recode_teams) > 0) {
    sc_year <- sc_year |> mutate(team = recode(team, !!!recode_teams))
  }
  
  team_snaps_year <- sc_year |>
    group_by(season, week, game_id, team) |>
    summarise(
      off_snaps = round(max(offense_snaps[offense_pct == max(offense_pct)] / max(offense_pct))),
      def_snaps = round(max(defense_snaps[defense_pct == max(defense_pct)] / max(defense_pct))),
      st_snaps_check = round(max(st_snaps[st_pct == max(st_pct)] / max(st_pct))),
      .groups = "drop"
    )
    
    st_from_pbp_year <- load_pbp(year) |>
      filter(special_teams_play == 1 | play_type_nfl == "FIELD_GOAL") |>
      count(season, week, game_id, posteam, defteam, name = "st_plays")
      
    individual_snap_counts_year <- sc_year |>
      arrange(week) |>
      group_by(season, pfr_player_id) |>
      summarise(player = last(player),
                offense_snaps = sum(offense_snaps, na.rm = TRUE), 
                defense_snaps = sum(defense_snaps, na.rm = TRUE), 
                st_snaps = sum(st_snaps, na.rm = TRUE), 
                .groups = "drop")
    
    sc_teams <- unique(team_snaps_year$team)
    pbp_teams <- unique(na.omit(c(st_from_pbp_year$posteam, st_from_pbp_year$defteam)))
    mismatch <- union(setdiff(sc_teams, pbp_teams), setdiff(pbp_teams, sc_teams))
    if(length(mismatch) > 0) {
      stop("Team abbreviations don't match between sources: ",
           paste(mismatch, collapse = ", "),
           ". Add them to recode_teams e.g. c(LAR = \"LA\".")
    }
    
    od_year <- team_snaps_year |>
      select(season, week, team, Offense = off_snaps, Defense = def_snaps) |>
      pivot_longer(c(Offense, Defense), names_to = "phase", values_to = "snaps")
    
    st_year <- bind_rows(
      st_from_pbp_year |> filter(!is.na(posteam)) |>
        group_by(season, week, team = posteam) |>
        summarise(snaps = sum(st_plays), .groups = "drop") |>
        mutate(phase = "ST (starting with possession)"),
      st_from_pbp_year |> filter(!is.na(defteam)) |>
        group_by(season, week, team = defteam) |>
        summarise(snaps = sum(st_plays), .groups = "drop") |>
        mutate(phase = "ST (starting without possession)")
    )
    snaps_long_year <- bind_rows(od_year, st_year)
    
    snaps_long_year <- snaps_long_year |>
      bind_rows(
        snaps_long_year |>
          group_by(season, week, team) |>
          summarise(snaps = sum(snaps), .groups = "drop") |>
          mutate(phase = "Total")
      ) |>
      group_by(season, week, team) |> 
      mutate(pct = snaps / snaps[phase == "Total"]) |>
      ungroup()
    
    teams <- load_teams() |> select(team = team_abbr, team_name)
    snaps_long_year <- snaps_long_year |> left_join(teams, by = "team")
    
    snaps_long_year |> filter(is.na(team_name)) |> distinct(team)
    
    unmatched <- snaps_long_year |> filter(is.na(team_name)) |> distinct(team) |> pull(team)
    if (length(unmatched) > 0) {
      warning("No team name found for: ", paste(unmatched, collapse = ", "))
    }
    
    season_totals_year <- snaps_long_year |>
      group_by(season, team, phase) |>
      summarise(season_snaps = sum(snaps), .groups = "drop") |>
      group_by(season, team) |>
      mutate(season_pct = season_snaps / season_snaps[phase == "Total"]) |>
      ungroup()
    
    phase_order <- c("Offense", "Defense", "ST (starting with possession)", "ST (starting without possession)", "Total")
    
    snaps_wide_year <- snaps_long_year |>
      arrange(week) |>
      pivot_wider(
        id_cols = c(season, team, team_name, phase), 
        names_from = week,
        values_from = c(snaps, pct),
        names_glue = "wk{week}_{.value}",
        names_vary = "slowest"
      ) |>
      left_join(season_totals_year, by = c("season", "team", "phase")) |>
      mutate(phase = factor (phase, levels = phase_order)) |>
      arrange(season, team, phase)
    
      all_stats_year <- load_player_stats(seasons = year, summary_level = "reg+post") |>
        filter(!is.na(player_id))
      
      full_stats_year <- individual_snap_counts_year |>
        left_join(id_bridge, by = c("pfr_player_id" = "pfr_id"),
                  relationship = "many-to-one", na_matches = "never") |>
        left_join(all_stats_year, by = c("gsis_id" = "player_id", "season"),
                  relationship = "one-to-one", na_matches = "never")
    
      write.csv(snaps_wide_year, paste0(folder, "team_weekly_phase_breakdown/major_detail/team_snaps_", year, ".csv"),
                  row.names = FALSE)
      write.csv(snaps_long_year, paste0(folder, "team_weekly_phase_breakdown/minor_detail/team_snaps_long_", year, ".csv"),
                  row.names = FALSE)
      write.csv(full_stats_year, paste0(folder, "player_stats_and_snaps/player_stats_snaps_", year, ".csv"), 
                row.names = FALSE)
      
      invisible(list(long = snaps_long_year, 
                     wide = snaps_wide_year,
                     team_check = team_snaps_year,
                     full_stats = full_stats_year))
}

s23 <- snap_counter(2023)
s24 <- snap_counter(2024)
s25 <- snap_counter(2025)


#s25_staging <- s25[["wide"]] |>
 # select(team, phase, season_snaps) |>
  #right_join(s25[["full_stats"]],
   #       by = c("team" = "recent_team"),
    #      relationship = "one-to-many"),
#for phase == offense, snaps == offense_snaps
#for phase == defense, snaps == defense_snaps
#for phase == 

plays <- load_pbp(seasons = 2023) |>
  select(game_id,
         play_id, 
         home_team, 
         away_team, 
         season_type, 
         posteam, 
         defteam, 
         play_type)
plays <- plays[plays$play_id > 1, ]

presnap_fouls <- c("False Start", 
                   "Delay of Game", 
                   "Encroachment", 
                   "Neutral Zone Infraction", 
                   "Illegal Substitution", 
                   "Defensive Too Many Men on Field", 
                   "Offensive Too Many Men on Field", 
                   "Delay of Kickoff",
                   "Defensive Delay of Game")

messy_types <- c("PENALTY", 
                 "UNSPECIFIED", 
                 "FUMBLE_RECOVERED_BY_OPPONENT")

players_on_play <- function(year, presnap_fouls, messy_types) {
  pbp_types <- load_pbp(seasons = year) |>
    select(game_id, play_id, play_type_nfl, play_type, desc, penalty_type, special_teams_play)
  
  snap_detail <- load_participation(seasons = year) |>
    left_join(pbp_types, 
              by = join_by(nflverse_game_id == game_id, play_id), 
              relationship = "one-to-one", 
              na_matches = "never") |>
    separate_wider_delim(nflverse_game_id, delim = "_",
                         names = c(NA, NA, "away_team", "home_team"),
                         cols_remove = FALSE) |>
    mutate(def_team = if_else(possession_team == home_team, away_team, home_team))
  
  player_plays <- snap_detail |>
    select(nflverse_game_id, play_id, possession_team, def_team, 
           play_type_nfl, play_type, desc, penalty_type,
           offense_formation, offense_personnel, defense_personnel, 
           offense_players, defense_players) |>
    pivot_longer(c(offense_players, defense_players),
                 names_to = "side", values_to = "gsis_id") |>
    separate_longer_delim(gsis_id, delim = ";") |>
    filter(!is.na(gsis_id), gsis_id != "") |>
    mutate(
      team = if_else(side == "offense_players", possession_team, def_team),
      
      no_snap = coalesce(penalty_type %in% presnap_fouls & !str_detect(desc, "declined"), FALSE),
      
      st_sub = case_when(
        str_detect(defense_personnel, "\\bK\\b") ~"kickoff",
        str_detect(offense_personnel, "\\bK\\b") ~"fg_xp",
        str_detect(offense_personnel, "\\bP\\b") ~"punt",
        .default = NA_character_
      ),
      
      play_cat = case_when(
        no_snap ~NA_character_,
        play_type_nfl == "KICK_OFF" ~ "kickoff",
        play_type_nfl == "PUNT" ~ "punt",
        play_type_nfl %in% c("FIELD_GOAL", "XP_KICK") ~ "fg_xp",
        play_type_nfl %in% c("PASS", "RUSH", "SACK" , "PAT2", "INTERCEPTION") ~ "scrimmage",
        play_type_nfl %in% messy_types & !is.na(offense_formation) ~ "scrimmage", 
        play_type_nfl %in% messy_types ~ st_sub,
        .default = NA_character_
      ),
      
      snap_type = case_when(
        play_cat == "scrimmage" & side == "offense_players" ~ "Offense",
        play_cat == "scrimmage" & side == "defense_players" ~ "Defense",
        play_cat == "kickoff" & side == "offense_players" ~ "Kick return",
        play_cat == "kickoff" & side == "defense_players" ~ "Kickoff coverage",
        play_cat == "punt" & side == "offense_players" ~ "Punt coverage",
        play_cat == "punt" & side == "defense_players" ~ "Punt return",
        play_cat == "fg_xp" & side == "offense_players" ~"FG/XP unit",
        play_cat == "fg_xp" & side == "defense_players" ~"FG/XP block",
        .default = NA_character_
      )
    )
  
  return(list(snap_detail = snap_detail, player_plays = player_plays))
}

snap_detail_2023 <- players_on_play(2023, presnap_fouls, messy_types)
snap_detail_2024 <- players_on_play(2024, presnap_fouls, messy_types)
snap_detail_2025 <- players_on_play(2025, presnap_fouls, messy_types)
  

j <- snap_detail_2023$snap_detail |>
  filter(play_type_nfl == "UNSPECIFIED") |>
  select(desc, play_type, penalty_type, offense_formation, offense_personnel) |>
  head(30)
  
