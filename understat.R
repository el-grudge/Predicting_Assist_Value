library(understatr)

league_seasons <- get_league_seasons("EPL")
View(league_seasons)

league_team_stats <- get_league_teams_stats(league_name = "EPL", year = 2018)
View(league_team_stats)

leagues_meta <- get_leagues_meta()
View(leagues_meta)

match_shots <- get_match_shots(match_id = 11662)
View(match_shots)

match_stats <- get_match_stats(match_id = 11662)
View(match_stats)

player_matches_stats <- get_player_matches_stats(player_id = 882)
View(player_matches_stats)

player_seasons_stats <- get_player_seasons_stats(player_id = 882)
View(player_seasons_stats)

player_shots <- get_player_shots(player_id = 882)
View(player_shots)

team_meta <- get_team_meta(team_name = "Newcastle United")
View(team_meta)

team_players_stats <- get_team_players_stats(team_name = "Newcastle United", year = 2018)
View(team_players_stats)
