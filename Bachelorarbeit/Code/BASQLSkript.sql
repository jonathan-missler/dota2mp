SELECT
        
xp_per_min xp_per_min,gold_per_min gold_per_min,gold_t[11] gold_t10, xp_t[11] xp_t10, dn_t[11] dn_t10,radiant_win,radiant_team_id,dire_team_id,
matches.match_id,
((player_matches.player_slot < 128) = matches.radiant_win) win,
player_matches.hero_id,
player_matches.account_id

FROM matches
JOIN match_patch using(match_id)
JOIN leagues using(leagueid)
JOIN player_matches using(match_id)
JOIN heroes on heroes.id = player_matches.hero_id
LEFT JOIN notable_players ON notable_players.account_id = player_matches.account_id
LEFT JOIN teams using(team_id)
WHERE TRUE
AND xp_per_min IS NOT NULL AND gold_t[11] IS NOT NULL AND xp_t[11] IS NOT NULL AND dn_t[11] IS NOT NULL AND gold_per_min IS NOT NULL 
AND matches.start_time >= extract(epoch from timestamp '2018-12-31T23:00:00.000Z')
AND matches.start_time <= extract(epoch from timestamp '2019-08-24T23:00:00.000Z')
ORDER BY matches.match_id NULLS LAST
LIMIT 100000