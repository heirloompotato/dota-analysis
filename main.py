import requests
import json
import pymysql

# Connection to MySQL local database
connection = pymysql.connect(host = "localhost", user = "****", password = "****" , db = "dotadb")

# Three different API request types will be used.
session = requests.Session()
accountID = 91506696
playermatcheslink = "https://api.opendota.com/api/players/{}/matches".format(accountID)
matchesinfolink = "https://api.opendota.com/api/matches/{matchID}"
heroesinfolink = "https://api.opendota.com/api/heroes"

# The following API request uses my account ID and fetches data for all the matches I've played
wintype = [1,0]
matchdict = {}

# Data on whether I won is captured in a parameter, thus there are two calls, first picking wins (1) and losses (0)
for winnumber in wintype:
    # Calling and parsing JSON
    response = session.get(playermatcheslink, params={"win": winnumber})
    matches = json.loads(response.content.decode("utf-8"))

    for match in matches:
            # Inserting data into the dotamatches table in database
            query = """ INSERT INTO dotamatches (win, match_id, player_slot, radiant_win, duration, game_mode, lobby_type, hero_id,
                    start_time, version, kills, deaths, assists, skill, average_rank, leaver_status, party_size)
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"""

            connection.cursor().execute(query, (winnumber, match["match_id"], match["player_slot"], match["radiant_win"],
                                                match["duration"], match["game_mode"], match["lobby_type"], match["hero_id"],
                                                match["start_time"], match["version"], match["kills"], match["deaths"],
                                                match["assists"], match["skill"], match["average_rank"], match["leaver_status"],
                                                match["party_size"]))

            connection.commit()

            # Data on whether I was on team radiant or dire for each match is captured in this loop, this is useful subsequently
            if winnumber + match["radiant_win"] == 1:
                matchdict[match["match_id"]] = False

            else:
                matchdict[match["match_id"]] = True

# The following API request fetches more data on the heroes on my team in each match, since this information is not captured above
heroidlist = []

for mID, team in matchdict.items():
    # Calling for each individual match ID and parsing JSON
    response = session.get(matchesinfolink.format(matchID = mID))
    matchinfo = json.loads(response.content.decode("utf-8"))

    # Searches through each player in a match, and if the player is in the same team as me, the hero ID is captured
    for player in matchinfo["players"]:
        if player["isRadiant"] is team:
            heroidlist += [player["hero_id"]]

    # Inserting data into dotamatchesinfo table in database
    query = """INSERT INTO dotamatchesinfo (match_id, heroID1, heroID2, heroID3, heroID4, heroID5)
            VALUES (%s, %s, %s, %s, %s, %s)"""

    connection.cursor().execute(query,(mID, heroidlist[0], heroidlist[1], heroidlist[2], heroidlist[3], heroidlist[4]))

    connection.commit()

    # Resetting hero ID list
    heroidlist = []

# This API request fetches basic information on the heroes
# Calling and parsing JSON
response = session.get(heroesinfolink)
heroesinfo = json.loads(response.content.decode("utf-8"))

for hero in heroesinfo:
    # Roles of the heroes are omitted, because more detailed role information is captured during web scraping
    # Inserting data into heroesinfo table in database
    query = """INSERT INTO heroesinfo (id, name, localized_name, primary_attr, attack_type)
                VALUES (%s, %s, %s, %s, %s)"""

    connection.cursor().execute(query, (hero["id"], hero["name"], hero["localized_name"], hero["primary_attr"], hero["attack_type"]))

    connection.commit()