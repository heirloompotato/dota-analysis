import pandas as pd
from bs4 import BeautifulSoup
import requests

# More detailed information on the hero roles and complexity can be found on this wiki page, the HTML is parsed here
link = "https://liquipedia.net/dota2/Hero_Roles"
page = requests.get(link)
soup = BeautifulSoup(page.text, features="html.parser")

heroattributes = {}

# The specific role and level of the role is found in each header
for roleheader in soup.find_all("th"):
    rolestring = roleheader.get_text()

    # Each ■ denotes how strong a hero is in the particular role
    level = rolestring.count("■")
    role = rolestring.replace("■", "").strip()

    # Captures the role and level in a dictionary
    if role in heroattributes:
        heroattributes[role].update({level: []})

    else:
        heroattributes[role] = {level: []}

    # Adds heroes into the specific role and level
    for hero in roleheader.find_next("td").find_all("a"):
        heroname = hero["title"]
        heroattributes[role][level] += [heroname]

# The dictionary will be converted into a list that can be converted to a csv for export
dfheroattributes = []
totalrolelist = []

for role in heroattributes:
    # A list of all the available roles is accumulated and captured, for use subsequently
    totalrolelist += [role]

    for level in heroattributes[role]:
        for hero in heroattributes[role][level]:
            # A new column for the heroes is created, with adjacent columns denoting all the roles
            herochecker = 0
            for dfrow in dfheroattributes:
                if dfrow["Hero"] == hero:
                    dfrow[role] = level
                    herochecker = 1
                    break
            if herochecker == 0:
                dfheroattributes += [{"Hero": hero, role: level}]

# Heroes do not appear in the role table if they are not even considered strong in it. A zero is allocated for this
for role in totalrolelist:
    for dfrow in dfheroattributes:
        if role not in dfrow:
            dfrow[role] = 0

# Exporting list as csv
df = pd.DataFrame(dfheroattributes)
df.to_csv("dotaheroroles.csv", index=False)
