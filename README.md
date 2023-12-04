# Dota Match Analysis
Dota 2 is a multiplayer online battle arena (MOBA) video game that I played primarily between 2014 and 2020. Data collection is first done using the OpenDota API and a Dota 2 Wikipedia page, followed by some data cleanup. Next, Exploratory Data Analysis (EDA) is done on R to understand what factors improve my odds of winning.

## Directory Layout
- `Dota-Match-Analysis.md` contains the analysis conducted
- `Dota-Match-Analysis.Rmd` contains the R code used for analysis
- `main.py` makes the API call to the open-source platform [OpenDota API](https://docs.opendota.com/)
- `webscrape.py` scrapes data from a Dota 2 Wikipedia page [Liquipedia](https://liquipedia.net/dota2/Hero_Roles)
- `Dota-Match-Analysis_files` contains the data collected
