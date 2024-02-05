## Corporate Essence Analyses ##
## Scraping Twitter

import snscrape.modules.twitter as sntwitter
import pandas as pd
import os

# --------------------------------------------
# Get current working directory
cwd = os.getcwd()
os.chdir(cwd)

# --------------------------------------------
# Define variables
comp_n = 20

# Read in company names
comp_df = pd.read_csv("data/Fortune_1000.csv") #

# Create list of companies
comp_series = comp_df[
    (comp_df["rank"] <= comp_n) |
    (comp_df["rank"] > (len(comp_df) - comp_n))
    ]["company"]

comp_list = comp_series.tolist()

# Append "Big Companies" in general
comp_list.append("Big Companies")

# --------------------------------------------
# Scrape Twitter for mention of company names

# Creating list to append tweet data to
tweets_list = []

# Using TwitterSearchScraper to scrape data and append tweets to list
for comp_name in comp_list:
    for i, tweet in enumerate(sntwitter.TwitterSearchScraper(comp_name).get_items()):
        if i >= 100:
            break
        tweets_list.append([comp_name, tweet.date, tweet.id, tweet.content, tweet.user.username])

# Creating a dataframe from the tweets list above
tweets_df = pd.DataFrame(tweets_list, columns=["Company", "Datetime", "Tweet_Id", "Text", "Username"])
# print(tweets_df)
# print(tweets_df["Text"])

# --------------------------------------------
# Save to file
tweets_df.to_csv("comp_tweets.csv")

# --------------------------------------------

## END ##