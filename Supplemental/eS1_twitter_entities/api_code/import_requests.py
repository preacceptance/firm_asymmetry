import requests
import os
import json
from twitter_api_keys import bearer_token
from pdb import set_trace
import pandas as pd
import ast
import yaml
import matplotlib.pyplot as plt
import csv
import numpy as np
import math
from scipy import stats

# language detector
import spacy
from spacy.language import Language
from spacy_langdetect import LanguageDetector

def get_lang_detector(nlp, name):
    return LanguageDetector()

nlp = spacy.load("en_core_web_sm")
Language.factory("language_detector", func=get_lang_detector)
nlp.add_pipe('language_detector', last=True)

# sentiment analyzer
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()

def create_twitter_url_mentions(mention):
    max_results = 100
    mrf = "max_results={}".format(max_results)
    q = "query=" + mention
    url = "https://api.twitter.com/2/tweets/search/recent?{}&{}".format(
        mrf, q
    )
    return url

def twitter_auth_and_connect(bearer_token, url):
    headers = {"Authorization": "Bearer {}".format(bearer_token)}
    response = requests.request("GET", url, headers=headers)
    return response.json()

# def get_companies():
#     small = []
#     big = []
#     companies = pd.read_excel('entities.xlsx', sheet_name = [0])[0]

#     for i in range(0, companies.shape[0]):
#             if (i <= 99) & (companies['handle'][i] != 'na'): #20 
#                 big.append(companies['handle'][i])
#             if (i >= 899) & (companies['handle'][i] != 'na'): #979
#                 small.append(companies['handle'][i])
#     print(big)
#     print(len(big))
#     print(small)
#     print(len(small))
#     return big, small

def get_tweets(entities, entity_name):
    print('getting tweets: ', entity_name)

    tweets = []
    for entity in entities:
        if entity != entity:
            continue
        
        res_json = []
        
        while len(res_json) == 0:
            url = create_twitter_url_mentions(entity)
            res_json = twitter_auth_and_connect(bearer_token, url)
        
        if 'data' in res_json:
            for t in res_json['data']:
                text = t['text']
                ps = analyzer.polarity_scores(text) #sentiment
                doc = nlp(text) #is it english? 
                if (doc._.language['language'] == 'en') & (doc._.language['score'] > 0.85): 
                    try:
                        tweets.append({'entity': entity, 'text':text, 'sentiment':ps['compound']})
                    except:
                        set_trace()
    return tweets

def get_descriptives(tweets, entity_name):
    print('getting descriptives: ', entity_name)

    df = pd.DataFrame(tweets)
    counts = df.groupby('entity').count().reset_index()
    means = df.groupby('entity').mean().reset_index()
    means['count'] = counts['sentiment']
    mean = np.mean(means['sentiment'][means['count'] >= 10])
    sem = stats.sem(means['sentiment'][means['count'] >= 10])

    print('mean: ', mean)
    print('sem: ', sem)
    print('# entries: ', str(len(means['sentiment'])))

    return mean, sem, means[means['count']>=10]

def plot(entity_names, means, sems):
    print('plotting...')
    plt.bar(entity_names, means,yerr=sems)
    plt.xlabel("Entity", fontsize=20)
    plt.ylabel("Tweet Sentiment", fontsize=20)
    plt.savefig('new_plot.png')


def main():

    # get entity names
    xl = pd.ExcelFile('entities.xlsx')
    entities = xl.sheet_names
    num_entities = len(entities) 

    # initialie dictionaries
    dd = {}; tweets = {}; means = {}; sems = {}; stats = {}

    # process data for remaining entities
    for i in range(2,num_entities-2):
        dd[i] = pd.read_excel('entities.xlsx', sheet_name = [i])[i]
        tweets[i] = get_tweets(dd[i]['handle'], entities[i])
        means[i], sems[i], stats[i] = get_descriptives(tweets[i], entities[i])

    set_trace()
    plot(entities, means, sems)

    print('SAVING...')
    with pd.ExcelWriter('output.xlsx') as writer:
        for i in range(1,num_entities):
            tweets[i].to_excel(writer, sheet_name=entities[i])

    
if __name__ == "__main__":
    #mode = 'mentions'
    main()