from social_blade_creds import *
import requests
from pdb import set_trace
import pandas as pd

twitter_url = 'https://matrix.sbapis.com/b/twitter/statistics'

def twitter_auth_and_connect(user):
    headers = {"query": user, "history": 'default', 'clientid': my_client_id, 'token': my_access_token}
    response = requests.request("GET", twitter_url, headers=headers)
    return response.json()

def get_tweets(person_list):
    tweet_counts = []
    for person in person_list:

        res_json = []
        
        while len(res_json) == 0:
            res_json = twitter_auth_and_connect(person)
        
        #get tweet counts at end of april 24 and 26
        print(person)
        tweet_counts.append({'person': person, 'date1': res_json['data']['daily'][15]['date'], 'date1_followers': res_json['data']['daily'][15]['followers'], 'date1_tweets': res_json['data']['daily'][15]['tweets'],
                             'date2': res_json['data']['daily'][13]['date'], 'date2_followers': res_json['data']['daily'][13]['followers'], 'date2_tweets': res_json['data']['daily'][13]['tweets']})

    return tweet_counts

if __name__ == "__main__":
    senators = pd.read_excel('congress_twitter_040722.xlsx', sheet_name = [0])
    senator_counts = get_tweets(senators[0]['handle'])
    senator_counts = pd.DataFrame(senator_counts)
    senator_out = pd.concat([senators[0], senator_counts.iloc[:,1:]], axis=1)

    senator_out.to_csv("senators_wt_followers.csv") 
