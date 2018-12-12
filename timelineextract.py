import tweepy
import os
import csv
import sys
import subprocess
command = 'C:/Program Files/R/R-3.4.3/bin/Rscript'
path = 'C:/Users/user/Desktop/minor2/username/usernametry.r'
cmd = [command, path]

#twitter api
consumer_key = '794TI5L9FxMMwWgstcmAGqmqm'
consumer_secret = 'PkvlCxUDYGrJYkrrbvkYG8VzOmU0olUfBKOAYOvnJkCKkt75Qn'
access_key = '966365642193432576-PWcHLPyFXGySppwIC41ggEvWDppvpO9'
access_secret = 'DXby7jAaXiVFOBsBrms7JNksStxBmElR2MJGuLKnHvscj'

#take input from user
#value= sys.argv[1]
#user = unicode(value, "utf-8", errors="ignore")
#user = "DrAdwitiya"
user =input("enter user handle:")

#setting twitter connection
auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_key, access_secret)
api = tweepy.API(auth)
results = api.user_timeline(id = user,count=200,tweet_mode = 'extended')
#print(user)

#writing to file
with open('timelineextract.csv', 'wb') as csvfile:
    field = ['tweets','likes']
    
    f = csv.DictWriter(csvfile,fieldnames = field)
    f.writeheader()
    for tweet in results:
        if(tweet.in_reply_to_status_id == None):
            x = (tweet.full_text)
            z = x.encode("ascii",'ignore')
            y = (tweet.favorite_count)
            f.writerow({'tweets':z,'likes' :y})
#print("csv file is created for all the tweets u have posted")
#print("\n")
csvfile.close()
"""try:
   process = subprocess.Popen(cmd,shell = False)
   process.wait()
   print("\n")
   print("I hope you are happy with the results!!!")
except subprocess.CalledProcessError as e:
    raise RuntimeError("command '{}' return with error (code {}): {}".format(e.cmd, e.returncode, e.output))
os.system("pause")"""
    
