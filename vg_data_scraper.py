from bs4 import BeautifulSoup
import requests
import pandas as pd

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

pages = 50
genres=['Action', 'Action-Adventure', 'Adventure', 'Board+Game', 'Education', 'Fighting', 'Misc', 'MMO', 'Music', 'Party', 'Platform', 'Puzzle', 'Racing', 'Role-Playing', 'Sandbox', 'Shooter', 'Simulation', 'Sports', 'Strategy', 'Visual+Novel']
rank = []
gname = []
platform = []
release_date = []
publisher = []
sales_na = []
sales_eu = []
sales_jp = []
sales_ot = []
sales_gl = []
developer= []
total_shipped=[]
game_url=[]
game_genre=[]
game_url_string=[]
count=0
fails=0

urlhead = 'http://www.vgchartz.com/games/games.php?page='
urlmid = '&results=200&name=&console=&keyword=&publisher=&genre='
urltail = '&order=Sales&ownership=Both&boxart=Both&banner=Both&showdeleted=&region=All&goty_year=&developer=&direction=DESC&showtotalsales=1&shownasales=1&showpalsales=1&showjapansales=1&showothersales=1&showpublisher=1&showdeveloper=1&showreleasedate=1&showlastupdate=1&showvgchartzscore=1&showcriticscore=1&showuserscore=1&showshipped=1&alphasort=&showmultiplat=No'

for genre in genres:
    for page in range(1,pages):
        surl = urlhead + str(page) +urlmid +genre + urltail    
        r = requests.get(surl)
        r = r.text
        soup = BeautifulSoup(r,'html.parser')
        for row in soup.find_all('tr'):
            try:
                col=row.find_all('td')
                col_0=col[0].text
                col_4=col[4].text
                col_5=col[5].text
                col_9=col[6].text
                col_10=col[10].text
                col_11=col[11].text
                col_12=col[12].text
                col_13=col[13].text
                col_14=col[14].text
                col_15=col[15].text
                img = col[3].find('img')
                col_3=img['alt']
                a_tag=col[2].find('a')
                url_col=a_tag['href']
                col_2=(a_tag.text)
                url_string=url_col.rsplit('/', 2)[1]

                if len(col_0)<6:
                    rank.append(col_0)
                    gname.append(col_2)
                    publisher.append(col_4)
                    developer.append(col_5)
                    total_shipped.append(col_9)
                    sales_gl.append(col_10)
                    sales_na.append(col_11)
                    sales_eu.append(col_12)
                    sales_jp.append(col_13)
                    sales_ot.append(col_14)
                    release_date.append(col_15)
                    platform.append(col_3)
                    game_url.append(url_col)
                    game_genre.append(genre)
                    game_url_string.append(url_string)
                    count+=1
            except:
                fails+=1
                continue
print('vg_chartz count = '+str(count))
print('vg_chartz fails = '+str(fails))
        
columns = {'total_shipped' : total_shipped,
           'developer' : developer,
           'rank': rank,
           'name': gname,
           'platform': platform,
           'release_date': release_date,
           'publisher': publisher,
           'na_sales':sales_na,
           'eu_sales': sales_eu,
           'jp_sales': sales_jp,
           'other_sales':sales_ot,
           'global_sales':sales_gl,
           'game_genre':game_genre,
           'game_url':game_url,
           'game_url_string':game_url_string}

df = pd.DataFrame(columns)
df = df[['total_shipped','developer','rank','name','platform','release_date','publisher','na_sales','eu_sales','jp_sales','other_sales','global_sales','game_genre','game_url','game_url_string']]
df.to_csv("vgsales_full_third.csv",sep=",",encoding='utf-8')

#Rewording platforms from vgchartz wording to metacritic wording for use in url
platform_rewording_dict = {'PS3': 'playstation-3',
					   'X360': 'xbox-360',
					   'PC': 'pc',
					   'WiiU': 'wii-u',
					   '3DS': '3ds',
					   'PSV': 'playstation-vita',
					   'iOS': 'ios',
					   'Wii': 'wii',
					   'DS': 'ds',
					   'PSP': 'psp',
					   'PS2': 'playstation-2',
					   'PS': 'playstation',
					   'XB': 'xbox', 
					   'GC': 'gamecube',
					   'GBA': 'game-boy-advance',
					   'DC': 'dreamcast',
                       'PS4':'playstation-4',
                       'XOne':'xbox-one',
                       'NS':'switch'
					   }

#filtering for games that have at least 10,000 sales 
mask=df['global_sales']!='N/A'
url_search=df[mask]

#creating a list of metacritic urls  using game url string and platform to scrape for user score and further data 
meta_full_url_list=[]
meta_url= None
index=list(range(0,len(url_search)))
url_search.index=index

for row in range(0,len(url_search)):
    plat_temp=url_search.loc[row,'platform']
    url_string_temp=url_search.loc[row,'game_url_string']
    if plat_temp in platform_rewording_dict.keys():
        meta_url='https://www.metacritic.com/game/'+platform_rewording_dict[plat_temp]+'/'+url_string_temp
        meta_full_url_list.append(meta_url)
        

#=============================================================================
#Meta_critic_scraper 
#=============================================================================
meta_game_name=[]
meta_user_score=[]
meta_critic_score=[]
meta_esrb=[]
meta_full_url=[]
meta_platform=[]
meta_publisher=[]
meta_release_date=[]
meta_genre=[]
meta_error_url=[]
meta_multiplayer=[]
count=0
fails=0

headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'}

for i in range(0,len(meta_full_url_list)):
    url= meta_full_url_list[i]
    r = requests.get(url, headers=headers)
    r = r.text
    soup = BeautifulSoup(r,'html.parser')
    critic_score_data='N/A'
    multiplayer_data='no'
    try:
        title=soup.find('div',class_='product_title')
        title_a_tag=title.find('a')
        title_data=title_a_tag.text
        
        platform=soup.find('span',class_='platform')
        platform_data=platform.text
        
        publisher_li=soup.find('li',class_="summary_detail publisher")
        publisher=publisher_li.find('span',class_='data')
        publisher_data=publisher.text
        
        release_date_li=soup.find('li',class_="summary_detail release_data")
        release_date=release_date_li.find('span',class_='data')
        release_date_data=release_date.text
        
        try:
            critic_score_a_tag = soup.find('a',class_="metascore_anchor")
            critic_score=critic_score_a_tag.find('span')
            critic_score_data=critic_score.text
        except:
            continue
            
        user_score_div_tag=soup.find('div',class_="userscore_wrap feature_userscore")
        user_score=user_score_div_tag.find('a')
        user_score_data=user_score.text
        
        rating_li=soup.find("li",class_="summary_detail product_rating")
        rating=rating_li.find('span',class_='data')
        rating_data=rating.text
        
        genre_li=soup.find('li',class_="summary_detail product_genre")
        genre=genre_li.find('span',class_='data')
        genre_data=genre.text
        
        player_li=soup.find('li',class_="summary_detail product_players")
        number=player_li.find('span',class_='data')        
        if ('1 Player' not in number)&('No Online Multiplayer' not in number):
            multiplayer_data='yes'
    
        meta_game_name.append(title_data)
        meta_platform.append(platform_data)
        meta_publisher.append(publisher_data)
        meta_release_date.append(release_date_data)
        meta_critic_score.append(critic_score_data)
        meta_user_score.append(user_score_data)
        meta_esrb.append(rating_data)
        meta_genre.append(genre_data)
        meta_full_url.append(url)
        meta_multiplayer.append(multiplayer_data)
        count+=1
        
    except:
        meta_error_url.append(url)
        fails+=1

print('metacritic count = '+str(count))
print('metacricic fails = '+str(fails))

meta_columns ={'meta_game_name':meta_game_name,
          'meta_platform':meta_platform,
          'meta_publisher':meta_publisher,
          'meta_release_date':meta_release_date,
          'meta_critic_score':meta_critic_score,
          'meta_user_score':meta_user_score,
          'meta_esrb':meta_esrb,
          'meta_genre':meta_genre,
          'meta_multiplayer':meta_multiplayer,
          'meta_full_url':meta_full_url}


meta_df = pd.DataFrame(meta_columns)
meta_df = meta_df[['meta_game_name','meta_platform','meta_publisher','meta_release_date','meta_critic_score','meta_user_score','meta_esrb','meta_genre','meta_multiplayer','meta_full_url']]
del meta_df.index.name
meta_df.to_csv("meta_full_third.csv",sep=",",encoding='utf-8')

#=============================================================================
#IGDB SCRAPER 
#=============================================================================


count=0
none=0
fails=0
igdb_game_string=[]
igdb_series=[]


headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36'}


for row in range(0,len(url_search)):
    try:
        urlhead= 'https://www.igdb.com/games/'
        urltail=url_search.loc[row,'game_url_string']
        url=urlhead+urltail
        r = requests.get(url, headers=headers)
        r = r.text
        soup = BeautifulSoup(r,'html.parser')
        soup=soup.text
        try:
            if ('Series:' in soup):
                igdb_series.append('yes')
                igdb_game_string.append(urltail)
                count+=1
            else:
                igdb_series.append('no')
                igdb_game_string.append(urltail)
                none+=1   
        except:     
            fails+=1
            continue
    except:
        fails+=1
        continue
    
print('igdb mp = '+str(count))
print('igdb no_mp = '+str(none))
print('igdb fails = '+str(fails))

    
igdb_columns ={'igdb_game_string':igdb_game_string,
          'igdb_series':igdb_series}

igdb_df = pd.DataFrame(igdb_columns)
igdb_df = igdb_df[['igdb_game_string','igdb_series']]
del igdb_df.index.name
igdb_df.to_csv("igdb_full_third.csv",sep=",",encoding='utf-8')
    


