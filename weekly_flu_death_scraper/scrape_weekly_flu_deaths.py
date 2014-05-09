import urllib2
import re
from BeautifulSoup import BeautifulSoup
import time

##### Step 1: Scrape a list of the 122 cities to be used in urls ##############
##### to scrape reports for the specific city #################################

# url to morbidity and mortality weekly report
address ='http://wonder.cdc.gov/mmwr/mmwrmort.asp'

# Read in the response to a HTTP request
response = urllib2.urlopen(address).read()

# Make a soup object of the website
soup = BeautifulSoup(response)

## We want the text in one of the tables
# fetch all of the tables
tables = soup.fetch('table')
# tables[7] has the list of all the city names
table = tables[7]
# the cities and some extra information are contained in the <option> tags
table.fetch('option')

# Make a list of the cities and states to be used in calling the city/state reports
city_row = str(table)
# remove the <option> html tags
city_row = re.sub(r'<.*?>','',city_row)
# split the city list into a list by the \n character
city_list = city_row.split('\n')

cities = []

for place in city_list:
  # cities are the only lines with commas in them, so
  # only parse lines that have commas in them
  if re.search(',',place):
    # split apart the city and the state     
    city_state = place.split(',')
    print city_state
    # replace spaces in the city name with a plus sign
    # this will make the cities with two words conform to report url
    city_url = re.sub(r' ','+',city_state[0])
    # strip spaces from the state
    state_url = re.sub(r' ','',city_state[1])
    cities.append([re.sub(r' ','',city_url), state_url, city_state[0]])    

# postal codes dictionary
states_dict = {'Kans.':'KS', 'Oreg.':'OR', 'Ariz.': 'AZ', 'Minn.':'MN', 'Wis.' : 'WI', 'Ohio' : 'OH', 'Ind.' : 'IN', 'Idaho' : 'ID', 'D.C.' : 'DC', 'Calif.' : 'CA',  'Iowa' : 'IA', 'Ky.' : 'KY', 'R.I.' : 'RI', 'Utah' : 'UT', 'Ill.' : 'IL', 'Tex.' : 'TX', 'Colo.' : 'CO', 'Nebr.' : 'NE', 'N.J.' : 'NJ', 'Wash.' : 'WA', 'Pa.***' : 'PA', 'Ark.' : 'AR', 'Va.' : 'VA', 'Conn.' : 'CT', 'N.C.': 'NC', 'Hawaii': 'HI', 'N.Y.' : 'NY', 'Md.': 'MD', 'Mass.': 'MA', 'Pa.': 'PA', 'La.': 'LA', 'Nev.': 'NV', 'Tenn.': 'TN', 'N.M.' : 'NM', 'Ala.' : 'AL', 'Del.' : 'DE', 'Ga.' : 'GA', 'Mich.': 'MI', 'Mo.' : 'MO', 'Okla.' : 'OK','Fla.': 'FL'}

# Create an output text file
output = open('./flu_surveil_data/mmwr_formatted.csv','w')
output.write('city, state, mmrw_week_year, all_ages_all , all_plus_65, all_ages_45_64, all_25_44, all_1_24, all_less_than_1, pneum_flu,\n')

# year span to be scraped
years = range(2008,2014)

for i in range(len(cities)):
    time.sleep(1)
    print str(cities[i])
    print ''
    for yr in years:
        # url for report for a specific city
        address_city ='http://wonder.cdc.gov/mmwr/mmwr_reps.asp?mmwr_table=4A&mmwr_year=' + str(yr) + '&mmwr_week=38&mmwr_location=' + cities[i][0] + '%2C+' + cities[i][1]

        # Read in the response to a HTTP request
        response_city = urllib2.urlopen(address_city).read()

        # Make a soup object of the website
        soup_city = BeautifulSoup(response_city)
        # fetch all of the tables on the page
        get_tables = soup_city.fetch('table')
        # get_tables[6] has the weekly information on it
        table = get_tables[6]
        # read through the rows and columns, extracting the morbidity data
        rows = table.findAll('tr')
        for tr in rows:
            cols = tr.findAll('td')
            line = ''
            for td in cols:
                text = ''.join(td.find(text=True))
                text = text.strip()
                text = re.sub(r',','',text)
                line = line + text + ','
            # if the first two characters in the line are a number, 
            # then write the line to the output
            if line[0:2].isdigit():
                output.write(cities[i][2]+','+states_dict[cities[i][1]] + ',' + str(yr) + line + '\n')
output.close()
