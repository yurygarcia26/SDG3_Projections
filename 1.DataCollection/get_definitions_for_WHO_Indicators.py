from bs4 import BeautifulSoup
import requests
import pandas as pd
import tqdm
from multiprocessing import Pool

"""
Web Scraping WHO indicators-index page to get definitions for all indicators.
"""

def get_value(soup1, type_use):
    result = ''
    for metadata_box in soup1.findAll('div', attrs = {'class':['metadata-box']}):
        for metadata_title in metadata_box.findAll('div', attrs = {'class':['metadata-title']}):
            title = metadata_title.get_text().strip()
            if title == f'{type_use}:':
                result = metadata_box.get_text().replace(f'{type_use}:  ','')
    return str(result)

url = "https://www.who.int/data/gho/data/indicators/indicators-index"
headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.61 Safari/537.36'}
r = requests.get(url, headers=headers)
soup1 = BeautifulSoup(r.content, 'html.parser')

data_url = pd.DataFrame({})
for alph_box in soup1.findAll('div', attrs = {'class':['alphabetical-box']}):
    
    indicator_names_letter = []
    indicator_hrefs_letter = []
    letter = ''

    for letter_tag in alph_box.findAll('div', attrs = {'class':['box-letter']}):
        letter = letter_tag.get('id').replace('letter-','')

    for a_tag in alph_box.findAll('a'):
        indicator_hrefs_letter.append(a_tag.get("href"))
        indicator_names_letter.append(a_tag.get_text().strip())
    
    data_letter = pd.DataFrame({
        'Indicator' : indicator_names_letter,
        'Indicator_URL': indicator_hrefs_letter,
        'Letter': [letter]*len(indicator_hrefs_letter)
    })
    data_url = pd.concat([data_url, data_letter], ignore_index=True)

print("data_url Ready")

def _get_values(url):
    try:
        r = requests.get(url, headers=headers)
        soup1 = BeautifulSoup(r.content, 'html.parser')
        definition = get_value(soup1, 'Definition')
        rationale = get_value(soup1, 'Rationale')
    except Exception:
        definition, rationale = '', ''
    return (definition, rationale)

if __name__ == '__main__':

    print('Getting definitions and rationales ...')
    url_list = data_url['Indicator_URL'].tolist()
    with Pool(3) as p:
        values = list(tqdm.tqdm(p.imap(_get_values, url_list), total=len(url_list)))

    definitions = [vl[0] for vl in values]
    rationales = [vl[1] for vl in values]
    data_url['Indicator_Definition'] = definitions
    data_url['Indicator_Rationale'] = rationales
    data_url.to_csv('Full_Indicator_Data.csv')
