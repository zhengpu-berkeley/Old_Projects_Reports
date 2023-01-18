from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from bs4 import BeautifulSoup
import os
import requests
import urllib3
from urllib3.exceptions import InsecureRequestWarning
import time

def image_grabber(searchword):
    urllib3.disable_warnings(InsecureRequestWarning)
    searchurl = 'https://www.google.com/search?q=' + searchword + '&source=lnms&tbm=isch'
    dirs = str(searchword)
    print('searching for {} on google image'.format(searchword))
    chromedriver = '/usr/lib/chromium-browser/chromedriver'

    #creating directory
    if not os.path.exists('data_raw/'+dirs):
        print('making directory for the search word...')
        os.mkdir('data_raw/'+dirs)

    def download_google_staticimages():
        #search options
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        driver = webdriver.Chrome(chromedriver, options=options)
        driver.set_window_size(800, 800)
        driver.get(searchurl)
        time.sleep(1)

        print('getting images, will take time...')

        element = driver.find_element_by_tag_name('body')

        #scrolling
        for i in range(50):
            element.send_keys(Keys.PAGE_DOWN)
            time.sleep(0.3)
        try:
            driver.find_element_by_id('smb').click()
            for i in range(50):
                element.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.3)
        except:
            for i in range(10):
                element.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.3)
        print('end of first page.')
        time.sleep(0.5)
        print('trying to show more results')
        time.sleep(0.5)

        #getting 2nd page
        driver.find_element_by_xpath('//input[@value="Show more results"]').click()

        #scrolling again
        for i in range(50):
            element.send_keys(Keys.PAGE_DOWN)
            time.sleep(0.3)
        try:
            driver.find_element_by_id('smb').click()
            for i in range(50):
                element.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.3)
        except:
            for i in range(10):
                element.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.3)
        print('getting more images, will take time...')

        #getting images
        page_source = driver.page_source 
        soup = BeautifulSoup(page_source, 'lxml')
        images = soup.find_all('img')
        urls = []
        for image in images:
            try:
                url = image['data-src']
                if not url.find('https://'):
                    urls.append(url)
            except:
                try:
                    url = image['src']
                    if not url.find('https://'):
                        urls.append(image['src'])
                except Exception as e:
                    print(f'No found image sources.')
                    print(e)

        #downloading
        print('start downloading... will take time')
        count = 0
        if urls:
            for url in urls:
                try:
                    res = requests.get(url, verify=False, stream=True)
                    rawdata = res.raw.read()
                    with open(os.path.join('data_raw', dirs, str(searchword) + str(count) + '.jpg'), 'wb') as f:
                        f.write(rawdata)
                        count += 1
                except Exception as e:
                    print('failed to write rawdata.')
                    print(e)
        driver.close()
        return count
    
    #generating search report
    t0 = time.time()
    count = download_google_staticimages()
    t1 = time.time()
    total_time = t1 - t0
    print('Download completed. [Successful count = {}]'.format(count))
    print('Total time is {} seconds.'.format(total_time))
