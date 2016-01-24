from bs4 import BeautifulSoup
import urllib.request
import time

class AZlyrics_API:

    url_search = "http://search.azlyrics.com/search.php?q="
    base_url = "http://www.azlyrics.com"
    
    str_no_results = "no results"

    @classmethod
    def getListSongs(cls, link):
        response = urllib.request.urlopen(link).read()
        # suppress all <br>, they are bad formed (next_sibling doesn't work in that case
        response = str(response).replace("<br>",  "")
        soup = BeautifulSoup(response,  'html.parser')
        albums = soup.find_all(attrs="album")
        songs = {}
        for i in albums[15:]:
            contents = i.contents
            if contents[0] == "album: ": # just albums
                album = {"album": contents[1].contents[0][1:-1],
                         "year": contents[2][2:-1]}
                # after an album are the songs (until next album)
                next = i.next_sibling.next_sibling
                while (next.name == 'a'):
                    if (len(next.contents) > 0):
                        link = cls.base_url+next["href"][2:]
                        song = {"album": album['album'], 
                                "year": album['year'],
                                "song": next.contents[0], 
                                "link": link, 
                                "lyrics": cls.getLyrics(link)} # empty for now
                        if song["song"] not in songs: # don´t repeat songs
                            songs[song["song"]] = song
                    next = next.next_sibling.next_sibling
        return songs

    @classmethod
    def search(cls,  artist):
        # search result page
        url_search_string = cls.url_search + artist.lower().replace(" ",  "+")
        response = urllib.request.urlopen(url_search_string).read()
        soup = BeautifulSoup(response,  'html.parser')
        if soup.find(string=cls.str_no_results) is not None:
            # no results found
            return None
        else:
            # get link to artist (first table)
            element = soup.find(attrs="table table-condensed")
            link = element.find("a")["href"]
            #get list of songs
            songs = cls.getListSongs(link) # get songs
        return songs
    
    @classmethod
    def getLyrics(cls,  link):
        response = urllib.request.urlopen(link).read()
        soup = BeautifulSoup(response,  'html.parser')
        lyrics = soup.find_all("div", attrs={"class": None, "id": None})
        lyrics = [x.getText() for x in lyrics]
        time.sleep(5) # to slow queries to azlyrics.
        return " ".join(lyrics)
       
# testing
if __name__ == "__main__":
    result = AZlyrics_API.search("David Bowie")
    print(result)
    import csv
    w = csv.writer(open("david_bowie_songs_15_.csv", "w"))
    for s in result.keys():
        song = result[s]
        w.writerow([song['song'],  song['album'],  song['year'], song['link'], song['lyrics']])
