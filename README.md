# Servant-Play

About 
---------
This piece of software is an API for a search engine written in Haskell using the Elasticsearch, Bloodhound & Servant. All it does is, it takes any URL you POST to it through the API endpoint /url and index it in its datastore(Elasticsearch) after that when you search for a phrase using the /search?q="Search Phrase" it gives the URLs and the fetched contents of that URL in a ranked order of priority 


* For running this program first you need to install lastest versions of Elasticsearch (>= 2.3.x), Haskell and Stack installed.

*  Next run the following commands
 * git clone https://github.com/dinnu93/Servant-Play
 * cd Servant-Play
 * /path/to/elasticsearch 
 * stack build
 * stack exec Servant-Play-exe
 
* Now the web service accepts only two endpoints
 * POST /url  { "url" : "http://haskell.org/" } 
 * GET /search?q=haskell
