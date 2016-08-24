# Servant-Play

* For running this program first you need to install lastest version of Elasticsearch (>= 2.3.x), Haskell and Stack installed.

*  Next run the following commands
 * git clone https://github.com/dinnu93/Servant-Play
 * cd Servant-Play
 * /path/to/elasticsearch 
 * stack build
 * stack exec Servant-Play-exe
 
* Now the web service accepts only two endpoints
 * POST /url  { "url" : "http://haskell.org/" } 
 * GET /search?q=haskell
