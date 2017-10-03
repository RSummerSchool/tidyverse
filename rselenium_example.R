library(seleniumPipes)
library(RSelenium)
library(rvest)

# https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-docker.html

system("docker pull selenium/standalone-chrome-debug:3.5.0")
system("docker run -d -p 4445:4444 -p 5901:5900 --name chrome_selenium selenium/standalone-chrome-debug:3.5.0")

remDr <- remoteDr(port = 4445L, browserName = "chrome")

remDr %>% setWindowSizeOld(1350, 980)
# Go to google
remDr %>% 
  go(url = "http://www.google.at") %>% 
  takeScreenshot

# Search for "r summer school vorau"
remDr %>% 
  findElement("name", "q") %>%
  elementSendKeys("r summer school vorau", key = "enter")

# Go to the Summer School Page
remDr %>% 
  findElement("css selector", "#ires #rso link+ div a") %>% 
  elementClick

remDr %>% seleniumPipes::back()
remDr %>% forward()
remDr %>% refresh()

remDr %>% takeScreenshot

# Showcase JS loading

song_url <- "https://www.youtube.com/watch?v=LUlZ5n0cyak"
browseURL(song_url)

remDr %>% 
  go(url = song_url)

remDr %>% 
  executeScriptOld("scroll(0, 250);")

remDr %>% 
  getPageSource %>% 
  html_node("#content-text") %>% 
  html_text

read_html(song_url) %>% 
  html_node("#content-text") %>% 
  html_text


remDr %>% deleteSession

system("docker stop chrome_selenium")
sleep(5) # give docker some time
system("docker rm chrome_selenium")
