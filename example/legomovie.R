library(cheerio)
doc <- cheerio("http://www.imdb.com/title/tt1490017/")
node <- doc("strong span")
node$text()
node$html()

# Try to change doc
node1 <- doc("span")
node1$attr("id")
node1$attr("id", "test")$html()
