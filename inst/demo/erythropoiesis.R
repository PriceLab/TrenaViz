library(TrenaViz)
tv <- TrenaViz("TrenaProjectLymphocyte")
PORT=5874
runApp(createApp(tv, port=PORT))
url <- sprintf("http://0.0.0.0:%d", PORT)
later(function(){browseURL(url)}, 2)
