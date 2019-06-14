library(TrenaViz)
tv <- TrenaViz("TrenaProjectErythropoiesis")
#tv <- TrenaViz("TrenaProjectLymphocyte")
PORT=5874
url <- sprintf("http://0.0.0.0:%d", PORT)
runApp(createApp(tv, port=PORT))
# later(function(){browseURL(url)}, 5)
