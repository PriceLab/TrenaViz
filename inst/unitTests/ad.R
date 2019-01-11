library(TrenaViz)
tv <- TrenaViz("TrenaProjectAD")
app <- createApp(tv, port=3838)

later(function(){browseURL("http://0.0.0.0:3838")}, 2)
app
