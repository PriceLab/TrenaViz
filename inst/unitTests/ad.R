library(TrenaViz)
tv <- TrenaViz("TrenaProjectAD")
app <- createApp(tv, port=5838)
later(function(){browseURL("http://0.0.0.0:5838")}, 2)
