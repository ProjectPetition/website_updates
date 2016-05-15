# write charts for Ji

library(RMySQL)

## website_path = "/home/will/ppet/Project-Petition-Web-App"
website_path = "/network/rit/lab/projpet/will/Project-Petition-Web-App"
graph_path = paste0(website_path, "/grails-app/assets/images/Graph.png")
graph2_path =
  paste0(website_path, "/grails-app/assets/images/last_month.png")

# get website data
con = dbConnect(MySQL(), dbname = "website")
month_ago = as.numeric(as.POSIXct(Sys.Date() - 30))
month_ago = format(month_ago, scientific = F)
q0 = paste0("select * from petitions where created>", month_ago,
            " order by signatureCount desc limit 10")
pets = dbGetQuery(con, q0)
two_months_ago = as.numeric(as.POSIXct(Sys.Date() - 60))
two_months_ago = format(two_months_ago, scientific = F)
q1 = paste0("select * from petitions where created>", two_months_ago,
    " and created<", month_ago,
    " order by signatureCount desc limit 10")
pets2 = dbGetQuery(con, q1)
dbDisconnect(con)

mgp2 = c(2, 1, 0)
mgp3 = c(2, .5, 0)
mar2 = c(3.5, 2, 2, 2) + 0.1
bcol = "#666666"

write_graph = function(graph_path, pets, month = "Current") {
  xmax = max(max(pets$signatureCount), 10500)
  png(graph_path, height = 600, width = 600, res = 100)
  par(mar = mar2)
  par(mgp = mgp2)
  barplot(rev(pets$signatureCount), space = 0, horiz = T,
          border = bcol,
          xlab = "Number of Signatures", col = "#aac6FF",
          xaxt = "n", yaxt = "n", xlim = c(0, xmax))
  title(paste(month, "Month's Top Petitions:"), line = .3)
  axis(1, pos = 0)
  axis(2, at = 1:10 - .5, pos = 0, labels = 10:1, tick = F,
       las = 1, mgp = mgp3)
  lines(rep(100000, 2), c(0, 10), col = "#999999")
  text(rep(1, 10), 10:1 - .55, labels = pets$title, pos = 4)
  abline(h = 0:10, col = bcol)
  dev.off()
}

write_graph(graph_path, pets)
write_graph(graph2_path, pets2, month = "Last")
