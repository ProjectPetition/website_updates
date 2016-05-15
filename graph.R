# trying out some charts for Ji

library(RMySQL)

## website_path = "/home/will/ppet/Project-Petition-Web-App"
website_path = "/network/rit/lab/projpet/will/Project-Petition-Web-App"
graph_path = paste0(website_path, "/grails-app/assets/images/Graph.png")

# get website data
con = dbConnect(MySQL(), dbname = "website")
month_ago = as.numeric(as.POSIXct(Sys.Date() - 30))
q0 = paste0("select * from petitions where created>", month_ago,
            " order by signatureCount desc limit 10")
pets = dbGetQuery(con, q0)
dbDisconnect(con)
system2("pkill", '-f "ssh -fN -L.*3307"')

mgp2 = c(2, 1, 0)
mgp3 = c(2, .5, 0)
mar2 = c(3.5, 2, 2, 2) + 0.1
xmax = max(max(pets$signatureCount), 10500)
bcol = "#666666"

png(graph_path, height = 600, width = 600, res = 100)
par(mar = mar2)
par(mgp = mgp2)
barplot(rev(pets$signatureCount), space = 0, horiz = T,
        border = bcol,
        xlab = "Number of Signatures", col = "#aac6FF",
        xaxt = "n", yaxt = "n", xlim = c(0, xmax))
title("Current Month's Top Petitions:", line = .3)
axis(1, pos = 0)
axis(2, at = 1:10 - .5, pos = 0, labels = 10:1, tick = F,
     las = 1, mgp = mgp3)
lines(rep(100000, 2), c(0, 10), col = "#999999")
text(rep(1, 10), 10:1 - .55, labels = pets$title, pos = 4)
abline(h = 0:10, col = bcol)
dev.off()
