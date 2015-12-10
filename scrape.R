# making forecasts for current petitions

# create database:
## create database website;
## days = paste(paste0("day", 1:30, " int"), collapse = ", ")
## lowers = paste(paste0("lower", 1:30, " int"), collapse = ", ")
## uppers = paste(paste0("upper", 1:30, " int"), collapse = ", ")
## table1 = "CREATE TABLE petitions (id int(7) NOT NULL PRIMARY KEY, type tinytext, title text, body mediumtext, signatureThreshold int, signatureCount int, url text, deadline int, created int, status tinytext, "
## table2 = ");"
## command = paste0(table1, days, ", ", lowers, ", ", uppers, table2)
## CREATE TABLE responses (id int(7) NOT NULL PRIMARY KEY, url text, title text, date date);

library(jsonlite)
library(RMySQL)

url = "https://api.whitehouse.gov/v1/petitions.json?limit=999&status=open"

get_sigs = function(id, nsigs) {
  offset = 0
  created = vector()
  while (offset < nsigs) {
    json = fromJSON(paste0("https://api.whitehouse.gov/v1/petitions/",
        id, "/signatures.json?offset=", offset))
    created = c(created, json$results$created)
    offset = offset + 1000
    print(offset)
    Sys.sleep(.2)
  }
  created
}

add_to_table = function(p, con) {
  p$title = gsub("'", "\\\\'", p$title)
  p$body = gsub("'", "\\\\'", p$body)
  pvals = paste(p[1, vals], collapse = "','")
  q = paste0("insert into petitions (id, type, title, body, signatureThreshold, signatureCount, url, deadline, created, status) values('", pvals, "')")
  dbSendQuery(con, q)
}

update_sigs = function(p, con) {
  # get daily signature counts
  sigs = get_sigs(p$id, p$signatureCount)
  sig_counts =
    table(as.Date(as.POSIXct(sigs, origin = "1970-01-01")))
  creation_date =
    as.Date(as.POSIXct(p$created, origin = "1970-01-01"))
  ndays = min(30, Sys.Date() - creation_date)
  
  dates = creation_date + 0:(ndays-1)
  days = setNames(sig_counts[as.character(dates)], dates)

  # replace old day NA's with zero
  days[is.na(days)] = 0
  pvals = paste(paste0("day", 1:ndays, "=", cumsum(days)),
      collapse = ", ")
  pvalsl = paste(paste0("lower", 1:ndays, "=", cumsum(days)),
      collapse = ", ")
  pvalsu = paste(paste0("upper", 1:ndays, "=", cumsum(days)),
      collapse = ", ")
  q = paste0("update petitions set signatureCount=",
      p$signatureCount, ", ", pvals, ", ", pvalsl,
      ", ", pvalsu, " where id=", p$id)
  dbSendQuery(con, q)
  # return the daily counts for convenience
  days
}

update_forecast = function(p, con, days, reg) {
  # set numbers for the days we already have
  lowers = days
  uppers = days
  ndays = length(days)
  newdata =
    data.frame(l1 = log(days[ndays] + 1),
               l2 = log(days[ndays - 1] + 1))
  for (date in 1:(30 - ndays)) {
    reg1 = reg[[ndays]][[date]]
    ## prediction = predict(reg1, newdata = newdata,
    ##     interval = "confidence")
    prediction = predict(reg1, newdata = newdata,
        interval = "predict")
    days[ndays + date] =
      max(round(exp(prediction[, "fit"]) - 1), 0)
    lowers[ndays + date] =
      max(round(exp(prediction[, "lwr"]) - 1), 0)
    uppers[ndays + date] =
      max(round(exp(prediction[, "upr"]) - 1), 0)
  }

  cur_count = sum(days[1:ndays])
  pvals = paste(paste0("day", (ndays + 1):30,
      "=", cur_count + days[(ndays + 1):30]), collapse = ", ")
  pvalsl = paste(paste0("lower", (ndays + 1):30,
      "=", cur_count + lowers[(ndays + 1):30]),
      collapse = ", ")
  pvalsu = paste(paste0("upper", (ndays + 1):30,
      "=", cur_count + uppers[(ndays + 1):30]),
      collapse = ", ")
  q = paste0("update petitions set ", pvals, ", ", pvalsl,
      ", ", pvalsu, " where id=", p$id)
  dbSendQuery(con, q)
}


response = fromJSON(url)
petitions = response$results

## con = dbConnect(MySQL(), dbname = "website", user = "root",
##     password = "root")
con = dbConnect(MySQL(), dbname = "website")
pets = dbReadTable(con, "petitions")

vals = c("id", "type", "title", "body",
    "signatureThreshold", "signatureCount", "url",
    "deadline", "created", "status")
load("reg.RData")
for (row in 1:nrow(petitions)) {
  print(row)
  p = petitions[row, ]

  # get the age of the petition, in days
  creation_date =
    as.Date(as.POSIXct(p$created, origin = "1970-01-01"))
  ndays = Sys.Date() - creation_date

  # add to petitions table, if needed
  if (!(p$id %in% pets$id)) {
    add_to_table(p, con)
    # also add daily signature counts, if any
    if (ndays > 0) days = update_sigs(p, con)
  } else {
    # only update signature counts if needed (petition was created
    # before today and less than 31 days old)
    if (ndays > 0 && ndays < 31) days = update_sigs(p, con)
  }

  # update forecast, if needed
  if (ndays > 1 && ndays < 30 && sum(days) >= 150)
    update_forecast(p, con, days, reg)
}


# get responses (sort of)
response = "!"
responses = data.frame()
x = 1
while (response != "") {
  print(x)
  url =
    paste0("https://petitions.whitehouse.gov/responses/more/desc/",
           x, "/2/0/")

  response = fromJSON(url)$markup
  ids0 = regmatches(response,
      gregexpr("id=\"response\\-[0-9]*\"", response))[[1]]
  ids = gsub("id=\"response\\-([0-9]*)\"", "\\1", ids0)
  urls0 = regmatches(response,
      gregexpr("title\"><a href=\"[^\"]*\"", response))[[1]]
  urls = substr(urls0, 17, nchar(urls0) - 1)
  titles0 = regmatches(response,
      gregexpr("title\"><a href=\"[^\"]*\" rel=\"nofollow\">[^<]*<", response))[[1]]
  titles = gsub("title\"><a href=\"[^\"]*\" rel=\"nofollow\">(.*)<", "\\1", titles0)
  dates0 = regmatches(response,
      gregexpr("date\">[^>]*<", response))[[1]]
  dates = as.Date(gsub("date\">([^>]*)<", "\\1", dates0), "%B %d, %Y")
  responses = rbind(responses,
      data.frame(id = ids, url = urls, title = titles, date = dates))
  x = x + 1
}


responses0 = dbReadTable(con, "responses")
for (row in 1:nrow(responses)) {
  p = responses[row, ]

  # add to responses table, if needed
  if (!(p$id %in% responses0$id)) {
    p$title = gsub("'", "\\\\'", p$title)
    pvals = paste(p[1, c("id", "url", "title", "date")], collapse = "','")
    q = paste0("insert into responses (id, url, title, date) values('", p$id, "','", p$url, "','", p$title, "','", p$date, "')")
    dbSendQuery(con, q)
  }
}


# plot petitions (for fun)
## petplot = function(pets, n) {
##   creation_date =
##     as.Date(as.POSIXct(pets$created[n], origin = "1970-01-01"))
##   ndays = Sys.Date() - creation_date
##   days = pets[n, paste0("day", 1:30)]
##   lowers = pets[n, paste0("lower", (ndays + 1):30)]
##   uppers = pets[n, paste0("upper", (ndays + 1):30)]
##   subt = paste0("Started ",
##       as.character(as.Date(as.POSIXct(pets$created[n],
##                                       origin = "1970-01-01"))),
##       ", Currently ", pets$signatureCount[n], " Signatures")
##   plot(1:30, days, type = "l",
##        ylim = c(0, max(unlist(c(uppers, days)))),
##        main = pets$title[n], sub = subt,
##        xlab = "Day", ylab = "Total Signatures")
##   points(ndays:30, c(days[ndays], lowers), type = "l", col = "blue")
##   points(ndays:30, c(days[ndays], uppers), type = "l", col = "blue")
## }
## n = sample(1:nrow(pets), 1)
## petplot(pets, 1)

dbDisconnect(con)
