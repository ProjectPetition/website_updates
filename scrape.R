# making forecasts for current petitions

# create database:
## create database website;
## CREATE TABLE petitions (id int(7) NOT NULL PRIMARY KEY, type tinytext, title text, body mediumtext, signatureThreshold int, signatureCount int, url text, deadline int, created int, status tinytext);
## CREATE TABLE responses (id int(7) NOT NULL PRIMARY KEY, url text, title text, date date);
## CREATE TABLE signatureCounts (id MEDIUMINT NOT NULL AUTO_INCREMENT PRIMARY KEY, petition_id int(7) NOT NULL, date date NOT NULL, count int, cumulative int NOT NULL, forecast boolean NOT NULL, upperBound int, lowerBound int, foreign key(petition_id) references petitions(id));
## CREATE TABLE petitionIssues (id MEDIUMINT NOT NULL AUTO_INCREMENT PRIMARY KEY, petition_id int(7) NOT NULL, issue tinytext NOT NULL, date date NOT NULL, foreign key(petition_id) references petitions(id));

library(jsonlite)
library(RMySQL)
library(populr)

url = "https://api.whitehouse.gov/v1/petitions.json?limit=999&status=open"
reg_file = "/network/rit/lab/projpet/will/reg.RData"
#reg_file = "/home/will/ppet/website_updates/reg.RData"

get_sigs = function(id, nsigs) {
  offset = 0
  created = vector()
  while (offset < nsigs) {
    json = NULL
    # 'try' because the server randomly gives bogus 404 errors
    try(json <- fromJSON(paste0("https://api.whitehouse.gov/v1/petitions/", id, "/signatures.json?offset=", offset)))
    if (!is.null(json)) {
      # if the API call was successful, get the data and move on, else
      # we're trying again
      created = c(created, json$results$created)
      offset = offset + 1000
    }
    cat(paste0(offset, "\n"))
    Sys.sleep(1)
  }
  created
}

add_to_table = function(p, con, vals) {
  p$title = gsub("'", "\\\\'", p$title)
  p$body = gsub("'", "\\\\'", p$body)
  pvals = paste(p[1, vals], collapse = "','")
  q1 = paste0("insert into petitions (id, type, title, body, signatureThreshold, signatureCount, url, deadline, created, status) values('", pvals, "')")

  dbSendQuery(con, q1)

  # add to the issues table
  pvals2 = list()
  pvals2$petition_id = p$id[1]
  pvals2$date = as.Date(as.POSIXct(p$created[1], origin = "1970-01-01"))
  for (name in p$issues[1][[1]]$name) {
    pvals2$issue = name
    pvals2$issue = gsub("'", "\\\\'", pvals2$issue)
    q2 = paste0("insert into petitionIssues (petition_id, issue, date) values('",
        pvals2$petition_id, "','", pvals2$issue, "','",
        pvals2$date, "')")
    dbSendQuery(con, q2)
  }
}

update_sigs = function(p, con) {
  # get daily signature counts

  sigs = get_sigs(p$id, p$signatureCount)
  sig_counts =
    table(as.Date(as.POSIXct(sigs, origin = "1970-01-01"),
                  tz = "America/New_York"))

  ndays = min(30, Sys.Date() - p$creation_date)
  
  # quick test to see if WtP is acting strange. Will prevent
  # short-lived problems, but if things are screwy multiple days in a
  # row it'll still become an issue
  yesterday = sig_counts[as.character(Sys.Date() - 1)]
  daybefore = sig_counts[as.character(Sys.Date() - 2)]
  if (!is.na(daybefore)) {
    is_screwy = is.na(yesterday) && daybefore > 100
    if (is_screwy) return(FALSE)
  }
  
  dates = p$creation_date + 0:(ndays - 1)
  days = setNames(sig_counts[as.character(dates)], dates)

  # remove stupid days
  q0 = paste0("select * from signatureCounts where petition_id=", p$id)
  cur_counts = dbGetQuery(con, q0)
  dates0 = subset(cur_counts, forecast == 0)$date
  for (wrong_date in setdiff(dates0, as.character(dates))) {
    q1 = paste0("delete from signatureCounts where petition_id=",
                p$id, " and date='", wrong_date, "'")
    dbSendQuery(con, q1)
  }

  # replace old day NA's with zero
  days[is.na(days)] = 0
  for (n in 1:length(days)) {
    q0 = paste0("select * from signatureCounts where petition_id=",
                p$id, " and date='", names(days)[n], "'")
    count_exists = nrow(dbGetQuery(con, q0)) > 0
    if (count_exists) {
      q1 = paste0("update signatureCounts set count=", days[n],
          ", cumulative=", sum(days[1:n]),
          ", upperBound=NULL, lowerBound=NULL, forecast=false where petition_id=", p$id,
          " and date='", dates[n], "'")
    } else {
      q1 = paste0("insert into signatureCounts (petition_id, date, count, cumulative, forecast) values(",
          p$id, ",'", dates[n], "',", days[n], ",", sum(days[1:n]), ",false)")
    }
    dbSendQuery(con, q1)
  }
  days
}

update_forecast = function(p, con, days, reg, forecast_exists) {
  cur_count = sum(days)
  ndays = length(days)
  daysGT150 = sum(cumsum(days) > 150)
  ## if (ndays > 1) {
  newdata =
    data.frame(l1 = log(days[ndays] + 1),
               l2 = ifelse(ndays > 1, log(days[ndays - 1] + 1), NA),
               day = as.character(daysGT150),
               wkday = weekdays(p$creation_date + ndays - 1))
  # the dates being forecasted (starts with today)
  predictions = predict(reg, daysGT150, newdata = newdata,
      interval = "prediction")
  # only keep the relevant ones
  predictions = predictions[1:(31 - ndays), ]
  dates = Sys.Date() + 0:(31 - ndays - 1)
  new_sigs = pmax(round(exp(predictions[, "fit"]) - 1), 0)
  lowers = pmax(round(exp(predictions[, "lwr"]) - 1), 0)
  uppers = pmax(round(exp(predictions[, "upr"]) - 1), 0)
  if (forecast_exists) {
    q = paste0("update signatureCounts set count=NULL",
        ", cumulative=", cur_count + new_sigs,
        ", upperBound=", cur_count + uppers,
        ", lowerBound=", cur_count + lowers,
        ", forecast=true where petition_id=", p$id,
        " and date='", dates, "'")
  } else {
    q = paste0("insert into signatureCounts (petition_id, date, cumulative, upperBound, lowerBound, forecast) values(",
        p$id, ",'", dates, "',", cur_count + new_sigs, ",",
        cur_count + uppers, ",", cur_count + lowers, ",true)")
  }
  sapply(q, function(x) dbSendQuery(con, x))
  ## }
}


response = fromJSON(url)
petitions = response$results

## con = dbConnect(MySQL(), dbname = "website", user = "root", password = "root")
con = dbConnect(MySQL(), dbname = "website")
pets = dbReadTable(con, "petitions")
counts = dbReadTable(con, "signatureCounts")

vals = c("id", "type", "title", "body",
    "signatureThreshold", "signatureCount", "url",
    "deadline", "created", "status")
load(reg_file)
for (row in 1:nrow(petitions)) {
  cat(paste0(row, "\n"))
  p = petitions[row, ]

  # get the age of the petition, in days
  p$creation_date =
    as.Date(as.POSIXct(p$created, origin = "1970-01-01"),
            tz = "America/New_York")
  ndays = Sys.Date() - p$creation_date

  # add to petitions table, if needed
  if (!(p$id %in% pets$id)) {
    add_to_table(p, con, vals)
  } else {
    # update the signatureCount, at least (for Rahul's wordcloud)
    q = paste0("update petitions set signatureCount=",
        p$signatureCount, " where id=", p$id)
    dbSendQuery(con, q)
  }
  
  # only update signature counts if needed (petition was created
  # before today and less than 31 days old)
  if (ndays > 0 && ndays < 31) {
    days = update_sigs(p, con)
  }
}

# forecasting, separate from downloading data
for (row in 1:nrow(petitions)) {
  cat(paste0(row, "\n"))
  p = petitions[row, ]

  # get the age of the petition, in days
  p$creation_date =
    as.Date(as.POSIXct(p$created, origin = "1970-01-01"),
            tz = "America/New_York")

  q = paste0("select * from signatureCounts where petition_id=",
      p$id, " order by date")
  counts = dbGetQuery(con, q)
  ndays = sum(!counts$forecast)
  days = counts$count[!counts$forecast]

  # update forecast too, if appropriate
  if (ndays < 30 && sum(days) >= 150) {
    forecast_exists = any(counts$forecast)
    # need to fix this so can't accidentally create multiple forecasts--
    update_forecast(p, con, days, reg, forecast_exists)
  }
}


# get responses (sort of)

# this needs to be updated! https://petitions.whitehouse.gov/responses
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

dbDisconnect(con)




# and finally, update the graph for Ji!
source("graph.R")
