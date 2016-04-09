# making forecasts for current petitions

# create database:
## create database website;
## CREATE TABLE petitions (id int(7) NOT NULL PRIMARY KEY, type tinytext, title text, body mediumtext, signatureThreshold int, signatureCount int, url text, deadline int, created int, status tinytext);
## CREATE TABLE responses (id int(7) NOT NULL PRIMARY KEY, url text, title text, date date);
## CREATE TABLE signatureCounts (id MEDIUMINT NOT NULL AUTO_INCREMENT PRIMARY KEY, petition_id int(7) NOT NULL, date date NOT NULL, count int, cumulative int NOT NULL, forecast boolean NOT NULL, upperBound int, lowerBound int, foreign key(petition_id) references petitions(id));

library(jsonlite)
library(RMySQL)

url = "https://api.whitehouse.gov/v1/petitions.json?limit=999&status=open"
#reg_file = "/network/rit/lab/projpet/will/reg.RData"
reg_file = "/home/will/ppet/website_updates/reg.RData"

get_sigs = function(id, nsigs) {
  offset = 0
  created = vector()
  while (offset < nsigs) {
    json = NA
    # 'try' because the server randomly gives bogus 404 errors
    try(json <- fromJSON(paste0("https://api.whitehouse.gov/v1/petitions/", id, "/signatures.json?offset=", offset)))
    if (!is.na(json)) {
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
  q = paste0("insert into petitions (id, type, title, body, signatureThreshold, signatureCount, url, deadline, created, status) values('", pvals, "')")
  dbSendQuery(con, q)
}

update_sigs = function(p, con, counts) {
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
  
  dates = p$creation_date + 0:(ndays-1)
  days = setNames(sig_counts[as.character(dates)], dates)

  # replace old day NA's with zero
  days[is.na(days)] = 0
  for (n in 1:length(days)) {
    if (counts_exist) {
      q = paste0("update signatureCounts set count=", days[n],
          ", cumulative=", sum(days[1:n]),
          ", upperBound=NULL, lowerBound=NULL, forecast=false where petition_id=", p$id,
          " and date='", dates[n], "'")
    } else {
      q = paste0("insert into signatureCounts (petition_id, date, count, cumulative, forecast) values(",
          p$id, ",'", dates[n], "',", days[n], ",", sum(days[1:n]), ",false)")
    }
    dbSendQuery(con, q)
  }
  days
}

update_forecast = function(p, con, days, reg, forecast_exists) {
  cur_count = sum(days)
  ndays = length(days)
  newdata =
    data.frame(l1 = log(days[ndays] + 1))
  # the dates being forecasted (starts with today)
  dates = Sys.Date() + 0:31
  for (n in 1:(31 - ndays)) {
    reg1 = reg[[ndays]][[n]]
    ## prediction = predict(reg1, newdata = newdata,
    ##     interval = "confidence")
    prediction = predict(reg1, newdata = newdata,
        interval = "predict")
    new_sigs = max(round(exp(prediction[, "fit"]) - 1), 0)
    lower = max(round(exp(prediction[, "lwr"]) - 1), 0)
    upper = max(round(exp(prediction[, "upr"]) - 1), 0)
    if (forecast_exists) {
      q = paste0("update signatureCounts set count=NULL",
          ", cumulative=", cur_count + new_sigs,
          ", upperBound=", cur_count + upper,
          ", lowerBound=", cur_count + lower,
          ", forecast=true where petition_id=", p$id,
          " and date='", dates[n], "'")
    } else {
      q = paste0("insert into signatureCounts (petition_id, date, cumulative, upperBound, lowerBound, forecast) values(",
          p$id, ",'", dates[n], "',", cur_count + new_sigs, ",",
          cur_count + upper, ",", cur_count + lower, ",true)")
    }
    dbSendQuery(con, q)
  }
}


response = fromJSON(url)
petitions = response$results

## con = dbConnect(MySQL(), dbname = "website", user = "root",
##     password = "root")
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

  # check to see if database contains signature counts already
  counts_exist = any(counts$petition_id == p$id)
  
  # only update signature counts if needed (petition was created
  # before today and less than 31 days old)
  if (ndays > 0 && ndays < 31) {
    days = update_sigs(p, con, counts_exist)

    # update forecast too, if appropriate
    if (ndays < 30 && sum(days) >= 150) {
      forecast_exists =
        any(counts$forecast[counts$petition_id == p$id])
      update_forecast(p, con, days, reg, forecast_exists)
    }
  }
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


## # plot petitions (just checking)
## petplot = function(pet, counts) {
##   pet_counts = subset(counts, petition_id == pet$id)
##   pet_counts = pet_counts[order(pet_counts$date), ]
##   subt = paste0("Started ", as.character(creation_date),
##       ", Currently ", pet$signatureCount, " Signatures")
##   plot(1:30, pet_counts$cumulative, type = "l",
##        ylim = c(0, 100000),
##        main = pet$title, sub = subt,
##        xlab = "Day", ylab = "Total Signatures")
##   points(1:30, pet_counts$upperBound, type = "l", col = "blue")
##   points(1:30, pet_counts$lowerBound, type = "l", col = "blue")
## }
## pet = petitions[sample(1:nrow(petitions), 1), ]
## petplot(pet, counts)

dbDisconnect(con)
