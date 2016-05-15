# predicting with linear regression

# trying to narrow this down to only create the reg.RData file, but
# still needs some work

library(RMySQL)
library(reshape2)
library(populr)


con = dbConnect(MySQL(), dbname = "wtp0116", user = "root",
    password = "root")

q1 = "select id, created, from_unixtime(created, '%Y-%m-%d') as date from wtp_data_petitions"
pet = dbGetQuery(con, q1)
pet$date = as.Date(pet$date)

q2 = paste0("select petition_id, from_unixtime(created, '%Y-%m-%d') as date, count(*) as count from wtp_data_signatures group by petition_id, date")
tsigs = dbGetQuery(con, q2)
save(tsigs, pet, file = "tsigs.RData")
load("tsigs.RData")
tsigs$date = as.Date(tsigs$date)

tsigs$day =
  as.integer(tsigs$date - pet$date[match(tsigs$petition_id, pet$id)]) + 1

x1 = dcast(data = tsigs[tsigs$day > 0 & tsigs$day <= 31, ],
    petition_id ~ day, value.var = "count")
x1[is.na(x1)] = 0

x2 = t(apply(x1[, -1], 1, cumsum))

pet2 = data.frame(id = x1$petition_id, x2)

cutoff_date = as.Date(as.POSIXct(
    dbGetQuery(con, "select max(created) from wtp_data_signatures")$`max(created)`, origin = "1970-01-01"))
dbDisconnect(con)


# helpful variables:
s_mat = x1[, -1]
# log of votes by day
logs = as.matrix(log(x1[, -1] + 1))
## logs = log(s_mat + 1)
# cumulative vote totals
cums = x2
## cums = t(x2)
pet_dates = pet$date[match(x1$petition_id, pet$id)]
count_weekdays = t(sapply(pet_dates, function(x) weekdays(x + 0:30)))
## total_days = (cutoff_date - pet_dates) %*% t(rep(1, 31))
total_days = cutoff_date - pet_dates
petdays = col(cums)
for (pet in 1:length(total_days)) {
  if (total_days[pet] < 31)
    s_mat[pet, (total_days[pet] + 1):31] = NA
}


# changing starting day to first day over 150 signatures
start_days = apply(cums, 1, function(x) which(x > 150)[1])
indices = cbind(as.vector(row(s_mat)),
    as.vector(t(sapply(start_days, function(x) 1:31 + x - 1))))
indices0 = cbind(as.vector(row(s_mat)),
    as.vector(col(s_mat)))
s_mat2 = matrix(nrow = nrow(s_mat), ncol = ncol(s_mat))
cums2 = matrix(nrow = nrow(s_mat), ncol = ncol(s_mat))
wkdays2 = matrix(nrow = nrow(s_mat), ncol = ncol(s_mat))
indices0 = indices0[which(indices[, 2] <= 31), ]
indices = indices[which(indices[, 2] <= 31), ]
s_mat2[indices0] = s_mat[indices]
cums2[indices0] = cums[indices]
s_mat2[is.na(s_mat2)] = 0
cums2[is.na(cums2)] = 0
wkdays2[indices0] = count_weekdays[indices]
# phwew!


reg = populrlm(s_mat2)
save(reg, file = "reg.RData")
