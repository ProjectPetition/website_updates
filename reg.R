# predicting with linear regression

library(RMySQL)
library(reshape2)

con = dbConnect(MySQL(), dbname = "wtp", user = "root",
    password = "root")

q1 = "select id, from_unixtime(created, '%Y-%m-%d') as date from wtp_data_petitions"
pet = dbGetQuery(con, q1)
pet$date = as.Date(pet$date)

q2 = paste0("select petition_id, from_unixtime(created, '%Y-%m-%d') as date, count(*) as count from wtp_data_signatures group by petition_id, date")
tsigs = dbGetQuery(con, q2)
save(tsigs, pet, file = "tsigs.RData")
load("tsigs.RData")
tsigs$date = as.Date(tsigs$date)

tsigs$day =
  as.integer(tsigs$date - pet$date[match(tsigs$petition_id, pet$id)])

x1 = dcast(data = tsigs[tsigs$day >= 0 & tsigs$day <= 30, ],
    petition_id ~ day, value.var = "count")
x1[is.na(x1)] = 0

x2 = t(apply(x1[, -1], 1, cumsum))

pet2 = data.frame(id = x1$petition_id, x2)

# use logs for better predictions!!!
reg = lm(as.matrix(pet2[, 8:ncol(pet2)]) ~ X5, data = pet2)
g150 = which(pet2$X10 > 150)

# why I'm not using petitions with < 150 signatures
png("150forecasts.png", height = 750, width = 1000, res = 100)
par(mfrow = c(1, 1))
plot(log(pet2$X5), log(pet2$X30), main = "Signature Forecasts",
     sub = "(Line is 150 signatures)",
     xlab = "Day 5 (log)", ylab = "Day 30 (log)")
abline(v = log(150))
dev.off()

reg1 = lm(log(X30 - X10 + 1) ~ log(X10) + log(X8),
    data = pet2, subset = g150)
reg_res = resid(reg1)
plot(log(pet2$X10[g150]), reg_res)
reg1 = lm(log(X30 - X10 + 1) ~
    log(X10) + log(X9) + log(X8) + log(X7) + log(X6) + log(X5) +
     log(X4) + log(X3) + log(X2) + log(X1),
    data = pet2, subset = g150)
reg1 = lm(X30 - X10 ~ X10 + X9,
    data = pet2, subset = g150)
reg2 = lm(X30 ~ X10 + X8, data = pet2, subset = g150)
reg3 = lm(X30 ~ X10 + X7, data = pet2, subset = g150)

# for panel with many petitions
n1 = 10
g150 = which(pet2[, paste0("X", n1)] > 150)
fits = matrix(nrow = length(g150), ncol = 30 - n1)
lowers = matrix(nrow = length(g150), ncol = 30 - n1)
uppers = matrix(nrow = length(g150), ncol = 30 - n1)
for (n in (n1 + 1):30) {
  reg = lm(log(pet2[, paste0("X", n)] - pet2[, paste0("X", n1)] + 1) ~
      log(pet2[, paste0("X", n1 - 2)]) + log(pet2[, paste0("X", n1)]),
      data = pet2, subset = g150)
  print(summary(reg))
  y = predict(reg, interval="predict")
  y = as.data.frame(y)
  y = exp(y) + pet2[g150, paste0("X", n1)] - 1
  fits[, n - n1] = y$fit
  lowers[, n - n1] = pmax(pet2[g150, paste0("X", n1)], y$lwr)
  uppers[, n - n1] = y$upr
}

png("forecasts.png", height = 750, width = 1000, res = 100)
ns = sample(1:length(g150), 9)
par(mfrow = c(3, 3))
for (n in ns) {
  maxy = max(uppers[n, 30 - n1], pet2[g150[n], "X30"])
  plot(0:30, pet2[g150[n], -1], type = "l", xlim = c(0, 30),
       ylim = c(0, maxy), xlab = "Day", ylab = "Signatures", lwd = 2)
  points(n1:30, c(pet2[g150[n], n1 + 2], fits[n, ]), type = "l")
  segments((n1 + 1):30, lowers[n, ], (n1 + 1):30, uppers[n, ])
}
dev.off()


# for panel with one petition changing over time
n1 = 10
g150 = which(pet2[, paste0("X", n1)] > 150)
n2 = sample(1:length(g150), 1)
png("forecasts2.png", height = 750, width = 1000, res = 100)
par(mfrow = c(3, 3))
for (n1 in seq(6, 22, by = 2)) {
  fits = matrix(nrow = length(g150), ncol = 30 - n1)
  lowers = matrix(nrow = length(g150), ncol = 30 - n1)
  uppers = matrix(nrow = length(g150), ncol = 30 - n1)
  for (n in (n1 + 1):30) {
    reg = lm(log(pet2[, paste0("X", n)] - pet2[, paste0("X", n1)] + 1) ~
        log(pet2[, paste0("X", n1 - 2)]) + log(pet2[, paste0("X", n1)]),
        data = pet2, subset = g150)
    y = predict(reg, interval="predict", level = .9)
    y = as.data.frame(y)
    y = exp(y) + pet2[g150, paste0("X", n1)] - 1
    fits[, n - n1] = y$fit
    lowers[, n - n1] = pmax(pet2[g150, paste0("X", n1)], y$lwr)
    uppers[, n - n1] = y$upr
  }
  
  maxy = max(uppers[n2, 30 - n1], pet2[g150[n2], "X30"])
  plot(0:n1, pet2[g150[n2], 2:(n1 + 2)], type = "l", xlim = c(0, 30),
       ylim = c(0, maxy), xlab = "Day", ylab = "Signatures", lwd = 2)
  points(n1:30, c(pet2[g150[n2], n1 + 2], fits[n2, ]), type = "l")
  segments((n1 + 1):30, lowers[n2, ], (n1 + 1):30, uppers[n2, ])
}
dev.off()



## reg1 = lm(pet2[, "X30"] ~ X5 + X6, data = pet2, g150)
## reg12 = lm(log(X30) ~ log(X5) + log(X6), data = pet2, g150)
## reg2 = lm(X30 ~ X25, data = pet2)
## reg22 = lm(log(X30) ~ log(X24) + log(X25), data = pet2, g150)

## cor(pet2$X5, pet2$X30)
## plot(pet2$X5[g150], pet2$X30[g150])
## abline(reg1)
## plot(log(pet2$X5[g150]), log(pet2$X30[g150]))
## abline(reg12)
## plot(log(pet2$X25[g150]), log(pet2$X30[g150]))
## abline(reg22)

## y = predict(reg22, newdata = pet2[g150, ], interval="predict")
## y = as.data.frame(y)
## y = exp(y)
## y$lwr = pmax(pet2$X25[g150], y$lwr)
## n = 5
## plot(0:30, pet2[g150[n], -1], type = "l", ylim = c(0, y$upr[n]), lwd = 2)
## segments(c(25, 30), c(pet2$X25[g150[n]], y$lwr[n]),
##          c(30, 30), c(y$fit[n], y$upr[n]))
## segments(rep(6, times = 3), rep(pet2$X6[g150[n]], times = 3),
##          rep(30, times = 3), as.numeric(exp(y[n, ])))




## save(pet, file = "reg.RData")

## pet3 = t(pet2[, -1])
## lx1 = log(t(x1[, -1] + 1))
## cx1 = t(pet2[, -1])
## l0 = as.vector(lx1[-(1:2), ])
## l1 = as.vector(lx1[-c(1, 31), ])
## l2 = as.vector(lx1[-(30:31), ])
## c1 = as.vector(t(pet2[, -1])[-c(1, 31), ])
## c2 = as.vector(t(pet2[, -1])[-(30:31), ])
## lags = data.frame(l0 = l0, l1 = l1, l2 = l2, c2 = c2)
## cutoff = log(150)
## reg = lm(l0 ~ l1 + l2, data = lags, c2 > 150)
## summary(reg)
## reg1 = lm(l0 ~ l1, data = lags, c1 > 150)
## summary(reg1)
## lpet3 = log(pet3)
## reg = ar(pet3, aic = F, order.max = 2)
## reg = ar.ols(pet3, order.max = 2)

# helpful variables:
lx1 = log(t(x2 + 1))
lx1 = log(t(x1[, -1] + 1))
cx1 = t(pet2[, -1])

# log, 29 regressions
reg = list()
for (l in 1:29) {
  # log signatures from current day to predicted day
  ## l0 = as.vector(lx1[-(1:l), ])
  # total signatures (predicted day) minus total signatures ()
  l0 = as.vector(log(cx1[-(1:l), ] - cx1[-c((31 - l + 1):31), ] + 1))
  # log signatures on 'current' day
  l1 = as.vector(lx1[-c((31 - l + 1):31), ])
  # total signatures on current day
  c1 = as.vector(cx1[-c((31 - l + 1):31), ])

  reg[[l]] = lm(l0 ~ l1, subset = c1 > 150)
}
lapply(reg, summary)
# I like!!

# non-log version
reg = list()
for (l in 1:29) {
  # log signatures from current day to predicted day
  ## l0 = as.vector(lx1[-(1:l), ])
  # total signatures (predicted day) minus total signatures ()
  l0 = as.vector(cx1[-(1:l), ] - cx1[-c((31 - l + 1):31), ])
  # total signatures on current day
  c1 = as.vector(cx1[-c((31 - l + 1):31), ])

  reg[[l]] = lm(l0 ~ c1, subset = c1 > 150)
}
lapply(reg, summary)
# this one sucks.

# log again, separate regression for each day
reg = list()
for (day in 1:29) {
  reg[[day]] = list()
  # log signatures on 'current' day
  l1 = as.vector(lx1[day, ])
  # total signatures on current day
  c1 = as.vector(cx1[day, ])
  for (l in 1:(29 - day + 1)) {
    # log signatures from current day to predicted day
    l0 = as.vector(log(cx1[day + l, ] - c1 + 1))

    reg[[day]][[l]] = lm(l0 ~ l1, subset = c1 > 150)
  }
}
lapply(reg[[2]], summary)
# I like-- good except for day1

# log again, separate regression for each day, 2 days
reg = list()
for (day in 2:29) {
  reg[[day]] = list()
  # log signatures on 'current' day
  l1 = as.vector(lx1[day, ])
  # total signatures on previous day
  l2 = as.vector(lx1[day - 1, ])
  # total signatures on current day
  c1 = as.vector(cx1[day, ])
  for (l in 1:(29 - day + 1)) {
    # log signatures from current day to predicted day
    l0 = as.vector(log(cx1[day + l, ] - c1 + 1))

    reg[[day]][[l]] = lm(l0 ~ l1 + l2, subset = c1 > 150)
  }
}
lapply(reg[[2]], summary)
# very good all around
save(reg, file = "reg.RData")
## load("reg.RData")

# for panel with many petitions (nested style), 1 day
n1 = 2
g150 = which(x2[, n1] > 150)
fits = matrix(nrow = length(g150), ncol = 30 - n1)
lowers = matrix(nrow = length(g150), ncol = 30 - n1)
uppers = matrix(nrow = length(g150), ncol = 30 - n1)
## l1 = pet2[g150, paste0("X", n1)]
## l2 = pet2[g150, paste0("X", n1 - 1)]
l1 = x1[g150, n1 + 1]
newdata = data.frame(l1 = log(l1))
for (n in (n1 + 1):30) {
  reg1 = reg[[n1]][[n - n1]]
  y = predict(reg1, newdata = newdata, interval="predict")
  y = as.data.frame(y)
  y = exp(y) + x2[g150, n1] - 1
  fits[, n - n1] = y$fit
  lowers[, n - n1] = pmax(x2[g150, n1], y$lwr)
  uppers[, n - n1] = y$upr
}

# for panel with many petitions (nested style), 2 days
n1 = 20
g150 = which(pet2[, paste0("X", n1)] > 150)
fits = matrix(nrow = length(g150), ncol = 30 - n1)
lowers = matrix(nrow = length(g150), ncol = 30 - n1)
uppers = matrix(nrow = length(g150), ncol = 30 - n1)
## l1 = pet2[g150, paste0("X", n1)]
## l2 = pet2[g150, paste0("X", n1 - 1)]
l1 = x1[g150, n1 + 1]
l2 = x1[g150, n1]
newdata = data.frame(l1 = log(l1), l2 = log(l2))
for (n in (n1 + 1):30) {
  reg1 = reg[[n1]][[n - n1]]
  y = predict(reg1, newdata = newdata, interval="predict")
  y = as.data.frame(y)
  y = exp(y) + x2[g150, n1] - 1
  fits[, n - n1] = y$fit
  lowers[, n - n1] = pmax(l1, y$lwr)
  uppers[, n - n1] = y$upr
}



# for panel with many petitions (old style)
n1 = 20
g150 = which(pet2[, paste0("X", n1)] > 150)
fits = matrix(nrow = length(g150), ncol = 30 - n1)
lowers = matrix(nrow = length(g150), ncol = 30 - n1)
uppers = matrix(nrow = length(g150), ncol = 30 - n1)
l1 = pet2[g150, paste0("X", n1)]
for (n in (n1 + 1):30) {
  reg1 = reg[[n - n1]]
  y = predict(reg1, newdata = data.frame(l1 = log(l1)),
      interval="predict")
  y = as.data.frame(y)
  y = exp(y) + l1 - 1
  fits[, n - n1] = y$fit
  lowers[, n - n1] = pmax(l1, y$lwr)
  uppers[, n - n1] = y$upr
}

# Awful!!
png("forecasts.png", height = 750, width = 1000, res = 100)
ns = sample(1:length(g150), 9)
ns = ns[!is.na(uppers[ns, 30 - n1])]
par(mfrow = c(3, 3))
for (n in ns) {
  maxy = max(uppers[n, 30 - n1], pet2[g150[n], "X30"])
  plot(1:31, pet2[g150[n], -1], type = "l", xlim = c(0, 30),
       ylim = c(0, maxy), xlab = "Day", ylab = "Signatures", lwd = 2)
  points(n1:30, c(pet2[g150[n], n1 + 1], fits[n, ]), type = "l")
  points(n1:30, c(pet2[g150[n], n1 + 1], lowers[n, ]), type = "l",
         col = "blue")
  points(n1:30, c(pet2[g150[n], n1 + 1], uppers[n, ]), type = "l",
         col = "blue")
  #segments((n1 + 1):30, lowers[n, ], (n1 + 1):30, uppers[n, ])
}
dev.off()




## # cheating with ar()
## lx2 = rbind(lx1, matrix(NA, nrow = nrow(lx1), ncol = ncol(lx1)))

## vlx2 = as.vector(lx2)

## reg = ar(vlx2, na.action = na.remove)
