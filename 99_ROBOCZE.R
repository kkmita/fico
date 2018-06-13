p <- ggplot() +
  geom_histogram(data=db, aes(x=names(db)[4]), stat="count")


############
# PercentTradesNeverDelq
############


n = nrow(db)
proc = apply(db, 2, function(x) round(length(unique(x))/n,2))
names(proc) = 1:ncol(db)

data.frame(name = colnames(db), proc = proc)

temp = db$PercentTradesNeverDelq

# temp = temp[temp!=100]
temp = temp[temp>0]

hist(temp, breaks = seq(0,100))

cut(temp, breaks = c(0, 60, 90, 98, Inf))



czek <- c(rnorm(20, 50, 1), rnorm(35, 150, 1), rnorm(5, 1100, 1))

cut(czek, breaks = c(0, 100, 1000, Inf))



############
# PercentTradesNeverDelq
############

temp = db$MSinceMostRecentDelq

temp = temp[temp>=0]

temp[temp>100] <- 100

hist(temp, breaks = seq(0, max(temp)))



dbtemp = db[db$MSinceMostRecentDelq==-7,c("MaxDelqEver")]





atemp = cor(dplyr::select(db, -RiskPerformance))


temp = MSinceMostRecentTradeOpen






dbd[dbd$NumInqLast6M!=dbd$NumInqLast6Mexcl7days,]





dbd[dbd$MSinceOldestTradeOpen==-8,][1,]

# uzupelnij zmienna o srednia wartosc dla danego `AvgMInFile`\

dbd = db2

dbd[dbd$NumInqLast6M != dbd$NumInqLast6Mexcl7days,
    c("NumInqLast6M", "NumInqLast6Mexcl7days")]


