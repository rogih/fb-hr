options(scipen = 9999)
library(readr)
library(dplyr)
library(ggplot2)
# Need to install robustbase

#  reading the data
bids.data <- read_csv("bids.csv")
test.data <- read_csv("test.csv")
train.data <- read_csv("train.csv")


# unique elements
length(unique(bids.data$bidder_id))
length(unique(test.data$bidder_id))
length(unique(train.data$bidder_id))


glimpse(bids.data)
names(bids.data)
# "bid_id"      "bidder_id"   "auction"     "merchandise" "device"      "time"        "country"     "ip"          "url"
dim(bids.data)

glimpse(train.data)
names(train.data)
# "bidder_id"       "payment_account" "address"         "outcome"
dim(train.data)

# selecting bots and humans from train data
bots.train.data <- dplyr::filter(train.data, outcome == 1)
humans.train.data <- dplyr::filter(train.data, outcome == 0)

total.train.data <- c(
  dim(dplyr::distinct(humans.train.data))[1],
  dim(dplyr::distinct(bots.train.data))[1]
)

plot <- barplot(total.train.data, ylim = c(0, max(total.train.data) + 500), names.arg = c("Humans", "Bots"), main = "Number of each type of user in the training dataset")
text(x = plot, y = total.train.data + 100, labels = paste("n = ", total.train.data, sep = ""))

# now, selecting from the bid dataset only the bids made by bots
bots.bids.data <- dplyr::inner_join(bids.data, bots.train.data, by = "bidder_id")
humans.bids.data <- dplyr::inner_join(bids.data, humans.train.data, by = "bidder_id")
dim(bots.bids.data)
dim(humans.bids.data)


dim(bots.bids.data)[1] + dim(humans.bids.data)[1]
dim(bids.data)
# obviously, not all entries are classified


### Classifield bids
class.bids.data <- bind_rows(humans.bids.data, bots.bids.data)
# same as dplyr::inner_join(bids.data, train.data, by = "bidder_id")
glimpse(class.bids.data)
# class.bids.data %>% janitor::tabyl(auction)


######### Analysing bidder_id
# quantity of bids per bidder_id
# feature
total.bid.class.bids.data <- class.bids.data %>%
  dplyr::group_by(bidder_id) %>%
  summarise(totalbids = n()) %>%
  inner_join(distinct(select(class.bids.data, bidder_id, outcome)), by = "bidder_id")

# plotting the quantity of bids per type of user
total.bids.perclass <- total.bid.class.bids.data %>%
  dplyr::group_by(outcome) %>%
  summarise(sum(totalbids))
plot <- barplot(total.bids.perclass$`sum(totalbids)`, ylim = c(0, max(total.bids.perclass$`sum(totalbids)`) + 150000), names.arg = c("Humans", "Bots"))
text(x = plot, y = total.bids.perclass$`sum(totalbids)` + 100000, labels = paste("n = ", total.bids.perclass$`sum(totalbids)`, sep = ""))


par(mfrow = c(1, 2))
plot(filter(total.bid.class.bids.data, outcome == 0)$totalbids, xlab = "Bidder", ylab = "Number of bids", ylim = c(0, 500000), main = "Humans", type = "p", cex = 0.8)
plot(filter(total.bid.class.bids.data, outcome == 1)$totalbids, xlab = "Bidder", ylab = "Number of bids", ylim = c(0, 500000), main = "Robots", type = "p", cex = 0.8)

aux <- cbind(EstatDescr(filter(total.bid.class.bids.data, outcome == 0)$totalbids, coluna = F), EstatDescr(filter(total.bid.class.bids.data, outcome == 1)$totalbids, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


robustbase::Qn(filter(total.bid.class.bids.data, outcome == 0)$totalbids)
robustbase::Qn(filter(total.bid.class.bids.data, outcome == 1)$totalbids)

# how many bids above or below the mean and 75 percentile
# sum(filter(total.bid.class.bids.data, outcome==0)$totalbids>=mean(filter(total.bid.class.bids.data, outcome==0)$totalbids))
# sum(filter(total.bid.class.bids.data, outcome==1)$totalbids>=mean(filter(total.bid.class.bids.data, outcome==1)$totalbids))
# sum(filter(total.bid.class.bids.data, outcome==0)$totalbids>=quantile(filter(total.bid.class.bids.data, outcome==0)$totalbids,probs=0.75))
# sum(filter(total.bid.class.bids.data, outcome==1)$totalbids>=quantile(filter(total.bid.class.bids.data, outcome==1)$totalbids,probs=0.75))



### Analysing auction
total.auctions <- length(unique(class.bids.data$auction))
bids.per.auction <- class.bids.data %>%
  dplyr::group_by(auction) %>%
  summarise(total = n())
# summary of bids per auction
EstatDescr(bids.per.auction$total)

# average number of bids per user
# feature
avg.bids.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, auction) %>%
  summarise(numberbids = n()) %>%
  dplyr::group_by(bidder_id) %>%
  summarise(meanbids = mean(numberbids)) %>%
  inner_join(distinct(select(class.bids.data, bidder_id, outcome)), by = "bidder_id")

par(mfrow = c(1, 2))
plot(filter(avg.bids.per.user, outcome == 0)$meanbids, xlab = "Bidder", ylab = "Number of bids", ylim = c(0, 1000), main = "Humans", type = "p", cex = 0.8)
plot(filter(avg.bids.per.user, outcome == 1)$meanbids, xlab = "Bidder", ylab = "Number of bids", ylim = c(0, 1000), main = "Robots", type = "p", cex = 0.8)

# summary of average number of bids per user
aux <- cbind(EstatDescr(filter(avg.bids.per.user, outcome == 0)$meanbids, coluna = F), EstatDescr(filter(avg.bids.per.user, outcome == 1)$meanbids, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


# total number of participated auctions
# feature
total.auctions.partipation.per.user <- class.bids.data %>%  dplyr::dplyr::group_by(bidder_id,outcome) %>% dplyr::summarise(total=dplyr::n_distinct(auction))
par(mfrow = c(1, 2))
plot(filter(total.auctions.partipation.per.user, outcome == 0)$total, xlab = "Bidder", ylab = "Number of auctions", ylim = c(0, 2000), main = "Humans", type = "p", cex = 0.8)
plot(filter(total.auctions.partipation.per.user, outcome == 1)$total, xlab = "Bidder", ylab = "Number of auctions", ylim = c(0, 2000), main = "Robots", type = "p", cex = 0.8)

aux <- cbind(EstatDescr(filter(total.auctions.partipation.per.user, outcome == 0)$total, coluna = F), EstatDescr(filter(total.auctions.partipation.per.user, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


### Analysing merchandise
# 10 classes
total.auctions <- (unique(class.bids.data$merchandise))

total.auctions.per.category <- class.bids.data %>%
  dplyr::group_by(merchandise) %>%
  summarise(total = n())
total.auctions.per.category %>% arrange(desc(total), .by_group = TRUE)

# total auctions per category and per user
total.auctions.per.category.per.user <- class.bids.data %>%
  dplyr::group_by(merchandise, outcome) %>%
  summarise(total = n())

total.auctions.per.category.per.user %>%
  filter(outcome == 0) %>%
  full_join(total.auctions.per.category.per.user %>% filter(outcome == 1), by = "merchandise")

# most common merchandise per user
most.common.merch.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, merchandise) %>%
  summarise(number = n()) %>%
  dplyr::group_by(bidder_id) %>%
  summarise(common.merchandise = merchandise[which.max(number)]) %>%
  inner_join(distinct(select(class.bids.data, bidder_id, outcome)), by = "bidder_id")


tab.aux <- most.common.merch.per.user %>%
  dplyr::group_by(common.merchandise, outcome) %>%
  summarise(total = n())

tab.aux %>%
  filter(outcome == 0) %>%
  full_join(tab.aux %>% filter(outcome == 1), by = "common.merchandise")


### Analysing device
# 5729 different devices
total.devices <- length(unique(class.bids.data$device))
# feature
most.common.phone.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, device) %>%
  summarise(number = n()) %>%
  dplyr::group_by(bidder_id) %>%
  summarise(common.phone = device[which.max(number)]) %>%
  inner_join(distinct(select(class.bids.data, bidder_id, outcome)), by = "bidder_id")

tab.aux <- most.common.phone.per.user %>%
  dplyr::group_by(common.phone, outcome) %>%
  summarise(total = n())
par(mfrow = c(1, 2))
plot.aux <- (tab.aux %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of users", main = "Humans", xlab = "Device nº", type = "p", cex = 0.8)
plot.aux <- (tab.aux %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of users", main = "Robots", xlab = "Device nº", type = "p", cex = 0.8)


# checking the most common phone in each group sorted by device name
head(tab.aux %>% filter(outcome == 0) %>% arrange(desc(total)), 10) %>% arrange(desc(commonphone))
head(tab.aux %>% filter(outcome == 1) %>% arrange(desc(total)), 10) %>% arrange(desc(commonphone))


tab.aux <- most.common.phone.per.user %>%
  dplyr::group_by(commonphone, outcome) %>%
  summarise(total = n())
par(mfrow = c(1, 2))
plot.aux <- (tab.aux %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of users", main = "Humans", xlab = "Device nº", type = "p", cex = 0.8)
plot.aux <- (tab.aux %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of users", main = "Robots", xlab = "Device nº", type = "p", cex = 0.8)

# number of devices per user
# feature
number.devices.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(total = n_distinct(device))
plot.aux <- (number.devices.per.user %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of phones", main = "Humans", xlab = "Bidder", type = "p", cex = 0.8)
plot.aux <- (number.devices.per.user %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of phones", main = "Robots", xlab = "Bidder", type = "p", cex = 0.8)

# statistics of number of devices per user
aux <- cbind(EstatDescr(filter(number.devices.per.user, outcome == 0)$total, coluna = F), EstatDescr(filter(number.devices.per.user, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux

# number of devices per auction
# this is equals to the number of bidders in each auction
# number.devices.per.auction <- class.bids.data %>% dplyr::group_by(auction,outcome) %>% summarise(n())

# number of devices per auction
number.distinc.devices.per.auction <- class.bids.data %>%
  dplyr::group_by(auction, outcome) %>%
  summarise(total = n_distinct(device))
par(mfrow = c(1, 2))
plot.aux <- (number.distinc.devices.per.auction %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of devices", main = "Humans", xlab = "Auction nº", type = "p", cex = 0.8)
plot.aux <- (number.distinc.devices.per.auction %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of devices", main = "Robots", xlab = "Auction nº", type = "p", cex = 0.8)

# average number of devices per auction
# feature
avg.dev.auc.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, auction, device, outcome) %>%
  summarise(n()) %>%
  dplyr::group_by(bidder_id, auction, outcome) %>%
  summarise(totaldev = n()) %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(avg.dev.auc = mean(totaldev))

plot.aux <- (avg.dev.auc.per.user %>% filter(outcome == 0))$avg.dev.auc
plot(plot.aux, ylab = "Number of phones(avg)", main = "Humans", xlab = "Bidder", type = "p", cex = 0.8)
plot.aux <- (avg.dev.auc.per.user %>% filter(outcome == 1))$avg.dev.auc
plot(plot.aux, ylab = "Number of phones(avg)", main = "Robots", xlab = "Bidder", type = "p", cex = 0.8)

# statistics of number of devices per auction per user
aux <- cbind(EstatDescr(filter(avg.dev.auc.per.user, outcome == 0)$avg.dev.auc, coluna = F), EstatDescr(filter(avg.dev.auc.per.user, outcome == 1)$avg.dev.auc, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


### Analysing time
# 742669 different time points
# applying log transformation on time
class.bids.data$time <- log(class.bids.data$time)

length(unique(class.bids.data$time))
plot.ts(y = 1:742669, x = unique(class.bids.data$time))

# extracting time difference between bids in an auction
time.diff.bids <- class.bids.data %>%
  group_by(auction) %>%
  do(., differences(.))

# Average time between bids and also the proportion of times when the bidder was the first to bid
mean.proport.bids <- time.diff.bids %>%
  group_by(bidder_id,outcome) %>%
  summarise(average.time.to.bid = mean(time), percent.first = sum(.[["time"]] == 0) / n())

aux <- cbind(EstatDescr(filter(mean.proport.bids, outcome == 0)$average.time.to.bid, coluna = F), EstatDescr(filter(mean.proport.bids, outcome == 1)$average.time.to.bid, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux
aux <- cbind(EstatDescr(filter(mean.proport.bids, outcome == 0)$percent.first, coluna = F), EstatDescr(filter(mean.proport.bids, outcome == 1)$percent.first, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux

# Proportion of how many times the bidder was the last
percent.final <- time.diff.bids %>%
  group_by(auction) %>%
  mutate(last = c(rep(0, n() - 1), 1)) %>%
  group_by(bidder_id,outcome) %>%
  summarise(percent.final = mean(last))

aux <- cbind(EstatDescr(filter(percent.final, outcome == 0)$percent.final, coluna = F), EstatDescr(filter(percent.final, outcome == 1)$percent.final, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux
aux[,1]>aux[,2]

### Analysing country
# 199 countries
length(unique(class.bids.data$country))

# number of distinct bidders per country
number.distinc.user.per.country <- class.bids.data %>%
  dplyr::group_by(country, outcome) %>%
  summarise(total = n_distinct(bidder_id)) %>%
  arrange(desc(total))
# common countries
number.distinc.user.per.country %>%
  filter(outcome == 0) %>%
  full_join(number.distinc.user.per.country %>% filter(outcome == 1), by = "country")

# common country
# feature
common.country.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, country, outcome) %>%
  summarise(number = n()) %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(commoncountry = country[which.max(number)])

# we can plot but the table above is more informative
par(mfrow = c(1, 2))
plot.aux <- (number.distinc.user.per.country %>% filter(outcome == 0) %>% arrange(desc(total)))
plot <- barplot(plot.aux$total, ylim = c(0, max(plot.aux$total) + 500), names.arg = plot.aux$country, main = "User per country (human)")
plot.aux <- (number.distinc.user.per.country %>% filter(outcome == 1) %>% arrange(desc(total)))
plot <- barplot(plot.aux$total, ylim = c(0, max(plot.aux$total) + 500), names.arg = plot.aux$country, main = "User per country (robot)")


# number of countries per user
# feature
number.countries.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(total = n_distinct(country))
par(mfrow = c(1, 2))
plot.aux <- (number.countries.per.user %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of countries", main = "Humans", xlab = "Bidder", type = "p", cex = 0.8)
plot.aux <- (number.countries.per.user %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of countries", main = "Robots", xlab = "Bidder", type = "p", cex = 0.8)

# statistics of number of distinct countries per user
aux <- cbind(EstatDescr(filter(number.countries.per.user, outcome == 0)$total, coluna = F), EstatDescr(filter(number.countries.per.user, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux

# number of countries per auction
# feature
number.distinc.countries.per.auction <- class.bids.data %>%
  dplyr::group_by(auction, outcome) %>%
  summarise(total = n_distinct(country))
par(mfrow = c(1, 2))
plot.aux <- (number.distinc.countries.per.auction %>% filter(outcome == 0))$total
plot(plot.aux, ylab = "Number of countries", main = "Humans", xlab = "Auction nº", type = "p", cex = 0.8)
plot.aux <- (number.distinc.countries.per.auction %>% filter(outcome == 1))$total
plot(plot.aux, ylab = "Number of countries", main = "Robots", xlab = "Auction nº", type = "p", cex = 0.8)

# average number of countries per auction
avg.number.countries.per.auction <- class.bids.data %>%
  dplyr::group_by(bidder_id, auction, country, outcome) %>%
  summarise(n()) %>%
  dplyr::group_by(bidder_id, auction, outcome) %>%
  summarise(totalc = n()) %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(avg.country.auc = mean(totalc))

par(mfrow = c(1, 2))
plot.aux <- (avg.number.countries.per.auction %>% filter(outcome == 0))$avg.country.auc
plot(plot.aux, ylab = "Number of countries(avg)", main = "Humans", xlab = "Bidder", type = "p", cex = 0.8)
plot.aux <- (avg.number.countries.per.auction %>% filter(outcome == 1))$avg.country.auc
plot(plot.aux, ylab = "Number of countries(avg)", main = "Robots", xlab = "Bidder", type = "p", cex = 0.8)

# statistics of number of countries per auction per user
aux <- cbind(EstatDescr(filter(avg.number.countries.per.auction, outcome == 0)$avg.country.auc, coluna = F), EstatDescr(filter(avg.number.countries.per.auction, outcome == 1)$avg.country.auc, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


### Analysing ip
# 1030950 IPs
length(unique(class.bids.data$ip))

# number of distinct bidders per country
# feature
number.distinc.ip.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(total = n_distinct(ip)) %>%
  arrange(desc(total))
aux <- cbind(EstatDescr(filter(number.distinc.ip.per.user, outcome == 0)$total, coluna = F), EstatDescr(filter(number.distinc.ip.per.user, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux

# number of distinct ip per auction.
number.distinc.ip.per.auction <- class.bids.data %>%
  dplyr::group_by(auction, outcome) %>%
  summarise(total = n_distinct(ip)) %>%
  arrange(desc(total))
aux <- cbind(EstatDescr(filter(number.distinc.ip.per.auction, outcome == 0)$total, coluna = F), EstatDescr(filter(number.distinc.ip.per.auction, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux



# average number of ip per auction
# feature
avg.number.ip.per.auction <- class.bids.data %>%
  dplyr::group_by(bidder_id, auction, ip, outcome) %>%
  summarise(n()) %>%
  dplyr::group_by(bidder_id, auction, outcome) %>%
  summarise(totalip = n()) %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(avg.ip.auc = mean(totalip))


# par(mfrow = c(1, 2))
# plot.aux <- (avg.number.ip.per.auction %>% filter(outcome == 0))$avg.ip.auc
# plot(plot.aux, ylab = "Number of countries(avg)", main = "Humans", xlab = "Bidder",type="p",cex=0.8)
# plot.aux <- (avg.number.ip.per.auction %>% filter(outcome == 1))$avg.ip.auc
# plot(plot.aux, ylab = "Number of countries(avg)", main = "Robots", xlab = "Bidder",type="p",cex=0.8)

# statistics of number of ip per auction per user
aux <- cbind(EstatDescr(filter(avg.number.ip.per.auction, outcome == 0)$avg.ip.auc, coluna = F), EstatDescr(filter(avg.number.ip.per.auction, outcome == 1)$avg.ip.auc, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


### Analysing URÇ
# 1030950 URL
length(unique(class.bids.data$url))

# number of distinct url per user
number.distinc.url.per.user <- class.bids.data %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(total = n_distinct(url)) %>%
  arrange(desc(total))
aux <- cbind(EstatDescr(filter(number.distinc.ip.per.user, outcome == 0)$total, coluna = F), EstatDescr(filter(number.distinc.ip.per.user, outcome == 1)$total, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux


# average number of ip per auction
avg.number.ip.per.auction <- class.bids.data %>%
  dplyr::group_by(bidder_id, auction, ip, outcome) %>%
  summarise(n()) %>%
  dplyr::group_by(bidder_id, auction, outcome) %>%
  summarise(totalip = n()) %>%
  dplyr::group_by(bidder_id, outcome) %>%
  summarise(avg.ip.auc = mean(totalip))


# par(mfrow = c(1, 2))
# plot.aux <- (avg.number.ip.per.auction %>% filter(outcome == 0))$avg.ip.auc
# plot(plot.aux, ylab = "Number of countries(avg)", main = "Humans", xlab = "Bidder",type="p",cex=0.8)
# plot.aux <- (avg.number.ip.per.auction %>% filter(outcome == 1))$avg.ip.auc
# plot(plot.aux, ylab = "Number of countries(avg)", main = "Robots", xlab = "Bidder",type="p",cex=0.8)

# statistics of number of ip per auction per user
aux <- cbind(EstatDescr(filter(avg.number.ip.per.auction, outcome == 0)$avg.ip.auc, coluna = F), EstatDescr(filter(avg.number.ip.per.auction, outcome == 1)$avg.ip.auc, coluna = F))
colnames(aux) <- c("Human", "Robot")
aux

%>% 
