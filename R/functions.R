#### Functions for feature extraction and EDA
library(tidyr)

EstatDescr <- function(dado, coluna = F, ingles = T) {
  sumario <- (summary(dado))
  res <- cbind(t(as.vector(sumario)), var(dado), sd(dado))
  colnames(res) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Var.", "St. dev.")
  if (!ingles) {
    colnames(res) <- c("Mín.", "1º Quart.", "Mediana", "Média", "3º Quart.", "Máx.", "Var.", "Desv.")
  }
  if (!coluna) {
    return(t(res))
  }
  return(res)
}

total.bids.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(total.bids = dplyr::n())
}


total.auctions.partipation.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(total.auc = n_distinct(auction))
}

avg.bids.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, auction) %>%
    dplyr::summarise(number.bids = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(avg.bids = mean(number.bids))
}

most.common.phone.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, device) %>%
    dplyr::summarise(number = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(common.phone = device[which.max(number)])
}

number.devices.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(total.dist.dev = n_distinct(device))
}

avg.dev.auc.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, auction, device) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::group_by(bidder_id, auction) %>%
    dplyr::summarise(total.dev = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(avg.dev.auc = mean(total.dev))
}

common.country.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, country) %>%
    dplyr::summarise(number = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(common.country = country[which.max(number)] %>% replace(is.na(.), "xx"))
}


number.countries.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(total.country = n_distinct(country))
}

avg.number.countries.per.auction <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, auction, country) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::group_by(bidder_id, auction) %>%
    dplyr::summarise(total.c = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(avg.country.auc = mean(total.c))
}

number.distinc.ip.per.user <- function(df) {
  df %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(total.dist.ip = n_distinct(ip)) %>%
    arrange(desc(total.dist.ip))
}

avg.number.ip.per.auction <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, auction, ip) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::group_by(bidder_id, auction) %>%
    dplyr::summarise(total.ip = dplyr::n()) %>%
    dplyr::group_by(bidder_id) %>%
    dplyr::summarise(avg.ip.auc = mean(total.ip))
}

bid.times <- function(df) {
  df %>%
    group_by(bidder_id) %>%
    summarise(average.time.to.bid = mean(time), percent.first = sum(.[["time"]] == 0) / n())
}

differences <- function(df) {
  df <- df[order(df$time), ]
  df$time <- c(0, diff(df$time))
  df
}

porc.final <- function(df) {
  df %>%
    group_by(auction) %>%
    mutate(last = c(rep(0, n() - 1), 1)) %>%
    group_by(bidder_id) %>%
    summarise(percent.final = mean(last))
}

outcome <- function(df) {
  df %>%
    dplyr::group_by(bidder_id, outcome) %>%
    dplyr::distinct(bidder_id)
}

# outcome.2 <- function(df){
#   df %>% dplyr::select(bidder_id,outcome) %>% dplyr::group_by(bidder_id,outcome) %>% dplyr::distinct(bidder_id)
# }

create.x <- function(bids) {
  time.diff.bids <- bids %>%
    group_by(auction) %>%
    do(., differences(.))
  data.x <- join_all(
    list(
      # outcome=outcome(bids),
      total.bids.per.user = total.bids.per.user(bids),
      total.auctions.partipation.per.user = total.auctions.partipation.per.user(bids),
      avg.bids.per.user = avg.bids.per.user(bids),

      # most.common.phone.per.user=most.common.phone.per.user(bids),
      number.devices.per.user = number.devices.per.user(bids),
      avg.dev.auc.per.user = avg.dev.auc.per.user(bids),

      bid.times = bid.times(time.diff.bids),
      porc.final = porc.final(time.diff.bids),

      # common.country.per.user=common.country.per.user(bids),
      number.countries.per.user = number.countries.per.user(bids),
      avg.number.countries.per.auction = avg.number.countries.per.auction(bids),



      number.distinc.ip.per.user = number.distinc.ip.per.user(bids),
      avg.number.ip.per.auction = avg.number.ip.per.auction(bids)
    ),
    by = "bidder_id", type = "full"
  )
}


summ_df <- function(df) {
  df %>%
    summarise_each(funs(
      min = min,
      q25 = quantile(., 0.25),
      median = median,
      q75 = quantile(., 0.75),
      max = max,
      mean = mean,
      sd = sd
    )) %>%
    gather(stat, val) %>%
    separate(stat, into = c("var", "stat"), sep = "_") %>%
    spread(stat, val) %>%
    select(var, min, q25, median, q75, max, mean, sd)
}
