#setwd("~/Dropbox/Dottorato/Courses/Statistical Consulting/FinancialData")

library(quantmod)
library(rugarch)
library(rmgarch)
library(tidyquant)
library(tidyverse)
library(ggplot2)

startDate <- as.Date("2000-05-01") 
fullIndexes <- tq_get(c("^FCHI","^GSPC","^FTSE","^N225", "^STOXX50E"), get = "stock.prices", from = startDate)

Symbols <- c("^FCHI","^GSPC","^FTSE","^N225")
getSymbols(Symbols, from=startDate)
CAC40 <- FCHI; SP500 <- GSPC; FTSE100 <- FTSE; NIKKEI225 <- N225
# ### importing FTSEMIB weekly index
FTSEMIB <- read.csv("dailyFTSE.csv", header = T, sep=";", dec = ",", col.names = c("DATE", "PRICE"))
FTSEMIB$DATE <- as.Date(FTSEMIB$DATE, "%m/%d/%y")
FTSEMIB <- xts(x = FTSEMIB$PRICE, order.by = FTSEMIB$DATE)

FTSEMIBtq <- FTSEMIB %>% fortify.zoo %>% as_tibble(.name_repair = "minimal")
names(FTSEMIBtq) <- c("date", "close")

fullIndexes <- fullIndexes[,c("symbol", "date", "close")]
FTSEMIBtqtojoin <- cbind("FTSEMIB.MIB", FTSEMIBtq); names(FTSEMIBtqtojoin) <- c("symbol", "date", "close")
fullIndexes <- rbind(fullIndexes, FTSEMIBtqtojoin)
fullIndexes$symbol[fullIndexes$symbol=="^FCHI"] <- "CAC 40 (France)"
fullIndexes$symbol[fullIndexes$symbol=="^GSPC"] <- "S&P 500 (USA)"
fullIndexes$symbol[fullIndexes$symbol=="^FTSE"] <- "FTSE 100 (UK)"
fullIndexes$symbol[fullIndexes$symbol=="^N225"] <- "NIKKEI 225 (Japan)"
fullIndexes$symbol[fullIndexes$symbol=="FTSEMIB.MIB"] <- "FTSE MIB (Italy)"
fullIndexes$symbol[fullIndexes$symbol=="^STOXX50E"] <- "EURO STOKK 50 (EU)"

# Nice descriptive graphs ------------------------------------------------------
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ04-charting-with-tidyquant.html

rect <- data.frame(xmin=as.Date("2020-02-21"), xmax=as.Date(today()), ymin=-Inf, ymax=Inf)

fullIndexes %>%
  filter(date > "2018-01-01") %>%
  ggplot(aes(x = date, y = close, col = symbol)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="grey",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line() +
  facet_wrap(~symbol, ncol=2, scales = "free_y") +
  labs(y = "", x = "", caption="source: Yahoo! Finance") + 
  theme_tq() +
  theme(legend.position = "none")

fullIndexes %>%
  filter(date > "2018-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log") %>%
  ggplot(aes(x = date, y = abs(daily.returns), col = symbol)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="grey",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line() +
  facet_wrap(~symbol, ncol=2) +
  labs(y = "", x = "", caption="source: Yahoo! Finance") + 
  theme_tq() +
  theme(legend.position = "none")

# Returns -----------------------------------------------------------------

#### daily returns
rFTSEMIB_daily <- dailyReturn(FTSEMIB)

#### weekly returns
rFTSEMIB_weekly <- weeklyReturn(FTSEMIB)
rCAC40_weekly <- weeklyReturn(CAC40)
rSP500_weekly <- weeklyReturn(SP500)
rFTSE100_weekly <- weeklyReturn(FTSE100)
rNIKKEI225_weekly <- weeklyReturn(NIKKEI225); rNIKKEI225_weekly[1] <- 0

# ITALY -------------------------------------------------------------------

lineChart(FTSEMIB, subset="2015-01-01/")
ug_spec <- ugarchspec(variance.model = list(model = "eGARCH"),
                      mean.model = list(include.mean = FALSE),
                      distribution.model = "sstd")
ugfit <- ugarchfit(spec = ug_spec, data = rFTSEMIB_weekly)
ugfit

predictions <- ugarchforecast(ugfit, n.ahead = 100)
boot <- ugarchboot(ugfit, method = "Partial", n.ahead = 100)
bootstrapdf <- as.data.frame(boot)
bootq25 <- as.numeric(apply(bootstrapdf, 2, function(x) quantile(x, 0.25)))
bootq75 <- as.numeric(apply(bootstrapdf, 2, function(x) quantile(x, 0.75)))

fitted_sigma <- sigma(ugfit)
forecasted_sigma <- as.numeric(predictions@forecast$sigmaFor)
forecast_band <- data.frame(bootq25=bootq25,bootq75=bootq75)

####### now a focus on italy:
fullIndexes %>%
  filter(symbol=="FTSE MIB (Italy)") %>%
  filter(date > "2005-01-01") %>%
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               type       = "log") %>%
  ggplot(aes(x = date, y = abs(weekly.returns))) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightblue",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line(color="darkgrey") +
  geom_ribbon(data=data.frame(date=seq(from=as.Date("2020-05-26"), length.out = 100, by="week"),
                              bootq25=bootq25,
                              bootq75=bootq75), aes(x=date, ymin = bootq25, ymax = bootq75), fill = "lightblue", inherit.aes = FALSE) +
  geom_line(data=data.frame(date=index(fitted_sigma), sigma=as.numeric(fitted_sigma)), aes(x = date, y = sigma), col="black") +
  geom_line(data=data.frame(date=seq(from=as.Date("2020-05-26"), length.out = 100, by="week"), sigma=forecasted_sigma), aes(x = date, y = sigma), col="blue", size=0.75) +
  scale_x_date(limits=as.Date(c("2013-01-01", "2022-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Volatility", x = "", caption="source: Yahoo! Finance") + 
  theme_tq()


fullIndexes %>%
  filter(symbol=="FTSE MIB (Italy)") %>%
  filter(date > "2020-01-01") %>%
  ggplot(aes(x = date, y = close)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightblue",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line(size=0.75, alpha=0.5) +
  geom_point(aes(x=as.Date("2020-01-31"), y=FTSEMIBtqtojoin$close[FTSEMIBtqtojoin$date==as.Date("2020-01-31")])) +
  geom_point(aes(x=as.Date("2020-02-21"), y=FTSEMIBtqtojoin$close[FTSEMIBtqtojoin$date==as.Date("2020-02-21")])) +
  geom_point(aes(x=as.Date("2020-03-06"), y=FTSEMIBtqtojoin$close[FTSEMIBtqtojoin$date==as.Date("2020-03-06")])) +
  geom_point(aes(x=as.Date("2020-03-12"), y=FTSEMIBtqtojoin$close[FTSEMIBtqtojoin$date==as.Date("2020-03-12")])) +
  geom_point(aes(x=as.Date("2020-05-04"), y=FTSEMIBtqtojoin$close[FTSEMIBtqtojoin$date==as.Date("2020-05-04")])) +
  theme_tq() +
  labs(caption="source: Yahoo! Finance") +
  theme(axis.title = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

mydata <- fullIndexes %>%
  filter(symbol=="FTSE MIB (Italy)") %>%
  filter(date > "2015-01-01")

mylm <- lm(close~log(as.numeric(date)), data=subset(mydata, date > "2020-03-11"))
mypred <- predict(mylm, newdata=data.frame(date=seq(from=as.Date("2020-05-27"), to=as.Date("2021-03-01"), by="day")), interval="predict")

f_o_p <- mylm$coefficients[1] + mylm$coefficients[2] * log(as.numeric(seq(from=as.Date("2020-03-11"), to=as.Date("2021-03-01"), by="day")))

predline <- data.frame(date=seq(from=as.Date("2020-03-11"), to=as.Date("2021-03-01"), by="day"),
                       fitted_or_predicted=f_o_p)

ggplot(mydata, aes(x = date, y = close)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightblue",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_hline(yintercept = median(mydata$close), color="grey") +
  geom_line() +
  geom_ribbon(data=data.frame(date=seq(from=as.Date("2020-05-27"), to=as.Date("2021-03-01"), by="day"),
                              low=mypred[,2],
                              upp=mypred[,3]), aes(x=date, ymin = low, ymax = upp), fill = "lightblue", inherit.aes = FALSE, alpha=0.75) +
  geom_line(data=predline, aes(x=date,y=fitted_or_predicted), alpha=0.5, color="blue") +
  theme_tq() +
  scale_x_date(limits = as.Date(c("2018-01-01", "2021-03-01"))) +
  labs(caption="source: Yahoo! Finance") +
  theme(axis.title = element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# MGARCH ------------------------------------------------------------------

rMultiv <- data.frame(rFTSEMIB_weekly, rCAC40_weekly,
                      rFTSE100_weekly, rSP500_weekly)
names(rMultiv)[1] <- "rFTSEMIB"
names(rMultiv)[2] <- "rCAC40"
names(rMultiv)[3] <- "rFTSE100"
names(rMultiv)[4] <- "rSP500"

uspec.n <- multispec(c(ugarchspec(variance.model = list(model = "eGARCH"),
                                  mean.model = list(include.mean = FALSE),
                                  distribution.model = "sstd"),
                       ugarchspec(variance.model = list(model = "eGARCH"),
                                  mean.model = list(armaOrder = c(0, 1), include.mean = FALSE),
                                  distribution.model = "sstd"),
                       ugarchspec(variance.model = list(model = "eGARCH"),
                                  mean.model = list(include.mean = FALSE),
                                  distribution.model = "sstd"),
                       ugarchspec(variance.model = list(model = "eGARCH"),
                                  mean.model = list(include.mean = FALSE),
                                  distribution.model = "sstd")))

multf <- multifit(uspec.n, rMultiv)
spec1 <- dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
fit1 <- dccfit(spec1, data = rMultiv, fit.control = list(eval.se = TRUE), fit = multf)

dfToPlot <- data.frame(dates=rep(index(rFTSE100_weekly), 3),
                       correlations=c(as.numeric(rcor(fit1)[1,2,]),
                                      as.numeric(rcor(fit1)[1,3,]),
                                      as.numeric(rcor(fit1)[1,4,])),
                       label=factor(c(rep("FTSEMIB and CAC 40 (France)", length(index(rFTSE100_weekly))),
                                      rep("FTSEMIB and FTSE 100 (UK)", length(index(rFTSE100_weekly))),
                                      rep("FTSEMIB and S&P 500 (USA)", length(index(rFTSE100_weekly))))))
ggplot(data = dfToPlot, aes(x=dates, y=correlations, col=label)) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightblue",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line(size=0.75) +
  scale_x_date(limits=as.Date(c("2013-01-01", today())), date_breaks = "1 year", date_labels = "%Y") +
  labs(y="Conditional correlations") +
  scale_color_brewer(palette = "Set1") +
  theme_tq() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_blank())


# SCENARIO ----------------------------------------------------

weekly_returns <- fullIndexes %>%
  filter(symbol=="FTSE MIB (Italy)") %>%
  filter(date > "2005-01-01") %>%
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               type       = "log")

ggplot(weekly_returns, aes(x=date, y=weekly.returns)) +
  geom_line(aes(y=abs(weekly.returns))) +
  scale_x_date(limits=c(as.Date("2015-01-01"), today()))

fullIndexes %>%
  filter(symbol=="FTSE MIB (Italy)") %>%
  filter(date > "2020-01-01") %>%
  tq_transmute(select     = close, 
               mutate_fun = periodReturn, 
               period     = "weekly", 
               type       = "log") %>%
  print(n=22)

new_weekly_returns <- weekly_returns %>%
  filter(date >= "2020-02-21" & date <= "2020-03-20") %>% 
  select(weekly.returns) %>% transform(weekly.returns.shifted = 0.75 * weekly.returns) %>%
  select(weekly.returns.shifted)

weekly_returns_newcontagion <- rbind(weekly_returns,
                                     data.frame(date=seq(from=as.Date("2020-05-26"), by = "week", length.out = (nrow(new_weekly_returns)+1))[-1],
                                                weekly.returns=as.numeric(new_weekly_returns$weekly.returns.shifted)))

ggplot(weekly_returns_newcontagion, aes(x=date, y=weekly.returns)) +
  geom_line(aes(y=abs(weekly.returns))) +
  scale_x_date(limits=c(as.Date("2015-01-01"), as.Date("2020-06-30")))



ug_spec_newcont <- ugarchspec(variance.model = list(model = "eGARCH"),
                              mean.model = list(include.mean = FALSE),
                              distribution.model = "sstd")
ugfit_newcont <- ugarchfit(spec = ug_spec_newcont, data = xts(x = weekly_returns_newcontagion$weekly.returns, 
                                                              order.by = weekly_returns_newcontagion$date))
ugfit_newcont

predictions_newcont <- ugarchforecast(ugfit_newcont, n.ahead = 100)
boot_newcont <- ugarchboot(ugfit_newcont, method = "Partial", n.ahead = 100)
bootstrapdf_newcont <- as.data.frame(boot_newcont)
bootq25_newcont <- as.numeric(apply(bootstrapdf_newcont, 2, function(x) quantile(x, 0.25)))
bootq75_newcont <- as.numeric(apply(bootstrapdf_newcont, 2, function(x) quantile(x, 0.75)))

fitted_sigma_newcont <- sigma(ugfit_newcont)
forecasted_sigma_newcont <- as.numeric(predictions_newcont@forecast$sigmaFor)

rect_newcontagion <- data.frame(xmin=today(), xmax=as.Date("2020-06-30"), ymin=-Inf, ymax=Inf)
weekly_returns_newcontagion %>%
  ggplot(aes(x = date, y = abs(weekly.returns))) +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightblue",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_rect(data=rect_newcontagion, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color=NA,
            fill="lightgreen",
            alpha=0.5,
            inherit.aes = FALSE) +
  geom_line(color="darkgrey") +
  geom_ribbon(data=data.frame(date=seq(from=as.Date("2020-06-30"), length.out = 100, by="week"),
                              bootq25_newcont=bootq25_newcont,
                              bootq75_newcont=bootq75_newcont), aes(x=date, ymin = bootq25_newcont, ymax = bootq75_newcont), fill = "lightblue", inherit.aes = FALSE) +
  geom_line(data=data.frame(date=index(fitted_sigma_newcont), sigma=as.numeric(fitted_sigma_newcont)), aes(x = date, y = sigma), col="black") +
  geom_line(data=data.frame(date=seq(from=as.Date("2020-06-30"), length.out = 100, by="week"), sigma=forecasted_sigma_newcont), aes(x = date, y = sigma), col="blue", size=0.75) +
  scale_x_date(limits=as.Date(c("2013-01-01", "2022-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Volatility", x = "", caption="source: Yahoo! Finance") + 
  theme_tq()

