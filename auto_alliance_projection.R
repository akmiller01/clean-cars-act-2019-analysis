list.of.packages <- c("data.table","ggplot2","scales","reshape2","bsts","lubridate","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "C:/git/clean-cars-act-2019-analysis"
setwd(wd)

dat = fread("data/alliance_dat.csv")
ZEV = dat$BEV + dat$PHEV

### Load the data
Y <- ts(ZEV, frequency=12, start=c(2011,1))
y <- Y


### Run the bsts model
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
bsts.model <- bsts(y, state.specification = ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)

### Predict
p <- predict.bsts(bsts.model, horizon = 12, burn = burn, quantiles = c(.025, .975))
p.ts = ts(rep(NA,12),frequency=12,start=c(2019,1))
### Actual versus predicted
d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+y),  
    as.numeric(p$mean)),
  # actual data and dates 
  as.numeric(c(y,rep(NA,12))),
  c(as.Date(time(y)),as.Date(time(p.ts))))
names(d2) <- c("Fitted", "Actual", "Date")

### MAPE (mean absolute percentage error)
MAPE <- filter(d2, year(Date)<2018) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))

### 95% forecast credible interval
posterior.interval <- cbind.data.frame(
  as.numeric(p$interval[1,]),
  as.numeric(p$interval[2,]), 
  subset(d2, year(Date)>2018)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")

### Plot actual versus predicted with credible intervals for the holdout period
proj.p = ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(aes(y=Fitted, colour = "Fitted"), size=1.2, linetype=1) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2019-01-01")), linetype=2) + 
  geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title="Forecasted monthly Maryland ZEV sales, 2011-2020")
ggsave("output/projection.png", proj.p, width=10, height=5)

estimate_2020 = sum(d3$Actual,na.rm=T) + sum(subset(d3,Date>as.Date("2018-12-01"))$Fitted)
low_end = sum(d3$Actual,na.rm=T) + sum(d3$LL,na.rm=T)
high_end = sum(d3$Actual,na.rm=T) + sum(d3$UL,na.rm=T)

message("Estimates 2020 ZEVs: ", round(estimate_2020))
message("Estimates 2020 ZEVs low end: ", round(low_end))
message("Estimates 2020 ZEVs high end: ", round(high_end))
fwrite(d3, "output/forecast.csv")

### Seasonality
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons = 12)
bsts.model <- bsts(y, state.specification = ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)

### Extract the components
components <- cbind.data.frame(
  colMeans(bsts.model$state.contributions[-(1:burn),"trend",]),                               
  colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]),
  as.Date(time(Y)))  
names(components) <- c("Trend", "Seasonality", "Date")
components <- melt(components, id="Date")
names(components) <- c("Date", "Component", "Value")

### Plot
seasonality.p = ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("output/seasonality.png", seasonality.p, width=5, height=5)
