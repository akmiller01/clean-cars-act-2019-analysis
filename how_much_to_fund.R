list.of.packages <- c("data.table","reshape2","bsts","lubridate","dplyr","ggplot2","scales","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/clean-cars-act-2019-analysis"
setwd(wd)

# Read in and format data
dat_march = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_March_2019.csv")
setnames(dat_march,"Count","2019-03-01")
dat_april = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_April_2019.csv")
setnames(dat_april,"Count","2019-04-01")
dat_may = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_May_2019.csv")
setnames(dat_may,"Count","2019-05-01")
dat_june = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_June_2019.csv")
setnames(dat_june,"Count","2019-06-01")
dat_august = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_August_2019.csv")
setnames(dat_august,"Count","2019-08-01")
dat_september = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_September_2019.csv")
setnames(dat_september,"Count","2019-09-01")
dat_december = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_December_2019.csv")
setnames(dat_december,"Count","2019-12-01")
dat_reg = merge(dat_march,dat_april)
dat_reg = merge(dat_reg,dat_may)
dat_reg = merge(dat_reg,dat_june)
dat_reg = merge(dat_reg,dat_august)
dat_reg = merge(dat_reg,dat_september)
dat_reg = merge(dat_reg,dat_december)

dat_melt = melt(dat_reg,id.vars=c("Fuel_Category","County"),variable.name="Date")
dat_cast = dcast(dat_melt,County+Date~Fuel_Category)
names(dat_cast) = make.names(names(dat_cast))
dat_tab = data.table(dat_cast)[,.(
  PHEV=sum(Plug.In.Hybrid),
  BEV=sum(Electric)
),by=.(Date)]
dat_tab$Date = as.Date(dat_tab$Date)
missing_months = data.frame(
  Date=as.Date(
    c(
      "2019-01-01",
      "2019-02-01",
      "2019-07-01",
      "2019-10-01",
      "2019-11-01"
      )
  )
)
dat_tab = rbindlist(list(dat_tab,missing_months), fill=T)
dat_tab = dat_tab[order(dat_tab$Date)]
dat_tab$PHEV = na.approx(dat_tab$PHEV,na.rm=F)
dat_tab$BEV = na.approx(dat_tab$BEV,na.rm=F)
# First difference, as MVA data is in total
dat_tab$BEV = c(NA,diff(dat_tab$BEV))
dat_tab$PHEV = c(NA,diff(dat_tab$PHEV))

dat_alliance = fread("data/alliance_dat.csv")
dat_alliance$Date = as.Date(dat_alliance$Date,format="%m/%d/%y")

# Combine all our sources
dat = rbindlist(list(dat_alliance,dat_tab),fill=T)
dat = dat[order(dat$Date)]
dat$BEV = na.approx(dat$BEV)
dat$PHEV = na.approx(dat$PHEV)

bev_y = ts(dat$BEV, frequency=12, start=c(2011,1))

### Run the bsts model
bev_ss <- AddLocalLinearTrend(list(), bev_y)
bev_ss <- AddSeasonal(bev_ss, bev_y, nseasons = 12)
bev_bsts.model <- bsts(bev_y, state.specification = bev_ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
bev_burn <- SuggestBurn(0.1, bev_bsts.model)

### Predict, FY 2020 ends June 2020.
# Predict until August 2022 for 3 FY
bev_p <- predict.bsts(bev_bsts.model, horizon = 36, burn = bev_burn, quantiles = c(.025, .975))
bev_p.ts = ts(rep(NA,36),frequency=12,start=c(2020,1))
### Actual versus predicted
bev_d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(bev_bsts.model$one.step.prediction.errors[-(1:bev_burn),])+bev_y),  
    as.numeric(bev_p$mean)),
  # actual data and dates 
  as.numeric(c(bev_y,rep(NA,36))),
  c(as.Date(time(bev_y)),as.Date(time(bev_p.ts))))
names(bev_d2) <- c("Fitted", "Actual", "Date")

### 95% forecast credible interval
bev_posterior.interval <- cbind.data.frame(
  as.numeric(bev_p$interval[1,]),
  as.numeric(bev_p$interval[2,]), 
  subset(bev_d2, Date>as.Date("2019-12-01"))$Date)
names(bev_posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
bev_d3 <- left_join(bev_d2, bev_posterior.interval, by="Date")
bev_d3$Fitted[which(bev_d3$Fitted<0)] = 0
bev_d3$LL[which(bev_d3$LL<0)] = 0
bev_d3$UL[which(bev_d3$UL<0)] = 0

phev_y = ts(dat$PHEV, frequency=12, start=c(2011,1))

### Run the bsts model
phev_ss <- AddLocalLinearTrend(list(), phev_y)
phev_ss <- AddSeasonal(phev_ss, phev_y, nseasons = 12)
phev_bsts.model <- bsts(phev_y, state.specification = phev_ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
phev_burn <- SuggestBurn(0.1, phev_bsts.model)

### Predict, FY 2020 ends June 2020.
# Predict until June 2022 for 3 FY
phev_p <- predict.bsts(phev_bsts.model, horizon = 36, burn = phev_burn, quantiles = c(.025, .975))
phev_p.ts = ts(rep(NA,36),frequency=12,start=c(2020,1))
### Actual versus predicted
phev_d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(phev_bsts.model$one.step.prediction.errors[-(1:phev_burn),])+phev_y),  
    as.numeric(phev_p$mean)),
  # actual data and dates 
  as.numeric(c(phev_y,rep(NA,36))),
  c(as.Date(time(phev_y)),as.Date(time(phev_p.ts))))
names(phev_d2) <- c("Fitted", "Actual", "Date")

### 95% forecast credible interval
phev_posterior.interval <- cbind.data.frame(
  as.numeric(phev_p$interval[1,]),
  as.numeric(phev_p$interval[2,]), 
  subset(phev_d2, Date>as.Date("2019-12-01"))$Date)
names(phev_posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
phev_d3 <- left_join(phev_d2, phev_posterior.interval, by="Date")
phev_d3$Fitted[which(phev_d3$Fitted<0)] = 0
phev_d3$LL[which(phev_d3$LL<0)] = 0
phev_d3$UL[which(phev_d3$UL<0)] = 0

# Merge BEV and PHEV projections
d3 = merge(bev_d3,phev_d3,by="Date",suffixes=c(".bev",".phev"))
d3$LL.bev[which(is.na(d3$LL.bev))] = d3$Actual.bev[which(is.na(d3$LL.bev))]
d3$UL.bev[which(is.na(d3$UL.bev))] = d3$Actual.bev[which(is.na(d3$UL.bev))]
d3$Fitted.bev[which(!is.na(d3$Actual.bev))] = d3$Actual.bev[which(!is.na(d3$Actual.bev))]
d3$Actual.bev = NULL
d3$LL.phev[which(is.na(d3$LL.phev))] = d3$Actual.phev[which(is.na(d3$LL.phev))]
d3$UL.phev[which(is.na(d3$UL.phev))] = d3$Actual.phev[which(is.na(d3$UL.phev))]
d3$Fitted.phev[which(!is.na(d3$Actual.phev))] = d3$Actual.phev[which(!is.na(d3$Actual.phev))]
d3$Actual.phev = NULL

proj.p = ggplot(data=d3, aes(x=Date)) +
  geom_line(aes(y=Fitted.bev, colour = "BEV"), size=1.2) +
  geom_line(aes(y=Fitted.phev, colour = "PHEV"), size=1.2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_ribbon(aes(ymin=LL.bev, ymax=UL.bev, fill="BEV"), alpha=0.5) +
  geom_ribbon(aes(ymin=LL.phev, ymax=UL.phev, fill="PHEV"), alpha=0.5) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title="Forecasted monthly Maryland BEV and PHEV sales, 2011-2022")
ggsave("output/bev_phev_projection.png", proj.p, width=10, height=5)

# Assume all unfunded since May
# Assume 3,000 per bev, 1,500 per phev
d3_unfunded = subset(d3,Date>=as.Date("2019-05-01"))
d3_unfunded$Fitted.cost = 3000*d3_unfunded$Fitted.bev + 1500*d3_unfunded$Fitted.phev
d3_unfunded$LL.cost = 3000*d3_unfunded$LL.bev + 1500*d3_unfunded$LL.phev
d3_unfunded$UL.cost = 3000*d3_unfunded$UL.bev + 1500*d3_unfunded$UL.phev

cost.p = ggplot(data=d3_unfunded, aes(x=Date)) +
  geom_ribbon(aes(ymin=LL.cost, ymax=UL.cost), fill="grey", alpha=0.5) +
  geom_line(aes(y=Fitted.cost),color="blue", size=1.2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title="Forecasted monthly Maryland Tax Credit Cost, May 2019-June 2022") +
  scale_y_continuous(labels=scales::dollar)
ggsave("output/bev_phev_cost_projection.png", cost.p, width=10, height=5)

fy2020 = subset(d3_unfunded,Date<as.Date("2020-07-01"))
fy2021 = subset(d3_unfunded,Date>=as.Date("2020-07-01") & Date<as.Date("2021-07-01"))
fy2022 = subset(d3_unfunded,Date>=as.Date("2021-07-01") & Date<as.Date("2022-07-01"))

fy2020.min = sum(fy2020$LL.cost, na.rm=T)
fy2020.est = sum(fy2020$Fitted.cost, na.rm=T)
fy2020.max = sum(fy2020$UL.cost, na.rm=T)

fy2021.min = sum(fy2021$LL.cost, na.rm=T)
fy2021.est = sum(fy2021$Fitted.cost, na.rm=T)
fy2021.max = sum(fy2021$UL.cost, na.rm=T)

fy2022.min = sum(fy2022$LL.cost, na.rm=T)
fy2022.est = sum(fy2022$Fitted.cost, na.rm=T)
fy2022.max = sum(fy2022$UL.cost, na.rm=T)

fy.cost.df = data.frame(
  fy=c(2020,2020,2020,2021,2021,2021,2022,2022,2022),
  type=rep(c("Lower threshold","Point estimate","Upper threshold"),3),
  cost=c(fy2020.min,fy2020.est,fy2020.max,fy2021.min,fy2021.est,fy2021.max,fy2022.min,fy2022.est,fy2022.max)
)
fwrite(fy.cost.df,"output/Maryland Tax Credit Total FY cost estimates.csv")

fy.cost.p = ggplot(data=subset(fy.cost.df,type="Point estimate"), aes(x=fy)) +
  geom_bar(stat="identity",aes(y=cost),fill="red", size=1.2) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  labs(title="Forecasted Annual Maryland Tax Credit Cost, FY2020-FY2022") +
  scale_y_continuous(labels=scales::dollar)
ggsave("output/fy_cost_projection.png", fy.cost.p, width=7, height=5)
