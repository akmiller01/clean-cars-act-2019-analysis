list.of.packages <- c("sp","rgdal","leaflet","data.table","ggplot2","scales","rgeos","maptools","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/clean-cars-act-2019-analysis"
setwd(wd)

# Read in geometry
counties = readOGR("data/Maryland_Political_Boundaries__County_Boundaries/Maryland_Political_Boundaries__County_Boundaries.shp")

# Read in and format data
dat_march = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_March_2019.csv")
setnames(dat_march,"Count","March")
dat_april = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_April_2019.csv")
setnames(dat_april,"Count","April")
dat_may = fread("data/MVA_Electric_and_Hybrid_Vehicle_Registrations_by_County_as_of_May_2019.csv")
setnames(dat_may,"Count","May")
dat = merge(dat_march,dat_april)
dat = merge(dat,dat_may)
dat$diff_count_1 = dat$April-dat$March
dat$diff_count_2 = dat$May-dat$April
dat$diff_count = dat$May-dat$March

march_tot = sum(dat$March)
message("March 2019 ZEV total: ", march_tot)
april_tot = sum(dat$April)
message("April 2019 ZEV total: ", april_tot)
may_tot = sum(dat$May)
message("May 2019 ZEV total: ", may_tot)
growth_rate_1 = sum(dat$diff_count_1)
message("Monthly ZEV growth rate (March to April): ", growth_rate_1)
growth_rate_2 = sum(dat$diff_count_2)
message("Monthly ZEV growth rate (April to May): ", growth_rate_2)

months_until_jan_2020 = 7
months_until_jan_2025 = 7+(5*12)
message("Estimated January 2020 ZEV total: ", may_tot+(growth_rate_2*months_until_jan_2020))
message("Estimated January 2025 ZEV total: ", may_tot+(growth_rate_2*months_until_jan_2025))

# Harmonize county names
simpleCap <- function(x) {
  x = tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
simpleCap = Vectorize(simpleCap)

dat$County = simpleCap(dat$County)
dat$County[which(dat$County=="Prince Georges")] = "Prince George's"
dat$County[which(dat$County=="Queen Annes")] = "Queen Anne's"
dat$County[which(dat$County=="Saint Marys")] = "St. Mary's"

setnames(counties@data, "COUNTY", "County")
dat_tab = data.table(dat)[,.(
  March=sum(March),
  April=sum(April),
  May=sum(May),
  diff_count_1=sum(diff_count_1),
  diff_count_2=sum(diff_count_2),
  diff_count=sum(diff_count)
  ),by=.(County)]

# Quick bar plot
dat_long = melt(dat_tab,id.vars="County",measure.vars=c("March","April","May"),variable.name="Month")
dat_long_order = subset(dat_long,Month=="March")
dat_long_order = dat_long_order[order(-dat_long_order$value),]
dat_long$County = factor(dat_long$County, levels = dat_long_order$County)
p = ggplot(dat_long, aes(x=County, y=value, group=Month, fill=Month)) +
  geom_bar(stat="identity",position="dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y="Registrations",title="Maryland Active ZEV Registrations by County (2019)",fill="")
ggsave("output/registration_bar_plot.png", p, height=4, width=8)

# Prep for map
counties.f = fortify(counties, region="County")
setnames(counties.f,"id","County")

counties.f = merge(counties.f,dat_tab,by="County",all.x=T)

# Set our stops for the legend
diff.pal = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6")
diff.palbins = c(0, 2, 10, 40, 150)
names(diff.palbins)=c("≤ 0", "2", "10", "40", "≥ 150")

may.pal = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")
may.palbins = c(50, 100, 250, 1000, 5000)
names(may.palbins)=c("≤ 50", "100", "250", "1000", "≥ 5000")


diff_map = 
  ggplot(counties.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=diff_count,color="#ffffff",size=0.21))+
  scale_color_identity()+
  scale_size_identity()+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=diff.palbins,
    colors=diff.pal,
    values=rescale(diff.palbins)
  ) +
  expand_limits(x=counties.f$long,y=counties.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="",fill="Monthly growth\nin ZEV registrations")

ggsave("output/diff_map.png",diff_map,height=3,width=7)

may_map = 
  ggplot(counties.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=May,color="#ffffff",size=0.21))+
  scale_color_identity()+
  scale_size_identity()+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=may.palbins,
    colors=may.pal,
    values=rescale(may.palbins)
  ) +
  expand_limits(x=counties.f$long,y=counties.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="",fill="Active ZEV registrations\nas of May 2019")

ggsave("output/may_map.png",may_map,height=3,width=7)
