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
dat = merge(dat_march,dat_april)
dat$diff_count = dat$April-dat$March

march_tot = sum(dat$March)
message("March 2019 ZEV total: ", march_tot)
april_tot = sum(dat$April)
message("April 2019 ZEV total: ", april_tot)
growth_rate = sum(dat$diff_count)
message("Monthly ZEV growth rate: ", growth_rate)

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
dat_tab = data.table(dat)[,.(March=sum(March),April=sum(April),diff_count=sum(diff_count)),by=.(County)]

# Quick bar plot
dat_long = melt(dat_tab,id.vars="County",measure.vars=c("March","April"),variable.name="Month")
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

april.pal = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")
april.palbins = c(50, 100, 250, 1000, 5000)
names(april.palbins)=c("≤ 50", "100", "250", "1000", "≥ 5000")


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

april_map = 
  ggplot(counties.f)+
  geom_polygon( aes(x=long,y=lat,group=group,fill=April,color="#ffffff",size=0.21))+
  scale_color_identity()+
  scale_size_identity()+
  coord_fixed(1) +
  scale_fill_gradientn(
    na.value="#d0cccf",
    guide="legend",
    breaks=april.palbins,
    colors=april.pal,
    values=rescale(april.palbins)
  ) +
  expand_limits(x=counties.f$long,y=counties.f$lat)+
  theme_classic()+
  theme(axis.line = element_blank(),axis.text=element_blank(),axis.ticks = element_blank())+
  labs(x="",y="",fill="Active ZEV registrations\nas of April 2019")

ggsave("output/april_map.png",april_map,height=3,width=7)
