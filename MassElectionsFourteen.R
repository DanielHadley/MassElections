# setwd("/Users/dphnrome/Documents/Git/MassElections")
setwd("C:/Documents and Settings/dhadley/Documents/GitHub/MassElections")

d <- read.csv("./rawData/votePercents.csv")


library(plyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(reshape2)
# Maping tools
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("ggmap")


#### Map it with city-town GIS layer ####
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# http://www.kevjohnson.org/making-maps-in-r/
# http://www.kevjohnson.org/making-maps-in-r-part-2/


# First prepare the parcel df
mass <- readOGR(dsn="./shapefiles", layer="GISDATA_CENSUS2010TOWNS_POLY")
mass@data$id = rownames(mass@data)
names(mass)
mass.points <- fortify(mass, region="id")
mass.df <- join(mass.points, mass@data, by="id")


# Prepare data to match with "mass" gis layer
d$TOWN <- toupper(d$Municipality)

# Combine data
mass.df <- merge(mass.df, d, by="TOWN")

# Map
map <- get_map(location = "Grafton, Massachusetts", zoom=8, maptype="roadmap", color = "bw")
ggmap(map)


#### Automatic Maps ####
# uncomment to use #

# for(i in 27 : 46){
#   ggmap(map) +
#     geom_polygon(data=mass.df, aes(x=long, y=lat, group=group, fill=mass.df[,i]), colour=NA, alpha=0.7) +
#     scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
#     labs(fill="") +
#     theme_nothing(legend=TRUE) + ggtitle(paste("Percent of Votes for ",colnames(mass.df)[[i]],sep=""))
#   
#   ggsave(paste("./plots/Map",i,".png",sep=""), dpi=300, width=6, height=5)
# }


####  Visualize ####


lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )

# add Mass to d
m = as.data.frame(mass)
d <- merge(m, d, by="TOWN")
# new variable
d$WhichGov <- ifelse(d$Baker > .5, "Baker",
                     ifelse(d$Coakley > .5, "Coakley", "No Majority"))


ggplot(d, aes(x=d$Baker)) + geom_histogram(binwidth=.03, colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Votes for Baker") + xlab("Percent of City/Town's Vote")+ylab("Number of Cities/Towns") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot01.png", dpi=300, width=5, height=3)


ggplot(d, aes(x=d$Coakley)) + geom_histogram(binwidth=.03, colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley") + xlab("Percent of City/Town's Vote")+ylab("Number of Cities/Towns") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot02.png", dpi=300, width=5, height=3)


mass.df.top <- mass.df[which(mass.df$POP2010 > 70000),]

ggplot(mass.df.top, aes(x=reorder(mass.df.top$TOWN, -mass.df.top$Coakley), y=mass.df.top$Coakley)) + geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley in Cities > 70k pop.") + xlab("Cities")+ylab("Percent of City/Town's Vote") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot03.png", dpi=300, width=5, height=3)


Coakley <- as.data.frame(d$Coakley)
Baker <- as.data.frame(d$Baker)

Coakley <- rename(Coakley, c("d$Coakley"="votes"))
Baker <- rename(Baker, c("d$Baker"="votes"))

Coakley$Candidate <- "Coakley"
Baker$Candidate <- "Baker"

long <- rbind(Coakley, Baker)
ggplot(long, aes(votes, fill = Candidate)) + geom_density(alpha = 0.5, colour="white") +
  my.theme + ggtitle("Votes for Baker v Coakley") + xlab("Percent of City/Town's Vote")+ylab("Density") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot04.png", dpi=300, width=5, height=3)


ggplot(d, aes(x=d$Coakley, weight=d$POP2010)) + geom_histogram(binwidth=.03, colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley") + xlab("Percent of City/Town's Vote")+ylab("Sum Population of Places in This Bin") + 
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = comma)

ggsave("./plots/plot05.png", dpi=300, width=5, height=3)


CitiesForBaker <- d[which(d$POP2010 > 40000 & d$Baker > .5),]

ggplot(CitiesForBaker, aes(x=reorder(CitiesForBaker$TOWN, -CitiesForBaker$Baker), y=CitiesForBaker$Baker)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Cities for Baker: the Most Populous Places") + xlab("Cities")+ylab("Percent of City/Town's Vote for Baker") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot06.png", dpi=300, width=5, height=3)


ggplot(d, aes(x=POP2010, y=d$Coakley, color=WhichGov)) +
  geom_point(shape=1) + scale_x_log10() +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Vote By Population") + xlab("Log Of Population")+ylab("Percent of City/Town's Vote for Coakley")

ggsave("./plots/plot07.png", dpi=300, width=5, height=5)


Baker <- d[which(d$WhichGov == "Baker"),]

ggplot(Baker, aes(x=POP2010, y=Baker, color=WhichGov)) +
  geom_point(shape=1) + scale_x_log10() +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Vote By Population") + xlab("Log Of Population")+ylab("Percent of City/Town's Vote for Baker")

ggsave("./plots/plot08.png", dpi=300, width=5, height=5)


#### Census + Crime + Voting Visualization ####
# This brings in census data, which is a smaller sample, but more columns

dma <- read.csv("./rawData/MassData.csv")

dma$Municipality <- dma$Geography 
dma <- merge(dma, d, by="Municipality")



ggplot(dma, aes(x=percent.college.or.grad.school, y=Baker)) +
  geom_point(shape=1, color = pinkish_red) + geom_smooth(method=lm, color = "grey") +
  my.theme + ggtitle("Education/Baker") + xlab("Percent Currently Attending College+Grad?")+ylab("Percent of City/Town's Vote for Baker") +
  annotate("text", x=94, y=.18, label="Amherst", size=2)

ggsave("./plots/plot09.png", dpi=300, width=5, height=3)



ggplot(dma, aes(x=dma$percent.bachelor.s.degree.or.higher, y=Baker, color = WhichGov)) +
  geom_point(shape=1) + geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Education/Baker") + xlab("Percent Bachelors or Higher")+ylab("Percent of City/Town's Vote for Baker") 

ggsave("./plots/plot10.png", dpi=300, width=5, height=3)


ggplot(dma, aes(x=dma$graduate.or.professional.degree, y=Baker, color = WhichGov)) +
  geom_point(shape=1) + geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Education/Baker") + xlab("Professional Degree or Higher")+ylab("Percent of City/Town's Vote for Baker") 

ggsave("./plots/plot11.png", dpi=300, width=5, height=3)


