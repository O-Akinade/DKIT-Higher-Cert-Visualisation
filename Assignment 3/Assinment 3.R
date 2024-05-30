library(tidyverse)
library(dplyr)
library(png)
library(gridExtra)
library(grid)

setwd("C:/Users/bosun/Desktop/HDip Data-Analysis/Semester 2/Visualisation/Assinment 3")


allplayer = read.csv(file = "player_file_fh_tr_v2.csv")

str(allplayer)
head(allplayer)
tail(allplayer)

# Part 2
dataPlayer4 = allplayer[,c(1,11,12,13)]

head(dataPlayer4)
tail(dataPlayer4)

distance = sum(dataPlayer4$speed4)/25
distance

mean(dataPlayer4$xpos4)
averagePos = c(mean(dataPlayer4$xpos4), mean(dataPlayer4$ypos4))
averagePos

# All players
averagePosAllPlayers = colMeans(allplayer)
averagePosAllPlayers



sprintDistance = sum(dataPlayer4[dataPlayer4$speed4 >= 7, 2]) / 25
sprintDistance


dists = c()

for(i in seq(1,7,1.5)) {
  dists = c(dists, sum(dataPlayer4[dataPlayer4$speed4 >= i, 2]) / 25)
}

dists


dataInSample = dataPlayer4[seq(1,68500,10),]
tail(dataInSample)

ggSp = ggplot(dataInSample, aes(x = ID / (25*60), y = speed4))
ggSp = ggSp + geom_line(col = "blue") + coord_cartesian(ylim = c(0, 10))
ggSp = ggSp + labs(title = "Speed Plot", x = "Time (minutes)", y = "Speed (m/s)")
ggSp = ggSp + theme(panel.background = element_rect(fill = 'white', colour = 'white'))
ggSp


ggMove = ggplot(dataInSample, aes(x=xpos4, y=ypos4))
ggMove = ggMove + geom_path()
ggMove = ggMove + coord_cartesian(xlim=c(-52,52), ylim=c(-34, 34))
ggMove = ggMove + theme(aspect.ratio = 68/104)
ggMove


dataInSample = mutate(dataInSample, 
                       speedCat = ifelse(dataInSample$speed4 >= 5, "High", "Low"))

ggMove = NULL
ggMove = ggplot(dataInSample, aes(x=xpos4, y=ypos4))
ggMove = ggMove + geom_path(aes(col=speedCat))
ggMove = ggMove + coord_cartesian(xlim=c(-52,52), ylim=c(-34, 34))
ggMove = ggMove + theme(aspect.ratio = 68/104)
ggMove 

dataInSample = mutate(dataInSample, groupSpeed="1")



image = png::readPNG("pitch3.png")
g_pic = rasterGrob(image, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)

ggMove =NULL
ggMove = ggplot(dataInSample, aes(x=xpos4, y=ypos4))
ggMove = ggMove + annotation_custom(g_pic, -Inf, Inf, -Inf, Inf)
ggMove = ggMove + geom_path(aes(col=speedCat, group = groupSpeed))
ggMove = ggMove + coord_cartesian(xlim=c(-52,52), ylim=c(-34, 34))
ggMove = ggMove + theme(aspect.ratio = 68/104)
ggMove = ggMove +theme(panel.grid = element_blank(),
                        axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(), legend.position = 'bottom')
ggMove = ggMove + scale_color_manual(values=c('red','blue'))
ggMove


ggHeat =NULL
ggHeat = ggplot(dataInSample, aes(x=xpos4, y=ypos4))
ggHeat = ggHeat + annotation_custom(g_pic, -Inf, Inf, -Inf, Inf)
ggHeat = ggHeat + coord_cartesian(xlim=c(-52,52), ylim=c(-34, 34))
ggHeat = ggHeat + theme(aspect.ratio = 68/104)
ggHeat = ggHeat + stat_density_2d(aes(fill= ..level..), geom = "polygon", show.legend = TRUE)
ggHeat = ggHeat + theme(panel.grid = element_blank(),
                         axis.title = element_blank(),
                         axis.text = element_blank(),
                         axis.ticks = element_blank(), legend.position = 'none')
ggHeat = ggHeat + scale_fill_distiller(palette = 'OrRd')
ggHeat


image2 =  image2  = png::readPNG("crest.png")
g_pic2 = rasterGrob(image2, width=unit(0.7,"npc"), height=unit(1,"npc"), interpolate = FALSE)


grid.arrange(g_pic2, ggHeat, ggMove,
             top="Player 4 Match Performance", widths=c(1,1), 
             heights = c(1,2) , layout_matrix = rbind(c(1, 3), c(2)))
