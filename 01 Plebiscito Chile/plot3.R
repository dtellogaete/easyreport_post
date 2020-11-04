# Librería
library(dplyr)
library(ggplot2)

# Import data
data <- read.csv("data/data.csv", sep = ";")

# Plebisctio 1989
# Fuente: Carlos Andrade Geywitz (1991). 
# «Reforma de la Constitución Política de la República de Chile de 1980
votes <- data %>%
            filter(year == 1980) %>%
            select(-c, -name_c, -name_a, -name_b, -year, -votantes)
votes <- data.frame(t(votes))
votes$votes <- as.integer(votes$votes)
colnames(votes) <- "votes"
votes$label <- c("Si", "No", "Nulo", "Blanco")

votes <- votes %>%
            arrange((votes))
votes$percent <- votes$votes/sum(votes$votes)

## ggplot2 
dev.print(png, file = "plot1.png", width = 4000, height = 3900)
png(file = "plot1.png", bg = "transparent", units = "px",
    width = 4000, height = 3900, pointsize = 300)
ggplot(votes, aes(reorder(label, votes/1e6), votes/1e6, 
                       fill=label))+
  geom_col()+
  coord_flip()+
  labs(y = "Votos (millones)", x = NULL)+
  scale_fill_manual(breaks = c("Si", "No", "Nulo", "Blanco"), 
                    values=c("white", "#FF0033", "gray","#3300FF"))+
  geom_text(aes(label=scales::percent(percent)), size = 3.5)+
  guides(fill = FALSE)+
  scale_color_brewer()+
  theme(
    panel.background = element_rect(fill = "#E8EAFF") # bg of the panel
    , plot.background = element_rect(fill = "#E8EAFF") # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "#E8EAFF") # get rid of legend bg
    , legend.box.background = element_rect(fill = "#E8EAFF") # get rid of legend panel bg
  )
dev.off()
