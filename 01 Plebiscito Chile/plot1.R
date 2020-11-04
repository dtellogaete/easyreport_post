# Librería
library(dplyr)
library(ggplot2)

# Import data
data <- read.csv("data/data.csv", sep = ";")

# Plebisctio 1989
# Fuente: Carlos Andrade Geywitz (1991). 
# «Reforma de la Constitución Política de la República de Chile de 1980
votes_1989 <- data %>%
              filter(year == 1989) %>%
              select(-c, -name_c, -name_a, -name_b, -year, -votantes)
votes_1989 <- data.frame(t(votes_1989))
votes_1989$votes <- as.integer(votes_1989$votes)
colnames(votes_1989) <- "votes"
votes_1989$label <- c("Apruebo", "Rechazo", "Nulo", "Blanco")

votes_1989 <- votes_1989 %>%
              arrange((votes))
votes_1989$percent <- votes_1989$votes/sum(votes_1989$votes)

## ggplot2 
dev.print(png, file = "plot1.png", width = 4000, height = 3900)
png(file = "plot1.png", bg = "transparent", units = "px",
    width = 4000, height = 3900, pointsize = 300)
ggplot(votes_1989, aes(reorder(label, votes/1e6), votes/1e6, 
                       fill=label))+
  geom_col()+
  coord_flip()+
  labs(y = "Votos (millones)", x = NULL)+
  scale_fill_manual(breaks = c("Apruebo", "Rechazo", "Nulo", "Blanco"), 
                    values=c("#FF0033", "white", "gray","#3300FF"))+
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
