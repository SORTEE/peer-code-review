## Packages i might use #######
rm(list=ls())

install.packages("devtools")
devtools::install_github("squidgroup/squidSim")
library(glmmTMB)
library(tidyverse)
library(lme4)
library(DHARMa)
library(hablar)
dat <- read_csv("Very Important Data.csv")

theme_Publication <- function(base_size = 15, base_family = "Arial") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2), hjust = 0.5
      ),
      text = element_text(),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.direction = "horizontal",
      # legend.key.size = unit(0.2, "cm"),
      legend.position = "bottom",
      # legend.justification = "bottom",
      # legend.box.just = "right",
      # legend.margin = margin(6, 6, 6, 6),
      legend.title = element_text(face = "italic"),
      # legend.spacing.x = unit(1.0, "cm"),
      plot.margin = unit(c(10, 5, 5, 5), "mm"),
      legend.key.width = unit(2, "line"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    ))
}


dat$molly <- as.factor(dat$molly)
dat$sortee <- as.factor(dat$sortee)

#Analysis Code
#Model 1 with graph

m1 = glm(attendees ~ molly, family = "poisson", data = dat)

summary(m1)

#plot graph for publication
plot(x = dat$molly, y = dat$attendees, data = dat)
ggplot(dat, aes(x = molly, y = attendees)) + geom_jitter() + theme_Publication() + facet_grid(.~sortee)


#m2?
m1 = glm(attendees ~ sortee, family = "poisson", data = dat)
summary(m1)

#plot graph for publication
plot(x = dat$molly, y = dat$attendees, data = dat)
ggplot(dat, aes(x = sortee, y = attendees, colour = sortee)) +
  geom_jitter()

#make a new column
dat2 <- cbind(dat, group = paste(dat$molly, dat$sortee))
write.csv(dat2, "more data.csv")
  
#check model
m1 = glm(attendees ~ sortee, family = "poisson", data = dat)
m1b = glm(attendees ~ group, family = "poisson", data = dat2)

AIC(m1, m1b)


