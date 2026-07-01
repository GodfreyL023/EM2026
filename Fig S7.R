rm(list = ls())
library(deSolve)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(viridis)
library(scales)
library(paletteer)

slmod <- function(time, y, pars){
  with(as.list(pars), {
    B1 <- y[1]
    B2 <- y[2]
    dB1 <- r * B0^(1-k) * B1^k - z * B1 - miu * B1 * B2
    dB2 <- r * B0^(1-k) * B2^k - z * B2 - miu * B1 * B2
    return(list(c(dB1, dB2)))
  })
}

pars <- c(r = 1, B0 = 1, z = 0.5, miu = 1, k = 0.2)
time <- seq(0, 50, by=0.5)
for(i in 1:10){
  ini <- runif(2, 0.1, 1)
  assign(paste0("out", i), 
         ode(ini, time, slmod, pars))
}

mydf <- data.frame()
for (i in 1:10) {
  outdf <- as.data.frame(get(paste0("out",i)))[,2:3]
  colnames(outdf) <- c("B1","B2")
  outdf <- gather(outdf, key = "species", value = "biomass")
  outdf$time <- time
  outdf$simul <- as.character(i)
  mydf <- rbind(mydf, outdf)
}
custom_labeller <- function(variable, value) {
  if (variable == "species") {
    return(c("B1" = expression(B[1]), "B2" = expression(B[2])))
  }
  return(value)
}
p1 <- ggplot(mydf, aes(time, biomass, group = simul, color = simul)) +
  geom_line() +
  facet_wrap(~species, labeller = labeller(species = function(value){
    return(c('B1'="B_1", 'B2'="B_2"))
  })) +
  labs(x="Time",y="Biomass")+
  scale_x_continuous(labels = function(x){scientific(x,digits=0)}, 
                     breaks = pretty(mydf$time, n = 2))+
  scale_color_manual(values = paletteer_dynamic(`"cartography::orange.pal"`, n=15))+
  #geom_text(aes(max(time)*0.9, max(biomass)*0.2), label = "k = 0.2", color = "black")+
  theme_classic()+
  theme(legend.position = "none")
p1

pars <- c(r = 1, B0 = 1, z = 1, miu = 0.1, k = 0.5)
time <- seq(0, 160, by=0.5)
for(i in 1:10){
  ini <- runif(2, 0.1, 1)
  assign(paste0("out", i), 
         ode(ini, time, slmod, pars))
}

mydf <- data.frame()
for (i in 1:10) {
  outdf <- as.data.frame(get(paste0("out",i)))[,2:3]
  colnames(outdf) <- c("B1","B2")
  outdf <- gather(outdf, key = "species", value = "biomass")
  outdf$time <- time
  outdf$simul <- as.character(i)
  mydf <- rbind(mydf, outdf)
}
custom_labeller <- function(variable, value) {
  if (variable == "species") {
    return(c("B1" = expression(B[1]), "B2" = expression(B[2])))
  }
  return(value)
}
p2 <- ggplot(mydf, aes(time, biomass, group = simul, color = simul)) +
  geom_line() +
  facet_wrap(~species, labeller = labeller(species = function(value){
    return(c('B1'="B_1", 'B2'="B_2"))
  })) +
  labs(x="Time",y="Biomass")+
  scale_x_continuous(labels = function(x){scientific(x,digits=0)}, 
                     breaks = pretty(mydf$time, n = 3))+
  scale_color_manual(values = paletteer_dynamic(`"cartography::orange.pal"`, n=15))+
  #geom_text(aes(max(time)*0.9, max(biomass)*0.2), label = "k = 0.8", color = "black")+
  theme_classic()+
  theme(legend.position = "none")
p2
p3 <- grid.arrange(p1,p2,ncol = 1)
ggsave("F:/OneDrive/AcadamyData/Comment_on_sublinear_models/r = 1, B0 = 1, z = 0.5, k = 0.5, miu = 1.png", 
       p3, width = 5, height = 5, dpi = 500)
ggsave("F:/OneDrive/AcadamyData/Comment_on_sublinear_models/r = 1, B0 = 1, z = 0.5, k = 0.5, miu = 1.eps", 
       p3, width = 5, height = 5)

