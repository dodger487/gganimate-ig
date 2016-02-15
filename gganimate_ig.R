# Make an animation of Instagram's growth over time

# Note: you will need devtools installed for this to run
# Only need to do this once.
# devtools::install_github("dgrtwo/gganimate") 
library(dplyr)
library(ggplot2)
library(gganimate)
library(RColorBrewer)

# Read data
df = read.csv("../LearnD3/data/photo_gps_time.csv", 
              header=FALSE, 
              col.names = c('uid', 'lat', 'lon', 't'))

df <- df %>% filter(-180 < lon, lon < 180)

df <- df %>% mutate(ts=as.POSIXct(t, origin="1970-01-01"), 
                    year_month=format(ts, "%Y-%m"))

df <- df %>% filter(year_month < "2013-09")

small <- df %>% sample_n(1e5)

# create a layer of borders
mapWorld <- borders("world", colour="gray50", fill="gray50")

small %>% 
  ggplot(aes(x=lon, y=lat)) + 
  mapWorld + 
  geom_bin2d(alpha=0.7) + 
  scale_fill_gradient(low="blue", high="hotpink", trans="log")

p <- small %>% 
  ggplot(aes(x=lon, y=lat, frame=year_month)) + 
  mapWorld + 
  geom_bin2d(alpha=0.7) + 
  scale_fill_gradient(low="blue", high="hotpink", trans="log")
gg_animate(p, "output1.gif")

theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  ret$rect <- element_blank()
  ret$strip.text <- element_blank()
  ret$axis.text <- element_blank()
  ret$axis.title <- element_blank()
  ret$plot.margin <- structure(
    c(0, 0, -1, -1), unit = "lines", 
    valid.unit = 3L, class = "unit")
  ret
}

p <- df %>% 
  ggplot(aes(x=lon, y=lat, frame=year_month)) + 
  mapWorld + 
  geom_bin2d(alpha=0.7) + 
  scale_fill_gradientn(
    colors=rev(brewer.pal(7, "Spectral")),  # 1. Nice colors!
    trans="log", 
    breaks=c(1, 10, 100, 1000, 10000)) +  # 2. Human-friendly
  theme_blank() +  # 3. Get rid of axes and grid
  coord_fixed()  # 4. Fix map distortion

gg_animate(p, 
           "output_final.gif", 
           interval=0.5,  # 5. Speed up animation
           ani.width=700, ani.height=400)
