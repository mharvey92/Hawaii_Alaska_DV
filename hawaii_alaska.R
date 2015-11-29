library(maptools)
library(mapproj)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggthemes)
library(grid)
library(gridExtra)

#Graph with HW and AL
# copied parts from http://loloflargenumbers.com/blog/?p=206#.VlKDG4Sjk7Y

us50_shp <- readShapePoly("states.shp")
us50_df <- as.data.frame(us50_shp) 
us50_points <- sp2tmap(us50_shp) 
names(us50_points) <- c("id", "x", "y") 
us50 <- merge(x = us50_df, y = us50_points, by.x = "DRAWSEQ", by.y = "id") 
ggplot(data = us50, aes(x=x, y=y, group = DRAWSEQ)) + geom_polygon(color = "black", fill = "white")
cont_us <- us50[us50$STATE_ABBR != "HI" & us50$STATE_ABBR != "AK", ] 
ak <- us50[us50$STATE_ABBR == "AK", ] 
hi <- us50[us50$STATE_ABBR == "HI", ] 
centerState <- function(.df) {
  .df$x <- .df$x - (diff(range(.df$x, na.rm = T))/2 + min(.df$x, na.rm = T))
  .df$y <- .df$y - (diff(range(.df$y, na.rm = T))/2 + min(.df$y, na.rm = T))
  return(.df)
}
scaleState <- function(.df, scale_matrix, scale_factor, x_shift, y_shift) {
  .df <- centerState(.df)
  coords <- t(cbind(.df$x, .df$y))
  scaled_coord <- t(scale_factor*scale_matrix %*% coords)
  .df$x <- scaled_coord[,1] + x_shift
  .df$y <- scaled_coord[,2] + y_shift
  return(.df)
}
scale_mat <- matrix(c(1,0,0,1.25), ncol = 2, byrow = T)
ak_scale <- scaleState(ak, scale_mat, 0.4, x_shift = -120, y_shift = 25)
hi_scale <- scaleState(hi, scale_mat, 1.5, x_shift = -107, y_shift = 25)

all_us <- rbind(cont_us, ak_scale, hi_scale)
proj_type <- "azequalarea"
projected <- mapproject(x = all_us$x, y = all_us$y, projection=proj_type)
all_us$x_proj <- projected[["x"]]
all_us$y_proj <- projected[["y"]]
save(all_us,file="all_us.rda")

blankground <- function() {
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.margin = unit(0,"null"),
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()
        
  )
}

# DATA MERGE

data<-read.csv("statebypop.csv")
colnames(data)[2] <- "STATE_ABBR"
newdata <- merge(data, all_us, by = "STATE_ABBR")

# index color

colors <- colorRampPalette(c('red', 'blue'))
group <- colors(20)[as.numeric(cut(newdata$Ttl_pop, 20))]

ggplot(data =all_us, aes(x=x, y=y))+
  geom_polygon(color = "white", aes(fill = group)) +
  blankground()

