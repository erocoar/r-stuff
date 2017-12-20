library(ggplot2)
library(ggforce)
library(grid)
library(extrafonts)

loadfonts()

# christmas card .... with ggforce
# didn't want to spend too much time on it so it doesn't look all that smooth. except the snowflakes!
# and also not worth fixing the snowflake rotations to use outside of 0,0 ... so i just insert them via viewport

rot <- function(angle){
  a = angle * pi / 180
  matrix(c(cos(a), -sin(a), sin(a), cos(a)), ncol = 2)
}

lineNorm <- function(x0, y0, x1, y1) {
  o <- cbind(x1, y1) - cbind(x0, y0)
  as.matrix(o / sqrt(sum(o^2)))
}

seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to"))

sf1 <- function(center_x, center_y) {
  struct <- data.frame(x = rep(center_x, 8),
                       y = rep(center_y, 8),
                       xend = center_x + c(-1, -2, -2, -1, 1, 2, 2, 1),
                       yend = center_y + c(2, 1, -1, -2, -2, -1, 1, 2))
  
  sides <- t(cbind(rot(30) %*% t(as.matrix(struct[, 3:4])),
                rot(-30) %*% t(as.matrix(struct[, 3:4]))))
  
  sideN <- cbind(rep(struct$xend/2, 2), rep(struct$yend/2, 2)) + 
    2*lineNorm(rep(struct$xend/2, 2), rep(struct$yend/2, 2), sides[, 1], sides[, 2])
  

  upper <- t(cbind(rot(1) %*% t(as.matrix(struct[, 3:4]/1.1)),
                   rot(-1) %*% t(as.matrix(struct[, 3:4]/1.1))))
  uppern <- cbind(rep(struct$xend, 2), rep(struct$yend, 2)) + 
    1.5 * lineNorm(rep(struct$xend/1.1, 2), rep(struct$yend/1.1, 2), upper[, 1], upper[, 2])
  
  lower = t(cbind(rot(100) %*% t(as.matrix(struct[, 3:4]/3.3)),
                  rot(-100) %*% t(as.matrix(struct[, 3:4]/3.3))))

  lowern <- cbind(rep(struct$xend/3.3, 2), rep(struct$yend/3.3, 2)) + 
    1 * lineNorm(rep(struct$xend/3.3, 2), rep(struct$yend/3.3, 2), lower[, 1], lower[, 2])
  
  p <- ggplot() + coord_fixed() + theme_void() + theme(legend.position = "none") + 
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend,
                  yend = struct$yend,
                  size = ..index..*-1), colour = "blue") +
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend,
                  yend = struct$yend,
                  size = ..index..*-1, alpha = ..index..), colour = "#c6fbff") +
    geom_link(aes(x = rep(struct$xend/2, 2),
                  y = rep(struct$yend/2, 2),
                  xend = sideN[, 1],
                  yend = sideN[, 2],
                  size = ..index..*-1, alpha = ..index..*2), colour = "blue", n = 100) +
    geom_link(aes(x = rep(struct$xend/2, 2),
                  y = rep(struct$yend/2, 2),
                  xend = sideN[, 1],
                  yend = sideN[, 2],
                  size = ..index..*-1, alpha = ..index..*2), colour = "#abf0ff", n = 100) +
    geom_link(aes(x = rep(struct$xend/1.1, 2),
                  y = rep(struct$yend/1.1, 2),
                  xend = uppern[, 1],
                  yend = uppern[, 2],
                  size = ..index..*-1, alpha = ..index..*2), colour = "blue", n = 100) +
    geom_link(aes(x = rep(struct$xend/1.1, 2),
                  y = rep(struct$yend/1.1, 2),
                  xend = uppern[, 1],
                  yend = uppern[, 2],
                  size = ..index..*-1, alpha = ..index..*2), colour = "#abf0ff", n = 100) +
    geom_link(aes(x = rep(struct$xend/3.3, 2),
                  y = rep(struct$yend/3.3, 2),
                  xend = lowern[, 1],
                  yend = lowern[, 2], size = ..index..*-1, alpha = ..index..), colour = "blue", n = 100) +
    
    geom_link(aes(x = rep(struct$xend/3.3, 2),
                  y = rep(struct$yend/3.3, 2),
                  xend = lowern[, 1],
                  yend = lowern[, 2], size = ..index..*-1, alpha = ..index..), colour = "#99c4ce", n = 100) +
    
    geom_link(aes(x = rep(struct$x, 2),
                  y = rep(struct$y, 2),
                  xend = lowern[, 1],
                  yend = lowern[, 2], size = ..index..*-0.8, alpha = ..index..*0.7), colour = "white", n = 100) 
  return(p)
}

sf1(0,0)

sf2 <- function(center_x, center_y) {
  struct <- data.frame(x = rep(center_x, 8),
                       y = rep(center_y, 8),
                       xend = center_x + c(-1, -2, -2, -1, 1, 2, 2, 1),
                       yend = center_y + c(2, 1, -1, -2, -2, -1, 1, 2))
  
  sides <- t(cbind(rot(30) %*% t(as.matrix(struct[, 3:4])),
                   rot(-30) %*% t(as.matrix(struct[, 3:4]))))
  
  snorm <- lineNorm(rep(struct$x, 2), rep(struct$y, 2), sides[, 1], sides[, 2])
  
  sides2 <- t(cbind(rot(30) %*% t(as.matrix(struct[, 3:4]/2)),
                    rot(-30) %*% t(as.matrix(struct[, 3:4]/2))))
  
  p <- ggplot() + coord_fixed() + theme_void() + theme(legend.position = "none") +
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend/1.7, #make lines shorter TODO a
                  yend = struct$yend/1.7,
                  size = ..index..*-1), colour = "#64b7ff") +
    
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend/1.7, #make lines shorter TODO a
                  yend = struct$yend/1.7,
                  size = ..index..*-1, alpha = ..index..*-1), colour = "#025e73") +
    
    geom_link(aes(x = rep(struct$xend/2, 2),
                  y = rep(struct$yend/2, 2),
                  xend = snorm[, 1]*2,
                  yend = snorm[, 2]*2,
                  size = ..index..*-1), colour = "blue", n = 50) +
    
    geom_link(aes(x = rep(struct$xend/2, 2),
                  y = rep(struct$yend/2, 2),
                  xend = snorm[, 1]*2,
                  yend = snorm[, 2]*2,
                  size = ..index..*-1, alpha = ..index..), colour = "#abf0ff", n = 50) 
  return(p)
}

sf2(0, 0)

sf3 <- function(center_x, center_y) {
  struct <- data.frame(x = rep(center_x, 8),
                       y = rep(center_y, 8),
                       xend = center_x + c(-1, -2, -2, -1, 1, 2, 2, 1),
                       yend = center_y + c(2, 1, -1, -2, -2, -1, 1, 2))
  
  sides <- t(cbind(rot(30) %*% t(as.matrix(struct[, 3:4])),
                   rot(-30) %*% t(as.matrix(struct[, 3:4]))))
  
  p <- ggplot() + coord_fixed() + theme_void() + theme(legend.position = "none") + 
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend,
                  yend = struct$yend,
                  size = ..index..*-1), colour = "#fffadc") +
    
    geom_link(aes(x = struct$x,
                  y = struct$y,
                  xend = struct$xend,
                  yend = struct$yend,
                  size = ..index..*-1, alpha = ..index..), colour = "#b0daff") +

    geom_link(aes(x = rep((struct$xend+center_x)/2, 2),
                  y = rep((struct$yend+center_y)/2, 2),
                  xend = (sides[, 1]),
                  yend = (sides[, 2]),
                  size = ..index..*-0.9), colour = "#1b578c", n = 50) +
  
    geom_link(aes(x = rep((struct$xend+center_x)/2, 2),
                  y = rep((struct$yend+center_y)/2, 2),
                  xend = (sides[, 1]),
                  yend = (sides[, 2]),
                  size = ..index..*-1, alpha = ..index..*-1), colour = "white", n = 50) 
    return(p)
}

sf3(0, 0)

# ---------- TREE
tannenbaum <- data.frame(x=c(-0.75, -1.75, -2.75, -1.75, -1.25, -0.75),
                         y=c(2, 2, 2, 3, 4, 5))

tannenbaum_fill_left <- with(tannenbaum, {
  cbind(sapply(seq(0, 0.75, 0.25), FUN = function(i) i + x),
        sapply(seq(0, 3, 1), FUN = function(i) i + y))
})

tannenbaum_fill_left_fill <- tannenbaum

tannenbaum_fill_left_fill <- do.call(rbind, lapply(seq(500), function(i) {
  tannenbaum_fill_left_fill$x <- tannenbaum_fill_left_fill$x + c(0, i/500)
  tannenbaum_fill_left_fill$y[2] <- tannenbaum_fill_left_fill$y[2] - i*0.3/500
  tannenbaum_fill_left_fill$group <- i+1
  tannenbaum_fill_left_fill
}))

tannenbaum_fill_left_fill <- tannenbaum_fill_left_fill[-3000, ]

t_x <- c(seq(-2.3, -1.8, length=50),
              seq(-2.1, -1.5, length=50),
              seq(-1.7, -1, length=50),
              seq(-1.6, 0, length=75))

t_xend <- c(seq(2.3, 1.8, length=50),
            seq(2.1, 1.5, length=50),
            seq(1.7, 1, length=50),
            seq(1.6, 0, length=75))

t_y <- c(seq(2, 3, length=50),
         seq(3, 4, length=50),
         seq(4, 5, length=50),
         seq(5, 8, length=75))

# --------- STAR
stern <- data.frame(x = c(0, 0.55, 0.45, 0.8, 0.3, 0.05, -0.15, -0.8, -0.4, -0.7, 0),
                    y = c(8, 7.5, 8.5, 8.9, 9.15, 9.8, 9.15, 9.05, 8.5, 7.6, 8))

stern_fill <- stern[c(1, rep(seq(3, nrow(stern)-2, 2), each = 2), nrow(stern)), ]
stern_fill <- cbind(stern[seq(1, nrow(stern)-2, 2), ],
                    stern[seq(3, nrow(stern), 2), ],
                    stern[seq(2, nrow(stern), 2), ])

# -------- BG
g <- rasterGrob(rev(blues9)[-c(1:2)], width=unit(1, "npc"), height = unit(1, "npc"), 
                interpolate = TRUE)

# -------- MAIN PLOT
MAIN <- ggplot() + 
  coord_fixed() + theme_void() + theme(legend.position = "none") + xlim(-5, 12) + ylim(0, 15) + 
  
  # ------ BG
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point(aes(x=runif(200, -5, 12), y=runif(200, 0, 15), alpha=sample(1:10, 200, replace=TRUE)),
             colour = "#c6fbff", shape = 16) +
  geom_bspline(aes(x=c(0, 5, 10), y=c(0, 5, 7), alpha=rev(..index..)), size = 2, colour="gold") +

  # ----- TREE STEM
  geom_link(aes(x = -1, y = 2, xend = 1, yend = 2)) +
  geom_link(aes(x = 0, y = 2, xend = 0, yend = 0.5), colour = "#262118", size = 15, n = 200) +
  geom_link(aes(x = 0, y = 2, xend = 0, yend = 0.5, alpha = ..index..*-1), colour = "#53350A",size = 15, n = 100) +

  # ---- FILL BEZIERS LEFT
  geom_bspline(aes(x=x, y=y), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#222721") +
  geom_bspline(aes(x=x+0.25, y=y+1), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#075600") +
  geom_bspline(aes(x=x+0.5, y=y+2), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#222721") +
  geom_bspline(aes(x=x+0.75, y=y+3), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#075600") +

  # ---- FILL BEZIERS RIGHT
  geom_bspline(aes(x=x*-1, y=y), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#075600") +
  geom_bspline(aes(x=(x*-1)-0.25, y=y+1), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#222721") +
  geom_bspline(aes(x=(x*-1)-0.5, y=y+2), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#1d2f0c") +
  geom_bspline(aes(x=(x*-1)-0.75, y=y+3), data = as.data.frame(tannenbaum_fill_left_fill), colour = "#075600") +
  
  # ---- FILL W/ LINK
  geom_link(aes(x=t_x, y=t_y, xend=t_xend, yend=t_y), colour = "darkgreen", alpha = 0.7) +
  
  # ------ TREE BEZIERS
  geom_bspline(aes(x = tannenbaum_fill_left[, 1], y = tannenbaum_fill_left[, 5]), colour = "#075600") +
  geom_bspline(aes(x = tannenbaum_fill_left[, 2], y = tannenbaum_fill_left[, 6]), colour = "#075600") + 
  geom_bspline(aes(x = tannenbaum_fill_left[, 3], y = tannenbaum_fill_left[, 7]), colour = "#075600") + 
  geom_bspline(aes(x = tannenbaum_fill_left[, 4], y = tannenbaum_fill_left[, 8])) +
  geom_bspline(aes(x = tannenbaum_fill_left[, 1]*-1, y = tannenbaum_fill_left[, 5])) +
  geom_bspline(aes(x = tannenbaum_fill_left[, 2]*-1, y = tannenbaum_fill_left[, 6])) +
  geom_bspline(aes(x = tannenbaum_fill_left[, 3]*-1, y = tannenbaum_fill_left[, 7])) +
  geom_bspline(aes(x = tannenbaum_fill_left[, 4]*-1, y = tannenbaum_fill_left[, 8])) +
  
  # ---- ADDITIONAL FILL BELOW ETC
  geom_bspline(aes(x=c(-2, 0, 2), y=c(2, 1.95, 2))) +
  geom_bspline(aes(x=c(-2, 0, 2), y=c(2, 1.85, 2))) +
  geom_bspline(aes(x=c(-2, 0, 2), y=c(2, 1.75, 2))) +
  geom_bspline(aes(x=c(-2, 0, 2), y=c(2.1, 2, 2.1))) +
  
  geom_bspline(aes(x=c(-2.1, 0, 2.1), y=c(3, 2.8, 3))) +
  geom_bspline(aes(x=c(-2.1, 0, 2.1), y=c(3, 2.9, 3))) +
  geom_bspline(aes(x=c(-1.5, 0, 1.5), y=c(4, 3.8, 4))) +
  geom_bspline(aes(x=c(-1.5, 0, 1.5), y=c(4, 3.9, 4))) +
  
  geom_bspline(aes(x=c(0.35, 0.4, 0.33), y=c(2, 1, 0.5))) +

  # ---- TREE DECOS
  geom_point(aes(x=c(runif(5, -2, 2), runif(5, -1.5, 1.5), runif(5, -1, 1), runif(5, -0.45, 0.45)),
                 y=c(runif(5, 2, 3), runif(5, 3, 4), runif(5, 4, 5), runif(5, 5, 7)),
                 alpha = sample(1:6, 20, replace=TRUE)), colour="lightgoldenrodyellow", shape=16) +
  
  # ---- SNOWMAN 
  geom_circle(aes(x0 = 5.05, y0 = 1, r = 0.8), fill = "white") +
  geom_circle(aes(x0 = 5, y0 = 1, r = 0.8), fill = "white") +
  geom_circle(aes(x0 = 5.05, y0 = 2.05, r = 0.65), fill = "white") +
  geom_circle(aes(x0 = 5, y0 = 2, r = 0.65), fill = "white") +
  geom_circle(aes(x0 = 5, y0 = 2.8, r = 0.5), fill = "white") +
  geom_polygon(aes(x=c(4.7, 4.7, 5, 5.3, 5.3), y=c(3.3, 3.9, 3.9, 3.9, 3.3)), alpha = 0.6)+
  geom_bspline(aes(x=c(4.5, 5, 5.5), y=c(3.3, 3.15, 3.3)), size=1) +
  geom_bspline(aes(x=c(4.5, 5, 5.5), y=c(3.3, 3.4, 3.3))) +
  geom_bspline(aes(x=c(4.65, 4.65, 5, 5.35, 5.35), y=c(3.3, 4, 3.9, 4, 3.3)), size=1) +
  
  # -- SNOWMAN NOSE
  geom_polygon(aes(x=c(5, 5, 4.5), y=c(2.65, 2.95, 2.6)), fill = "orange") +
  geom_bspline(aes(x=c(5, 5, 4.5), y=c(2.65, 2.65, 2.6))) +
  geom_bspline(aes(x=c(4.8, 4.95, 5.1), y=c(2.6, 2.45, 2.6))) +
  
  # -- SNOWMAN EYES
  geom_point(aes(x=c(4.8, 5.1), y=c(3, 3)), fill = "grey") +
  geom_point(aes(x=c(4.8, 5.1), y=c(3, 3)), fill = "white", size = 0.2) +
  
  # -- SNOWMAN SHADOWS, POINTS
  geom_bspline(aes(x=c(5.05, 5.2, 5.3, 5.5, 5.7), y=c(0.2, 0.25, 0.3, 0.5, 0.7))) +
  geom_bspline(aes(x=c(5.05, 5.2, 5.3, 5.5, 5.7), y=c(0.3, 0.35, 0.4, 0.6, 0.8))) +
  geom_point(aes(x=c(5,5,5), y=c(1.65, 1.9, 2.15))) +
  
  # -- SNOWMAN ARMS+HANDS
  geom_link(aes(x=c(4.35, 5.65), y=c(2, 2), xend=c(3.8, 6.2), yend=c(2.2, 2.3))) +
  geom_link(aes(x=c(3.8, 6.2, 3.8, 6.2), y=c(2.2, 2.2, 2.2, 2.2), xend=c(3.85, 6.15, 3.75, 6.15), yend=c(2.3, 2.3, 2.2, 2.2))) +

  # ---- GIFT RECTS 
  geom_rect(aes(xmin=c(0.525, 0.5, -1, -1.025), xmax=c(1.025, 1, -0.25, -0.275), 
                ymin=c(0.525, 0.5, 0.5, 0.475), ymax=c(1.025, 1, 0.9, 0.875), fill=c("A", "B", "C", "D"))) +
  scale_fill_manual(values = c("#800000", "#aa0000", "#006600", "#1A3112")) +
  geom_link(aes(x=0.75, y=0.5, xend=0.75, yend=1), colour = "#FFFFFF", size=2) +
  geom_link(aes(x=(-1.025+-0.275)/2, y=0.475, xend=(-1.025+-0.275)/2, yend=0.875), colour = "#C5A436", size = 2) +
  geom_link(aes(x=-1.025, y=(0.875+0.475)/2, xend=-0.275, yend=(0.875+0.475)/2), size=2, colour="#C5A436") +

  # ---- GIFT WRAPPINGS
  geom_bspline(aes(x=c(0.75, 0.45, 0.45, 0.6, 0.75, 0.9, 1, 1, 0.75), 
                   y=c(0.95, 1.15, 1.2, 1.2, 1, 1.2, 1.2, 1.15, 0.95)), size=2, colour="goldenrod") +
  geom_bspline(aes(x=c(-0.65, -0.85, -0.85, -0.7, -0.65, -0.4, -0.35, -0.35, -0.65), 
                   y=c(0.85, 1.05, 1.1, 1.1, 0.95, 1.1, 1.1, 1.05, 0.85)), size=2, colour="goldenrod") +

  # ---- TREE STAR
  geom_link(aes(x = stern$x[1:length(stern$x)-1], 
                y = stern$y[1:length(stern$y)-1], 
                xend = stern$x[2:length(stern$x)], 
                yend = stern$y[2:length(stern$y)]),
            size = 2, colour = "brown") + 
  
  geom_link(aes(x = c(seq2(stern_fill[, 1], stern_fill[, 3], length = 50)), 
                y = c(seq2(stern_fill[, 2], stern_fill[, 4], length = 50)),
                xend = rep(stern_fill[, 5], each = 50),
                yend = rep(stern_fill[, 6], each = 50)), colour = "#edc04d", alpha = 0.45) +

  geom_link(aes(x = rep(mean(stern$x), 250), y = rep(mean(stern$y), 250),
                xend = c(seq2(stern_fill[, 1], stern_fill[, 3], length = 50)),
                yend = c(seq2(stern_fill[, 2], stern_fill[, 4], length = 50))), colour = "#edc04d", alpha = 0.45)

  
MAIN

# ---------- "VIEWPORTS" FOR SNOWFLAKES
#viewport doesn't work with ggsave. insert grob-ified subplot
viewp <- function(main, sub, xleft, xright, ybottom, ytop) {
  l1 = ggplot_build(main)
  x1 = l1$layout$panel_ranges[[1]]$x.range[1]
  x2 = l1$layout$panel_ranges[[1]]$x.range[2]
  y1 = l1$layout$panel_ranges[[1]]$y.range[1]
  y2 = l1$layout$panel_ranges[[1]]$y.range[2]
  xdif = x2-x1
  ydif = y2-y1
  xmin  = x1 + (xleft*xdif)
  xmax  = x1 + (xright*xdif)
  ymin  = y1 + (ybottom*ydif)
  ymax  = y1 + (ytop*ydif) 
  
  g2 = ggplotGrob(sub)
  
  return (main + annotation_custom(grob = g2, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax))
}

MAIN2 <- viewp(MAIN, sf3(0, 0), 0.55, 0.8, 0.75, 1)
MAIN3 <- viewp(MAIN2, sf1(0, 0), 0.1, 0.35, 0.65, 0.9)
MAIN4 <- viewp(MAIN3, sf2(0, 0), 0.7, 0.95, 0.5, 0.75)

MAIN5 <- MAIN4 + geom_point(aes(x=runif(50, -5, 12), y=runif(50, 6, 15), alpha=sample(1:10, 50, replace=TRUE)),
                             colour = "gold", shape = 8) 

MAIN5

# requires "one starry night" font
# prevent not found error
windowsFonts(OneStarryNight2=windowsFont("One Starry Night"))

MAIN6 <- MAIN5 + 
  annotate("text", x = 7, y = 6.975, label = "Merry Christmas!", family = "OneStarryNight2", size = 20, colour="gold") +
  annotate("text", x = 7.025, y = 7, label = "Merry Christmas!", family = "OneStarryNight2", size = 20)

MAIN6 <- MAIN6 + labs(x=NULL, y=NULL)

ggsave(filename = "xmas.png", plot = MAIN6, dpi=300)
