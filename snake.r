library(tcltk)

window <- tktoplevel(height = 500, width = 500, bg = 'lightgrey')
tktitle(window) <- 'SnakeR'

score <- tclVar('Score:  ')
score_label <- tklabel(window, text = tclvalue(score))
tkconfigure(score_label, textvariable=  score)
tkplace(score_label, x = 0, y = 0)

tkbind(window, '<Up>', function() {assign('direction', 'UP', envir = .GlobalEnv)})
tkbind(window, '<Down>', function() {assign('direction', 'DOWN', envir = .GlobalEnv)})
tkbind(window, '<Left>', function() {assign('direction', 'LEFT', envir = .GlobalEnv)})
tkbind(window, '<Right>', function() {assign('direction', 'RIGHT', envir = .GlobalEnv)})

init <- function(){
  direction <- ''

  .GlobalEnv$bodies <- list()

  .GlobalEnv$points <- list()

  .GlobalEnv$direction <- ''
  
  .GlobalEnv$points.spawned <- 0

  .GlobalEnv$points.caught <- 0

  .GlobalEnv$head <- tkframe(window, width = 9, height = 9, bg = 'red', relief = 'solid', borderwidth = 1)

  .GlobalEnv$head.last.x <- tkwinfo('x', head)
  
  .GlobalEnv$head.last.y <- tkwinfo('y', head)

  tkplace(head, x = 250, y = 250)
}

game <- function(){
  while (TRUE){
    
    move.head()
    
    if (length(bodies) >= 1) {
      move.bodies()
    }
    
    if (length(bodies) >= 2) {
      if(self.collision.check()) {
        stop('Game over')
      }
    }
    
    .GlobalEnv$head.last.x <- tkwinfo('x', head)
    .GlobalEnv$head.last.y <- tkwinfo('y', head)
    
    border.collision.check()
    
    x <- as.integer(tkwinfo('x', head))
    y <- as.integer(tkwinfo('y', head))

    if (points.spawned > 0) {
      points.collision.check(x, y)
    }
    
    points.spawn.check()
    
    Sys.sleep(0.04)
  }
}

move.head <- function() {
  if (direction == 'UP') {
      tkplace(head, x = as.integer(tkwinfo('x', head)), 
              y = as.integer(tkwinfo('y', head)) - 8)
    }
  
  if (direction == 'DOWN') {
      tkplace(head, x = as.integer(tkwinfo('x', head)),
              y = as.integer(tkwinfo('y', head)) + 8)
    }
  
  if (direction == 'LEFT') {
      tkplace(head, x = as.integer(tkwinfo('x', head)) - 8,
              y = as.integer(tkwinfo('y', head)))
    }
  
  if (direction == 'RIGHT') {
      tkplace(head, x = as.integer(tkwinfo('x', head)) + 8,
              y = as.integer(tkwinfo('y', head)))
  }
}

move.bodies <- function(){
  tkplace(bodies[[1]], x = head.last.x, y = head.last.y)
  
  if (length(bodies) > 1){
    for (i in 2:length(bodies)) {
      tkplace(bodies[[i]], x = as.integer(tkwinfo('x', bodies[[i-1]])),
              y = as.integer(tkwinfo('y', bodies[[i-1]])))
    }
  }
}

border.collision.check <- function(){
  x <- as.integer(tkwinfo('x', head))
  y <- as.integer(tkwinfo('y', head))

  if (x < 0) {
    tkplace(head, x = 499, y = y)
  }
  else if (x > 500) {
    tkplace(head, x = 1, y = y)
  }
  else if (y < 0) {
    tkplace(head, x = x, y = 499)
  }
  else if (y > 500) {
    tkplace(head, x = x, y = 1)
  }
}

points.spawn.check <- function(){
  if (points.spawned <= 2) {
    if (sample(2,1) == 1) {
      create.spawn()
      assign('points.spawned', points.spawned + 1, envir = .GlobalEnv)
    }
  }
}

points.collision.check <- function(x, y){
  
    for (i in 1:points.spawned) {
      
      if ( (max(x + 8, as.integer(tkwinfo('x', points[[i]])) + 8) - min(x, as.integer(tkwinfo('x', points[[i]]))) <
            ((x + 8 - x) + (as.integer(tkwinfo('x', points[[i]])) + 8 - as.integer(tkwinfo('x', points[[i]]))))) &
           (max(y + 8, as.integer(tkwinfo('y', points[[i]])) + 8) - min(y, as.integer(tkwinfo('y', points[[i]]))) <
            ((y + 8 - y) + (as.integer(tkwinfo('y', points[[i]])) + 8 - as.integer(tkwinfo('y', points[[i]]))))) ) {
        
        tkplace.forget(points[[i]])
        .GlobalEnv$points[[i]] <- NULL
        .GlobalEnv$points.caught <- .GlobalEnv$points.caught + 1
        .GlobalEnv$points.spawned <- .GlobalEnv$points.spawned - 1
        
        create.body()
        
        score_update()
        
        return(NULL)
      }
    }
}

self.collision.check <- function(){
  
  head.x <- as.integer(tkwinfo('x', head))
  head.y <- as.integer(tkwinfo('y', head))
  
  for (i in 2:length(bodies)) {
    if ( (max(head.x + 8, as.integer(tkwinfo('x', bodies[[i]])) + 8) - min(head.x, as.integer(tkwinfo('x', bodies[[i]]))) <
          ((head.x + 8 - head.x) + (as.integer(tkwinfo('x', bodies[[i]])) + 8 - as.integer(tkwinfo('x', bodies[[i]]))))) &
         (max(head.y + 8, as.integer(tkwinfo('y', bodies[[i]])) + 8) - min(head.y, as.integer(tkwinfo('y', bodies[[i]]))) <
          ((head.y + 8 - head.y) + (as.integer(tkwinfo('y', bodies[[i]])) + 8 - as.integer(tkwinfo('y', bodies[[i]]))))) ) {
      game_over()
      return(TRUE)
      }
      else{return(FALSE)}
    }
}

create.body <- function(){
  
  frame <- tkframe(window, width = 8, height = 8, bg = 'black')
  
  if (length(bodies) == 0) {
    x <- as.integer(tkwinfo('x', head))
    y <- as.integer(tkwinfo('y', head))
  } else {
    x <- as.integer(tkwinfo('x', bodies[[length(bodies)]]))
    y <- as.integer(tkwinfo('y', bodies[[length(bodies)]]))
  }
  
  if (direction == 'UP') {
    tkplace(frame, x = x, y = y + 8)
  }
    
  else if (direction == 'DOWN') {
    tkplace(frame, x = x, y = y - 8)
  }
  
  else if (direction == 'LEFT') {
    tkplace(frame, x = x + 8, y = y)
  }
  
  else if (direction == 'RIGHT') {
    tkplace(frame, x = x - 8, y = y)
  }
  .GlobalEnv$bodies[[length(.GlobalEnv$bodies) + 1]] <- frame
}

create.spawn <- function(){
  .GlobalEnv$points[[length(points) + 1]] <- tkframe(window, width = 8, height = 8, relief = 'solid', bg = 'green', borderwidth = 1)
  tkplace(.GlobalEnv$points[[length(.GlobalEnv$points)]], x = sample(450, 1), y = sample(450, 1))
}

score_update <- function() {
  tclvalue(score) <- paste0('Score: ', toString(points.caught), sep = '')
}

game_over <- function(){
    tclvalue(score) <- 'GAME OVER'
}

game.clear <- function(){
  for (i in 1:length(bodies)){
    tkplace.forget(bodies[[i]])
  }
  
  for (i in 1:length(points)){
    tkplace.forget(points[[i]])
  }
  
  tkplace.forget(head)
  
  tclvalue(score) <- paste0('Score: ','0' , sep = '')
}

#to start game run:
init()
game()

#for new game run:
game.clear()
init()
game()
#without closing window
