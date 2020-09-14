library(imager)


conway <- function(board_size = 100 , initial = 4000, generations_size = 200){

  generations <- list()
  board <- matrix(0,nrow = board_size,ncol = board_size)
  board[sample(x = board_size*board_size, size = initial)] <- 1
  generations[[1]] <- board
  
  for(l in 2:generations_size){
    
    proximo_board <- generations[[l-1]]
    
    for(i in 1:nrow(generations[[l-1]])){
      for(k in 1:ncol(generations[[l-1]])){
        
        if(i == board_size & k != board_size){
          vizinhos <- sum(generations[[l-1]][c(i-1,i),c(k-1,k,k+1)]) - generations[[l-1]][i,k]
        } else if(k == board_size & i != board_size){
          vizinhos <- sum(generations[[l-1]][c(i-1,i,i+1),c(k-1,k)]) - generations[[l-1]][i,k]
        } else if(i == board_size & k == board_size){
          vizinhos <- sum(generations[[l-1]][c(i-1,i),c(k-1,k)]) - generations[[l-1]][i,k]
        } else{
          vizinhos <- sum(generations[[l-1]][c(i-1,i,i+1),c(k-1,k,k+1)]) - generations[[l-1]][i,k]
        }
        
        
        if( vizinhos == 3 & generations[[l-1]][i,k] == 0){
          proximo_board[i,k] <- 1
        } else if( (vizinhos < 2 | vizinhos > 3) & generations[[l-1]][i,k] == 1){
          proximo_board[i,k] <- 0
        } else {
          proximo_board[i,k] <- generations[[l-1]][i,k]
        }
        
      } 
    }
    
    generations[[l]] <- proximo_board
    cat(l, sep='\n')
  }
      
      generations <- array(data = unlist(generations),dim = c(board_size,board_size,generations_size,1))
      return(generations)
}

#board_size as 1000x1000 pixels  
imagem <- conway(generations_size = 1000, board_size = 200,initial = 15000 ) %>% as.cimg()
imagem  %>% play(loop = T, delay = 30)

#Saving the file
#f <- tempfile(fileext=".mp4",tmpdir = getwd() ) 
#save.video(imagem,f, fps = 8)
#rm(list = ls())

