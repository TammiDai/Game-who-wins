game=matrix(c(2,2,1,1,1,2,1,2,1),nrow=3,ncol=3,byrow=T)
r=list()

line=function(game){
  for (i in 1:ncol(game)){
    r[[i]]=unique(game[i,])
    if (length(r[[i]])==1 & game[1,1]!=0){
      return( game[1,1])
    }else{
      return(0)
    }
  }
}

column=function(game){
  for (i in 1:ncol(game)){
    r[[i]]=unique(t(game)[i,])
    if(length(r[[i]])==1 & t(game)[1,1]!=0){
      return(t(game)[1,1])
    }else{
      return(0)
    }
  }
}

diagonal=function(game){
  if(game[2,2]!=0){
    if (length(unique(diag(game)))==1){
      return(game[2,2])
    }else if(game[3,1]==game[2,2]&game[2,2]==game[1,3]){
      return(game[2,2])
    }else{
      return(0)
    }
  }
}

if (line(game)>0) {
  cat(line(game)," row Wins!")
}
if (column(game)>0) {
  cat(column(game)," column Wins!")
}
if(diagonal(game)>0){
  cat(diagonal(game)," diagonal Wins!")
}