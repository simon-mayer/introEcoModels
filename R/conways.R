# Funktion um Nachbarn aufzuaddieren
neighbours = function(M){
  if (!is.matrix(M)) return(NULL);
  m = dim(M)[1];
  n = dim(M)[2];
  left = cbind( rep(0,m), M[ ,1:(n-1)]);
  right = cbind(M[ ,2:n], rep(0,m));
  up = rbind( rep(0,n), M[1:(m-1),]);
  down = rbind( M[2:m,], rep(0,n));
  upper.right = M[1:(m-1),2:n];
  upper.right = rbind(rep(0,n-1),upper.right);
  upper.right = cbind(upper.right,rep(0,m));
  lower.right = M[2:m,2:n];
  lower.right = rbind(lower.right, rep(0,n-1));
  lower.right = cbind(lower.right, rep(0,m));
  lower.left = M[2:m,1:(n-1)];
  lower.left = cbind(rep(0,n-1),lower.left);
  lower.left = rbind(lower.left, rep(0,m));
  upper.left = M[1:(m-1),1:(n-1)];
  upper.left = rbind(rep(0,n-1),upper.left);
  upper.left = cbind(rep(0,m),upper.left);

  N =up+down+right+left+upper.right+lower.right+lower.left+upper.left;
  return(N);
}

# Regeln für Conway's Game of Life
N2 = function(M){
  m = dim(M)[1];
  n = dim(M)[2];
  N2 = (neighbours(M)==2);
  N2 = as.numeric(N2);
  dim(N2) = c(m,n);
  return(N2);
}

N3 = function(M){
  m = dim(M)[1];
  n = dim(M)[2];
  N3 = (neighbours(M)==3);
  N3 = as.numeric(N3);
  dim(N3) = c(m,n);
  return(N3);
}

update = function(M){
  return( N3(M) + N2(M)*M);
}


# zufällige Startmatrix
random.setup = function(n){
  M = matrix(sample(c(0,1),n*n,replace=TRUE),n,n);
  return(M);
}



conwayUI <- function(id){
  tabPanel("Conway's Game of Life",
           fluidRow(
             column(6,
                    actionButton(shiny::NS(id, "shuffle"), "Shuffle"))),
           fluidRow(
             column(12, plotOutput(NS(id, "conway")))))
}



conwayServer <- function(id){
  moduleServer(id, function(input, output, session){
    newGame <- eventReactive(input$shuffle, {M = random.setup(8)})

    output$conway <- renderPlot({
      M <- newGame()
      par(mfrow=c(5,5),mar=c(1,1,2,2));
      for (i in 1:25){
        M=update(M);
        image(M,col=c(rgb(1,1,1),rgb(0,0.5,0)),axes=FALSE,frame=TRUE);mtext(side=3,paste(quote(iteration),i-1));
        }
      })
  })
}

