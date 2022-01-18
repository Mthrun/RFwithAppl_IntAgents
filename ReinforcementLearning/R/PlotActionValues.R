PlotActionValues=function(Environment, Agent,NumberOfStatesPerRow=9,MarkGoal,main="",Normalize){
  #V=PlotActionValues(Environment, Agent)
  # plots q table as pixelmatrix, expects 8 possible actions in the grid (i.e., diagonal movements)
  #INPUT
  #Environment:         R6 object of package reinforce learn, 
  #                     Environment denotes an abstraction of the world that an intelligent 
  #                     agent operates, here and gridworld environment is expected
  #Agent                R6 object of package reinforcelearn, 
  #                     Intelligent agent is instantiation of RL 
  #                     algorithm that “intelligently” interacts with the environment
  #NumberOfStatesPerRow Number of states per row of grid
  #OPTIONAl
  #MarkGoal     #integer, marks state with NaN
  #main         # title of pixel matrix
  #Normalize    # sets all values above quantile(Q,Normalize) to zero
  #OUTPUT
  # Plot of Pixelmatrix

  #author: MCT, 01/2022  
  if (!requireNamespace('DataVisualizations',quietly = TRUE)) {
    message(
      'Subordinate package (DataVisualizations) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (DataVisualizations) is missing.
                Please install the package which is defined in 'Suggests'."
    )
  }
  
  mat=LookupTable(Environment, Agent,digits=3)
  if(!missing(Normalize)){
    mat[mat>quantile(mat,Normalize)]=0
  }
  
  mat_9=mat[,c(5,3,7,1,1,2,6,4,8)]
  colnames(mat_9)[5]="middle"
  
  if(!missing(MarkGoal)){
    mat_9[MarkGoal,]=NaN
  }
  mat_9[,5]=NaN
  
  neunerAllrow=c()
  neunerAll=rep(NaN,3*NumberOfStatesPerRow)
  for(i in 1:nrow(mat)){
    neuner=matrix(mat_9[i,],3,3,byrow = T)
    if(i%%NumberOfStatesPerRow==1){
      neunerAllrow=neuner
    }else if(i%%NumberOfStatesPerRow==0){
      neunerAllrow=cbind(neunerAllrow,neuner)
      neunerAll=rbind(neunerAll,neunerAllrow)
      print(i)
    }else{
      neunerAllrow=cbind(neunerAllrow,neuner)
    } 
  }
  neunerAll=neunerAll[-1,]
  DataVisualizations::Pixelmatrix(neunerAll,main = main,XNames = NULL,YNames = NULL)
  
}