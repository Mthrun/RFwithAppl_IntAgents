FindStatesPathOfOptimalPolicy=function(Qtable,GridRows,GridCols,Start,Goal){
  # FindStatesPathOfOptimalPolicy(Qtable,9,9,0,40)
  # Finds the optimal actions to be taken based on the qbalues stored in the table
  # for the case of a grid envirnment either with four actions or eight actions
  # (diagonal moves).
  # 
  # INPUT
  # Qtable                Numeric matrix where column entries denote edges
  #                       for all four cardinal directions for a set of nodes 
  #                       written rowwise. The nodes are aranged in a grid 
  #                       matrix (NxD) to create a network graph without
  #                       negative cycles!
  # GridRows              row number of grid environment
  # GridCols              col number of grid environment
  # Start                 intial state of agent
  # Goal                  terminal state of agent
  # 
  # OUTPUT
  # Traversedstates       States traversed using optimal (max Q values) behavior form start to goal
  # OptimalPolicy         action taken during the path of Traversedstates
  # PossibleActions       possible actionms defined in Qtable
  # StatesMatrix          states matrix based on states found in Qtable as well as GridRows,GridCols
  # QstarPerState         Optimal action per state
  # 
  # Author MT, 01/2022  
  #Example
  # Qtable=matrix(-10,4,4)
  # #grid env,4actions
  # colnames(Qtable)=c("left","right","up","down")
  # rownames(Qtable)=c("State0","State1","State2","State3")
  # #set wall, no pacman
  # Qtable[1,2]=-9999
  # #set optimal policy
  # Qtable[1,4]=-1
  # Qtable[3,2]=-1
  # Qtable[4,3]=-1
  # 
  # V=FindStatesPathOfOptimalPolicy(Qtable = Qtable,GridRows = 2,GridCols = 2,Start = 0,Goal = 1)
  # 
  # 
  ActionNames=colnames(Qtable)
  MaxAction=apply(Qtable, 1, which.max)
  Actions=1:max(MaxAction)
  names(Actions)=ActionNames
  StatesMatrix=matrix(0:(length(MaxAction)-1),nrow =GridRows ,ncol = GridCols,byrow = T)
  
  Current=Start
  Path=c(Current)
  #traverse path
  while (Current!=Goal) {
    Action=MaxAction[Current+1]
    NextState=takeGridAction(StatesMatrix,Current,Action)
    Path=c(Path,NextState)
    Current=NextState
    if(length(Path)>length(StatesMatrix)){
      warning("FindStatesPathOfOptimalPolicy: path is longer than number of states, therefore function stops.")
      break; 
    }

    if(length(NextState)==0){
      warning("FindStatesPathOfOptimalPolicy: path cannot be found, maybe pacman universum")
      break; #path cannot be found, maybe pacman universum?
    }
     
  }
  #SteNames of Optimal Policy
  names(MaxAction)=names(Actions)[MaxAction]
  #Get Optimal Policy from start to terminal state
  OptimalPolicy=MaxAction[head(Path,length(Path)-1)+1]
  #map optimal actions of all states to states
  QstarPerState=MaxAction
  QstarPerState=cbind(names(MaxAction),MaxAction)
  if(!is.null(rownames(Qtable)))
    QstarPerState=cbind(QstarPerState,rownames(Qtable))
  else
    QstarPerState=cbind(QstarPerState,0:(nrow(QstarPerState)-1))
  
  rownames(QstarPerState)=NULL
  colnames(QstarPerState)=c("ActionName","ActionNo","StateNo")
  
  return(list(Traversedstates=Path,OptimalPolicy=OptimalPolicy,
              PossibleActions=Actions,StatesMatrix=StatesMatrix,
              QstarPerState=QstarPerState))
}
