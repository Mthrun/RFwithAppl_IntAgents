takeGridAction=function(StatesMatrix,CurrentState,Action){
  indStateInMat=which(StatesMatrix==CurrentState,arr.ind = T)
  
  if(Action==1) #"left"
    Outstate=StatesMatrix[indStateInMat[1],indStateInMat[2]-1]
  if(Action==2) #"right"  
    Outstate=StatesMatrix[indStateInMat[1],indStateInMat[2]+1]
  if(Action==3)# "up" 
    Outstate=StatesMatrix[indStateInMat[1]-1,indStateInMat[2]]
  if(Action==4) #"down" 
    Outstate=StatesMatrix[indStateInMat[1]+1,indStateInMat[2]]
  if(Action==5) #"leftup"
    Outstate=StatesMatrix[indStateInMat[1]-1,indStateInMat[2]-1]
  if(Action==6) # "leftdown"
    Outstate=StatesMatrix[indStateInMat[1]+1,indStateInMat[2]-1]
  if(Action==7) #"rightup"
    Outstate=StatesMatrix[indStateInMat[1]-1,indStateInMat[2]+1]
  if(Action==8) #"rightdown"
    Outstate=StatesMatrix[indStateInMat[1]+1,indStateInMat[2]+1]
  
  return(Outstate)
}