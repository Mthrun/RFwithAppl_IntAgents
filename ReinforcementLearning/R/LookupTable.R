LookupTable=function(Environment, Agent,digits=2){
#Q=LookupTable(Environment, Agent)
# gets the lookup table given environment and Agent
#INPUT
#  Environment: R6 object of package reinforce learn, 
#             Environment denotes an abstraction of the world that an intelligent 
#             agent operates 
#   Agent        R6 object of package reinforcelearn, 
#             Intelligent agent is instantiation of RL 
#             algorithm that “intelligently” interacts with the environment
#OUTPUT
# Matrix[1:s,1:a] of Q values per state s and action a
#author: MCT, 01/2022
  
  if (!requireNamespace('reinforcelearn',quietly = TRUE)) {
    message(
      'Subordinate package (reinforcelearn) is missing. No computations are performed.
            Please install the package which is defined in "Suggests".'
    )
    return(
      "Subordinate package (reinforcelearn) is missing.
                Please install the package which is defined in 'Suggests'."
    )
  }
  Qraw=reinforcelearn::getValueFunction(Agent)
  optionsraw=Environment$action.names
  if(!is.null(names(optionsraw)))
    colnames(Qraw)=names(optionsraw)
  else
    colnames(Qraw)=optionsraw
  
  Qtable=round(Qraw,digits)
  
  rownames(Qtable)=paste0("State",0:(nrow(Qtable)-1))
  return(Qtable)
}