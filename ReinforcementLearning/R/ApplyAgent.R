ApplyAgent=function(Agent,Environment,Trials=100,Episodes=100){
  #V=ApplyAgent(Agent,Environment)
  # performs a benchmarking of trials per algorithms defined by Agent and newly intialized Environment
  #INPUT
  #Agent        R6 object of package reinforcelearn, 
  #             Intelligent agent is instantiation of RL 
  #             algorithm that “intelligently” interacts with the environment
  #Environment: R6 object of package reinforce learn, 
  #             Environment denotes an abstraction of the world that an intelligent 
  #             agent operates 
  #OUTPUT
  # ReturnsPerTrial[1:episodes,1:trial]     matrix of expected returns G of discounted rewards per trial
  # QstatesPerTrial[1:s,1:a]                matrix of Q values per state s and action a of last epside
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
  
  Environment$resetEverything()
  res=list()
  res=lapply(1:Trials, function(i,Environment,Agent,Episodes){
    x=suppressMessages(reinforcelearn::interact(Environment, Agent, n.episodes = Episodes,learn = T,visualize = F))
    mat=LookupTable(Environment, Agent,digits=3)
    Returns=x$returns
    Qstate=apply(mat,1,min)
    return(list(Qstate=Qstate,Returns=Returns))
  },Environment,Agent,Episodes)
  
  QstateList=lapply(res, "[[",1)
  Returns=lapply(res, "[[",2)
  QstatesPerTrial=do.call(what = rbind,QstateList)
  ReturnsPerTrial=do.call(what = rbind,Returns)
  
  return(list(ReturnsPerTrial=ReturnsPerTrial,QstatesPerTrial=QstatesPerTrial))
}