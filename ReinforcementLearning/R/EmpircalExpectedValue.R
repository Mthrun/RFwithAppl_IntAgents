EmpiricalExpectedValue=function(Variable){
  #EW=EmpiricalExpectedValue(Variable)$EW
  #estimates the expected value based on empirical pdf 
  #INPUT:
  # Variable[1:n]: Zufallsvariable X mit n eintraegen
  #OUTPUT
  #list V of
  # EW            #scalar product of Kernels and Probs
  # Kernels[1:m]  # m unique values of given Variable
  # Probs[1:m]     probability of being in each value
  
  #author MCT, 02/2022
  
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
  V=DataVisualizations::ParetoDensityEstimation(Variable,PlotIt = F,MinAnzKernels = 1000)
  Kernels=V$kernels
  Dens=V$paretoDensity
  MinD=  min(Kernels)
  Kernels=Kernels
  ind=which(!duplicated(Kernels))
  Kernels=Kernels[ind]
  Dens=Dens[ind]
  Dens=Dens/sum(Dens)
  EW=Kernels%*%Dens

  return(list(EW=EW,x=Kernels,Probs=Dens))
}