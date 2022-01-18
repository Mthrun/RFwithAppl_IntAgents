PlotStatesAndQstarPath = function(StatesMatrix, Traversedstates, OptimalPolicy,Directions,
                             StateColor = "black", TextColor = "white",
                             IdxNodes = NULL, NodeColors = NULL,
                             LineColor = "black", LineWidth = 2,
                             MarkerSize = 37, TextSize = 14){
  # 
  # DESCRIPTION
  # Plot path of optimal policy.
  # 
  # INPUT
  # StatesMatrix[1:N, 1:M] Numeric matrix with enumeration of nodes which are
  #                        aranged in a matrix grid topology network.
  # Traversedstates[1:X]   Numeric vector with the order of nodes making up
  #                        a shortest path from the first to last node.
  # OptimalPolicy[1:X-1]   Numeric vector with numbers between 1 and 4 indicating
  #                        the directions to go in the matrix grid topology 
  #                        network in order to get from the first node in 
  #                        parameter SP_NodeOrder to the last one in it.
  # 
  # OPTIONAL
  # Directions             Names of Actions to plotted if missing, default value are used. if NULL: names of OptimalPolicy is used
  # StateColor             (Optional. Default: StateColor="black"). Character
  #                        indicating the default color of states.
  # TextColor              (Optional. Default: TextColor="white"). Character
  #                        indicating the text color.
  # IdxNodes[1:n]          (Optional. Default: IdxNodes=NULL). Numeric vector 
  #                        indicating Nodes for specific coloring.
  # NodeColors[1:n]        (Optional. Default: NodeColors=NULL). Character
  #                        vector with colors for each given node in IdxNodes. 
  #                        If NodeColors is NULL, then color gray will
  #                        be used as default.
  # LineColor              (Optional. Default: LineColor="black"). Character
  #                        indicating the color of lines around the node
  #                        markers.
  # LineWidth              (Optional. Default: LineWidth=2). Numeric indicating
  #                        the widht of the lines.
  # MarkerSize             (Optional. Default: MarkerSize=37). Numeric indicating
  #                        the size of the nodes in the plot.
  # TextSize               (Optional. Default: TextSize=14). Numeric indicating
  #                        the text used within the nodes.
  # 
  # OUTPUT
  # plotOut    Plotly object for direct visualization.
  # 
  # Author: QS 12.01.2021
  if(missing(Directions)){
    Directions = c("<", ">", "^", "v","< ^","< v","> ^","> v") #c("<", ">", "^", "v","lu","ld","ru","rd")
  }
  if(!is.null(Directions)){
    names(OptimalPolicy)=Directions[OptimalPolicy]
  }
  NumNodes = dim(StatesMatrix)[1] * dim(StatesMatrix)[2]
  NumRows = dim(StatesMatrix)[1]
  NumCols = dim(StatesMatrix)[2]
  # The order of direction arrows must match the direction assignment in
  # function nodeOrder2ArrowOrder
  MyScatter = rbind()
  for(i in 0:(NumNodes-1)){
    Idx = which(StatesMatrix == i, arr.ind = T)
    MyScatter = rbind(MyScatter, rev(Idx))
  }
  MyScatter = data.frame(MyScatter)
  First = Traversedstates[1]
  Last = Traversedstates[length(Traversedstates)]
  FirstAndLast = c(First, Last) + 1
  NotPartOfSP = setdiff(0:(NumNodes-1), Traversedstates) + 1
  MyText = c()
  for(i in 0:(NumNodes-1)){
    Extra = " "
    if(i %in% Traversedstates[1:(length(Traversedstates)-1)]){
        Extra = paste0(names(OptimalPolicy)[which(Traversedstates == i)])
    }
    MyText = c(MyText, paste(i, Extra, sep="\n"))
  }
  
  if(!is.null(IdxNodes)){
    IdxNodes = IdxNodes + 1 # Increment by one: IdxNodes starts from 0, but Indices starts from 1
    if(is.null(IdxNodes)){
      NodeColors = rep("gray", length(IdxNodes))
    }
  }
  TmpColors = c("blue", "green", NodeColors)
  IdxColors = c(FirstAndLast, IdxNodes)
  plotOut = plotly::plot_ly()
  plotOut = plotly::add_markers(p = plotOut,
                                x = MyScatter[,1],
                                y = MyScatter[,2],
                                marker = list(color = StateColor, size = MarkerSize, 
                                              type = "scatter", line = list(
                                  color = LineColor,
                                  width = LineWidth
                                )
                                ))
  # Colorize nodes
  for(i in 1:length(IdxColors)){
    plotOut = plotly::add_markers(p = plotOut,
                                  x = MyScatter[IdxColors[i],1],
                                  y = MyScatter[IdxColors[i],2],
                                  marker = list(color = TmpColors[i], size = MarkerSize, 
                                                type = "scatter",line = list(
                                                  color = LineColor,
                                                  width = LineWidth
                                                )))
  }
  # Add text to nodes
  plotOut = plotly::add_text(p = plotOut,
                             x = MyScatter[,1],
                             y = MyScatter[,2],
                             text = MyText, textfont = list(color = TextColor, size = TextSize))
  XNoax = list(title = "", scaleanchor = "y", range = c(0, NumCols+1), zeroline = FALSE, showline = F,
               showticklabels = F, showgrid = F)
  YNoax = list(title = "", range = c(0, NumRows+1), zeroline = FALSE, autorange = "reversed", showline = F,
               showticklabels = F, showgrid = F)
  plotOut = plotly::layout(p = plotOut, xaxis = XNoax, yaxis = YNoax)
  plotOut = plotly::hide_colorbar(p = plotOut)
  plotOut = plotly::hide_legend(p = plotOut)
  plotOut = plotly::config(p = plotOut, displayModeBar=F, editable=T)
  return(plotOut)
}