#'LIU Theme
#'@importFrom ggplot2 theme theme_bw element_rect element_line element_blank element_text margin
#'
#'return a theme for ggplot2 outputs
#'@export


theme_linko<-function(){
  ggplot2::theme_bw()+
    ggplot2::theme(panel.grid.major = element_line(color="#54d8e0",linetype = 1),
                   panel.grid.minor = element_rect(color="#54d8e0",linetype = 5)
                   panel.background = element_rect(fill ="#54d8e0"),
                   plot.background = gelement_rect(),
                   panel.border = ggplot2::element_blank(),
                   text=ggplot2::element_text(color="#54d8e0"),
                   axis.text = ggplot2::element_text(color="#54d8e0"),
                   axis.ticks = ggplot2::element_line(color="#54d8e0"),
                   axis.line.x.bottom = element_line(color="#54d8e0",grid::arrow=(length =grid::unit(0.1) )
                    axis.line.y.left = element_line(color="#54d8e0")
                     )
  
}