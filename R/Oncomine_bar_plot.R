#' To plot bar chat
#'
#' @param filename the filename can be bar data filename or dataframe name of bar or box or omitted
#'
#' @return a bar chart plot by plotly
#' @export
#'
#' @examples Oncomine_bar_plot()
#' @examples Oncomine_bar_plot("bat.txt")
#' @examples Oncomine_bar_plot(bar_dataframe)
Oncomine_bar_plot<-function(filename){
  library(plotly)
  if (missing(filename)){
    bartemp=Oncomine_bar()
  }else if (is.data.frame(filename)){
    bartemp=filename
  }else if (is.character(filename)){
    bartemp=Oncomine_bar(filename)
  }
  bartemp$`Legend Value`=factor(bartemp$`Legend Value`,levels = unique(bartemp$`Legend Value`))
  bartemp$`Sample Name` <- factor(bartemp$`Sample Name`,
                                  levels = unique(bartemp$`Sample Name`)
                                  [order(bartemp$`Legend Value`,bartemp$`Expression value`)])
  plot_ly(bartemp,
          x = ~`Sample Name`,
          y = ~`Expression value`,
          type = "bar",
          color = ~`Legend Value`,
          text = paste("Expression: ", bartemp$`Expression value`)) %>%
    layout(title="<b>Bar plot</b>")
}
