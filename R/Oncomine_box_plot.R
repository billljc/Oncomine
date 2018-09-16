#' To plot box plot for Oncomine
#'
#' @param filename the filename can be box or bar data filename or dataframe name of bar or box or omitted
#'
#' @return a box plot using plotly package
#' @export
#'
#' @examples filename=Oncomine_box("bb.txt")
#' @examples filename=Oncomine_bar("bar.txt")
#' @examples filename="bb.txt"
#' @examples filename="bar.txt"
#' @examples Oncomine_box_plot(filename)
Oncomine_box_plot <- function(filename){
  library(testit);library(plotly);library(reshape2)
  if (missing(filename)){
    BARCHECK=suppressWarnings(tryCatch({Oncomine_bar()},error=function(e){"ERROR"}))
    if (is.data.frame(BARCHECK)){
        bar1=BARCHECK
        bar2=bar1[,c("Expression value","Legend Value" )]
        plot_ly(data = bar2,
                type = 'box',
                x = ~`Legend Value`,
                y = ~`Expression value`,
                color = ~`Legend Value`
        ) %>% layout(title="Box Plot",
                     xaxis = list(title = 'Legend Value'),
                     yaxis = list(title = 'Expression value'))
      }else if(BARCHECK=="ERROR"){
        BOXCHECK=suppressWarnings(tryCatch({Oncomine_box()},error=function(e){"ERROR"}))
        if (is.data.frame(BOXCHECK)){
          boxtemp1=BOXCHECK
          boxtemp2=boxtemp1[-c(grep("10th percentile",rownames(boxtemp1)),
                               grep("90th percentile",rownames(boxtemp1))),] #delet 10th and 90th,5 rows
          boxtemp3=rbind(boxtemp2,boxtemp2["Median",]) #6 rows
          boxtemp4=rbind(boxtemp3,boxtemp2["Median",]) #7 rows
          boxtemp5=rbind(boxtemp4,boxtemp2["75th percentile",]) # 8 rows
          boxtemp6=boxtemp5[order(-boxtemp5[,1]),]
          boxtemp7=melt(boxtemp6,id.vars=NULL)
          plot_ly(data = boxtemp7,
                  type = 'box',
                  x = ~variable,
                  y = ~value,
                  color = ~variable
          ) %>% layout(title="Box Plot",
                       xaxis = list(title = 'Legend Value'),
                       yaxis = list(title = 'Expression value'))
        }else if(BOXCHECK=="ERROR"){
        files=list.files() #get all the data picture
        txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
        docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
        doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
        doccheck1=paste(doccheck,collapse="\n")
        warning("The following files shoul be removed","\n",doccheck1)
        stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
      }
    }
  } else if(is.data.frame(filename)){
      if ("Legend Value" %in% colnames(filename)){
        bar1=filename
        bar2=bar1[,c("Expression value","Legend Value" )]
        plot_ly(data = bar2,
                type = 'box',
                x = ~`Legend Value`,
                y = ~`Expression value`,
                color = ~`Legend Value`
        ) %>% layout(title="Box Plot",
                     xaxis = list(title = 'Legend Value'),
                     yaxis = list(title = 'Expression value'))
      } else if ( "Minimum" %in% rownames(filename)){
        boxtemp1=filename
        boxtemp2=boxtemp1[-c(grep("10th percentile",rownames(boxtemp1)),
                             grep("90th percentile",rownames(boxtemp1))),] #delet 10th and 90th,5 rows
        boxtemp3=rbind(boxtemp2,boxtemp2["Median",]) #6 rows
        boxtemp4=rbind(boxtemp3,boxtemp2["Median",]) #7 rows
        boxtemp5=rbind(boxtemp4,boxtemp2["75th percentile",]) # 8 rows
        boxtemp6=boxtemp5[order(-boxtemp5[,1]),]
        boxtemp7=melt(data = boxtemp6,id.vars=NULL)
        plot_ly(data = boxtemp7,
                type = 'box',
                x = ~variable,
                y = ~value,
                color = ~variable
        ) %>% layout(title="Box Plot",
                     xaxis = list(title = 'Legend Value'),
                     yaxis = list(title = 'Expression value'))
      }else {
        stop('Please Make sure your data is acquaired by function Oncomine_bar() or Oncomine_box()')
      }
  } else if (is.character(filename)) {
    BARCHECK=suppressWarnings(tryCatch({Oncomine_bar(filename)},error=function(e){"ERROR"}))
    if (is.data.frame(BARCHECK)){
      bar1=BARCHECK
      bar2=bar1[,c("Expression value","Legend Value" )]
      plot_ly(data = bar2,
              type = 'box',
              x = ~`Legend Value`,
              y = ~`Expression value`,
              color = ~`Legend Value`
      ) %>% layout(title="Box Plot",
                   xaxis = list(title = 'Legend Value'),
                   yaxis = list(title = 'Expression value'))
    }else if(BARCHECK=="ERROR"){
      BOXCHECK=suppressWarnings(tryCatch({Oncomine_box(filename)},error=function(e){"ERROR"}))
      if (is.data.frame(BOXCHECK)){
        boxtemp1=BOXCHECK
        boxtemp2=boxtemp1[-c(grep("10th percentile",rownames(boxtemp1)),
                             grep("90th percentile",rownames(boxtemp1))),] #delet 10th and 90th,5 rows
        boxtemp3=rbind(boxtemp2,boxtemp2["Median",]) #6 rows
        boxtemp4=rbind(boxtemp3,boxtemp2["Median",]) #7 rows
        boxtemp5=rbind(boxtemp4,boxtemp2["75th percentile",]) # 8 rows
        boxtemp6=boxtemp5[order(-boxtemp5[,1]),]
        boxtemp7=melt(boxtemp6,id.vars=NULL)
        plot_ly(data = boxtemp7,
                type = 'box',
                x = ~variable,
                y = ~value,
                color = ~variable
        ) %>% layout(title="Box Plot",
                     xaxis = list(title = 'Legend Value'),
                     yaxis = list(title = 'Expression value'))
      }else if(BOXCHECK=="ERROR"){
        files=list.files() #get all the data picture
        txtcheck=files[grepl(pattern = ".txt",x = files)==FALSE]
        docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
        doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
        doccheck1=paste(doccheck,collapse="\n")
        warning("The following files shoul be removed","\n",doccheck1)
        stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
      }
    }
  }
}
