#' To dowanload Oncomine heatmap data DiseaseSummaryfor_SomeGene
#'
#' @param filename string filename
#'
#' @return A dataframe
#' @export
#'
#' @examples Oncomine_heatmap_DiseaseSummaryforGene("txt.txt")
Oncomine_heatmap_DiseaseSummaryfor_SomeGene<-function(filename){
  library("textreadr")
  library("stringr")
  if (missing(filename)){
    files=list.files()
    if (length(files)>1){
      stop("besides data file, your working directory contains more than one files")
    }else{
      filename=files
    }
  }
  #read the long string
  if (grepl(".txt",filename)) { oncomine=suppressWarnings(paste(readLines(filename), collapse=" "))
  }else if(grepl(".doc",filename) | grepl(".docx",filename)) {
    oncomine=read_docx(filename)}
  a=oncomine
  b=str_extract(string =a,pattern = '<area.*\">')#get dot data
  c=strsplit(x = b,split = '<area')[[1]][-1]
  #headercontent to get color data
  d=grepl(pattern = 'headercontent=',x = c)
  e=c[d==TRUE]
  #delet gray data
  f=grepl(pattern = 'om4:leftcontent=\"\" om4:rightcontent',x = e)
  g=e[f==FALSE]
  h=gsub(pattern = '"',replacement = "",x = g )#successful to delet gray data
  expression=ifelse (grepl(';under',h),"under",'over')
  #get left content
  il = str_extract(h,'om4:leftcontent.*om4:rightcontent')
  jl=gsub(pattern = 'om4:leftcontent=',replacement = '',x = il)
  kl=gsub(pattern = 'om4:rightcontent',replacement = '',x =jl)
  ll=strsplit(kl,split = '\\|\\|')
  #get right content
  ir = str_extract(h,'om4:rightcontent.*om4:headercontent')
  jr=gsub(pattern = 'om4:headercontent',replacement = '',x = ir)
  kr=gsub(pattern = 'om4:rightcontent=',replacement = '',x =jr)
  lr=strsplit(kr,split = '\\|\\|')
  #get header content
  ih = str_extract(h,'om4:headercontent.*onclick')
  jh=gsub(pattern = 'om4:headercontent=',replacement = '',x = ih)
  meat.threshold=str_extract(string = jh,pattern = '\\d*')
  meatintotal=sub(pattern = 'out of ',replacement = '',x =  str_extract(pattern = 'out\\sof\\s\\d*',string = jh))
  colnameS=c(sub(pattern = ':',replacement = '', unique(ll)[[1]][1:length(unique(ll)[[1]])]),"meat threshold",'meat intotal','expression')
  rightcontent=t(as.data.frame(lr,col.names = 1:length(lr)))
  head1=as.data.frame(meat.threshold)
  head2=as.data.frame(meatintotal)
  withheaderdata=cbind(rightcontent,head1,head2,expression)
  colnames(withheaderdata)=colnameS
  return(withheaderdata)
}
