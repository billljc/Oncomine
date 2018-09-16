Oncomine_meta_manyfiles_notempty <-function(filenames){
  meta_many_have=data.frame()
  for (filei in 1:length(filenames)){
    file=filenames[filei]
    meta_many_one=Oncomine_meta_onefile_notempty(file)
    meta_many_one_file=cbind(meta_many_one,file)
    meta_many_have=rbind(meta_many_have,meta_many_one_file)
  }
  return(meta_many_have)
}
