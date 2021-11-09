#create CDMInstances_preselect if pre_select=="Yes"

if(pre_select=="Yes"){
if ("CDMInstances_preselect" %in% list.files(paste0(dir_base,"/CDMInstances/",StudyName,"/"))){
  unlink(paste0(dir_base,"/CDMInstances/",StudyName,"/","CDMInstances_preselect/"), recursive = T)#delete folder
  dir.create(paste0(dir_base,"/CDMInstances/",StudyName,"/","CDMInstances_preselect/"))
} else {
  dir.create(paste0(dir_base,"/CDMInstances/",StudyName,"/","CDMInstances_preselect/"))
}
}
