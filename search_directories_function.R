search.directories <- funtion(path, file_type, subfolders = subfolders) {
  
  
  
  
}





path = c("/mnt/Storage/Hallgrimsson/Users/Rebecca_Green/data")
setwd(path)
subfolders <- list.dirs(path = ".")[-1]
subfolders <- subfolders[grep("fgf8 het", subfolders, ignore.case = TRUE)] #Only the NN control folders

subpath <- vector(mode="character", length=(length(subfolders))) #this is supposed to be blank... i think
volume_data <- matrix(data=NA, nrow=0, ncol=4)
colnames(volume_data) <- c("specimen","filename", "folder", "full_path")

for (i in 1: length(subfolders)){
  subpath[i] <- paste0(path, sub(".", "", subfolders[i])) # we are removing the first character in the subfolder path
  all.files <- list.files(path = subpath[i])
  aim.ids <- grep(pattern = "\\.aim", x = all.files, ignore.case = T)
  aim.files <- all.files[aim.ids]
  full_path <- Sys.glob(file.path(subpath[i], aim.files)) # are they all .aim, or could they be in caps? Double-check
  setwd(subpath[i])
  #filename <- list.files(pattern="*.aim$") # the dollar sign makes sure that it is the end of the string only
  folder <- rep(substr(subfolders[i], start = 3, stop=nchar(subfolders[i])), length(aim.files)) # just the folder name
  seq <- grep(pattern = "\\.aim\\.1", x = aim.files, ignore.case = T)
  for (j in seq){
    name <- aim.files[[j]]
    name_new <- gsub('.{2}$', '', name)
    aim.files[[j]] <- name_new
  }
  specimen <- gsub('.{4}$', '', aim.files) # potential specimen name? Need to check with Wei and Jay. Here we extract 4 characters at the end
  setwd(path)
  temp <- cbind.data.frame(specimen, aim.files, folder, full_path)
  volume_data <- rbind(volume_data, temp)
}

df_all <- as.data.frame(volume_data)