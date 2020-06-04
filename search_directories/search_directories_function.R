search.directories <- funtion(dir = NULL, file_type, folders = NULL) {
  
  # Set up directory
  if (is.null(dir) == TRUE){
    path <- getwd() # current directory
    setwd(path)
  }
  else {
    path <- dir # we indicate which directory
    setwd(path)
  }
  
  # set up which file type you are looking for
  file <- paste(".",file_type, sep = "")
  
  # Set up subfolder
  if (is.null(subfolders) == TRUE){   #option a (NULL), default: look at all the subfolder within that directory
    subfolders <- list.dirs(path = ".")[-1]
  }
  else {  # option b: give a vector of subfolders' relative path
    all_folders <- list.dirs(path = ".")[-1]
    subfolders <- all_folders[grep(pattern = folders, x = all_folders, ignore.case = TRUE)] #If folders are specified, search through all subfolders to find matches... maybe there's a better way to do this?
  }
  
  #Set up the paths that the loop will use to search for files  
  subpath <- vector(mode="character", length=(length(subfolders))) 
  volume_data <- matrix(data=NA, nrow=0, ncol=4) #Initialize the matrix 
  colnames(volume_data) <- c("specimen","filename", "folder", paste(file_type, "path", sep = "_"))
  
  #Loop through directories and create dataframe from filenames 
  for (i in 1: length(subfolders)){
    subpath[i] <- paste0(path, sub(".", "", subfolders[i])) # we are removing the first character in the subfolder path
    all.files <- list.files(path = subpath[i])
    search.ids <- grep(pattern = paste0("\\", file), x = all.files, ignore.case = T)
    search.files <- all.files[search.ids]
    full_path <- Sys.glob(file.path(subpath[i], search.files)) 
    setwd(subpath[i])
    folder <- rep(substr(subfolders[i], start = 3, stop=nchar(subfolders[i])), length(search.files))# just the folder name
    gsub_id <- paste('.{', nchar(as.character(file)), '}$', sep = '')
    specimen <- gsub(pattern = gsub_id, '', search.files) #Get the specimen name, which is the file name minus the file extension
    setwd(path)
    temp <- cbind.data.frame(specimen, search.files, folder, full_path)
    volume_data <- rbind(volume_data, temp)
  }
  
  
  df_all <- as.data.frame(volume_data)

  return(df_all)
}


#Down here is the original code 

# # subpath <- vector(mode="character", length=(length(subfolders))) #this is supposed to be blank... i think
# # volume_data <- matrix(data=NA, nrow=0, ncol=4)
# # colnames(volume_data) <- c("specimen", "filename", "folder", "full_path")
# 
# for (i in 1: length(subfolders)){
#   subpath[i] <- paste0(path, sub(".", "", subfolders[i])) # we are removing the first character in the subfolder path
#   all.files <- list.files(path = subpath[i])
#   aim.ids <- grep(pattern = paste0("\\", file_type), x = all.files, ignore.case = T)
#   aim.files <- all.files[aim.ids]
#   full_path <- Sys.glob(file.path(subpath[i], aim.files)) # are they all .aim, or could they be in caps? Double-check
#   setwd(subpath[i])
#   #filename <- list.files(pattern="*.aim$") # the dollar sign makes sure that it is the end of the string only
#   folder <- rep(substr(subfolders[i], start = 3, stop=nchar(subfolders[i])), length(aim.files)) # just the folder name
#   # seq <- grep(pattern = paste0("\\", file_type, "\\.1"), x = aim.files, ignore.case = T)
#   # for (j in seq){
#   #   name <- aim.files[[j]]
#   #   name_new <- gsub('.{2}$', '', name)
#   #   aim.files[[j]] <- name_new
#   # }
#   specimen <- gsub('.{4}$', '', aim.files) # potential specimen name? Need to check with Wei and Jay. Here we extract 4 characters at the end
#   setwd(path)
#   temp <- cbind.data.frame(specimen, aim.files, folder, full_path)
#   volume_data <- rbind(volume_data, temp)
# }
# 
# df_all <- as.data.frame(volume_data)