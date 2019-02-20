##' This function stores a list of raster stacks
##' @title Stores a list of raster stacks
##' @param rstacklist a list of raster stacks
##' @param pathfolder folder to which the lists rasterstacks should be exported
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @export store_rasterstacklist
##' @author Wolfgang Hamer
##' @examples
store_rasterstacklist <- function(rstacklist, pathfolder){
  dir.create(pathfolder, showWarnings = FALSE)

  tem <- Map(function(x){
    writeRaster(rstacklist[[x]],
                filename= paste0(pathfolder,"/",x,".tif"),
                options="INTERLEAVE=BAND",
                overwrite=TRUE)
    write.table(names(rstacklist[[x]]),
                paste0(pathfolder,"/",x,"_names.txt"))
  },
  x = names(rstacklist))
}




##' This function reads a list of raster stacks as stored by store_rasterstacklist
##' @title Reads a list of raster stacks
##' @param pathfolder folder to which the lists rasterstacks should be exported
##' @importFrom raster raster
##' @importFrom raster stack
##' @importFrom raster mean
##' @importFrom raster calc
##' @importFrom raster extent
##' @importFrom raster projectRaster
##' @importFrom raster writeRaster
##' @export read_rasterstacklist
##' @author Wolfgang Hamer
##' @examples
read_rasterstacklist <- function(pathfolder){
  fili <- list.files(pathfolder,
                     pattern = ".tif")

  tem <- Map(function(y){
    ret <- stack(paste0(pathfolder,"/",y))
    names(ret) <- read.table(paste0(pathfolder,"/",gsub(".tif","_names.txt",y)))$x
    return(ret)
  },
  y = fili)

  names(tem) <- gsub(".tif","",fili)

  return(tem)
}
