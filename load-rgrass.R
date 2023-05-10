.GRASS_CACHE <- new.env(FALSE, parent=globalenv())

if(!exists("Sys.setenv", envir = baseenv())) Sys.setenv <- Sys.putenv

.onLoad <- function(lib, pkg) {
  assign(".GRASS_old.GRASS_PAGER", Sys.getenv("GRASS_PAGER"), envir=.GRASS_CACHE)
  Sys.setenv("GRASS_PAGER"="cat")
  assign(".GRASS_old.GRASS_MESSAGE_FORMAT", Sys.getenv("GRASS_MESSAGE_FORMAT"),
         envir=.GRASS_CACHE)
  assign("INIT_USED", FALSE, envir=.GRASS_CACHE)
  assign("remove_GISRC", FALSE, envir=.GRASS_CACHE)
  
  Sys.setenv("GRASS_MESSAGE_FORMAT"="text")
  
  gisrc <- Sys.getenv("GISRC")
  loc <- Sys.getenv("LOCATION_NAME")
  
  assign("cmdCACHE", list(), envir=.GRASS_CACHE)
  assign("override_encoding", "", envir=.GRASS_CACHE)
  SYS <- ""
  if (.Platform$OS.type == "windows") SYS <- "WinNat"
  else if (.Platform$OS.type == "unix") SYS <- "unix"
  else SYS <- "unknown"
  assign("SYS", SYS, envir=.GRASS_CACHE)
  res <- ""
  if (SYS == "WinNat") res =".exe"
  assign("addEXE", res, envir=.GRASS_CACHE)
  assign("WN_bat", "", envir=.GRASS_CACHE)
  
  assign("ignore.stderr", FALSE, envir=.GRASS_CACHE)
  assign("useGDAL", TRUE, envir=.GRASS_CACHE)
  assign("stop_on_no_flags_paras", TRUE, envir=.GRASS_CACHE)
  assign("plugin", NULL, envir=.GRASS_CACHE)
  assign("echoCmd", FALSE, envir=.GRASS_CACHE)
  assign("GV", "", envir=.GRASS_CACHE)
  assign("useIntern", FALSE, envir=.GRASS_CACHE)
  assign("legacyExec", .Platform$OS.type == "windows", envir=.GRASS_CACHE)
  assign("defaultFlags", NULL, envir=.GRASS_CACHE)
  assign("suppressEchoCmdInFunc", TRUE, envir=.GRASS_CACHE)
  assign("R_interface", NULL, envir=.GRASS_CACHE)
}

.onLoad()

.onAttach <- function(lib, pkg) {
  gisrc <- Sys.getenv("GISRC")
  loc <- Sys.getenv("LOCATION_NAME")
  if (nchar(gisrc) == 0) gv <- "(GRASS not running)"
  else {
    gv <- .grassVersion()
    comp <- .compatibleGRASSVersion(gv)
    if (!is.na(comp) && !comp ){
      stop( attr(comp, "message") )
    }
    assign("GV", gv, envir=.GRASS_CACHE)
    if(nchar(loc) == 0) {
      loc <- read.dcf(gisrc)[1,"LOCATION_NAME"]
    }
    
  }
  
  Smess <- paste('Package deprecated; switch to package rgrass. This package will\n', 'be retired before rgdal is retired during 2023\n', 'GRASS GIS interface loaded ',
                 'with GRASS version: ', gv, '\n',
                 ifelse(nchar(loc) == 0, '', paste('and location: ', loc, '\n', sep="")),
                 sep="")
  packageStartupMessage(Smess, appendLF = FALSE)
}

.onAttach()

.grassVersion <- function(ignore.stderr=TRUE) {
  Gver <- try(execGRASS(
    "g.version",
    intern = TRUE, 
    ignore.stderr = ignore.stderr), silent=TRUE)
  if (inherits(Gver, "try-error"))
    Gver <- "sh: line 1: g.version: command not found"
  return(Gver)
}

.compatibleGRASSVersion <- function(gv=.grassVersion()) {
  compatible <- ( (gv >= "GRASS 7.0") & (gv < "GRASS 9.0") )
  if (gv == "sh: line 1: g.version: command not found") {
    compatible <- as.logical(NA)
    attr(compatible, "message") <- gv
    return(compatible)
  }
  if ( !compatible ){
    attr(compatible, "message") <- paste0(
      "\n### rgrass7 is not compatible with the GRASS GIS version '", gv, "'!",
      "\n### Please use the package appropriate to the GRASS GIS version:",
      "\n### GRASS GIS Version 5.x.y  --  GRASS",
      "\n### GRASS GIS Version 6.x.y  --  spgrass6",
      "\n### GRASS GIS Version 7.x.y  --  rgrass7",
      "\n### GRASS GIS Version 8.x.y  --  rgrass"
    )
  } else {
    attr(compatible, "message") <- paste0("rgrass7 is compatible with the GRASS GIS version '", gv, "' R is running in!")
  }
  return(compatible)
}
