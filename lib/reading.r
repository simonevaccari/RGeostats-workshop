suppressWarnings(suppressPackageStartupMessages(library("ncdf4")))
suppressWarnings(suppressPackageStartupMessages(library(RGeostats)))

# This function is an R function which loads NetCDF file from
# a local directory into a RGeostats database.
#
# IMR dataset files can be downloaded from the OpenDAP server:
# http://opendap1-test.nodc.no/thredds/catalogs/physics/physics_point_yearly.html
#
load_data <- function(dir = ".")
{
  load_file <- function(dir, file, vname = vname, vid = vid, dbin = NA)
  {
    # Load all data from the given file
    #  - Temperature = "TEMP"
    #  - Conductivity = "CNDC"
    #  - Salinity = "PSAL"
    file_path = file.path(dir, file)
    nc    = nc_open(file_path)
    temp  = ncvar_get(nc, "TEMP")
    cndc  = ncvar_get(nc, "CNDC")
    psal  = ncvar_get(nc, "PSAL")
    lat   = ncvar_get(nc, "LATITUDE")
    long  = ncvar_get(nc, "LONGITUDE")
    depth = ncvar_get(nc, "DEPH")
    time  = ncvar_get(nc, "TIME")
    nc_close(nc)

    # Number of data points = np * nd
    np = length(lat)
    nd = dim(depth)[1]
    
    # Profil index
    pid = 1
    if (!is.na(dbin)) pid = max(dbin[,"Profil_id"]) + 1

    # Input data frame preparation
    df = data.frame(Longitude    = as.numeric(long[sort(rep(1:np, nd))]), 
                    Latitude     = as.numeric(lat[sort(rep(1:np, nd))]), 
                    Depth        = as.vector(depth), 
                    Temperature  = as.vector(temp), 
                    Conductivity = as.vector(cndc), 
                    Salinity     = as.vector(psal), 
                    Time         = as.vector(time),
                    Profil_id    = as.numeric(sort(rep(seq(pid,pid+np-1), nd))),
                    Vessel_name  = rep(vname,np*nd),
                    Vessel_id    = rep(vid,np*nd)
                    )
    
    # Remove NA samples
    df = df[!is.na(df$Depth), ]
    
    # Append samples to given database
    if (is.na(dbin)) { db = db.create(df) }
    else             { db = db.append(dbin, df, flag.match = TRUE) }
    
    cat("Reading", file, ": Number of positions =", np, 
        ", Maximum depth =", nd, "m)\n")
    db
  }
  
  # Load all the data from all the NetCDF files
  files = list.files(path = dir, pattern = "*.nc")
  if (length(files) ==  0) stop("No NetCDF file found")
  init = TRUE
  vessels = c()
  for (file in files)
  {
    # Get vessel name
    vname = substr(unlist(strsplit(file,"^58"))[2],1,2)
    vid = which(vessels == vname)
    if (length(vid) == 0) {
      # Unknown vessel, add it to the list
      vessels = c(vessels, vname)
      vid = length(vessels)
    }
    else {
      # Else, get the vessel index
      vid = vid[1]
    }
    
    # Load the file
    if (init) {
       dbin = load_file(dir = dir, file = file, vname = vname, vid = vid);
       init = FALSE
    }
    else {
       dbin = load_file(dir = dir, file = file, vname = vname, vid = vid,
       dbin = dbin)
    }
  }
  
  # Identify coordinates (Longitude, Latitude and Depth)
  dbin = db.locate(dbin, 2:4, "x")
  dbin
}
