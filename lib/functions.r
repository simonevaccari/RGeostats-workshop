suppressWarnings(suppressPackageStartupMessages(library(RGeostats)))
suppressWarnings(suppressPackageStartupMessages(library(maps)))
suppressWarnings(suppressPackageStartupMessages(require(fields)))
suppressWarnings(suppressPackageStartupMessages(require(raster)))
suppressWarnings(suppressPackageStartupMessages(require(misc3d)))
suppressWarnings(suppressPackageStartupMessages(require(rgl)))

#
# Dump the whole contents in an external ASCII file (CSV format)
#
write_csv <- function(dbin,name,append=TRUE)
{
  ff = file(name)
  writeLines(paste(shQuote(dbin$names,type="cmd"),collapse=","),ff)
  close(ff)
  db.write.format(db=dbin,mode="csv",file=name,append=append,
                  sep=',',dec='.')
}

#
# Define complete path name
#
complete_path <- function(filename,flag.tif=FALSE,flag.png=FALSE)
{
	pathname = paste0(intaros.load.environ()$Image_name,"/",filename)
	if (flag.tif) pathname = paste0(pathname,".tif")
	if (flag.png) pathname = paste0(pathname,".png")
	pathname
}

#
# Read the contents of an external ASCII file (CSV format)
# and create 2D points database
#
read_csv <- function(name,na.strings="NA")
{
  table = read.table(name,header=T,sep=',',dec='.',na.strings=na.strings)

  # Suppress non-numeric columns within table
  for (id in names(table))
    if (is.factor(table[,id])) table[,id] = NULL

  # Create the new Data Base
  db = db.create(table[,names(table)!="rank"],flag.grid=FALSE,ndim=2,autoname=F)

  # Check mandatory variables
  if (is.na(match("Longitude", db$names)))
    stop("Missing variable name: ", "Longitude")
  if (is.na(match("Latitude", db$names)))
    stop("Missing variable name: ", "Latitude")
  if (is.na(match("Time", db$names)))
    stop("Missing variable name: ", "Time")
  if (is.na(match("Depth", db$names)))
    stop("Missing variable name: ", "Depth")
  db = db.locate(db, seq(1,db$natt))
  db = db.locate(db, c("Longitude","Latitude"), "x")
  db
}

#
# Converts the given dataset time (number of days since 1950) into a date
#
time2date <- function(time)
{
  date = as.Date(time, as.Date("1950-01-01"))
  date
}

#
# Converts the given date into dataset time convention
# (number of days since 1950)
#
date2time <- function(date)
{
  time = round(as.numeric(as.POSIXct(date) - as.POSIXct("1950-01-01")))
  time
}

#
# Converts a date / trimester into a date interval
#
create_date_lim <- function(year, trimester = NA, semester = NA)
{
  if (! miss(trimester))
  {
    if (trimester == 1)
      date_lim = c(paste0(year,"-01-01"), paste0(year,"-03-31"))
    else if (trimester == 2)
      date_lim = c(paste0(year,"-04-01"), paste0(year,"-06-30"))
    else if (trimester == 3)
      date_lim = c(paste0(year,"-07-01"), paste0(year,"-09-30"))
    else if (trimester == 4)
      date_lim = c(paste0(year,"-10-01"), paste0(year,"-12-31"))
  }
  else if (! miss(semester))
  {
    if (semester == 1)
      date_lim = c(paste0(year,"-01-01"), paste0(year,"-06-30"))
    else if (semester == 2)
      date_lim = c(paste0(year,"-07-01"), paste0(year,"-12-31"))
  }
  else
    date_lim = c(paste0(year,"-01-01"), paste0(year,"-12-31"))
  date_lim
}

#
# Returns the first day of the given date month (Start Of Month)
#
som <- function(date)
{
  # Calculate first day of the month
  lt   = as.POSIXlt(date)
  mon  = lt$mon+1
  year = lt$year+1900
  x = as.Date(paste0(year,'-',mon,'-1'), format="%Y-%m-%d")
  x
}

#
# Returns the last day of the given date month (End Of Month)
#
eom <- function(date)
{
  # Calculate first day of the month
  x = som(date)
  # Now advance one month and then go back one day
  next.mon = seq(x, length=2, by='1 month')[2]
  last.day = seq(next.mon, length=2, by='-1 day')[2]
  last.day = as.Date(last.day)
  last.day 
}

#
# Internal function to perform a selection
#
define.sel <- function(dbin,name,mini,maxi)
{
    dbin  = db.sel(dbin, dbin[, name] >= mini & dbin[, name] <= maxi, 
                   combine = "and", flag.replace = TRUE)
    dbin
}

#
# Masks some database samples which are out of the given intervals.
# The created selection variable is combined to the previous one (if it exists).
# 
# depth_limit: interval [min, max] between 0 and a maximum depth
#              (increasing downwards)
# long_lim:    longitude interval [min, max] in degrees between -60° and +60°
# lat_lim:     latitude interval [min, max] in degrees between -90° and +90°
# date_lim:    time interval of dates [min, max]
# var_lim:     z_var interval [min, max] 
# var_name:    name of the variable for var_lim interval
# compress:    if TRUE, the data base is 'reduced' to the valid samples
# verbose:     if TRUE, print the number of remaining active samples
#
apply_sel <- function(dbin, 
                      depth_lim = NA, 
                      long_lim = NA, 
                      lat_lim = NA, 
                      date_lim = NA, 
                      var_lim = NA, 
                      var_name = NA, 
		      compress = FALSE,
                      verbose = FALSE)
{
  # Longitude limits
  if (! miss(long_lim))
  {
    dbin  = define.sel(dbin, "Longitude", long_lim[1], long_lim[2])
  }

  # Latitude limits
  if (! miss(lat_lim))
  {
    dbin  = define.sel(dbin, "Latitude", lat_lim[1], lat_lim[2])
  }

  # Depth limits
  if (! miss(depth_lim))
  {
    dbin  = define.sel(dbin, "Depth", depth_lim[1], depth_lim[2])
  }

  # Dates limits
  if (! miss(date_lim))
  {
    dbin  = define.sel(dbin, "Time",
    	    	       date2time(date_lim[1]), date2time(date_lim[2]))
  }

  # Custom variable limits
  if (! miss(var_lim) & ! miss(var_name))
  {
    # Check input variable
    if (is.na(match(var_name, dbin$names))) 
      stop("Unknown variable name: ", var_name)
    dbin  = define.sel(dbin, var_name, var_lim[1], var_lim[2])
  }
  
  # Reducing the data base (optional) 
  if (compress)
  {
    vnames = setdiff(dbin$names,c("rank", "sel"))
    dbin   = db.reduce(dbin, names = vnames, flag.sel = TRUE)
  }

  if (verbose)
    cat("Number of active samples = ", dbin$nactive, 
    	    "( out of", dbin$nech, ")\n")
  
  dbin
}

#
# Removes the current active selection from the given database
#
remove_sel <- function(dbin)
{
  # Cleanup and remove all selection variables
  dbin = db.sel(dbin)
  dbin = db.delete(dbin, "sel*")
  dbin
}

#
# Returns the longitude range of the given database
#
get_long_lim <- function(dbin)
{
  lon_tab = db.extract(dbin, names="Longitude", flag.compress=FALSE)
  lat_tab = db.extract(dbin, names="Latitude" , flag.compress=FALSE)

  if (projec.query())
  {
	res = projec.operate(lon_tab, lat_tab)
	lon_tab = res$x
	lat_tab = res$y
  }

  limits   = range(lon_tab, na.rm=TRUE)
  long_lim = c(floor(limits[1]), ceiling(limits[2]))
  long_lim
}

#
# Returns the latitude range of the given database
#
get_lat_lim <- function(dbin)
{
  lon_tab = db.extract(dbin, names="Longitude", flag.compress=FALSE)
  lat_tab = db.extract(dbin, names="Latitude" , flag.compress=FALSE)

  if (projec.query())
  {
	res = projec.operate(lon_tab, lat_tab)
	lon_tab = res$x
	lat_tab = res$y
  }

  limits  = range(lat_tab, na.rm=TRUE)
  lat_lim = c(floor(limits[1]), ceiling(limits[2]))
  lat_lim
}

#
# Returns the depth range of the given database
#
get_depth_lim <- function(dbin)
{
  depth_lim = get_var_lim(dbin, var = "Depth")
  depth_lim
}

#
# Returns the dates range of the given database
#
get_time_lim <- function(dbin)
{
  tlim = get_var_lim(dbin, var = "Time")
  tlim
}

#
# Returns the variable range for the given database and the given variable name
#
get_var_lim <- function(dbin, var, do_round = FALSE)
{
  limits = range(db.extract(dbin, names=var, flag.compress=TRUE), na.rm = TRUE)
  if (do_round)
    var_lim = c(floor(limits[1]), ceiling(limits[2]))
  else
    var_lim = c(limits[1], limits[2])
  var_lim
}

#
# Creates a title composed from a variable name and a given date interval
#
get_title_date <- function(var, date_lim, prefix=NA)
{
  if (miss(prefix))
    title = paste0(var," (",date_lim[1]," => ",date_lim[2],")")
  else
    title = paste0(prefix,var," (",date_lim[1]," => ",date_lim[2],")")
  title
}

#
# Creates a title composed from the variable of interest and 
# the time interval from the db
#
get_db_title_time <- function(dbin, var = NA)
{
  if (miss(var)) var = db.getname(dbin, "z", 1)
  date_lim = time2date(get_time_lim(dbin))
  title = get_title_date(var, date_lim)
  title
}

#
# Creates a title composed from the variable of interest and 
# the depth interval from the db
#
get_db_title_depth <- function(dbin, var = NA)
{
  if (miss(var)) var = db.getname(dbin, "z", 1)
  depth_lim = get_depth_lim(dbin)
  title = paste0(var," for Depth ")
  title = get_title_date(title, depth_lim)
  title
}

#
# Creates a 2D grid database [long,lat] with a given mesh (degrees):
# - which enclosed input data from the given database or
# - with the given longitude (long_lim) and latitude (lat_lim) size
#
create_grid_2D <- function(dbin, long_lim=NA, lat_lim=NA, mesh = 1)
{
  # Define bounds (if not already defined)
  if (miss(long_lim)) long_lim = get_long_lim(dbin)
  if (miss(lat_lim))  lat_lim  = get_lat_lim(dbin)
  
  nx  = ceiling((long_lim[2]-long_lim[1])/mesh)+1
  ny  = ceiling((lat_lim[2]-lat_lim[1])/mesh)+1

  dbg = db.create(nx = c(nx, ny),
                  x0 = c(long_lim[1], lat_lim[1]), 
                  dx = c(mesh, mesh))
  dbg
}

#
# Creates a 3D grid database [long,lat] with a given horizontal mesh (degrees):
# - which enclosed input data from the given database or
# - with the given longitude (long_lim) and latitude (lat_lim) size
# - with given vertical mesh
#
create_grid_3D <- function(dbin, long_lim=NA, lat_lim=NA,
	       	           mesh, depth0, ndepth, ddepth)
{
  # Define bounds (if not already defined)
  if (miss(long_lim)) long_lim = get_long_lim(dbin)
  if (miss(lat_lim))  lat_lim  = get_lat_lim(dbin)
  
  nx  = ceiling((long_lim[2]-long_lim[1])/mesh)+1
  ny  = ceiling((lat_lim[2]-lat_lim[1])/mesh)+1
  nz  = ndepth

  dbg = db.create(nx = c(nx, ny, nz),
                  x0 = c(long_lim[1], lat_lim[1], depth0), 
                  dx = c(mesh, mesh, ddepth))
  dbg
}

#
# Plots the cells of a grid built from the given database
#
# dbin:     input database
# long_lim: longitude interval of the output plot
# lat_lim:  latitude interval of the output plot
# mesh:     output grid mesh
# col:      color of the contour lines
#
display_grunit <- function(dbin, long_lim=NA, lat_lim=NA, mesh, 
                           col = "lightgrey",...)
{
  # Define bounds (if not already defined)
  if (miss(long_lim)) long_lim = get_long_lim(dbin)
  if (miss(lat_lim))  lat_lim  = get_lat_lim(dbin)
  
  # Creates the output grid
  dbunit = create_grid_2D(dbin, long_lim, lat_lim, mesh)

  # Add the grid contour lines to the current plot
  db.grid.plot(dbunit, col.lines = col,...)

  invisible()
}

#
# Plots the given variable from the given database
#
# dbin:        input database
# var:         variable name to be ploted
# var_lim:     variable interval limit to be applied
# title:       title attached to the plot (or NA)
# flag.mesh:   if TRUE, display the grid built from the input database
# mesh:        mesh of the grid if flag.mesh = TRUE
# flag.xvalid: When TRUE, display the result of Cross-validation
# pos.legend:  when >0, gives the location of the legend
# flag.coast:  if TRUE, represents the coast line
# filename:    provides the filename (used for external plotting)
#
display_var <- function(dbin, var = NA, var_lim = NA,
                        title=NA, flag.mesh = TRUE, mesh = 1, flag.xvalid=FALSE,
			pos.legend = 1, flag.coast = TRUE, 
			filename=NA)
{
  # Field and variable limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim
  if (miss(var_lim))  var_lim  = get_var_lim(dbin, var)

  # Locatorize variable of interest
  dbin = db.locate(dbin, var, "z")  
  
  # Open the external file (if needed)
  if (intaros.load.environ()$Flag_file)
  { 
    filename = complete_path(filename,flag.png=TRUE)
    png(filename = filename)
  }

  # First, display the unit grid
  add = FALSE
  if (flag.mesh)
  {
    display_grunit(dbin, long_lim, lat_lim, mesh = mesh,
                   title = title, xlab = "Longitude", ylab = "Latitude")
    add = TRUE
  }

  # Then Perform the plot 
  coast = NA
  if (flag.coast) coast = "world"

  if (flag.xvalid)
    plot(dbin, pch = 16, name.prop = var,
         col = rg.colors(), 
         xlim = long_lim, ylim = lat_lim, 
         zmin = var_lim[1], zmax = var_lim[2], flag.abs=TRUE,
	 pos.legend = pos.legend, coast = coast, 
         add = add, title = title, xlab = "Longitude", ylab = "Latitude")
  else
    plot(dbin, pch = 16, name.color = var,
         cex = 0.8, col = rg.colors(), 
         xlim = long_lim, ylim = lat_lim,
         zmin = var_lim[1], zmax = var_lim[2], 
	 pos.legend = pos.legend, coast = coast, 
         add = add, title = title, xlab = "Longitude", ylab = "Latitude")
  
  # Close the external file (if needed)
  if (intaros.load.environ()$Flag_file) dev.off()

  invisible()
}

#
# Displays the result of stats_grid
#
# dbg:        grid database containing the computed statistics
# var:        interest variable
# var_lim:    interest variable limit for colorscale
# title:       title attached to the plot (or NA)
# flag.mesh:  if TRUE, display the grid built from the input database
# mesh:       mesh (degrees) of the output grid
# pos.legend: when >0, gives the location of the legend
# flag.coast: if TRUE, represents the coast line
# filename:   provides the filename (used for external plotting)
#
display_stats <- function(dbg, var = NA, var_lim = NA,
                          title = NA, flag.mesh = TRUE, mesh = 1,
			  pos.legend = 1, flag.coast = TRUE,
			  filename=NA)
{
  # Estimated variable limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim
  if (miss(var_lim))  var_lim  = get_var_lim(dbg, var)

  # Open the external file (if needed)
  if (intaros.load.environ()$Flag_file)
  { 
    filename = complete_path(filename,flag.png=TRUE)
    png(filename = filename)
    cat("Graphic file created:",filename,"\n")
  }

  # First, display the unit grid
  add = FALSE
  if (flag.mesh)
  {
    display_grunit(dbg, long_lim, lat_lim, mesh = mesh,
                   title = title, xlab = "Longitude", ylab = "Latitude")
    add = TRUE
  }

  # Plot
  cols = rg.colors(100)
  coast = NA
  if (flag.coast) coast = "world"
  plot(dbg, col = cols,
       xlim = long_lim, ylim = lat_lim, zlim = var_lim,
       pos.legend = pos.legend, coast = coast,
       add = add, title = title, xlab = "Longitude", ylab = "Latitude")

  # Close the external file (if needed)
  if (intaros.load.environ()$Flag_file)
    dev.off()

  invisible()
}

#
# Displays the result of the interpolation
#
# dbin:       Data file (optional)
# dbg:        Output grid file
# var:        variable name to be ploted
# depth:      estimation depth
# var_lim:    interest variable limit for colorscale
# flag.estim: TRUE for estimation display, FALSE for standard deviation
# ref:        Reference corner (only used in 3D)
# pos.legend: when >0, gives the location of the legend
# flag.coast: if TRUE, represents the coast line
# filename:   provides the filename (used for external plotting)
#
display_result <- function(dbin = NA, dbg, var = NA, depth, var_lim = NA,
                           flag.estim = TRUE, ref=NA,
			   pos.legend = 7, flag.coast = TRUE, filename=NA)
{
  # Estimated variable limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim
  
  # Select the variable
  if (flag.estim) {
    name  = paste0("*",var,"*estim")
    title = "Estimation "
    cols  = rg.colors(100)
    rank  = 1
  }
  else {
    name  = paste0("*",var,"*stdev")
    title = "St. Deviation "
    cols  = tim.colors(100)
    rank  = 2
  }
  if (miss(var_lim))  var_lim  = get_var_lim(dbg, name)

  # Open the external file (if needed)
  if (intaros.load.environ()$Flag_file)
  { 
    filename = complete_path(filename,flag.png=TRUE)
    png(filename = filename)
  }

  # Title of the plot
  title = paste0(title, "at ", depth, "m depth\n",
                 get_title_date(var, time2date(get_time_lim(dbin))))
  
  # Plot
  coast = NA
  if (flag.coast) coast = "world"
  plot(dbg, name = name, col = cols,
       xlim = long_lim, ylim = lat_lim, zlim = var_lim,ref=ref,
       xlab = "Longitude", ylab = "Latitude", 
       title = title, pos.legend = pos.legend, coast = coast)

  # Overlay the data
  if (flag.estim)
    plot(dbin, name = var, pch = 3, col = "black", add = TRUE)
  else
    plot(dbin, name.post = "rank", pch=21, bg = "red", fg = "black", add=TRUE)
  
  # Close the external file (if needed)
  if (intaros.load.environ()$Flag_file) dev.off()

  invisible()
}

#
# Aggregate data along Depth axis
#
# dbin:       input data grid
# depth0:     origin of depth values
# ddepth:     step along depth
# ndepth:     number of depth steps
#
aggregate_depth <- function(dbin, depth0, ddepth, ndepth, flag.center = FALSE)
{
  # Update locators
  dbin = db.locate(dbin, name = c("Longitude", "Latitude", "Depth"), "x")
  dbin = db.locerase(dbin, "z")
  vnames = setdiff(dbin$names,
                   c("rank","Longitude","Latitude","Depth","Profil_id"))
  dbin = db.locate(dbin, vnames, "z")
  dbin = db.locate(dbin, "Profil_id", "code")
  
  # Establish the 1-D regular grid
  box      = dbin$bbox
  x0       = box[1,]
  x0[3]    = depth0
  nx       = rep(1,3)
  nx[3]    = ndepth
  dx       = box[2,] - box[1,]
  dx[3]    = ddepth
  dbbox    = db.create(nx = nx, x0 = x0, dx = dx)
  dbout    = db.regularize(dbin, dbbox, flag.center=flag.center)

  # Remove the unused third dimension
  dbout = db.locerase(dbout, "x")
  if (ndepth > 1)
    dbout = db.locate(dbout, name = c("Longitude", "Latitude", "Depth"), "x")
  else
    dbout = db.locate(dbout, name = c("Longitude", "Latitude"), "x")
  dbout = db.locerase(dbout, "z")
  
  dbout
}

#
# Calculate an experimental varigoram
#
# db2:        input data grid
# dirvect:    List of calculation angles (or NA for omni-directional)
# vario_lag:  lag of the variogram (used if model not defined)
# vario_nlag: number of variogram lags (used if model not defined)
# bench:      size of the bench (0 if not used)
# nbench:     Number of benches (0 if not used)
# flag.plot:  plot the variogram if TRUE
# verbose:    Verbosity flag
#
prepar_vario <- function(db2, dirvect = c(0, 45, 90, 135), vario_lag = 0.5, 
                         vario_nlag = 20, bench = 0, nbench = 0,
			 flag.plot=FALSE, verbose = FALSE, ...)
{
  # Experimental Variogram (XoY plane)
  vario = vario.calc(db2, dirvect=dirvect, lag=vario_lag, nlag = vario_nlag,
                     bench = bench)

  # Add the vertical variogram (optional)
  # The next line has been added artificially. It converts the 3-D initial
  # variogram into a 3-D isotropic one. This is necessary to let the 3-D viewer
  # produce a reasonable picture.
  nbench = 0
  if (nbench > 0)
	vario = vario.calc(db2,
	        dirvect=get.directions(matrix(c(0,0,1),ncol=1),ndim=3),
		tolang = 5, lag = bench, nlag = nbench, vario.add=vario)
  
  # Optional printout
  if (verbose) print(vario, verbose = verbose)

  # Plot the variogram
  if (flag.plot) plot(vario,...)
  vario
}

#
# Fit a Model (or return the input Model)
#
# model:      variogram model (if not defined, the model is automatically
#             calculated)
# db2:        input data grid
# var:        interest variable
# dirvect:    List of calculation angles (or NA for omni-directional)
# vario_lag:  lag of the variogram (used if model not defined)
# vario_nlag: number of variogram lags (used if model not defined)
# bench:      size of the bench (0 if not used)
# nbench:     Number of benches (0 if not used)
# struct:     covariance indices to be used for fitting the variogram model
#             (see melem.name())
# draw:       Flag for drawing the experimental variogram and the fitted model
# verbose:    Verbosity flag
#
prepar_model <- function(model=NA, db2=NA, var=NA,
                         dirvect = c(0, 45, 90, 135), vario_lag = 0.5, 
                         vario_nlag = 20, bench = 0, nbench = 0,
			 struct = c(1,12), draw = TRUE, verbose = FALSE)
{
  if (miss(model)) 
  {
    # Experimental variogram
    vario = prepar_vario(db2,
                         dirvect = dirvect, vario_lag = vario_lag,
                         vario_nlag = vario_nlag, bench = bench,
			 nbench = nbench, verbose = verbose)

    # Fit automatically the previous variogram
    pos.legend = 7
    if (length(dirvect) == 1) pos.legend = 0
    model = model.auto(vario, maxiter = 2000, struct = struct,
                       flag.noreduce = TRUE, draw = draw,
                        pos.legend = pos.legend,
		       cex = 0.2, opt.varname=2, cex.varname=1)

    # Optional printout
    if (verbose) print(model)
  }
  model
}

#
# Cross-validation for a given variable
#
# dbin:       input data grid
# var:        interest variable
# vario_lag:  lag of the variogram (used if model not defined)
# vario_nlag: number of variogram lags (used if model not defined)
# moving:     if TRUE, use a moving neighborhood
# nmaxi:      Maximum number of samples per Moving neighborhood 
# model:      variogram model (if not defined, the model is automatically
#             calculated)
# struct:     covariance indices to be used for fitting the variogram model
#             (see melem.name())
# dirvect:    List of calculation angles (or NA for omni-directional)
# radix:      Radix added to any output variable
# draw:       Drawing the Model
# verbose:    Verbosity flag
#
xvalid_2D <- function(dbin, var,
                      vario_lag = 0.5, vario_nlag = 20,
 		      moving = FALSE, nmaxi = 40, model = NA,
                      struct = c(1,12), dirvect = c(0, 45, 90, 135),
                      radix = "Xvalid", draw = FALSE, verbose=FALSE)
{
  # Field limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim

  # Define the locators
  dbin = db.locerase(dbin,"x")
  dbin = db.locate(dbin,c("Longitude","Latitude"),"x")
  dbin = db.locerase(dbin,"z")
  dbin = db.locate(dbin,var,"z")
  
  # Clean the data set from duplicates
  dbin = duplicate(dbin,print=FALSE)
  if (verbose) cat("Number of active samples=",dbin$nactive,"\n")
  
  # Calculate the Model
  model = prepar_model(model, dbin, var, dirvect = dirvect, 
                       vario_lag = vario_lag, vario_nlag = vario_nlag,
                       struct = struct, draw = draw, verbose = verbose)
                       
  # Create the appropriate required neighborhood
  if (moving)
    neigh = neigh.create(ndim = 2, type = 2, nmaxi = nmaxi, 
                         radius = 20, flag.sector = TRUE, 
                         nsect = 8, nsmax = nmaxi / 8)
  else
    neigh = neigh.create(ndim = 2, type = 0)
  
  # Cross-validation
  dbp = xvalid(db = dbin, model = model, neigh = neigh, radix = radix)

  dbp
}

#
# Calculates interpolation map in 2D for a given variable
# on an output grid covering the data
#
# dbin:       input data grid
# var:        interest variable
# mesh:       mesh (degrees) of the output grid
# vario_lag:  lag of the variogram (used if model not defined)
# vario_nlag: number of variogram lags (used if model not defined)
# moving:     if TRUE, use a moving neighborhood
# nmaxi:      Maximum number of samples per Moving neighborhood 
# model:      variogram model (if not defined, the model is automatically
#             calculated)
# struct:     covariance indices to be used for fitting the variogram model
#             (see melem.name())
# dirvect:    List of calculation angles (or NA for omni-directional)
# draw:       Drawing the Model
# verbose:    Verbosity flag
#
interpolate_2D <- function(dbin, var, mesh = 1,
                           vario_lag = 0.5, vario_nlag = 20,
			   moving = FALSE, nmaxi = 40, model = NA,
                           struct = c(1,12), dirvect = c(0, 45, 90, 135),
                           draw = FALSE, verbose=FALSE)
{
  # Field limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim

  # Define the locators
  dbin = db.locerase(dbin,"x")
  dbin = db.locate(dbin,c("Longitude","Latitude"),"x")
  dbin = db.locerase(dbin,"z")
  dbin = db.locate(dbin,var,"z")
  
  # Clean the data set from duplicates
  dbin = duplicate(dbin,print=FALSE)
  if (verbose) cat("Number of active samples=",dbin$nactive,"\n")
  
  # Calculate the Model
  model = prepar_model(model, dbin, var, dirvect = dirvect, 
                       vario_lag = vario_lag, vario_nlag = vario_nlag,
                       struct = struct, draw = draw, verbose = verbose)
                       
  # Creates the output grid and mask continental targets
  dbg  = create_grid_2D(dbin, long_lim, lat_lim, mesh)
  bbox = dbg$bbox
  w    = map(xlim = bbox[,1], ylim = bbox[,2], plot = FALSE)
  p    = polygon.create(w$x, w$y)
  dbg  = db.polygon(dbg, p, flag.out = TRUE)
  
  # Create the appropriate required neighborhood
  if (moving)
    neigh = neigh.create(ndim = 2, type = 2, nmaxi = nmaxi, 
                         radius = 20, flag.sector = TRUE, 
                         nsect = 8, nsmax = nmaxi / 8)
  else
    neigh = neigh.create(ndim = 2, type = 0)
  
  # Interpolate the variable on a 2D grid covering the whole field
  # by using the Kriging method with a 2D Moving Neighborhood
  radix = "Kriging"
  if (length(var) > 1) radix = "CoKriging"
  dbg = kriging(dbin = dbin, dbout = dbg, model = model, neigh = neigh,
                radix = radix)

  dbg
}

#
# Calculates interpolation map in 3D for a given variable
# on an output grid covering the data
#
# dbin:       input data grid
# var:        interest variable
# mesh:       mesh (degrees) of the output grid
# depth0:     origin of the vertical mesh
# ndepth:     number of layers
# ddepth:     layer width
# vario_lag:  lag of the variogram (used if model not defined)
# vario_nlag: number of variogram lags (used if model not defined)
# struct:     covariance indices to be used for fitting the variogram model
#             (see melem.name())
# dirvect:    List of calculation angles (or NA for omni-directional)
# draw:       Drawing the Model
# verbose:    Verbosity flag
#
interpolate_3D <- function(dbin, var, mesh = 1, depth0, ndepth, ddepth,
  	                   vario_lag = 0.5, vario_nlag = 20, 
                           struct = c(1,12), dirvect = c(0, 45, 90, 135),
                           draw = FALSE, verbose=FALSE)
{
  # Field limits
  long_lim = intaros.load.environ()$Long_lim
  lat_lim  = intaros.load.environ()$Lat_lim

  # Define the locators
  dbin = db.locerase(dbin,"x")
  dbin = db.locate(dbin,c("Longitude","Latitude","Depth"),"x")
  dbin = db.locerase(dbin,"z")
  dbin = db.locate(dbin,var,"z")
  
  # Clean the data set from duplicates
  dbin = duplicate(dbin,print=FALSE)
  if (verbose) cat("Number of active samples=",dbin$nactive,"\n")
  
  # Create the appropriate bench neighborhood
  neigh = neigh.create(ndim = 3, type = 1, width = ddepth/2)
  
  # Creates the output grid and mask continental targets
  dbg  = create_grid_3D(dbin,long_lim,lat_lim,
                        mesh, depth0, ndepth, ddepth)
  bbox = dbg$bbox
  w    = map(xlim = bbox[,1], ylim = bbox[,2], plot = FALSE)
  p    = polygon.create(w$x, w$y)
  dbg  = db.polygon(dbg, p, flag.out = TRUE)

  # Calculate the 3-D Model
  # (Bench slicing in horizontal variogram + Vertical variogram)
  model = prepar_model(model=NA, dbin, var, dirvect = dirvect, 
                       vario_lag = vario_lag, vario_nlag = vario_nlag,
		       bench = ddepth, nbench = ndepth/2, struct = struct,
		       draw = draw, verbose = verbose)
  
  # Interpolate the variable on a 3D grid covering the whole field
  radix = "Kriging"
  if (length(var) > 1) radix = "CoKriging"
  dbg = kriging(dbin = dbin, dbout = dbg, model = model, neigh = neigh,
                radix = radix)

  dbg
}

#
# Calculates statistics for a given variable on an output grid covering the data
#
# dbin:      input data grid
# var:       interest variable
# fun:       statistic function to be executed (see ?blockstat)
# mesh:      mesh (degrees) of the output grid
# verbose:   if TRUE, print the number of remaining active samples
#
stats_grid <- function(dbin, var, fun = "num", mesh = 1, verbose=FALSE)
{
  # Field limits
  long_lim  = intaros.load.environ()$Long_lim
  lat_lim   = intaros.load.environ()$Lat_lim
  
  # Define the locators
  dbin = db.locerase(dbin, "z")
  dbin = db.locate(dbin, c(var,"Time"), "z")
  
  # Create the output grid
  dbg = create_grid_2D(dbin, long_lim, lat_lim, mesh)
  
  # Return cell statistics
  dbg = blockstat(dbgrid = dbg, db = dbin, fun = fun)

  dbg
}

#
# Calculate the average along time of the target variable
#
# dbin:      input database
# var:       interet variable
# years:     array of dates
#
average_time <- function(dbin, var, years)
{
  nyears = length(years)
  count  = numeric(nyears)
  means  = numeric(nyears)
  vars   = numeric(nyears)

  ecr = 1
  for (year in years)
  {
    dbin      = remove_sel(dbin)
    date_lim  = create_date_lim(year)
    dbin      = apply_sel(dbin, date_lim = date_lim)
  
    # Calculate the mean and variance on initial data
    tab         = db.extract(dbin,var)
    count[ecr]  = length(tab)
    means[ecr]  = mean(tab)
    vars[ecr]   = var(tab)
    ecr         = ecr + 1
  }
  res = list(count=count, means=means, vars=vars)
  res
}

#
# Calculate the average along depth of the target variable
#
# dbin:      input database
# var:       interet variable
# depth0:    origin of depth values
# ndepth:    number of depth steps
# ddepth:    step along depth
#
average_depth <- function(dbin, var, depth0, ndepth, ddepth)
{
  depths = numeric(ndepth)
  count  = numeric(ndepth)
  means  = numeric(ndepth)
  vars   = numeric(ndepth)

  for (i in 1:ndepth)
  {
    dbin = remove_sel(dbin)
    depths[i] = ddepth * (i + 0.5)
    depth_lim = c(depths[i] - ddepth/2, depths[i] + ddepth/2)
    dbin = apply_sel(dbin, depth_lim = depth_lim)

    # Calculate the mean and variance
    tab      = db.extract(dbin,var)
    count[i] = length(tab)
    means[i] = mean(tab)
    vars[i]  = var(tab)
  }
  res = list(count=count, means=means, vars=vars, depths=depths)
  res
}

#
# Builds a new 1D database (along time) for the given variable
# by calculating the mean for each time interval
#
# dbin:      input database
# var:       interet variable
# time_step: time interval length (number of days)
# depth_lim: depth interval to be considered for the data
# date_lim:  date interval to be considered for the data
#
regular_time <- function(dbin, var, time_step = NA,
                         depth_lim = NA, date_lim = NA)
{
  # Field limits
  if (miss(depth_lim)) depth_lim = get_depth_lim(dbin)
  if (miss(date_lim))
  {
    time_lim  = get_time_lim(dbin)
    date_lim  = time2date(time_lim)
  }
  
  # Select data  
  db2 = apply_sel(dbin,
                  depth_lim = depth_lim,
                  date_lim = date_lim,
                  compress=TRUE, verbose = FALSE)
  
  # Create new variable
  breaks = seq(time_lim[1], time_lim[2] + time_step - 1, time_step)
  cuts = cut(db2$Time, breaks = breaks)
  values = as.numeric(tapply(db2[,var],cuts,"mean"))
  
  dbg = db.create(nx = length(breaks) - 1,
                  x0 = time_lim[1], dx = time_step, z1 = values)
  dbg = db.rename(dbg, "x1", "Time")
  dbg = db.rename(dbg, "z1", var)
  dbg
}

#
# Builds a new 1D database (along depth) for the given variable
# by calculating the mean for each depth interval
#
# dbin:       input database
# var:        interet variable
# depth_step: depth interval length (meter)
# depth_lim:  required minimum and maximum depth
# date_lim:   date interval to be considered for the data
#
regular_depth <- function(dbin, var, depth_step = NA,
	                  depth_lim = NA, date_lim = NA)
{
  # Field limits
  if (miss(depth_lim)) depth_lim = get_depth_lim(dbin)
  if (miss(date_lim))  date_lim  = time2date(get_time_lim(dbin))
  
  # Select data  
  db2 = apply_sel(dbin,
                  depth_lim = depth_lim,
                  date_lim = date_lim,
                  compress=TRUE, verbose = FALSE)
  
  # Create new variable
  breaks = seq(depth_lim[1], depth_lim[2] + depth_step - 0.1, depth_step)
  cuts = cut(db2$Depth, breaks = breaks)
  values = as.numeric(tapply(db2[,var],cuts,"mean"))
  
  dbg = db.create(nx = length(breaks) - 1,
                  x0 = depth_lim[1], dx = depth_step, z1 = values)
  dbg = db.rename(dbg, "x1", "Depth")
  dbg = db.rename(dbg, "z1", var)
  dbg
}

#
# Dumping out the results in a GeoTiff format
#
write_geotiff <- function(dbg, radix=NA)
{
  if (!dbg$flag.grid || dbg$ndim != 2)
    stop("Wrong db grid format")

  coords = db.getcols(dbg,"x")
  vars   = db.getcols(dbg,"z")
  if (length(vars) <= 0 || length(coords) <= 0)
    stop("Missing locator x or z in the given db")
  
  # Raster extension include cell size
  nrow = dbg$nx[2]
  ncol = dbg$nx[1]
  dx   = dbg$dx
  min  = dbg$limits[1,] - dx / 2
  max  = dbg$limits[2,] + dx / 2
  r    = raster(ncol = ncol,   nrow = nrow , 
                xmn  = min[1], ymn  = min[2], 
		xmx  = max[1], ymx  = max[2])
  
  # Test resolution
  if (abs(prod(res(r)[1]-dx[1],res(r)[2]-dx[2])) > 1e-10)
    stop("Wrong resolution")
  
  # Write each variable into a GeoTiff file
  for (var in seq(1:length(vars)))
  {
    varname = db.getname(dbg, "z", var)
    vals = dbg[,varname]
    m = matrix(nrow=nrow, ncol=ncol, vals, byrow=TRUE)
    r = setValues(r, as.vector(t(m[nrow:1,])))
    if (! miss(radix))
      filename = paste0(radix, "_", varname)
    else
      filename = paste0(varname)
    filename = complete_path(filename,flag.tif=TRUE)
    writeRaster(r, filename=filename, format="GTiff", overwrite=TRUE)
  }
}

#
# Display the results as iso-surfaces in RGL library
#
display_rgl <- function(dbg,isoval,isocol,
                        theta=0, phi=-55, size=700,
			aspect.x=1.4, aspect.y=1.1, aspect.z=0.4,
			flag.coast=FALSE, flag.movie=FALSE, ...)
{
  rgl.open()
  rgl.clear()
  par3d(windowRect = 50 + c( 0, 0, size, size) )
  rgl.bg(color="white")
  rgl.viewpoint(theta = theta, phi = phi, ...)

  zmax = range(dbg[,"x3"])[2]
  Gval = db.extract(dbg,names="*estim",flag.compress=FALSE)
  Gval[is.na(Gval)] = zmax
  Gval = array(Gval, dim=dbg$nx)

  contour3d(Gval, x = unique(dbg[,"x1"]), y = unique(dbg[,"x2"]),
            z = unique(dbg[,"x3"]), level = isoval, color = isocol)

  if (flag.coast)
  {
	res = maps::map("world",
	                xlim = intaros.load.environ()$Long_lim,
	                ylim = intaros.load.environ()$Lat_lim,
	                plot=FALSE, fill=TRUE, lforce="e")
	cat("Number of vertices = ",length(res$x),"\n")

	polygon3d(res$x,res$y,rep(zmax,length(res$x)),fill=FALSE,lwd=2)
  }

  box3d(col=1)
  title3d(xlab='X', ylab='Y', zlab='Z')
  aspect3d(aspect.x,aspect.y,aspect.z)

  

  if (flag.movie) movie3d( spin3d(), duration=5, dir=paste0(getwd(),"/movie"))
}

#
# Load the environment variables
#
intaros.load.environ <- function(filename = ".Intaros.Environ",
                                 verbose = FALSE)
{
  if (exists(filename)) 
     res = get(filename, pos = 1)
  else
     res = list(Long_lim = NA, Lat_lim = NA, Flag_file = NA, Image_name = NA)

  # Cancel Longitude and Latitude limits if a projection is defined
  if (projec.query())
  {
	res$Long_lim = NA
	res$Lat_lim  = NA
  }

  if (verbose) cat("Intaros Environment = ",paste(res),"\n")
  res     
}

#
# Save the environment variables
#
intaros.save.environ <- function(long_lim = NA, 
                                 lat_lim = NA,
                                 flag_file = NA, 
                                 image_name = NA,
       	                         filename = ".Intaros.Environ")
{
  res = intaros.load.environ(filename = filename)
  
  # Check that the directory exists
  if (flag_file && ! miss(image_name))
     dir.create(image_name, showWarnings = FALSE)

  # Set the Global Environment variables
  if (! miss(long_lim))   res$Long_lim = long_lim
  if (! miss(lat_lim))    res$Lat_lim  = lat_lim
  if (! miss(flag_file))  res$Flag_file = flag_file
  if (! miss(image_name)) res$Image_name = image_name

  assign(filename, res, pos = 1)
  invisible()
}

create_color_scale_rgba <- function(res)
{
  colors = rg.colors()
  ncolor = length(colors)
  rgb    = col2rgb(colors)
  rg    = range(res,na.rm=TRUE)
  zscale = seq(rg[1],rg[2],length.out=ncolor)
  
  tab = matrix(0,ncol=5, nrow=ncolor)
  tab[,1] = zscale
  tab[,2] = rgb[1,]
  tab[,3] = rgb[2,]
  tab[,4] = rgb[3,]
  tab[,5] = rep(40,ncolor)
  
  tab
}