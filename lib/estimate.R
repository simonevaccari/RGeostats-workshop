#!/opt/anaconda/envs/python2/bin/Rscript --vanilla --slave --quiet

# Source the IMR-RGeostats functions
# This script do not use (yet) the new RIntaros package.
source("./lib/functions.r")

# Retrieve the ordered script parameters
args = commandArgs(trailingOnly=TRUE)

csv_file = args[1]
date_param = args[2]
long_param = args[3]
lat_param = args[4]
depth_param = args[5]
var_param = args[6]
varlim_param = args[7]
mesh_param = args[8]
vario_lag_param = args[9]
vario_nlag_param = args[10]
struct_param = args[11]

# Print parameters
cat("Parameter values:\n")
cat(paste(" - csv_file =",csv_file,"\n"))
cat(paste(" - date_param =",date_param,"\n"))
cat(paste(" - long_param =",long_param,"\n"))
cat(paste(" - lat_param =",lat_param,"\n"))
cat(paste(" - depth_param =",depth_param,"\n"))
cat(paste(" - var_param =",var_param,"\n"))
cat(paste(" - varlim_param =",varlim_param,"\n"))
cat(paste(" - mesh_param =",mesh_param,"\n"))
cat(paste(" - vario_lag_param =",vario_lag_param,"\n"))
cat(paste(" - vario_nlag_param =",vario_nlag_param,"\n"))
cat(paste(" - struct_param =",struct_param,"\n"))

# Convert parameters
date_lim <- as.character(strsplit(date_param,",")[[1]])
long_lim <- as.numeric(strsplit(long_param,",")[[1]])
lat_lim <- as.numeric(strsplit(lat_param,",")[[1]])
depth_lim <- as.numeric(strsplit(depth_param,",")[[1]])
var <- as.character(var_param)
var_lim <- as.numeric(strsplit(varlim_param,",")[[1]])
mesh <- as.numeric(mesh_param)
vario_lag <- as.numeric(vario_lag_param)
vario_nlag <- as.numeric(vario_nlag_param)
struct <- as.numeric(strsplit(struct_param,",")[[1]])

# Calculate aggregation parameters
depth0 = depth_lim[1]
ddepth = depth_lim[2] - depth_lim[1]
mdepth = depth0 + 0.5 * ddepth

# Set global environment# Estimation parameters
intaros.save.environ(long_lim = long_lim, lat_lim = lat_lim,
                     flag_file = FALSE, image_name = file.path(getwd(),"images"))
# Load the CSV file
dbr = read_csv(csv_file)
# Select measures in the required interval
dbr = apply_sel(dbr, long_lim = long_lim, lat_lim = lat_lim, date_lim = date_lim, depth_lim = depth_lim)
# Aggregate all measures along vertical direction
dbr = aggregate_depth(dbr, depth0 = depth0, ddepth = ddepth, ndepth = 1, flag.center = TRUE)
# Interpolate values
res = interpolate_2D(dbin = dbr, var = var, mesh = mesh, 
                     vario_lag = vario_lag, vario_nlag = vario_nlag,
                     struct = struct, moving = FALSE)
# Display results
display_result(dbr, res, var = var, var_lim = var_lim, depth = mdepth, flag.estim = TRUE)
