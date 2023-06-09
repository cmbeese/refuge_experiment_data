#########################################################################################
# Parameters script
# Author: Alice Rogers
# Last update: 27/01/2021
# Purpose: Define function to set all parameters used in model, both variable and fixed
#########################################################################################

set_param <- function(){
  
  param = list()
  
  #---------------------------------------------------------------------------------------
  ##### Variable Parameters ####
  #---------------------------------------------------------------------------------------
  
  # Read in any refuge data being used in model - not used in current version
  
    # refuges      <- read.delim("data/FORCE_refuge_ss_per_site.txt", header = TRUE)
    # param$refuge <- refuges[,8]
  
  
  #### Algal Dynamics ####
  param$alr = 109         # Max algal growth rate per year
  
  
  #### Primary productivity and search rates ####
  param$pp       = -0.5   # Primary productivity 
  param$A.u_reef = 6.4    # Yearly rate volume searched by predators pre settlement
  param$A.u_pel  = 10     # Yearly rate volume searched by predators post settlement
  param$A.u  = 6.4        # Yearly rate volume searched by predators - settlement not considered?
  param$A.v  = 0.1        # Yearly rate volume searched by invertebrates 
  param$A.h  = 0.2        # Yearly rate volume searched by herbivores
  param$flow = 10         # WHAT IS THIS?
  
  
  ##### Feeding preferences (PPMR) ####
  param$pref.pel  = 0.33  # Preference for "pelagic" carnivorous prey 
  param$pref.ben  = 0.5   # Preference for "benthic" invertebrate prey
  param$pref.herb = 0.17  # Preference for herbivorous prey
  param$pref.det  = 0     # Preference for detritivores
  param$pref.alg  = 1     # Preference for algae
  
  
  ##### Fishing ####
  param$Fmort_pred = 0            # Fishing mortality rate of predators per year 
  param$Fmort_herb = 0            # Fishing mortality rate of herbivores per year
  param$min.fishing.size = 1      # Minimum log10 body size fished
  param$max.fishing.size = 3.5    # Maximum log10 body size fished
  
  
  ##### Habitat complexity ####
  param$setsp  = 1000       # slope of decrease (settlement onto reef structure)
  param$emsp   = 100        # increasing slope (emergence from reef structure)
  param$settle = 0.1        # weight when settle to reef structure
  param$emerge = 1000       # maximum refuge size in body mass (range explored = 1-1500) 
  
  
  ##### Experimental complexity parameters ####
  param$bin1   = 0.1       # proportion of designated population bin available to predators (range: 1-0.1, 1 = no complexity)
  param$bin2   = 1
  param$bin3   = 1
  param$bin4   = 1
  param$bin5   = 1
  param$bin6   = 1
  param$bin7   = 1
  param$bin8   = 1
  param$bin9   = 1
  param$bin10  = 1
  param$elseAv = 1
  
  param$start1  = 0         # start weight to define experimental bin
  param$start2  = 0
  param$start3  = 0
  param$start4  = 0
  param$start5  = 0
  param$start6  = 0
  param$start7  = 0
  param$start8  = 0
  param$start9  = 0
  param$start10 = 0
  
  
  param$end1   = 25        # end weight to define experimental bin
  param$end2   = 0
  param$end3   = 0
  param$end4   = 0
  param$end5   = 0
  param$end6   = 0
  param$end7   = 0
  param$end8   = 0
  param$end9   = 0
  param$end10  = 0

  #---------------------------------------------------------------------------------------
  ##### Fixed parameters ####
  #---------------------------------------------------------------------------------------
  
  #### PPMR ####
  # Predator Prey Mass Ratio - feeding kernel distribution
  param$q0   = 2.0              # Mean log10 predator prey mass ratio  100:1.(beta in paper)
  param$sd.q = 1.0              # 0.6 to 1.0 for lognormal prey preference function. 5 = B&R ref value (sigma, standard deviation)
  param$q.inv   = 2.5
  param$sd.qinv = 2
  
  
  param$qmax = param$q0 + 2*param$sd.q        # Truncation of the feeding kernel 95% distribution between +/- 2*sd.q
  param$qmin = param$q0 - 2*param$sd.q
  
  
  # Detrital Recycling
  param$det.coupling = 1.0
  param$sinking.rate = 0.8        # 35% of PP material reaches the seafloor, Davies & Payne 1983 Marine biology
  
  
  # Metabolic Requirements
  param$alpha = 0.75            # exponent for metabolic requirements plus swimming for predators (Ware et al 1978)
  
  # NOTE:this exponent = 0.75 for whole organism basal (sedentary) metabolic rate (see growth.v) 
  # from Peters (1983) and Brown et al. (2004) for detritivores
  param$alpha.h = 0.75
  
  
  # Growth conversion efficiency
  param$K.u = 0.15                # Gross growth conversion efficiency for organisms in the "predator" spectrum Ware (1978)
  param$K.v = 0.15                # Gross growth conversion efficiency for organisms in the "detritivore" spectrum
  param$K.h = 0.15                # Gross growth conversion efficiency for organisms in the "herbivore" spectrum
  param$K.d = 0.1                 # Gross growth conversion efficiency for detritus
  
  
  # Fish poo
  param$def.high = 0.4            # Fraction of ingested food that is defecated (Peters,1983)
  param$def.low  = 0.4            # Low = low quality (K) food, high = high quality (K) food
  
  param$K.a = 0.1                 # Gross growth conversion efficiency for algae?
  
  
  # Mortality and senescence
  param$mu0  = 0.2	              # Residual natural mortality
  param$k.sm = 0.1                # Constant for senescence mortality 
  param$xs   = 3                  # Size at sensenscence e
  param$p.s  = 0.3  			        # Exponent of senescence
  
  
  # Initial size spectra properties
  
  #param$ui0 = 10^(param$pp)      #Initial intercept of plankton size spectrum, from P.McCloghrie ERSEM output.
  #param$vi0 = sinking.rate*ui0   #Initial intercept of detritivore size spectrum  - same fraction as sinking rate
  
  param$r.plank      = -1.0       # Initial (fixed) slope of phyto-zooplankton spectrum
  param$pred.slope   = -1         # Initial slope of fish spectrum, same as that used in B&R
  param$herb.slope   = -1         # Initial slope of detrtivore spectrum
  param$invert.slope = -0.8       
  
 
  # Proportion of initial plankton intercept that depicts the abundance of larvae / eggs of:
  
  param$pred.prod   = 1           # predators
  param$herb.prod   = 1           # herbivorous fish
  param$invert.prod = 2           # invertebrates
  #param$sinking.rate     
  
  
  # Detritus
  param$W.init = 5                # Initial detritus density per scenario
  param$A.init = 25
  
  
  # Reproductive investment - where do these come from?
  param$r.u = 0.1
  param$r.h = 0.1
  param$r.v = 0.1
  param$r.a = 0.1
  param$r.d = 0.1
  
  
  #---------------------------------------------------------------------------------------
  ##### Parameters for numerical integration ####
  #---------------------------------------------------------------------------------------
  
  param$dx      = 0.1             # Size increment after discretization for integration (log body weight) 
  param$xmin    = -12             # Minimum log10 body size of plankton
  param$x1      = -1.5            # Minimum log10 body size of predators
  param$x1.det  = -4              # Minimum log10 body size in dynamics benthic detritivores
  param$x1.herb = -1.5            # Minimum log10 body size of herbivores
  param$xmax    = 3.5             # Maximum log10 body size of predators
  param$xmax2   = 3               # Maximum log10 body size before senescence kicks in (departure form linearity)
  
  
  # NEW - why?
  param$inv_max <- 3.5
  
  
  # Vector with size bins 
  param$x    = seq(param$xmin, param$xmax, param$dx)
  param$y    = param$x
  param$end  = length(param$x)
  
  
  #******NEW
  param$inv_end <- which(param$x == param$inv_max)
  
  
  # Time Steps
  param$tstepdays = 1.0                                         # Timestep in days
  param$tmaxyears = 200                                         # Maximum number of years for model run
  
  param$N  = as.integer(365.0*param$tmaxyears/param$tstepdays)	  # Total number of steps in a period
  param$dt = param$tmaxyears/param$N                              # Time step (rates per year)   
  
  
  # Organism locations
  param$ref = ((param$x1-param$xmin)/param$dx)+1                     # Location of smallest size consumer
  
  # param$phyto.ref=((-6-param$xmin)/param$dx)+1                      # Position in x of upper size of phytoplankton
  
  param$ref.det  = ((param$x1.det-param$xmin)/param$dx)+1            # Position in x of x1.det
  param$ref.herb = ((param$x1.herb-param$xmin)/param$dx)+1           # Position in x of x1.herb
  param$ref2     = ((param$xmax2-param$xmin)/param$dx)+1             # Position in x of xmax2 (largest benthic detritivores)
  
  
  # Setting data ranges for field measurement
  
   # Considering fish bigger than 5cm - or log10 weight > 0
   param$dat_start_5 = 0
   # Considering fish bigger than 10cm - or log10 weight > 1
   param$dat_start_10 = 1
   param$dat_end = 3.5 #4.5
  
   param$ref.dst_5 = ((param$dat_start_5-param$xmin)/param$dx)+1       # position in x of fish bigger than ~5cm (surveyable)
   param$ref.dst_10 = ((param$dat_start_5-param$xmin)/param$dx)+1      # position in x of fish bigger than ~10cm (surveyable)
  
   param$ref.den = ((param$dat_end-param$xmin)/param$dx)+1             # position in x of largest surveyed fish
  
   param$Fref = ((param$min.fishing.size-param$xmin)/param$dx)+1       # position in F vector corresponding to smallest size fished
   param$Fref2 = ((param$max.fishing.size-param$xmin)/param$dx)+1
  
  return(param)
}
