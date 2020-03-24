# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Example script for using bayesm
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Load bayesm package
require(bayesm) 

# Load cheese dataset
data(cheese)

# For each unit of observation (i)
# get the levels 
retailer=levels(cheese$RETAILER)

# Get number of units
nreg=length(retailer)

# Create matrix of potential covariates for our random effect
# Setting to 1 because our model assumes that there is a common mean across groups
Z=matrix(c(rep(1,nreg)),ncol=1)
nz=ncol(Z)

# Create both x and y data
# We want to run the following regression:
# 
# volume sold ~ constant + beta*price 
nvar=2 #         <- Number of RHS (IV) variables
regdata=NULL    #<- This will be a named list to store the data
                # Each element of the list is the observations (both IV and DV) for each i
for(reg in 1:nreg){
  y=log(cheese$VOLUME[cheese$RETAILER==retailer[reg]])        # Create LHS variable
  iota=c(rep(1,length(y)))                                    # Create constant
  X=cbind(iota,cheese$PRICE[cheese$RETAILER==retailer[reg]])  # Create RHS variables
  regdata[[reg]]=list(y=y,X=X)                                # For each unit i, add their observations to list regdata
}

# Create data for MCMC
Data=list(regdata=regdata,Z=Z)
Mcmc=list(R=1000,keep=1)

# Run linear hierarchical model
out=rhierLinearModel(Data=Data,Mcmc=Mcmc)

out$betadraw[,,-1]
