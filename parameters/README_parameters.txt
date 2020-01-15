1. parameters_default.txt

The first parameter column of 'parameters_default.txt' contains (still preliminary) values for Turrialba.
The second parameter column has the same values except that tree density (TREEDENS0) is set to zero.

2. parameters_BC_Turrialba_1[].txt

The two files with parameters to be calibrated define the prior distribution for the calibration parameters. The files have five columns.
- Column 1: Parameter name,
- Column 2: Minimum value of the parameter in the prior,
- Column 3: Mode,
- Column 4: Maximum,
- Column 5: Site numbers.
The site numbers in column 5 show for which sites the values in columns 2-4 are intended. "nSites" is the number of sites in the calibration (so if there is only one site, then nSites=1). Parameters can be generic or site-specific. For generic parameters, we can write "1:nSites" in column 5. For each site-specific parameter, there must be more than one line in the file, and in column 5 we then indicate to which site(s) the values in column 2-4 apply.

But what do we do with site-specific parameters for which the prior is the same in different sites although the posterior can be different)? Say we have two sites, and a site-specific parameter "P". Let's further assume that we want to have the same prior for P at both sites (although of course the posterior for P will differ). Say, the {min,mode,max} are {0,50,100}.Then we have to specify two lines for P, which are identical in all columns except the last:
  P 0 50 100 1
  P 0 50 100 2
