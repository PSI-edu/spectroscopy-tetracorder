
# integer*4 MAXCHNS # maximum number of channels per spectrum   # now set in specpr common/spmaxes
# integer*4 MAXTEXT # maximum number characters in a text record   # now set in specpr common/spmaxes

# parameter (MAXCHNS = 4852)   # now set in specpr common/spmaxes
# parameter (MAXTEXT = 19408)   # now set in specpr common/spmaxes

integer*4 NBND    # number of band depths allowed
integer*4 NMINL   # number of minerals allowed in the intimate mix computation

parameter (NBND    = 3)
parameter (NMINL   = 20)

integer*4 NAREF   # number of spectra for areal fractions
integer*4 NMMIX   # number of working arrays for molecular mix

parameter (NAREF = 2)
parameter (NMMIX = 5)

integer*4 MAXLAYERS # number of layers allowed in layered surfaces

parameter (MAXLAYERS = 3)
