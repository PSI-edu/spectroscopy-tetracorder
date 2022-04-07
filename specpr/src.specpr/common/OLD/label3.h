#     /label3/ common:
#       iwch  : wavelength calibration shift in angstroms ( used oringinally
#         for wedge cvf spectrometer- no longer needed ).
#       alat  : latitude of observatory in radians.
#       ra    : right ascension in radians.
#       dec   : declination in radians.
#       ha    : hour angle in radians.
#       airmas: air mass of object.
#
      common /label3/ alat, ra, dec, ha, airmas,iwch
	real*4 alat, ra, dec, ha, airmas
      integer*4 iwch
