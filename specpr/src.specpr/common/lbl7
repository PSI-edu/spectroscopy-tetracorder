#     /lbl7/ common:
#       the following are device control variables:
#       idevc (1) : file w
#       idevc (2) : file v
#       idevc (3) : file d
#       idevc (4) : file s
#
#       idevc  = 0:  dummy
#       idevc  =-1:  disk 1
#       idevc  =-2:  disk 0
#       idevc  =-3:  mag tape 0
#       idevc  =-4:  mag tape 1
#
#       idvc  : used to indicate v file and w file device:
#         idvc (1,j) = mag tape, idvc (2,j) = disk.
#       idevc and idvc were originally used in the mit version of the program
#         but need to be phased out ( they're partially phased out now )
#         completely.  use the volume numbers for this control now.  (in
#             /lblvol/ common).
#       itrol (1) : wavelength file id: V W D U or Y in use
#       itrol (2) : wavelength record currently in use.
#       itrol (3) : channel, wavelength, energy plot control flag.
#       itl  : title option l selected by the user in math operations.
#
        common /lbl7/   idevc(4), idvc(2,4), itl, itrol(3)

        character*40    itl
	integer*4  idevc, idvc, itrol
