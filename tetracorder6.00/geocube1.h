
#ccc
#ccc Geologic origins cube (11 channels), same pixel width as the image cube
#ccc
#ccc   R. Clark, January, February, 2026
#ccc

	common /gcub1/  geocub, geocubi2, geofilname

	real*4     geocub(maxpix,geochans)     # geologic origins cube
	integer*2  geocubi2(maxpix)            # geologic origins cube 16-bit integer for output 1 line
						# 1.0 scaled 0 to 1,000
	character*40 geofilname

# geocube channels are (2/2026):
#   GO1   Lacustrine/Marine     
#   GO2   Evaporitic / playa   
#   GO3   Metamorphic  
#   GO4   Hydrothermal  
#   GO5   Pedogenic /diagenic /weathering (include secondary coatings)  
#   GO6   Igneous (include carbonatites) 
#   GO7   Biogenic, Includes Vegetation
#   G08   Organic, not necessarily biogenic
#   G09   Man-Made
#   G10   Water Liquid or Ice
#   G11   Fire /Thermal Emission
#
# channels 1 - 6 are geologic origins, 7 - 11 other categories

