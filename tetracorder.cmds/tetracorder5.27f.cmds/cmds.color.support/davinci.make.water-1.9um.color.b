#!/usr/bin/env -S davinci -q -f  
####!/usr/local/bin/davinci -q -f

###########################  was:  #!/usr/local/bin/davinci -q -f



# A master color set.  Just read in the files, assign the colors
# and write the results.    R. Clark 4/16/2015 - 11/16/2015

# command line:
# prog    gray_base_image  prefix-file-name   [dual|nodual]

prog="davinci.make.water-1.9um.color.b"     # CHANGE THIS if program name changes

####################  Begin static: no need to change inything in this section ################

if ($argc < 2) {

	printf("Insufficient input\n")
	printf("%s input_gray_image  outputcolorimage\n", prog)
	printf("exit 1\n")
	exit(1)
}


dual    = 1     # default color + gray image 
dualmax = 2000  # no dual if width is greater than this

if ($argc >= 3) {

	if ( "$3" == "nodual" ) {
		dual = 0
	}
	if ( "$3" == "dual" ) {
		dual = 1
		dualmax = 999999 
	}
}

# read base gray image

gbase=read(filename=$1)
if (HasValue(gbase) ==0) {                   # check that read worked
      printf ("read ERROR on file: %s\n",$1)
      printf ("^GEXIT 1\n")
      exit(1)
}

# execute this program in the same directory asthe source code below
source ("cmds.color.support/davinci.master.colors")

# available colors:
# c_red     c_green          c_blue       c_yellow        c_magenta       c_cyan          c_armygreen  
# c_salmon  c_flesh          c_orange     c_deeppink      c_yellowgreen   c_lavender  
# c_skyblue c_paleseagreen   c_white      c_redpink       c_gold          c_mudyellow     c_lightpink

# a = first input image, 3-band color, xcolor = the color output image, RGB float
# colorout = the color output image, 3-byte RGB


define read_an_image(1,1) {

	# read a gray-scale image and convert to 3-bands

	afile = $1   # file to read
	printf ("reading file %s\n", afile)
	aaa=read(filename=afile)
	if (HasValue(aaa) ==0) {  # check that read worked
		printf ("read ERROR on file: %s\n",afile)
		printf ("setting to zero\n")
		aaa=0
	}
	qqq = cat(aaa,aaa,axis=z)  # make into 2-band image
	aaa = cat(aaa,qqq,axis=z)  # make into 3-band image

	return (aaa)

}  # end define



###################   begin custom section ##########################################

# in this directory but not used, group 0:
# group.1.9um/organic_dry_long_grass-thick.fd.gif
# group.1.9um/organic_green_plastic_tarp.fd.gif
# group.1.9um/organic_white_pvc_pipe.fd.gif
# group.1.9um/snow.and.ice.fd.gif
# group.1.9um/snow.melting16+0.5veg.fd.gif
# group.1.9um/snow.melting1a+0.5veg.fd.gif
# group.1.9um/snow.melting.1a.fd.gif
# group.1.9um/snow.melting.3.fd.gif
# group.1.9um/snow.melting.8.fd.gif
# group.1.9um/snow.melting9+0.5veg.fd.gif
# group.1.9um/snow.slush.16.fd.gif
# group.1.9um/snow.slush.9.fd.gif
# group.1.9um/vegetation1.fd.gif
# group.1.9um/vegetation.dry+green.fd.gif
# group.1.9um/water.high.chlorophyll.fd.gif
# group.1.9um/water.low.chlorophyll.fd.gif
# group.1.9um/water+mont0.5gpl.fd.gif
# group.1.9um/water+mont16.5gpl.fd.gif
# group.1.9um/water+mont1.67gpl.fd.gif
# group.1.9um/water+mont5.01gpl.fd.gif
# group.1.9um/water.red.algae.fd.gif

# 21 available colors:
# c_red     c_green          c_blue       c_yellow        c_magenta       c_cyan          c_armygreen  
# c_salmon  c_flesh          c_orange     c_deeppink      c_yellowgreen   c_lavender  
# c_skyblue c_paleseagreen   c_white      c_redpink       c_gold          c_mudyellow     c_lightpink

# first image:

# c_white
a = read_an_image(" group.1.9um/h20_ice_2.02um-77k-a.fd.gif")

	# set up output image xcolor:
	xcolor=float(a*0)
	xdim=dim(a)

	# now add the colors

c=c_white
xcolor= xcolor + a*( c /255.)   #   ice


c=c_blue

a = read_an_image("group.1.9um/water_1.896_beryl.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_red

a = read_an_image("group.1.9um/water_1.924_perchlorate-na.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.918_perchlorate-mg.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_lavender

a = read_an_image("group.1.9um/water_1.902_silica-opal.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.923_chert.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_magenta

a = read_an_image("group.1.9um/water_1.903_smectite-saponite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.906_montmor-na.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.907_smectite-nontronite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.908_smectite-beidell.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.909_mont-saz.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_skyblue

a = read_an_image("group.1.9um/water_1.908_phyllo-illite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_cyan

a = read_an_image("group.1.9um/water_1.910_stonwplaya.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.931_narrow-cheatgrass.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.943_brick.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_broad_1.927_veg.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_mudyellow

a = read_an_image("group.1.9um/water_1.910_halloysite.fd.gif")
xcolor= xcolor + a*( c /255.)

# * = used
# c_red*     c_green*         c_blue*      c_yellow*       c_magenta*      c_cyan*         c_armygreen*
# c_salmon   c_flesh          c_orange*    c_deeppink      c_yellowgreen   c_lavender* 
# c_skyblue* c_paleseagreen   c_white*     c_redpink*      c_gold*         c_mudyellow*    c_lightpink

c=c_green

a = read_an_image("group.1.9um/water_1.940_zeolite-natrolite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.910_zeolite-clinopt.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.910_zeolite-mordonite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.912_zeolite-analcime.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.916_nesosil-sillim.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.917_zeolite-heuland.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.941_zeolite-natrolite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.941_zeolite-scolecite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_yellow

a = read_an_image("group.1.9um/water_1.920_sulfate-bassanite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.929_sulfate-epsomite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.932_sulfate-eusgterite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_gold

a = read_an_image("group.1.9um/water_1.942_sulfate-schwert.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.943_sulfate-gypsum.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.944_sulfate-polyhalite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.946_sulfate-copiapite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.951_sulfate-bloedite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.960_sulfate-butler.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.964_sulfate-feso4.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.971_sulfate-kainite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_1.981_sulfate-coquimb.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_orange

a = read_an_image("group.1.9um/water_1.991_sulfate-syngite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_2.060_sulfate_szomoln.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_2.073_sulfate-kieserite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_2.093_sulfate_szomoln.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.1.9um/water_2.130_sulfate_kieserite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_flesh

a = read_an_image("group.1.9um/water_2.122_nh4-buddingt.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_armygreen

a = read_an_image("group.1.9um/water_2.136_nh4-mascagnite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_redpink

a = read_an_image("group.1.9um/water_1.980_salt-carnallite.fd.gif")
xcolor= xcolor + a*( c /255.)

########### end custom, no need to change anything below this line ###############################

xout=byte(xcolor+0.5)   # output color image, byte
xout = bip(xout)
xdim=dim(xout)
xdimx=xdim[1,,]      # color image width

fout=$2

foute="color.results-envi/" + fout

write (xout, filename=foute, type=vicar)


gbasedim=dim(gbase)
x=gbasedim[1,,]      # base image width

if (x > dualmax) {
	dual = 0
}

if ( dual == 1 ) {
	
	gbase = bip(gbase)
	xout = cat (xout, gbase, axis=x)
}


adim=dim(xout)
if ( adim[1,,] > 5 || adim[2,,] > 5 ) {
	printf ("writing %s\n", fout)
	write (xout, filename=fout, type=png)
} else {
	printf ("not writing %s\n", fout)
	printf ("    image size < 6 pixels\n")
}
