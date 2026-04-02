#!/usr/local/bin/davinci -f
#####!/usr/bin/env -S davinci -f  

###########################  was:  #!/usr/local/bin/davinci -f



# command line:
# prog    gray_base_image  prefix-file-name   [dual|nodual]

prog="davinci.make.prehnite-chloriteserp-perchlorate.b"

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

# A master color set.  Just read in the files, assign the colors
# and write the results.    R. Clark 4/16/2015 - 11/16/2015
# execute this program in the same directory asthe source code below
source ("cmds.color.support/davinci.master.colors")


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

# available colors:
# c_red     c_green          c_blue       c_yellow        c_magenta       c_cyan          c_armygreen  
# c_salmon  c_flesh          c_orange     c_deeppink      c_yellowgreen   c_lavender      c_bluegreen
# c_skyblue c_paleseagreen   c_white      c_redpink       c_gold          c_mudyellow     c_lightpink
# c_purple

# a purple/magenta to red 12 spectral color sequence:
#
# c_magenta c_purple c_blue c_skyblue c_cyan c_green c_armygreen c_yellow c_gold c_salmon c_orange c_red

# a = first input image, 3-band color, xcolor = the color output image, RGB float
# colorout = the color output image, 3-byte RGB

###################   begin custom section ##########################################


# first image:

a = read_an_image("group.2um/prehnite.fd.gif")

        # set up output image xcolor:
        xcolor=float(a*0)
        xdim=dim(a)

        # now add the colors

c=c_red
xcolor= xcolor + a*( c /255.)

c=c_orange

a = read_an_image("group.2um/prehnite+.50chlorite.fd.gif")
xcolor= xcolor + a*( c /255.)

c=c_yellow

a = read_an_image("group.2um/prehnite+.67chlorite.fd.gif")
xcolor= xcolor + a*( c /255.)

c=c_green

a = read_an_image("group.2um/prehnite+.75chlorite.fd.gif")
xcolor= xcolor + a*( c /255.)

# a purple/magenta to red 12 spectral color sequence:
#
# c_magenta c_purple c_blue c_skyblue c_cyan c_green c_armygreen c_yellow c_gold c_salmon c_orange c_red

c=c_cyan

a = read_an_image("group.2um/chlorite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_skyblue

a = read_an_image("group.2um/chlorite-skarn.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_blue

a = read_an_image("group.2um/chlorite_thuringite.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_clinochlore.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_clinochlore.nmnh83369.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_clinochlore.fe.gds157.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_clinochlore.fe.sc-cca-1.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_cookeite-car-1.a.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/chlorite_cookeite-car-1.c.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_bluegreen

a = read_an_image("group.2um/serpentine_chrysotile.coarse-fibrous.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/serpentine_chrysotile.fine-fibrous.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/serpentine_chrysotile.med-fine-fibrous.fd.gif")
xcolor= xcolor + a*( c /255.)

a = read_an_image("group.2um/serpentine_cronstedtite.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_magenta

a = read_an_image("group.2um/perchlorate_mg.fd.gif")
xcolor= xcolor + a*( c /255.)


c=c_purple

a = read_an_image("group.2um/perchlorate_na.fd.gif")
xcolor= xcolor + a*( c /255.)

########### end custom, no need to change anything below this line ###############################

xout=byte(xcolor+0.5)   # output color image, byte
xout = bip(xout)
xdim=dim(xout)
xdimx=xdim[1,,]      # color image width

gbasedim=dim(gbase)
x=gbasedim[1,,]      # base image width

if (x > dualmax) {
	dual = 0
}

if ( dual == 1 ) {
	
	gbase = bip(gbase)
	xout = cat (xout, gbase, axis=x)
}


fout=$2

adim=dim(xout)
if ( adim[1,,] > 5 || adim[2,,] > 5 ) {
	printf ("writing %s\n", fout)
	write (xout, filename=fout, type=png)
} else {
	printf ("not writing %s\n", fout)
	printf ("    image size < 6 pixels\n")
}
