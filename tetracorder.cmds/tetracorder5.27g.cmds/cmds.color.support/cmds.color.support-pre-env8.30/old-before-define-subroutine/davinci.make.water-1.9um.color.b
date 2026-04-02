#!/usr/local/bin/davinci -f

# A master color set.  Just read in the files, assign the colors
# and write the results.    R. Clark 4/16/2015 - 11/16/2015

# command line:
# prog    gray_base_image  prefix-file-name   [dual|nodual]

dual=0  # no dual

prog="davinci.make.water-1.9um.color.b"     # CHANGE THIS if program name changes

####################  Begin static: no need to change inything in this section ################

if ($argc < 2) {

	printf("Insufficient input\n")
	printf("%s input_gray_image  outputcolorimage\n", prog)
	printf("exit 1\n")
	exit(1)
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
f1=" group.1.9um/h20_ice_2.02um-77k-a.fd.gif"
a=read(filename=f1)

	if (HasValue(a) ==0) {  # check that read worked
	      printf ("read ERROR on file: %s\n",f1)
              printf ("setting to zero\n")
	      a=0
	}
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image

	# set up output image xcolor:
	xcolor=float(a*0)
	xdim=dim(a)

	# now add the colors

c=c_white
xcolor= xcolor + a*( c /255.)   #   ice

c=c_blue
a=read(filename="group.1.9um/water_1.896_beryl.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_red
a=read(filename="group.1.9um/water_1.924_perchlorate-na.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.918_perchlorate-mg.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_lavender

a=read(filename="group.1.9um/water_1.902_silica-opal.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.923_chert.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_magenta
a=read(filename="group.1.9um/water_1.903_smectite-saponite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.906_montmor-na.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.907_smectite-nontronite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.908_smectite-beidell.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.909_mont-saz.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_skyblue

a=read(filename="group.1.9um/water_1.908_phyllo-illite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_cyan

a=read(filename="group.1.9um/water_1.910_stonwplaya.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.931_narrow-cheatgrass.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.943_brick.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_broad_1.927_veg.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_mudyellow

a=read(filename="group.1.9um/water_1.910_halloysite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

# * = used
# c_red*     c_green*         c_blue*      c_yellow*       c_magenta*      c_cyan*         c_armygreen*
# c_salmon   c_flesh          c_orange*    c_deeppink      c_yellowgreen   c_lavender* 
# c_skyblue* c_paleseagreen   c_white*     c_redpink*      c_gold*         c_mudyellow*    c_lightpink

c=c_green

a=read(filename="group.1.9um/water_1.940_zeolite-natrolite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.910_zeolite-clinopt.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.910_zeolite-mordonite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.912_zeolite-analcime.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.916_nesosil-sillim.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.917_zeolite-heuland.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.941_zeolite-natrolite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.941_zeolite-scolecite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_yellow

a=read(filename="group.1.9um/water_1.920_sulfate-bassanite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.929_sulfate-epsomite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.932_sulfate-eusgterite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_gold

a=read(filename="group.1.9um/water_1.942_sulfate-schwert.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.943_sulfate-gypsum.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.944_sulfate-polyhalite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.946_sulfate-copiapite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.951_sulfate-bloedite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.960_sulfate-butler.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.964_sulfate-feso4.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.971_sulfate-kainite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_1.981_sulfate-coquimb.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_orange

a=read(filename="group.1.9um/water_1.991_sulfate-syngite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_2.060_sulfate_szomoln.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_2.073_sulfate-kieserite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_2.093_sulfate_szomoln.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

a=read(filename="group.1.9um/water_2.130_sulfate_kieserite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_flesh

a=read(filename="group.1.9um/water_2.122_nh4-buddingt.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_armygreen

a=read(filename="group.1.9um/water_2.136_nh4-mascagnite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

c=c_redpink

a=read(filename="group.1.9um/water_1.980_salt-carnallite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image
xcolor= xcolor + a*( c /255.)

########### end custom, no need to change anything below this line ###############################

xout=byte(xcolor+0.5)   # output color image, byte
xdim=dim(xout)
xdimx=xdim[1,,]      # color image width

gbasedim=dim(gbase)
x=gbasedim[1,,]      # base image width

if (x > 6000) {
	dual = 0
}
if ( dual == 1 ) {
	
	xout = cat (xout, gbase, axis=x)
}


fout=$2

printf ("writing %s\n", fout)
write (xout, filename=fout, type=png)

