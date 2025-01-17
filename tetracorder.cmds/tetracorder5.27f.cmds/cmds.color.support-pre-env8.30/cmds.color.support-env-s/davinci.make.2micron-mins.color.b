#!/usr/bin/env -S davinci -f  
####!/usr/local/bin/davinci -f

###########################  was:  #!/usr/local/bin/davinci -f



# command line:
# prog    gray_base_image  prefix-file-name   [dual|nodual]


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



# goal: to show the following minerals
#       for global dust radiative forcing
# calcite (VNIR does this well, along with other carbonaies)
# gypsum (VNIR does this well)
# illite      (VNIR does this well, although includes muscovite)
# kaolinite  (VNIR does this well)
# smectite (Montmorillonite) (VNIR does this well)
# other (unspecified mineralogy) ((VNIR does many minerals well)
#       on 1-um maps:
# hematite  (VNIR does this well, including separating goethite from hematite). 


###### calcite aragonite              yellow
a=read(filename="group.2um/carbonate_aragonite.fd.gif")
        if (HasValue(a) ==0) {  # check that read worked
              printf ("read ERROR on results file\n")
              printf ("setting to zero\n")
              a=0
        }
	q=cat(a,a,axis=z)  # make into 3-band image
	a=cat(a,q,axis=z)  # make into 3-band image

b=read(filename="group.2um/carbonate_calcite.fd.gif")
c1=a+b    # calcite   yellow
	q1=cat(c1,c1,axis=z)  # make into 3-band image
	c1=cat(c1,q1,axis=z)  # make into 3-band image

# calcite mixtures                         gold
a=read(filename="group.2um/carbonate_calcite+0.2Ca-mont.fd.gif")
b=read(filename="group.2um/carbonate_calcite+0.2kaolwxl.fd.gif")
c=read(filename="group.2um/calcite+0.2Na-mont.fd.gif")
d=read(filename="group.2um/carbonate_calcite+0.3muscovite.fd.gif")
e=read(filename="group.2um/calcite+0.5Ca-mont.fd.gif")
f=read(filename="group.2um/carbonate_calcite0.7+kaol0.3.fd.gif")
g=read(filename="group.2um/carbonate_calcite.25+dolom.25+Ca-mont.5.fd.gif")
h=read(filename="group.2um/calcite.25+dolom.25+Na-mont.5.fd.gif")
i=read(filename="group.2um/carbonate_smectite_calcite.33+Ca-mont.67.fd.gif")
j=read(filename="group.2um/carbonate_calcite+dolomite.5.fd.gif")
c2=a+b+c+d+e+f+g+h+i+j
	q2=cat(c2,c2,axis=z)  # make into 3-band image
	c2=cat(c2,q2,axis=z)  # make into 3-band image

# other carbnonates                          orange
a=read(filename="group.2um/carbonate_dolo+.5ca-mont.fd.gif")
b=read(filename="group.2um/carbonate_dolomite.5+Na-mont.5.fd.gif")
c=read(filename="group.2um/carbonate_dolomite.fd.gif")
d=read(filename="group.2um/carbonate_magnesite.fd.gif")
e=read(filename="group.2um/carbonate_malachite.fd.gif")
f=read(filename="group.2um/carbonate_rhodochrosite.fd.gif")
g=read(filename="group.2um/carbonate_siderite.fd.gif")
h=read(filename="group.2um/carbonate_strontianite.fd.gif")
i=read(filename="group.2um/carbonate_azurite.fd.gif")
c3=a+b+c+d+e+f+g+h+i
	q3=cat(c3,c3,axis=z)  # make into 3-band image
	c3=cat(c3,q3,axis=z)  # make into 3-band image

###### gypsuum                                 red
a=read(filename="group.2um/sulfate_gypsum.fd.gif")
g1=a
	q1=cat(g1,g1,axis=z)  # make into 3-band image
	g1=cat(g1,q1,axis=z)  # make into 3-band image

# gypsum mixtures                               red-pink
a=read(filename="group.2um/sulfate-mix_gyp+jar+musc.amix.fd.gif")
b=read(filename="group.2um/sulfate-mix_gyp+jar+musc+dick.amix.fd.gif")
c=read(filename="group.2um/sulfate-mix_gypsum+jar+illite.intmix.fd.gif")
d=read(filename="group.2um/sulfate-mix_gypsum.trace.dust+debris-WTC01-28.fd.gif")
e=read(filename="group.2um/sulfate-mix_gypsum.trace.dust+debris-WTC01-2.fd.gif")
g2=a+b+c+d+e
	q2=cat(g2,g2,axis=z)  # make into 3-band image
	g2=cat(g2,q2,axis=z)  # make into 3-band image

##### illite/muscovite                      cyan
a=read(filename="group.2um/micagrp_illite.fd.gif")
b=read(filename="group.2um/micagrp_illite.gds4.fd.gif")
c=read(filename="group.2um/micagrp_illite.roscoelite.fd.gif")
d=read(filename="group.2um/smectite_ammonillsmec.fd.gif")
i1=a+b+c+d
	q1=cat(i1,i1,axis=z)  # make into 3-band image
	i1=cat(i1,q1,axis=z)  # make into 3-band image

# illitte/muscovite mixtures                        blue
a=read(filename="group.2um/micagrp_muscovite-low-Al.fd.gif")
b=read(filename="group.2um/micagrp_muscovite-med-Al.fd.gif")
c=read(filename="group.2um/micagrp_muscovite-medhigh-Al.fd.gif")
d=read(filename="group.2um/musc+gyp+jar+dick.amix.fd.gif")
e=read(filename="group.2um/musc+jarosite.intimat.fd.gif")
f=read(filename="group.2um/muscovite+chlorite.fd.gif")
g=read(filename="group.2um/micagrp_muscoviteFerich.fd.gif")
h=read(filename="group.2um/musc+pyroph.fd.gif")
i2=a+b+c+d+e+f+g+h
	q2=cat(i2,i2,axis=z)  # make into 3-band image
	i2=cat(i2,q2,axis=z)  # make into 3-band image

###### Kaolinit group                        lavender
a=read(filename="group.2um/kaolgrp_kaolinite_pxl.fd.gif")
b=read(filename="group.2um/kaolgrp_kaolinite_wxl.fd.gif")
c=read(filename="group.2um/kaolgrp_dickite.fd.gif")
d=read(filename="group.2um/kaolgrp_halloysite.fd.gif")
k1=a+b+c+d
	q1=cat(k1,k1,axis=z)  # make into 3-band image
	k1=cat(k1,q1,axis=z)  # make into 3-band image

# kaolinite mixtures                         magenta
a=read(filename="group.2um/kaol.75+alun.25.fd.gif")
b=read(filename="group.2um/kaol.75+pyroph.25.fd.gif")
c=read(filename="group.2um/kaolin.3+smect.7.fd.gif")
d=read(filename="group.2um/kaolin.5+muscov.medAl.fd.gif")
e=read(filename="group.2um/kaolin.5+muscov.medhighAl.fd.gif")
f=read(filename="group.2um/kaolin.5+smect.5.fd.gif")
g=read(filename="group.2um/kaolinite.3+.5+smectite.5+.7.added.fd.gif")
h=read(filename="group.2um/kaolinite+muscovite.added.fd.gif")
i=read(filename="group.2um/kaolin+musc.intimat.fd.gif")
k2=a+b+c+d+e+f+g+h+i
	q2=cat(k2,k2,axis=z)  # make into 3-band image
	k2=cat(k2,q2,axis=z)  # make into 3-band image

###### smectites                              green
a=read(filename="group.2um/smectite_beidellite_gds123.fd.gif")
b=read(filename="group.2um/smectite_beidellite_gds124.fd.gif")
c=read(filename="group.2um/smectite_montmorillonite_ca_swelling.fd.gif")
d=read(filename="group.2um/smectite_montmorillonite_fe_swelling.fd.gif")
e=read(filename="group.2um/smectite_montmorillonite_na_highswelling.fd.gif")
f=read(filename="group.2um/smectite_nontronite_swelling.fd.gif")
s1=a+b+c+d+e+f
	q1=cat(s1,s1,axis=z)  # make into 3-band image
	s1=cat(s1,q1,axis=z)  # make into 3-band image

# smectite mixtures                            army green
a=read(filename="group.2um/organic_benzene+montswy.fd.gif")
b=read(filename="group.2um/organic_drygrass+.17Na-mont.fd.gif")
s2=a+b
	q2=cat(s2,s2,axis=z)  # make into 3-band image
	s2=cat(s2,q2,axis=z)  # make into 3-band image


############### other ####################

##### chlorites, amphiboles, serpentines, talc       white
a=read(filename="group.2um/chlorite.fd.gif")
b=read(filename="group.2um/chlorite-skarn.fd.gif")
c=read(filename="group.2um/chlorite_thuringite.fd.gif")
d=read(filename="group.2um/chlorite_clinochlore.fd.gif")
e=read(filename="group.2um/amphibole.fd.gif")
f=read(filename="group.2um/chrysotile.coarse-fibrous.fd.gif")
g=read(filename="group.2um/chrysotile.fine-fibrous.fd.gif")
h=read(filename="group.2um/chrysotile.gypsum.wtc01-8.fd.gif")
i=read(filename="group.2um/chrysotile.med-fine-fibrous.fd.gif")
j=read(filename="group.2um/richterite.fd.gif")
k=read(filename="group.2um/tremolite.or.talc.fd.gif")
l=read(filename="group.2um/talc+calcite.parkcity.fd.gif")
m=read(filename="group.2um/talc+carbonate.parkcity.fd.gif")
n=read(filename="group.2um/talc.crsgrnd.fd.gif")
o=read(filename="group.2um/talc.fngrnd.fd.gif")
o1=a+b+c+d+e+f+g+h+i+j+k+l+m+n+o
	q1=cat(o1,o1,axis=z)  # make into 3-band image
	o1=cat(o1,q1,axis=z)  # make into 3-band image

##### zeolites                                         flesh
a=read(filename="group.2um/zeolite_natrolite.fd.gif")
b=read(filename="group.2um/zeolite_analcime.fd.gif")
z1=a+b
	q1=cat(z1,z1,axis=z)  # make into 3-band image
	z1=cat(z1,q1,axis=z)  # make into 3-band image

# other sulfates                                        deep pink
a=read(filename="group.2um/alunite.33+kaol.33+musc.33.fd.gif")
b=read(filename="group.2um/alunite.5+kaol.5.fd.gif")
c=read(filename="group.2um/alunite.5+musc.5.fd.gif")
d=read(filename="group.2um/sulfate_alunite_k.all.fd.gif")
e=read(filename="group.2um/sulfate_alunite_na.all.fd.gif")
f=read(filename="group.2um/alunite+kaolinite.muscovite.added.fd.gif")
g=read(filename="group.2um/alunite+musc+pyroph.fd.gif")
o2=a+b+c+d+e+f+g
	q2=cat(o2,o2,axis=z)  # make into 3-band image
	o2=cat(o2,q2,axis=z)  # make into 3-band image


gbase=read(filename=$1)
if (HasValue(gbase) ==0) {                   # check that read worked
      printf ("read ERROR on file: %s\n",$1)
      printf ("^GEXIT 1\n")
      exit(1)
}

# create RGB color multiplier variables
c1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
c2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
c3rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
g1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
g2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
i1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
i2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
k1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
k2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
s1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
s2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
o1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
z1rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)
o2rgb =  create(1,1,3, format=FLOAT, start=0.0, step=0.0)

# /info/COLORS.screen.to.printers
#
#      "color"         r    g    b
#    1 red            255   60    0   (255   50    0 better)
#    2 green           60  255    0
#    3 blue             0  105  255
#   
#    4 yellow         255  255    0
#    5 magenta        205   60  255
#    6 cyan             0  255  255
#  
#    7 army green     150  165    0   (160  175    0 better?)
#    8 salmon         255  170  170
#    9 flesh          255  200  155
#  
#   10 orange         255  110    0   (255  140    0 better?)
#   11 deep pink      235    0  180
#   12 yellow green   190  255    0
#
#   13 lavender       185  120  255
#   14 sky blue       140  210  255
#   15 pale sea green 110  255  190
#  
#   16 white          255  255  255
#   17 red-pink       255  125  125
#   18 gold           255  213    0
#
#   19 mud yellow     186  165    0
#   20 light pink     255  211  255
#

# c1    calcite              yellow
# c2    calcite mix          gold
# c3    other carbonates     orange

# g1    gypsum               red
# g2    gypsum mixtures      red-pink

# i1    illite               cyan
# i2    illite mixtures      blue

# k1    kaolinite group      lavender
# k2    kaolinite mixtures   magenta

# s1    smectites            green
# s2    smectite mixtures    army green

# o1    chlorites, etc       white
# z1    zeolites             flesh
# o2    other sulfates       deep pink

# Colors:
#              Red                  Green                Blue

c1rgb[1,1,1] =  255. ; c1rgb[1,1,2]=   255. ;  c1rgb[1,1,3]=   0.   # yellow
c2rgb[1,1,1] =  255. ; c2rgb[1,1,2]=   213. ;  c2rgb[1,1,3]=   0.   # gold
c3rgb[1,1,1] =  255. ; c3rgb[1,1,2]=   110. ;  c3rgb[1,1,3]=   0.   # orange

g1rgb[1,1,1] =  255. ; g1rgb[1,1,2]=    60. ;  g1rgb[1,1,3]=   0.   # red
g2rgb[1,1,1] =  255. ; g2rgb[1,1,2]=   125. ;  g2rgb[1,1,3]= 125.   # red pink

i1rgb[1,1,1] =    0. ; i1rgb[1,1,2]=   255. ;  i1rgb[1,1,3]= 255.   # cyan
i2rgb[1,1,1] =    0. ; i2rgb[1,1,2]=   105. ;  i2rgb[1,1,3]= 255.   # blue

k1rgb[1,1,1] =  185. ; k1rgb[1,1,2]=   120. ;  k1rgb[1,1,3]= 255.   # lavender
k2rgb[1,1,1] =  255. ; k2rgb[1,1,2]=    60. ;  k2rgb[1,1,3]= 255.   # magenta

s1rgb[1,1,1] =   60. ; s1rgb[1,1,2]=   255. ;  s1rgb[1,1,3]=   0.   # green
s2rgb[1,1,1] =  160. ; s2rgb[1,1,2]=   175. ;  s2rgb[1,1,3]=   0.   # army green

o1rgb[1,1,1] =  255. ; o1rgb[1,1,2]=   255. ;  o1rgb[1,1,3]= 255.   # white
z1rgb[1,1,1] =  255. ; z1rgb[1,1,2]=   200. ;  z1rgb[1,1,3]= 155.   # flesh
o2rgb[1,1,1] =  235. ; o2rgb[1,1,2]=     0. ;  o2rgb[1,1,3]= 180.   # deep pink


xaz=float(a*0)

xaz= xaz + c1*(c1rgb/255.)
xaz= xaz + c2*(c2rgb/255.)
xaz= xaz + c3*(c3rgb/255.)
xaz= xaz + g1*(g1rgb/255.)
xaz= xaz + g2*(g2rgb/255.)
xaz= xaz + i1*(i1rgb/255.)
xaz= xaz + i2*(i2rgb/255.)
xaz= xaz + k1*(k1rgb/255.)
xaz= xaz + k2*(k2rgb/255.)
xaz= xaz + s1*(s1rgb/255.)
xaz= xaz + s2*(s2rgb/255.)
xaz= xaz + o1*(o1rgb/255.)
xaz= xaz + z1*(z1rgb/255.)
xaz= xaz + o2*(o2rgb/255.)

xout=byte(xaz+0.5)
xout = bip(xout)

gbasedim=dim(gbase)
x=gbasedim[1,,]

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
