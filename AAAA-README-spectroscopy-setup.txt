Procedure for installing the spectroscopy system on linux.

# This software was written by Roger N. Clark and colleagues and falls under the following license:
# 
# Copyright (c) 1975 - 2022, Roger N. Clark,
#                            Planetary Science Institute, PSI
#                            rclark@psi.edu, and colleagues named in the code.
# 
# All rights reserved.
# 
# GNU General Public License https://www.gnu.org/licenses/gpl-3.0.html
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#   Redistributions of the program must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
#
#   Neither Roger N. Clark, Planetary Science Institute, nor the names of
#   contributors to this software may be used to endorse or promote products
#   derived from this software without specific prior written permission.
#
#   Translation/recoding into other languages: the translation and source code
#   must be made available for free.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
#   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
#   BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
#   OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
#   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
#   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
#   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
#   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
#   THE POSSIBILITY OF SUCH DAMAGE.


Note, commands below should be executed in another window,  Cut and paste
the command and follow the directions with each one.  Be sure you understand what
each is doing, as the system may/will be modified.


## STEP 1  install linux should already be complete ####################################
######################################################

   This readme assumes linux is already installed and running.

## STEP 2  System basic setp for spectroscopy   ##################################################
######################################################

  Next execute the following command to set up the apectroscopy system:

    WARNING: read the script first to see with it does.  It modifies the system
             and installs software.
             It creates the user rclark  (you can do a global search and replace to your user name)

sudo ./AAA.INSTALL.spectroscopy-os-setup-linux.sh install |& tee AAA.INSTALL.spectroscopy-os-setup-linux.sh.out1

    check   AAA.INSTALL.spectroscopy-os-setup-linux.sh.out1   for errors

Set the ownership pn the /t1 and /sl1 directories to the person who will manage the data
in those directories.  For example, if the person's use name is rclark:

sudo chown rclark /t1/ /sl1/


## STEP 3  untar the source code and spectral data files  ########################################
############################################################

Log in with the account you will manage software with.  E.g. rclark.

Copy the sl1 spectral libraries into the /sl1 directory

    There are currently 3 spectral libraries:
        library06   library06.conv   rlib06
    and they go in the /sl1/usgs directory:

    /sl1/usgs/library06
    /sl1/usgs/library06.conv
    /sl1/usgs/rlib06

Copy the  specpr and tetracorder5.26 directories  into the source code directory, /src/local.

Copy the  tetracorder.cmds directory into /t1.

    You should noy have the directory
             /t1/tetracorder.cmds/tetracorder5.26e.cmds/

##### this completes setup of the operating system and spectral libraries.
#####     Next step is to compile the code.

