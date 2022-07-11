@mainpage Documentation for THDM_EWPOS

Introduction
============

THDM_EWPOS is a Fortran-program for the evaluation of electroweak precision observables in the  Two-Higgs-doublet-model (THDM).
It was developed during my PhD thesis at the [Max-Planck-Institute for Physic](https://www.mpp.mpg.de/) from December 2013 to January 2018.
The currently included precision observables are:

- the mass of the W boson as  calculated from the muon decay,
- the effective leptonic mixing angle at the Z pole,
- the total and partial widths of the Z boson.

The following higher-order corrections are incorporated:

- a complete one-loop calculation (the SM-like contribution as well as non-standard scalar contributions),
- the non-standard two-loop top-Yukawa corrections to the ρ parameter,
- the non-standard two-loop corrections to the ρ parameter from the scalar self-interaction,
- the known higher-order corrections from the Standard Model (SM).

The one-loop contributions were calculated using the Mathematica programs [FeynArts](http://www.feynarts.de/) and [FormCalc](http://www.feynarts.de/formcalc/),
 developed by [Thomas Hahn](https://wwwth.mpp.mpg.de/members/hahn/). For the numerical evaluation of the one-loop integrals, the library [LoopTools](http://www.feynarts.de/looptools/) is used.
 
The non-standard two-loop contributions were calculated using the Mathematica package [TwoCalc](https://arxiv.org/abs/hep-ph/9310358),
with the methods developed for a similar [calculation](https://arxiv.org/abs/1508.00562) for the program [FeynHiggs](https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).
For the evaluation of the required two-loop integrals, the Fortran implementation is also taken from [FeynHiggs](https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

The evaluation of the higher-order corrections from the SM use different simple parameterizations
for the [mass of the W boson](https://arxiv.org/abs/hep-ph/0311148), the [effective leptonic mixing angle](https://arxiv.org/abs/hep-ph/0608099) 
and the [decay widths of the Z boson](https://arxiv.org/abs/1401.2447). 
For the higher-order corrections from the SM for the mass of the W boson,
also a Fortran implementation from [Wolfgang Hollik](https://www.mpp.mpg.de/ueber-uns/organisation/direktoren/prof-dr-wolfgang-hollik) is used.

If you are publishing results obtained with THDM_EWPOS please also cite the following literature

- ["Two-loop corrections to the ρ parameter in Two-Higgs-Doublet Models"](https://arxiv.org/abs/1607.04610) by Stephan Hessenberger and Wolfgang Hollik,
- ["Two-Loop Corrections to Electroweak Precision Observables in Two-Higgs-Doublet-Models"](https://mediatum.ub.tum.de/1415546) by Stephan Hessenberger.
- ["Two-loop improved predictions for MW and sin2θeff in Two-Higgs-Doublet Models"](https://arxiv.org/abs/2207.03845) by Stephan Hessenberger and Wolfgang Hollik.

The whole structure of THDM_EWPOS is heavily inspired by the Fortran routines from [FormCalc](http://www.feynarts.de/formcalc/).
Especially the definition of the input parameter was adapted directly from these Fortran routines.

THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>. 

Installation
============

After unpacking the compressed tar archive THDM_EWPOS.tar.gz,
 there are a few adjustments that have to be made in the makefile,
  in order to get the program running:

  - the Fortran-compiler can be set by the variable FC;

  - the program requires the library [LoopTools](http://www.feynarts.de/looptools/),
   for the evaluation of the one-loop integrals. After succesfully
   installing the LoopTools library, the variables LTlib and LTinclude
   have to be set in the makefile:

    - LTinclude sets the option of the compiler,
   which looks for the LoopTools include file.

    - LTlib sets the option for the compiler,
   which links to the LoopTool library.

    For more details see the section "Using LoopTools with Fortran"
    in the LoopTools manual.

 After adjusting the makefile correctly, execute the command

 make

 to build the library for the subroutines (called libEWPOTHDM.a)
 and a few example programs. In order to use the different subroutines in your own program,
 you have to link it to libEWPOTHDM.a.

Documentation
=============

 A documentation of the program can be build with the help of the tool
 [doxygen](http://www.stack.nl/~dimitri/doxygen/index.html).
 The configuration file DOXYCONF sits in the main directory.
 After running the command

 doxygen DOXYCONF

 the documentation is available as a HTML output and a LaTeX output in the directory Doc.

 The HTML output sits in the directory

 THDM_EWPOS/Doc/html

 and can be accessed by opening the file index.html.

 The LaTeX output sits in the directory

 THDM_EWPOS/Doc/latex

 and can be complied into a pdf-file called refman.pdf by executing the command

 make

 in this directory.

 The documentation can also be generated by setting the variable DOXYCOMMAND
 in the makefile to the command which executes doxygen and running

 make DOC

 in the main directory.


 Content
 =======

 After unpacking the compressed tar archive THDM_EWPOS.tar.gz, the following files and
 directorys should be present:

 - THDM_EWPOS/src/: directory for the source files of the functions and subroutines

 - THDM_EWPOS/Examples/: directory for the source files of the example programs

 - THDM_EWPOS/include/: directory for the header files, which contain the
 definitions of the input parameters and of the counterterms.

 - THDM_EWPOS/doxyfiles/: contains images and library for the manual

 - THDM_EWPOS/DOXYCONF: configuration file for doxygen

 - THDM_EWPOS/makefile: Makefile for the compilation of the library and the example programs

 - THDM_EWPOS/README.md: instructions and mainpage of the documentation

