* util.h
* prototypes for the functions in util.a
* this file is part of FormCalc
* modified by Stephan Hessenberger 27.6.2018

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

#ifndef LOOP

#define LOOP(var,from,to,step) do var = from, to, step
#define ENDLOOP(var) enddo

#endif
