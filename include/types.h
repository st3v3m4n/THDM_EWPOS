* types.h
* real-based type declarations
* this file is part of FeynHiggs
* last modified 3 Jan 15 th

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).

#ifndef TYPES_H
#define TYPES_H

#define RealType double precision
#define ComplexType double complex
#define Re DBLE
#define Im DIMAG
#define Conjugate DCONJG
#define ToComplex DCMPLX

#define Sq(c) Re((c)*Conjugate(c))
#define Sqrtc(c) sqrt(ToComplex(c))
#define Logc(c) log(ToComplex(c))

#endif
