* const.h
* some constants
* this file is part of FeynHiggs
* modified by Stephan Hessenberger 10.12.2015

c This file is used here for the program THDM_EWPOS.
c The original file can be found in the program FeynHiggs (https://wwwth.mpp.mpg.de/members/heinemey/feynhiggs/cFeynHiggs.html).


	RealType bogus, pi, degree, sqrt2, hbar_c2, GeV_cm
	parameter (pi = 3.1415926535897932384626433832795029D0)
	parameter (degree = pi/180D0)
	parameter (sqrt2 = 1.41421356237309504880168872421D0)
* Parameters for the evaluation of the radiative functions
* for the QED/QCD corrections of the partial Widths

        RealType Zeta3, Cgamma2, C02, C03, C04, nq
        parameter (nq=5D0)
        parameter (Zeta3=1.202056903159594D0)
        parameter (Cgamma2=-55D0/6D0+20D0/3D0*Zeta3)
        parameter (C02=365D0/24D0 - 11D0*Zeta3
     &  +(-11D0/12D0 +2D0/3D0*Zeta3)*nq)
        parameter (C03=-6.63694D0-1.20013D0*nq
     &  - 0.005178D0*nq**2)
        parameter (C04= -156.61D0+18.77D0*nq
     &   -0.7974*nq**2 + 0.0215*nq**3 )

	parameter (hbar_c2 = 3.8937966D8)
*	  = hbar c^2 in picobarn

	parameter (GeV_cm = 1.98D-14)
*	  = GeV^-1 in cm

	parameter (bogus = -1D123)
*	  some weird number likely to noticeably distort the final result;
*	  used for initializing arrays to check that all components
*	  have been calculated

	ComplexType cI
	parameter (cI = (0D0, 1D0))


	integer  Finite
	common /renorm/  Finite
