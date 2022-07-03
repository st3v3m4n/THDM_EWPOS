c This File loads all necessary declarations
c It is taken taken from FormCalc and used here for the program THDM_EWPOS.
c The original file can be found in the program FormCalc (http://www.feynarts.de/formcalc/).

#include "types.h"
#include "lt.h"
#include "looptools.h"
#include "util.h"
#include "paraSM.h"



c#ifdef NO_CKM

	CKM(1,1) = 1
	CKM(2,2) = 1
	CKM(3,3) = 1
	CKM(2,1) = 0
	CKM(3,1) = 0
	CKM(1,2) = 0
	CKM(3,2) = 0
	CKM(1,3) = 0
	CKM(2,3) = 0

c#else

c	RealType s12, s23, s13
c	RealType c12, c23, c13
c	ComplexType s13c

c	s12 = CKMlambda
c	c12 = sqrt((1 - s12)*(1 + s12))

c	s23 = s12**2*CKMA
c	c23 = sqrt((1 - s23)*(1 + s23))

c	s13c = s12*s23*ToComplex(CKMrhobar, CKMetabar)
c	s13c = c23/c12*s13c/(1 - s23/s12*s13c)
c	s13 = abs(s13c)
c	c13 = sqrt((1 - s13)*(1 + s13))

c	CKM(1,1) = c12*c13
c	CKM(2,1) = -s12*c23 - c12*s23*s13c
c	CKM(3,1) = s12*s23 - c12*c23*s13c
c	CKM(1,2) = s12*c13
c	CKM(2,2) = c12*c23 - s12*s23*s13c
c	CKM(3,2) = -c12*s23 - s12*c23*s13c
c	CKM(1,3) = Conjugate(s13c)
c	CKM(2,3) = s23*c13
c	CKM(3,3) = c23*c13

c#endif

	Mf(1,1) = 0
	Mf(2,1) = ME
	Mf(3,1) = MU
	Mf(4,1) = MD
	Mf(1,2) = 0
	Mf(2,2) = MM
	Mf(3,2) = MC
	Mf(4,2) = MS
	Mf(1,3) = 0
	Mf(2,3) = ML
	Mf(3,3) = MT
	Mf(4,3) = MB

	Mf2(1,1) = 0
	Mf2(2,1) = ME2
	Mf2(3,1) = MU2
	Mf2(4,1) = MD2
	Mf2(1,2) = 0
	Mf2(2,2) = MM2
	Mf2(3,2) = MC2
	Mf2(4,2) = MS2
	Mf2(1,3) = 0
	Mf2(2,3) = ML2
	Mf2(3,3) = MT2
	Mf2(4,3) = MB2

	EL = sqrt(4*pi*Alfa)

c Fermion-charges, Isospin color-factor and squarred fermionmasses
c defined for the radiative corrections
c to the Z decays
c The indices in the three fields correspond to:
c	1	-	neutrino
c	2	-	electron
c	3	-	muon
c	4	-	tau lepton
c	5	-	up quark
c	6	-	down quark
c	7	-	charm quark
c	8	-	strange quark
c	9	-	bottom quark

      Qf(1)=0D0
      Qf(2)=-1D0
      Qf(3)=-1D0
      Qf(4)=-1D0
      Qf(5)=2D0/3D0
      Qf(6)=-1D0/3D0
      Qf(7)=2D0/3D0
      Qf(8)=-1D0/3D0
      Qf(9)=-1D0/3D0

      I3f(1)=1D0/2D0
      I3f(2)=-1D0/2D0
      I3f(3)=-1D0/2D0
      I3f(4)=-1D0/2D0
      I3f(5)=1D0/2D0
      I3f(6)=-1D0/2D0
      I3f(7)=1D0/2D0
      I3f(8)=-1D0/2D0
      I3f(9)=-1D0/2D0

      NCf(1)=1D0
      NCf(2)=1D0
      NCf(3)=1D0
      NCf(4)=1D0
      NCf(5)=3D0
      NCf(5)=3D0
      NCf(6)=3D0
      NCf(7)=3D0
      NCf(8)=3D0
      NCf(9)=3D0


C for the quark masses the running masses at s=M_Z^2 should be used
c The remaining quark mass are neglected

      fmass2(1)=0D0
      fmass2(2)=ME2
      fmass2(3)=MM2
      fmass2(4)=ML2
      fmass2(5)=0D0
      fmass2(6)=0D0
      fmass2(8)=0D0

c Results for c- and b- are obtained by Cyril Pietsch with the programm RunDec.

      fmass2(7)=0.435D0**2
      fmass2(9)=2.859D0**2

c Results for c- and b-quarks extracted from Zfitter

c      fmass2(7)=0.56211049663458046**2
c      fmass2(9)=2.8169784636196455**2
c      fmass2(7)=MC2
c      fmass2(9)=MB2




c	Mh0=126D0
c        Mh02=Mh0**2
