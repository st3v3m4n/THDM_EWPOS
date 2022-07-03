c> Header-file for SM Parameters
c> Adapted from FormCalc

c This file taken from FormCalc and used here for the program THDM_EWPOS.
c The original file can be found in the program FormCalc (http://www.feynarts.de/formcalc/).


#include "types.h"
#include "looptools.h"
#include "lt.h"
#include "util.h"


      RealType CKMlambda, CKMA, CKMrhobar, CKMetabar
      RealType  MZ, MZ2, MZFW, MZFW2
      RealType GF, Alfa, Alfa2, AlfaMZ, AlfasMZ, LambdaQCD, DelAlfaMZ
      RealType ME, ME2, MM, MM2, ML, ML2
      RealType MU, MU2, MC, MC2, MT, MT2
      RealType MD, MD2, MS, MS2, MB, MB2
      ComplexType CKM(3,3)
      RealType Mf(4,3), Mf2(4,3)
      RealType EL
      RealType Qf(1:9), fmass2(1:9), I3f(1:9), NCf(1:9)
      RealType MW, MW2, CW, CW2, SW, SW2
      RealType Mh0, Mh02
      RealType MWSMho
*   AlfasMT denotes alfas(mt)
      RealType AlfasMT


      COMMON /SM_PARA/ CKMlambda, CKMA, CKMrhobar, CKMetabar
      COMMON /SM_PARA/  MZ, MZ2, MZFW, MZFW2
      COMMON /SM_PARA/ GF, Alfa, Alfa2, AlfaMZ, AlfasMZ, LambdaQCD, DelAlfaMZ
      COMMON /SM_PARA/ ME, ME2, MM, MM2, ML, ML2
      COMMON /SM_PARA/ MU, MU2, MC, MC2, MT, MT2
      COMMON /SM_PARA/ MD, MD2, MS, MS2, MB, MB2
      COMMON /SM_PARA/ CKM, Qf, fmass2, I3f, NCf
      COMMON /SM_PARA/ Mf, Mf2
      COMMON /SM_PARA/ Mh0, Mh02
      COMMON /SM_PARA/ EL, AlfasMT
      COMMON /SM_PARA/ MW, MW2, SW, SW2, CW, CW2
      COMMON /SM_PARA/ MWSMho


#ifndef CKMC
#define CKMC(i,j) Conjugate(CKM(i,j))
#endif

#ifndef NO_CKM
#define NO_CKM
#endif
