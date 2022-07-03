c This file is part of THDM_EWPOS.
c It is a modification of the file 'model_sm.F from the program FormCalc.'
c The original file can be found under http://www.feynarts.de/formcalc/. 

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief This subroutine initializes the values for the input-parameters in the SM.
C>
C> The parameters are defined in the header-file paraSM.h,
C> which has to be included in every program and subroutine.
C> The values of the input-parameters are then set by calling the subroutine @ref setsmparameter
C> at the beginning of every executable.
C> If one of the SM parameters needs to be adjusted, it has to be changed in this subroutine.
C>
C>
C> @ingroup group_Input
      subroutine SetSMParameter
      Integer ivar

#include "paraSM.h"

      Print*,"Setting SM Parameters"

#ifdef NO_CKM

      CKM(1,1) = 1
      CKM(2,2) = 1
      CKM(3,3) = 1
      CKM(2,1) = 0
      CKM(3,1) = 0
      CKM(1,2) = 0
      CKM(3,2) = 0
      CKM(1,3) = 0
      CKM(2,3) = 0

#else
      CKMlambda = .2253D0
      CKMA = .808D0
      CKMrhobar = .132D
      CKMetabar = .341D0


      RealType s12, s23, s13
      RealType c12, c23, c13c	ComplexType s13c

      s12 = CKMlambda
      c12 = sqrt((1 - s12)*(1 + s12))

      s23 = s12**2*CKMA
      c23 = sqrt((1 - s23)*(1 + s23))

      s13c = s12*s23*ToComplex(CKMrhobar, CKMetabar)
      s13c = c23/c12*s13c/(1 - s23/s12*s13c)
      s13 = abs(s13c)
      c13 = sqrt((1 - s13)*(1 + s13))
      CKM(1,1) = c12*c13
      CKM(2,1) = -s12*c23 - c12*s23*s13c
      CKM(3,1) = s12*s23 - c12*c23*s13c
      CKM(1,2) = s12*c13
      CKM(2,2) = c12*c23 - s12*s23*s13c
      CKM(3,2) = -c12*s23 - s12*c23*s13c
      CKM(1,3) = Conjugate(s13c)
      CKM(2,3) = s23*c13
      CKM(3,3) = c23*c13

#endif

c Definition of Z boson mass with running width
      MZ = 91.1875D0
      MZ2 = MZ**2
c Definition of Z boson mass with constant width
      MZFW = 91.1875D0-2.4952D0**2/(2D0*91.1875D0)
      MZFW2 = MZFW**2

      GF = 1.166379D-5
      Alfa = 1/137.035999074D0
c      Alfa = sqrt2/pi*GF*MW2*SW2
      Alfa2 = Alfa**2
      DelAlfaMZ=5.907D-2
      AlfaMZ = Alfa/(1-DelAlfaMZ)
c     LambdaQCD is Lambda_QCD for 5 flavours
      AlfasMZ = 0.1190D0
      LambdaQCD=0.2203D0
c     AlfasMZ = 0.1184D0
c     LambdaQCD=0.2131D0

c Lepton Masses
      ME = 0.5109981D-3
      ME2 = ME**2
      MM = 105.658371D-3
      MM2 = MM**2
      ML = 1.777D0
      ML2 = ML**2

c Masses of the up-type Quarks
      MU = 0.05811D0
      MU2 = MU**2
      MC = 1.5D0
      MC2 = MC**2
      MT = 173.2D0
      MT2 = MT**2

c Masses of the down-type Quarks
      MD = 0.05831D0
      MD2 = MD**2
      MS = 0.15D0
      MS2 = MS**2
      MB = 4.7D0
      MB2 = MB**2



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





      end
