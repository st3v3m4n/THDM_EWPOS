c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Subroutine for calculating the radiation factors for the final state
C> QED and QCD corrections of the partial Z widths
C>
C> The formulas for the radiation factors and information
C> about the included higher-order corrections can be found in
C> [arXiv:1401.2447](https://arxiv.org/abs/1401.2447) \cite Freitas:2014hra.
C>
C> @ingroup group_Zwidths
      subroutine CalcRfactors(RV,RA)
      Implicit none
C> @param[out] The array RV gives the radiation factor \f$R_V^f\f$.
C> The entries in the array correspond to:
C> - 1: radiation factor for the neutrino
C> - 2: radiation factor for the electron
C> - 3: radiation factor for the muon
C> - 4: radiation factor for the tau lepton
C> - 5: radiation factor for the up quark
C> - 6: radiation factor for the down quark
C> - 7: radiation factor for the charm quark
C> - 8: radiation factor for the strange quark
C> - 9: radiation factor for the bottom quark
      RealType RV(2:9)
C> @param[out] The array RA gives the radiation factor \f$R_A^f\f$.
C> The entries in the array are equivalent to the corresponding entries in RV.
      RealType RA(2:9)
      RealType MFsq(2:9)
      RealType RVQED, RAQED, RVQCD, RAQCD
      Integer l

#include "paraSM.h"



      RV(2)=RVQED(2)
      RV(3)=RVQED(3)
      RV(4)=RVQED(4)
c      RV(5)=1.039851410947789D0
c      RV(6)=1.039242048696966D0
c      RV(7)=1.039874938317419D0
c      RV(8)=1.039242048696966D0
c      RV(9)=1.039834635577104D0

      RA(2)=RAQED(2)
      RA(3)=RAQED(3)
      RA(4)=RAQED(4)
c      RA(5)=1.046960454176231D0
c      RA(6)=1.032133005468524D0
c      RA(7)=1.046702386802622D0
c      RA(8)=1.032133005468524D0
c      RA(9)=1.025571441454676D0

      Do 100 l= 5,9

      RV(l)=RVQED(l) + RVQCD(l)
      RA(l)=RAQED(l) + RAQCD(l)


 100  Continue

      end


c Functions Ifun2, Ifun3, Ifun4, Ct2  taken from arXiv:1401.2447 p. 16

C> @brief Function needed for the calculation of the final state correction
C> of the partial width of the Z boson.
C>
C> @ingroup group_Zwidths
      RealType Function Ct2(x)
      Implicit none
      RealType x

       Ct2 = x*(44D0/675D0 - 2D0/135D0*DLOG(x))

      END

C> @brief Function needed for the calculation of the final state correction
C> of the partial width of the Z boson.
C>
C> @ingroup group_Zwidths
      RealType Function Ifun2(x)
      Implicit none
      RealType x

        Ifun2 = -37D0/12D0 + DLOG(x) +7D0/81D0*x
     & +79D0/6000D0* x**2

      END

C> @brief Function needed for the calculation of the final state correction
C> of the partial width of the Z boson.
C>
C> @ingroup group_Zwidths
      RealType Function Ifun3(x)
      Implicit none
      RealType x

       Ifun3 = -15.9877D0 + 67D0/18D0* DLOG(x)
     & + 23D0/12D0*DLOG(x)**2

      END

C> @brief Function needed for the calculation of the final state correction
C> of the partial width of the Z boson.
C>
C> @ingroup group_Zwidths
      RealType Function Ifun4(x)
      RealType x

       Ifun4 = 49.0309D0 - 17.6637D0* DLOG(x)
     &  +14.6597D0* DLOG(x)**2+3.6736D0*DLOG(x)**3

      END

C> @brief Function for the vector radiation factor of the QED corrections
C> and the finite mass effects of the external fermions
C>
C> @ingroup group_Zwidths
      RealType Function RVQED(I)
      Implicit none
      Integer I
      RealType Ct2

#include "paraSM.h"

      RVQED= 1D0 + 3D0/(4D0*pi)*Qf(I)**2*AlfaMZ
     & +Qf(I)**2*(Cgamma2+2*Ct2(MZ2/MT2))
     & *(AlfaMZ/pi)**2
     & -6D0*fmass2(I)**2D0/MZ2**2D0

      END


C> @brief Function for the axial radiation factor of the QED corrections
C> and the finite mass effects of the external fermions
C>
C> @ingroup group_Zwidths
      RealType Function RAQED(I)
      Implicit none
      Integer I
      RealType Ct2

#include "paraSM.h"

      RAQED= 1D0 + 3D0/(4D0*pi)*Qf(I)**2*AlfaMZ
     & +Qf(I)**2*(Cgamma2+2*Ct2(MZ2/MT2))
     & *(AlfaMZ/pi)**2
     & -6D0*fmass2(I)/MZ2
     & +6D0*fmass2(I)**2D0/MZ2**2D0

      END

C> @brief Function for the vector radiation factor of the QCD corrections
C>
C> @ingroup group_Zwidths
      RealType Function RVQCD(I)
      Implicit none
      Integer I
      RealType Ct2
#include "paraSM.h"

       RVQCD= AlfasMZ/pi-Qf(I)**2/4D0*AlfaMZ/pi*AlfasMZ/pi
     &  + 12D0*fmass2(I)/MZ2*AlfasMZ/pi
     &  + (C02 + Ct2(MZ2/MT2))*(AlfasMZ/pi)**2
     &  + C03*(AlfasMZ/pi)**3
     &  + C04*(AlfasMZ/pi)**4

      END


C> @brief Function for the axial radiation factor of the QCD corrections
C>
C> @ingroup group_Zwidths
      RealType Function RAQCD(I)
      Integer I
      RealType Ct2, Ifun2, Ifun3, Ifun4
#include "paraSM.h"

       RAQCD= AlfasMZ/pi-Qf(I)**2/4D0*AlfaMZ/pi*AlfasMZ/pi
     &  - 22D0*fmass2(I)/MZ2*AlfasMZ/pi
     &  + (C02 + Ct2(MZ2/MT2)-2D0*I3f(I)*Ifun2(MZ2/MT2))*(AlfasMZ/pi)**2
     &  + (C03 -2D0*I3f(I)*Ifun3(MZ2/MT2))*(AlfasMZ/pi)**3
     &  + (C04 -2D0*I3f(I)*Ifun4(MZ2/MT2))*(AlfasMZ/pi)**4


      END
