c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



#include "types.h"

C> @brief Calculation of the partial Z widths in the SM at the one-loop order,
C> \f$\Gamma^{\mathrm{(1)}}_{f,\mathrm{SM}}\f$.
C>
C> The one-loop SM result is given by
C>\f[
C>\Gamma^{(1)}_{f,\mathrm{SM}}= N_c^f \Gamma_0\left[\left(v_f^2
C>+ 2 v_f \Delta^{(1)}g^f_{V,\text{SM}}\right) R^f_V
C> + \left(a_f^2+ 2 a_f \Delta^{(1)}g^f_{A,\text{SM}}\right) R^f_A\right].
C>\f]
C> Internally, the radiation factors \f$R^f_{V,A}\f$
C> are obtained by the subroutine @ref calcrfactors.
C> The one-loop corrections to the effective couplings are calculated by
C> the subroutines @ref calcdelgasm1l (for \f$\Delta^{(1)}g^f_{A,\mathrm{SM}}\f$)
C> and @ref calcdelgvsm1l (for \f$\Delta^{(1)}g^f_{V,\mathrm{SM}}\f$).
C>
C> @ingroup group_Zwidths
      subroutine CalcZWidthsSM1Loop(GZSM1Loop)
      implicit none
C> @param[out] The array GZSM1Loop gives \f$\Gamma^{\mathrm{(1)}}_{f,\mathrm{SM}}\f$. The entries in the array correspond to:
C> - 1: partial width of the neutrino
C> - 2: partial width of the electron
C> - 3: partial width of the muon
C> - 4: partial width of the tau lepton
C> - 5: partial width of the up quark
C> - 6: partial width of the down quark
C> - 7: partial width of the charm quark
C> - 8: partial width of the strange quark
C> - 9: partial width of the bottom quark
      RealType GZSM1Loop
      dimension GZSM1Loop(9)
      RealType DelgVSM1L(2:9), DelgASM1L(2:9)
      RealType RV(2:9), RA(2:9), vf(1:9), af(1:9)
      RealType drSM1L, Gamma0
      RealType gnuSM, DelgnuSM
      Integer int1, M, I

#include "CTdeclarationSM.h"
#include "paraSM.h"


      call SetTreeCouplings(vf,af)

      call CalcdrSM1Loop(drSM1L)

      call CalcRfactors(RV,RA)

c Calculation of the 1Loop SM Counterterms
      call CalcdMW2SM
      call CalcdMZ2SM
      call CalcdZZZSM
      call CalcdZZASM
      call CalcdZAZSM
      call CalcPiAA0SM


c Calculation of the effective couplings
      call CalcgnuSM(gnuSM, drSM1L)
      DelgnuSM=gnuSM-af(1)
      call CalcDelgVSM1L(DelgVSM1L,vf,drSM1L)
      call CalcDelgASM1L(DelgASM1L,af,drSM1L)

c Calculation of the partial Z Widths

      Gamma0=GF*MZ**3D0/(24D0*Sqrt(2D0)*pi)

      GZSM1Loop(1)=8D0*Gamma0*(af(1)**2+2D0*af(1)*DelgnuSM)

      Do 100 int1=2,9

      GZSM1Loop(int1)=4D0*Gamma0*NCf(int1)*(
     &  (vf(int1)**2+2D0*vf(int1)*DelgVSM1L(int1))*RV(int1)
     & +(af(int1)**2+2D0*af(int1)*DelgASM1L(int1))*RA(int1))

 100  continue


      end
