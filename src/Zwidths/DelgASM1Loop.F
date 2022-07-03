c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates the one-loop corrections to the effective
C> axial-vector-couplings in the SM, \f$\Delta^{(1)}g^f_{A,\mathrm{SM}}\f$.
C>
C> In order to avoid multiple calculation, the counterterms contained
C> in \f$\Delta^{(1)}g^f_{A,\mathrm{SM}}\f$
C> are calculated before executing the subroutine and then passed
C> by the common block in CTdeclarationSM.h.
C>
C>
C> @ingroup group_Zwidths
      subroutine CalcDelgASM1L(DelgASM1L,af,drSM1L)
C> @param[out] The array DelgASM1L returns the corrections to the effective couplings.
C> The index in the array corrsponds to
C> - 2: axial-vector-coupling of the electron
C> - 3: axial-vector-coupling of the muon
C> - 4: axial-vector-coupling of the tau lepton
C> - 5: axial-vector-coupling of the up quark
C> - 6: axial-vector-coupling of the down quark
C> - 7: axial-vector-coupling of the charm quark
C> - 8: axial-vector-coupling of the strange quark
C> - 9: axial-vector-coupling of the bottom quark
      RealType DelgASM1L(2:9)
C> @param[in] The tree-level axial-vector-couplings are passed to the subroutine
C> by the array af.
C> The index in the array corresponds to:
C> - 1: axial-vector-coupling of the neutrino
C> - 2: axial-vector-coupling of the electron
C> - 3: axial-vector-coupling of the muon
C> - 4: axial-vector-coupling of the tau lepton
C> - 5: axial-vector-coupling of the up quark
C> - 6: axial-vector-coupling of the down quark
C> - 7: axial-vector-coupling of the charm quark
C> - 8: axial-vector-coupling of the strange quark
C> - 9: axial-vector-coupling of the bottom quark
      RealType af(1:9)
C> @param[in] The one-loop SM correction to \f$\Delta r\f$ is passed to the subroutine
C> by the parameter drSM1L.
      RealType drSM1L
      RealType gAeSM, gAmuSM, gAtauSM,
     & gAuSM, gAdSM, gAcSM, gAsSM, gAbSm

#include "CTdeclarationSM.h"
#include "paraSM.h"

      call CalcgAeSM(gAeSM,drSM1L)
      call CalcgAmuSM(gAmuSM,drSM1L)
      call CalcgAtauSM(gAtauSM,drSM1L)
      call CalcgAuSM(gAuSM,drSM1L)
      call CalcgAdSM(gAdSM,drSM1L)
      call CalcgAcSM(gAcSM,drSM1L)
      call CalcgAsSM(gAsSM,drSM1L)
      call CalcgAbSM(gAbSM,drSM1L)


      DelgASM1L(2)=gAeSM-af(2)
      DelgASM1L(3)=gAmuSM-af(3)
      DelgASM1L(4)=gAtauSM-af(4)
      DelgASM1L(5)=gAuSM-af(5)
      DelgASM1L(6)=gAdSM-af(6)
      DelgASM1L(7)=gAcSM-af(7)
      DelgASM1L(8)=gAsSM-af(8)
      DelgASM1L(9)=gAbSM-af(9)



      end
