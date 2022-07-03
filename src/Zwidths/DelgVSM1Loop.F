c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates the one-loop corrections to the effective
C> vector-couplings in the SM, \f$\Delta^{(1)}g^f_{V,\mathrm{SM}}\f$.
C>
C> In order to avoid multiple calculation, the counterterms contained
C> in \f$\Delta^{(1)}g^f_{V,\mathrm{SM}}\f$
C> are calculated before executing the subroutine and then passed
C> by the common block in CTdeclarationSM.h.
C>
C> @ingroup group_Zwidths
      subroutine CalcDelgVSM1L(DelgVSM1L,vf,drSM1L)
C> @param[out] The array DelgVSM1L returns the corrections to the effective couplings.
C> The index in the array corrsponds to
C> - 2: vector-coupling of the electron
C> - 3: vector-coupling of the muon
C> - 4: vector-coupling of the tau lepton
C> - 5: vector-coupling of the up quark
C> - 6: vector-coupling of the down quark
C> - 7: vector-coupling of the charm quark
C> - 8: vector-coupling of the strange quark
C> - 9: vector-coupling of the bottom quark
      RealType DelgVSM1L(2:9)
C> @param[in] The tree-level vector-couplings are passed to the subroutine
C> by the array vf.
C> The index in the array corresponds to:
C> - 1: vector-coupling of the neutrino
C> - 2: vector-coupling of the electron
C> - 3: vector-coupling of the muon
C> - 4: vector-coupling of the tau lepton
C> - 5: vector-coupling of the up quark
C> - 6: vector-coupling of the down quark
C> - 7: vector-coupling of the charm quark
C> - 8: vector-coupling of the strange quark
C> - 9: vector-coupling of the bottom quark
      RealType vf(1:9)
C> @param[in] The one-loop SM correction to \f$\Delta r\f$ is passed to the subroutine
C> by the parameter drSM1L.
      RealType drSM1L
      RealType gVeSM, gVmuSM, gVtauSM,
     & gVuSM, gVdSM, gVcSM, gVsSM, gVbSM
#include "CTdeclarationSM.h"
#include "paraSM.h"

      call CalcgVeSM(gVeSM,drSM1L)
      call CalcgVmuSM(gVmuSM,drSM1L)
      call CalcgVtauSM(gVtauSM,drSM1L)
      call CalcgVuSM(gVuSM,drSM1L)
      call CalcgVdSM(gVdSM,drSM1L)
      call CalcgVcSM(gVcSM,drSM1L)
      call CalcgVsSM(gVsSM,drSM1L)
      call CalcgVbSM(gVbSM,drSM1L)


      DelgVSM1L(2)=gVeSM-vf(2)
      DelgVSM1L(3)=gVmuSM-vf(3)
      DelgVSM1L(4)=gVtauSM-vf(4)
      DelgVSM1L(5)=gVuSM-vf(5)
      DelgVSM1L(6)=gVdSM-vf(6)
      DelgVSM1L(7)=gVcSM-vf(7)
      DelgVSM1L(8)=gVsSM-vf(8)
      DelgVSM1L(9)=gVbSM-vf(9)


      end
