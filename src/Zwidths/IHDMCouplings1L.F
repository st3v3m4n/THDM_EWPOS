c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates non-standard one-loop corrections to
C> the effective vector- and axial-vector couplings in the IHDM.
C>
C> In order to avoid multiple calculation, the counterterms contained
C> in \f$\Delta^{(1)}g^f_{A,\mathrm{NS}}\f$ and
C> \f$\Delta^{(1)}g^f_{V,\mathrm{NS}}\f$
C> are calculated before executing the subroutine and then passed
C> by the common block in CTdeclarationTHDM.h.
C>
C> @ingroup group_Zwidths
      subroutine CalcIHDMCouplings1L(gVIHDM,gAIHDM)
      implicit none
C> @param[out] The array gVIHDM gives the non-standard corrections to the effective vector-couplings.
C> The entries in the array correspond to:
C> - 1: vector-coupling of the neutrino
C> - 2: vector-coupling of the electron
C> - 3: vector-coupling of the muon
C> - 4: vector-coupling of the tau lepton
C> - 5: vector-coupling of the up quark
C> - 6: vector-coupling of the down quark
C> - 7: vector-coupling of the charm quark
C> - 8: vector-coupling of the strange quark
C> - 9: vector-coupling of the bottom quark
      RealType gVIHDM(1:9)
C> @param[out] The array gAIHDM gives the non-standard corrections
C> to the effective axial-vector-couplings.
C> The entries of the array are equivalent to the corresponding index of the vector
C> coupling.
      RealType gAIHDM(1:9)
      RealType  drns1L
      RealType gnuTHDM, gVeTHDM, gVmuTHDM,
     & gVuTHDM, gVdTHDM, gVbTHDM
      RealType gAeTHDM,
     & gAuTHDM, gAdTHDM, gAbTHDM
#include "CTdeclarationTHDM.h"
#include "paraTHDM.h"
#include "paraSM.h"

      call Calcdrns(drns1L,0)


      call CalcgnuTHDM(gnuTHDM,drns1L)

      call CalcgVeTHDM(gVeTHDM,drns1L)
      call CalcgVuTHDM(gVuTHDM,drns1L)
      call CalcgVdTHDM(gVdTHDM,drns1L)

      gVIHDM(1)=gnuTHDM
      gVIHDM(2)=gVeTHDM
      gVIHDM(3)=gVeTHDM
      gVIHDM(4)=gVeTHDM
      gVIHDM(5)=gVuTHDM
      gVIHDM(6)=gVdTHDM
      gVIHDM(7)=gVuTHDM
      gVIHDM(8)=gVdTHDM
      gVIHDM(9)=gVdTHDM

      call CalcgAeTHDM(gAeTHDM,drns1L)
      call CalcgAuTHDM(gAuTHDM,drns1L)
      call CalcgAdTHDM(gAdTHDM,drns1L)

      gAIHDM(1)=gnuTHDM
      gAIHDM(2)=gAeTHDM
      gAIHDM(3)=gAeTHDM
      gAIHDM(4)=gAeTHDM
      gAIHDM(5)=gAuTHDM
      gAIHDM(6)=gAdTHDM
      gAIHDM(7)=gAuTHDM
      gAIHDM(8)=gAdTHDM
      gAIHDM(9)=gAdTHDM


      end
