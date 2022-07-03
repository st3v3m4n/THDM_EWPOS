c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates non-standard one-loop corrections to
C> the effective vector- and axial-vector couplings in the THDM.
C>
C> In order to avoid multiple calculation, the counterterms contained
C> in \f$\Delta^{(1)}g^f_{V,\mathrm{NS}}\f$ and
C> \f$\Delta^{(1)}g^f_{A,\mathrm{NS}}\f$
C> are calculated before executing the subroutine and then passed
C> by the common block in CTdeclarationTHDM.h.
C>
C> @ingroup group_Zwidths
      subroutine CalcTHDMCouplings1L(gVTHDM,gATHDM)
      implicit none
C> @param[out] The array gVTHDM gives \f$\Delta^{(1)}g^f_{V,\mathrm{NS}}\f$
C> the non-standard corrections to the effective vector-couplings
C> in the THDM.
C> The entries in the array corresponds to:
C> - 1: vector-coupling of the neutrino
C> - 2: vector-coupling of the electron
C> - 3: vector-coupling of the muon
C> - 4: vector-coupling of the tau lepton
C> - 5: vector-coupling of the up quark
C> - 6: vector-coupling of the down quark
C> - 7: vector-coupling of the charm quark
C> - 8: vector-coupling of the strange quark
C> - 9: vector-coupling of the bottom quark
      RealType gVTHDM(1:9)
C> @param[out] The array gATHDM gives \f$\Delta^{(1)}g^f_{A,\mathrm{NS}}\f$
C> the non-standard corrections
C> to the effective axial-vector-couplings in the THDM.
C> The entries in the array are
C> equiavlent to the corresponding entries of the vector-couplings array.
      RealType gATHDM(1:9)
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
      call CalcgVbTHDM(gVbTHDM,drns1L)

      gVTHDM(1)=gnuTHDM
      gVTHDM(2)=gVeTHDM
      gVTHDM(3)=gVeTHDM
      gVTHDM(4)=gVeTHDM
      gVTHDM(5)=gVuTHDM
      gVTHDM(6)=gVdTHDM
      gVTHDM(7)=gVuTHDM
      gVTHDM(8)=gVdTHDM
      gVTHDM(9)=gVbTHDM

      call CalcgAeTHDM(gAeTHDM,drns1L)
      call CalcgAuTHDM(gAuTHDM,drns1L)
      call CalcgAdTHDM(gAdTHDM,drns1L)
      call CalcgAbTHDM(gAbTHDM,drns1L)

      gATHDM(1)=gnuTHDM
      gATHDM(2)=gAeTHDM
      gATHDM(3)=gAeTHDM
      gATHDM(4)=gAeTHDM
      gATHDM(5)=gAuTHDM
      gATHDM(6)=gAdTHDM
      gATHDM(7)=gAuTHDM
      gATHDM(8)=gAdTHDM
      gATHDM(9)=gAbTHDM


      end
