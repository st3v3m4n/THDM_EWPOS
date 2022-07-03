c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Subroutine for the non-standard correction \f$\Delta\kappa_\mathrm{NS}\f$
C> to the quantity \f$\kappa\f$.
C>
C> @ingroup group_mixangles
      subroutine CalcDelKNS(DelKNS,loflag)
      implicit none
C> @param[in] The parameter DelKNS gives the non-standard contribution \f$\Delta\kappa_\mathrm{NS}\f$.
      RealType DelKNS
      RealType drhoNS1L, drhotop1L, drhoNSt2L,
     & drhoNSH2L, drhoMixH2L, drhoIHDM2L
C> @param[in] The flag loflag selects the included corrections in \f$\Delta\kappa_\mathrm{NS}\f$:
C> - loflag=0: only one-loop correction
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}\f]
C> - loflag=1: one-loop correction + reducible two-loop correction
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}\f]
C> - loflag=2: one-loop correction + reducible two-loop correction + non-standard two-loop top-Yukawa correction
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}+\Delta\kappa^{(2)}_\mathrm{t,NS}\f]
C> - loflag=3: one-loop correction + reducible two-loop correction + pure non-standard two-loop scalar correction
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}+\Delta\kappa^{(2)}_\mathrm{H,NS}\f]
C> - loflag=4:  one-loop correction + reducible two-loop correction + mixed non-standard two-loop scalar correction
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}+\Delta\kappa^{(2)}_\mathrm{H,Mix}\f]
C> - loflag=5: one-loop correction + all available non-standard two-loop corrections in the alignment limit
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}+\Delta\kappa^{(2)}_\mathrm{NS,irr}\f]
C> - loflag=6: one-loop correction + reducible two-loop correction +scalar two-loop correction in the IHDM
C> \f[\Delta\kappa_\mathrm{NS}=\Delta\kappa^{(1)}_\mathrm{NS}+\Delta\kappa^{(2)}_\mathrm{NS,red}+\Delta\kappa^{(2)}_\mathrm{IHDM}\f]
      Integer loflag


#include "CTdeclarationTHDM.h"
#include "paraTHDM.h"
#include "paraSM.h"


      call CalcdSWsqTHDM
      call CalcdZAZTHDM


      DelKNS=-1D0/2D0*CW/SW*dZAZTHDM + dSWsqTHDM/SW2


      IF (loflag.eq.0) THEN
      RETURN
      END IF

      call CalcdrhoNS1L(drhoNS1L)
      call Calcdrhotop1L(drhotop1L)

      DelKNS= DelKNS + DelAlfaMZ*CW2/SW2*drhoNS1L
     &   -(CW2**2D0)/(SW2**2D0)
     &   *(2*drhotop1L*drhoNS1L+drhoNS1L**2D0)

      IF(loflag.eq.1) THEN
      RETURN
      ENDIF


      IF(loflag.eq.2) THEN
      call CalcNStop2Loop(drhoNSt2L)
      DelKNS= DelKNS +CW2/SW2*drhoNSt2L
      ENDIF

      IF(loflag.eq.3) THEN
      call CalcNSHiggs2Loop(drhoNSH2L)
      DelKNS=DelKNS +CW2/SW2*drhoNSH2L
      ENDIF

      IF(loflag.eq.4) THEN
      call CalcMixHiggs2Loop(drhoMixH2L)
      DelKNS=DelKNS +CW2/SW2*drhoMixH2L
      ENDIF


      IF(loflag.eq.5) THEN
      call CalcNStop2Loop(drhoNSt2L)
      call CalcNSHiggs2Loop(drhoNSH2L)
      call CalcMixHiggs2Loop(drhoMixH2L)
      DelKNS=DelKNS+CW2/SW2*(drhoNSt2L + drhoNSH2L + drhoMixH2L)
      ENDIF

      IF(loflag.eq.6) THEN
      call CalcIHDM2Loop(drhoIHDM2L)
      DelKNS=DelKNS+CW2/SW2*drhoIHDM2L
      ENDIF

      end
