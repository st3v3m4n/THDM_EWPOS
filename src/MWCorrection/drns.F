c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

#include "types.h"


C> @brief This subroutine calculates the non-standard contributions to \f$\Delta r\f$ in the THDM
C>
C> The non-standard contribution to \f$\Delta  r\f$ at the one-loop order is given by
C> \f[
C> \Delta r_\mathrm{NS}^{(1)}=\Pi^{AA}_\mathrm{THDM}\left(0\right)
C> +\frac{c_W^2}{s_W^2}\left(\frac{\delta M_{W,\mathrm{THDM}}^2}{M_W^2}
C> -\frac{\delta M_{Z,\mathrm{THDM}}^2}{M_Z^2}\right)+\frac{\Sigma^W_\mathrm{THDM}\left(0\right)
C> -\delta M_{W,\mathrm{THDM}}^2}{M_W^2},
C> \f]
C> with the following non-standard contributions to the counterterms and self-energies:
C> - the photon vacuum polarization \f$\Pi^{AA}_\mathrm{THDM}\left(0\right)\f$ (calculated by \ref calcpiaa0thdm),
C> - the W boson mass counterterm \f$\delta M_{W,\mathrm{THDM}}^2\f$ (calculated by the subroutine \ref calcdmw2thdm),
C> - the Z boson mass counterterm \f$\delta M_{Z,\mathrm{THDM}}^2\f$ (calculated by the subroutine \ref calcdmz2thdm),
C> - the self-energy of the W boson \f$\Sigma^W_\mathrm{THDM}\left(0\right)\f$ (calculated by the function \ref sigmawthdm).
C>
C> In addition to the one-loop contribution also the leading two-loop contributions from the \f$\rho\f$ parameter are implemented.
C> For details see the description of the input-parameter sflag.
C>
C> @ingroup group_MW
      subroutine Calcdrns(drns,sflag)
      Implicit none
C> @param[out] The parameter drns is the non-standard contribution \f$\Delta r_\mathrm{NS}\f$.
      RealType drns
      RealType drhoNS1L, drhotop1L, drhoNSt2L,
     & drhoNSH2L, drhoMixH2L, drhoIHDM2L, drhoAllH2L
C> @param[in] The flag sflag selects the possible non-standard corrections to \f$\Delta r\f$.
C> Possible values are:
C> - 0: only one-loop contribution;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS},\f]
C> - 1: one-loop contribution and reducible non-standard corrections from \f$\Delta\alpha\f$ and \f$\Delta\rho^{(1)}_\mathrm{NS}\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}-2\frac{c_W^2}{s_W^2}\Delta\alpha\Delta\rho^{(1)}_\mathrm{NS},\f]
C> - 2: one-loop contribution together with reducible two-loop contributions;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red},\f]
C> - 3: one-loop + two-loop reducible + two-loop non-standard top-Yukawa contributions from \f$\Delta \rho\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{t,NS},\f]
C> - 4: one-loop + two-loop reducible + two-loop non-standard scalar contributions from \f$\Delta \rho\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{H,NS},\f]
C> - 5: one-loop + two-loop reducible + two-loop mixed scalar contributions from \f$\Delta\rho\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{H,Mix},\f]
C> - 6: one-loop + two-loop reducible +  two-loop corrections in the alignment limit \f$\Delta\rho\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}
C>	+\Delta r^{(2)}_\mathrm{NS,red}
C> 	+ \Delta r^{(2)}_\mathrm{t,NS}
C> 	+ \Delta r^{(2)}_\mathrm{H,NS}
C> 	+ \Delta r^{(2)}_\mathrm{H,Mix}
C>	,\f]
C> - 7: one-loop + two-loop reducible + scalar two-loop correction from \f$\Delta \rho\f$ in the IHDM;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{IHDM}.\f]
c> - 8: one-loop + non-standard top-Yukawa correction in the alignment limit + general scalar two-loop corrections (with \f$\overline{\mathrm{MS}}\f$ renormalization of \f$\delta m_{h^0H^0}^2\f$)
c>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{t,NS}+ \Delta r^{(2)}_\mathrm{H}.\f]
c> - 9: one-loop + non-standard top-Yukawa correction in the alignment limit + general scalar two-loop corrections (with on-shell renormalization of \f$\delta m_{h^0H^0}^2\f$)
c>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{t,NS}+ \Delta r^{(2)}_\mathrm{H}.\f]


      Integer sflag
      double complex SigmaWTHDM
      external SigmaWTHDM
#include "CTdeclarationTHDM.h"
#include "paraTHDM.h"
#include "paraSM.h"


      call CalcdMW2THDM
      call CalcdMZ2THDM
      call CalcPiAA0THDM

      drns = PiAA0THDM+CW2/SW2*(dMW2THDM/MW2-dMZ2THDM/MZ2)
     & +(SigmaWTHDM(0D0)-dMW2THDM)/MW2

      IF (sflag.eq.0) THEN
      RETURN
      END IF

      call CalcdrhoNS1L(drhoNS1L)
      call Calcdrhotop1L(drhotop1L)

      drns= drns - 2D0*(CW2/SW2)*DelAlfaMZ*drhoNS1L

      IF(sflag.eq.1) THEN
      RETURN
      ENDIF
      drns = drns +2D0*(CW2/SW2)**2*
     &       (2D0*drhotop1L+drhoNS1L)*drhoNS1L

      IF(sflag.eq.2) THEN
      RETURN
      ENDIF

      IF(sflag.eq.3) THEN
      call CalcNStop2Loop(drhoNSt2L)
      drns=drns -CW2/SW2*drhoNSt2L
      ENDIF

      IF(sflag.eq.4) THEN
      call CalcNSHiggs2Loop(drhoNSH2L)
      drns=drns -CW2/SW2*drhoNSH2L
      ENDIF

      IF(sflag.eq.5) THEN
      call CalcMixHiggs2Loop(drhoMixH2L)
      drns=drns -CW2/SW2*drhoMixH2L
      ENDIF


      IF(sflag.eq.6) THEN
      call CalcNStop2Loop(drhoNSt2L)
      call CalcNSHiggs2Loop(drhoNSH2L)
      call CalcMixHiggs2Loop(drhoMixH2L)
      drns=drns-CW2/SW2*(drhoNSt2L + drhoNSH2L + drhoMixH2L)
      ENDIF

      IF(sflag.eq.7) THEN
      call CalcIHDM2Loop(drhoIHDM2L)
      drns=drns -CW2/SW2*drhoIHDM2L
      ENDIF

      IF(sflag.eq.8) THEN
      call CalcNStop2Loop(drhoNSt2L)
      call CalcAllH2Loop(drhoAllH2L,1)
      drns=drns-CW2/SW2*(drhoNSt2L + drhoAllH2L)
      ENDIF

      IF(sflag.eq.9) THEN
      call CalcNStop2Loop(drhoNSt2L)
      call CalcAllH2Loop(drhoAllH2L,2)
      drns=drns-CW2/SW2*(drhoNSt2L + drhoAllH2L)
      ENDIF

      end
