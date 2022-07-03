c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

#include "types.h"

C> @brief Subroutine for the calculation of the partial Z widths in the THDM.
C>
C> The calculation of the partial widths in the THDM
C> \f[
C> \Gamma_f=\Gamma_{f,\mathrm{SM}}+\Delta\Gamma_{f,\mathrm{NS}}
C>\f]
C> consists of the calculation of the SM part \f$\Gamma_{f,\mathrm{SM}}\f$
C> and the calculation of the non-standard part \f$\Delta\Gamma_{f,\mathrm{NS}}\f$
C>
C> The SM part is calculated by calling the subroutines @ref calczwidthssm1loop
C> and @ref calczwidthssmho internally.
C>
C> The non-standard corrections which are included in \f$\Delta\Gamma_{f,\mathrm{NS}}\f$
C> are selected by the flag \b contflag.
C>
C> @ingroup group_Zwidths
      Subroutine CalcZWidthsTHDM(GZTHDM,contflag)
      implicit none
C> @param[out] The array GZTHDM gives the partial Z widths in the THDM.
C> The entries in the array correspond to:
C> - 1: partial width of the neutrino
C> - 2: partial width of the electron
C> - 3: partial width of the muon
C> - 4: partial width of the tau lepton
C> - 5: partial width of the up quark
C> - 6: partial width of the down quark
C> - 7: partial width of the charm quark
C> - 8: partial width of the strange quark
C> - 9: partial width of the bottom quark
      RealType GZTHDM(9)
      RealType Gamma0, Drhofred, Dkappared
      RealType GZSMho(9), GZSM1Loop(9), GZtree(9)
      RealType RV(2:9), RA(2:9), vf(1:9), af(1:9)
      RealType gVTHDM(1:9), gATHDM(1:9)
      RealType drhoNS1L, drhotop1L,drhoNS2L , drhoNSt2L,
     & drhoNSH2L, drhoMixH2L, drhoIHDM2L
      Integer K
C> @param[in] The flag contflag selects the contributions which should be included in \f$\Delta\Gamma_{f,\mathrm{NS}}\f$
C>  - contflag=0: include one-loop corrections
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}\f]
C>  - contflag=1: one-loop +reducible two-loop corrections
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}+\Delta^{(2)}\Gamma^f_\mathrm{NS,red}\f]
C>  - contflag=2: one-loop+two-loop reducible+two-loop non-standard top-Yukawa corrections
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}+\Delta^{(2)}\Gamma^f_\mathrm{NS,red}
C> +\Delta^{(2)}\Gamma^f_\mathrm{t,NS}\f]
C>  - contflag=3: one-loop+two-loop reducible+pure non-standard scalar two-loop corrections
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}+\Delta^{(2)}\Gamma^f_\mathrm{NS,red}
C> +\Delta^{(2)}\Gamma^f_\mathrm{H,NS}\f]
C>  - contflag=4: one-loop+two-loop reducible+mixed scalar two-loop corrections
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}+\Delta^{(2)}\Gamma^f_\mathrm{NS,red}
C> +\Delta^{(2)}\Gamma^f_\mathrm{H,NS}\f]
C>  - contflag=5: one-loop+two-loop reducible+all available two-loop corrections in the aligned THDM
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{NS}+\Delta^{(2)}\Gamma^f_\mathrm{NS,red}
C> +\Delta^{(2)}\Gamma^f_\mathrm{NS,irr}\f]
C>  - contflag=6: one-loop corrections in the IHDM
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{IHDM}\f]
C>  - contflag=7: one-loop+two-loop reducible+ two-loop corrections in the IHDM
C> \f[\Delta\Gamma_{f,\mathrm{NS}}=\Delta^{(1)}\Gamma^f_\mathrm{IHDM}
C> +\Delta^{(2)}\Gamma^f_\mathrm{NS,red}+\Delta^{(2)}\Gamma^f_\mathrm{IHDM}
C>\f]
      Integer contflag


#include "CTdeclarationSM.h"
#include "CTdeclarationTHDM.h"
#include "paraTHDM.h"
#include "paraSM.h"


      call SetTreeCouplings(vf,af)
c Estimation of the higher order effect on the partial widths
      call CalcZWidthsSMho(GZSMho)
c tree-level result of partial widths
      call CalcZWidthstree(GZtree)
c One-loop result in the SM
      call CalcZWidthsSM1Loop(GZSM1Loop)

c Setting the radiation factors
      call CalcRfactors(RV,RA)
c Calculation of the 1Loop SM and THDM counterterms

      call CalcdMW2SM
      call CalcdMZ2SM
      call CalcdZZZSM
      call CalcdZZASM
      call CalcdZAZSM
      call CalcPiAA0SM



      call CalcdMW2THDM
      call CalcdMZ2THDM
      call CalcdZZZTHDM
      call CalcdZAZTHDM
      call CalcPiAA0THDM





c Calculation of the effective couplings
      IF (contflag.ne.6 .AND. contflag.ne.7 ) THEN
c eff. couplings at one-loop for the general THDM
      call CalcTHDMCouplings1L(gVTHDM,gATHDM)
      ELSE
c eff. couplings at one-loop in the IHDM
      call CalcIHDMCouplings1L(gVTHDM,gATHDM)
      ENDIF

c Calculation of the partial Z decay widths

      Gamma0=GF*MZ**3D0/(24D0*Sqrt(2D0)*pi)
      GZTHDM(1)=GZSMho(1)+GZSM1Loop(1)
     & +8D0*Gamma0*(2D0*af(1)*gATHDM(1))

      Do 100 K=2,9
      GZTHDM(K)=GZSMho(K)+GZSM1Loop(K)
     &  +4D0*Gamma0*NCf(K)*(
     & (2D0*vf(K)*gVTHDM(K))*RV(K)
     & +(2D0*af(K)*gATHDM(K))*RA(K)
     & )
 100  CONTINUE


      IF (contflag.eq.0 .or. contflag.eq.6) THEN
      RETURN
      ENDIF

      call CalcdrhoNS1L(drhoNS1L)
      call Calcdrhotop1L(drhotop1L)
      Drhofred= DelAlfaMZ*drhoNS1L
     & +(1D0-CW**2/SW**2)
     &   *(2*drhotop1L*drhoNS1L+drhoNS1L**2)

      Dkappared=DelAlfaMZ*CW2/SW2*drhoNS1L
     &   -(CW2**2)/(SW2**2)
     &   *(2*drhotop1L*drhoNS1L+drhoNS1L**2)





      GZTHDM(1)=GZTHDM(1) + GZtree(1)*Drhofred

      Do 200 K=2,9
      GZTHDM(K)= GZTHDM(K) + GZtree(K)*Drhofred
     & -16D0*NCf(K)*Gamma0*SW2*Qf(K)*vf(K)*RV(K)*Dkappared
     & +16D0*NCf(K)*Gamma0*CW2*Qf(K)*RV(K)
     &  *(CW2*Qf(K)-vf(K))
     &   *(2*drhotop1L*drhoNS1L+drhoNS1L**2)
 200  Continue


      IF (contflag.eq.1) THEN
      RETURN
      ENDIF

      IF (contflag.eq.2) THEN
      call CalcNStop2Loop(drhoNSt2L)
      drhoNS2L=drhoNSt2L
      ENDIF

      IF(contflag.eq.3) THEN
      call CalcNSHiggs2Loop(drhoNSH2L)
      drhoNS2L= drhoNSH2L
      ENDIF

      IF(contflag.eq.4) THEN
      call CalcMixHiggs2Loop(drhoMixH2L)
      drhoNS2L= drhoMixH2L
      ENDIF


      IF(contflag.eq.5) THEN
      call CalcNStop2Loop(drhoNSt2L)
      call CalcNSHiggs2Loop(drhoNSH2L)
      call CalcMixHiggs2Loop(drhoMixH2L)
      drhoNS2L= (drhoNSt2L + drhoNSH2L + drhoMixH2L)
      ENDIF


      IF(contflag.eq.7) THEN
      call CalcIHDM2Loop(drhoIHDM2L)
      drhoNS2L = drhoIHDM2L
      ENDIF

      GZTHDM(1)= GZTHDM(1) + GZtree(1)*drhoNS2L

      Do 300 K=2,9
      GZTHDM(K)=GZTHDM(K) + GZtree(K)*drhoNS2L
     & -16D0*Gamma0*NCf(K)*Qf(K)*vf(K)*RV(K)*CW2*drhoNS2L
 300  Continue



      end
