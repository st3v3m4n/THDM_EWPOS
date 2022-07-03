c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

#include "types.h"

C> @brief This Subroutine calculates the W-boson mass with the non-standard contributions from the THDM
C>
C> @ingroup group_MW
      Subroutine CalcMWTHDM(drSM, drns, SMflag, NSflag)
C> @param[in] The flag SMflag specifies how the SM contribution to \f$\Delta r\f$ is calculated.
C> Possible values are:
C> - 0: approximation via the parameterization in [hep-ph/0311148](https://arxiv.org/abs/hep-ph/0311148)
C>      (\f$\Delta r_\mathrm{SM}\f$ is obtained by
C> calling the subroutines \ref calcdrsm1loop and \ref calcdrsmho internally and summing their results),
C> - 1: calculation with explicit \f$M_W\f$ dependence in complete SM contribution,
C>        (\f$\Delta r_\mathrm{SM}\f$ is calculated by the subroutine \ref rnorm from Prof. Hollik).
      Integer SMflag
C> @param[in] The flag NSflag selects the possible non-standard corrections to \f$\Delta r\f$.
C> It is passed to the subroutine \ref calcdrns.
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
C> - 6: one-loop + two-loop reducible + all available two-loop corrections from \f$\Delta\rho\f$;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}
C>	+\Delta r^{(2)}_\mathrm{NS,red}
C> 	+ \Delta r^{(2)}_\mathrm{t,NS}
C> 	+ \Delta r^{(2)}_\mathrm{H,NS}
C> 	+ \Delta r^{(2)}_\mathrm{H,Mix}
C>	,\f]
C> - 7: one-loop + two-loop reducible + scalar two-loop correction from \f$\Delta \rho\f$ in the IHDM;
C>      \f[\Delta r_\mathrm{NS}=\Delta r^{(1)}_\mathrm{NS}+\Delta r^{(2)}_\mathrm{NS,red} + \Delta r^{(2)}_\mathrm{IHDM}.\f]
      Integer I,NSflag
C> @param[out] drns gives the non-standard contribution to \f$\Delta r\f$. It is obtained by calling the subroutine \ref calcdrns internally.
      RealType drns
C> @param[out] drsm is the contribution to \f$\Delta r\f$ in the SM. For \b SMflag=0 it is obtained by
C> calling the subroutines \ref calcdrsm1loop and \ref calcdrsmho internally and summing their results.
C> For \b SMflag=1 it is calculated by the subroutine @ref rnorm.
      RealType drSM
      RealType MWNEW, MZsave, drSMho, drSM1Loop, GammaW

#include "CTdeclarationTHDM.h"
#include "CTdeclarationSM.h"
#include "paraTHDM.h"
#include "paraSM.h"


      If (SMflag.eq.0) THEN
c Approximation of the higher-order SM contributions to Delta r
      call CalcMWSMho(2)
      call CalcdrSMho(drSMho)
      ELSE
c use fixed width definition for MZ
      MZsave=MZ
      MZ=MZFW
      MZ2=MZ**2
      call calcAlfasMT()
      ENDIF


      MWNEW=80.385D0
      I=0
c Iteration
 10   CONTINUE
      I=I+1
      MW=MWNEW
      IF (MW.GT.90D0) THEN
c	Print*,"Error: MW larger than 90 GeV"
c	Print*,"MW is set equal to 90 GeV"
      MW=90D0
      MW2=MW**2
      call SetWeakAngles
	RETURN
      ENDIF
      MW2=MW**2
      call SetWeakAngles
      IF (SMflag.eq.0) THEN
      Call CalcdrSM1Loop(drSM1Loop)
      drSM= drSM1Loop + drSMho
      ELSE
      call RNORM(MZ,MW,MT,Mh0,DelAlfaMZ,AlfasMT,drSM)
      ENDIF

      Call Calcdrns(drns,NSflag)

      MWNEW = MZ*SQRT( 1D0/2D0 +
     & SQRT(1D0/4D0-pi*Alfa/(SQRT(2D0)*GF*MZ2)*
     &  (1D0 + drSM + drns )))

c	Print*, "--------------------------"

c      Print*,"Iteration",I
c      Print*,"MW",MWNEW
c      Print*, "drSM1Loop=", drSM1Loop
c      Print*, "drns=", drns
c      Print*, "SW2:",SW2
      IF (I .GT. 100) THEN
	 Write(*,*) "Keine Konvergenz nach", I, "Schritten"
         Print*,"MHH=",MHH
         Print*,"MHp=",MHp
         Print*,"MA0=",MA0
         Print*,"TB=",TB
         Print*,"Lamda5=",Lambda5
      MW=1D0
	 GOTO 20
      ELSE IF (DABS(MW-MWNEW) .GT. 0.000001D0 ) THEN
	 GOTO 10
      ElSE

        IF(SMflag.eq.0) THEN
        MW= MWNEW
        MW2=MW**2
        ELSE
C Conversion to running width definition
        call calcWWidth(GammaW)
        MW= MWNEW+GammaW**2/(2D0*MWNEW)
        MW2=MW**2
        MZ=MZsave
        MZ2=MZ**2
        ENDIF
        call SetWeakAngles


c SM contribution to delta r
c	Write(*,900) "Ergebnis nach ",I," Iterationen:"
 900  format (A,I2,A)
c	Print*, "--------------------------"
c        Write(*,*) "MW=", MW
c        Write(*,*) "drSM1Loop=", drSM1Loop
c        Write(*,*) "drSMho=",drSMho
c        Write(*,*) "drSM:",drSM
c        Write(*,*) "drns=", drns
c	Print*, "--------------------------"
	 GOTO 20
      END IF


 20   continue

      end
