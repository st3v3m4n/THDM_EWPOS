c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This Subroutine calculates the W-boson mass in the SM.
C>
C> The complete expression for \f$\Delta r\f$ in the SM with an explicit \f$M_W\f$
C> is used.
C>
C> @ingroup group_MW
      Subroutine CalcWmassSM(drSM)
C> @param[out] drsm is the contribution to \f$\Delta r\f$ in the SM. It is obtained by
C> calling the subroutine \ref rnorm internally.
      RealType drSM

      RealType MWNEW, GammaW

#include "paraSM.h"


      call calcAlfasMT()


      MWNEW=80.385D0
      I=0
c Iteration
      Print*,"MW",MWNEW
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


      call RNORM(MZFW,MW,MT,Mh0,DelAlfaMZ,AlfasMT,drSM)

      MWNEW = MZFW*SQRT( 1D0/2D0 +
     & SQRT(1D0/4D0-pi*Alfa/(SQRT(2D0)*GF*MZFW2)*
     &  (1D0 + drSM )))

      IF (I .GT. 100) THEN
	 Write(*,*) "Keine Konvergenz nach", I, "Schritten"
      MW=1D0
	 GOTO 20
      ELSE IF (DABS(MW-MWNEW) .GT. 0.000001D0 ) THEN
	 GOTO 10
      ElSE

C> Conversion to running width definition

      call calcWWidth(GammaW)
      MW= MWNEW+GammaW**2/(2D0*MWNEW)
      MW2=MW**2
      call SetWeakAngles

	Write(*,900) "Ergebnis nach ",I," Iterationen:"
 900  format (A,I2,A)
        Print*, "--------------------------"
        Write(*,*) "MW=", MW

        Write(*,*) "drSM:",drSM
        Print*, "--------------------------"
        GOTO 20
      END IF


 20   continue

      end
