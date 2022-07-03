c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief This subroutine calculates the W-boson mass including only the one-loop corrections in the SM.
C>
C> The one-loop contribution to \f$\Delta r\f$ in the SM, \f$\Delta r^{(1)}_\mathrm{SM}\f$, is calculated internally
C> by calling the subroutine \ref calcdrsm1loop.
C>
C> @ingroup group_MW
      Subroutine CalcMWSM1Loop(drSM)
      Integer I
C> @param[out] The parameter drSM gives \f$\Delta r\f$ in the SM at the one-loop order.
      double precision drSM
      double precision MWNEW  ,drSM1Loop


#include "paraSM.h"


      MWNEW=80.385D0
      I=0
c Iteration
 10   CONTINUE
      I=I+1
      MW=MWNEW
      MW2=MW**2
      CW=MW/MZ
      CW2=MW2/MZ2
      SW2=1-CW2
      SW=SQRT(SW2)
c      Print*, "MWstart=", MW
      Call CalcdrSM1Loop(drSM1Loop)

c      Print*, "drSM1Loop=", drSM1Loop
      MWNEW = MZ*SQRT( 1D0/2D0 +
     & SQRT(1D0/4D0-pi*Alfa/(SQRT(2D0)*GF*MZ2)*
     &  (1D0 + drSM1Loop)))

c      Print*, "**********************************"
c      Print*, "MW-MWNEW=", MW-MWNEW
c      Print*, "**********************************"
      IF (I .GT. 20) THEN
	 Print*, "Keine Konvergenz nach", I, "Schritten"
	 GOTO 20
      ELSE IF (DABS(MW-MWNEW) .GT. 0.000001D0 ) THEN
	 GOTO 10
      ElSE
      MW= MWNEW
      MW2=MW**2
      CW=MW/MZ
      CW2=MW2/MZ2
      SW2=1-CW2
      SW=SQRT(SW2)

c SM contribution to delta r
      drSM= drSM1Loop
	Write(*,900) "Ergebnis nach ",I," Iterationen:"
 900  format (A,I2,A)
	Print*, "--------------------------"
	Write(*,*) "MW=", MW
        Write(*,*) "drSM1Loop=", drSM1Loop
	Print*, "--------------------------"
	 GOTO 20
      END IF


 20   continue

      end
