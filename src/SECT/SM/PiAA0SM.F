c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief One-loop contribution to the photon vacuum polarization in the SM.
C>
C> @ingroup group_SECT
C>
C> The vacuum polarization is evaluated for zero external momentum
	subroutine CalcPiAA0SM
	implicit none


	integer Gen3

#include "CTdeclarationSM.h"

#include "paraSM.h"

        PiAA0SM = Alfa*
     &    (-(1/(6.D0*Pi)*Finite) - 5/(4.D0*Pi)*B0i(bb0,0.D0,MW2,MW2) -
     &      1/(2.D0*Pi)*B0i(bb1,0.D0,MW2,MW2) -
     &      3/Pi*B0i(dbb00,0.D0,MW2,MW2))

	LOOP(Gen3, 1,3,1)

        PiAA0SM = PiAA0SM +
     &    Alfa*(-(1/Pi*B0i(bb1,0.D0,Mf2(2,Gen3),Mf2(2,Gen3))) -
     &       4/(3.D0*Pi)*B0i(bb1,0.D0,Mf2(3,Gen3),Mf2(3,Gen3)) -
     &       1/(3.D0*Pi)*B0i(bb1,0.D0,Mf2(4,Gen3),Mf2(4,Gen3)) +
     &       2/Pi*B0i(dbb00,0.D0,Mf2(2,Gen3),Mf2(2,Gen3)) +
     &       8/(3.D0*Pi)*B0i(dbb00,0.D0,Mf2(3,Gen3),Mf2(3,Gen3)) +
     &       2/(3.D0*Pi)*B0i(dbb00,0.D0,Mf2(4,Gen3),Mf2(4,Gen3)))

	ENDLOOP(Gen3)

	end
