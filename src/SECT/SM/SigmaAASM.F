c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  One-loop contribution to the photon self-energy in the SM
C> @ingroup group_SECT
	double complex function SigmaAASM(p)
C>   squared momentum as argument for photon self-energy
	double precision p

	Integer Gen3, Gen4


#include "paraSM.h"
        SigmaAASM = Alfa*
     &    (-(1/(6.D0*Pi)*(Finite*p**2)) + 3/(2.D0*Pi)*A0(MW2) -
     &      3/Pi*B0i(bb00,p**2,MW2,MW2) +
     &      p**2*(-(5/(4.D0*Pi)*B0i(bb0,p**2,MW2,MW2)) -
     &         1/(2.D0*Pi)*B0i(bb1,p**2,MW2,MW2)))

	LOOP(Gen3, 1,3,1)

        SigmaAASM = SigmaAASM +
     &    Alfa*(-(1/Pi*A0(Mf2(2,Gen3))) -
     &       4/(3.D0*Pi)*A0(Mf2(3,Gen3)) -
     &       1/(3.D0*Pi)*A0(Mf2(4,Gen3)) +
     &       2/Pi*B0i(bb00,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) +
     &       8/(3.D0*Pi)*B0i(bb00,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) +
     &       2/(3.D0*Pi)*B0i(bb00,p**2,Mf2(4,Gen3),Mf2(4,Gen3)) +
     &       p**2*(-(1/Pi*B0i(bb1,p**2,Mf2(2,Gen3),Mf2(2,Gen3))) -
     &          4/(3.D0*Pi)*B0i(bb1,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) -
     &          1/(3.D0*Pi)*B0i(bb1,p**2,Mf2(4,Gen3),Mf2(4,Gen3))))

	ENDLOOP(Gen3)

	end
