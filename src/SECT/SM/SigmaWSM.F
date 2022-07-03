c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  One-loop contribution to the W self-energy in the SM
C> @ingroup group_SECT
	double complex function SigmaWSM(p)
C>   squared momentum as argument for W self-energy
	double precision p

	Integer Gen3, Gen4


#include "paraSM.h"

        SigmaWSM = B0i(bb0,p**2,MW2,MZ2)*
     &     ((Alfa*(-2*CW2**2*MZ2 + MW2*SW2**2))/(4.D0*CW2*Pi*SW2) -
     &       (3*Alfa*CW2)/(4.D0*Pi*SW2)*p**2) -
     &    Alfa/(6.D0*Pi*SW2)*(Finite*p**2) +
     &    Alfa/(16.D0*Pi*SW2)*A0(Mh02) -
     &    (Alfa*(-7 + 4*CW2 + 4*SW2))/(8.D0*Pi*SW2)*A0(MW2) +
     &    (Alfa*(1 + 12*CW2))/(16.D0*Pi*SW2)*A0(MZ2) +
     &    (Alfa*MW2)/(4.D0*Pi*SW2)*B0i(bb0,p**2,Mh02,MW2) -
     &    (2*Alfa)/Pi*B0i(bb00,p**2,MW2,0.D0) -
     &    Alfa/(4.D0*Pi*SW2)*B0i(bb00,p**2,MW2,Mh02) -
     &    (Alfa*(1 + 8*CW2))/(4.D0*Pi*SW2)*B0i(bb00,p**2,MW2,MZ2) +
     &    Alfa/(2.D0*Pi)*(p**2*B0i(bb1,p**2,MW2,0.D0)) +
     &    (Alfa*CW2)/(2.D0*Pi*SW2)*(p**2*B0i(bb1,p**2,MW2,MZ2)) +
     &    Alfa*(B0i(bb0,p**2,0.D0,MW2)*
     &       (MW2/(4.D0*Pi) - 3/(4.D0*Pi)*p**2))

	LOOP(Gen3, 1,3,1)

        SigmaWSM = SigmaWSM +
     &    Alfa/(4.D0*Pi*SW2)*
     &     (-A0(Mf2(2,Gen3)) + 2*B0i(bb00,p**2,0.D0,Mf2(2,Gen3)) -
     &       p**2*B0i(bb1,p**2,0.D0,Mf2(2,Gen3)))

	ENDLOOP(Gen3)

	LOOP(Gen4, 1,3,1)
	LOOP(Gen3, 1,3,1)

        SigmaWSM = SigmaWSM +
     &    Alfa/(4.D0*Pi*SW2)*
     &     (-3*A0(Mf2(4,Gen4))*CKM(Gen3,Gen4)*CKMC(Gen3,Gen4) +
     &       6*B0i(bb00,p**2,Mf2(3,Gen3),Mf2(4,Gen4))*
     &        CKM(Gen3,Gen4)*CKMC(Gen3,Gen4) -
     &       3*p**2*B0i(bb1,p**2,Mf2(3,Gen3),Mf2(4,Gen4))*
     &        CKM(Gen3,Gen4)*CKMC(Gen3,Gen4) -
     &       3*B0i(bb0,p**2,Mf2(3,Gen3),Mf2(4,Gen4))*
     &        CKM(Gen3,Gen4)*CKMC(Gen3,Gen4)*Mf2(3,Gen3))

	ENDLOOP(Gen3)
	ENDLOOP(Gen4)

	end
