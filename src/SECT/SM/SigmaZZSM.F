c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  One-loop contribution to the Z self-energy in the SM
C> @ingroup group_SECT
	double complex function SigmaZZSM(p)
C>   squared momentum as argument for Z self-energy
	double precision p

	Integer Gen3, Gen4


#include "paraSM.h"

        SigmaZZSM = -((Alfa*CW2)/(6.D0*Pi*SW2)*(Finite*p**2)) +
     &    (Alfa*(CW2*(9*CW2 - 2*SW2) + SW2**2))/(8.D0*CW2*Pi*SW2)*
     &     A0(MW2) + Alfa/(16.D0*CW2*Pi*SW2)*(A0(Mh02) + A0(MZ2)) +
     &    (Alfa*MW2)/(4.D0*CW2**2*Pi*SW2)*B0i(bb0,p**2,Mh02,MZ2) +
     &    (3*Alfa)/(4.D0*CW2*Pi*SW2)*B0i(bb00,p**2,0.D0,0.D0) -
     &    Alfa/(4.D0*CW2*Pi*SW2)*B0i(bb00,p**2,Mh02,MZ2) -
     &    (Alfa*(CW2*(9*CW2 - 2*SW2) + SW2**2))/(4.D0*CW2*Pi*SW2)*
     &     B0i(bb00,p**2,MW2,MW2) -
     &    (3*Alfa)/(8.D0*CW2*Pi*SW2)*(p**2*B0i(bb1,p**2,0.D0,0.D0)) -
     &    (Alfa*CW2)/(2.D0*Pi*SW2)*(p**2*B0i(bb1,p**2,MW2,MW2)) +
     &    Alfa/(4.D0*CW2*Pi*SW2)*
     &     (B0i(bb0,p**2,MW2,MW2)*
     &       (2*MW2*SW2 + CW2*(-2*MW2 - 5*CW2*p**2)))

	LOOP(Gen3, 1,3,1)

        SigmaZZSM = SigmaZZSM -
     &    (Alfa*(1 + SW2*(-4 + 8*SW2)))/(8.D0*CW2*Pi*SW2)*
     &     A0(Mf2(2,Gen3)) -
     &    (Alfa*(9 + SW2*(-24 + 32*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     A0(Mf2(3,Gen3)) -
     &    (Alfa*(9 + SW2*(-12 + 8*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     A0(Mf2(4,Gen3)) +
     &    (Alfa*(1 + SW2*(-4 + 8*SW2)))/(4.D0*CW2*Pi*SW2)*
     &     B0i(bb00,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) +
     &    (Alfa*(9 + SW2*(-24 + 32*SW2)))/(12.D0*CW2*Pi*SW2)*
     &     B0i(bb00,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) +
     &    (Alfa*(9 + SW2*(-12 + 8*SW2)))/(12.D0*CW2*Pi*SW2)*
     &     B0i(bb00,p**2,Mf2(4,Gen3),Mf2(4,Gen3)) -
     &    (Alfa*(1 + SW2*(-4 + 8*SW2)))/(8.D0*CW2*Pi*SW2)*
     &     (p**2*B0i(bb1,p**2,Mf2(2,Gen3),Mf2(2,Gen3))) -
     &    (Alfa*(9 + SW2*(-24 + 32*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     (p**2*B0i(bb1,p**2,Mf2(3,Gen3),Mf2(3,Gen3))) -
     &    (Alfa*(9 + SW2*(-12 + 8*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     (p**2*B0i(bb1,p**2,Mf2(4,Gen3),Mf2(4,Gen3))) -
     &    Alfa/(8.D0*CW2*Pi*SW2)*
     &     (B0i(bb0,p**2,Mf2(2,Gen3),Mf2(2,Gen3))*Mf2(2,Gen3)) -
     &    (3*Alfa)/(8.D0*CW2*Pi*SW2)*
     &     (B0i(bb0,p**2,Mf2(3,Gen3),Mf2(3,Gen3))*Mf2(3,Gen3)) -
     &    (3*Alfa)/(8.D0*CW2*Pi*SW2)*
     &     (B0i(bb0,p**2,Mf2(4,Gen3),Mf2(4,Gen3))*Mf2(4,Gen3))

	ENDLOOP(Gen3)

	end
