c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  One-loop contribution to the photon-Z-mixing in the SM
C> @ingroup group_SECT
	double complex function SigmaAZSM(p)
C>   squared momentum as argument for the photon-Z-mixing
	double precision p

	Integer Gen3


#include "paraSM.h"
        SigmaAZSM = -((Alfa*CW)/(6.D0*Pi)*(Finite*p**2)/SW) +
     &    (Alfa*(5*CW2 - SW2))/(4.D0*CW*Pi)*A0(MW2)/SW +
     &    (Alfa*(-5*CW2 + SW2))/(2.D0*CW*Pi)*
     &     B0i(bb00,p**2,MW2,MW2)/SW -
     &    (Alfa*CW)/(2.D0*Pi)*(p**2*B0i(bb1,p**2,MW2,MW2))/SW +
     &    Alfa/(4.D0*CW*Pi)*(B0i(bb0,p**2,MW2,MW2)*
     &        (-2*MW2 - 5*CW2*p**2))/SW

	LOOP(Gen3, 1,3,1)

        SigmaAZSM = SigmaAZSM +
     &    Alfa/(12.D0*CW*Pi)*
     &     (-3*A0(Mf2(2,Gen3)) - 6*A0(Mf2(3,Gen3)) -
     &        3*A0(Mf2(4,Gen3)) +
     &        6*B0i(bb00,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) +
     &        12*B0i(bb00,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) +
     &        6*B0i(bb00,p**2,Mf2(4,Gen3),Mf2(4,Gen3)) +
     &        SW2*(12*A0(Mf2(2,Gen3)) + 16*A0(Mf2(3,Gen3)) +
     &           4*A0(Mf2(4,Gen3)) -
     &           24*B0i(bb00,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) -
     &           32*B0i(bb00,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) -
     &           8*B0i(bb00,p**2,Mf2(4,Gen3),Mf2(4,Gen3))) +
     &        p**2*(-3*B0i(bb1,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) -
     &           6*B0i(bb1,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) -
     &           3*B0i(bb1,p**2,Mf2(4,Gen3),Mf2(4,Gen3)) +
     &           SW2*(12*B0i(bb1,p**2,Mf2(2,Gen3),Mf2(2,Gen3)) +
     &              16*B0i(bb1,p**2,Mf2(3,Gen3),Mf2(3,Gen3)) +
     &              4*B0i(bb1,p**2,Mf2(4,Gen3),Mf2(4,Gen3)))))/SW

	ENDLOOP(Gen3)

	end
