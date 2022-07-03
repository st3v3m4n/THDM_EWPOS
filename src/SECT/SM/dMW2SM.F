c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief One-loop contribution to the mass counterterm of the W boson in the SM.
C>
C> @ingroup group_SECT
C>
C> The mass counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
	subroutine CalcdMW2SM
	implicit none


	integer Gen3, Gen4

#include "CTdeclarationSM.h"

#include "paraSM.h"

        dMW2SM = -((Alfa*MW2)/(6.D0*Pi*SW2)*Finite) +
     &    Alfa/(16.D0*Pi*SW2)*Re(A0(Mh02)) -
     &    (Alfa*(-7 + 4*CW2 + 4*SW2))/(8.D0*Pi*SW2)*Re(A0(MW2)) +
     &    (Alfa*(1 + 12*CW2))/(16.D0*Pi*SW2)*Re(A0(MZ2)) -
     &    (Alfa*MW2)/(2.D0*Pi)*Re(B0i(bb0,MW2,0.D0,MW2)) +
     &    (Alfa*MW2)/(4.D0*Pi*SW2)*Re(B0i(bb0,MW2,Mh02,MW2)) +
     &    ((-3*Alfa*CW2*MW2)/(4.D0*Pi*SW2) +
     &       (Alfa*(-2*CW2**2*MZ2 + MW2*SW2**2))/(4.D0*CW2*Pi*SW2))*
     &     Re(B0i(bb0,MW2,MW2,MZ2)) -
     &    (2*Alfa)/Pi*Re(B0i(bb00,MW2,MW2,0.D0)) -
     &    Alfa/(4.D0*Pi*SW2)*Re(B0i(bb00,MW2,MW2,Mh02)) -
     &    (Alfa*(1 + 8*CW2))/(4.D0*Pi*SW2)*
     &     Re(B0i(bb00,MW2,MW2,MZ2)) +
     &    (Alfa*MW2)/(2.D0*Pi)*Re(B0i(bb1,MW2,MW2,0.D0)) +
     &    (Alfa*CW2*MW2)/(2.D0*Pi*SW2)*Re(B0i(bb1,MW2,MW2,MZ2))

	LOOP(Gen3, 1,3,1)

        dMW2SM = dMW2SM -
     &    Alfa/(4.D0*Pi*SW2)*Re(A0(Mf2(2,Gen3))) +
     &    Alfa/(2.D0*Pi*SW2)*Re(B0i(bb00,MW2,0.D0,Mf2(2,Gen3))) -
     &    (Alfa*MW2)/(4.D0*Pi*SW2)*Re(B0i(bb1,MW2,0.D0,Mf2(2,Gen3)))

	ENDLOOP(Gen3)

	LOOP(Gen4, 1,3,1)
	LOOP(Gen3, 1,3,1)

        dMW2SM = dMW2SM -
     &    (3*Alfa)/(4.D0*Pi*SW2)*
     &(CKM(Gen3,Gen4)*CKMC(Gen3,Gen4)*Re(A0(Mf2(4,Gen4))))-
     &    (3*Alfa)/(4.D0*Pi*SW2)*
     &     (CKM(Gen3,Gen4)*CKMC(Gen3,Gen4)*Mf2(3,Gen3)*
     &       Re(B0i(bb0,MW2,Mf2(3,Gen3),Mf2(4,Gen4)))) +
     &    (3*Alfa)/(2.D0*Pi*SW2)*
     &     (CKM(Gen3,Gen4)*CKMC(Gen3,Gen4)*
     &       Re(B0i(bb00,MW2,Mf2(3,Gen3),Mf2(4,Gen4)))) -
     &    (3*Alfa*MW2)/(4.D0*Pi*SW2)*
     &     (CKM(Gen3,Gen4)*CKMC(Gen3,Gen4)*
     &       Re(B0i(bb1,MW2,Mf2(3,Gen3),Mf2(4,Gen4))))

	ENDLOOP(Gen3)
	ENDLOOP(Gen4)

	end
