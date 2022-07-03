c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief One-loop contribution to the mass counterterm of the Z boson in the SM.
C>
C> @ingroup group_SECT
C>
C> The mass counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
	subroutine CalcdMZ2SM
	implicit none


	integer Gen3

#include "CTdeclarationSM.h"

#include "paraSM.h"

        dMZ2SM = -((Alfa*CW2*MZ2)/(6.D0*Pi*SW2)*Finite) +
     &    (Alfa*(CW2*(9*CW2 - 2*SW2) + SW2**2))/(8.D0*CW2*Pi*SW2)*
     &     Re(A0(MW2)) + Alfa/(16.D0*CW2*Pi*SW2)*
     &     (Re(A0(Mh02)) + Re(A0(MZ2))) +
     &    (Alfa*MW2)/(4.D0*CW2**2*Pi*SW2)*
     &     Re(B0i(bb0,MZ2,Mh02,MZ2)) +
     &    (Alfa*(CW2*(-2*MW2 - 5*CW2*MZ2) + 2*MW2*SW2))/
     &      (4.D0*CW2*Pi*SW2)*Re(B0i(bb0,MZ2,MW2,MW2)) +
     &    (3*Alfa)/(4.D0*CW2*Pi*SW2)*Re(B0i(bb00,MZ2,0.D0,0.D0)) -
     &    Alfa/(4.D0*CW2*Pi*SW2)*Re(B0i(bb00,MZ2,Mh02,MZ2)) -
     &    (Alfa*(CW2*(9*CW2 - 2*SW2) + SW2**2))/(4.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb00,MZ2,MW2,MW2)) -
     &    (3*Alfa*MZ2)/(8.D0*CW2*Pi*SW2)*Re(B0i(bb1,MZ2,0.D0,0.D0)) -
     &    (Alfa*CW2*MZ2)/(2.D0*Pi*SW2)*Re(B0i(bb1,MZ2,MW2,MW2))

	LOOP(Gen3, 1,3,1)

        dMZ2SM = dMZ2SM -
     &    (Alfa*(1 + SW2*(-4 + 8*SW2)))/(8.D0*CW2*Pi*SW2)*
     &     Re(A0(Mf2(2,Gen3))) -
     &    (Alfa*(9 + SW2*(-24 + 32*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     Re(A0(Mf2(3,Gen3))) -
     &    (Alfa*(9 + SW2*(-12 + 8*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     Re(A0(Mf2(4,Gen3))) -
     &    Alfa/(8.D0*CW2*Pi*SW2)*
     &     (Mf2(2,Gen3)*Re(B0i(bb0,MZ2,Mf2(2,Gen3),Mf2(2,Gen3))))-
     &      (3*Alfa)/(8.D0*CW2*Pi*SW2)*
     &     (Mf2(3,Gen3)*Re(B0i(bb0,MZ2,Mf2(3,Gen3),Mf2(3,Gen3))))-
     &      (3*Alfa)/(8.D0*CW2*Pi*SW2)*
     &     (Mf2(4,Gen3)*Re(B0i(bb0,MZ2,Mf2(4,Gen3),Mf2(4,Gen3))))+
     &      (Alfa*(1 + SW2*(-4 + 8*SW2)))/(4.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb00,MZ2,Mf2(2,Gen3),Mf2(2,Gen3))) +
     &    (Alfa*(9 + SW2*(-24 + 32*SW2)))/(12.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb00,MZ2,Mf2(3,Gen3),Mf2(3,Gen3))) +
     &    (Alfa*(9 + SW2*(-12 + 8*SW2)))/(12.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb00,MZ2,Mf2(4,Gen3),Mf2(4,Gen3))) -
     &    (Alfa*MZ2*(1 + SW2*(-4 + 8*SW2)))/(8.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb1,MZ2,Mf2(2,Gen3),Mf2(2,Gen3))) -
     &    (Alfa*MZ2*(9 + SW2*(-24 + 32*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb1,MZ2,Mf2(3,Gen3),Mf2(3,Gen3))) -
     &    (Alfa*MZ2*(9 + SW2*(-12 + 8*SW2)))/(24.D0*CW2*Pi*SW2)*
     &     Re(B0i(bb1,MZ2,Mf2(4,Gen3),Mf2(4,Gen3)))

	ENDLOOP(Gen3)

	end
