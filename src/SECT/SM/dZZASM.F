c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief One-loop contribution to the off-diagonal element
C> of the field counterterm matrix of the Z boson and the photon in the SM.
C>
C> @ingroup group_SECT
C>
C> The field counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
	subroutine CalcdZZASM
	implicit none


	integer Gen3

#include "CTdeclarationSM.h"

#include "paraSM.h"

        dZZASM = (Alfa*(5*CW2 - SW2))/(2.D0*CW*MZ2*Pi)*
     &     Re(A0(MW2))/SW -
     &    (Alfa*MW2)/(CW*MZ2*Pi)*Re(B0i(bb0,0.D0,MW2,MW2))/SW +
     &    (Alfa*(-5*CW2 + SW2))/(CW*MZ2*Pi)*
     &     Re(B0i(bb00,0.D0,MW2,MW2))/SW

	LOOP(Gen3, 1,3,1)

        dZZASM = dZZASM +
     &    2/MZ2*((Alfa*(-1 + 4*SW2))/(4.D0*CW*Pi)*
     &        Re(A0(Mf2(2,Gen3)))/SW +
     &       (Alfa*(-3 + 8*SW2))/(6.D0*CW*Pi)*
     &        Re(A0(Mf2(3,Gen3)))/SW +
     &       (Alfa*(-3 + 4*SW2))/(12.D0*CW*Pi)*
     &        Re(A0(Mf2(4,Gen3)))/SW -
     &       (Alfa*(-1 + 4*SW2))/(2.D0*CW*Pi)*
     &        Re(B0i(bb00,0.D0,Mf2(2,Gen3),Mf2(2,Gen3)))/SW -
     &       (Alfa*(-3 + 8*SW2))/(3.D0*CW*Pi)*
     &        Re(B0i(bb00,0.D0,Mf2(3,Gen3),Mf2(3,Gen3)))/SW -
     &       (Alfa*(-3 + 4*SW2))/(6.D0*CW*Pi)*
     &        Re(B0i(bb00,0.D0,Mf2(4,Gen3),Mf2(4,Gen3)))/SW)

	ENDLOOP(Gen3)

	end
