c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFVmuSM(FVmuSM)
	implicit none


	double complex FVmuSM

#include "paraSM.h"
        FVmuSM = Alfa/(32.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(1 + SW2*(-6 + (12 - 16*SW2)*SW2))*Finite +
     &      MM2*MW2*(8*C0i(cc0,MM2,MM2,MZ2,Mh02,MM2,MZ2) -
     &         2*C0i(cc0,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &         SW2*(-32*C0i(cc0,MM2,MM2,MZ2,Mh02,MM2,MZ2) +
     &            8*C0i(cc0,MZ2,MM2,MM2,MM2,MM2,MZ2))) +
     &      MW2*(-2*B0i(bb0,MM2,MM2,MZ2) +
     &         SW2*(12*B0i(bb0,MM2,MM2,MZ2) -
     &            24*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &            SW2*(-24*B0i(bb0,MM2,MM2,MZ2) +
     &               32*SW2*B0i(bb0,MM2,MM2,MZ2)))) +
     &      CW2*(-(2*MW2*Finite) +
     &         MW2*(4*B0i(bb0,MM2,0.D0,MW2) -
     &            8*C0i(cc00,MZ2,MM2,MM2,0.D0,0.D0,MW2)) +
     &         CW2*(4*MW2*Finite +
     &            MW2*(-8*B0i(bb0,MM2,0.D0,MW2) -
     &               16*C0i(cc00,MM2,MZ2,MM2,0.D0,MW2,MW2) -
     &               8*MW2*C0i(cc0,MM2,MZ2,MM2,0.D0,MW2,MW2)) +
     &            MM2*(-4*C0i(cc00,MM2,MZ2,MM2,0.D0,MW2,MW2) +
     &               8*MW2*C0i(cc0,MM2,MZ2,MM2,0.D0,MW2,MW2))) +
     &         MM2*(2*B0i(bb0,MM2,0.D0,MW2) -
     &            B0i(bb0,MM2,Mh02,MM2) - B0i(bb0,MM2,MM2,MZ2) +
     &            2*C0i(cc00,MM2,MZ2,MM2,Mh02,MM2,MM2) -
     &            4*C0i(cc00,MZ2,MM2,MM2,0.D0,0.D0,MW2) +
     &            SW2*(4*B0i(bb0,MM2,Mh02,MM2) +
     &               4*B0i(bb0,MM2,MM2,MZ2) +
     &               4*C0i(cc00,MM2,MZ2,MM2,0.D0,MW2,MW2) -
     &               8*C0i(cc00,MM2,MZ2,MM2,Mh02,MM2,MM2) -
     &               8*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2)) +
     &            MM2*(4*C0i(cc0,MM2,MZ2,MM2,Mh02,MM2,MM2) -
     &               2*C0i(cc0,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &               SW2*(-16*C0i(cc0,MM2,MZ2,MM2,Mh02,MM2,MM2) +
     &                  8*C0i(cc0,MZ2,MM2,MM2,MM2,MM2,MZ2))))))

        FVmuSM = FVmuSM +
     &    Alfa/(32.D0*CW2*MW2*Pi*SW2)*
     &     (MM2*MW2*(4*C0i(cc1,MM2,MM2,MZ2,Mh02,MM2,MZ2) +
     &          8*C0i(cc2,MM2,MM2,MZ2,Mh02,MM2,MZ2) +
     &          4*C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &          SW2*(-16*C0i(cc1,MM2,MM2,MZ2,Mh02,MM2,MZ2) -
     &             32*C0i(cc2,MM2,MM2,MZ2,Mh02,MM2,MZ2) -
     &             24*C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2))) +
     &       CW2*(MW2*MZ2*(4*C0i(cc1,MZ2,MM2,MM2,0.D0,0.D0,MW2) +
     &             4*C0i(cc2,MZ2,MM2,MM2,0.D0,0.D0,MW2)) +
     &          CW2*(8*MW2*MZ2*C0i(cc1,MM2,MZ2,MM2,0.D0,MW2,MW2) +
     &             MM2*MW2*
     &              (-16*C0i(cc1,MM2,MZ2,MM2,0.D0,MW2,MW2) -
     &                16*C0i(cc2,MM2,MZ2,MM2,0.D0,MW2,MW2))) +
     &          MM2*(2*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &             8*MW2*C0i(cc2,MZ2,MM2,MM2,0.D0,0.D0,MW2) +
     &             MZ2*(2*C0i(cc1,MZ2,MM2,MM2,0.D0,0.D0,MW2) -
     &                C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &                C0i(cc2,MM2,MZ2,MM2,Mh02,MM2,MM2) +
     &                SW2*(4*C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &                   4*C0i(cc2,MM2,MZ2,MM2,Mh02,MM2,MM2))) +
     &             MM2*(2*C0i(cc1,MM2,MZ2,MM2,Mh02,MM2,MM2) +
     &                2*C0i(cc2,MM2,MZ2,MM2,Mh02,MM2,MM2) +
     &                4*C0i(cc2,MZ2,MM2,MM2,0.D0,0.D0,MW2) +
     &                SW2*(-8*C0i(cc1,MM2,MZ2,MM2,Mh02,MM2,MM2) -
     &                   8*C0i(cc2,MM2,MZ2,MM2,Mh02,MM2,MM2) +
     &                   8*C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2))))) +
     &       MW2*(4*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &          SW2**2*(48*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &             64*SW2*C0i(cc00,MZ2,MM2,MM2,MM2,MM2,MZ2)) +
     &          MZ2*(-2*C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &             SW2*(12*C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &                12*C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &                SW2*(-24*C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &                   32*SW2*C0i(cc1,MZ2,MM2,MM2,MM2,MM2,MZ2))))
     &          ))

        FVmuSM = FVmuSM -
     &    (Alfa*MM2**2)/(16.D0*MW2*Pi*SW2)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &    (Alfa*MZ2)/(16.D0*CW2*Pi*SW2)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &    (3*Alfa*MM2*SW2)/(2.D0*CW2*Pi)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &    (3*Alfa*MZ2*SW2)/(4.D0*CW2*Pi)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) -
     &    (2*Alfa*MM2*SW2**2)/(CW2*Pi)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &    (Alfa*MZ2*SW2**2)/(CW2*Pi)*
     &     C0i(cc2,MZ2,MM2,MM2,MM2,MM2,MZ2) +
     &    1/2.D0*(-(Alfa/(8.D0*Pi*SW2)*Finite) -
     &       (Alfa*(1 - 2*SW2)**2)/(16.D0*CW2*Pi*SW2)*Finite -
     &       Alfa/(4.D0*Pi*SW2)*Re(B0i(bb1,MM2,0.D0,MW2)) +
     &       (Alfa*MM2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MM2,Mh02,MM2)) +
     &          Re(B0i(bb1,MM2,Mh02,MM2))) +
     &       (-(Alfa*MM2)/(16.D0*MW2*Pi*SW2) -
     &          (Alfa*(1 - 2*SW2)**2)/(8.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MM2,MM2,MZ2)) +
     &       (Alfa*MM2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MM2,Mh02,MM2)) +
     &       (Alfa*MM*(-(CW2*MM*MM2) + MM*MW2*(8 - 16*SW2)*SW2))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MM2,MM2,MZ2)) -
     &       (Alfa*MM2*(MM2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,0.D0,MW2)) +
     &       (Alfa*MM2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,Mh02,MM2)) -
     &       (Alfa*MM2*(CW2*MM2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MM2,MM2,MZ2))) +
     &    SW2*(Alfa/(8.D0*Pi*SW2)*Finite +
     &       (Alfa*(1 - 2*SW2)**2)/(16.D0*CW2*Pi*SW2)*Finite +
     &       Alfa/(4.D0*Pi*SW2)*Re(B0i(bb1,MM2,0.D0,MW2)) -
     &       (Alfa*MM2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MM2,Mh02,MM2)) +
     &          Re(B0i(bb1,MM2,Mh02,MM2))) +
     &       ((Alfa*MM2)/(16.D0*MW2*Pi*SW2) +
     &          (Alfa*(1 - 2*SW2)**2)/(8.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MM2,MM2,MZ2)) -
     &       (Alfa*MM2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MM2,Mh02,MM2)) -
     &       (Alfa*MM*(-(CW2*MM*MM2) + MM*MW2*(8 - 16*SW2)*SW2))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MM2,MM2,MZ2)) +
     &       (Alfa*MM2*(MM2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,0.D0,MW2)) -
     &       (Alfa*MM2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,Mh02,MM2)) +
     &       (Alfa*MM2*(CW2*MM2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MM2,MM2,MZ2)))

        FVmuSM = FVmuSM +
     &    SW2*((Alfa*SW2)/(4.D0*CW2*Pi)*Finite +
     &       (Alfa*MM2)/(8.D0*MW2*Pi*SW2)*Re(B0i(bb1,MM2,0.D0,MW2)) -
     &       (Alfa*MM2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MM2,Mh02,MM2)) +
     &          Re(B0i(bb1,MM2,Mh02,MM2))) +
     &       (Alfa*(CW2*MM2 + 8*MW2*SW2**2))/(16.D0*CW2*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MM2,MM2,MZ2)) -
     &       (Alfa*MM2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MM2,Mh02,MM2)) -
     &       (Alfa*MM*(-(CW2*MM*MM2) + MM*MW2*(8 - 16*SW2)*SW2))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MM2,MM2,MZ2)) +
     &       (Alfa*MM2*(MM2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,0.D0,MW2)) -
     &       (Alfa*MM2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MM2,Mh02,MM2)) +
     &       (Alfa*MM2*(CW2*MM2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &         (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MM2,MM2,MZ2)))

	end
