c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFVeSM(FVeSM)
	implicit none


	double complex FVeSM

#include "paraSM.h"
        FVeSM = Alfa/(32.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(1 + SW2*(-6 + (12 - 16*SW2)*SW2))*Finite +
     &      ME2*MW2*(4*C0i(cc0,ME2,MZ2,ME2,ME2,Mh02,MZ2) -
     &         2*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &         SW2*(-16*C0i(cc0,ME2,MZ2,ME2,ME2,Mh02,MZ2) +
     &            8*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,MZ2))) +
     &      MW2*(-2*B0i(bb0,ME2,ME2,MZ2) +
     &         SW2*(12*B0i(bb0,ME2,ME2,MZ2) -
     &            24*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &            SW2*(-24*B0i(bb0,ME2,ME2,MZ2) +
     &               32*SW2*B0i(bb0,ME2,ME2,MZ2)))) +
     &      CW2*(-(2*MW2*Finite) +
     &         MW2*(4*B0i(bb0,ME2,0.D0,MW2) -
     &            8*C0i(cc00,MZ2,ME2,ME2,0.D0,0.D0,MW2)) +
     &         CW2*(4*MW2*Finite +
     &            MW2*(-8*B0i(bb0,ME2,0.D0,MW2) -
     &               16*C0i(cc00,ME2,MZ2,ME2,0.D0,MW2,MW2) -
     &               8*MW2*C0i(cc0,ME2,MZ2,ME2,0.D0,MW2,MW2)) +
     &            ME2*(-4*C0i(cc00,ME2,MZ2,ME2,0.D0,MW2,MW2) +
     &               8*MW2*C0i(cc0,ME2,MZ2,ME2,0.D0,MW2,MW2))) +
     &         ME2*(2*B0i(bb0,ME2,0.D0,MW2) -
     &            B0i(bb0,ME2,ME2,Mh02) - B0i(bb0,ME2,ME2,MZ2) -
     &            4*C0i(cc00,MZ2,ME2,ME2,0.D0,0.D0,MW2) +
     &            2*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,Mh02) +
     &            SW2*(4*B0i(bb0,ME2,ME2,Mh02) +
     &               4*B0i(bb0,ME2,ME2,MZ2) +
     &               4*C0i(cc00,ME2,MZ2,ME2,0.D0,MW2,MW2) -
     &               8*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,Mh02) -
     &               8*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2)) +
     &            ME2*(2*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,Mh02) -
     &               2*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &               SW2*(-8*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,Mh02) +
     &                  8*C0i(cc0,MZ2,ME2,ME2,ME2,ME2,MZ2))))))

        FVeSM = FVeSM +
     &    Alfa/(32.D0*CW2*MW2*Pi*SW2)*
     &     (ME2*MW2*(-4*C0i(cc1,ME2,MZ2,ME2,ME2,Mh02,MZ2) +
     &          4*C0i(cc2,ME2,MZ2,ME2,ME2,Mh02,MZ2) +
     &          4*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &          SW2*(16*C0i(cc1,ME2,MZ2,ME2,ME2,Mh02,MZ2) -
     &             16*C0i(cc2,ME2,MZ2,ME2,ME2,Mh02,MZ2) -
     &             24*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2))) +
     &       CW2*(MW2*MZ2*(4*C0i(cc1,MZ2,ME2,ME2,0.D0,0.D0,MW2) +
     &             4*C0i(cc2,MZ2,ME2,ME2,0.D0,0.D0,MW2)) +
     &          CW2*(8*MW2*MZ2*C0i(cc1,ME2,MZ2,ME2,0.D0,MW2,MW2) +
     &             ME2*MW2*
     &              (-16*C0i(cc1,ME2,MZ2,ME2,0.D0,MW2,MW2) -
     &                16*C0i(cc2,ME2,MZ2,ME2,0.D0,MW2,MW2))) +
     &          ME2*(2*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2) -
     &             8*MW2*C0i(cc2,MZ2,ME2,ME2,0.D0,0.D0,MW2) +
     &             MZ2*(2*C0i(cc1,MZ2,ME2,ME2,0.D0,0.D0,MW2) -
     &                C0i(cc1,MZ2,ME2,ME2,ME2,ME2,Mh02) -
     &                C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &                SW2*(4*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,Mh02) +
     &                   4*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2))) +
     &             ME2*(4*C0i(cc2,MZ2,ME2,ME2,0.D0,0.D0,MW2) -
     &                2*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,Mh02) -
     &                2*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &                SW2*(8*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,Mh02) +
     &                   8*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2))))) +
     &       MW2*(4*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &          SW2**2*(48*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2) -
     &             64*SW2*C0i(cc00,MZ2,ME2,ME2,ME2,ME2,MZ2)) +
     &          MZ2*(-2*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2) -
     &             2*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &             SW2*(12*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &                12*C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &                SW2*(-24*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &                   32*SW2*C0i(cc1,MZ2,ME2,ME2,ME2,ME2,MZ2))))
     &          ))

        FVeSM = FVeSM +
     &    (3*Alfa*ME2*SW2)/(2.D0*CW2*Pi)*
     &     C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) -
     &    (3*Alfa*MZ2*SW2)/(4.D0*CW2*Pi)*
     &     C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) -
     &    (2*Alfa*ME2*SW2**2)/(CW2*Pi)*
     &     C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &    (Alfa*MZ2*SW2**2)/(CW2*Pi)*
     &     C0i(cc2,MZ2,ME2,ME2,ME2,ME2,MZ2) +
     &    SW2*(Alfa/(8.D0*Pi*SW2)*Finite +
     &       (Alfa*(1 - 2*SW2)**2)/(16.D0*CW2*Pi*SW2)*Finite -
     &       2*ME*((Alfa*ME*ME2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,ME2,ME2,Mh02)) +
     &          (Alfa*(-(CW2*ME*ME2) + ME*MW2*(8 - 16*SW2)*SW2))/
     &            (16.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,ME2,ME2,MZ2)))-
     &         ME*(-((Alfa*ME*(ME2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,ME2,0.D0,MW2))) -
     &          (Alfa*ME*ME2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,ME2,ME2,Mh02)) -
     &          (Alfa*ME*(CW2*ME2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &            (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,ME2,ME2,MZ2))) +
     &       Alfa/(4.D0*Pi*SW2)*Re(B0i(bb1,ME2,0.D0,MW2)) +
     &       (Alfa*ME2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,ME2,ME2,Mh02)) +
     &       ((Alfa*ME2)/(16.D0*MW2*Pi*SW2) +
     &          (Alfa*(1 - 2*SW2)**2)/(8.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,ME2,ME2,MZ2))) +
     &    SW2*((Alfa*SW2)/(4.D0*CW2*Pi)*Finite -
     &       2*ME*((Alfa*ME*ME2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,ME2,ME2,Mh02)) +
     &          (Alfa*(-(CW2*ME*ME2) + ME*MW2*(8 - 16*SW2)*SW2))/
     &            (16.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,ME2,ME2,MZ2)))-
     &         ME*(-((Alfa*ME*(ME2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,ME2,0.D0,MW2))) -
     &          (Alfa*ME*ME2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,ME2,ME2,Mh02)) -
     &          (Alfa*ME*(CW2*ME2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &            (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,ME2,ME2,MZ2))) +
     &       (Alfa*ME2)/(8.D0*MW2*Pi*SW2)*Re(B0i(bb1,ME2,0.D0,MW2)) +
     &       (Alfa*ME2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,ME2,ME2,Mh02)) +
     &       (Alfa*(CW2*ME2 + 8*MW2*SW2**2))/(16.D0*CW2*MW2*Pi*SW2)*
     &        Re(B0i(bb1,ME2,ME2,MZ2)))

        FVeSM = FVeSM +
     &    1/2.D0*(-(Alfa/(8.D0*Pi*SW2)*Finite) -
     &       (Alfa*(1 - 2*SW2)**2)/(16.D0*CW2*Pi*SW2)*Finite +
     &       2*ME*((Alfa*ME*ME2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,ME2,ME2,Mh02)) +
     &          (Alfa*(-(CW2*ME*ME2) + ME*MW2*(8 - 16*SW2)*SW2))/
     &            (16.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,ME2,ME2,MZ2)))+
     &         ME*(-((Alfa*ME*(ME2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,ME2,0.D0,MW2))) -
     &          (Alfa*ME*ME2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,ME2,ME2,Mh02)) -
     &          (Alfa*ME*(CW2*ME2 + MW2*(1 + SW2*(-4 + 8*SW2))))/
     &            (8.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,ME2,ME2,MZ2))) -
     &       Alfa/(4.D0*Pi*SW2)*Re(B0i(bb1,ME2,0.D0,MW2)) -
     &       (Alfa*ME2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,ME2,ME2,Mh02)) +
     &       (-(Alfa*ME2)/(16.D0*MW2*Pi*SW2) -
     &          (Alfa*(1 - 2*SW2)**2)/(8.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,ME2,ME2,MZ2)))

	end
