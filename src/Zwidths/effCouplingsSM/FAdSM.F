c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFAdSM(FAdSM)
	implicit none


	double complex FAdSM

#include "paraSM.h"
        FAdSM = Alfa/(288.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(9 + SW2*(-18 + 12*SW2))*Finite +
     &      MD2*MW2*(36*C0i(cc0,MD2,MZ2,MD2,MD2,Mh02,MZ2) -
     &         18*C0i(cc0,MZ2,MD2,MD2,MD2,MD2,MZ2) -
     &         36*C0i(cc1,MD2,MZ2,MD2,MD2,Mh02,MZ2) -
     &         36*C0i(cc2,MD2,MZ2,MD2,MD2,Mh02,MZ2) +
     &         SW2*(24*C0i(cc0,MZ2,MD2,MD2,MD2,MD2,MZ2) -
     &            16*SW2*C0i(cc0,MZ2,MD2,MD2,MD2,MD2,MZ2))) +
     &      CW2*(24*MW2*SW2*(Finite*CKM(1,1)*CKMC(1,1)) +
     &         MD2*(9*B0i(bb0,MD2,MD2,Mh02) +
     &            9*B0i(bb0,MD2,MD2,MZ2) -
     &            72*C0i(cc00,MD2,MZ2,MD2,MD2,Mh02,MZ2) -
     &            18*C0i(cc00,MZ2,MD2,MD2,MD2,MD2,Mh02) -
     &            18*C0i(cc00,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &            MZ2*(9*C0i(cc1,MZ2,MD2,MD2,MD2,MD2,Mh02) +
     &               9*C0i(cc1,MZ2,MD2,MD2,MD2,MD2,MZ2)) +
     &            MD2*(18*C0i(cc0,MZ2,MD2,MD2,MD2,MD2,Mh02) +
     &               18*C0i(cc0,MZ2,MD2,MD2,MD2,MD2,MZ2) -
     &               18*C0i(cc2,MZ2,MD2,MD2,MD2,MD2,Mh02) +
     &               18*C0i(cc2,MZ2,MD2,MD2,MD2,MD2,MZ2)))) +
     &      MW2*(-18*B0i(bb0,MD2,MD2,MZ2) +
     &         36*C0i(cc00,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &         SW2*(36*B0i(bb0,MD2,MD2,MZ2) -
     &            72*C0i(cc00,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &            SW2*(-24*B0i(bb0,MD2,MD2,MZ2) +
     &               48*C0i(cc00,MZ2,MD2,MD2,MD2,MD2,MZ2))) +
     &         MZ2*(-18*C0i(cc1,MZ2,MD2,MD2,MD2,MD2,MZ2) -
     &            18*C0i(cc2,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &            SW2*(36*C0i(cc1,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &               36*C0i(cc2,MZ2,MD2,MD2,MD2,MD2,MZ2) +
     &               SW2*(-24*C0i(cc1,MZ2,MD2,MD2,MD2,MD2,MZ2) -
     &                  24*C0i(cc2,MZ2,MD2,MD2,MD2,MD2,MZ2))))))

        FAdSM = FAdSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (-(3*MW2*(Finite*CKM(1,1)*CKMC(1,1))) +
     &       MD2*(-3*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) +
     &          6*C0i(cc00,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &           CKMC(1,1) -
     &          3*MU2*(C0i(cc0,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) -
     &          12*MW2*SW2*
     &           (C0i(cc1,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          SW2*(4*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) -
     &             6*C0i(cc00,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             8*C0i(cc00,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1))) +
     &       MW2*(6*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) -
     &          12*C0i(cc00,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &           CKMC(1,1) +
     &          SW2*(-8*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) +
     &             16*C0i(cc00,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1))) +
     &       MU2*(-(3*MU2*(C0i(cc0,MZ2,MD2,MD2,MU2,MU2,MW2)*
     &               CKM(1,1)*CKMC(1,1))) +
     &          SW2*(-4*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) +
     &             6*C0i(cc00,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             8*C0i(cc00,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1)) +
     &          MW2*(6*C0i(cc0,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             12*SW2*(C0i(cc0,MD2,MZ2,MD2,MU2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1)))) +
     &       CW2*(6*MW2*(Finite*CKM(1,1)*CKMC(1,1)) -
     &          6*MU2*(C0i(cc00,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          MW2*(-12*B0i(bb0,MD2,MU2,MW2)*CKM(1,1)*CKMC(1,1) -
     &             24*C0i(cc00,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             12*MW2*(C0i(cc0,MD2,MZ2,MD2,MU2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1)) +
     &             12*MZ2*(C0i(cc1,MD2,MZ2,MD2,MU2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1))) +
     &          MD2*(6*C0i(cc00,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             MW2*(12*C0i(cc0,MD2,MZ2,MD2,MU2,MW2,MW2)*
     &                 CKM(1,1)*CKMC(1,1) +
     &                12*C0i(cc1,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &                 CKMC(1,1)))))

        FAdSM = FAdSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (-(4*MU2*MZ2*SW2*
     &          (C0i(cc1,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &            CKMC(1,1))) +
     &       MW2*(Finite*(-3*CKM(2,1)*CKMC(2,1) +
     &            4*SW2*(CKM(2,1)*CKMC(2,1)))) +
     &       MD2*(-3*B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1) -
     &          12*MW2*SW2*
     &           (C0i(cc2,MD2,MZ2,MD2,MU2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) -
     &          6*MU2*(C0i(cc2,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          SW2*(4*B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1) -
     &             6*C0i(cc00,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1)) +
     &          MZ2*(-3*C0i(cc1,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             4*SW2*(C0i(cc1,MZ2,MD2,MD2,MU2,MU2,MW2)*
     &                CKM(1,1)*CKMC(1,1)))) +
     &       MW2*(6*B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1) -
     &          8*SW2*(B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1)) +
     &          MZ2*(6*C0i(cc1,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             6*C0i(cc2,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             SW2*(-8*C0i(cc1,MZ2,MD2,MD2,MU2,MU2,MW2)*
     &                 CKM(1,1)*CKMC(1,1) -
     &                8*C0i(cc2,MZ2,MD2,MD2,MU2,MU2,MW2)*CKM(1,1)*
     &                 CKMC(1,1)))) +
     &       CW2*(6*MW2*(Finite*CKM(2,1)*CKMC(2,1)) +
     &          MD2*MW2*(12*C0i(cc2,MD2,MZ2,MD2,MU2,MW2,MW2)*
     &              CKM(1,1)*CKMC(1,1) +
     &             12*C0i(cc0,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1)) +
     &          MW2*(-12*B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1) -
     &             24*C0i(cc00,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             12*MW2*(C0i(cc0,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1)))) +
     &       MC2*(-(3*MC2*(C0i(cc0,MZ2,MD2,MD2,MC2,MC2,MW2)*
     &               CKM(2,1)*CKMC(2,1))) -
     &          3*MD2*(C0i(cc0,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          SW2*(-4*B0i(bb0,MD2,MC2,MW2)*CKM(2,1)*CKMC(2,1) +
     &             6*C0i(cc00,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1)) +
     &          MW2*(6*C0i(cc0,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             12*SW2*(C0i(cc0,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1)))))

        FAdSM = FAdSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (-(4*MT2*SW2*(B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*
     &            CKMC(3,1))) +
     &       MC2*(8*SW2*(C0i(cc00,MZ2,MD2,MD2,MC2,MC2,MW2)*
     &             CKM(2,1)*CKMC(2,1)) -
     &          4*MZ2*SW2*(C0i(cc1,MZ2,MD2,MD2,MC2,MC2,MW2)*
     &             CKM(2,1)*CKMC(2,1)) -
     &          6*MD2*(C0i(cc2,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &             CKMC(2,1))) +
     &       MW2*(Finite*(-3*CKM(3,1)*CKMC(3,1) +
     &            4*SW2*(CKM(3,1)*CKMC(3,1)))) +
     &       MD2*(6*C0i(cc00,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &           CKMC(2,1) +
     &          MW2*SW2*(-12*C0i(cc1,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &              CKM(2,1)*CKMC(2,1) -
     &             12*C0i(cc2,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1)) +
     &          SW2*(-8*C0i(cc00,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             4*B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*CKMC(3,1)) +
     &          MZ2*(-3*C0i(cc1,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             4*SW2*(C0i(cc1,MZ2,MD2,MD2,MC2,MC2,MW2)*
     &                CKM(2,1)*CKMC(2,1)))) +
     &       CW2*(-(6*MC2*(C0i(cc00,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &               CKM(2,1)*CKMC(2,1))) +
     &          6*MW2*(Finite*CKM(3,1)*CKMC(3,1)) +
     &          MW2*(-12*B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*CKMC(3,1) +
     &             12*MZ2*(C0i(cc1,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1))) +
     &          MD2*(6*C0i(cc00,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             MW2*(12*C0i(cc1,MD2,MZ2,MD2,MC2,MW2,MW2)*
     &                 CKM(2,1)*CKMC(2,1) +
     &                12*C0i(cc2,MD2,MZ2,MD2,MC2,MW2,MW2)*CKM(2,1)*
     &                 CKMC(2,1)))) +
     &       MW2*(-12*C0i(cc00,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &           CKMC(2,1) +
     &          6*B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*CKMC(3,1) +
     &          SW2*(16*C0i(cc00,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             8*B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*CKMC(3,1)) +
     &          MZ2*(6*C0i(cc1,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             6*C0i(cc2,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             SW2*(-8*C0i(cc1,MZ2,MD2,MD2,MC2,MC2,MW2)*
     &                 CKM(2,1)*CKMC(2,1) -
     &                8*C0i(cc2,MZ2,MD2,MD2,MC2,MC2,MW2)*CKM(2,1)*
     &                 CKMC(2,1)))))

        FAdSM = FAdSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MT2*(-(3*MT2*(C0i(cc0,MZ2,MD2,MD2,MT2,MT2,MW2)*
     &               CKM(3,1)*CKMC(3,1))) -
     &          4*MZ2*SW2*(C0i(cc1,MZ2,MD2,MD2,MT2,MT2,MW2)*
     &             CKM(3,1)*CKMC(3,1)) +
     &          SW2*(6*C0i(cc00,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*
     &              CKMC(3,1) +
     &             8*C0i(cc00,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &              CKMC(3,1)) +
     &          MW2*(6*C0i(cc0,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &              CKMC(3,1) -
     &             12*SW2*(C0i(cc0,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &                CKM(3,1)*CKMC(3,1)))) +
     &       MD2*(-3*B0i(bb0,MD2,MT2,MW2)*CKM(3,1)*CKMC(3,1) +
     &          6*C0i(cc00,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &           CKMC(3,1) -
     &          3*MT2*(C0i(cc0,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &             CKMC(3,1)) +
     &          SW2*(-6*C0i(cc00,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*
     &              CKMC(3,1) -
     &             8*C0i(cc00,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &              CKMC(3,1)) +
     &          MW2*SW2*(-12*C0i(cc1,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &              CKM(3,1)*CKMC(3,1) -
     &             12*C0i(cc2,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*
     &              CKMC(3,1)) +
     &          MZ2*(-3*C0i(cc1,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &              CKMC(3,1) +
     &             4*SW2*(C0i(cc1,MZ2,MD2,MD2,MT2,MT2,MW2)*
     &                CKM(3,1)*CKMC(3,1)))) +
     &       MW2*(-12*C0i(cc00,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &           CKMC(3,1) +
     &          16*SW2*(C0i(cc00,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &             CKMC(3,1)) +
     &          MZ2*(6*C0i(cc1,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*
     &              CKMC(3,1) -
     &             8*SW2*(C0i(cc1,MZ2,MD2,MD2,MT2,MT2,MW2)*
     &                CKM(3,1)*CKMC(3,1)))) +
     &       CW2*(-(6*MT2*(C0i(cc00,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &               CKM(3,1)*CKMC(3,1))) +
     &          MW2*(-24*C0i(cc00,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &              CKM(3,1)*CKMC(3,1) -
     &             12*MW2*(C0i(cc0,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &                CKM(3,1)*CKMC(3,1)) +
     &             12*MZ2*(C0i(cc1,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &                CKM(3,1)*CKMC(3,1))) +
     &          MD2*(6*C0i(cc00,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*
     &              CKMC(3,1) +
     &             MW2*(12*C0i(cc0,MD2,MZ2,MD2,MT2,MW2,MW2)*
     &                 CKM(3,1)*CKMC(3,1) +
     &                12*C0i(cc1,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*
     &                 CKMC(3,1)))))

        FAdSM = FAdSM +
     &    (Alfa*CW2*MD2)/(4.D0*Pi*SW2)*
     &(C0i(cc2,MD2,MZ2,MD2,MT2,MW2,MW2)*CKM(3,1)*CKMC(3,1))-
     &    (Alfa*MZ2)/(6.D0*Pi)*
     &(C0i(cc2,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*CKMC(3,1))-
     &    (Alfa*MD2*MT2)/(8.D0*MW2*Pi*SW2)*
     &(C0i(cc2,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*CKMC(3,1))+
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)*
     &(C0i(cc2,MZ2,MD2,MD2,MT2,MT2,MW2)*CKM(3,1)*CKMC(3,1))-
     &    SW2/3.D0*((Alfa*SW2)/(36.D0*CW2*Pi)*Finite -
     &       2*MD*(-((Alfa*MC2*MD)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MD2,MC2,MW2))))+
     &            (Alfa*MD*MD2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MD2,MD2,Mh02)) +
     &  (Alfa*(-9*CW2*MD*MD2 + MD*MW2*(24 - 16*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MD2,MD2,MZ2))-
     &            (Alfa*MD*MT2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb0,MD2,MT2,MW2))) -
     &          (Alfa*MD*MU2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MD2,MU2,MW2)))) -
     &       MD*(-((Alfa*MD*(MC2 + MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MD2,MC2,MW2))))-
     &            (Alfa*MD*MD2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MD2,MD2,Mh02)) -
     &          (Alfa*MD*(9*CW2*MD2 +
     &                MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MD2,MD2,MZ2)) -
     & (Alfa*MD*(MD2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb1,MD2,MT2,MW2))) -
     & (Alfa*MD*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MD2,MU2,MW2)))) +
     &       (Alfa*MD2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MD2,MC2,MW2))) +
     & (Alfa*MD2)/(16.D0*MW2*Pi*SW2)*
     & Re(B0i(bb1,MD2,MD2,Mh02)) +
     & (Alfa*(9*CW2*MD2 + 8*MW2*SW2**2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(bb1,MD2,MD2,MZ2)) +
     &       (Alfa*MD2)/(8.D0*MW2*Pi*SW2)*
     &(CKM(3,1)*CKMC(3,1)*Re(B0i(bb1,MD2,MT2,MW2))) +
     & (Alfa*MD2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MD2,MU2,MW2))))

        FAdSM = FAdSM +
     &    SW2/3.D0*((Alfa*(3 - 2*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*Finite +
     &       Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,1)*CKMC(1,1) + CKM(2,1)*CKMC(2,1) +
     &            CKM(3,1)*CKMC(3,1))) -
     &       2*MD*(-((Alfa*MC2*MD)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MD2,MC2,MW2))))+
     &            (Alfa*MD*MD2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MD2,MD2,Mh02)) +
     & (Alfa*(-9*CW2*MD*MD2 + MD*MW2*(24 - 16*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MD2,MD2,MZ2))-
     &            (Alfa*MD*MT2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb0,MD2,MT2,MW2))) -
     &          (Alfa*MD*MU2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MD2,MU2,MW2)))) -
     & MD*(-((Alfa*MD*(MC2 + MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MD2,MC2,MW2))))-
     &            (Alfa*MD*MD2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MD2,MD2,Mh02)) -
     &          (Alfa*MD*(9*CW2*MD2 +
     &                MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MD2,MD2,MZ2)) -
     & (Alfa*MD*(MD2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb1,MD2,MT2,MW2))) -
     & (Alfa*MD*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MD2,MU2,MW2)))) +
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MD2,MC2,MW2))) +
     &       (Alfa*MD2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MD2,MD2,Mh02)) +
     &       ((Alfa*MD2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MD2,MD2,MZ2)) +
     &       (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(bb1,MD2,MT2,MW2))) +
     &       (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MD2,MU2,MW2))))

        FAdSM = FAdSM +
     &    1/2.D0*(-((Alfa*(3 - 2*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*
     &          Finite) - Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,1)*CKMC(1,1) + CKM(2,1)*CKMC(2,1) +
     &            CKM(3,1)*CKMC(3,1))) +
     &       2*MD*(-((Alfa*MC2*MD)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MD2,MC2,MW2))))+
     &            (Alfa*MD*MD2)/(16.D0*MW2*Pi*SW2)*
     & Re(B0i(dbb0,MD2,MD2,Mh02)) +
     & (Alfa*(-9*CW2*MD*MD2 + MD*MW2*(24 - 16*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MD2,MD2,MZ2))-
     & (Alfa*MD*MT2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb0,MD2,MT2,MW2))) -
     &  (Alfa*MD*MU2)/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MD2,MU2,MW2)))) +
     &  MD*(-((Alfa*MD*(MC2 + MD2 + 2*MW2))/
     &  (8.D0*MW2*Pi*SW2)*
     &  (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MD2,MC2,MW2))))-
     &            (Alfa*MD*MD2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MD2,MD2,Mh02)) -
     &          (Alfa*MD*(9*CW2*MD2 +
     &                MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MD2,MD2,MZ2)) -
     & (Alfa*MD*(MD2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(dbb1,MD2,MT2,MW2))) -
     & (Alfa*MD*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MD2,MU2,MW2)))) -
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MD2,MC2,MW2))) -
     &       (Alfa*MD2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MD2,MD2,Mh02)) +
     &       (-(Alfa*MD2)/(16.D0*MW2*Pi*SW2) -
     & (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MD2,MD2,MZ2)) -
     & (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,1)*CKMC(3,1)*Re(B0i(bb1,MD2,MT2,MW2))) -
     & (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MD2,MU2,MW2))))

	end
