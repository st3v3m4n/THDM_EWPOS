c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFAcSM(FAcSM)
	implicit none


	double complex FAcSM

#include "paraSM.h"
        FAcSM = Alfa/(288.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(-9 + (36 - 48*SW2)*SW2)*Finite +
     &      MC2*MW2*(-36*C0i(cc0,MC2,MZ2,MC2,MC2,Mh02,MZ2) +
     &         18*C0i(cc0,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &         36*C0i(cc1,MC2,MZ2,MC2,MC2,Mh02,MZ2) +
     &         36*C0i(cc2,MC2,MZ2,MC2,MC2,Mh02,MZ2) +
     &         SW2*(-48*C0i(cc0,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &            64*SW2*C0i(cc0,MZ2,MC2,MC2,MC2,MC2,MZ2))) +
     &      CW2*(-(12*MW2*SW2*(Finite*CKM(2,1)*CKMC(2,1))) +
     &         MC2*(-9*B0i(bb0,MC2,MC2,Mh02) -
     &            9*B0i(bb0,MC2,MC2,MZ2) +
     &            72*C0i(cc00,MC2,MZ2,MC2,MC2,Mh02,MZ2) +
     &            18*C0i(cc00,MZ2,MC2,MC2,MC2,MC2,Mh02) +
     &            18*C0i(cc00,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &            MZ2*(-9*C0i(cc1,MZ2,MC2,MC2,MC2,MC2,Mh02) -
     &               9*C0i(cc1,MZ2,MC2,MC2,MC2,MC2,MZ2)) +
     &            MC2*(-18*C0i(cc0,MZ2,MC2,MC2,MC2,MC2,Mh02) -
     &               18*C0i(cc0,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &               18*C0i(cc2,MZ2,MC2,MC2,MC2,MC2,Mh02) -
     &               18*C0i(cc2,MZ2,MC2,MC2,MC2,MC2,MZ2)))) +
     &      MW2*(18*B0i(bb0,MC2,MC2,MZ2) -
     &         36*C0i(cc00,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &         SW2*(-72*B0i(bb0,MC2,MC2,MZ2) +
     &            144*C0i(cc00,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &            SW2*(96*B0i(bb0,MC2,MC2,MZ2) -
     &               192*C0i(cc00,MZ2,MC2,MC2,MC2,MC2,MZ2))) +
     &         MZ2*(18*C0i(cc1,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &            18*C0i(cc2,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &            SW2*(-72*C0i(cc1,MZ2,MC2,MC2,MC2,MC2,MZ2) -
     &               72*C0i(cc2,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &               SW2*(96*C0i(cc1,MZ2,MC2,MC2,MC2,MC2,MZ2) +
     &                  96*C0i(cc2,MZ2,MC2,MC2,MC2,MC2,MZ2))))))

        FAcSM = FAcSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (3*MW2*(Finite*CKM(2,1)*CKMC(2,1)) +
     &       MW2*(-6*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) +
     &          12*C0i(cc00,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &           CKMC(2,1) +
     &          SW2*(4*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) -
     &             8*C0i(cc00,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1))) +
     &       MC2*(3*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) -
     &          6*C0i(cc00,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &           CKMC(2,1) +
     &          3*MD2*(C0i(cc0,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          12*MW2*SW2*
     &           (C0i(cc1,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          SW2*(-2*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) +
     &             6*C0i(cc00,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             4*C0i(cc00,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1))) +
     &       MD2*(3*MD2*(C0i(cc0,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          SW2*(2*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) -
     &             6*C0i(cc00,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             4*C0i(cc00,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1)) +
     &          MW2*(-6*C0i(cc0,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             12*SW2*(C0i(cc0,MC2,MZ2,MC2,MD2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1)))) +
     &       CW2*(-(6*MW2*(Finite*CKM(2,1)*CKMC(2,1))) +
     &          6*MD2*(C0i(cc00,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          MW2*(12*B0i(bb0,MC2,MD2,MW2)*CKM(2,1)*CKMC(2,1) +
     &             24*C0i(cc00,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             12*MW2*(C0i(cc0,MC2,MZ2,MC2,MD2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1)) -
     &             12*MZ2*(C0i(cc1,MC2,MZ2,MC2,MD2,MW2,MW2)*
     &                CKM(2,1)*CKMC(2,1))) +
     &          MC2*(-6*C0i(cc00,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             MW2*(-12*C0i(cc0,MC2,MZ2,MC2,MD2,MW2,MW2)*
     &                 CKM(2,1)*CKMC(2,1) -
     &                12*C0i(cc1,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &                 CKMC(2,1)))))

        FAcSM = FAcSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (2*MD2*MZ2*SW2*(C0i(cc1,MZ2,MC2,MC2,MD2,MD2,MW2)*
     &          CKM(2,1)*CKMC(2,1)) +
     &       MW2*(Finite*(3*CKM(2,2)*CKMC(2,2) -
     &            2*SW2*(CKM(2,2)*CKMC(2,2)))) +
     &       MC2*(3*B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2) +
     &          12*MW2*SW2*
     &           (C0i(cc2,MC2,MZ2,MC2,MD2,MW2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          6*MD2*(C0i(cc2,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &             CKMC(2,1)) +
     &          3*MS2*(C0i(cc0,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &             CKMC(2,2)) +
     &          SW2*(-2*B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2) +
     &             6*C0i(cc00,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          MZ2*(3*C0i(cc1,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             2*SW2*(C0i(cc1,MZ2,MC2,MC2,MD2,MD2,MW2)*
     &                CKM(2,1)*CKMC(2,1)))) +
     &       MW2*(-6*B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2) +
     &          4*SW2*(B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1) -
     &             6*C0i(cc2,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &              CKMC(2,1) +
     &             SW2*(4*C0i(cc1,MZ2,MC2,MC2,MD2,MD2,MW2)*
     &                 CKM(2,1)*CKMC(2,1) +
     &                4*C0i(cc2,MZ2,MC2,MC2,MD2,MD2,MW2)*CKM(2,1)*
     &                 CKMC(2,1)))) +
     &       CW2*(-(6*MW2*(Finite*CKM(2,2)*CKMC(2,2))) +
     &          MC2*MW2*(-12*C0i(cc2,MC2,MZ2,MC2,MD2,MW2,MW2)*
     &              CKM(2,1)*CKMC(2,1) -
     &             12*C0i(cc0,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          MW2*(12*B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2) +
     &             24*C0i(cc00,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             12*MW2*(C0i(cc0,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))) +
     &       MS2*(3*MS2*(C0i(cc0,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &             CKMC(2,2)) +
     &          SW2*(2*B0i(bb0,MC2,MS2,MW2)*CKM(2,2)*CKMC(2,2) -
     &             6*C0i(cc00,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          MW2*(-6*C0i(cc0,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             12*SW2*(C0i(cc0,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))))

        FAcSM = FAcSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (2*MB2*SW2*(B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3)) +
     &       MS2*(-(4*SW2*(C0i(cc00,MZ2,MC2,MC2,MS2,MS2,MW2)*
     &               CKM(2,2)*CKMC(2,2))) +
     &          2*MZ2*SW2*(C0i(cc1,MZ2,MC2,MC2,MS2,MS2,MW2)*
     &             CKM(2,2)*CKMC(2,2))) +
     &       MW2*(Finite*(3*CKM(2,3)*CKMC(2,3) -
     &            2*SW2*(CKM(2,3)*CKMC(2,3)))) +
     &       MC2*(-6*C0i(cc00,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &           CKMC(2,2) +
     &          6*MS2*(C0i(cc2,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &             CKMC(2,2)) +
     &          MW2*SW2*(12*C0i(cc1,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &              CKM(2,2)*CKMC(2,2) +
     &             12*C0i(cc2,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          SW2*(4*C0i(cc00,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             2*B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3)) +
     &          MZ2*(3*C0i(cc1,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             2*SW2*(C0i(cc1,MZ2,MC2,MC2,MS2,MS2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))) +
     &       CW2*(6*MS2*(C0i(cc00,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &             CKM(2,2)*CKMC(2,2)) -
     &          6*MW2*(Finite*CKM(2,3)*CKMC(2,3)) +
     &          MW2*(12*B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3) -
     &             12*MZ2*(C0i(cc1,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2))) +
     &          MC2*(-6*C0i(cc00,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             MW2*(-12*C0i(cc1,MC2,MZ2,MC2,MS2,MW2,MW2)*
     &                 CKM(2,2)*CKMC(2,2) -
     &                12*C0i(cc2,MC2,MZ2,MC2,MS2,MW2,MW2)*CKM(2,2)*
     &                 CKMC(2,2)))) +
     &       MW2*(12*C0i(cc00,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &           CKMC(2,2) -
     &          6*B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3) +
     &          SW2*(-8*C0i(cc00,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             4*B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             6*C0i(cc2,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             SW2*(4*C0i(cc1,MZ2,MC2,MC2,MS2,MS2,MW2)*
     &                 CKM(2,2)*CKMC(2,2) +
     &                4*C0i(cc2,MZ2,MC2,MC2,MS2,MS2,MW2)*CKM(2,2)*
     &                 CKMC(2,2)))))

        FAcSM = FAcSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MB2*(3*MB2*(C0i(cc0,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &             CKMC(2,3)) +
     &          3*MC2*(C0i(cc0,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &             CKMC(2,3)) +
     &          2*MZ2*SW2*(C0i(cc1,MZ2,MC2,MC2,MB2,MB2,MW2)*
     &             CKM(2,3)*CKMC(2,3)) +
     &          SW2*(-6*C0i(cc00,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     &             4*C0i(cc00,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &              CKMC(2,3)) +
     &          MW2*(-6*C0i(cc0,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             12*SW2*(C0i(cc0,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     &       MC2*(3*B0i(bb0,MC2,MB2,MW2)*CKM(2,3)*CKMC(2,3) -
     &          6*C0i(cc00,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &           CKMC(2,3) +
     &          SW2*(6*C0i(cc00,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             4*C0i(cc00,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &              CKMC(2,3)) +
     &          MW2*SW2*(12*C0i(cc1,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &              CKM(2,3)*CKMC(2,3) +
     &             12*C0i(cc2,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3)) +
     &          MZ2*(3*C0i(cc1,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     &             2*SW2*(C0i(cc1,MZ2,MC2,MC2,MB2,MB2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     &       MW2*(12*C0i(cc00,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &           CKMC(2,3) -
     &          8*SW2*(C0i(cc00,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &             CKMC(2,3)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             4*SW2*(C0i(cc1,MZ2,MC2,MC2,MB2,MB2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     &       CW2*(6*MB2*(C0i(cc00,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &             CKM(2,3)*CKMC(2,3)) +
     &          MW2*(24*C0i(cc00,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             12*MW2*(C0i(cc0,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3)) -
     &             12*MZ2*(C0i(cc1,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3))) +
     &          MC2*(-6*C0i(cc00,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             MW2*(-12*C0i(cc0,MC2,MZ2,MC2,MB2,MW2,MW2)*
     &                 CKM(2,3)*CKMC(2,3) -
     &                12*C0i(cc1,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*
     &                 CKMC(2,3)))))

        FAcSM = FAcSM -
     &    (Alfa*CW2*MC2)/(4.D0*Pi*SW2)*
     &(C0i(cc2,MC2,MZ2,MC2,MB2,MW2,MW2)*CKM(2,3)*CKMC(2,3))+
     &    (Alfa*MZ2)/(12.D0*Pi)*
     &(C0i(cc2,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*CKMC(2,3))+
     &    (Alfa*MB2*MC2)/(8.D0*MW2*Pi*SW2)*
     &(C0i(cc2,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*CKMC(2,3))-
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)*
     &(C0i(cc2,MZ2,MC2,MC2,MB2,MB2,MW2)*CKM(2,3)*CKMC(2,3))+
     &    (2*SW2)/3.D0*((Alfa*SW2)/(9.D0*CW2*Pi)*Finite -
     &       2*MC*(-((Alfa*MB2*MC)/(8.D0*MW2*Pi*SW2)*
     &    (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MC2,MB2,MW2))))+
     &            (Alfa*MC*MC2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MC2,MC2,Mh02)) +
     &  (Alfa*(-9*CW2*MC*MC2 + MC*MW2*(48 - 64*SW2)*SW2))/
     &  (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MC2,MC2,MZ2))-
     &            (Alfa*MC*MD2)/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MC2,MD2,MW2))) -
     &          (Alfa*MC*MS2)/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MC2,MS2,MW2)))) -
     &  MC*(-((Alfa*MC*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MC2,MB2,MW2))))-
     &            (Alfa*MC*MC2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MC2,MC2,Mh02)) -
     &          (Alfa*MC*(9*CW2*MC2 +
     &          MW2*(9 + SW2*(-24 + 32*SW2))))/
     &   (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MC2,MC2,MZ2)) -
     &  (Alfa*MC*(MC2 + MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MC2,MD2,MW2))) -
     & (Alfa*MC*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MC2,MS2,MW2)))) +
     &       (Alfa*MC2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MC2,MB2,MW2))) +
     &       (Alfa*MC2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MC2,MC2,Mh02)) +
     &       (Alfa*(9*CW2*MC2 + 32*MW2*SW2**2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(bb1,MC2,MC2,MZ2)) +
     &       (Alfa*MC2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MC2,MD2,MW2))) +
     &       (Alfa*MC2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MC2,MS2,MW2))))

        FAcSM = FAcSM +
     & 1/2.D0*((Alfa*(3 - 4*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*Finite +
     &       Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(2,1)*CKMC(2,1) + CKM(2,2)*CKMC(2,2) +
     &            CKM(2,3)*CKMC(2,3))) -
     &       2*MC*(-((Alfa*MB2*MC)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MC2,MB2,MW2))))+
     &            (Alfa*MC*MC2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MC2,MC2,Mh02)) +
     & (Alfa*(-9*CW2*MC*MC2 + MC*MW2*(48 - 64*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MC2,MC2,MZ2))-
     &            (Alfa*MC*MD2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MC2,MD2,MW2))) -
     &          (Alfa*MC*MS2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MC2,MS2,MW2)))) -
     & MC*(-((Alfa*MC*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MC2,MB2,MW2))))-
     & (Alfa*MC*MC2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MC2,MC2,Mh02)) -
     &          (Alfa*MC*(9*CW2*MC2 +
     & MW2*(9 + SW2*(-24 + 32*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MC2,MC2,MZ2)) -
     & (Alfa*MC*(MC2 + MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MC2,MD2,MW2))) -
     & (Alfa*MC*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MC2,MS2,MW2)))) +
     &       (Alfa*(MB2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MC2,MB2,MW2))) +
     &       (Alfa*MC2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MC2,MC2,Mh02)) +
     &       ((Alfa*MC2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 4*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MC2,MC2,MZ2)) +
     &       (Alfa*(MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MC2,MD2,MW2))) +
     &       (Alfa*(MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MC2,MS2,MW2))))

        FAcSM = FAcSM -
     &    (2*SW2)/3.D0*((Alfa*(3 - 4*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*
     &        Finite + Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(2,1)*CKMC(2,1) + CKM(2,2)*CKMC(2,2) +
     &            CKM(2,3)*CKMC(2,3))) -
     &       2*MC*(-((Alfa*MB2*MC)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MC2,MB2,MW2))))+
     &            (Alfa*MC*MC2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MC2,MC2,Mh02)) +
     & (Alfa*(-9*CW2*MC*MC2 + MC*MW2*(48 - 64*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MC2,MC2,MZ2))-
     &            (Alfa*MC*MD2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb0,MC2,MD2,MW2))) -
     &          (Alfa*MC*MS2)/(8.D0*MW2*Pi*SW2)*
     &(CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MC2,MS2,MW2)))) -
     &MC*(-((Alfa*MC*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MC2,MB2,MW2))))-
     &            (Alfa*MC*MC2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MC2,MC2,Mh02)) -
     &          (Alfa*MC*(9*CW2*MC2 +
     &                MW2*(9 + SW2*(-24 + 32*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MC2,MC2,MZ2)) -
     & (Alfa*MC*(MC2 + MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(dbb1,MC2,MD2,MW2))) -
     & (Alfa*MC*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MC2,MS2,MW2)))) +
     &       (Alfa*(MB2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MC2,MB2,MW2))) +
     &       (Alfa*MC2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MC2,MC2,Mh02)) +
     &       ((Alfa*MC2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 4*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MC2,MC2,MZ2)) +
     &       (Alfa*(MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,1)*CKMC(2,1)*Re(B0i(bb1,MC2,MD2,MW2))) +
     &       (Alfa*(MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MC2,MS2,MW2))))

	end
