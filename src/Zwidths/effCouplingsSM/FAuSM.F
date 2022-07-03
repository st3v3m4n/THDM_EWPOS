c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFAuSM(FAuSM)
	implicit none


	double complex FAuSM

#include "paraSM.h"
        FAuSM = Alfa/(288.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(-9 + (36 - 48*SW2)*SW2)*Finite +
     &      MU2*MW2*(-72*C0i(cc0,MU2,MU2,MZ2,Mh02,MU2,MZ2) +
     &         18*C0i(cc0,MZ2,MU2,MU2,MU2,MU2,MZ2) -
     &         36*C0i(cc1,MU2,MU2,MZ2,Mh02,MU2,MZ2) +
     &         SW2*(-48*C0i(cc0,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &            64*SW2*C0i(cc0,MZ2,MU2,MU2,MU2,MU2,MZ2))) +
     &      CW2*(-(12*MW2*SW2*(Finite*CKM(1,1)*CKMC(1,1))) +
     &         MU2*(-9*B0i(bb0,MU2,Mh02,MU2) -
     &            9*B0i(bb0,MU2,MU2,MZ2) +
     &            72*C0i(cc00,MU2,MU2,MZ2,Mh02,MU2,MZ2) +
     &            18*C0i(cc00,MU2,MZ2,MU2,Mh02,MU2,MU2) +
     &            18*C0i(cc00,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &            MZ2*(-9*C0i(cc1,MZ2,MU2,MU2,MU2,MU2,MZ2) -
     &               9*C0i(cc2,MU2,MZ2,MU2,Mh02,MU2,MU2)) +
     &            MU2*(-36*C0i(cc0,MU2,MZ2,MU2,Mh02,MU2,MU2) -
     &               18*C0i(cc0,MZ2,MU2,MU2,MU2,MU2,MZ2) -
     &               18*C0i(cc1,MU2,MZ2,MU2,Mh02,MU2,MU2) -
     &               18*C0i(cc2,MU2,MZ2,MU2,Mh02,MU2,MU2) -
     &               18*C0i(cc2,MZ2,MU2,MU2,MU2,MU2,MZ2)))) +
     &      MW2*(18*B0i(bb0,MU2,MU2,MZ2) -
     &         36*C0i(cc00,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &         SW2*(-72*B0i(bb0,MU2,MU2,MZ2) +
     &            144*C0i(cc00,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &            SW2*(96*B0i(bb0,MU2,MU2,MZ2) -
     &               192*C0i(cc00,MZ2,MU2,MU2,MU2,MU2,MZ2))) +
     &         MZ2*(18*C0i(cc1,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &            18*C0i(cc2,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &            SW2*(-72*C0i(cc1,MZ2,MU2,MU2,MU2,MU2,MZ2) -
     &               72*C0i(cc2,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &               SW2*(96*C0i(cc1,MZ2,MU2,MU2,MU2,MU2,MZ2) +
     &                  96*C0i(cc2,MZ2,MU2,MU2,MU2,MU2,MZ2))))))

        FAuSM = FAuSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (3*MW2*(Finite*CKM(1,1)*CKMC(1,1)) +
     &       MW2*(-6*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) +
     &          12*C0i(cc00,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &           CKMC(1,1) +
     &          SW2*(4*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) -
     &             8*C0i(cc00,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1))) +
     &       MU2*(3*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) -
     &          6*C0i(cc00,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &           CKMC(1,1) +
     &          12*MW2*SW2*
     &           (C0i(cc1,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          SW2*(-2*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) +
     &             6*C0i(cc00,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             4*C0i(cc00,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1))) +
     &       MD2*(3*MD2*(C0i(cc0,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          3*MU2*(C0i(cc0,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          SW2*(2*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) -
     &             6*C0i(cc00,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             4*C0i(cc00,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1)) +
     &          MW2*(-6*C0i(cc0,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             12*SW2*(C0i(cc0,MU2,MZ2,MU2,MD2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1)))) +
     &       CW2*(-(6*MW2*(Finite*CKM(1,1)*CKMC(1,1))) +
     &          6*MD2*(C0i(cc00,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          MW2*(12*B0i(bb0,MU2,MD2,MW2)*CKM(1,1)*CKMC(1,1) +
     &             24*C0i(cc00,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             12*MW2*(C0i(cc0,MU2,MZ2,MU2,MD2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1)) -
     &             12*MZ2*(C0i(cc1,MU2,MZ2,MU2,MD2,MW2,MW2)*
     &                CKM(1,1)*CKMC(1,1))) +
     &          MU2*(-6*C0i(cc00,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             MW2*(-12*C0i(cc0,MU2,MZ2,MU2,MD2,MW2,MW2)*
     &                 CKM(1,1)*CKMC(1,1) -
     &                12*C0i(cc1,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &                 CKMC(1,1)))))

        FAuSM = FAuSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MD2*(2*MZ2*SW2*
     &           (C0i(cc1,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          6*MU2*(C0i(cc2,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &             CKMC(1,1))) +
     &       MW2*(Finite*(3*CKM(1,2)*CKMC(1,2) -
     &            2*SW2*(CKM(1,2)*CKMC(1,2)))) +
     &       MU2*(3*B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2) +
     &          12*MW2*SW2*
     &           (C0i(cc2,MU2,MZ2,MU2,MD2,MW2,MW2)*CKM(1,1)*
     &             CKMC(1,1)) +
     &          SW2*(-2*B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2) +
     &             6*C0i(cc00,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2)) +
     &          MZ2*(3*C0i(cc1,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             2*SW2*(C0i(cc1,MZ2,MU2,MU2,MD2,MD2,MW2)*
     &                CKM(1,1)*CKMC(1,1)))) +
     &       MW2*(-6*B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2) +
     &          4*SW2*(B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1) -
     &             6*C0i(cc2,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &              CKMC(1,1) +
     &             SW2*(4*C0i(cc1,MZ2,MU2,MU2,MD2,MD2,MW2)*
     &                 CKM(1,1)*CKMC(1,1) +
     &                4*C0i(cc2,MZ2,MU2,MU2,MD2,MD2,MW2)*CKM(1,1)*
     &                 CKMC(1,1)))) +
     &       CW2*(-(6*MW2*(Finite*CKM(1,2)*CKMC(1,2))) +
     &          MU2*MW2*(-12*C0i(cc2,MU2,MZ2,MU2,MD2,MW2,MW2)*
     &              CKM(1,1)*CKMC(1,1) -
     &             12*C0i(cc0,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2)) +
     &          MW2*(12*B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2) +
     &             24*C0i(cc00,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             12*MW2*(C0i(cc0,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))) +
     &       MS2*(3*MS2*(C0i(cc0,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) +
     &          3*MU2*(C0i(cc0,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) +
     &          SW2*(2*B0i(bb0,MU2,MS2,MW2)*CKM(1,2)*CKMC(1,2) -
     &             6*C0i(cc00,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2)) +
     &          MW2*(-6*C0i(cc0,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             12*SW2*(C0i(cc0,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))))

        FAuSM = FAuSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (2*MB2*SW2*(B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3)) +
     &       MS2*(-(4*SW2*(C0i(cc00,MZ2,MU2,MU2,MS2,MS2,MW2)*
     &               CKM(1,2)*CKMC(1,2))) +
     &          2*MZ2*SW2*(C0i(cc1,MZ2,MU2,MU2,MS2,MS2,MW2)*
     &             CKM(1,2)*CKMC(1,2)) +
     &          6*MU2*(C0i(cc2,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &             CKMC(1,2))) +
     &       MW2*(Finite*(3*CKM(1,3)*CKMC(1,3) -
     &            2*SW2*(CKM(1,3)*CKMC(1,3)))) +
     &       MU2*(-6*C0i(cc00,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &           CKMC(1,2) +
     &          MW2*SW2*(12*C0i(cc1,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &              CKM(1,2)*CKMC(1,2) +
     &             12*C0i(cc2,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2)) +
     &          SW2*(4*C0i(cc00,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             2*B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3)) +
     &          MZ2*(3*C0i(cc1,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             2*SW2*(C0i(cc1,MZ2,MU2,MU2,MS2,MS2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))) +
     &       CW2*(6*MS2*(C0i(cc00,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &             CKM(1,2)*CKMC(1,2)) -
     &          6*MW2*(Finite*CKM(1,3)*CKMC(1,3)) +
     &          MW2*(12*B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3) -
     &             12*MZ2*(C0i(cc1,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &                CKM(1,2)*CKMC(1,2))) +
     &          MU2*(-6*C0i(cc00,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             MW2*(-12*C0i(cc1,MU2,MZ2,MU2,MS2,MW2,MW2)*
     &                 CKM(1,2)*CKMC(1,2) -
     &                12*C0i(cc2,MU2,MZ2,MU2,MS2,MW2,MW2)*CKM(1,2)*
     &                 CKMC(1,2)))) +
     &       MW2*(12*C0i(cc00,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &           CKMC(1,2) -
     &          6*B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3) +
     &          SW2*(-8*C0i(cc00,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             4*B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             6*C0i(cc2,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             SW2*(4*C0i(cc1,MZ2,MU2,MU2,MS2,MS2,MW2)*
     &                 CKM(1,2)*CKMC(1,2) +
     &                4*C0i(cc2,MZ2,MU2,MU2,MS2,MS2,MW2)*CKM(1,2)*
     &                 CKMC(1,2)))))

        FAuSM = FAuSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MB2*(3*MB2*(C0i(cc0,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     &          3*MU2*(C0i(cc0,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     &          2*MZ2*SW2*(C0i(cc1,MZ2,MU2,MU2,MB2,MB2,MW2)*
     &             CKM(1,3)*CKMC(1,3)) +
     &          SW2*(-6*C0i(cc00,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) -
     &             4*C0i(cc00,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &              CKMC(1,3)) +
     &          MW2*(-6*C0i(cc0,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &             12*SW2*(C0i(cc0,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &                CKM(1,3)*CKMC(1,3)))) +
     &       MU2*(3*B0i(bb0,MU2,MB2,MW2)*CKM(1,3)*CKMC(1,3) -
     &          6*C0i(cc00,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &           CKMC(1,3) +
     &          SW2*(6*C0i(cc00,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &             4*C0i(cc00,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &              CKMC(1,3)) +
     &          MW2*SW2*(12*C0i(cc1,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &              CKM(1,3)*CKMC(1,3) +
     &             12*C0i(cc2,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3)) +
     &          MZ2*(3*C0i(cc1,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &              CKMC(1,3) -
     &             2*SW2*(C0i(cc1,MZ2,MU2,MU2,MB2,MB2,MW2)*
     &                CKM(1,3)*CKMC(1,3)))) +
     &       MW2*(12*C0i(cc00,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &           CKMC(1,3) -
     &          8*SW2*(C0i(cc00,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     &          MZ2*(-6*C0i(cc1,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &             4*SW2*(C0i(cc1,MZ2,MU2,MU2,MB2,MB2,MW2)*
     &                CKM(1,3)*CKMC(1,3)))) +
     &       CW2*(6*MB2*(C0i(cc00,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &             CKM(1,3)*CKMC(1,3)) +
     &          MW2*(24*C0i(cc00,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &             12*MW2*(C0i(cc0,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &                CKM(1,3)*CKMC(1,3)) -
     &             12*MZ2*(C0i(cc1,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &                CKM(1,3)*CKMC(1,3))) +
     &          MU2*(-6*C0i(cc00,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &             MW2*(-12*C0i(cc0,MU2,MZ2,MU2,MB2,MW2,MW2)*
     &                 CKM(1,3)*CKMC(1,3) -
     &                12*C0i(cc1,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*
     &                 CKMC(1,3)))))

        FAuSM = FAuSM -
     &    (Alfa*CW2*MU2)/(4.D0*Pi*SW2)*
     &(C0i(cc2,MU2,MZ2,MU2,MB2,MW2,MW2)*CKM(1,3)*CKMC(1,3))+
     &    (Alfa*MZ2)/(12.D0*Pi)*
     &(C0i(cc2,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*CKMC(1,3))+
     &    (Alfa*MB2*MU2)/(8.D0*MW2*Pi*SW2)*
     &(C0i(cc2,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*CKMC(1,3))-
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)*
     &(C0i(cc2,MZ2,MU2,MU2,MB2,MB2,MW2)*CKM(1,3)*CKMC(1,3))+
     & 1/2.D0*((Alfa*(3 - 4*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*Finite+
     &       Alfa/(8.D0*Pi*SW2)*
     &(Finite*(CKM(1,1)*CKMC(1,1) + CKM(1,2)*CKMC(1,2) +
     &            CKM(1,3)*CKMC(1,3))) +
     &(Alfa*(MB2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &(CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MU2,MB2,MW2))) +
     &(Alfa*(MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MU2,MD2,MW2)))-
     &       (Alfa*MU2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MU2,Mh02,MU2)) +
     &          Re(B0i(bb1,MU2,Mh02,MU2))) +
     &       (Alfa*(MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MU2,MS2,MW2))) +
     &       ((Alfa*MU2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 4*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MU2,MU2,MZ2)) +
     &       (Alfa*MB2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MU2,MB2,MW2))) +
     &       (Alfa*MD2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MU2,MD2,MW2))) -
     &       (Alfa*MU2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MU2,Mh02,MU2)) +
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MU2,MS2,MW2))) -
     & (Alfa*MU*(-9*CW2*MU*MU2 + MU*MW2*(48 - 64*SW2)*SW2))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MU2,MU2,MZ2)) +
     & (Alfa*MU2*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MU2,MB2,MW2))) +
     & (Alfa*MU2*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MU2,MD2,MW2))) -
     &       (Alfa*MU2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MU2,Mh02,MU2)) +
     & (Alfa*MU2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &   (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MU2,MS2,MW2)))+
     & (Alfa*MU2*(9*CW2*MU2 + MW2*(9 + SW2*(-24 + 32*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MU2,MU2,MZ2)))

        FAuSM = FAuSM -
     &    (2*SW2)/3.D0*((Alfa*(3 - 4*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*
     &        Finite + Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,1)*CKMC(1,1) + CKM(1,2)*CKMC(1,2) +
     &            CKM(1,3)*CKMC(1,3))) +
     &       (Alfa*(MB2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MU2,MB2,MW2))) +
     &       (Alfa*(MD2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MU2,MD2,MW2))) -
     &       (Alfa*MU2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MU2,Mh02,MU2)) +
     &          Re(B0i(bb1,MU2,Mh02,MU2))) +
     &       (Alfa*(MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MU2,MS2,MW2))) +
     &       ((Alfa*MU2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 4*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MU2,MU2,MZ2)) +
     &       (Alfa*MB2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MU2,MB2,MW2))) +
     &       (Alfa*MD2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MU2,MD2,MW2))) -
     &       (Alfa*MU2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MU2,Mh02,MU2)) +
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MU2,MS2,MW2))) -
     & (Alfa*MU*(-9*CW2*MU*MU2 + MU*MW2*(48 - 64*SW2)*SW2))/
     &  (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MU2,MU2,MZ2)) +
     & (Alfa*MU2*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MU2,MB2,MW2))) +
     & (Alfa*MU2*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MU2,MD2,MW2))) -
     &  (Alfa*MU2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MU2,Mh02,MU2)) +
     & (Alfa*MU2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MU2,MS2,MW2)))+
     & (Alfa*MU2*(9*CW2*MU2 + MW2*(9 + SW2*(-24 + 32*SW2))))/
     &  (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MU2,MU2,MZ2)))

        FAuSM = FAuSM +
     &    (2*SW2)/3.D0*((Alfa*SW2)/(9.D0*CW2*Pi)*Finite +
     &       (Alfa*MU2)/(8.D0*MW2*Pi*SW2)*
     &        (CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MU2,MB2,MW2)))+
     &       (Alfa*MU2)/(8.D0*MW2*Pi*SW2)*
     &        (CKM(1,1)*CKMC(1,1)*Re(B0i(bb1,MU2,MD2,MW2)))-
     &       (Alfa*MU2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MU2,Mh02,MU2)) +
     &          Re(B0i(bb1,MU2,Mh02,MU2))) +
     &       (Alfa*MU2)/(8.D0*MW2*Pi*SW2)*
     &        (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MU2,MS2,MW2)))+
     &       (Alfa*(9*CW2*MU2 + 32*MW2*SW2**2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(bb1,MU2,MU2,MZ2)) +
     &       (Alfa*MB2*MU2)/(4.D0*MW2*Pi*SW2)*
     &(CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MU2,MB2,MW2))) +
     &       (Alfa*MD2*MU2)/(4.D0*MW2*Pi*SW2)*
     &   (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb0,MU2,MD2,MW2))) -
     &       (Alfa*MU2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MU2,Mh02,MU2)) +
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MU2,MS2,MW2))) -
     & (Alfa*MU*(-9*CW2*MU*MU2 + MU*MW2*(48 - 64*SW2)*SW2))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MU2,MU2,MZ2)) +
     & (Alfa*MU2*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MU2,MB2,MW2))) +
     & (Alfa*MU2*(MD2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,1)*CKMC(1,1)*Re(B0i(dbb1,MU2,MD2,MW2))) -
     &       (Alfa*MU2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MU2,Mh02,MU2)) +
     & (Alfa*MU2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MU2,MS2,MW2))) +
     & (Alfa*MU2*(9*CW2*MU2 + MW2*(9 + SW2*(-24 + 32*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MU2,MU2,MZ2)))

	end
