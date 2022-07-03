c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFVbSM(FVbSM)
	implicit none


	double complex FVbSM

#include "paraSM.h"
        FVbSM = Alfa/(864.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(27 + SW2*(-54 + (36 - 16*SW2)*SW2))*Finite +
     &      MB2*MW2*(108*C0i(cc0,MB2,MZ2,MB2,MB2,Mh02,MZ2) -
     &         54*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2) -
     &         108*C0i(cc1,MB2,MZ2,MB2,MB2,Mh02,MZ2) +
     &         SW2*(-144*C0i(cc0,MB2,MZ2,MB2,MB2,Mh02,MZ2) +
     &            72*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &            144*C0i(cc1,MB2,MZ2,MB2,MB2,Mh02,MZ2))) +
     &      CW2*MB2*(-27*B0i(bb0,MB2,MB2,Mh02) -
     &         27*B0i(bb0,MB2,MB2,MZ2) +
     &         54*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,Mh02) +
     &         54*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &         SW2*(36*B0i(bb0,MB2,MB2,Mh02) +
     &            36*B0i(bb0,MB2,MB2,MZ2) -
     &            72*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,Mh02) -
     &            72*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2)) +
     &         MB2*(54*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,Mh02) -
     &            54*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &         SW2*(-72*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,Mh02) +
     &               72*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2))) +
     &         MZ2*(-27*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,Mh02) +
     &            SW2*(36*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,Mh02)+
     &               36*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2))))+
     &      MW2*(-54*B0i(bb0,MB2,MB2,MZ2) +
     &         108*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &         108*MZ2*SW2*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2)+
     &         SW2*(108*B0i(bb0,MB2,MB2,MZ2) -
     &            216*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &            SW2*(-72*B0i(bb0,MB2,MB2,MZ2) +
     &               144*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &               SW2*(32*B0i(bb0,MB2,MB2,MZ2) -
     &       64*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2))))))

        FVbSM = FVbSM +
     &    Alfa/(864.D0*CW2*MW2*Pi*SW2)*
     &     (MB2*MW2*(108*C0i(cc2,MB2,MZ2,MB2,MB2,Mh02,MZ2) +
     &          108*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &          SW2*(-144*C0i(cc2,MB2,MZ2,MB2,MB2,Mh02,MZ2)-
     &             216*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &             SW2*(144*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2)-
     &      64*SW2*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2)))) +
     &       MW2*MZ2*(-54*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2)-
     &          54*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &          SW2*(108*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &          SW2*(-72*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2)-
     &                72*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &         SW2*(32*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &        32*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2))))) +
     &       CW2*(MW2*(Finite*
     &             (-54*CKM(1,3)*CKMC(1,3) +
     &               72*SW2*(CKM(1,3)*CKMC(1,3)))) +
     &   MW2*(108*B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*CKMC(1,3) -
     &             144*SW2*
     &   (B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*CKMC(1,3))) +
     &          MU2*(-(72*SW2*
     &   (B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*CKMC(1,3))) -
     &             216*MW2*SW2*
     &   (C0i(cc0,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &                CKMC(1,3))) +
     &  MB2*(54*B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*CKMC(1,3) -
     &             27*MZ2*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2)-
     &             72*SW2*(B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*
     &                CKMC(1,3)) -
     &             288*MU2*SW2*
     &     (C0i(cc0,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &                CKMC(1,3)) +
     &     MB2*(-54*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,Mh02) -
     &   54*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &     SW2*(72*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,Mh02) +
     &     72*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2)))) +
     &    CW2*(108*MW2*(Finite*CKM(1,3)*CKMC(1,3)) +
     &             216*MB2*MW2*
     &  (C0i(cc0,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &                CKMC(1,3)) +
     &  MW2*(-216*B0i(bb0,MB2,MU2,MW2)*CKM(1,3)*
     &                 CKMC(1,3) -
     &                216*MW2*
     & (C0i(cc0,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &                   CKMC(1,3))))))

        FVbSM = FVbSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MU2*(-(3*MU2*(C0i(cc0,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &     CKM(1,3)*CKMC(1,3))) +
     & 6*MW2*(C0i(cc0,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) -
     &  4*MZ2*SW2*(C0i(cc1,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &             CKM(1,3)*CKMC(1,3)) +
     &  SW2*(6*C0i(cc00,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &  8*C0i(cc00,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3))) +
     & MB2*(-6*C0i(cc00,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &           CKMC(1,3) -
     & 8*MB2*SW2*(C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &             CKM(1,3)*CKMC(1,3)) +
     & SW2*(6*C0i(cc00,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     &  8*C0i(cc00,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3)) +
     & MZ2*(3*C0i(cc1,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3) -
     & 4*SW2*(C0i(cc1,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &                CKM(1,3)*CKMC(1,3))) +
     & MU2*(9*C0i(cc0,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3) -
     & 8*SW2*(C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &                CKM(1,3)*CKMC(1,3))) +
     & MW2*(-12*C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     & 16*SW2*(C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &                CKM(1,3)*CKMC(1,3)))) +
     & CW2*(-(6*MU2*(C0i(cc00,MB2,MZ2,MB2,MU2,MW2,MW2)*
     &               CKM(1,3)*CKMC(1,3))) +
     & MW2*(-24*C0i(cc00,MB2,MZ2,MB2,MU2,MW2,MW2)*
     &              CKM(1,3)*CKMC(1,3) +
     & 12*MZ2*(C0i(cc1,MB2,MZ2,MB2,MU2,MW2,MW2)*
     &                CKM(1,3)*CKMC(1,3))) +
     & MB2*(-6*C0i(cc00,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     & MW2*(-24*C0i(cc1,MB2,MZ2,MB2,MU2,MW2,MW2)*
     &                 CKM(1,3)*CKMC(1,3) -
     & 24*C0i(cc2,MB2,MZ2,MB2,MU2,MW2,MW2)*CKM(1,3)*
     &                 CKMC(1,3)))) +
     &  MW2*(-12*C0i(cc00,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &           CKMC(1,3) +
     & 16*SW2*(C0i(cc00,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     & MZ2*(6*C0i(cc1,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &              CKMC(1,3) +
     & SW2*(-8*C0i(cc1,MZ2,MB2,MB2,MU2,MU2,MW2)*
     &                 CKM(1,3)*CKMC(1,3) -
     & 8*C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &                 CKMC(1,3)))))

        FVbSM = FVbSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MW2*(Finite*(-3*CKM(2,3)*CKMC(2,3) +
     &            4*SW2*(CKM(2,3)*CKMC(2,3)))) +
     & MW2*(6*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) -
     & 12*C0i(cc00,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &           CKMC(2,3) +
     & 6*MZ2*(C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     & SW2*(-8*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) +
     & 16*C0i(cc00,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3))) +
     &  CW2*(6*MW2*(Finite*CKM(2,3)*CKMC(2,3)) -
     &  6*MC2*(C0i(cc00,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &             CKMC(2,3)) +
     &  MW2*(-12*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) -
     &   24*C0i(cc00,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     & 12*MW2*(C0i(cc0,MB2,MZ2,MB2,MC2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3))) +
     & MB2*(-6*C0i(cc00,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     & 12*MW2*(C0i(cc0,MB2,MZ2,MB2,MC2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     & MC2*(-(3*MC2*(C0i(cc0,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &              CKM(2,3)*CKMC(2,3))) +
     & SW2*(-4*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) +
     & 6*C0i(cc00,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     & 8*C0i(cc00,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3)) +
     & MW2*(6*C0i(cc0,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     & 12*SW2*(C0i(cc0,MB2,MZ2,MB2,MC2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     & MB2*(3*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) +
     & 6*MB2*(C0i(cc2,MZ2,MB2,MB2,MU2,MU2,MW2)*CKM(1,3)*
     &             CKMC(1,3)) +
     & SW2*(-4*B0i(bb0,MB2,MC2,MW2)*CKM(2,3)*CKMC(2,3) +
     & 6*C0i(cc00,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     & 8*C0i(cc00,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3)) +
     & MC2*(9*C0i(cc0,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     & 16*SW2*(C0i(cc0,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))))

        FVbSM = FVbSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (-(4*MC2*MZ2*SW2*
     &          (C0i(cc1,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &            CKMC(2,3))) +
     &       MW2*(Finite*(-3*CKM(3,3)*CKMC(3,3) +
     &            4*SW2*(CKM(3,3)*CKMC(3,3)))) +
     &       MT2*(-(4*SW2*(B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*
     &               CKMC(3,3))) -
     &          12*MW2*SW2*
     &           (C0i(cc0,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &             CKMC(3,3))) +
     &  MB2*(-6*C0i(cc00,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &           CKMC(2,3) +
     &          3*B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*CKMC(3,3) -
     &          8*MC2*SW2*(C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &             CKM(2,3)*CKMC(2,3)) -
     &  4*SW2*(B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*CKMC(3,3)) +
     &  MZ2*(3*C0i(cc1,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     & 4*SW2*(C0i(cc1,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &                CKM(2,3)*CKMC(2,3))) +
     & MB2*(6*C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) -
     & 8*SW2*(C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &                CKM(2,3)*CKMC(2,3))) +
     & MW2*(-12*C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     & 16*SW2*(C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &                CKM(2,3)*CKMC(2,3)))) +
     & MW2*(6*B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*CKMC(3,3) -
     &  8*SW2*(B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*CKMC(3,3)) +
     &  MZ2*(6*C0i(cc1,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &  CKMC(2,3) +
     &  6*C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &             SW2*(-8*C0i(cc1,MZ2,MB2,MB2,MC2,MC2,MW2)*
     &                 CKM(2,3)*CKMC(2,3) -
     &  8*C0i(cc2,MZ2,MB2,MB2,MC2,MC2,MW2)*CKM(2,3)*
     &                 CKMC(2,3)))) +
     &       CW2*(6*MW2*(Finite*CKM(3,3)*CKMC(3,3)) +
     &  MB2*MW2*(-24*C0i(cc1,MB2,MZ2,MB2,MC2,MW2,MW2)*
     &              CKM(2,3)*CKMC(2,3) -
     &   24*C0i(cc2,MB2,MZ2,MB2,MC2,MW2,MW2)*CKM(2,3)*
     &              CKMC(2,3) +
     &  12*C0i(cc0,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &              CKMC(3,3)) +
     & MW2*(-12*B0i(bb0,MB2,MT2,MW2)*CKM(3,3)*CKMC(3,3) +
     & 12*MZ2*(C0i(cc1,MB2,MZ2,MB2,MC2,MW2,MW2)*
     &                CKM(2,3)*CKMC(2,3)) -
     &             12*MW2*(C0i(cc0,MB2,MZ2,MB2,MT2,MW2,MW2)*
     &                CKM(3,3)*CKMC(3,3)))))

        FVbSM = FVbSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MT2*(-(3*MT2*(C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &               CKM(3,3)*CKMC(3,3))) +
     & 6*MW2*(C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &             CKMC(3,3)) -
     & 4*MZ2*SW2*(C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &             CKM(3,3)*CKMC(3,3)) +
     & SW2*(6*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &              CKMC(3,3) +
     & 8*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &              CKMC(3,3))) +
     & CW2*(-(6*MT2*(C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2)*
     &               CKM(3,3)*CKMC(3,3))) +
     & MW2*(-24*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2)*
     &              CKM(3,3)*CKMC(3,3) +
     & 12*MZ2*(C0i(cc1,MB2,MZ2,MB2,MT2,MW2,MW2)*
     &                CKM(3,3)*CKMC(3,3))) +
     & MB2*(-6*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &              CKMC(3,3) +
     & MW2*(-24*C0i(cc1,MB2,MZ2,MB2,MT2,MW2,MW2)*
     &                 CKM(3,3)*CKMC(3,3) -
     &  24*C0i(cc2,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &                 CKMC(3,3)))) +
     & MB2*(-6*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &           CKMC(3,3) -
     & 8*MB2*SW2*(C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &             CKM(3,3)*CKMC(3,3)) +
     &          16*MW2*SW2*
     & (C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &             CKMC(3,3)) +
     & SW2*(6*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2)*CKM(3,3)*
     &              CKMC(3,3) +
     &  8*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &              CKMC(3,3)) +
     & MZ2*(3*C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &              CKMC(3,3) -
     & 4*SW2*(C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &                CKM(3,3)*CKMC(3,3))) +
     & MT2*(9*C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &              CKMC(3,3) +
     & SW2*(-16*C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &                 CKM(3,3)*CKMC(3,3) -
     & 8*C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &                 CKMC(3,3)))) +
     & MW2*(-12*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &           CKMC(3,3) +
     & 16*SW2*(C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &             CKMC(3,3)) +
     & MZ2*(6*C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &              CKMC(3,3) +
     & SW2*(-8*C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2)*
     &                 CKM(3,3)*CKMC(3,3) -
     &  8*C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*
     &                 CKMC(3,3)))))

        FVbSM = FVbSM -
     &    (Alfa*MB2)/(4.D0*Pi*SW2)*
     & (C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*CKMC(3,3))+
     &    (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     & (C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*CKMC(3,3)) +
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)*
     & (C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2)*CKM(3,3)*CKMC(3,3)) +
     &    SW2/3.D0*((Alfa*SW2)/(36.D0*CW2*Pi)*Finite -
     &       2*MB*((Alfa*MB*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MB2,Mh02)) +
     & (Alfa*(-9*CW2*MB*MB2 + MB*MW2*(24 - 16*SW2)*SW2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MB2,MB2,MZ2))-
     &            (Alfa*MB*MC2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MB2,MC2,MW2))) -
     &          (Alfa*MB*MT2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb0,MB2,MT2,MW2))) -
     &          (Alfa*MB*MU2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MB2,MU2,MW2)))) -
     &       MB*(-((Alfa*MB*MB2)/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,MB2,MB2,Mh02))) -
     &          (Alfa*MB*(9*CW2*MB2 +
     &                MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MB2,MB2,MZ2)) -
     & (Alfa*MB*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MB2,MC2,MW2))) -
     & (Alfa*MB*(MB2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb1,MB2,MT2,MW2))) -
     & (Alfa*MB*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MB2,MU2,MW2)))) +
     &  (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &   Re(B0i(bb1,MB2,MB2,Mh02)) +
     &   (Alfa*(9*CW2*MB2 + 8*MW2*SW2**2))/
     &  (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(bb1,MB2,MB2,MZ2)) +
     &  (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MB2,MC2,MW2))) +
     & (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,3)*CKMC(3,3)*Re(B0i(bb1,MB2,MT2,MW2))) +
     &       (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MB2,MU2,MW2))))

        FVbSM = FVbSM +
     & SW2/3.D0*((Alfa*(3 - 2*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*Finite +
     &       Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,3)*CKMC(1,3) + CKM(2,3)*CKMC(2,3) +
     &            CKM(3,3)*CKMC(3,3))) -
     &       2*MB*((Alfa*MB*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MB2,Mh02)) +
     &   (Alfa*(-9*CW2*MB*MB2 + MB*MW2*(24 - 16*SW2)*SW2))/
     &   (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MB2,MB2,MZ2))-
     &   (Alfa*MB*MC2)/(8.D0*MW2*Pi*SW2)*
     &    (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MB2,MC2,MW2))) -
     &   (Alfa*MB*MT2)/(8.D0*MW2*Pi*SW2)*
     &   (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb0,MB2,MT2,MW2))) -
     &   (Alfa*MB*MU2)/(8.D0*MW2*Pi*SW2)*
     &   (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MB2,MU2,MW2)))) -
     &       MB*(-((Alfa*MB*MB2)/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,MB2,MB2,Mh02))) -
     &          (Alfa*MB*(9*CW2*MB2 +
     &                MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MB2,MB2,MZ2)) -
     & (Alfa*MB*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MB2,MC2,MW2))) -
     & (Alfa*MB*(MB2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb1,MB2,MT2,MW2))) -
     &  (Alfa*MB*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MB2,MU2,MW2)))) +
     &       (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MB2,MB2,Mh02)) +
     &       ((Alfa*MB2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MB2,MB2,MZ2)) +
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MB2,MC2,MW2))) +
     & (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,3)*CKMC(3,3)*Re(B0i(bb1,MB2,MT2,MW2))) +
     & (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MB2,MU2,MW2))))

        FVbSM = FVbSM +
     & 1/2.D0*(-((Alfa*(3 - 2*SW2)**2)/(144.D0*CW2*Pi*SW2)*
     &          Finite) - Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,3)*CKMC(1,3) + CKM(2,3)*CKMC(2,3) +
     &            CKM(3,3)*CKMC(3,3))) +
     &       2*MB*((Alfa*MB*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MB2,Mh02)) +
     &  (Alfa*(-9*CW2*MB*MB2 + MB*MW2*(24 - 16*SW2)*SW2))/
     &  (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MB2,MB2,MZ2))-
     &            (Alfa*MB*MC2)/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb0,MB2,MC2,MW2))) -
     &          (Alfa*MB*MT2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb0,MB2,MT2,MW2))) -
     &          (Alfa*MB*MU2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb0,MB2,MU2,MW2)))) +
     &       MB*(-((Alfa*MB*MB2)/(8.D0*MW2*Pi*SW2)*
     &             Re(B0i(dbb1,MB2,MB2,Mh02))) -
     &          (Alfa*MB*(9*CW2*MB2 +
     &  MW2*(9 + SW2*(-12 + 8*SW2))))/
     &  (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &  (Alfa*MB*(MB2 + MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(2,3)*CKMC(2,3)*Re(B0i(dbb1,MB2,MC2,MW2))) -
     &  (Alfa*MB*(MB2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(3,3)*CKMC(3,3)*Re(B0i(dbb1,MB2,MT2,MW2))) -
     &  (Alfa*MB*(MB2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &   (CKM(1,3)*CKMC(1,3)*Re(B0i(dbb1,MB2,MU2,MW2)))) -
     &       (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &        Re(B0i(bb1,MB2,MB2,Mh02)) +
     &       (-(Alfa*MB2)/(16.D0*MW2*Pi*SW2) -
     &          (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MB2,MB2,MZ2)) -
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        (CKM(2,3)*CKMC(2,3)*Re(B0i(bb1,MB2,MC2,MW2))) -
     &       (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        (CKM(3,3)*CKMC(3,3)*Re(B0i(bb1,MB2,MT2,MW2))) -
     &       (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &        (CKM(1,3)*CKMC(1,3)*Re(B0i(bb1,MB2,MU2,MW2))))

	end
