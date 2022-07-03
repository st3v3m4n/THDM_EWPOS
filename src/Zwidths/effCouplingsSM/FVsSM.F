c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFVsSM(FVsSM)
	implicit none


	double complex FVsSM

#include "paraSM.h"
        FVsSM = Alfa/(864.D0*CW2*MW2*Pi*SW2)*
     &    (MW2*(27 + SW2*(-54 + (36 - 16*SW2)*SW2))*Finite +
     &      MS2*MW2*(216*C0i(cc0,MS2,MS2,MZ2,Mh02,MS2,MZ2) -
     &         54*C0i(cc0,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &         108*C0i(cc1,MS2,MS2,MZ2,Mh02,MS2,MZ2) +
     &         SW2*(-288*C0i(cc0,MS2,MS2,MZ2,Mh02,MS2,MZ2) +
     &            72*C0i(cc0,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &            144*C0i(cc1,MS2,MS2,MZ2,Mh02,MS2,MZ2))) +
     &      CW2*MS2*(-27*B0i(bb0,MS2,Mh02,MS2) -
     &         27*B0i(bb0,MS2,MS2,MZ2) +
     &         54*C0i(cc00,MS2,MZ2,MS2,Mh02,MS2,MS2) +
     &         54*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &         SW2*(36*B0i(bb0,MS2,Mh02,MS2) +
     &            36*B0i(bb0,MS2,MS2,MZ2) -
     &            72*C0i(cc00,MS2,MZ2,MS2,Mh02,MS2,MS2) -
     &            72*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2)) +
     &         36*MZ2*SW2*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &         MS2*(108*C0i(cc0,MS2,MZ2,MS2,Mh02,MS2,MS2) -
     &            54*C0i(cc0,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &            54*C0i(cc1,MS2,MZ2,MS2,Mh02,MS2,MS2) +
     &            SW2*(-144*C0i(cc0,MS2,MZ2,MS2,Mh02,MS2,MS2) +
     &               72*C0i(cc0,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &               72*C0i(cc1,MS2,MZ2,MS2,Mh02,MS2,MS2)))) +
     &      MW2*(-54*B0i(bb0,MS2,MS2,MZ2) +
     &         108*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &         108*MZ2*SW2*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &         SW2*(108*B0i(bb0,MS2,MS2,MZ2) -
     &            216*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &            SW2*(-72*B0i(bb0,MS2,MS2,MZ2) +
     &               144*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &               SW2*(32*B0i(bb0,MS2,MS2,MZ2) -
     &                  64*C0i(cc00,MZ2,MS2,MS2,MS2,MS2,MZ2))))))

        FVsSM = FVsSM +
     &    Alfa/(864.D0*CW2*MW2*Pi*SW2)*
     &     (MS2*MW2*(216*C0i(cc2,MS2,MS2,MZ2,Mh02,MS2,MZ2) +
     &          108*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &          SW2*(-288*C0i(cc2,MS2,MS2,MZ2,Mh02,MS2,MZ2) -
     &             216*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &             SW2*(144*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &                64*SW2*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2)))) +
     &       MW2*MZ2*(-54*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &          54*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &          SW2*(108*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &             SW2*(-72*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &                72*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &                SW2*(32*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &                   32*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2))))) +
     &       CW2*(MW2*(Finite*
     &             (-54*CKM(1,2)*CKMC(1,2) +
     &               72*SW2*(CKM(1,2)*CKMC(1,2)))) +
     &          MW2*(108*B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*CKMC(1,2) -
     &             144*SW2*
     &              (B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*CKMC(1,2))) +
     &          CW2*(108*MW2*(Finite*CKM(1,2)*CKMC(1,2)) -
     &             216*MW2*
     &              (B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*CKMC(1,2)) +
     &             216*MS2*MW2*
     &              (C0i(cc0,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &                CKMC(1,2))) +
     &          MU2*(-(72*SW2*
     &                (B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*CKMC(1,2))) -
     &             216*MW2*SW2*
     &              (C0i(cc0,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &                CKMC(1,2))) +
     &          MS2*(54*B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*CKMC(1,2) -
     &             72*SW2*(B0i(bb0,MS2,MU2,MW2)*CKM(1,2)*
     &                CKMC(1,2)) +
     &             MZ2*(-27*C0i(cc1,MZ2,MS2,MS2,MS2,MS2,MZ2) -
     &                27*C0i(cc2,MS2,MZ2,MS2,Mh02,MS2,MS2) +
     &                36*SW2*C0i(cc2,MS2,MZ2,MS2,Mh02,MS2,MS2)) +
     &             MS2*(54*C0i(cc2,MS2,MZ2,MS2,Mh02,MS2,MS2) -
     &                54*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2) +
     &                SW2*(-72*C0i(cc2,MS2,MZ2,MS2,Mh02,MS2,MS2) +
     &                   72*C0i(cc2,MZ2,MS2,MS2,MS2,MS2,MZ2))))))

        FVsSM = FVsSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MU2*(-(3*MU2*(C0i(cc0,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &               CKM(1,2)*CKMC(1,2))) +
     &          6*MW2*(C0i(cc0,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) -
     &          4*MZ2*SW2*(C0i(cc1,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &             CKM(1,2)*CKMC(1,2)) +
     &          SW2*(6*C0i(cc00,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             8*C0i(cc00,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2))) +
     &       MS2*(-6*C0i(cc00,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &           CKMC(1,2) -
     &          8*MS2*SW2*(C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &             CKM(1,2)*CKMC(1,2)) +
     &          16*MW2*SW2*
     &           (C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) +
     &          SW2*(6*C0i(cc00,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             8*C0i(cc00,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2)) +
     &          MU2*(9*C0i(cc0,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             16*SW2*(C0i(cc0,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &                CKM(1,2)*CKMC(1,2))) +
     &          MZ2*(3*C0i(cc1,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             4*SW2*(C0i(cc1,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))) +
     &       MW2*(-12*C0i(cc00,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &           CKMC(1,2) +
     &          16*SW2*(C0i(cc00,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) +
     &          MZ2*(6*C0i(cc1,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             8*SW2*(C0i(cc1,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))) +
     &       CW2*(-(6*MU2*(C0i(cc00,MS2,MZ2,MS2,MU2,MW2,MW2)*
     &               CKM(1,2)*CKMC(1,2))) +
     &          MW2*(-24*C0i(cc00,MS2,MZ2,MS2,MU2,MW2,MW2)*
     &              CKM(1,2)*CKMC(1,2) -
     &             12*MW2*(C0i(cc0,MS2,MZ2,MS2,MU2,MW2,MW2)*
     &                CKM(1,2)*CKMC(1,2)) +
     &             12*MZ2*(C0i(cc1,MS2,MZ2,MS2,MU2,MW2,MW2)*
     &                CKM(1,2)*CKMC(1,2))) +
     &          MS2*(-6*C0i(cc00,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &              CKMC(1,2) +
     &             MW2*(-24*C0i(cc1,MS2,MZ2,MS2,MU2,MW2,MW2)*
     &                 CKM(1,2)*CKMC(1,2) -
     &                24*C0i(cc2,MS2,MZ2,MS2,MU2,MW2,MW2)*CKM(1,2)*
     &                 CKMC(1,2)))))

        FVsSM = FVsSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MW2*(Finite*(-3*CKM(2,2)*CKMC(2,2) +
     &            4*SW2*(CKM(2,2)*CKMC(2,2)))) +
     &       MS2*(3*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) +
     &          6*MS2*(C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) -
     &          12*MW2*(C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &             CKMC(1,2)) -
     &          8*MU2*SW2*(C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &             CKM(1,2)*CKMC(1,2)) +
     &          SW2*(-4*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) +
     &             6*C0i(cc00,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2))) +
     &       MW2*(6*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) +
     &          SW2*(-8*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) +
     &             16*C0i(cc00,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          MZ2*(6*C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*CKM(1,2)*
     &              CKMC(1,2) -
     &             8*SW2*(C0i(cc2,MZ2,MS2,MS2,MU2,MU2,MW2)*
     &                CKM(1,2)*CKMC(1,2)))) +
     &       CW2*(6*MW2*(Finite*CKM(2,2)*CKMC(2,2)) -
     &          6*MC2*(C0i(cc00,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &             CKMC(2,2)) +
     &          MW2*(-12*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) -
     &             24*C0i(cc00,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             12*MW2*(C0i(cc0,MS2,MZ2,MS2,MC2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2))) +
     &          MS2*(-6*C0i(cc00,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             12*MW2*(C0i(cc0,MS2,MZ2,MS2,MC2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))) +
     &       MC2*(-(3*MC2*(C0i(cc0,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &               CKM(2,2)*CKMC(2,2))) +
     &          SW2*(-4*B0i(bb0,MS2,MC2,MW2)*CKM(2,2)*CKMC(2,2) +
     &             6*C0i(cc00,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          MW2*(6*C0i(cc0,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             12*SW2*(C0i(cc0,MS2,MZ2,MS2,MC2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2))) +
     &          MS2*(9*C0i(cc0,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             16*SW2*(C0i(cc0,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))))

        FVsSM = FVsSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (-(4*MT2*SW2*(B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*
     &            CKMC(3,2))) +
     &       MC2*(8*SW2*(C0i(cc00,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &             CKM(2,2)*CKMC(2,2)) -
     &          4*MZ2*SW2*(C0i(cc1,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &             CKM(2,2)*CKMC(2,2)) -
     &          8*MS2*SW2*(C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &             CKM(2,2)*CKMC(2,2))) +
     &       MW2*(Finite*(-3*CKM(3,2)*CKMC(3,2) +
     &            4*SW2*(CKM(3,2)*CKMC(3,2)))) +
     &       CW2*(MS2*MW2*(-24*C0i(cc1,MS2,MZ2,MS2,MC2,MW2,MW2)*
     &              CKM(2,2)*CKMC(2,2) -
     &             24*C0i(cc2,MS2,MZ2,MS2,MC2,MW2,MW2)*CKM(2,2)*
     &              CKMC(2,2)) +
     &          6*MW2*(Finite*CKM(3,2)*CKMC(3,2)) +
     &          MW2*(-12*B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*CKMC(3,2) +
     &             12*MZ2*(C0i(cc1,MS2,MZ2,MS2,MC2,MW2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))) +
     &       MS2*(-6*C0i(cc00,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &           CKMC(2,2) +
     &          3*B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*CKMC(3,2) +
     &          SW2*(8*C0i(cc00,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             4*B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*CKMC(3,2)) +
     &          MZ2*(3*C0i(cc1,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             4*SW2*(C0i(cc1,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &                CKM(2,2)*CKMC(2,2))) +
     &          MS2*(6*C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) -
     &             8*SW2*(C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &                CKM(2,2)*CKMC(2,2))) +
     &          MW2*(-12*C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             16*SW2*(C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &                CKM(2,2)*CKMC(2,2)))) +
     &       MW2*(-12*C0i(cc00,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &           CKMC(2,2) +
     &          6*B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*CKMC(3,2) -
     &          8*SW2*(B0i(bb0,MS2,MT2,MW2)*CKM(3,2)*CKMC(3,2)) +
     &          MZ2*(6*C0i(cc1,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             6*C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &              CKMC(2,2) +
     &             SW2*(-8*C0i(cc1,MZ2,MS2,MS2,MC2,MC2,MW2)*
     &                 CKM(2,2)*CKMC(2,2) -
     &                8*C0i(cc2,MZ2,MS2,MS2,MC2,MC2,MW2)*CKM(2,2)*
     &                 CKMC(2,2)))))

        FVsSM = FVsSM +
     &    Alfa/(48.D0*MW2*Pi*SW2)*
     &     (MT2*(-(3*MT2*(C0i(cc0,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &               CKM(3,2)*CKMC(3,2))) -
     &          4*MZ2*SW2*(C0i(cc1,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &             CKM(3,2)*CKMC(3,2)) +
     &          SW2*(6*C0i(cc00,MS2,MZ2,MS2,MT2,MW2,MW2)*CKM(3,2)*
     &              CKMC(3,2) +
     &             8*C0i(cc00,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2)) +
     &          MW2*(6*C0i(cc0,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2) -
     &             12*SW2*(C0i(cc0,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &                CKM(3,2)*CKMC(3,2)))) +
     &       MS2*(-6*C0i(cc00,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &           CKMC(3,2) +
     &          16*MW2*SW2*
     &           (C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &             CKMC(3,2)) +
     &          SW2*(6*C0i(cc00,MS2,MZ2,MS2,MT2,MW2,MW2)*CKM(3,2)*
     &              CKMC(3,2) +
     &             8*C0i(cc00,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2)) +
     &          MT2*(9*C0i(cc0,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2) -
     &             16*SW2*(C0i(cc0,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &                CKM(3,2)*CKMC(3,2))) +
     &          MZ2*(3*C0i(cc1,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2) -
     &             4*SW2*(C0i(cc1,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &                CKM(3,2)*CKMC(3,2)))) +
     &       MW2*(-12*C0i(cc00,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &           CKMC(3,2) +
     &          16*SW2*(C0i(cc00,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &             CKMC(3,2)) +
     &          MZ2*(6*C0i(cc1,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2) -
     &             8*SW2*(C0i(cc1,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &                CKM(3,2)*CKMC(3,2)))) +
     &       CW2*(-(6*MT2*(C0i(cc00,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &               CKM(3,2)*CKMC(3,2))) +
     &          MW2*(-24*C0i(cc00,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &              CKM(3,2)*CKMC(3,2) -
     &             12*MW2*(C0i(cc0,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &                CKM(3,2)*CKMC(3,2)) +
     &             12*MZ2*(C0i(cc1,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &                CKM(3,2)*CKMC(3,2))) +
     &          MS2*(-6*C0i(cc00,MS2,MZ2,MS2,MT2,MW2,MW2)*CKM(3,2)*
     &              CKMC(3,2) +
     &             MW2*(12*C0i(cc0,MS2,MZ2,MS2,MT2,MW2,MW2)*
     &                 CKM(3,2)*CKMC(3,2) -
     &                24*C0i(cc1,MS2,MZ2,MS2,MT2,MW2,MW2)*CKM(3,2)*
     &                 CKMC(3,2) -
     &                24*C0i(cc2,MS2,MZ2,MS2,MT2,MW2,MW2)*CKM(3,2)*
     &                 CKMC(3,2)))))

        FVsSM = FVsSM +
     &    Alfa/(24.D0*MW2*Pi*SW2)*
     &     (MW2*MZ2*(3*C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &           CKMC(3,2) -
     &          4*SW2*(C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &             CKMC(3,2))) +
     &       MS2*(-(6*MW2*(C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &               CKM(3,2)*CKMC(3,2))) -
     &          4*MT2*SW2*(C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &             CKM(3,2)*CKMC(3,2)) +
     &          MS2*(3*C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*CKM(3,2)*
     &              CKMC(3,2) -
     &             4*SW2*(C0i(cc2,MZ2,MS2,MS2,MT2,MT2,MW2)*
     &                CKM(3,2)*CKMC(3,2)))))

        FVsSM = FVsSM +
     & 1/2.D0*(-((Alfa*(3 - 2*SW2)**2)/(144.D0*CW2*Pi*SW2)*
     &          Finite) - Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,2)*CKMC(1,2) + CKM(2,2)*CKMC(2,2) +
     &            CKM(3,2)*CKMC(3,2))) -
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MS2,MC2,MW2))) +
     &       (Alfa*MS2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MS2,Mh02,MS2)) +
     &          Re(B0i(bb1,MS2,Mh02,MS2))) +
     &       (-(Alfa*MS2)/(16.D0*MW2*Pi*SW2) -
     & (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MS2,MS2,MZ2)) -
     &       (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(bb1,MS2,MT2,MW2))) -
     &       (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MS2,MU2,MW2))) -
     &       (Alfa*MC2*MS2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MS2,MC2,MW2))) +
     &       (Alfa*MS2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MS2,Mh02,MS2)) +
     & (Alfa*MS*(-9*CW2*MS*MS2 + MS*MW2*(24 - 16*SW2)*SW2))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MS2,MS2,MZ2)) -
     &       (Alfa*MS2*MT2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb0,MS2,MT2,MW2))) -
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MS2,MU2,MW2))) -
     &       (Alfa*MS2*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MS2,MC2,MW2))) +
     &       (Alfa*MS2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MS2,Mh02,MS2)) -
     & (Alfa*MS2*(9*CW2*MS2 + MW2*(9 + SW2*(-12 + 8*SW2))))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MS2,MS2,MZ2)) -
     & (Alfa*MS2*(MS2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb1,MS2,MT2,MW2))) -
     & (Alfa*MS2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MS2,MU2,MW2))))

        FVsSM = FVsSM +
     &    SW2/3.D0*((Alfa*SW2)/(36.D0*CW2*Pi)*Finite +
     &       (Alfa*MS2)/(8.D0*MW2*Pi*SW2)*
     &        (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MS2,MC2,MW2)))-
     &       (Alfa*MS2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MS2,Mh02,MS2)) +
     &          Re(B0i(bb1,MS2,Mh02,MS2))) +
     &       (Alfa*(9*CW2*MS2 + 8*MW2*SW2**2))/
     & (144.D0*CW2*MW2*Pi*SW2)*Re(B0i(bb1,MS2,MS2,MZ2))+
     &       (Alfa*MS2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(bb1,MS2,MT2,MW2))) +
     &       (Alfa*MS2)/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MS2,MU2,MW2))) +
     &       (Alfa*MC2*MS2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MS2,MC2,MW2))) -
     &       (Alfa*MS2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MS2,Mh02,MS2)) -
     & (Alfa*MS*(-9*CW2*MS*MS2 + MS*MW2*(24 - 16*SW2)*SW2))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MS2,MS2,MZ2)) +
     &       (Alfa*MS2*MT2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb0,MS2,MT2,MW2))) +
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MS2,MU2,MW2))) +
     & (Alfa*MS2*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MS2,MC2,MW2))) -
     &       (Alfa*MS2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MS2,Mh02,MS2)) +
     & (Alfa*MS2*(9*CW2*MS2 + MW2*(9 + SW2*(-12 + 8*SW2))))/
     &         (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MS2,MS2,MZ2)) +
     & (Alfa*MS2*(MS2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb1,MS2,MT2,MW2))) +
     & (Alfa*MS2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MS2,MU2,MW2))))

        FVsSM = FVsSM +
     &    SW2/3.D0*((Alfa*(3 - 2*SW2)**2)/
     & (144.D0*CW2*Pi*SW2)*Finite +
     &       Alfa/(8.D0*Pi*SW2)*
     & (Finite*(CKM(1,2)*CKMC(1,2) + CKM(2,2)*CKMC(2,2) +
     &            CKM(3,2)*CKMC(3,2))) +
     &       (Alfa*(MC2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(bb1,MS2,MC2,MW2))) -
     &       (Alfa*MS2)/(16.D0*MW2*Pi*SW2)*
     &        (Re(B0i(bb0,MS2,Mh02,MS2)) +
     &          Re(B0i(bb1,MS2,Mh02,MS2))) +
     &       ((Alfa*MS2)/(16.D0*MW2*Pi*SW2) +
     & (Alfa*(3 - 2*SW2)**2)/(72.D0*CW2*Pi*SW2))*
     &        Re(B0i(bb1,MS2,MS2,MZ2)) +
     &       (Alfa*(MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(bb1,MS2,MT2,MW2))) +
     &       (Alfa*(MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     &  (CKM(1,2)*CKMC(1,2)*Re(B0i(bb1,MS2,MU2,MW2))) +
     & (Alfa*MC2*MS2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb0,MS2,MC2,MW2))) -
     &       (Alfa*MS2**2)/(4.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb0,MS2,Mh02,MS2)) -
     & (Alfa*MS*(-9*CW2*MS*MS2 + MS*MW2*(24 - 16*SW2)*SW2))/
     & (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb0,MS2,MS2,MZ2)) +
     &       (Alfa*MS2*MT2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb0,MS2,MT2,MW2))) +
     &       (Alfa*MS2*MU2)/(4.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb0,MS2,MU2,MW2))) +
     & (Alfa*MS2*(MC2 + MS2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(2,2)*CKMC(2,2)*Re(B0i(dbb1,MS2,MC2,MW2))) -
     &       (Alfa*MS2**2)/(8.D0*MW2*Pi*SW2)*
     &        Re(B0i(dbb1,MS2,Mh02,MS2)) +
     & (Alfa*MS2*(9*CW2*MS2 + MW2*(9 + SW2*(-12 + 8*SW2))))/
     &  (72.D0*CW2*MW2*Pi*SW2)*Re(B0i(dbb1,MS2,MS2,MZ2)) +
     & (Alfa*MS2*(MS2 + MT2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(3,2)*CKMC(3,2)*Re(B0i(dbb1,MS2,MT2,MW2))) +
     & (Alfa*MS2*(MS2 + MU2 + 2*MW2))/(8.D0*MW2*Pi*SW2)*
     & (CKM(1,2)*CKMC(1,2)*Re(B0i(dbb1,MS2,MU2,MW2))))

	end
