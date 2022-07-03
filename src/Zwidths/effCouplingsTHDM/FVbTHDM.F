c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFVbTHDM(FVbTHDM)
	implicit none


	double complex FVbTHDM, tmp1

#include "paraTHDM.h"
#include "paraSM.h"
        tmp1 = (Alfa*MB2*(3 - 4*SW2))/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MB2,MZ2))/SW +
     &    (Alfa*(4*MT2*SW2 + MB2*(-3 + 4*SW2)))/
     &      (96.D0*CW*MW2*Pi*SW2)*(EL*B0i(bb0,MB2,MT2,MW2))/SW +
     &    (Alfa*MT2)/(8.D0*CW*Pi)*
     &     (EL*C0i(cc0,MB2,MZ2,MB2,MT2,MW2,MW2))/SW +
     &    (Alfa*(3*MT2**2 + MB2*MT2*(-9 + 16*SW2)))/
     &      (96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2))/SW +
     &    (Alfa*MB2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MB2,Mh02)*(3*Yuk1**2 - 4*SW2*Yuk1**2))/
     &      SW + (Alfa*MB2**2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,Mh02)*
     &        (-3*Yuk1**2 + 4*SW2*Yuk1**2))/SW +
     &    (Alfa*MB2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MB2,MHH2)*(3*Yuk2**2 - 4*SW2*Yuk2**2))/
     &      SW + (Alfa*MB2**2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MHH2)*
     &        (-3*Yuk2**2 + 4*SW2*Yuk2**2))/SW +
     &    (Alfa*MB2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MA02,MB2)*(3*Yuk3**2 - 4*SW2*Yuk3**2))/
     &      SW + Alfa/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MHp2,MT2)*
     &        (4*MT2*SW2 +
     &          MB2*(-3*TB2*Yuk3**2 + 4*SW2*(TB2*Yuk3**2))))/
     &      (SW*TB2) + C0i(cc0,MB2,MZ2,MB2,MHp2,MT2,MT2)*
     &     (-((Alfa*MT2)/(24.D0*CW*MW2*Pi)*
     &          (EL*(-(MT2*TB) +
     &               MB2*(2*TB2*Yuk3 + TB*(2 + TB2*Yuk3**2))))/
     &           (SW*TB*TB2)) -
     &       (Alfa*(-3 + 4*SW2))/(96.D0*CW*MW2*Pi*SW2)*
     &        (EL*(MT2**2*TB +
     &             MB2*(2*MB2*(TB*TB2*Yuk3**2) +
     &                MT2*(2*TB2*Yuk3 - TB*TB2*Yuk3**2))))/
     &         (SW*TB*TB2))

        tmp1 = tmp1 + (Alfa*(MB2 + MT2)*(CW2 - SW2))/
     &      (16.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2))/SW +
     &    (Alfa*MB2*(-3 + 4*SW2))/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2))/SW +
     &    (Alfa*(MB2*(3 - 4*SW2) - 4*MT2*SW2))/(48.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2))/SW +
     &    (Alfa*MB2)/(48.D0*CW*CW2*Pi*SW2)*
     &     (EL*SBA*C0i(cc1,MB2,MZ2,MB2,MB2,Mh02,MZ2)*
     &        (3*Yuk1 - 4*SW2*Yuk1))/SW +
     &    (Alfa*MB2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,Mh02)*
     &        (-3*Yuk1**2 + 4*SW2*Yuk1**2))/SW +
     &    (Alfa*MB2)/(48.D0*CW*CW2*Pi*SW2)*
     &     (CBA*EL*C0i(cc1,MB2,MZ2,MB2,MB2,MHH2,MZ2)*
     &        (3*Yuk2 - 4*SW2*Yuk2))/SW +
     &    (Alfa*MB2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MHH2)*
     &        (-3*Yuk2**2 + 4*SW2*Yuk2**2))/SW +
     &    (Alfa*MB2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MB2,MZ2,MB2,MA02,MB2,MB2)*
     &        (-3*Yuk3**2 + 4*SW2*Yuk3**2))/SW +
     &    (Alfa*MB2**2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MB2,MZ2,MB2,MA02,MB2,MB2)*
     &        (-3*Yuk3**2 + 4*SW2*Yuk3**2))/SW +
     &    (Alfa*(CW2 - SW2))/(16.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MHp2,MHp2,MT2)*
     &        (MT2 + MB2*(TB2*Yuk3**2)))/(SW*TB2) +
     &    Alfa/(48.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MB2,MZ2,MB2,MHp2,MT2,MT2)*
     &        (-4*MT2*SW2 +
     &          MB2*(3*TB2*Yuk3**2 - 4*SW2*(TB2*Yuk3**2))))/
     &      (SW*TB2) + (Alfa*MB2)/(48.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MB2,MZ2,MB2,MHp2,MT2,MT2)*
     &        (-4*MT2*SW2 +
     &          MB2*(3*TB2*Yuk3**2 - 4*SW2*(TB2*Yuk3**2))))/
     &      (SW*TB2)

        tmp1 = tmp1 + (Alfa*MB2*MZ2*(3 - 4*SW2))/
     &      (192.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2))/SW +
     &    (Alfa*(4*MT2*MZ2*SW2 + MB2*MZ2*(-3 + 4*SW2)))/
     &      (96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2))/SW +
     &    (Alfa*MB2)/(48.D0*CW*CW2*Pi*SW2)*
     &     (EL*SBA*(C0i(cc0,MB2,MZ2,MB2,MB2,Mh02,MZ2) +
     &          C0i(cc2,MB2,MZ2,MB2,MB2,Mh02,MZ2))*
     &        (-3*Yuk1 + 4*SW2*Yuk1))/SW +
     &    (Alfa*MB2*MZ2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,Mh02)*
     &        (3*Yuk1**2 - 4*SW2*Yuk1**2))/SW +
     &    (Alfa*MB2**2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,Mh02)*
     &        (3*Yuk1**2 - 4*SW2*Yuk1**2))/SW +
     &    (Alfa*MB2)/(48.D0*CW*CW2*Pi*SW2)*
     &     (CBA*EL*(C0i(cc0,MB2,MZ2,MB2,MB2,MHH2,MZ2) +
     &          C0i(cc2,MB2,MZ2,MB2,MB2,MHH2,MZ2))*
     &        (-3*Yuk2 + 4*SW2*Yuk2))/SW +
     &    (Alfa*MB2*MZ2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MHH2)*
     &        (3*Yuk2**2 - 4*SW2*Yuk2**2))/SW +
     &    (Alfa*MB2**2)/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MHH2)*
     &        (3*Yuk2**2 - 4*SW2*Yuk2**2))/SW +
     &    (Alfa*MB2)/(192.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MB2,MZ2,MB2,MA02,MB2,MB2)*
     &        (MZ2*(3*Yuk3**2 - 4*SW2*Yuk3**2) +
     &          MB2*(-6*Yuk3**2 + 8*SW2*Yuk3**2)))/SW +
     &    Alfa/(96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MB2,MZ2,MB2,MHp2,MT2,MT2)*
     &        (4*MT2*MZ2*SW2 +
     &          MB2*(-8*MT2*SW2 +
     &             MZ2*(-3*TB2*Yuk3**2 + 4*SW2*(TB2*Yuk3**2)) +
     &             MB2*(6*TB2*Yuk3**2 - 8*SW2*(TB2*Yuk3**2)))))/
     &      (SW*TB2)

        tmp1 = tmp1 + (Alfa*MB2**2*(3 - 4*SW2))/
     &      (96.D0*CW*MW2*Pi*SW2)*
     &     (EL*(C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &          C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2)))/SW +
     &    (Alfa*MB2*(4*MT2*SW2 + MB2*(-3 + 4*SW2)))/
     &      (48.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2))/SW -
     &    1/(6.D0*CW)*(EL*SW*
     &       (-((Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &            (Yuk3**2*
     &              (Re(B0i(bb0,MB2,MA02,MB2)) +
     &                Re(B0i(bb1,MB2,MA02,MB2))))) +
     &         (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &          (Yuk1**2*Re(B0i(bb1,MB2,MB2,Mh02))) +
     &         (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &          (Yuk2**2*Re(B0i(bb1,MB2,MB2,MHH2))) +
     &         (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &          Re(B0i(bb1,MB2,MB2,MZ2)) -
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk3**2*(Re(B0i(bb0,MB2,MHp2,MT2)) +
     &              Re(B0i(bb1,MB2,MHp2,MT2)))) +
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &          Re(B0i(bb1,MB2,MT2,MW2)) -
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk1**2*Re(B0i(dbb0,MB2,MB2,Mh02))) -
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk2**2*Re(B0i(dbb0,MB2,MB2,MHH2))) +
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          Re(B0i(dbb0,MB2,MB2,MZ2)) -
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &          ((MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 2*TB2*Yuk3))*
     &             Re(B0i(dbb0,MB2,MHp2,MT2)))/(TB*TB2) +
     &         (Alfa*MB2*MT2)/(4.D0*MW2*Pi*SW2)*
     &          Re(B0i(dbb0,MB2,MT2,MW2)) -
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk3**2*Re(B0i(dbb1,MB2,MA02,MB2))) +
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk1**2*Re(B0i(dbb1,MB2,MB2,Mh02))) +
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          (Yuk2**2*Re(B0i(dbb1,MB2,MB2,MHH2))) +
     &         (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &          Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &          ((MT2 + MB2*(TB2*Yuk3**2))*
     &             Re(B0i(dbb1,MB2,MHp2,MT2)))/TB2 +
     &         (Alfa*MB2*(MB2 + MT2))/(8.D0*MW2*Pi*SW2)*
     &          Re(B0i(dbb1,MB2,MT2,MW2))))

        tmp1 = tmp1 + (3 - 2*SW2)/(24.D0*CW)*
     &     (EL*(-((Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &             (Yuk3**2*
     &               (Re(B0i(bb0,MB2,MA02,MB2)) +
     &                 Re(B0i(bb1,MB2,MA02,MB2))))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(bb1,MB2,MB2,Mh02))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(bb1,MB2,MB2,MHH2))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,MZ2)) -
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*
     &           (Re(B0i(bb0,MB2,MHp2,MT2)) +
     &              Re(B0i(bb1,MB2,MHp2,MT2)))/TB2 +
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MT2,MW2)) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(dbb0,MB2,MB2,Mh02))) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(dbb0,MB2,MB2,MHH2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MB2,MZ2)) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           ((MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 2*TB2*Yuk3))*
     &              Re(B0i(dbb0,MB2,MHp2,MT2)))/(TB*TB2) +
     &          (Alfa*MB2*MT2)/(4.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MT2,MW2)) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk3**2*Re(B0i(dbb1,MB2,MA02,MB2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(dbb1,MB2,MB2,Mh02))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(dbb1,MB2,MB2,MHH2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           ((MT2 + MB2*(TB2*Yuk3**2))*
     &              Re(B0i(dbb1,MB2,MHp2,MT2)))/TB2 +
     &          (Alfa*MB2*(MB2 + MT2))/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MB2,MT2,MW2))))/SW

        tmp1 = tmp1 - (-3 + 2*SW2)/(24.D0*CW)*
     &     (EL*(-((Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &             (Yuk3**2*
     &               (Re(B0i(bb0,MB2,MA02,MB2)) +
     &                 Re(B0i(bb1,MB2,MA02,MB2))))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(bb1,MB2,MB2,Mh02))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(bb1,MB2,MB2,MHH2))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,MZ2)) -
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*
     &           (Re(B0i(bb0,MB2,MHp2,MT2)) +
     &              Re(B0i(bb1,MB2,MHp2,MT2)))/TB2 +
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MT2,MW2)) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(dbb0,MB2,MB2,Mh02))) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(dbb0,MB2,MB2,MHH2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MB2,MZ2)) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           ((MB2*(TB*TB2*Yuk3**2) + MT2*(TB + 2*TB2*Yuk3))*
     &              Re(B0i(dbb0,MB2,MHp2,MT2)))/(TB*TB2) +
     &          (Alfa*MB2*MT2)/(4.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb0,MB2,MT2,MW2)) -
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk3**2*Re(B0i(dbb1,MB2,MA02,MB2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk1**2*Re(B0i(dbb1,MB2,MB2,Mh02))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           (Yuk2**2*Re(B0i(dbb1,MB2,MB2,MHH2))) +
     &          (Alfa*MB2**2)/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           ((MT2 + MB2*(TB2*Yuk3**2))*
     &              Re(B0i(dbb1,MB2,MHp2,MT2)))/TB2 +
     &          (Alfa*MB2*(MB2 + MT2))/(8.D0*MW2*Pi*SW2)*
     &           Re(B0i(dbb1,MB2,MT2,MW2))))/SW

	FVbTHDM = -(2*CW*(SW*tmp1)/EL)

	end
