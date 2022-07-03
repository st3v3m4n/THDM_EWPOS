c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFAbSMHiggs(FAbSMHiggs)
	implicit none


	double complex FAbSMHiggs, tmp1

#include "paraTHDM.h"
#include "paraSM.h"
        tmp1 = (Alfa*MB2)/(64.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MB2,Mh02))/SW +
     &    (Alfa*MB2)/(64.D0*CW*MW2*Pi*SW2)*
     &     (EL*B0i(bb0,MB2,MB2,MZ2))/SW +
     &    (Alfa*(-4*MT2*SW2 + MB2*(-3 + 4*SW2)))/
     &      (96.D0*CW*MW2*Pi*SW2)*(EL*B0i(bb0,MB2,MT2,MW2))/SW +
     &    (Alfa*MB2)/(16.D0*CW*CW2*Pi*SW2)*
     &     (EL*C0i(cc0,MB2,MZ2,MB2,MB2,Mh02,MZ2))/SW -
     &    (Alfa*MT2)/(8.D0*CW*Pi)*
     &     (EL*C0i(cc0,MB2,MZ2,MB2,MT2,MW2,MW2))/SW +
     &    (Alfa*MB2**2)/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc0,MZ2,MB2,MB2,MB2,MB2,Mh02))/SW +
     &    (Alfa*(-(MB2*MT2) - MT2**2))/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc0,MZ2,MB2,MB2,MT2,MT2,MW2))/SW -
     &    (Alfa*MB2)/(8.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MB2,MZ2,MB2,MB2,Mh02,MZ2))/SW -
     &    (Alfa*(-MB2 + MT2)*(CW2 - SW2))/(16.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MB2,MZ2,MB2,MT2,MW2,MW2))/SW -
     &    (Alfa*MB2)/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,Mh02))/SW -
     &    (Alfa*MB2)/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MB2,MB2,MZ2))/SW +
     &    (Alfa*(MB2*(3 - 4*SW2) + 4*MT2*SW2))/(48.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc00,MZ2,MB2,MB2,MT2,MT2,MW2))/SW +
     &    (Alfa*MB2*MZ2)/(64.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,Mh02))/SW

        tmp1 = tmp1 + (Alfa*MB2*MZ2)/(64.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MB2,MB2,MZ2))/SW +
     &    (Alfa*(-4*MT2*MZ2*SW2 + MB2*MZ2*(-3 + 4*SW2)))/
     &      (96.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc1,MZ2,MB2,MB2,MT2,MT2,MW2))/SW -
     &    (Alfa*MB2)/(16.D0*CW*CW2*Pi*SW2)*
     &     (EL*(C0i(cc1,MB2,MZ2,MB2,MB2,Mh02,MZ2) +
     &          C0i(cc2,MB2,MZ2,MB2,MB2,Mh02,MZ2)))/SW -
     &    (Alfa*MB2)/(8.D0*CW*Pi)*
     &     (EL*(C0i(cc1,MB2,MZ2,MB2,MT2,MW2,MW2) +
     &          C0i(cc2,MB2,MZ2,MB2,MT2,MW2,MW2)))/SW -
     &    (Alfa*MB2**2)/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MZ2,MB2,MB2,MB2,MB2,Mh02))/SW +
     &    (Alfa*MB2**2)/(32.D0*CW*MW2*Pi*SW2)*
     &     (EL*(C0i(cc0,MZ2,MB2,MB2,MB2,MB2,MZ2) +
     &          C0i(cc2,MZ2,MB2,MB2,MB2,MB2,MZ2)))/SW -
     &    (Alfa*MB2*MT2)/(16.D0*CW*MW2*Pi*SW2)*
     &     (EL*C0i(cc2,MZ2,MB2,MB2,MT2,MT2,MW2))/SW -
     &    1/(6.D0*CW)*(EL*SW*
     &       (-((Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &            (MB2*(Re(B0i(dbb0,MB2,MB2,Mh02)) -
     &                 Re(B0i(dbb0,MB2,MB2,MZ2))) -
     &              2*MT2*Re(B0i(dbb0,MB2,MT2,MW2)))) -
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &          (MB2*(-Re(B0i(dbb1,MB2,MB2,Mh02)) -
     &               Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &               Re(B0i(dbb1,MB2,MT2,MW2))) -
     &            MT2*Re(B0i(dbb1,MB2,MT2,MW2))) +
     &         (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &          Re(B0i(bb1,MB2,MB2,Mh02)) +
     &         (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &          Re(B0i(bb1,MB2,MB2,MZ2)) +
     &         (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*Re(B0i(bb1,MB2,MT2,MW2)))
     &       )

        tmp1 = tmp1 - (3 - 2*SW2)/(24.D0*CW)*
     &     (EL*(-((Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &             (MB2*(Re(B0i(dbb0,MB2,MB2,Mh02)) -
     &                  Re(B0i(dbb0,MB2,MB2,MZ2))) -
     &               2*MT2*Re(B0i(dbb0,MB2,MT2,MW2)))) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           (MB2*(-Re(B0i(dbb1,MB2,MB2,Mh02)) -
     &                Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &                Re(B0i(dbb1,MB2,MT2,MW2))) -
     &             MT2*Re(B0i(dbb1,MB2,MT2,MW2))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,Mh02)) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,MZ2)) +
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*Re(B0i(bb1,MB2,MT2,MW2))
     &          ))/SW + (-3 + 2*SW2)/(24.D0*CW)*
     &     (EL*(-((Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &             (MB2*(Re(B0i(dbb0,MB2,MB2,Mh02)) -
     &                  Re(B0i(dbb0,MB2,MB2,MZ2))) -
     &               2*MT2*Re(B0i(dbb0,MB2,MT2,MW2)))) -
     &          (Alfa*MB2)/(8.D0*MW2*Pi*SW2)*
     &           (MB2*(-Re(B0i(dbb1,MB2,MB2,Mh02)) -
     &                Re(B0i(dbb1,MB2,MB2,MZ2)) -
     &                Re(B0i(dbb1,MB2,MT2,MW2))) -
     &             MT2*Re(B0i(dbb1,MB2,MT2,MW2))) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,Mh02)) +
     &          (Alfa*MB2)/(16.D0*MW2*Pi*SW2)*
     &           Re(B0i(bb1,MB2,MB2,MZ2)) +
     &          (Alfa*MT2)/(8.D0*MW2*Pi*SW2)*Re(B0i(bb1,MB2,MT2,MW2))
     &          ))/SW

	FAbSMHiggs = 2*CW*(SW*tmp1)/EL

	end
