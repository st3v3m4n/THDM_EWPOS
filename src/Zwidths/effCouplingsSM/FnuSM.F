c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

	subroutine CalcFnuSM(FnuSM)
	implicit none


	double complex FnuSM

#include "paraSM.h"
        FnuSM = -(Alfa/(8.D0*Pi)*Finite) +
     &    Alfa/(16.D0*Pi*SW2)*Finite -
     &    Alfa/(32.D0*CW2*Pi*SW2)*Finite -
     &    (Alfa*CW2)/(8.D0*Pi*SW2)*Finite +
     &    Alfa/(16.D0*CW2*Pi*SW2)*B0i(bb0,0.D0,0.D0,MZ2) +
     &    Alfa/(4.D0*Pi)*B0i(bb0,0.D0,ME2,MW2) +
     &    (Alfa*ME2)/(8.D0*MW2*Pi)*B0i(bb0,0.D0,ME2,MW2) -
     &    Alfa/(8.D0*Pi*SW2)*B0i(bb0,0.D0,ME2,MW2) +
     &    (Alfa*CW2)/(4.D0*Pi*SW2)*B0i(bb0,0.D0,ME2,MW2) +
     &    (Alfa*ME2)/(4.D0*Pi)
     &    *C0i(cc0,0.D0,MZ2,0.D0,ME2,MW2,MW2)+
     &    (Alfa*CW2*MW2)/(4.D0*Pi*SW2)*
     &     C0i(cc0,0.D0,MZ2,0.D0,ME2,MW2,MW2) -
     &    (Alfa*ME2)/(8.D0*Pi*SW2)
     &    *C0i(cc0,MZ2,0.D0,0.D0,ME2,ME2,MW2) +
     &    (Alfa*ME2**2)/(16.D0*MW2*Pi*SW2)*
     &     C0i(cc0,MZ2,0.D0,0.D0,ME2,ME2,MW2) -
     &    (Alfa*ME2)/(8.D0*MW2*Pi)
     &    *C0i(cc00,0.D0,MZ2,0.D0,ME2,MW2,MW2) +
     &    (Alfa*CW2)/(2.D0*Pi*SW2)
     &    *C0i(cc00,0.D0,MZ2,0.D0,ME2,MW2,MW2) +
     &    (Alfa*CW2*ME2)/(8.D0*MW2*Pi*SW2)*
     &     C0i(cc00,0.D0,MZ2,0.D0,ME2,MW2,MW2) -
     &    Alfa/(8.D0*CW2*Pi*SW2)
     &    *C0i(cc00,MZ2,0.D0,0.D0,0.D0,0.D0,MZ2) -
     &    Alfa/(2.D0*Pi)*C0i(cc00,MZ2,0.D0,0.D0,ME2,ME2,MW2) -
     &    (Alfa*ME2)/(4.D0*MW2*Pi)
     &    *C0i(cc00,MZ2,0.D0,0.D0,ME2,ME2,MW2) +
     &    Alfa/(4.D0*Pi*SW2)
     &    *C0i(cc00,MZ2,0.D0,0.D0,ME2,ME2,MW2) -
     &    (Alfa*CW2*MZ2)/(4.D0*Pi*SW2)*
     &     C0i(cc1,0.D0,MZ2,0.D0,ME2,MW2,MW2) +
     &    (Alfa*MZ2)/(16.D0*CW2*Pi*SW2)*
     &     C0i(cc1,MZ2,0.D0,0.D0,0.D0,0.D0,MZ2) +
     &    (Alfa*MZ2)/(4.D0*Pi)
     &    *C0i(cc1,MZ2,0.D0,0.D0,ME2,ME2,MW2) +
     &    (Alfa*ME2*MZ2)/(8.D0*MW2*Pi)*
     &     C0i(cc1,MZ2,0.D0,0.D0,ME2,ME2,MW2) -
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)
     &    *C0i(cc1,MZ2,0.D0,0.D0,ME2,ME2,MW2) +
     &    (Alfa*MZ2)/(16.D0*CW2*Pi*SW2)*
     &     C0i(cc2,MZ2,0.D0,0.D0,0.D0,0.D0,MZ2) +
     &    (Alfa*MZ2)/(4.D0*Pi)
     &    *C0i(cc2,MZ2,0.D0,0.D0,ME2,ME2,MW2) -
     &    (Alfa*MZ2)/(8.D0*Pi*SW2)
     &    *C0i(cc2,MZ2,0.D0,0.D0,ME2,ME2,MW2) +
     &    Alfa/(32.D0*CW2*MW2*Pi*SW2)*
     &     (MW2*Finite + CW2*
     &        (2*MW2*Finite +
     &       2*ME2*Re(B0i(bb1,0.D0,ME2,MW2)) +
     &          4*MW2*Re(B0i(bb1,0.D0,ME2,MW2))) +
     &       2*MW2*Re(B0i(bb1,0.D0,0.D0,MZ2)))

	end
