c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C> @brief Non-standard one-loop contribution to the field counterterm of the Z boson.
C>
C> @ingroup group_SECT
C>
C> The field counterterm is renormalized on-shell, following the conventions from [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).
	subroutine CalcdZZZTHDM
	implicit none



#include "CTdeclarationTHDM.h"

#include "paraTHDM.h"
#include "paraSM.h"
        dZZZTHDM = -(Alfa/(16.D0*CW2**2*Pi*SW2)*
     &      (MW2*(-4*Re(B0i(dbb0,MZ2,Mh02,MZ2)) +
     &           4*SBA2*Re(B0i(dbb0,MZ2,Mh02,MZ2))) +
     &        CW2*(CW2*(-(4*CW2*Re(B0i(dbb00,MZ2,MHp2,MHp2)))
     &         + 8*SW2*Re(B0i(dbb00,MZ2,MHp2,MHp2))) -
     &           4*SW2**2*Re(B0i(dbb00,MZ2,MHp2,MHp2)) +
     &           SBA2*(-4*Re(B0i(dbb00,MZ2,MA02,MHH2)) -
     &              4*Re(B0i(dbb00,MZ2,Mh02,MZ2))) +
     &           4*Re(B0i(dbb00,MZ2,Mh02,MZ2))) +
     &        CBA2*(4*MW2*Re(B0i(dbb0,MZ2,MHH2,MZ2)) +
     &           CW2*(-4*Re(B0i(dbb00,MZ2,MA02,Mh02)) -
     &              4*Re(B0i(dbb00,MZ2,MHH2,MZ2))))))



	end
