c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  Non-standard one-loop contribution to the W self-energy
C> @ingroup group_SECT
	double complex function SigmaWTHDM(p2)
C>   squared momentum as argument for W self-energy
	double precision p2

#include "paraTHDM.h"
#include "paraSM.h"

        SigmaWTHDM =
     &   Alfa/(16.D0*Pi*SW2)*
     &    (A0(MA02) + A0(MHH2) + 2*A0(MHp2) -
     &      4*B0i(bb00,p2,MA02,MHp2) + 4*B0i(bb00,p2,Mh02,MW2) +
     &      SBA2*(-4*B0i(bb00,p2,Mh02,MW2) -
     &         4*B0i(bb00,p2,MHH2,MHp2)) +
     &      MW2*(-4*B0i(bb0,p2,Mh02,MW2) +
     &         4*SBA2*B0i(bb0,p2,Mh02,MW2)) +
     &      CBA2*(-4*B0i(bb00,p2,Mh02,MHp2) -
     &         4*B0i(bb00,p2,MHH2,MW2) + 4*MW2*B0i(bb0,p2,MHH2,MW2)
     &         ))

	end
