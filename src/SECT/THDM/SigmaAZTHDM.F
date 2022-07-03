c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich

C>  Non-standard one-loop contribution to the photon-Z-mixing
C> @ingroup group_SECT
	double complex function SigmaAZTHDM(p2)
C>   squared momentum as argument for the photon-Z-mixing
	double precision p2

#include "paraTHDM.h"
#include "paraSM.h"

        SigmaAZTHDM =
     &   (Alfa*(CW2 - SW2))/(4.D0*CW*Pi)*
     &    (A0(MHp2) - 2*B0i(bb00,p2,MHp2,MHp2))/SW

	end
