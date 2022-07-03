c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


c> Header-file for THDM parameters

c> @ingroup group_Input

#include "types.h"

      RealType MHH, MHH2, MA0, MA02, MHp, MHp2,
     & SBA, SBA2, CBA, CBA2, TB, TB2, Yuk1, Yuk2, Yuk3, Lambda5,
     & mu2sq, L345, m12sq, Lambda6, Lambda7, SB, SB2, CB, CB2,
     & SAB, CAB, S2B, C2B, SA, CA, SA2, CA2, S2A, C2A
      COMMON /THDMmasses/ MHH, MHH2, MA0, MA02, MHp, MHp2
      COMMON /THDMangles/ SBA, SBA2, CBA, CBA2, TB, TB2,
     &  SB, SB2, CB, CB2,
     & Yuk1, Yuk2, Yuk3, Lambda5, mu2sq, L345,
     & m12sq, Lambda6, Lambda7,
     & SAB, CAB, S2B, C2B, SA, CA, SA2, CA2, S2A, C2A


C select Inputparameter for IHDM
c uncomment/comment the following lines if Lambda_345 should be used as input
#ifndef L345INPUT
#define L345INPUT
#endif
