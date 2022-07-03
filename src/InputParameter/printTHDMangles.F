c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich



C> @brief This subroutine prints the values for the different trigonometric functions
C> of the mixing-angles \f$\alpha\f$ and \f$\beta\f$ in the THDM.
C>
C> @ingroup group_Input
      subroutine printTHDMangles

#include "paraTHDM.h"

      Print*,"TB2=",TB2
      Print*,"SB=",SB
      Print*,"CB=",CB
      Print*,"SB2=",SB2
      Print*,"CB2=",CB2
      Print*,"SA2=",SA2
      Print*,"CA2=",CA2
      Print*,"CA=",CA
      Print*,"SBA=",SBA
      Print*,"SBA2=",SBA2
      Print*,"CBA=",CBA
      Print*,"CBA2=",CBA2
      Print*,"SAB=",SAB
      Print*,"CAB=",CAB
      Print*,"S2A=",S2A
      Print*,"S2B=",S2B
      Print*,"C2A=",C2A
      Print*,"C2B=",C2B

      end
