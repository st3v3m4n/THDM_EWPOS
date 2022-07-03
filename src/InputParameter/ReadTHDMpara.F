c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C> @brief This subroutine can be called to read in the input for the THDM parameters
C> from the command-line
C>
C> @ingroup group_Input
      subroutine ReadTHDMpara

#include "paraTHDM.h"


      Print*,"Enter value for MHH"
      Read(*,*) MHH
      MHH2=MHH**2
      Print*,"Enter value for MA0"
      Read(*,*) MA0
      MA02=MA0**2
      Print*,"Enter value for MHp"
      Read(*,*) MHp
      MHp2=MHp**2
      Print*,"Enter value for TB"
      Read(*,*) TB
      TB2=TB**2
      Print*,"Enter value for Lambda5"
      Read(*,*) Lambda5
#ifndef L345INPUT
      Print*,"Enter value for mu2sq"
      Read(*,*) mu2sq
#endif
#ifdef L345INPUT
      Print*,"Enter value for Lambda_345"
      Read(*,*) L345
#endif
      end
