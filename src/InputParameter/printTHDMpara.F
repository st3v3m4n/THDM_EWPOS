c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


C> @brief This subroutine prints the values for the non-standard input-parameters of the THDM.
C>
C> @ingroup group_Input
      subroutine printTHDMpara(SelectModel)
      implicit none
      Integer SelectModel
C> @param[in] SelectModel selects for which model the parameters
C> should be printed to the screen:
C> - SelectModel=1: THDM with soft \f$Z_2\f$ violation in the alignment limit
C> - SelectModel=2: THDM with hard \f$Z_2\f$ violation (without alignment limit)
C> - SelectModel=3: Inert-Higgs-Doublet Model
#include "paraTHDM.h"


      Print*,"THDM parameter:"
      Print*,"---------------"
      Print*,"MHH =", MHH
      Print*,"MA0 =", MA0
      Print*,"MHp =", MHp
      IF (SelectModel.EQ.1) THEN
      Print*,"TB =", TB
      Print*,"SBA =",SBA
      Print*,"CBA =",CBA
      Print*,"Lamba5", Lambda5
      ELSE IF (SelectModel.EQ.2) THEN
      Print*,"TB =", TB
      Print*,"SA =",SA
      call printTHDMangles
      Print*,"m12sq =",m12sq
      Print*,"Lambda6 =",Lambda6
      Print*,"Lambda7 =",Lambda7
      ELSE
#ifndef L345INPUT
        Print*,"mu2sq=", mu2sq
#else
        Print*,"L345=", L345
#endif

      END IF

      end
