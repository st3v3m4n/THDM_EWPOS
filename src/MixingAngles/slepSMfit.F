c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief This subroutine calculates the SM result of the effective leptonic mixing angle, \f$s^2_{l\mathrm{SM}}\f$,
C> with the parametrization given in [hep-ph/0608099v2](https://arxiv.org/abs/hep-ph/0608099) \cite Awramik:2006uz.
C>
C> @ingroup group_mixangles
      Subroutine CalcslepSMfit(slepSMfit)
C> @param[out] slepSMfit gives \f$s^2_{l,\mathrm{SM}}\f$
      RealType slepSMfit
      RealType s0, Lhiggs,
     & dhiggs, dalpha, dtop, dalphas, dz
      RealType d1, d2, d3, d4, d5, d6, d7, d8, d9, d10

#include "paraSM.h"

      Lhiggs=DLOG(Mh0/100D0)
      dhiggs=Mh0/100D0
      dalpha=DelAlfaMZ/0.05907D0-1D0
      dtop=(MT/178.0D0)**2-1D0
      dalphas=AlfasMZ/0.117D0-1
      dz=MZ/91.1876D0-1

      s0=0.2312527D0
      d1=4.729D-4
      d2=2.07D-5
      d3=3.85D-6
      d4=-1.85D-6
      d5=2.07D-2
      d6=-2.851D-3
      d7=1.82D-4
      d8=-9.74D-6
      d9=3.98D-4
      d10=-6.55D-1


      slepSMfit= s0+d1*Lhiggs+d2*Lhiggs**2
     & +d3*Lhiggs**4+d4*(dhiggs**2-1)+d5*dalpha
     & +d6*dtop+d7*dtop**2+d8*dtop*(dhiggs-1)
     & +d9*dalphas+d10*dz


      end
