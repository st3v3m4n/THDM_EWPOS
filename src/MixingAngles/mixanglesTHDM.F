c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief Subroutine for the calculation of the effective leptonic mixing angle \f$s_l^2\f$
C> in the THDM.
C>
C> The effective leptonic mixing angle in the THDM contains the corrections
C> \f[
C> s^2_{l}=\left(1-\frac{M_W^2}{M_Z^2}\right)\left(1+\Delta\kappa_\mathrm{SM}^{(1)}
C>+\delta\kappa^\mathrm{(h.o)}_\mathrm{SM}+\Delta\kappa_\mathrm{NS}\right)
C> \f]
C> The one-loop SM contribution \f$\Delta\kappa_\mathrm{SM}^{(1)}\f$ is calculated
C> internally by the subroutine @ref calcdelksm1loop.
C> The higher-order contributions in the SM are obtained by calling the subroutine
C> @ref calcdelksmho.
C> The non-standard corrections, which are included in \f$\Delta\kappa_\mathrm{NS}\f$,
C> are selected by the argument \b sproc, which is passed internally to the
C> subroutine @ref calcdelkns.
C> Note that the W boson mass \f$M_W\f$ has to be calculated before calling @ref calcslepthdm.
C> To be consistent, it needs to be calculated at the same order as
C> \f$\Delta\kappa_\mathrm{NS}\f$.
C>
C> @ingroup group_mixangles
      subroutine CalcslepTHDM(slepTHDM,sproc)
C> @param[out] slepTHDM gives the result for \f$s_l^2\f$ in the THDM.
      RealType slepTHDM
      RealType slepSMho
      RealType gVeSM, gAeSM
      RealType gVeTHDM, gAeTHDM
      RealType DelKSM1L, DelKSMho, DelKNS
C> @param[in] The flag sproc selects the implementation of the higher-order corrections.
C> It is passed internally to the subroutine @ref calcdelkns. For a description
C> of the included corrections see the input parameter \b loflag in @ref calcdelkns.
      Integer sproc



#include "CTdeclarationTHDM.h"
#include "CTdeclarationSM.h"
#include "paraTHDM.h"
#include "paraSM.h"



      call CalcDelKSM1Loop(DelKSM1L,1)
      call CalcDelKSMho(DelKSMho)
      call CalcDelKNS(DelKNS,sproc)

      slepTHDM=SW2*(1+DelKSM1L+DelKSMho+DelKNS)

      end
