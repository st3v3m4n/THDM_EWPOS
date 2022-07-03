c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"


C> This subroutine calculates the partial widths of the Z boson
C> using just the tree-level vector and axial-vector couplings.
C>
C> The result of the subroutine is
C> \f[\Gamma^{(0)}_f=N_c^f\Gamma_0\left[v_f^2 R_V^f+a_f^2 R_A^f\right],\f]
C> with the tree-level couplings \f$v_f\f$, \f$a_f\f$
C>(set by the subroutine @ref settreecouplings)
C> and the radiation factors \f$R_{V,A}^f\f$
C> (calculated by the subroutine @ref calcrfactors).
C>
C> @ingroup group_Zwidths
      subroutine CalcZWidthstree(GZtree)
C> @param[out] The array GZtree gives \f$\Gamma^{\mathrm{(0)}}_{f}\f$.
C> The entries in the array correspond to:
C> - 1: partial width of the neutrino
C> - 2: partial width of the electron
C> - 3: partial width of the muon
C> - 4: partial width of the tau lepton
C> - 5: partial width of the up quark
C> - 6: partial width of the down quark
C> - 7: partial width of the charm quark
C> - 8: partial width of the strange quark
C> - 9: partial width of the bottom quark
      RealType GZtree(1:9)
      RealType vf(1:9), af(1:9)
      RealType RV(2:9), RA(2:9)
      Integer ivar

#include "paraSM.h"


      call SetTreeCouplings(vf,af)

      call CalcRfactors(RV,RA)


      Gamma0=GF*MZ**3D0/(24D0*Sqrt(2D0)*pi)

      GZtree(1)=8D0*Gamma0*af(1)**2


      Do 100 ivar=2,4

      GZtree(ivar)=4D0*Gamma0*((vf(ivar))**2*RV(ivar)
     & +(af(ivar))**2*RA(ivar))

 100  continue
      Do 200 ivar=5,9

      GZtree(ivar)=4D0*3D0*Gamma0*((vf(ivar))**2*RV(ivar)
     & +(af(ivar))**2*RA(ivar))

 200  continue


      end
