c This file is part of THDM_EWPOS.

c THDM_EWPOS is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

c THDM_EWPOS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

c   You should have received a copy of the GNU General Public License along with THDM_EWPOS. If not, see <https://www.gnu.org/licenses/>.

c Copyright 2013 - 2018 Stephan Hessenberger for the Max-Planck-Institute for Physics in Munich


#include "types.h"

C> @brief The subroutines sets the values of the tree-level vector-
C> and axial-vector-couplings.
C>
C> The tree-level vector couplings of the Z boson to a fermion species f are given by
C> \f[v_f=I_3^f-2s_W^2Q_f,\f]
C> with the third isospin component \f$I_3^f\f$ and the electric charge \f$Q_f\f$.
C> The corresponding tree-level axial-vector coupling is given by
C> \f[a_f=I_3^f.\f]
C>
C> @ingroup group_Input
      subroutine SetTreeCouplings(vf,af)
C> @param[out] The array vf gives the tree-level vector couplings.
C> The different entries in the array correspond to:
C> - 1: vector coupling of the neutrino
C> - 2: vector coupling of the electron
C> - 3: vector coupling of the muon
C> - 4: vector coupling of the tau lepton
C> - 5: vector coupling of the up quark
C> - 6: vector coupling of the down quark
C> - 7: vector coupling of the charm quark
C> - 8: vector coupling of the strange quark
C> - 9: vector coupling of the bottom quark
      RealType vf(1:9)
C> @param[out] The array af gives the tree-level axial vector couplings.
C> The fermion species corresponding to the entries of the array
C> are equivalent to the entries in the vector
C> coupling array vf.
      RealType af(1:9)
      Integer ivar

#include "paraSM.h"



      Do 100 ivar=1,9

      vf(ivar)=I3f(ivar)-2*SW2*Qf(ivar)
      af(ivar)=I3f(ivar)

100   Continue


      END
