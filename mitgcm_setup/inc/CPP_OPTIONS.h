! $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code/CPP_OPTIONS.h,v 1.15 2013/01/23 17:00:15 jmc Exp $
! $Name:  $

#ifndef CPP_OPTIONS_H
#define CPP_OPTIONS_H

!BOP
! !ROUTINE: CPP_OPTIONS.h
! !INTERFACE:
! #include "CPP_OPTIONS.h"

! !DESCRIPTION:
! *==================================================================*
! | main CPP options file for the model:
! | Control which optional features to compile in model/src code.
! *==================================================================*
!EOP

! CPP flags controlling particular source code features

! o Shortwave heating as extra term in external_forcing.F
! Note: this should be a run-time option
#define SHORTWAVE_HEATING

! o Include/exclude phi_hyd calculation code
#define INCLUDE_PHIHYD_CALCULATION_CODE

! o Include/exclude call to S/R CONVECT
#define INCLUDE_CONVECT_CALL

! o Include/exclude call to S/R CALC_DIFFUSIVITY
#define INCLUDE_CALC_DIFFUSIVITY_CALL

! o Allow full 3D specification of vertical diffusivity
#undef ALLOW_3D_DIFFKR

! o Allow latitudinally varying BryanLewis79 vertical diffusivity
#undef ALLOW_BL79_LAT_VARY

! o Include/exclude Implicit vertical advection code
#define INCLUDE_IMPLVERTADV_CODE

! o Include/exclude AdamsBashforth-3rd-Order code
#undef ALLOW_ADAMSBASHFORTH_3

! o Include/exclude nonHydrostatic code
#define ALLOW_NONHYDROSTATIC

! o Allow to account for heating due to friction (and momentum dissipation)
#undef ALLOW_FRICTION_HEATING

! o Allow mass source or sink of Fluid in the interior
!   (3-D generalisation of oceanic real-fresh water flux)
#undef ALLOW_ADDFLUID

! o Include pressure loading code
#define ATMOSPHERIC_LOADING

! o exclude/allow external forcing-fields load
!   this allows to read & do simple linear time interpolation of oceanic
!   forcing fields, if no specific pkg (e.g., EXF) is used to compute them.
#undef EXCLUDE_FFIELDS_LOAD

! o Include/exclude balancing surface forcing fluxes code
#define ALLOW_BALANCE_FLUXES

! o Include/exclude balancing surface forcing relaxation code
#define ALLOW_BALANCE_RELAX

! o Include/exclude GM-like eddy stress in momentum code
#undef ALLOW_EDDYPSI

! o Use "Exact Convervation" of fluid in Free-Surface formulation
!   so that d/dt(eta) is exactly equal to - Div.Transport
#define EXACT_CONSERV

! o Allow the use of Non-Linear Free-Surface formulation
!   this implies that surface thickness (hFactors) vary with time
#define NONLIN_FRSURF

! o Include/exclude code for single reduction Conjugate-Gradient solver
#define ALLOW_SRCG

! o Choices for implicit solver routines solve_*diagonal.F
!   The following has low memory footprint, but not suitable for AD
#undef SOLVE_DIAGONAL_LOWMEMORY
!   The following one suitable for AD but does not vectorize
#undef SOLVE_DIAGONAL_KINNER

! o ALLOW isotropic scaling of harmonic and bi-harmonic terms when
!   using an locally isotropic spherical grid with (dlambda) x (dphi*cos(phi))
! *only for use on a lat-lon grid*
!   Setting this flag here affects both momentum and tracer equation unless
!   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
!   The definition of the flag is commented to avoid interference with
!   such other header files.
!   The preferred method is specifying a value for viscAhGrid or viscA4Grid
!   in data which is then automatically scaled by the grid size;
!   the old method of specifying viscAh/viscA4 and this flag is provided
!   for completeness only (and for use with the adjoint).
!#define ISOTROPIC_COS_SCALING

! o This flag selects the form of COSINE(lat) scaling of bi-harmonic term.
! *only for use on a lat-lon grid*
!   Has no effect if ISOTROPIC_COS_SCALING is undefined.
!   Has no effect on vector invariant momentum equations.
!   Setting this flag here affects both momentum and tracer equation unless
!   it is set/unset again in other header fields (e.g., GAD_OPTIONS.h).
!   The definition of the flag is commented to avoid interference with
!   such other header files.
!#define COSINEMETH_III

! o Use "OLD" UV discretisation near boundaries (*not* recommended)
!   Note - only works with  #undef NO_SLIP_LATERAL  in calc_mom_rhs.F
!          because the old code did not have no-slip BCs
#undef  OLD_ADV_BCS

! o Use LONG.bin, LATG.bin, etc., initialization for ini_curviliear_grid.F
!   Default is to use "new" grid files (OLD_GRID_IO undef) but OLD_GRID_IO
!   is still useful with, e.g., single-domain curvilinear configurations.
#undef OLD_GRID_IO

! o Use thsice+seaice (old) call sequence: ice-Dyn,ice-Advect,ice-Thermo(thsice)
!              as opposed to new sequence: ice-Thermo(thsice),ice-Dyn,ice-Advect
#undef OLD_THSICE_CALL_SEQUENCE

! o Execution environment support options
#include "CPP_EEOPTIONS.h"

! o Include/exclude single header file containing multiple packages options
!   (AUTODIFF, COST, CTRL, ECCO, EXF ...) instead of the standard way where
!   each of the above pkg get its own options from its specific option file.
!   Although this method, inherited from ECCO setup, has been traditionally
!   used for all adjoint built, work is in progress to allow to use the
!   standard method also for adjoint built.
!#ifdef PACKAGES_CONFIG_H
!# include "ECCO_CPPOPTIONS.h"
!#endif

#endif /* CPP_OPTIONS_H */
