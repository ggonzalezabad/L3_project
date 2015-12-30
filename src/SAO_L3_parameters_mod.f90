MODULE SAO_L3_parameters_mod

  IMPLICIT NONE

  INTEGER*4, PARAMETER :: MAXLEN = 254

  ! ----------
  ! File units
  ! ----------
  INTEGER*4, PARAMETER :: funit_ctr      = 11 !Input control file
  INTEGER*4, PARAMETER :: funit_L2       = 12 !L2 file list
  INTEGER*4, PARAMETER :: funit_out_grid = 13 !Output grid file

END MODULE SAO_L3_parameters_mod
