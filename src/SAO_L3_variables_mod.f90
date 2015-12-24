MODULE SAO_L3_variables_mod

  USE SAO_L3_parameters_mod, ONLY:MAXLEN

  IMPLICIT NONE
  
  ! -----------------------
  ! L2 Orbit list variables
  ! -----------------------
  INTEGER*4 :: n_L2_files
  CHARACTER(LEN=MAXLEN), ALLOCATABLE :: L2_files(:)

  ! ---------------
  ! Error variables
  ! ---------------
  CHARACTER(LEN=MAXLEN) :: errmsg

END MODULE SAO_L3_VARIABLES_MOD
