PROGRAM test_main

  USE SAO_L3_parameters_mod
  USE SAO_L3_read_control_file_mod, ONLY : input, in_ctr_file, &
       read_input_control_file

  IMPLICIT NONE

  INTEGER*4 :: errstat
  ! -----------------------
  ! Read input control file
  ! -----------------------
  ! Obtain filename
  ! ---------------
  CALL GETARG(1,in_ctr_file)

  ! ------------------------------------------------------
  ! Call control file reader (in SAO_L3_read_control_file)
  ! ------------------------------------------------------
  CALL read_input_control_file(errstat)

  print*, input
  STOP

END PROGRAM test_main
