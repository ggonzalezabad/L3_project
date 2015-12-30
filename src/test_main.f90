PROGRAM test_main

  USE SAO_L3_read_control_file_mod, ONLY : in_ctr_file, &
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

  ! ------------------
  ! Read L2 orbit list
  ! ------------------
  CALL read_l2_file_list(errstat)

  ! ----------------
  ! Read Output grid
  ! ----------------
  CALL read_output_grid(errstat)

  ! ---------------------------------
  ! Allocate output variables
  ! We are going to keep the same
  ! variable type that the input
  ! data.
  ! Security check to be sure that
  ! the output variable is read from
  ! L2 or CTM files
  ! --------------------------------
  CALL allocate_l3_output_variables(errstat)

  ! --------------------------------
  ! Deallocate allocatable variables
  ! --------------------------------
  CALL deallocate(errstat)

  STOP

END PROGRAM test_main
