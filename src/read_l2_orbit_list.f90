SUBROUTINE read_l2_file_list(local_error)

  USE SAO_L3_parameters_mod, ONLY: funit_L2
  USE SAO_L3_variables_mod, ONLY: n_L2_files, L2_files, errmsg
  USE SAO_L3_read_control_file_mod, ONLY: input


  IMPLICIT NONE

  ! ---------------
  ! In/Out variable
  ! ---------------
  INTEGER*4, INTENT(INOUT) :: local_error

  ! ---------------
  ! Local variables
  ! ---------------
  INTEGER*4 :: ifile

  local_error = 0
  errmsg = 'Error reading L2 file list'

  ! ---------------------------
  ! Find out number of L2 files
  ! ---------------------------
  n_L2_files = 0
  OPEN(UNIT=funit_L2, FILE=TRIM(ADJUSTL(input%orbit_list_filename)), &
  STATUS='OLD', ACTION='READ', IOSTAT=local_error, ERR=98 )

  DO
     READ(UNIT=funit_L2, FMT='(A)', IOSTAT=local_error, END=10)
     n_L2_files = n_L2_files + 1
  END DO
  10 CLOSE(funit_L2)

  ! -----------------
  ! Allocate L2_files
  ! -----------------
  ALLOCATE(L2_files(1:n_L2_files), STAT=local_error)
  IF (local_error .NE. 0) GOTO 98

  ! ------------------
  ! Read L3 file names
  ! ------------------
  OPEN(funit_L2, file=input%orbit_list_filename,iostat=local_error,err=98)
  DO ifile = 1, n_L2_files
     READ(funit_L2,'(A)',iostat=local_error,err=98) L2_files(ifile)
  END DO
  CLOSE(funit_L2)  

  RETURN

98 WRITE(*,*) errmsg
  WRITE(*,*) local_error
  STOP

END SUBROUTINE read_l2_file_list
