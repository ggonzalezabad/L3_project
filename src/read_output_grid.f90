SUBROUTINE read_output_grid(local_error)

  USe SAO_L3_parameters_mod, ONLY: funit_out_grid
  USE SAO_L3_variables_mod, ONLY: input, out_ndim, out_grid, errmsg
  IMPLICIT NONE
  
  ! ---------------------
  ! Input/Output variable
  ! ---------------------
  INTEGER*4, INTENT(INOUT) :: local_error

  ! ---------------
  ! Local variables
  ! ---------------
  INTEGER*4 :: idim
  local_error = 0
  
  errmsg = 'Error reading output grid file'
  OPEN(UNIT=funit_out_grid, FILE=TRIM(ADJUSTL(input%grid_filename)), &
       ACTION='READ',STATUS='OLD',IOSTAT = local_error, ERR = 99)
  READ(funit_out_grid, *, IOSTAT=local_error, ERR=99) out_ndim

  ! Allocate variable out_grid
  ALLOCATE(out_grid(1:out_ndim), STAT=local_error)
  IF (local_error .NE. 0) GO TO 99

  ! Loop over dimensions
  DO idim = 1, out_ndim
     READ(funit_out_grid, '(A)', IOSTAT=local_error, ERR=99) &
          out_grid(idim)%out_dim_name !Reading dimension name
     READ(funit_out_grid, *, IOSTAT=local_error, ERR=99) & !Reading idx and size
          out_grid(idim)%out_dim_idx, out_grid(idim)%out_dim_size
     ALLOCATE(out_grid(idim)%out_dim_val(1:out_grid(idim)%out_dim_size), &
          STAT=local_error)
     IF (local_error .NE. 0) GO TO 99
     READ(funit_out_grid, *, IOSTAT=local_error, ERR=99) &
          out_grid(idim)%out_dim_val
  END DO

  CLOSE(UNIT=funit_out_grid)

  RETURN

99 WRITE(*,*) TRIM(errmsg)
  STOP
END SUBROUTINE read_output_grid
