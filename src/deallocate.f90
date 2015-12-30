SUBROUTINE deallocate(local_error)

  USE SAO_L3_variables_mod, ONLY: errmsg, L2_files, out_grid, &
       L3_output_L2, L3_output_CTM
  IMPLICIT NONE

  ! ---------------
  ! In/Out variable
  ! ---------------
  INTEGER*4 :: local_error

  local_error = 0
  errmsg = 'Error deallocating variables'
  IF (ALLOCATED(L2_files)) DEALLOCATE(L2_files, STAT=local_error)
  IF (ALLOCATED(out_grid)) DEALLOCATE(out_grid, STAT=local_error)
  IF (ALLOCATED(L3_output_L2)) DEALLOCATE(L3_output_L2, STAT=local_error)
  IF (ALLOCATED(L3_output_CTM)) DEALLOCATE(L3_output_CTM, STAT=local_error)

  IF (local_error .NE. 0) GOTO 98

  RETURN

98 WRITE(*,*) errmsg
  STOP

END SUBROUTINE deallocate
