SUBROUTINE allocate_l3_output_variables(local_error)

  USE SAO_L3_variables_mod, ONLY:input, out_grid, out_ndim, L3_output_L2, &
       L3_output_CTM, errmsg

  IMPLICIT NONE

  ! ---------------------
  ! Input/Output variable
  ! ---------------------
  INTEGER*4 :: local_error

  ! ---------------
  ! Local variables
  ! ---------------
  INTEGER*4 :: ivar, idim
  INTEGER*4, DIMENSION(5) :: size_mask

  local_error = 0
  errmsg = 'Error allocating L3 output variables'

  ALLOCATE(L3_output_L2(1:input%L2_ngrid_fields), STAT=local_error)
  IF (local_error .NE. 0) GO TO 99
  DO ivar = 1, input%L2_ngrid_fields
     L3_output_L2(ivar)%name=input%L2_grid_fields(ivar)
     size_mask = 1
     DO idim = 1, 5
        IF (input%L2_grid_fields_dim_idx(idim,ivar) .NE. 0) THEN
           size_mask(idim) = out_grid(input%L2_grid_fields_dim_idx(idim,ivar))%out_dim_size
        ENDIF
     END DO
     SELECT CASE (input%L2_grid_fields_data_type(ivar))
        CASE (2)
           ALLOCATE(L3_output_L2(ivar)%data_int(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE (4)
           ALLOCATE(L3_output_L2(ivar)%data_real_4(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE (8)
           ALLOCATE(L3_output_L2(ivar)%data_real_8(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE DEFAULT
           errmsg = TRIM(errmsg) // '; bad data type in L3 allocation ' // &
                TRIM(L3_output_L2(ivar)%name)
           GO TO 99
        END SELECT
  END DO

  ALLOCATE(L3_output_CTM(1:input%CTM_ngrid_fields), STAT=local_error)
  IF (local_error .NE. 0) GO TO 99
  DO ivar = 1, input%CTM_ngrid_fields
     L3_output_CTM(ivar)%name=input%CTM_grid_fields(ivar)
     size_mask = 1
     DO idim = 1, 5
        IF (input%CTM_grid_fields_dim_idx(idim,ivar) .NE. 0) THEN
           size_mask(idim) = out_grid(input%CTM_grid_fields_dim_idx(idim,ivar))%out_dim_size
        ENDIF
     END DO
     SELECT CASE (input%CTM_grid_fields_data_type(ivar))
        CASE (2)
           ALLOCATE(L3_output_CTM(ivar)%data_int(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE (4)
           ALLOCATE(L3_output_CTM(ivar)%data_real_4(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE (8)
           ALLOCATE(L3_output_CTM(ivar)%data_real_8(1:size_mask(1),               &
                1:size_mask(2), 1:size_mask(3), 1:size_mask(4), 1:size_mask(5)), &
                STAT=local_error)
        CASE DEFAULT
           errmsg = TRIM(errmsg) // '; bad data type in L3 allocation ' // &
                TRIM(L3_output_CTM(ivar)%name)
           GO TO 99
        END SELECT
  END DO


  RETURN
99 WRITE(*,*) errmsg
  STOP

END SUBROUTINE allocate_l3_output_variables
