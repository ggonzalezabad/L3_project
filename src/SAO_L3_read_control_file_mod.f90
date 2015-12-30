MODULE SAO_L3_read_control_file_mod

  USE SAO_L3_parameters_mod, ONLY: MAXLEN, funit_ctr
  USE SAO_L3_variables_mod, ONLY: input

  IMPLICIT NONE

  ! ------------------------------------------------
  ! Strings to locate sections in control input file
  ! ------------------------------------------------
  CHARACTER(LEN=7),  PARAMETER :: yn_ctm_str ='Use CTM'
  CHARACTER(LEN=15), PARAMETER :: L2_orbits_str ='List of L2 Data'
  CHARACTER(LEN=17), PARAMETER :: L2_read_fields_str ='L2 Fields to Read'
  CHARACTER(LEN=18), PARAMETER :: CTM_read_fields_str ='CTM Fields to Read'
  CHARACTER(LEN=16), PARAMETER :: out_file_str = 'Output File Name'
  CHARACTER(LEN=17), PARAMETER :: out_grid_str = 'Output Grid Input'
  CHARACTER(LEN=20), PARAMETER :: L2_coor_str = 'L2 Coordinate Fields'
  CHARACTER(LEN=21), PARAMETER :: CTM_coor_str = 'CTM Coordinate Fields'
  CHARACTER(LEN=17), PARAMETER :: L2_grid_fields_str = 'L2 Fields to Grid'
  CHARACTER(LEN=18), PARAMETER :: CTM_grid_fields_str = 'CTM Fields to Grid'
  CHARACTER(LEN=10), PARAMETER :: L2_filters_str = 'L2 Filters'
  CHARACTER(LEN=11), PARAMETER :: CTM_filters_str = 'CTM Filters'
  CHARACTER(LEN=16), PARAMETER :: grid_option_str = 'Gridding Options'
  CHARACTER(LEN=17), PARAMETER :: yn_AMF_str = 'AMF Recalculation'

  ! Input filename variable
  CHARACTER(LEN=MAXLEN) :: in_ctr_file

CONTAINS

  SUBROUTINE skip_to_filemark ( funit, lm_string, lastline, file_read_stat )

    IMPLICIT NONE
    
    ! ===============
    ! Input variables
    ! ===============
    INTEGER*4,        INTENT (IN) :: funit
    CHARACTER(LEN=*), INTENT (IN) :: lm_string
    
    ! ================
    ! Output variables
    ! ================
    INTEGER*4,             INTENT (OUT) :: file_read_stat
    CHARACTER(LEN=MAXLEN), INTENT (OUT) :: lastline
    
    ! ===============
    ! Local variables
    ! ===============
    INTEGER*4    :: lmlen, ios
    CHARACTER(LEN=MAXLEN) :: tmpline
    
    ! -----------
    ! Rewind file
    ! -----------
    REWIND(funit)
    ! -------------------------------------------
    ! Determine the length of the string landmark
    ! -------------------------------------------
    lmlen = LEN(TRIM(ADJUSTL(lm_string)))
    
    ! -------------------------------------------------------
    ! Read lines in the file until we either find the string,
    ! reach the end of the file, or reading fails otherwise.
    ! ----------------------------------------------------
    ios = 0
    getlm: DO WHILE ( ios == 0 )
       READ (UNIT=funit, FMT='(A)', IOSTAT=ios) tmpline
       tmpline = TRIM(ADJUSTL(tmpline))
       IF ( ios /= 0 .OR. TRIM(ADJUSTL(tmpline)) == TRIM(ADJUSTL(lm_string)) ) EXIT getlm
    END DO getlm
    
    ! ---------------------------------------------------
    ! Return the last line read for the case that we need 
    ! to extract further information from it
    ! ---------------------------------------------------
    lastline = TRIM(ADJUSTL(tmpline))
    file_read_stat = ios
    RETURN
  END SUBROUTINE skip_to_filemark
  
  SUBROUTINE read_input_control_file(local_error)

    IMPLICIT NONE
    
    ! --------------
    ! INOUT variable
    ! --------------
    INTEGER*4, INTENT(INOUT) :: local_error
    CHARACTER(LEN=MAXLEN) :: lastline, errmsg, tmpline
    INTEGER*4 :: pos1, pos2, i_dummy
    
    ! ----------------------------------------------------
    ! Open file, if not possible finish with error message
    ! ----------------------------------------------------
    OPEN(UNIT=funit_ctr, FILE=TRIM(ADJUSTL(in_ctr_file)), &
         ACTION='READ',STATUS='OLD', IOSTAT = local_error, ERR = 99)
    
    ! -----------------------------------------------------------------------
    ! Loop over control file fields (defined in SAO_L3_read_control_file_mod)
    ! 1. Skip to filemark
    ! 2. Different fields should be readed in different ways, that removes
    !    some flexibility. Discuss with Chris!!!!
    ! -----------------------------------------------------------------------
    ! Logical for CTM
    errmsg = 'Error reading '// yn_ctm_str
    CALL skip_to_filemark(funit_ctr, yn_ctm_str, &
         lastline, local_error)
    READ(funit_ctr, *, ERR = 98, IOSTAT=local_error) input%USE_CTM
    ! L2 orbits list
    errmsg = 'Error reading '// L2_orbits_str
    CALL skip_to_filemark(funit_ctr, L2_orbits_str, &
         lastline, local_error)
    READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%orbit_list_filename
    ! L2 fields to be read
    errmsg = 'Error reading '// L2_read_fields_str
    input%n_L2_fields = 0
    CALL skip_to_filemark(funit_ctr, L2_read_fields_str, &
         lastline, local_error)
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF (tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%L2_fields(input%n_L2_fields+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_fields_data_norm(input%n_L2_fields+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:),*) input%L2_fields_data_type(input%n_L2_fields+1)
       input%n_L2_fields = input%n_L2_fields+1
    ENDDO
    ! CTM fields to be read
    errmsg = 'Error reading '// CTM_read_fields_str
    input%n_CTM_fields = 0
    CALL skip_to_filemark(funit_ctr, CTM_read_fields_str, &
         lastline, local_error)
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF (tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%CTM_fields(input%n_CTM_fields+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_fields_data_norm(input%n_CTM_fields+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:),*) input%CTM_fields_data_type(input%n_CTM_fields+1)
       input%n_CTM_fields = input%n_CTM_fields+1
    ENDDO
    ! L3 output file
    errmsg = 'Error reading '// out_file_str
    CALL skip_to_filemark(funit_ctr, out_file_str, &
         lastline, local_error)
    READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%output_filename
    ! Grid specifcation file
    errmsg = 'Error reading '// out_grid_str
    CALL skip_to_filemark(funit_ctr, out_grid_str, &
         lastline, local_error)
    READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%grid_filename
    ! L2 coordinates fields
    errmsg = 'Error reading '// L2_coor_str
    CALL skip_to_filemark(funit_ctr, L2_coor_str, &
         lastline, local_error)
    i_dummy = 0
    DO
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF ( tmpline(2:2) .EQ. ' ') EXIT
       input%L2_coor_fields(i_dummy+1) = tmpline
       i_dummy = i_dummy + 1
    ENDDO
    ! CTM coordinates fields
    errmsg = 'Error reading '// CTM_coor_str
    CALL skip_to_filemark(funit_ctr, CTM_coor_str, &
         lastline, local_error)
    i_dummy = 0
    DO
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF ( tmpline(2:2) .EQ. ' ') EXIT
       input%CTM_coor_fields(i_dummy+1) = tmpline
       i_dummy = i_dummy + 1
    ENDDO
    ! L2 fields to grid
    errmsg = 'Error reading '// L2_grid_fields_str
    CALL skip_to_filemark(funit_ctr, L2_grid_fields_str, &
         lastline, local_error)
    i_dummy = 0
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF (tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%L2_grid_fields(i_dummy+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_data_type(i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_dim_idx(1,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_dim_idx(2,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_dim_idx(3,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_dim_idx(4,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_grid_fields_dim_idx(5,i_dummy+1)
       i_dummy = i_dummy+1
    ENDDO
    input%L2_ngrid_fields = i_dummy

    ! CTM fields to grid
    errmsg = 'Error reading '// CTM_grid_fields_str
    i_dummy = 0
    CALL skip_to_filemark(funit_ctr, CTM_grid_fields_str, &
         lastline, local_error)
    i_dummy = 0
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF (tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%CTM_grid_fields(i_dummy+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_data_type(i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_dim_idx(1,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_dim_idx(2,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_dim_idx(3,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_dim_idx(4,i_dummy+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_grid_fields_dim_idx(5,i_dummy+1)
       i_dummy = i_dummy+1
    ENDDO
    input%CTM_ngrid_fields = i_dummy

    ! L2 filters
    errmsg = 'Error reading '// L2_filters_str
    CALL skip_to_filemark(funit_ctr, L2_filters_str, &
         lastline, local_error)
    input%n_L2_filter = 0
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF ( tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%L2_filter_field(input%n_L2_filter+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_filter_logic(input%n_L2_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_filter_arith(1,input%n_L2_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_filter_value(1,input%n_L2_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%L2_filter_arith(2,input%n_L2_filter+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:),*) input%L2_filter_value(2,input%n_L2_filter+1)
       input%n_L2_filter = input%n_L2_filter + 1
    ENDDO
    ! CTM filters
    errmsg = 'Error reading '// CTM_filters_str
    CALL skip_to_filemark(funit_ctr, CTM_filters_str, &
         lastline, local_error)
    input%n_CTM_filter = 0
    DO
       pos1 = 1
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) tmpline
       IF ( tmpline(2:2) .EQ. ' ') EXIT
       pos2 = INDEX(tmpline(pos1:),":")
       input%CTM_filter_field(input%n_CTM_filter+1) = tmpline(pos1:pos1+pos2-2) 
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_filter_logic(input%n_CTM_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_filter_arith(1,input%n_CTM_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_filter_value(1,input%n_CTM_filter+1)
       pos1 = pos1 + pos2 + 1
       pos2 = INDEX(tmpline(pos1:),":")
       READ(tmpline(pos1:pos1+pos2-2),*) input%CTM_filter_arith(2,input%n_CTM_filter+1)
       pos1 = pos1 + pos2 + 1
       READ(tmpline(pos1:),*) input%CTM_filter_value(2,input%n_CTM_filter+1)
       input%n_CTM_filter = input%n_CTM_filter + 1
    ENDDO
    ! Gridding option
    errmsg = 'Error reading '// grid_option_str
    CALL skip_to_filemark(funit_ctr, grid_option_str, &
         lastline, local_error)
    READ(funit_ctr, *, ERR = 98, IOSTAT=local_error) input%Gridding_option
    ! AMF recalculation options
    errmsg = 'Error reading '// yn_AMF_str
    CALL skip_to_filemark(funit_ctr, yn_AMF_str, &
         lastline, local_error)
    READ(funit_ctr, *, ERR = 98, IOSTAT=local_error) input%AMF_recalculation
    IF (input%AMF_recalculation) THEN
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(1)
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(2)
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(3)
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(4)
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(5)
       READ(funit_ctr, '(A)', ERR = 98, IOSTAT=local_error) input%AMF_recalculation_fields(6)
    END IF
    
    CLOSE(funit_ctr)
    
    RETURN
    
    ! ---------------------------------
    ! For the time being error catching
    ! ---------------------------------
98  WRITE(*,*) TRIM(errmsg)
    WRITE(*,*) local_error
    STOP
    
99  errmsg = 'Problems with ' // TRIM(ADJUSTL(in_ctr_file)) // ' control file!!!'
    WRITE(*,*) TRIM(errmsg)
    STOP
    
  END SUBROUTINE read_input_control_file
  
END MODULE SAO_L3_read_control_file_mod
