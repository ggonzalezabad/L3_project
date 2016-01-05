PROGRAM test_main

  USE SAO_L3_read_control_file_mod, ONLY : in_ctr_file, &
       read_input_control_file
  USE SAO_L3_variables_mod, ONLY: n_L2_files, L2_files, L2_data, input
  USE SAO_L3_type_mod, ONLY: FILTER_TYPE

  IMPLICIT NONE

  ! ---------------
  ! Local variables
  ! ---------------
  INTEGER*4 :: errstat, ifile, ifilter

  ! --------------------------------------
  ! Local variables for filtering approach
  ! Subject to discussion with Chris
  ! --------------------------------------
  TYPE(FILTER_TYPE), DIMENSION(:), ALLOCATABLE :: FILTER_MASK
  INTEGER*4, DIMENSION(4) :: FILTER_DATA_SIZE

  errstat = 0
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

  ! ------------------
  ! Loop over L2 files
  ! ------------------
  DO ifile = 1, n_L2_files
     print*, ifile
     CALL read_he5_file(TRIM(ADJUSTL(L2_files(ifile))), errstat)

     ! ---------------------------
     ! Temptative filtering squeme
     ! ---------------------------
     ALLOCATE( FILTER_MASK(1:input%n_L2_filter), STAT=errstat)

     DO ifilter = 1, input%n_L2_filter

        FILTER_MASK(ifilter)%name = TRIM(ADJUSTL(input%L2_filter_field(ifilter)))
        FILTER_DATA_SIZE = SHAPE(L2_data(input%L2_filter_idx(ifilter))%data)
        ALLOCATE(FILTER_MASK(ifilter)%data(1:filter_data_size(1), &
             1:filter_data_size(2), &
             1:filter_data_size(3), &
             1:filter_data_size(4)), stat = errstat)
        FILTER_MASK(ifilter)%data = .FALSE.

        SELECT CASE (input%L2_filter_logic(ifilter))
        CASE (11) !.AND.
           SELECT CASE (input%L2_filter_arith(1,ifilter))
           CASE (1) !.EQ.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (2) !.LE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (3) !.LT.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (4) !.GE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (5) !.GT.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (6) !.NE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .AND. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE DEFAULT
              print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
              stop   
           END SELECT

        CASE (12)
           SELECT CASE (input%L2_filter_arith(1,ifilter))
           CASE (1) !.EQ.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .EQ.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (2) !.LE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (3) !.LT.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .LT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (4) !.GE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (5) !.GT.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .GT.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE (6) !.NE.
              SELECT CASE (input%L2_filter_arith(2,ifilter))
              CASE (1) !.EQ.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .EQ.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (2) !.LE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (3) !.LT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .LT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (4) !.GE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (5) !.GT.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .GT.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE (6) !.NE.
                 WHERE(L2_data(input%L2_filter_idx(ifilter))%data .NE.  &
                      input%L2_filter_value(1,ifilter)            .OR. &
                      L2_data(input%L2_filter_idx(ifilter))%data  .NE.  &
                      input%L2_filter_value(2,ifilter))
                    FILTER_MASK(ifilter)%data = .TRUE.
                 END WHERE
              CASE DEFAULT
                 print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
                 stop
              END SELECT
           CASE DEFAULT
              print*, 'You must say if the condition is .EQ., .LE., .LT., .GE., .GT. or .NE.'
              stop   
           END SELECT
           
        CASE DEFAULT
           print*, 'You must say it the condition is .AND. or .OR.'
           stop
        END SELECT
     END DO

     IF (ALLOCATED(L2_data)) DEALLOCATE(L2_data)
     IF (ALLOCATED(FILTER_MASK)) DEALLOCATE(FILTER_MASK)

  END DO
  ! --------------------------------
  ! Deallocate allocatable variables
  ! --------------------------------
  CALL deallocate(errstat)

  STOP

END PROGRAM test_main
