MODULE SAO_L3_variables_mod

  USE SAO_L3_parameters_mod, ONLY:MAXLEN, rp
  USE SAO_L3_type_mod, ONLY: INPUT_FILE_TYPE, OUTPUT_GRID_TYPE, &
       OUTPUT_L3_TYPE, INPUT_L2_TYPE

  IMPLICIT NONE

  ! -------------------------------------
  ! Type variable to hold input file info
  ! -------------------------------------
  TYPE(INPUT_FILE_TYPE), SAVE :: input

  ! -----------------------
  ! L2 Orbit list variables
  ! -----------------------
  INTEGER*4 :: n_L2_files
  CHARACTER(LEN=MAXLEN), ALLOCATABLE :: L2_files(:)

  ! ---------------------
  ! Output grid variables
  ! ---------------------
  INTEGER*4 :: out_ndim !Number of dimesions in the output grid
                        !(first line in grid_test.inp file
  TYPE(OUTPUT_GRID_TYPE), DIMENSION(:), ALLOCATABLE :: out_grid !Data for each
                                                                !dimension

  ! -------------------
  ! L3 output variables
  ! -------------------
  TYPE(OUTPUT_L3_TYPE), DIMENSION(:), ALLOCATABLE :: L3_output_L2
  TYPE(OUTPUT_L3_TYPE), DIMENSION(:), ALLOCATABLE :: L3_output_CTM

  ! -------------------
  ! L2 input variables
  ! -------------------
  TYPE(INPUT_L2_TYPE), DIMENSION(:), ALLOCATABLE :: L2_data
  TYPE(INPUT_L2_TYPE), DIMENSION(:), ALLOCATABLE :: CTM_data

  ! ---------------
  ! Error variables
  ! ---------------
  CHARACTER(LEN=MAXLEN) :: errmsg

END MODULE SAO_L3_VARIABLES_MOD
