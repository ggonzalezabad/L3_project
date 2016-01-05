MODULE SAO_L3_type_mod

  USE SAO_L3_parameters_mod

  IMPLICIT NONE

  ! ====================================================
  ! TYPE INPUT_FILE_TYPE
  !      Keep values of input file (input_nightmare.inp)
  !      Data types definitions: 1 -- > INTEGER*2
  !                              2 -- > INTEGER*4
  !                              4 -- > REAL 4
  !                              8 -- > REAL 8
  !      Gridding options: 1 -- > Tesselation
  !                        2 -- > Point Spread Function
  ! ====================================================
  TYPE INPUT_FILE_TYPE
     LOGICAL                              :: USE_CTM = .FALSE. ! Use CTM LOGICAL
     CHARACTER(LEN=MAXLEN)                :: orbit_list_filename ! Orbit list filename
     CHARACTER(LEN=MAXLEN)                :: output_filename ! Output filename
     CHARACTER(LEN=MAXLEN)                :: grid_filename ! Output grid definition filename
     INTEGER*4                            :: n_L2_fields ! # of fields to be read from L2 files
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_fields ! Field names to be read from L2 files
     REAL*8, DIMENSION(50)                :: L2_fields_data_norm ! Normalization factor for L2 fields to be read
     INTEGER*4, DIMENSION(50)             :: L2_fields_data_type ! Data type for  L2 fields to be read
     INTEGER*4, DIMENSION(50)             :: L2_fields_idx ! Data idx for  L2 fields to be read
     INTEGER*4                            :: n_CTM_fields ! # of fields to be read from CTM files
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_fields ! Field names to be read from CTM files
     REAL*8, DIMENSION(50)                :: CTM_fields_data_norm ! Normalization factor for CTM fields to be read
     INTEGER*4, DIMENSION(50)             :: CTM_fields_data_type ! Data type for CTM fields to be read
     INTEGER*4, DIMENSION(50)             :: CTM_fields_idx ! Data idx for CTM fields to be read
     INTEGER*4, DIMENSION(5)              :: L2_coor_fields_idx ! L2 field idx containing geolocation information
     INTEGER*4, DIMENSION(5)              :: CTM_coor_fields_idx ! CTM idx names containing geolocation information
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_grid_fields ! L2 field names to be gridded
     INTEGER*4                            :: L2_ngrid_fields ! Number of L2 field to be gridded
     INTEGER*4, DIMENSION(50)             :: L2_grid_fields_idx ! Data idx for  L2 fields to be gridded
     INTEGER*4, DIMENSION(50)             :: L2_grid_fields_data_type ! Data type for  L2 fields to be gridded
     INTEGER*4, DIMENSION(5,50)           :: L2_grid_fields_dim_idx ! L3 dim idx
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_grid_fields ! CTM field names to be gridded
     INTEGER*4                            :: CTM_ngrid_fields ! Number of CTM fields to be gridded
     INTEGER*4, DIMENSION(50)             :: CTM_grid_fields_idx ! Data idx for CTM fields to be gridded
     INTEGER*4, DIMENSION(50)             :: CTM_grid_fields_data_type ! Data type for CTM fields to be gridded
     INTEGER*4, DIMENSION(5,50)           :: CTM_grid_fields_dim_idx ! L3 dim idx
     INTEGER*4                            :: n_L2_filter ! Number of L2 filters
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_filter_field ! L2 field names to be used as filters
     INTEGER*4, DIMENSION(50)             :: L2_filter_idx   ! L2 filters idx
     INTEGER*4, DIMENSION(50)             :: L2_filter_logic ! Logical for L2 filters
     INTEGER*4, DIMENSION(2,50)           :: L2_filter_arith ! Arithmetic logic for L2 filters
     REAL*4, DIMENSION(2,50)              :: L2_filter_value ! L2 filter values to define filters
     INTEGER*4                            :: n_CTM_filter ! Number of CTM filters
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_filter_field ! CTM field names to be used as filters
     INTEGER*4, DIMENSION(50)             :: CTM_filter_idx   ! CTM filters idx
     INTEGER*4, DIMENSION(50)             :: CTM_filter_logic ! Logical for CTM filters
     INTEGER*4, DIMENSION(2,50)           :: CTM_filter_arith ! Arithmetic logic for CTM filters
     REAL*4, DIMENSION(2,50)              :: CTM_filter_value ! CTM filter values to define filters
     INTEGER*4                            :: Gridding_option ! Gridding option
     LOGICAL                              :: AMF_recalculation = .FALSE. ! Logical to re compute AMF
     CHARACTER(LEN=MAXLEN), DIMENSION(6)  :: AMF_recalculation_fields    ! Fields involved in AMF computation     
  END type INPUT_FILE_TYPE

  ! --------------------------------------
  ! TYPE to store output grid readed from
  ! file specified in control file (Output
  ! Grid Input field).
  ! Each dimension of the grid needs to be
  ! written in the input file with the
  ! following information
  ! -Dimension name
  ! -Dimension idx, dimension # points
  ! -Dimension values(1 each line)
  ! --------------------------------------
  TYPE OUTPUT_GRID_TYPE
     CHARACTER(MAXLEN)                 :: out_dim_name ! Dimension name (we can output to L3 file)
     INTEGER*4                         :: out_dim_idx  ! Dimension bookeeping index (it should match CTM and L2 files)
     INTEGER*4                         :: out_dim_size ! Dimension size
     REAL*8, DIMENSION(:), ALLOCATABLE :: out_dim_val  ! Dimension values
  END type OUTPUT_GRID_TYPE

  ! -----------------------------
  ! TYPE to hold output variables
  ! -----------------------------
  TYPE OUTPUT_L3_TYPE
     CHARACTER(MAXLEN) :: name
     INTEGER*4, DIMENSION(:,:,:,:,:), ALLOCATABLE :: data_int
     REAL*4,    DIMENSION(:,:,:,:,:), ALLOCATABLE :: data_real_4
     REAL*8,    DIMENSION(:,:,:,:,:), ALLOCATABLE :: data_real_8
  END type OUTPUT_L3_TYPE

  ! ----------------------------
  ! TYPE to hold input variables
  ! ----------------------------
  TYPE INPUT_L2_TYPE
     CHARACTER(MAXLEN) :: name
     REAL*4,    DIMENSION(:,:,:,:), ALLOCATABLE :: data
  END type INPUT_L2_TYPE

  ! -----------------------------
  ! TYPE to hold filter variables
  ! -----------------------------
  TYPE FILTER_TYPE
     CHARACTER(MAXLEN) :: name
     LOGICAL, DIMENSION(:,:,:,:), ALLOCATABLE :: data
  END type FILTER_TYPE

END MODULE SAO_L3_type_mod
