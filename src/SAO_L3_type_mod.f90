MODULE SAO_L3_type_mod

  USE SAO_L3_parameters_mod

  IMPLICIT NONE

  ! ====================================================
  ! TYPE INPUT_FILE_TYPE
  !      Keep values of input file (input_nightmare.inp)
  !      Data types definitions: 1 -- > CHARACTER
  !                              2 -- > INTEGER
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
     INTEGER*4                            :: n_CTM_fields ! # of fields to be read from CTM files
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_fields ! Field names to be read from CTM files
     REAL*8, DIMENSION(50)                :: CTM_fields_data_norm ! Normalization factor for CTM fields to be read
     INTEGER, DIMENSION(50)               :: CTM_fields_data_type ! Data type for CTM fields to be read
     CHARACTER(LEN=MAXLEN), DIMENSION(5)  :: L2_coor_fields ! L2 field names containing geolocation information
     CHARACTER(LEN=MAXLEN), DIMENSION(5)  :: CTM_coor_fields ! CTM field names containing geolocation information
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_grid_fields ! L2 field names to be gridded
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_grid_fields ! CTM field names to be gridded
     INTEGER*4                            :: n_L2_filter ! Number of L2 filters
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_filter_field ! L2 field names to be used as filters
     INTEGER*4, DIMENSION(50)             :: L2_filter_logic ! Number of L2 filters
     INTEGER*4, DIMENSION(2,50)           :: L2_filter_arith ! Number of L2 filters
     REAL*8, DIMENSION(2,50)              :: L2_filter_value ! L2 filter values to define filters
     INTEGER*4                            :: n_CTM_filter ! Number of CTM filters
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_filter_field ! CTM field names to be used as filters
     INTEGER*4, DIMENSION(50)             :: CTM_filter_logic ! Number of L2 filters
     INTEGER*4, DIMENSION(2,50)           :: CTM_filter_arith ! Number of L2 filters
     REAL*8, DIMENSION(2,50)              :: CTM_filter_value ! CTM filter values to define filters
!--------------
     INTEGER*4                            :: Gridding_option ! Gridding option
     LOGICAL                              :: AMF_recalculation = .FALSE. ! Logical to re compute AMF
     CHARACTER(LEN=MAXLEN), DIMENSION(6)  :: AMF_recalculation_fields    ! Fields involved in AMF computation
  END type INPUT_FILE_TYPE

END MODULE SAO_L3_type_mod
