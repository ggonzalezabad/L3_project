MODULE SAO_L3_type_mod

  IMPLICIT NONE

  INTEGER*2, PARAMETER :: MAXLEN = 254

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
     INTEGER*2                            :: n_L2_fields ! # of fields to be read from L2 files
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_fields ! Field names to be read from L2 files
     REAL*8, DIMENSION(50)                :: L2_fields_data_norm ! Normalization factor for L2 fields to be read
     INTEGER*2, DIMENSION(50)             :: L2_fields_data_type ! Data type for  L2 fields to be read
     INTEGER*2                            :: n_CTM_fields ! # of fields to be read from CTM files
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_fields ! Field names to be read from CTM files
     REAL*8, DIMENSION(50)                :: CTM_fields_data_norm ! Normalization factor for CTM fields to be read
     INTEGER, DIMENSION(50)               :: CTM_fields_data_type ! Data type for CTM fields to be read
     CHARACTER(LEN=MAXLEN), DIMENSION(5)  :: L2_coor_fields ! L2 field names containing geolocation information
     CHARACTER(LEN=MAXLEN), DIMENSION(5)  :: CTM_coor_fields ! CTM field names containing geolocation information
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_grid_fields ! L2 field names to be gridded
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_grid_fields ! CTM field names to be gridded
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: L2_filter_fields ! L2 field names to be used as filters
     REAL*8, DIMENSION(2,5,50)            :: L2_fields_values ! L2 filter values to define filters
     CHARACTER(LEN=MAXLEN), DIMENSION(50) :: CTM_filter_fields ! CTM field names to be used as filters
     REAL*8, DIMENSION(2,5,50)            :: CTM_fields_values ! CTM filter values to define filters
     INTEGER*2                            :: Gridding_option ! Gridding option
     LOGICAL                              :: AMF_recalculation = .FALSE. ! Logical to re compute AMF
     CHARACTER(LEN=MAXLEN), DIMENSION(6)  :: AMF_recalculation_fields    ! Fields involved in AMF computation
  END type INPUT_FILE_TYPE

END MODULE SAO_L3_type_mod
