SUBROUTINE read_he5_file(filename, local_error)

  USE HDF5
  USE readh5dataset
  USE SAO_L3_parameters_mod
  USE SAO_L3_variables_mod, ONLY:input, L2_data

  IMPLICIT NONE
  
  ! ----------------------
  ! Input/Output variables
  ! ----------------------
  CHARACTER(LEN=*), INTENT(INOUT) :: filename
  INTEGER*4,        INTENT(INOUT) :: local_error

  ! ---------------
  ! Local variables
  ! ---------------
  CHARACTER(LEN=MAXLEN) :: errmsg
  INTEGER(HID_T) :: file_id, dataspace_id, dataset_id
  INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: dims, maxdims
  INTEGER*4 :: rank, ifield
  INTEGER*4, DIMENSION(4) :: size_mask

  ! Dummy variables to read L2 file datasets
  INTEGER*4, DIMENSION(:), POINTER :: dummy_1D_int_4 => NULL()
  INTEGER*4, DIMENSION(:,:), POINTER :: dummy_2D_int_4 => NULL()
  INTEGER*4, DIMENSION(:,:,:), POINTER :: dummy_3D_int_4 => NULL()
  INTEGER*4, DIMENSION(:,:,:,:), POINTER :: dummy_4D_int_4 => NULL()

  REAL*4, DIMENSION(:), POINTER :: dummy_1D_real_4 => NULL()
  REAL*4, DIMENSION(:,:), POINTER :: dummy_2D_real_4 => NULL()
  REAL*4, DIMENSION(:,:,:), POINTER :: dummy_3D_real_4 => NULL()
  REAL*4, DIMENSION(:,:,:,:), POINTER :: dummy_4D_real_4 => NULL()

  REAL*8, DIMENSION(:), POINTER :: dummy_1D_real_8 => NULL()
  REAL*8, DIMENSION(:,:), POINTER :: dummy_2D_real_8 => NULL()
  REAL*8, DIMENSION(:,:,:), POINTER :: dummy_3D_real_8 => NULL()
  REAL*8, DIMENSION(:,:,:,:), POINTER :: dummy_4D_real_8 => NULL()
  
  errmsg = 'Error reading he5 dataset'

  local_error = 0

  ALLOCATE(L2_data(1:input%n_L2_fields), STAT=local_error)
  IF (local_error .NE. 0) GO TO 99
  
  ! Open file
  CALL H5FOPEN_F(filename, H5F_ACC_RDONLY_F, file_id, local_error)
  IF (local_error .NE. 0) GO TO 99

  DO ifield = 1, input%n_L2_fields
     L2_data(ifield)%name = TRIM(ADJUSTL(input%L2_fields(ifield)))
     size_mask = 1
     ! Find Field dimensions (necessary to allocate variable)
     CALL H5DOPEN_F(file_id, TRIM(ADJUSTL(input%L2_fields(ifield))), &
          dataset_id, local_error)
     IF (local_error .NE. 0) GO TO 99
     
     CALL H5DGET_SPACE_F(dataset_id, dataspace_id, local_error)
     IF (local_error .NE. 0) GO TO 99
     
     CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dataspace_id, rank, local_error)
     IF (rank .GT. 4) THEN
        errmsg = TRIM(ADJUSTL(errmsg)) // ' no possible to read L2 variables with rank above 4'
        GO TO 99
     END IF

     ALLOCATE(dims(1:rank), STAT=local_error)
     IF (local_error .NE. 0) GO TO 99
     ALLOCATE(maxdims(1:rank), STAT=local_error)
     IF (local_error .NE. 0) GO TO 99
     
     CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dataspace_id, dims, maxdims, local_error)
     IF (local_error .EQ. -1) GO TO 99

     CALL H5DCLOSE_F(dataset_id, local_error)
     IF (local_error .NE. 0) GO TO 99

     CALL H5SCLOSE_F(dataspace_id, local_error)
     IF (local_error .NE. 0) GO TO 99

     size_mask(1:rank) = INT(dims(1:rank), KIND=4)
     ALLOCATE(L2_data(ifield)%data(1:size_mask(1),1:size_mask(2), &
          1:size_mask(3),1:size_mask(4)), STAT=local_error)
     IF (local_error .NE. 0) GO TO 99
    
     ! Now that we have the dimensions of the dataset and we can allocate the L2 variable
     ! to hold the data. The datatype is obtained from input file for now. We can try to
     ! add another layer of flexibility by extracting this info from L2 file prior to
     ! allocation. At this point I'm creating options for INTEGER, REAL*4 and REAL*8
     SELECT CASE (input%L2_fields_data_type(ifield))
     CASE (2)
        IF (rank .EQ. 1) THEN 
           ALLOCATE(dummy_1D_int_4(1:dims(1)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_1D_int_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_1D_int_4, KIND=rp),size_mask)

        END IF
        IF (rank .EQ. 2) THEN 
           ALLOCATE(dummy_2D_int_4(1:dims(1),1:dims(2)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_2D_int_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_2D_int_4, KIND=rp),size_mask)

        END IF
        IF (rank .EQ. 3) THEN 
           ALLOCATE(dummy_3D_int_4(1:dims(1),1:dims(2),1:dims(3)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_3D_int_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
           RESHAPE(REAL(dummy_3D_int_4, KIND=rp),size_mask)

        END IF
        IF (rank .EQ. 4) THEN 
           ALLOCATE(dummy_4D_int_4(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_4D_int_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
           RESHAPE(REAL(dummy_4D_int_4, KIND=rp),size_mask)

        END IF
     CASE (4)
        IF (rank .EQ. 1) THEN
           ALLOCATE(dummy_1D_real_4(1:dims(1)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_1D_real_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_1D_real_4, KIND=rp),size_mask)
           
        END IF
        IF (rank .EQ. 2) THEN
           ALLOCATE(dummy_2D_real_4(1:dims(1),1:dims(2)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_2D_real_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_2D_real_4, KIND=rp),size_mask)
           
        END IF
        IF (rank .EQ. 3) THEN
           ALLOCATE(dummy_3D_real_4(1:dims(1),1:dims(2),1:dims(3)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_3D_real_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_3D_real_4, KIND=rp),size_mask)

        END IF
        IF (rank .EQ. 4) THEN
           ALLOCATE(dummy_4D_real_4(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_4D_real_4)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_4D_real_4, KIND=rp),size_mask)

        END IF
     CASE (8)
        IF (rank .EQ. 1) THEN
           ALLOCATE(dummy_1D_real_8(1:dims(1)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_1D_real_8)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_1D_real_8, KIND=rp),size_mask)
     
        END IF
        IF (rank .EQ. 2) THEN
           ALLOCATE(dummy_2D_real_8(1:dims(1),1:dims(2)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_2D_real_8)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_2D_real_8, KIND=rp),size_mask)
        END IF
        IF (rank .EQ. 3) THEN
           ALLOCATE(dummy_3D_real_8(1:dims(1),1:dims(2),1:dims(3)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_3D_real_8)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_3D_real_8, KIND=rp),size_mask)
           
        END IF
        IF (rank .EQ. 4) THEN
           ALLOCATE(dummy_4D_real_8(1:dims(1),1:dims(2),1:dims(3),1:dims(4)))
           CALL H5ReadDataset(filename, TRIM(ADJUSTL(input%L2_fields(ifield))), &
                dummy_4D_real_8)
           L2_data(ifield)%data(1:size_mask(1),1:size_mask(2),1:size_mask(3),1:size_mask(4)) = &
                RESHAPE(REAL(dummy_4D_real_8, KIND=rp),size_mask)

        END IF
     CASE DEFAULT
        errmsg = TRIM(ADJUSTL(errmsg)) // ': no such data type'
        GO TO 99
     END SELECT

     ! Save data to L2_data variable. All data is converted to REAL*4.
     ! That will simplify further data management.

     IF (ALLOCATED(dims)) DEALLOCATE(dims)
     IF (ALLOCATED(maxdims)) DEALLOCATE(maxdims)
     IF (ASSOCIATED(dummy_1D_int_4)) DEALLOCATE(dummy_1D_int_4)
     IF (ASSOCIATED(dummy_2D_int_4)) DEALLOCATE(dummy_2D_int_4)
     IF (ASSOCIATED(dummy_3D_int_4)) DEALLOCATE(dummy_3D_int_4)
     IF (ASSOCIATED(dummy_1D_real_4)) DEALLOCATE(dummy_1D_real_4)
     IF (ASSOCIATED(dummy_2D_real_4)) DEALLOCATE(dummy_2D_real_4)
     IF (ASSOCIATED(dummy_3D_real_4)) DEALLOCATE(dummy_3D_real_4)
     IF (ASSOCIATED(dummy_4D_real_4)) DEALLOCATE(dummy_4D_real_4)
     IF (ASSOCIATED(dummy_1D_real_8)) DEALLOCATE(dummy_1D_real_8)
     IF (ASSOCIATED(dummy_2D_real_8)) DEALLOCATE(dummy_2D_real_8)
     IF (ASSOCIATED(dummy_3D_real_8)) DEALLOCATE(dummy_3D_real_8)
     
  END DO

  ! Close file
  CALL H5FCLOSE_F(file_id, local_error)
  IF (local_error .NE. 0) GO TO 99

  RETURN
99 WRITE(*,*) errmsg
  STOP

END SUBROUTINE read_he5_file
