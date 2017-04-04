
program test_dmdf
  use dmdf
  implicit none
  integer :: rank, ierr
  integer :: int4
  integer(8) :: int8
  real :: real4
  logical :: log
  real(8)    :: real8
  character(len=128) :: str
  real       :: real4_0d(4)
  real(8)    :: real8_0d(4)
  integer    :: int4_0d (4)
  integer(8) :: int8_0d (4)
  logical    :: log_0d  (4)
  real       :: real4_1d(15,4)
  real(8)    :: real8_1d(15,4)
  integer    :: int4_1d (15,4)
  integer(8) :: int8_1d (15,4)
  logical    :: log_1d  (15,4)
  real       :: real4_2d(15,14,4)
  real(8)    :: real8_2d(15,14,4)
  integer    :: int4_2d (15,14,4)
  integer(8) :: int8_2d (15,14,4)
  logical    :: log_2d  (15,14,4)

  write(*,*) 'Read attributes'

  call dmdf_read_attr(real4,'testing','att_real4') ; if (.not. success) write(*,*) error_string
  call dmdf_read_attr(real8,'testing','att_real8') ; if (.not. success) write(*,*) error_string
  call dmdf_read_attr(int4 ,'testing','att_int4' ) ; if (.not. success) write(*,*) error_string
  call dmdf_read_attr(int8 ,'testing','att_int8' ) ; if (.not. success) write(*,*) error_string
  call dmdf_read_attr(str  ,'testing','att_char' ) ; if (.not. success) write(*,*) error_string
  call dmdf_read_attr(log  ,'testing','att_log'  ) ; if (.not. success) write(*,*) error_string
  write(*,*) real4
  write(*,*) real8
  write(*,*) int4
  write(*,*) int8
  write(*,*) trim(str)
  write(*,*) log

  write(*,*) 'Read scalars'
  call dmdf_read(real4_0d,'testing','real4_0d',1,4,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(real8_0d,'testing','real8_0d',1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int4_0d ,'testing','int4_0d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int8_0d ,'testing','int8_0d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(log_0d  ,'testing','log_0d'  ,1,4,.false.,.true. ) ; if (.not. success) write(*,*) error_string
  write(*,*) real4_0d
  write(*,*) real8_0d
  write(*,*) int4_0d 
  write(*,*) int8_0d 
  write(*,*) log_0d  

  write(*,*) 'Read 1d'
  call dmdf_read(real4_1d,'testing','real4_1d',1,4,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(real8_1d,'testing','real8_1d',1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int4_1d ,'testing','int4_1d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int8_1d ,'testing','int8_1d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(log_1d  ,'testing','log_1d'  ,1,4,.false.,.true. ) ; if (.not. success) write(*,*) error_string
  write(*,*) real4_1d
  write(*,*) real8_1d
  write(*,*) int4_1d 
  write(*,*) int8_1d 
  write(*,*) log_1d  

  write(*,*) 'Read 2d'
  call dmdf_read(real4_2d,'testing','real4_2d',1,4,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(real8_2d,'testing','real8_2d',1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int4_2d ,'testing','int4_2d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(int8_2d ,'testing','int8_2d' ,1,4,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_read(log_2d  ,'testing','log_2d'  ,1,4,.false.,.true. ) ; if (.not. success) write(*,*) error_string
  write(*,*) real4_2d
  write(*,*) real8_2d
  write(*,*) int4_2d 
  write(*,*) int8_2d 
  write(*,*) log_2d  

  write(*,*) 'Finished'

end program test_dmdf



