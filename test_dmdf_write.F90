
program test_dmdf
  use mpi
  use dmdf
  implicit none
  integer :: rank, ierr
  integer :: int4 = 21
  integer(8) :: int8 = 57
  real :: real4 = 14.
  logical :: log = .true.
  real(8)    :: real8 = 15.
  real       :: real4_1d(15)
  real       :: real4_2d(15,14)
  real       :: real4_3d(15,14,3)
  real       :: real4_4d(15,14,3,2)
  real(8)    :: real8_1d(15)
  real(8)    :: real8_2d(15,14)
  real(8)    :: real8_3d(15,14,3)
  real(8)    :: real8_4d(15,14,3,2)
  integer    :: int4_1d (15)
  integer    :: int4_2d (15,14)
  integer    :: int4_3d (15,14,3)
  integer    :: int4_4d (15,14,3,2)
  integer(8) :: int8_1d (15)
  integer(8) :: int8_2d (15,14)
  integer(8) :: int8_3d (15,14,3)
  integer(8) :: int8_4d (15,14,3,2)
  logical    :: log_1d  (15)
  logical    :: log_2d  (15,14)
  logical    :: log_3d  (15,14,3)
  logical    :: log_4d  (15,14,3,2)
  character(len=128) :: str

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  write(*,*) 'Write attributes'

  call dmdf_write_attr(real4                   ,rank,'testing','att_real4') ; if (.not. success) write(*,*) error_string
  call dmdf_write_attr(real8                   ,rank,'testing','att_real8') ; if (.not. success) write(*,*) error_string
  call dmdf_write_attr(int4                    ,rank,'testing','att_int4' ) ; if (.not. success) write(*,*) error_string
  call dmdf_write_attr(int8                    ,rank,'testing','att_int8' ) ; if (.not. success) write(*,*) error_string
  call dmdf_write_attr('my character attribute',rank,'testing','att_char' ) ; if (.not. success) write(*,*) error_string
  call dmdf_write_attr(log                     ,rank,'testing','att_log'  ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'Beginning first write'

  real4_1d = 0.
  real8_1d = 1.
  int4_1d  = 2
  int8_1d  = 3
  log_1d   = .true.
  real4_2d = 4.
  real8_2d = 5.
  int4_2d  = 6
  int8_2d  = 7
  log_2d   = .false.
  real4_3d = 8.
  real8_3d = 9.
  int4_3d  = 10
  int8_3d  = 11
  log_3d   = .true.
  real4_4d = 12.
  real8_4d = 13.
  int4_4d  = 14
  int8_4d  = 15
  log_4d   = .false.
  call dmdf_write(real4   ,rank,'testing','real4_0d'                        ,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8   ,rank,'testing','real8_0d'                        ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4    ,rank,'testing','int4_0d'                         ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8    ,rank,'testing','int8_0d'                         ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log     ,rank,'testing','log_0d'                          ,.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_1d ,rank,'testing','int4_1d' ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_1d ,rank,'testing','int8_1d' ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_1d  ,rank,'testing','log_1d'  ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_2d ,rank,'testing','int4_2d' ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_2d ,rank,'testing','int8_2d' ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_2d  ,rank,'testing','log_2d'  ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_3d,rank,'testing','real4_3d',(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_3d,rank,'testing','real8_3d',(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_3d ,rank,'testing','int4_3d' ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_3d ,rank,'testing','int8_3d' ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_3d  ,rank,'testing','log_3d'  ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_4d,rank,'testing','real4_4d',(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_4d,rank,'testing','real8_4d',(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_4d ,rank,'testing','int4_4d' ,(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_4d ,rank,'testing','int8_4d' ,(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_4d  ,rank,'testing','log_4d'  ,(/'x1','x2','x3','x4'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'Beginning second write'

  real4_1d = 100.
  real8_1d = 101.
  int4_1d  = 102
  int8_1d  = 103
  log_1d   = .false.
  real4_2d = 104.
  real8_2d = 105.
  int4_2d  = 106
  int8_2d  = 107
  log_2d   = .true.
  real4_3d = 108.
  real8_3d = 109.
  int4_3d  = 110
  int8_3d  = 111
  log_3d   = .true.
  real4_4d = 112.
  real8_4d = 113.
  int4_4d  = 114
  int8_4d  = 115
  log_4d   = .false.
  call dmdf_write(real4   ,rank,'testing','real4_0d'                        ,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8   ,rank,'testing','real8_0d'                        ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4    ,rank,'testing','int4_0d'                         ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8    ,rank,'testing','int8_0d'                         ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log     ,rank,'testing','log_0d'                          ,.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_1d ,rank,'testing','int4_1d' ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_1d ,rank,'testing','int8_1d' ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_1d  ,rank,'testing','log_1d'  ,(/'x1'/)               ,.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_2d ,rank,'testing','int4_2d' ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_2d ,rank,'testing','int8_2d' ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_2d  ,rank,'testing','log_2d'  ,(/'x1','x2'          /),.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_3d,rank,'testing','real4_3d',(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_3d,rank,'testing','real8_3d',(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_3d ,rank,'testing','int4_3d' ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_3d ,rank,'testing','int8_3d' ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_3d  ,rank,'testing','log_3d'  ,(/'x1','x2','x3'     /),.false.,.false.) ; if (.not. success) write(*,*) error_string

  call dmdf_write(real4_4d,rank,'testing','real4_4d',(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_4d,rank,'testing','real8_4d',(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_4d ,rank,'testing','int4_4d' ,(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_4d ,rank,'testing','int8_4d' ,(/'x1','x2','x3','x4'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(log_4d  ,rank,'testing','log_4d'  ,(/'x1','x2','x3','x4'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'Finished'

  call mpi_finalize(ierr)

end program test_dmdf



