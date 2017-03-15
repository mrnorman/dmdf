
program test_dmdf
  use mpi
  use dmdf
  implicit none
  integer :: rank, ierr
  integer :: int4 = 21
  integer(8) :: int8 = 57
  real :: real4 = 14.
  real(8)    :: real8 = 15.
  real       :: real4_1d(15)
  real       :: real4_2d(15,14)
  real(8)    :: real8_1d(15)
  real(8)    :: real8_2d(15,14)
  integer    :: int4_1d(15)
  integer    :: int4_2d(15,14)
  integer(8) :: int8_1d(15)
  integer(8) :: int8_2d(15,14)

  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

  write(*,*) 'attributes'

  call dmdf_attr (real4                   ,rank,'testing','att_real4') ; if (.not. success) write(*,*) error_string
  call dmdf_attr (real8                   ,rank,'testing','att_real8') ; if (.not. success) write(*,*) error_string
  call dmdf_attr (int4                    ,rank,'testing','att_int4' ) ; if (.not. success) write(*,*) error_string
  call dmdf_attr (int8                    ,rank,'testing','att_int8' ) ; if (.not. success) write(*,*) error_string
  call dmdf_attr ('my character attribute',rank,'testing','att_char' ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'beginning first transfer'

  real4_1d = 0.
  real8_1d = 1.
  int4_1d  = 2
  int8_1d  = 3
  real4_2d = 4.
  real8_2d = 5.
  int4_2d  = 6
  int8_2d  = 7
  call dmdf_write(real4   ,rank,'testing','real4_0d'              ,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8   ,rank,'testing','real8_0d'              ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4    ,rank,'testing','int4_0d'               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8    ,rank,'testing','int8_0d'               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_1d ,rank,'testing','int4_1d' ,(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_1d ,rank,'testing','int8_1d' ,(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_2d ,rank,'testing','int4_2d' ,(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_2d ,rank,'testing','int8_2d' ,(/'x1','x2'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'beginning second transfer'

  real4_1d = 10.
  real8_1d = 11.
  int4_1d  = 12
  int8_1d  = 13
  real4_2d = 14.
  real8_2d = 15.
  int4_2d  = 16
  int8_2d  = 17
  call dmdf_write(real4   ,rank,'testing','real4_0d'              ,.true. ,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8   ,rank,'testing','real8_0d'              ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4    ,rank,'testing','int4_0d'               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8    ,rank,'testing','int8_0d'               ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_1d,rank,'testing','real4_1d',(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_1d,rank,'testing','real8_1d',(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_1d ,rank,'testing','int4_1d' ,(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_1d ,rank,'testing','int8_1d' ,(/'x1'/)     ,.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real4_2d,rank,'testing','real4_2d',(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(real8_2d,rank,'testing','real8_2d',(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int4_2d ,rank,'testing','int4_2d' ,(/'x1','x2'/),.false.,.false.) ; if (.not. success) write(*,*) error_string
  call dmdf_write(int8_2d ,rank,'testing','int8_2d' ,(/'x1','x2'/),.false.,.true. ) ; if (.not. success) write(*,*) error_string

  write(*,*) 'finished'

end program test_dmdf



