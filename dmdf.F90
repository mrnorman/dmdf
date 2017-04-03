
#define _NCERR(x) if ((x) /= NF90_NOERR) then; write(error_string,fmt='(A)') nf90_strerror(x); success = .false.; return; endif
#define _ERR(x) write(error_string,fmt='(A)') x; success = .false.; return
#define _RET_IF_ERR if (.not. success) return


module dmdf
  use netcdf
  implicit none
  private

  integer :: ncid

  character(len=1024), public :: error_string
  logical            , public :: success
  
  interface dmdf_write_attr
    module procedure dmdf_write_attr_real4
    module procedure dmdf_write_attr_real8
    module procedure dmdf_write_attr_int4
    module procedure dmdf_write_attr_int8
    module procedure dmdf_write_attr_char
    module procedure dmdf_write_attr_log
  end interface
  
  interface dmdf_read_attr
    module procedure dmdf_read_attr_real4
    module procedure dmdf_read_attr_real8
    module procedure dmdf_read_attr_int4
    module procedure dmdf_read_attr_int8
    module procedure dmdf_read_attr_char
    module procedure dmdf_read_attr_log
  end interface
  
  interface dmdf_write
    module procedure dmdf_write_real4_scalar
    module procedure dmdf_write_real8_scalar
    module procedure dmdf_write_int4_scalar
    module procedure dmdf_write_int8_scalar
    module procedure dmdf_write_log_scalar

    module procedure dmdf_write_real4_1d
    module procedure dmdf_write_real8_1d
    module procedure dmdf_write_int4_1d
    module procedure dmdf_write_int8_1d
    module procedure dmdf_write_log_1d

    module procedure dmdf_write_real4_2d
    module procedure dmdf_write_real8_2d
    module procedure dmdf_write_int4_2d
    module procedure dmdf_write_int8_2d
    module procedure dmdf_write_log_2d
  end interface

  !dmdf_write_attr(val,rank,fprefix,aname)
  public :: dmdf_write_attr

  !dmdf_read_attr(val,rank,fprefix,aname)
  public :: dmdf_read_attr

  !dmdf_write(dat,rank,fprefix,vname       ,first,last)
  !dmdf_write(dat,rank,fprefix,vname,dnames,first,last)
  public :: dmdf_write


contains


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! GLOBAL ATTRIBUTE READ ROUTINES
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine dmdf_read_attr_real4(val,rank,fprefix,aname)
    implicit none
    real(4)         , intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_real4


  subroutine dmdf_read_attr_real8(val,rank,fprefix,aname)
    implicit none
    real(8)         , intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_real8


  subroutine dmdf_read_attr_int4(val,rank,fprefix,aname)
    implicit none
    integer(4)      , intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_int4


  subroutine dmdf_read_attr_int8(val,rank,fprefix,aname)
    implicit none
    integer(8)      , intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_int8


  subroutine dmdf_read_attr_char(val,rank,fprefix,aname)
    implicit none
    character(len=*), intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_char


  subroutine dmdf_read_attr_log(val,rank,fprefix,aname)
    implicit none
    logical         , intent(  out) :: val
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    character(len=*), intent(in   ) :: aname
    integer :: ierr, ival
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_get_att(ncid,NF90_GLOBAL,trim(aname),ival) )
    val = merge(.true.,.false.,ival==1)
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_read_attr_log


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! GLOBAL ATTRIBUTE WRITE ROUTINES
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine dmdf_write_attr_real4(val,rank,fprefix,aname)
    implicit none
    real(4)         , intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_real4


  subroutine dmdf_write_attr_real8(val,rank,fprefix,aname)
    implicit none
    real(8)         , intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_real8


  subroutine dmdf_write_attr_int4(val,rank,fprefix,aname)
    implicit none
    integer(4)      , intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_int4


  subroutine dmdf_write_attr_int8(val,rank,fprefix,aname)
    implicit none
    integer(8)      , intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_int8


  subroutine dmdf_write_attr_char(val,rank,fprefix,aname)
    implicit none
    character(len=*), intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),val) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_char


  subroutine dmdf_write_attr_log(val,rank,fprefix,aname)
    implicit none
    logical         , intent(in) :: val
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: aname
    integer :: ierr
    success = .true.
    call procure_fileid(.true.,rank,fprefix,ncid)             ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    _NCERR( nf90_put_att(ncid,NF90_GLOBAL,trim(aname),merge(1,0,val)) )
    call close_file(.true.,ncid)                              ; _RET_IF_ERR
  endsubroutine dmdf_write_attr_log


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! SCALAR WRITE ROUTINES
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine dmdf_write_real4_scalar(dat,rank,fprefix,vname,first,last)
    implicit none
    integer, parameter :: ndims = 1
    real(4)         , intent(in) :: dat
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)                ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,(/dat/),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_scalar


  subroutine dmdf_write_real8_scalar(dat,rank,fprefix,vname,first,last)
    implicit none
    integer, parameter :: ndims = 1
    real(8)         , intent(in) :: dat
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)                ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_DOUBLE,varid)        ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,(/dat/),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_scalar


  subroutine dmdf_write_int4_scalar(dat,rank,fprefix,vname,first,last)
    implicit none
    integer, parameter :: ndims = 1
    integer(4)      , intent(in) :: dat
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)                ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,(/dat/),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int4_scalar


  subroutine dmdf_write_int8_scalar(dat,rank,fprefix,vname,first,last)
    implicit none
    integer, parameter :: ndims = 1
    integer(8)      , intent(in) :: dat
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)                ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT64,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,(/dat/),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int8_scalar


  subroutine dmdf_write_log_scalar(dat,rank,fprefix,vname,first,last)
    implicit none
    integer, parameter :: ndims = 1
    logical         , intent(in) :: dat
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)                ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,(/merge(1,0,dat)/),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_log_scalar


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! ARRAY WRITE ROUTINES
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine dmdf_write_real4_1d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 2
    real(4)         , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_1d


  subroutine dmdf_write_real8_1d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 2
    real(8)         , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_DOUBLE,varid)        ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_1d


  subroutine dmdf_write_int4_1d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 2
    integer(4)      , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int4_1d


  subroutine dmdf_write_int8_1d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 2
    integer(8)      , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT64,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int8_1d


  subroutine dmdf_write_log_1d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 2
    logical         , intent(in) :: dat(:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,merge(1,0,dat),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_log_1d


  subroutine dmdf_write_real4_2d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 3
    real(4)         , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_FLOAT,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real4_2d


  subroutine dmdf_write_real8_2d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 3
    real(8)         , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_DOUBLE,varid)        ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_real8_2d


  subroutine dmdf_write_int4_2d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 3
    integer(4)      , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int4_2d


  subroutine dmdf_write_int8_2d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 3
    integer(8)      , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT64,varid)         ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,dat,start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_int8_2d


  subroutine dmdf_write_log_2d(dat,rank,fprefix,vname,dnames,first,last)
    implicit none
    integer, parameter :: ndims = 3
    logical         , intent(in) :: dat(:,:)
    integer         , intent(in) :: rank
    character(len=*), intent(in) :: fprefix
    character(len=*), intent(in) :: vname
    character(len=*), intent(in) :: dnames(ndims-1)
    logical         , intent(in) :: first
    logical         , intent(in) :: last
    integer :: dimids(ndims), start(ndims), count(ndims), dsizes(ndims)
    integer :: varid, unlim_len, ierr
    success = .true.
    dsizes(1:ndims-1) = shape(dat)
    dsizes(ndims) = NF90_UNLIMITED
    call procure_fileid(first,rank,fprefix,ncid)                         ; _RET_IF_ERR
    ierr = nf90_redef(ncid)
    call procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)       ; _RET_IF_ERR
    call procure_varid(ndims,ncid,vname,dimids,NF90_INT,varid)           ; _RET_IF_ERR
    call compute_start_count(first,ndims,dsizes,unlim_len,start,count)   ; _RET_IF_ERR
    ierr = nf90_enddef(ncid)
    _NCERR( nf90_put_var(ncid,varid,merge(1,0,dat),start,count) )
    call close_file(last,ncid)                                           ; _RET_IF_ERR
  end subroutine dmdf_write_log_2d


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! REUSED INTERNAL ROUTINES
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  subroutine close_file(last,ncid)
    implicit none
    logical, intent(in) :: last
    integer, intent(in) :: ncid
    if (last) then
      _NCERR( nf90_close(ncid) )
    endif
  end subroutine close_file


  subroutine compute_start_count(first,ndims,dsizes,unlim_len,start,count)
    logical, intent(in   ) :: first
    integer, intent(in   ) :: ndims
    integer, intent(in   ) :: dsizes(ndims)
    integer, intent(in   ) :: unlim_len
    integer, intent(  out) :: start (ndims)
    integer, intent(  out) :: count (ndims)
    integer :: i
    do i = 1 , ndims
      if (dsizes(i) == NF90_UNLIMITED) then
        if (first) then
          start(i) = unlim_len+1
        else
          start(i) = unlim_len
        endif
        count(i) = 1
      else
        start(i) = 1
        count(i) = dsizes(i)
      endif
    enddo
  end subroutine compute_start_count


  subroutine procure_varid(ndims,ncid,vname,dimids,xtype,varid)
    implicit none
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    character(len=*), intent(in   ) :: vname
    integer         , intent(in   ) :: dimids(ndims)
    integer         , intent(in   ) :: xtype
    integer         , intent(  out) :: varid
    integer :: i, ierr, xtype_file
    integer, allocatable :: dimids_file(:)
    allocate(dimids_file(ndims))
    !This section procures the variable ID, whether by creating it or finding it.
    ierr = nf90_inq_varid( ncid , trim(vname) , varid )
    if     (ierr == NF90_ENOTVAR) then
      !If the variable doesn't exist, then define it
      _NCERR( nf90_def_var( ncid , trim(vname) , xtype , dimids , varid ) )
    elseif (ierr == NF90_NOERR  ) then
      !The variable already exists. Make sure it has the same dimension IDs.
      ierr = nf90_inquire_variable( ncid , varid, xtype=xtype_file , dimids=dimids_file )
      _NCERR( ierr )
      do i = 1 , ndims
        if (dimids(i) /= dimids_file(i)) then
          _ERR( 'Specified variable dimensions differ from file variable dimensions' )
        endif
      enddo
      if (xtype_file /= xtype) then
        _ERR( 'Passed variable type differs from file variable type' )
      endif
    else
      !Different error. Handle it
      _NCERR( ierr )
    endif
    deallocate(dimids_file)
  end subroutine procure_varid


  subroutine procure_dimids(ndims,ncid,dnames,dsizes,dimids,unlim_len)
    implicit none
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    character(len=*), intent(in   ) :: dnames(ndims)
    integer         , intent(in   ) :: dsizes(ndims)
    integer         , intent(  out) :: dimids(ndims)
    integer         , intent(  out) :: unlim_len
    integer :: i, ierr
    integer                       :: len
    !The goal of this pass is to procure dimension IDs, whether by creating them or finding them.
    do i = 1 , ndims-1
      !If the dimension is defined already, get the dimension id
      ierr = nf90_inq_dimid( ncid , trim(dnames(i)) , dimids(i) )
      if     (ierr == NF90_EBADDIM) then
        !If the dimension is not defined, then define it
        _NCERR( nf90_def_dim( ncid , trim(dnames(i)) , dsizes(i) , dimids(i) ) )
      elseif (ierr == NF90_NOERR  ) then
        !If the dimension is defined, then make sure the sizes are the same
        _NCERR( nf90_inquire_dimension( ncid , dimids(i) , len=len ) )
        if (len /= dsizes(i)) then
          _ERR( 'Specified dimension size does not match existing dimension size' )
        endif
      else
        !If there's a different error, then handle it
        _NCERR( ierr )
      endif
    enddo

    call procure_dimid_unlim(ndims,ncid,dimids,unlim_len)
  end subroutine procure_dimids


  subroutine procure_dimid_unlim(ndims,ncid,dimids,unlim_len)
    implicit none
    integer         , intent(in   ) :: ndims
    integer         , intent(in   ) :: ncid
    integer         , intent(  out) :: dimids(ndims)
    integer         , intent(  out) :: unlim_len
    integer :: i, ierr, len
    !If the dimension is defined already, get the dimension id
    ierr = nf90_inq_dimid( ncid , 'unlim', dimids(ndims) )
    if     (ierr == NF90_EBADDIM) then
      !If the dimension is not defined, then define it, and set unlim_len to zero
      _NCERR( nf90_def_dim( ncid , 'unlim' , NF90_UNLIMITED , dimids(ndims) ) )
       unlim_len = 0
    elseif (ierr == NF90_NOERR  ) then
      !If the dimension is defined, get the length
      _NCERR( nf90_inquire_dimension( ncid , dimids(ndims) , len=len ) )
      unlim_len = len
    else
      !If there's a different error, then handle it
      _NCERR( ierr )
    endif
  end subroutine procure_dimid_unlim


  subroutine procure_fileid(first,rank,fprefix,ncid)
    implicit none
    logical         , intent(in   ) :: first
    integer         , intent(in   ) :: rank
    character(len=*), intent(in   ) :: fprefix
    integer         , intent(  out) :: ncid
    logical :: file_exists
    integer :: groupnum
    character(len=256) :: fname
    if (first) then
      write(fname,fmt='(A,A,I0.6,A)') trim(fprefix) , '_' , rank , '.nc'

      !If the file exists, open for writing. Otherwise, create
      inquire(file=trim(fname), exist=file_exists) 
      if (file_exists) then
        _NCERR( nf90_open  ( trim(fname) , NF90_WRITE , ncid ) )
      else
        _NCERR( nf90_create( trim(fname) , NF90_HDF5  , ncid ) )
      endif
    endif
  end subroutine procure_fileid


end module dmdf

