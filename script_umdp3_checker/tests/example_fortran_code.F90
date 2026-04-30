! fortls: ignore file
! fortitude: ignore file
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! An example routine depicting how one should construct new code
! to meet the UMDP3 coding standards.
!
MODULE example_mod
IMPLICIT NONE
! Description:
!   A noddy routine that illustrates the way to apply the UMDP3
!   coding standards to new code to help code developers
!   pass code reviews.
!
! Method:
!   In this routine we apply many of the UMDP3 features
!   to construct a simple routine. The references on the RHS take the reader
!   to the appropriate section of the UMDP3 guide with further details.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Control
!
! Code description:
!   Language: Fortran 2003.
!   This code is written to UMDP3 standards.
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName="EXAMPLE_MOD"
CONTAINS
! Subroutine Interface:
SUBROUTINE example (xlen,ylen,l_unscale,input1,input2,   &
                    output, l_loud_opt)
! Description:
! Nothing further to add to module description.
USE atmos_constants_mod, ONLY: r
USE ereport_mod, ONLY: ereport
USE parkind1, ONLY: jpim, jprb
USE umprintMgr, ONLY: umprint,ummessage,PrNorm
USE errormessagelength_mod, ONLY: errormessagelength
USE yomhook, ONLY: lhook, dr_hook
IMPLICIT NONE
! Subroutine arguments
INTEGER, INTENT(IN) :: xlen !Length of first dimension of the arrays.
INTEGER, INTENT(IN) :: ylen !Length of second dimension of the arrays.
LOGICAL, INTENT(IN) :: l_unscale ! switch scaling off.
REAL, INTENT(IN) :: input1(xlen, ylen) !First input array
REAL, INTENT(IN OUT) :: input2(xlen, ylen) !Second input array
REAL, INTENT(OUT) :: output(xlen, ylen) !Contains the result
LOGICAL, INTENT(IN), OPTIONAL :: l_loud_opt !optional debug flag
! Local variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0 ! DrHook tracing entry
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1 ! DrHook tracing exit
INTEGER :: i ! Loop counter
INTEGER :: j ! Loop counter
INTEGER :: icode ! error code for EReport
LOGICAL :: l_loud ! debug flag (default false unless l_loud_opt is used)
REAL, ALLOCATABLE :: field(:,:) ! Scaling array to fill.
REAL, ALLOCATABLE :: field2(:,:) ! Scaling array to fill.
REAL(KIND=jprb) :: zhook_handle ! DrHook tracing
CHARACTER(LEN=*), PARAMETER :: RoutineName="EXAMPLE"
CHARACTER(LEN=errormessagelength) :: Cmessage ! used for EReport
CHARACTER(LEN=256) :: my_char ! string for output
! End of header
IF (lhook) CALL dr_hook(ModuleName//":"//RoutineName,zhook_in,zhook_handle)
! Set debug flag if argument is present
l_loud = .FALSE.
IF (PRESENT(l_loud_opt)) THEN
    l_loud = l_loud_opt
END IF
my_char                                                                        &
    = "This is a very very very very very very very "                          &
    // "long character assignment" ! A pointless long character example.
icode=0
! verbosity choice, output some numbers to aid with debugging
! protected by printstatus>=PrNorm and pe=0
WRITE(ummessage,"(A,I0)")"xlen=",xlen
CALL umprint(ummessage,level=PrNorm,pe=0,src="example_mod")
WRITE(ummessage,"(A,I0)")"ylen=",ylen
CALL umprint(ummessage,level=PrNorm,pe=0,src="example_mod")
IF (l_loud) CALL umprint(my_char,level=PrNormal,src="example_mod")
! Allocate and initialise scaling array
! Noddy code warns user when scalling is not employed.
IF ( l_unscale ) THEN
    icode = -100 ! set up WARNING message
    ALLOCATE(field( 1,1 ) )
    ALLOCATE(field2( 1,1 ) )
    cmessage="Scaling is switched off in run!"
    CALL ereport(RoutineName,icode,cmessage)
ELSE
    ALLOCATE(field( xlen, ylen ) )
    ALLOCATE(field2( xlen, ylen ) )
    DO j=1,ylen
        DO i=1,xlen
            field(i, j) = (1.0*i) + (2.0*j)
            input2(i, j) = input2(i, j) * field(i, j)
            field2(i, j) = (1.0*i) - (2.0*j)                                   &
                + (3.0*i*j) + (4.0*i**2) + field(i, j)*2
        END DO
    END DO
END IF
! The main calculation of the routine, using OpenMP.
!$OMP PARALLEL DEFAULT(NONE) &
!$OMP SHARED(xlen,ylen,input1,input2,field,output) &
!$OMP PRIVATE(i, j )
!$OMP DO SCHEDULE(STATIC)
DO j = 1, ylen
    i_loop: DO i = 1, xlen
    ! Calculate the Output value:
        output(i, j) = (input1(i, j) * input2(i, j))
    END DO i_loop
END DO ! j loop
!$OMP END DO
!$OMP END PARALLEL
DEALLOCATE (field)
IF (lhook) CALL dr_hook(ModuleName//":"//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE example
END MODULE example_mod