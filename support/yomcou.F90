MODULE YOMCOU

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------

!*    INFORMATION FOR USE WITH OASIS COUPLER

! NPIAT      PROCESS IDENTIFIER OF THE ATMOSPHERE RUN
! NCULMR     C UNIT IDENTIFIER FOR OASIS MODEL READ PIPE
! NCULMW     C UNIT IDENTIFIER FOR OASIS MODEL WRITE PIPE
! NCULF(20)  C UNIT IDENTIFIERS FOR OASIS MODEL FIELD PIPES

TYPE :: TCOU
INTEGER(KIND=JPIM) :: NCULF(0:20)
INTEGER(KIND=JPIM) :: NPIAT
INTEGER(KIND=JPIM) :: NCULMR
INTEGER(KIND=JPIM) :: NCULMW
 !---------------------------------------------------------------------
CONTAINS
   
END TYPE  TCOU
!======================================================================

!!TYPE(TCOU), POINTER :: YRCOU => NULL()

CONTAINS 
  


END MODULE YOMCOU
