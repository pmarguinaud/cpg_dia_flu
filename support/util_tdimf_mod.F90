MODULE UTIL_TDIMF_MOD

USE YOMDIMF, ONLY : TDIMF

INTERFACE SAVE
MODULE PROCEDURE SAVE_TDIMF
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TDIMF
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TDIMF
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TDIMF
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TDIMF (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TDIMF), INTENT (IN), TARGET :: YD
LOGICAL :: LNGRBSP2, LNGRBSP3
WRITE (KLUN) YD%NFTHER
WRITE (KLUN) YD%NF3D
WRITE (KLUN) YD%NFD2D
WRITE (KLUN) YD%NFC2D
WRITE (KLUN) YD%NS3D
WRITE (KLUN) YD%NS2D
WRITE (KLUN) YD%NS1D
LNGRBSP3 = ALLOCATED (YD%NGRBSP3)
WRITE (KLUN) LNGRBSP3
IF (LNGRBSP3) THEN
  WRITE (KLUN) LBOUND (YD%NGRBSP3)
  WRITE (KLUN) UBOUND (YD%NGRBSP3)
  WRITE (KLUN) YD%NGRBSP3
ENDIF
LNGRBSP2 = ALLOCATED (YD%NGRBSP2)
WRITE (KLUN) LNGRBSP2
IF (LNGRBSP2) THEN
  WRITE (KLUN) LBOUND (YD%NGRBSP2)
  WRITE (KLUN) UBOUND (YD%NGRBSP2)
  WRITE (KLUN) YD%NGRBSP2
ENDIF
WRITE (KLUN) YD%LVOR
WRITE (KLUN) YD%LADER
WRITE (KLUN) YD%LUVDER
WRITE (KLUN) YD%LSPT
END SUBROUTINE

SUBROUTINE LOAD_TDIMF (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TDIMF), INTENT (OUT), TARGET :: YD
INTEGER :: IL1(1), IU1(1)
LOGICAL :: LNGRBSP2, LNGRBSP3
READ (KLUN) YD%NFTHER
READ (KLUN) YD%NF3D
READ (KLUN) YD%NFD2D
READ (KLUN) YD%NFC2D
READ (KLUN) YD%NS3D
READ (KLUN) YD%NS2D
READ (KLUN) YD%NS1D
READ (KLUN) LNGRBSP3
IF (LNGRBSP3) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NGRBSP3 (IL1(1):IU1(1)))
  READ (KLUN) YD%NGRBSP3
ENDIF
READ (KLUN) LNGRBSP2
IF (LNGRBSP2) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%NGRBSP2 (IL1(1):IU1(1)))
  READ (KLUN) YD%NGRBSP2
ENDIF
READ (KLUN) YD%LVOR
READ (KLUN) YD%LADER
READ (KLUN) YD%LUVDER
READ (KLUN) YD%LSPT
END SUBROUTINE


SUBROUTINE COPY_TDIMF (YD, LDCREATED)

IMPLICIT NONE
TYPE (TDIMF), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LNGRBSP2, LNGRBSP3

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF







LNGRBSP3 = ALLOCATED (YD%NGRBSP3)
IF (LNGRBSP3) THEN
  !$acc enter data create (YD%NGRBSP3)
  !$acc update device (YD%NGRBSP3)
  !$acc enter data attach (YD%NGRBSP3)
ENDIF

LNGRBSP2 = ALLOCATED (YD%NGRBSP2)
IF (LNGRBSP2) THEN
  !$acc enter data create (YD%NGRBSP2)
  !$acc update device (YD%NGRBSP2)
  !$acc enter data attach (YD%NGRBSP2)
ENDIF





END SUBROUTINE

SUBROUTINE WIPE_TDIMF (YD, LDDELETED)

IMPLICIT NONE
TYPE (TDIMF), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LNGRBSP2, LNGRBSP3








LNGRBSP3 = ALLOCATED (YD%NGRBSP3)
IF (LNGRBSP3) THEN
  !$acc exit data detach (YD%NGRBSP3)
  !$acc exit data delete (YD%NGRBSP3)
ENDIF

LNGRBSP2 = ALLOCATED (YD%NGRBSP2)
IF (LNGRBSP2) THEN
  !$acc exit data detach (YD%NGRBSP2)
  !$acc exit data delete (YD%NGRBSP2)
ENDIF





LLDELETED = .FALSE.
IF (PRESENT (LDDELETED)) THEN
  LLDELETED = LDDELETED
ENDIF
IF (.NOT. LLDELETED) THEN
  !$acc exit data delete (YD)
ENDIF
END SUBROUTINE



END MODULE
