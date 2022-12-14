MODULE UTIL_ARRAY_2D_MOD

USE ARRAY_MOD, ONLY : ARRAY_2D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_2D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_2D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_2D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_2D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_2D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_2D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_2D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_2D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_2D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_2D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_3D_MOD

USE ARRAY_MOD, ONLY : ARRAY_3D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_3D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_3D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_3D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_3D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_3D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_3D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_3D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_3D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_3D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_3D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_4D_MOD

USE ARRAY_MOD, ONLY : ARRAY_4D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_4D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_4D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_4D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_4D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_4D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_4D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_4D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_4D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_4D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_4D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_5D_MOD

USE ARRAY_MOD, ONLY : ARRAY_5D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_5D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_5D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_5D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_5D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_5D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_5D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_5D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_5D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_5D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_5D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_INT2D_MOD

USE ARRAY_MOD, ONLY : ARRAY_INT2D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_INT2D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_INT2D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_INT2D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_INT2D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_INT2D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT2D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_INT2D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT2D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_INT2D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_INT2D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_INT3D_MOD

USE ARRAY_MOD, ONLY : ARRAY_INT3D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_INT3D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_INT3D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_INT3D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_INT3D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_INT3D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT3D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_INT3D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT3D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_INT3D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_INT3D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_INT4D_MOD

USE ARRAY_MOD, ONLY : ARRAY_INT4D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_INT4D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_INT4D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_INT4D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_INT4D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_INT4D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT4D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_INT4D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT4D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_INT4D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_INT4D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_INT5D_MOD

USE ARRAY_MOD, ONLY : ARRAY_INT5D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_INT5D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_INT5D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_INT5D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_INT5D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_INT5D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT5D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_INT5D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_INT5D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_INT5D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_INT5D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_INT5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_LOG2D_MOD

USE ARRAY_MOD, ONLY : ARRAY_LOG2D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_LOG2D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_LOG2D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_LOG2D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_LOG2D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_LOG2D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG2D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_LOG2D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG2D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_LOG2D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_LOG2D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG2D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_LOG3D_MOD

USE ARRAY_MOD, ONLY : ARRAY_LOG3D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_LOG3D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_LOG3D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_LOG3D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_LOG3D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_LOG3D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG3D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_LOG3D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG3D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_LOG3D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_LOG3D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG3D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_LOG4D_MOD

USE ARRAY_MOD, ONLY : ARRAY_LOG4D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_LOG4D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_LOG4D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_LOG4D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_LOG4D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_LOG4D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG4D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_LOG4D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG4D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_LOG4D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_LOG4D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG4D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
MODULE UTIL_ARRAY_LOG5D_MOD

USE ARRAY_MOD, ONLY : ARRAY_LOG5D

INTERFACE SAVE
MODULE PROCEDURE SAVE_ARRAY_LOG5D
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_ARRAY_LOG5D
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_ARRAY_LOG5D
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_ARRAY_LOG5D
END INTERFACE



CONTAINS

SUBROUTINE SAVE_ARRAY_LOG5D (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG5D), INTENT (IN), TARGET :: YD
LOGICAL :: LF_P
LF_P = ASSOCIATED (YD%F_P)
WRITE (KLUN) LF_P
IF (LF_P) THEN
  CALL SAVE (KLUN, YD%F_P)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_ARRAY_LOG5D (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (ARRAY_LOG5D), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_P
READ (KLUN) LF_P
IF (LF_P) THEN
  CALL LOAD (KLUN, YD%F_P)
ELSE
  NULLIFY (YD%F_P)
ENDIF
NULLIFY (YD%P)
END SUBROUTINE


SUBROUTINE COPY_ARRAY_LOG5D (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_P

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc enter data create (YD%F_P)
  !$acc update device (YD%F_P)
  CALL COPY (YD%F_P, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_P)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_ARRAY_LOG5D (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (ARRAY_LOG5D), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_P

LF_P = ASSOCIATED (YD%F_P)
IF (LF_P) THEN
  !$acc exit data detach (YD%F_P)
  CALL WIPE (YD%F_P, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_P)
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
