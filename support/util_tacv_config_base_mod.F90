MODULE UTIL_TACV_CONFIG_BASE_MOD

USE TYPE_ACV, ONLY : TACV_CONFIG_BASE

INTERFACE SAVE
MODULE PROCEDURE SAVE_TACV_CONFIG_BASE
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TACV_CONFIG_BASE
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TACV_CONFIG_BASE
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TACV_CONFIG_BASE
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TACV_CONFIG_BASE (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TACV_CONFIG_BASE), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%L_IN_1D
WRITE (KLUN) YD%L_IN_2D_SP
WRITE (KLUN) YD%L_IN_2D_GP
WRITE (KLUN) YD%L_IN_3D_SP
WRITE (KLUN) YD%L_IN_3D_GP
WRITE (KLUN) YD%CSETDESC
END SUBROUTINE

SUBROUTINE LOAD_TACV_CONFIG_BASE (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TACV_CONFIG_BASE), INTENT (OUT), TARGET :: YD

READ (KLUN) YD%L_IN_1D
READ (KLUN) YD%L_IN_2D_SP
READ (KLUN) YD%L_IN_2D_GP
READ (KLUN) YD%L_IN_3D_SP
READ (KLUN) YD%L_IN_3D_GP
READ (KLUN) YD%CSETDESC
END SUBROUTINE


SUBROUTINE COPY_TACV_CONFIG_BASE (YD, LDCREATED)

IMPLICIT NONE
TYPE (TACV_CONFIG_BASE), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF






END SUBROUTINE

SUBROUTINE WIPE_TACV_CONFIG_BASE (YD, LDDELETED)

IMPLICIT NONE
TYPE (TACV_CONFIG_BASE), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED







LLDELETED = .FALSE.
IF (PRESENT (LDDELETED)) THEN
  LLDELETED = LDDELETED
ENDIF
IF (.NOT. LLDELETED) THEN
  !$acc exit data delete (YD)
ENDIF
END SUBROUTINE



END MODULE
