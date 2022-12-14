MODULE UTIL_CPG_RCP_TYPE_MOD

USE CPG_TYPE_MOD, ONLY : CPG_RCP_TYPE

INTERFACE SAVE
MODULE PROCEDURE SAVE_CPG_RCP_TYPE
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_CPG_RCP_TYPE
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_CPG_RCP_TYPE
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_CPG_RCP_TYPE
END INTERFACE



CONTAINS

SUBROUTINE SAVE_CPG_RCP_TYPE (KLUN, YD)
USE UTIL_FIELD_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (CPG_RCP_TYPE), INTENT (IN), TARGET :: YD
LOGICAL :: LF_CP, LF_KAP, LF_R
WRITE (KLUN) YD%LOWNED
WRITE (KLUN) YD%IP_CP
LF_CP = ASSOCIATED (YD%F_CP)
WRITE (KLUN) LF_CP
IF (LF_CP) THEN
  CALL SAVE (KLUN, YD%F_CP)
ENDIF
WRITE (KLUN) YD%IP_R
LF_R = ASSOCIATED (YD%F_R)
WRITE (KLUN) LF_R
IF (LF_R) THEN
  CALL SAVE (KLUN, YD%F_R)
ENDIF
WRITE (KLUN) YD%IP_KAP
LF_KAP = ASSOCIATED (YD%F_KAP)
WRITE (KLUN) LF_KAP
IF (LF_KAP) THEN
  CALL SAVE (KLUN, YD%F_KAP)
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_CPG_RCP_TYPE (KLUN, YD)
USE UTIL_FIELD_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (CPG_RCP_TYPE), INTENT (OUT), TARGET :: YD
LOGICAL :: LF_CP, LF_KAP, LF_R
READ (KLUN) YD%LOWNED
NULLIFY (YD%ZVIEW)
NULLIFY (YD%ZDATA)
NULLIFY (YD%F_DATA)
READ (KLUN) YD%IP_CP
NULLIFY (YD%CP)
READ (KLUN) LF_CP
IF (LF_CP) THEN
  CALL LOAD (KLUN, YD%F_CP)
ELSE
  NULLIFY (YD%F_CP)
ENDIF
READ (KLUN) YD%IP_R
NULLIFY (YD%R)
READ (KLUN) LF_R
IF (LF_R) THEN
  CALL LOAD (KLUN, YD%F_R)
ELSE
  NULLIFY (YD%F_R)
ENDIF
READ (KLUN) YD%IP_KAP
NULLIFY (YD%KAP)
READ (KLUN) LF_KAP
IF (LF_KAP) THEN
  CALL LOAD (KLUN, YD%F_KAP)
ELSE
  NULLIFY (YD%F_KAP)
ENDIF
END SUBROUTINE


SUBROUTINE COPY_CPG_RCP_TYPE (YD, LDCREATED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (CPG_RCP_TYPE), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
LOGICAL :: LF_CP, LF_KAP, LF_R

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF


LF_CP = ASSOCIATED (YD%F_CP)
IF (LF_CP) THEN
  !$acc enter data create (YD%F_CP)
  !$acc update device (YD%F_CP)
  CALL COPY (YD%F_CP, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_CP)
ENDIF


LF_R = ASSOCIATED (YD%F_R)
IF (LF_R) THEN
  !$acc enter data create (YD%F_R)
  !$acc update device (YD%F_R)
  CALL COPY (YD%F_R, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_R)
ENDIF


LF_KAP = ASSOCIATED (YD%F_KAP)
IF (LF_KAP) THEN
  !$acc enter data create (YD%F_KAP)
  !$acc update device (YD%F_KAP)
  CALL COPY (YD%F_KAP, LDCREATED=.TRUE.)
  !$acc enter data attach (YD%F_KAP)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_CPG_RCP_TYPE (YD, LDDELETED)
USE UTIL_FIELD_MOD
IMPLICIT NONE
TYPE (CPG_RCP_TYPE), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
LOGICAL :: LF_CP, LF_KAP, LF_R



LF_CP = ASSOCIATED (YD%F_CP)
IF (LF_CP) THEN
  !$acc exit data detach (YD%F_CP)
  CALL WIPE (YD%F_CP, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_CP)
ENDIF


LF_R = ASSOCIATED (YD%F_R)
IF (LF_R) THEN
  !$acc exit data detach (YD%F_R)
  CALL WIPE (YD%F_R, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_R)
ENDIF


LF_KAP = ASSOCIATED (YD%F_KAP)
IF (LF_KAP) THEN
  !$acc exit data detach (YD%F_KAP)
  CALL WIPE (YD%F_KAP, LDDELETED=.TRUE.)
  !$acc exit data delete (YD%F_KAP)
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
