MODULE UTIL_TPTRGPPC_MOD

USE PTRGPPC, ONLY : TPTRGPPC

INTERFACE SAVE
MODULE PROCEDURE SAVE_TPTRGPPC
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TPTRGPPC
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TPTRGPPC
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TPTRGPPC
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TPTRGPPC (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TPTRGPPC), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%NFGPPC
WRITE (KLUN) YD%MGPPC
WRITE (KLUN) YD%MGPPCF_U
WRITE (KLUN) YD%MGPPCF_V
WRITE (KLUN) YD%MGPPCF_T
WRITE (KLUN) YD%MGPPCF_SPD
WRITE (KLUN) YD%MGPPCF_SVD
WRITE (KLUN) YD%MGPPCF_SP
WRITE (KLUN) YD%MGPPCF_CP
WRITE (KLUN) YD%MGPPCF_NHX
WRITE (KLUN) YD%MGPPCF_UP
WRITE (KLUN) YD%MGPPCF_VP
WRITE (KLUN) YD%MGPPCF_TP
WRITE (KLUN) YD%MGPPCF_GFLP
WRITE (KLUN) YD%MGPPCF_BBC
WRITE (KLUN) YD%MGPPC5
WRITE (KLUN) YD%MGPPCF_U5
WRITE (KLUN) YD%MGPPCF_V5
WRITE (KLUN) YD%MGPPCF_T5
WRITE (KLUN) YD%MGPPCF_SPD5
WRITE (KLUN) YD%MGPPCF_SVD5
WRITE (KLUN) YD%MGPPCF_SP5
END SUBROUTINE

SUBROUTINE LOAD_TPTRGPPC (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TPTRGPPC), INTENT (OUT), TARGET :: YD

READ (KLUN) YD%NFGPPC
READ (KLUN) YD%MGPPC
READ (KLUN) YD%MGPPCF_U
READ (KLUN) YD%MGPPCF_V
READ (KLUN) YD%MGPPCF_T
READ (KLUN) YD%MGPPCF_SPD
READ (KLUN) YD%MGPPCF_SVD
READ (KLUN) YD%MGPPCF_SP
READ (KLUN) YD%MGPPCF_CP
READ (KLUN) YD%MGPPCF_NHX
READ (KLUN) YD%MGPPCF_UP
READ (KLUN) YD%MGPPCF_VP
READ (KLUN) YD%MGPPCF_TP
READ (KLUN) YD%MGPPCF_GFLP
READ (KLUN) YD%MGPPCF_BBC
READ (KLUN) YD%MGPPC5
READ (KLUN) YD%MGPPCF_U5
READ (KLUN) YD%MGPPCF_V5
READ (KLUN) YD%MGPPCF_T5
READ (KLUN) YD%MGPPCF_SPD5
READ (KLUN) YD%MGPPCF_SVD5
READ (KLUN) YD%MGPPCF_SP5
END SUBROUTINE


SUBROUTINE COPY_TPTRGPPC (YD, LDCREATED)

IMPLICIT NONE
TYPE (TPTRGPPC), INTENT (IN), TARGET :: YD
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

SUBROUTINE WIPE_TPTRGPPC (YD, LDDELETED)

IMPLICIT NONE
TYPE (TPTRGPPC), INTENT (IN), TARGET :: YD
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
