MODULE UTIL_TXFU_KEYS_MOD

USE YOMXFU_TYPE, ONLY : TXFU_KEYS

INTERFACE SAVE
MODULE PROCEDURE SAVE_TXFU_KEYS
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TXFU_KEYS
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TXFU_KEYS
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TXFU_KEYS
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TXFU_KEYS (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TXFU_KEYS), INTENT (IN), TARGET :: YD

WRITE (KLUN) YD%LXFU
WRITE (KLUN) YD%LXTRD
WRITE (KLUN) YD%LXTRC
WRITE (KLUN) YD%LXTRT
WRITE (KLUN) YD%LXPLC
WRITE (KLUN) YD%LXPLCG
WRITE (KLUN) YD%LXPLCH
WRITE (KLUN) YD%LXPLS
WRITE (KLUN) YD%LXPLSG
WRITE (KLUN) YD%LXPLSH
WRITE (KLUN) YD%LXR
WRITE (KLUN) YD%LXNEBTT
WRITE (KLUN) YD%LXNEBPA
WRITE (KLUN) YD%LXCLS
WRITE (KLUN) YD%LXMWINDCLS
WRITE (KLUN) YD%LXNUVCLS
WRITE (KLUN) YD%LXTTCLS
WRITE (KLUN) YD%LXHHCLS
WRITE (KLUN) YD%LXTPWCLS
WRITE (KLUN) YD%LXSOIL
WRITE (KLUN) YD%LTXTRD
WRITE (KLUN) YD%LTXTRC
WRITE (KLUN) YD%LTXTRT
WRITE (KLUN) YD%LTXR
WRITE (KLUN) YD%LTXNEB
WRITE (KLUN) YD%LTXQICE
WRITE (KLUN) YD%LTXQLI
WRITE (KLUN) YD%LXICV
WRITE (KLUN) YD%LXCTOP
WRITE (KLUN) YD%LXCLP
WRITE (KLUN) YD%LXVEIN
WRITE (KLUN) YD%LXTGST
WRITE (KLUN) YD%LXXGST
WRITE (KLUN) YD%LXXGST2
WRITE (KLUN) YD%LXQCLS
WRITE (KLUN) YD%LXTHW
WRITE (KLUN) YD%LXXDIAGH
WRITE (KLUN) YD%LXMRT
WRITE (KLUN) YD%LXVISI
WRITE (KLUN) YD%LXVISI2
WRITE (KLUN) YD%LXPRECIPS1
WRITE (KLUN) YD%LXPRECIPS2
END SUBROUTINE

SUBROUTINE LOAD_TXFU_KEYS (KLUN, YD)

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TXFU_KEYS), INTENT (OUT), TARGET :: YD

READ (KLUN) YD%LXFU
READ (KLUN) YD%LXTRD
READ (KLUN) YD%LXTRC
READ (KLUN) YD%LXTRT
READ (KLUN) YD%LXPLC
READ (KLUN) YD%LXPLCG
READ (KLUN) YD%LXPLCH
READ (KLUN) YD%LXPLS
READ (KLUN) YD%LXPLSG
READ (KLUN) YD%LXPLSH
READ (KLUN) YD%LXR
READ (KLUN) YD%LXNEBTT
READ (KLUN) YD%LXNEBPA
READ (KLUN) YD%LXCLS
READ (KLUN) YD%LXMWINDCLS
READ (KLUN) YD%LXNUVCLS
READ (KLUN) YD%LXTTCLS
READ (KLUN) YD%LXHHCLS
READ (KLUN) YD%LXTPWCLS
READ (KLUN) YD%LXSOIL
READ (KLUN) YD%LTXTRD
READ (KLUN) YD%LTXTRC
READ (KLUN) YD%LTXTRT
READ (KLUN) YD%LTXR
READ (KLUN) YD%LTXNEB
READ (KLUN) YD%LTXQICE
READ (KLUN) YD%LTXQLI
READ (KLUN) YD%LXICV
READ (KLUN) YD%LXCTOP
READ (KLUN) YD%LXCLP
READ (KLUN) YD%LXVEIN
READ (KLUN) YD%LXTGST
READ (KLUN) YD%LXXGST
READ (KLUN) YD%LXXGST2
READ (KLUN) YD%LXQCLS
READ (KLUN) YD%LXTHW
READ (KLUN) YD%LXXDIAGH
READ (KLUN) YD%LXMRT
READ (KLUN) YD%LXVISI
READ (KLUN) YD%LXVISI2
READ (KLUN) YD%LXPRECIPS1
READ (KLUN) YD%LXPRECIPS2
END SUBROUTINE


SUBROUTINE COPY_TXFU_KEYS (YD, LDCREATED)

IMPLICIT NONE
TYPE (TXFU_KEYS), INTENT (IN), TARGET :: YD
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

SUBROUTINE WIPE_TXFU_KEYS (YD, LDDELETED)

IMPLICIT NONE
TYPE (TXFU_KEYS), INTENT (IN), TARGET :: YD
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
