MODULE UTIL_TEAERATM_MOD

USE YOEAERATM, ONLY : TEAERATM

INTERFACE SAVE
MODULE PROCEDURE SAVE_TEAERATM
END INTERFACE

INTERFACE LOAD
MODULE PROCEDURE LOAD_TEAERATM
END INTERFACE

INTERFACE COPY
MODULE PROCEDURE COPY_TEAERATM
END INTERFACE

INTERFACE WIPE
MODULE PROCEDURE WIPE_TEAERATM
END INTERFACE



CONTAINS

SUBROUTINE SAVE_TEAERATM (KLUN, YD)
USE UTIL_TYPE_AERO_DESC_MOD
IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEAERATM), INTENT (IN), TARGET :: YD
INTEGER :: J1
LOGICAL :: LYAERO_DESC
WRITE (KLUN) YD%NAERCONF
WRITE (KLUN) YD%NINIDAY
WRITE (KLUN) YD%NXT3DAER
WRITE (KLUN) YD%NDD1
WRITE (KLUN) YD%NSS1
WRITE (KLUN) YD%NBCOPTP
WRITE (KLUN) YD%NDDOPTP
WRITE (KLUN) YD%NOMOPTP
WRITE (KLUN) YD%NSSOPTP
WRITE (KLUN) YD%NSUOPTP
WRITE (KLUN) YD%NVISWL
WRITE (KLUN) YD%NMAXTAER
WRITE (KLUN) YD%NTAER
WRITE (KLUN) YD%NTYPAER
WRITE (KLUN) YD%NAER_BLNUCL
WRITE (KLUN) YD%NAERSCAV
WRITE (KLUN) YD%RGRATE
WRITE (KLUN) YD%REPSCAER
WRITE (KLUN) YD%LAERCLIMG
WRITE (KLUN) YD%LAERCLIMZ
WRITE (KLUN) YD%LAERCLIST
WRITE (KLUN) YD%LAERDRYDP
WRITE (KLUN) YD%LAERHYGRO
WRITE (KLUN) YD%LAERLISI
WRITE (KLUN) YD%LAERNGAT
WRITE (KLUN) YD%LAERSEDIM
WRITE (KLUN) YD%LAERSURF
WRITE (KLUN) YD%LAERELVS
WRITE (KLUN) YD%LAER6SDIA
WRITE (KLUN) YD%LAERSEDIMSS
WRITE (KLUN) YD%LAERGTOP
WRITE (KLUN) YD%LAERRAD
WRITE (KLUN) YD%LAERCCN
WRITE (KLUN) YD%LAEROPT
WRITE (KLUN) YD%LAERINIT
WRITE (KLUN) YD%LAERVOL
WRITE (KLUN) YD%LAERCSTR
WRITE (KLUN) YD%LAERDIAG1
WRITE (KLUN) YD%LAERDIAG2
WRITE (KLUN) YD%LAERRRTM
WRITE (KLUN) YD%LAERUVP
WRITE (KLUN) YD%LAEREXTR
WRITE (KLUN) YD%LAERGBUD
WRITE (KLUN) YD%LAERPRNT
WRITE (KLUN) YD%LAERNITRATE
WRITE (KLUN) YD%LAERSOA_CHEM
WRITE (KLUN) YD%LAERSOAEMIS_FLUX
WRITE (KLUN) YD%LSEASALT_RH80
WRITE (KLUN) YD%LAERDUSTSOURCE
WRITE (KLUN) YD%LAERDUST_NEWBIN
WRITE (KLUN) YD%RSS_DRY_DIAFAC
WRITE (KLUN) YD%RSS_DRY_DENSFAC
WRITE (KLUN) YD%RSS_DRY_MASSFAC
WRITE (KLUN) YD%RSS_RH80_DIAFAC
WRITE (KLUN) YD%RSS_RH80_DENSFAC
WRITE (KLUN) YD%RSS_RH80_MASSFAC
LYAERO_DESC = ASSOCIATED (YD%YAERO_DESC)
WRITE (KLUN) LYAERO_DESC
IF (LYAERO_DESC) THEN
  WRITE (KLUN) LBOUND (YD%YAERO_DESC)
  WRITE (KLUN) UBOUND (YD%YAERO_DESC)
  DO J1 = LBOUND (YD%YAERO_DESC, 1), UBOUND (YD%YAERO_DESC, 1)
    CALL SAVE (KLUN, YD%YAERO_DESC (J1))
  ENDDO
ENDIF
END SUBROUTINE

SUBROUTINE LOAD_TEAERATM (KLUN, YD)
USE UTIL_TYPE_AERO_DESC_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
TYPE (TEAERATM), INTENT (OUT), TARGET :: YD
INTEGER :: J1
INTEGER :: IL1(1), IU1(1)
LOGICAL :: LYAERO_DESC
READ (KLUN) YD%NAERCONF
READ (KLUN) YD%NINIDAY
READ (KLUN) YD%NXT3DAER
READ (KLUN) YD%NDD1
READ (KLUN) YD%NSS1
READ (KLUN) YD%NBCOPTP
READ (KLUN) YD%NDDOPTP
READ (KLUN) YD%NOMOPTP
READ (KLUN) YD%NSSOPTP
READ (KLUN) YD%NSUOPTP
READ (KLUN) YD%NVISWL
READ (KLUN) YD%NMAXTAER
READ (KLUN) YD%NTAER
READ (KLUN) YD%NTYPAER
READ (KLUN) YD%NAER_BLNUCL
READ (KLUN) YD%NAERSCAV
READ (KLUN) YD%RGRATE
READ (KLUN) YD%REPSCAER
READ (KLUN) YD%LAERCLIMG
READ (KLUN) YD%LAERCLIMZ
READ (KLUN) YD%LAERCLIST
READ (KLUN) YD%LAERDRYDP
READ (KLUN) YD%LAERHYGRO
READ (KLUN) YD%LAERLISI
READ (KLUN) YD%LAERNGAT
READ (KLUN) YD%LAERSEDIM
READ (KLUN) YD%LAERSURF
READ (KLUN) YD%LAERELVS
READ (KLUN) YD%LAER6SDIA
READ (KLUN) YD%LAERSEDIMSS
READ (KLUN) YD%LAERGTOP
READ (KLUN) YD%LAERRAD
READ (KLUN) YD%LAERCCN
READ (KLUN) YD%LAEROPT
READ (KLUN) YD%LAERINIT
READ (KLUN) YD%LAERVOL
READ (KLUN) YD%LAERCSTR
READ (KLUN) YD%LAERDIAG1
READ (KLUN) YD%LAERDIAG2
READ (KLUN) YD%LAERRRTM
READ (KLUN) YD%LAERUVP
READ (KLUN) YD%LAEREXTR
READ (KLUN) YD%LAERGBUD
READ (KLUN) YD%LAERPRNT
READ (KLUN) YD%LAERNITRATE
READ (KLUN) YD%LAERSOA_CHEM
READ (KLUN) YD%LAERSOAEMIS_FLUX
READ (KLUN) YD%LSEASALT_RH80
READ (KLUN) YD%LAERDUSTSOURCE
READ (KLUN) YD%LAERDUST_NEWBIN
READ (KLUN) YD%RSS_DRY_DIAFAC
READ (KLUN) YD%RSS_DRY_DENSFAC
READ (KLUN) YD%RSS_DRY_MASSFAC
READ (KLUN) YD%RSS_RH80_DIAFAC
READ (KLUN) YD%RSS_RH80_DENSFAC
READ (KLUN) YD%RSS_RH80_MASSFAC
READ (KLUN) LYAERO_DESC
IF (LYAERO_DESC) THEN
  READ (KLUN) IL1
  READ (KLUN) IU1
  ALLOCATE (YD%YAERO_DESC (IL1(1):IU1(1)))
  DO J1 = LBOUND (YD%YAERO_DESC, 1), UBOUND (YD%YAERO_DESC, 1)
    CALL LOAD (KLUN, YD%YAERO_DESC (J1))
  ENDDO
ELSE
  NULLIFY (YD%YAERO_DESC)
ENDIF
END SUBROUTINE


SUBROUTINE COPY_TEAERATM (YD, LDCREATED)
USE UTIL_TYPE_AERO_DESC_MOD
IMPLICIT NONE
TYPE (TEAERATM), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDCREATED
LOGICAL :: LLCREATED
INTEGER :: J1
LOGICAL :: LYAERO_DESC

LLCREATED = .FALSE.
IF (PRESENT (LDCREATED)) THEN
  LLCREATED = LDCREATED
ENDIF
IF (.NOT. LLCREATED) THEN
  !$acc enter data create (YD)
  !$acc update device (YD)
ENDIF
























































LYAERO_DESC = ASSOCIATED (YD%YAERO_DESC)
IF (LYAERO_DESC) THEN
  !$acc enter data create (YD%YAERO_DESC)
  !$acc update device (YD%YAERO_DESC)
  DO J1 = LBOUND (YD%YAERO_DESC, 1), UBOUND (YD%YAERO_DESC, 1)
    CALL COPY (YD%YAERO_DESC (J1), LDCREATED=.TRUE.)
  ENDDO
  !$acc enter data attach (YD%YAERO_DESC)
ENDIF

END SUBROUTINE

SUBROUTINE WIPE_TEAERATM (YD, LDDELETED)
USE UTIL_TYPE_AERO_DESC_MOD
IMPLICIT NONE
TYPE (TEAERATM), INTENT (IN), TARGET :: YD
LOGICAL, OPTIONAL, INTENT (IN) :: LDDELETED
LOGICAL :: LLDELETED
INTEGER :: J1
LOGICAL :: LYAERO_DESC

























































LYAERO_DESC = ASSOCIATED (YD%YAERO_DESC)
IF (LYAERO_DESC) THEN
  !$acc exit data detach (YD%YAERO_DESC)
  DO J1 = LBOUND (YD%YAERO_DESC, 1), UBOUND (YD%YAERO_DESC, 1)
    CALL WIPE (YD%YAERO_DESC (J1), LDDELETED=.TRUE.)
  ENDDO
  !$acc exit data delete (YD%YAERO_DESC)
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
