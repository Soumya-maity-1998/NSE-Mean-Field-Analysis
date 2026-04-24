# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=NSE_Mean Field Effect Study - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to NSE_Mean Field Effect Study\
 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "NSE_Mean Field Effect Study - Win32 Release" && "$(CFG)" !=\
 "NSE_Mean Field Effect Study - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "NSE_Mean Field Effect Study.mak"\
 CFG="NSE_Mean Field Effect Study - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "NSE_Mean Field Effect Study - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "NSE_Mean Field Effect Study - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "NSE_Mean Field Effect Study - Win32 Debug"
RSC=rc.exe
F90=fl32.exe

!IF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\NSE_Mean Field Effect Study.exe"

CLEAN : 
	-@erase ".\Release\NSE_Mean Field Effect Study.exe"
	-@erase ".\Release\Mean_Field.obj"
	-@erase ".\Release\SLy5_Model_parameters.mod"
	-@erase ".\Release\Wigner_Seitz.obj"
	-@erase ".\Release\Binding_Energy_MetaModelling.obj"
	-@erase ".\Release\FERMI_subroutine.obj"
	-@erase ".\Release\SLy5_Parameters.obj"
	-@erase ".\Release\Drip_Line_meta.obj"
	-@erase ".\Release\Eta_inverse.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x4009 /d "NDEBUG"
# ADD RSC /l 0x4009 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/NSE_Mean Field Effect Study.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/NSE_Mean Field Effect Study.pdb" /machine:I386\
 /out:"$(OUTDIR)/NSE_Mean Field Effect Study.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Mean_Field.obj" \
	"$(INTDIR)/Wigner_Seitz.obj" \
	"$(INTDIR)/Binding_Energy_MetaModelling.obj" \
	"$(INTDIR)/FERMI_subroutine.obj" \
	"$(INTDIR)/SLy5_Parameters.obj" \
	"$(INTDIR)/Drip_Line_meta.obj" \
	"$(INTDIR)/Eta_inverse.obj"

"$(OUTDIR)\NSE_Mean Field Effect Study.exe" : "$(OUTDIR)" $(DEF_FILE)\
 $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\NSE_Mean Field Effect Study.exe"

CLEAN : 
	-@erase ".\Debug\NSE_Mean Field Effect Study.exe"
	-@erase ".\Debug\Mean_Field.obj"
	-@erase ".\Debug/SLy5_Model_parameters.mod"
	-@erase ".\Debug\Eta_inverse.obj"
	-@erase ".\Debug\Binding_Energy_MetaModelling.obj"
	-@erase ".\Debug\SLy5_Parameters.obj"
	-@erase ".\Debug\Wigner_Seitz.obj"
	-@erase ".\Debug\FERMI_subroutine.obj"
	-@erase ".\Debug\Drip_Line_meta.obj"
	-@erase ".\Debug\NSE_Mean Field Effect Study.ilk"
	-@erase ".\Debug\NSE_Mean Field Effect Study.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Zi /I "Debug/" /c /nologo
F90_PROJ=/Zi /I "Debug/" /c /nologo /Fo"Debug/"\
 /Fd"Debug/NSE_Mean Field Effect Study.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x4009 /d "_DEBUG"
# ADD RSC /l 0x4009 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/NSE_Mean Field Effect Study.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/NSE_Mean Field Effect Study.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/NSE_Mean Field Effect Study.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Mean_Field.obj" \
	"$(INTDIR)/Eta_inverse.obj" \
	"$(INTDIR)/Binding_Energy_MetaModelling.obj" \
	"$(INTDIR)/SLy5_Parameters.obj" \
	"$(INTDIR)/Wigner_Seitz.obj" \
	"$(INTDIR)/FERMI_subroutine.obj" \
	"$(INTDIR)/Drip_Line_meta.obj"

"$(OUTDIR)\NSE_Mean Field Effect Study.exe" : "$(OUTDIR)" $(DEF_FILE)\
 $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "NSE_Mean Field Effect Study - Win32 Release"
# Name "NSE_Mean Field Effect Study - Win32 Debug"

!IF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Release"

!ELSEIF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Binding_Energy_MetaModelling.f

!IF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Release"

NODEP_F90_BINDI=\
	".\Release\SLy5_Model_parameters.mod"\
	

"$(INTDIR)\Binding_Energy_MetaModelling.obj" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SLy5_Model_parameters.mod"


!ELSEIF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Debug"

NODEP_F90_BINDI=\
	".\Debug/SLy5_Model_parameters.mod"\
	

"$(INTDIR)\Binding_Energy_MetaModelling.obj" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\sly5_model_parameters.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Drip_Line_meta.f

"$(INTDIR)\Drip_Line_meta.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Eta_inverse.f

"$(INTDIR)\Eta_inverse.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\FERMI_subroutine.f

"$(INTDIR)\FERMI_subroutine.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mean_Field.f

!IF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Release"

NODEP_F90_MEAN_=\
	".\Release\SLy5_Model_parameters.mod"\
	

"$(INTDIR)\Mean_Field.obj" : $(SOURCE) "$(INTDIR)"\
 "$(INTDIR)\SLy5_Model_parameters.mod"


!ELSEIF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Debug"

DEP_F90_MEAN_=\
	".\Debug/SLy5_Model_parameters.mod"\
	

"$(INTDIR)\Mean_Field.obj" : $(SOURCE) $(DEP_F90_MEAN_) "$(INTDIR)"\
 "$(INTDIR)\sly5_model_parameters.mod"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\SLy5_Parameters.f

!IF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Release"

F90_MODOUT=\
	"SLy5_Model_parameters"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\SLy5_Parameters.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\SLy5_Model_parameters.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "NSE_Mean Field Effect Study - Win32 Debug"

F90_MODOUT=\
	"SLy5_Model_parameters"


BuildCmds= \
	$(F90) $(F90_PROJ) $(SOURCE) \
	

"$(INTDIR)\SLy5_Parameters.obj" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\sly5_model_parameters.mod" : $(SOURCE) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\Wigner_Seitz.f

"$(INTDIR)\Wigner_Seitz.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
