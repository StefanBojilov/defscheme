# Microsoft Developer Studio Project File - Name="win32" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=win32 - Win32 Unicode Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "win32.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "win32.mak" CFG="win32 - Win32 Unicode Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "win32 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "win32 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "win32 - Win32 Unicode Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "win32 - Win32 Unicode Release" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "NDEBUG" /D CS=CS_ANSI /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CPPTABLES" /D SXMPATH=\"sxm\" /YX /FD /TP /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386 /out:"Release/sxm.exe"
# Begin Custom Build - Custom build steps:
TargetPath=.\Release\sxm.exe
InputPath=.\Release\sxm.exe
SOURCE="$(InputPath)"

"sxm.cs sxi" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	echo Copying sxm.exe to project dir... 
	copy $(TargetPath) sxm.exe 
	echo Building sxm.cs... 
	sxm -e ..\scheme\core.cs ..\scheme\src2cs.ss <..\scheme\sxm.src  >sxm.cs 
	echo Building sxi... 
	sxm -t -s sxi -e sxm.cs ..\scheme\complib.ss ..\scheme\locales.ss  ..\scheme\pretty.ss ..\scheme\engines.ss ..\scheme\cafe.ss ..\scheme\trace.ss ..\scheme\debug.ss ..\scheme\srfi.ss 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D CS=CS_ANSI /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CPPTABLES" /D SXMPATH=\"sxm\" /YX /FD /TP /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Debug/sxm.exe" /pdbtype:sept
# Begin Custom Build - Custom build steps:
TargetPath=.\Debug\sxm.exe
InputPath=.\Debug\sxm.exe
SOURCE="$(InputPath)"

"sxm.cs sxi" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	echo Copying sxm.exe to project dir... 
	copy $(TargetPath) sxm.exe 
	echo Building sxm.cs... 
	sxm -e ..\scheme\core.cs ..\scheme\src2cs.ss <..\scheme\sxm.src  >sxm.cs 
	echo Building sxi... 
	sxm -t -s sxi -e sxm.cs ..\scheme\complib.ss ..\scheme\locales.ss  ..\scheme\pretty.ss ..\scheme\engines.ss ..\scheme\cafe.ss ..\scheme\trace.ss ..\scheme\debug.ss ..\scheme\srfi.ss 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "win32___Win32_Unicode_Debug"
# PROP BASE Intermediate_Dir "win32___Win32_Unicode_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Unicode_Debug"
# PROP Intermediate_Dir "Unicode_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /I "../" /I "../unicode" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D CS=CS_WIDE /D "CPPTABLES" /YX /FD /TP /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D CS=CS_WIDE /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CPPTABLES" /D SXMPATH=\"sxm\" /YX /FD /TP /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Debug/usxm.exe" /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Unicode_Debug/usxm.exe" /pdbtype:sept
# Begin Custom Build - Custom build steps:
TargetPath=.\Unicode_Debug\usxm.exe
InputPath=.\Unicode_Debug\usxm.exe
SOURCE="$(InputPath)"

"usxm.cs usxi" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	echo Copying usxm.exe to project dir... 
	copy $(TargetPath) usxm.exe 
	echo Building usxm.cs... 
	usxm -e ..\scheme\core.cs ..\scheme\src2cs.ss <..\scheme\sxm.src  >usxm.cs 
	echo Building usxi... 
	usxm -t -s usxi -e usxm.cs ..\scheme\complib.ss ..\scheme\locales.ss  ..\scheme\pretty.ss ..\scheme\engines.ss ..\scheme\cafe.ss ..\scheme\trace.ss ..\scheme\debug.ss ..\scheme\srfi.ss 
	
# End Custom Build

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "win32___Win32_Unicode_Release"
# PROP BASE Intermediate_Dir "win32___Win32_Unicode_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Unicode_Release"
# PROP Intermediate_Dir "Unicode_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D CS=CS_WIDE /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CPPTABLES" /D SXMPATH=\"sxm\" /YX /FD /TP /GZ /c
# ADD CPP /nologo /ML /W3 /GX /O2 /Ob2 /D "NDEBUG" /D CS=CS_WIDE /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "CPPTABLES" /D SXMPATH=\"sxm\" /YX /FD /TP /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"Unicode_Debug/usxm.exe" /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /machine:I386 /out:"Unicode_Release/usxm.exe"
# SUBTRACT LINK32 /debug
# Begin Custom Build - Custom build steps:
TargetPath=.\Unicode_Release\usxm.exe
InputPath=.\Unicode_Release\usxm.exe
SOURCE="$(InputPath)"

"usxm.cs usxi" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	echo Copying usxm.exe to project dir... 
	copy $(TargetPath) usxm.exe 
	echo Building usxm.cs... 
	usxm -e ..\scheme\core.cs ..\scheme\src2cs.ss <..\scheme\sxm.src  >usxm.cs 
	echo Building usxi... 
	usxm -t -s usxi -e usxm.cs ..\scheme\complib.ss ..\scheme\locales.ss  ..\scheme\pretty.ss ..\scheme\engines.ss ..\scheme\cafe.ss ..\scheme\trace.ss ..\scheme\debug.ss ..\scheme\srfi.ss 
	
# End Custom Build

!ENDIF 

# Begin Target

# Name "win32 - Win32 Release"
# Name "win32 - Win32 Debug"
# Name "win32 - Win32 Unicode Debug"
# Name "win32 - Win32 Unicode Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "CS-Specific"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\unicode\Chucs2.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\unicode\Chucs2io.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

!ENDIF 

# End Source File
# End Group
# Begin Group "Optionals/Unix"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\osplan9.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=..\osunix.c
# PROP Exclude_From_Build 1
# End Source File
# End Group
# Begin Source File

SOURCE=..\booleans.c
# End Source File
# Begin Source File

SOURCE=..\boxes.c
# End Source File
# Begin Source File

SOURCE=..\bvectors.c
# End Source File
# Begin Source File

SOURCE=..\chars.c
# End Source File
# Begin Source File

SOURCE=..\chqueue.c
# End Source File
# Begin Source File

SOURCE=..\control.c
# End Source File
# Begin Source File

SOURCE=..\optional\direct.c
# End Source File
# Begin Source File

SOURCE=..\optional\dirent.c
# End Source File
# Begin Source File

SOURCE=..\equal.c
# End Source File
# Begin Source File

SOURCE=..\gcells.c
# End Source File
# Begin Source File

SOURCE=..\hash.c
# End Source File
# Begin Source File

SOURCE=..\htables.c
# End Source File
# Begin Source File

SOURCE=..\input.c
# End Source File
# Begin Source File

SOURCE=..\optional\iocomp.c
# End Source File
# Begin Source File

SOURCE=..\iofilstd.c

!IF  "$(CFG)" == "win32 - Win32 Release"

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\unicode\iofilucs.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\optional\iopipe.c

!IF  "$(CFG)" == "win32 - Win32 Release"

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\unicode\iopipucs.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\optional\ioprocess.c
# End Source File
# Begin Source File

SOURCE=..\iostr.c
# End Source File
# Begin Source File

SOURCE=..\iosys.c
# End Source File
# Begin Source File

SOURCE=..\iottystd.c

!IF  "$(CFG)" == "win32 - Win32 Release"

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\unicode\iottyucs.c
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\iottywin.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\iottywnt.c

!IF  "$(CFG)" == "win32 - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Debug"

!ELSEIF  "$(CFG)" == "win32 - Win32 Unicode Release"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\lists.c
# End Source File
# Begin Source File

SOURCE=..\locales.c
# End Source File
# Begin Source File

SOURCE=..\optional\more.c
# End Source File
# Begin Source File

SOURCE=..\numbers.c
# End Source File
# Begin Source File

SOURCE=.\oswin32.c
# End Source File
# Begin Source File

SOURCE=..\output.c
# End Source File
# Begin Source File

SOURCE=..\pairs.c
# End Source File
# Begin Source File

SOURCE=..\ports.c
# End Source File
# Begin Source File

SOURCE=..\records.c
# End Source File
# Begin Source File

SOURCE=..\strings.c
# End Source File
# Begin Source File

SOURCE=..\sxcom.c
# End Source File
# Begin Source File

SOURCE=..\sxdisasm.c
# End Source File
# Begin Source File

SOURCE=..\sxerror.c
# End Source File
# Begin Source File

SOURCE=..\sxhash.c
# End Source File
# Begin Source File

SOURCE=..\sxhtab.c
# End Source File
# Begin Source File

SOURCE=..\sximage.c
# End Source File
# Begin Source File

SOURCE=..\sxinit.c
# End Source File
# Begin Source File

SOURCE=..\sxintern.c
# End Source File
# Begin Source File

SOURCE=..\sxio.c
# End Source File
# Begin Source File

SOURCE=..\sxlocale.c
# End Source File
# Begin Source File

SOURCE=..\sxmain.c
# End Source File
# Begin Source File

SOURCE=..\sxmath.c
# End Source File
# Begin Source File

SOURCE=..\sxmathread.c
# End Source File
# Begin Source File

SOURCE=..\sxmem.c
# End Source File
# Begin Source File

SOURCE=..\sxmisc.c
# End Source File
# Begin Source File

SOURCE=..\sxread.c
# End Source File
# Begin Source File

SOURCE=..\sxvm.c
# End Source File
# Begin Source File

SOURCE=..\sxwrite.c
# End Source File
# Begin Source File

SOURCE=..\symbols.c
# End Source File
# Begin Source File

SOURCE=..\system.c
# End Source File
# Begin Source File

SOURCE=..\types.c
# End Source File
# Begin Source File

SOURCE=..\vectors.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\chqueue.h
# End Source File
# Begin Source File

SOURCE=..\define.h
# End Source File
# Begin Source File

SOURCE=..\extern.h
# End Source File
# Begin Source File

SOURCE=..\handles.h
# End Source File
# Begin Source File

SOURCE=..\io.h
# End Source File
# Begin Source File

SOURCE=..\mem.h
# End Source File
# Begin Source File

SOURCE=..\os.h
# End Source File
# Begin Source File

SOURCE=..\sxdisasm.h
# End Source File
# Begin Source File

SOURCE=..\sxhash.h
# End Source File
# Begin Source File

SOURCE=..\sxhtab.h
# End Source File
# Begin Source File

SOURCE=..\sximage.h
# End Source File
# Begin Source File

SOURCE=..\sxintern.h
# End Source File
# Begin Source File

SOURCE=..\sxio.h
# End Source File
# Begin Source File

SOURCE=..\sxlocale.h
# End Source File
# Begin Source File

SOURCE=..\sxm.h
# End Source File
# Begin Source File

SOURCE=..\sxmath.h
# End Source File
# Begin Source File

SOURCE=..\sxmathread.h
# End Source File
# Begin Source File

SOURCE=..\sxread.h
# End Source File
# Begin Source File

SOURCE=..\sxwrite.h
# End Source File
# Begin Source File

SOURCE=..\unichar.h
# End Source File
# Begin Source File

SOURCE=..\vmop.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\Copying
# End Source File
# Begin Source File

SOURCE=..\History
# End Source File
# Begin Source File

SOURCE=..\Install
# End Source File
# Begin Source File

SOURCE=..\Makefile
# End Source File
# Begin Source File

SOURCE=..\mkfile
# End Source File
# Begin Source File

SOURCE=..\Readme
# End Source File
# Begin Source File

SOURCE=..\sxm.man
# End Source File
# Begin Source File

SOURCE=..\sxm.spec
# End Source File
# Begin Source File

SOURCE=..\Todo
# End Source File
# End Group
# Begin Group "Scheme Files"

# PROP Default_Filter "cs;ss"
# Begin Source File

SOURCE=..\scheme\cafe.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\complib.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\core.cs
# End Source File
# Begin Source File

SOURCE=..\scheme\debug.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\engines.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\locales.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\pretty.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\src2cs.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\srfi.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\sxc.ss
# End Source File
# Begin Source File

SOURCE=..\scheme\sxm.src
# End Source File
# Begin Source File

SOURCE=..\scheme\trace.ss
# End Source File
# End Group
# Begin Group "Tests"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\tests\exit.ss
# End Source File
# Begin Source File

SOURCE=..\tests\r5rstest.ss
# End Source File
# End Group
# End Target
# End Project
