
  unit Deltics.Shell.API;

interface

  uses
    RegStr,
    ShlObj,
    Windows;

  const
    REGSTR_PATH_SPECIAL_FOLDERS   = REGSTR_PATH_EXPLORER + '\Shell Folders';

    CSIDL_DESKTOP                 = $0000;          // <desktop>
    CSIDL_INTERNET                = $0001;          // Internet Explorer (icon on desktop)
    CSIDL_PROGRAMS                = $0002;          // Start Menu\Programs
    CSIDL_CONTROLS                = $0003;          // My Computer\Control Panel
    CSIDL_PRINTERS                = $0004;          // My Computer\Printers
    CSIDL_PERSONAL                = $0005;          // My Documents
    CSIDL_FAVORITES               = $0006;          // <user name>\Favorites
    CSIDL_STARTUP                 = $0007;          // Start Menu\Programs\Startup
    CSIDL_RECENT                  = $0008;          // <user name>\Recent
    CSIDL_SENDTO                  = $0009;          // <user name>\SendTo
    CSIDL_BITBUCKET               = $000a;          // <desktop>\Recycle Bin
    CSIDL_STARTMENU               = $000b;          // <user name>\Start Menu
    CSIDL_MYDOCUMENTS             = CSIDL_PERSONAL; // Personal was just a silly name for My Documents
    CSIDL_MYMUSIC                 = $000d;          // "My Music" folder
    CSIDL_MYVIDEO                 = $000e;          // "My Videos" folder
    CSIDL_DESKTOPDIRECTORY        = $0010;          // <user name>\Desktop
    CSIDL_DRIVES                  = $0011;          // My Computer
    CSIDL_NETWORK                 = $0012;          // Network Neighborhood (My Network Places)
    CSIDL_NETHOOD                 = $0013;          // <user name>\nethood
    CSIDL_FONTS                   = $0014;          // windows\fonts
    CSIDL_TEMPLATES               = $0015;
    CSIDL_COMMON_STARTMENU        = $0016;          // All Users\Start Menu
    CSIDL_COMMON_PROGRAMS         = $0017;          // All Users\Start Menu\Programs
    CSIDL_COMMON_STARTUP          = $0018;          // All Users\Startup
    CSIDL_COMMON_DESKTOPDIRECTORY = $0019;          // All Users\Desktop
    CSIDL_APPDATA                 = $001a;          // <user name>\Application Data
    CSIDL_PRINTHOOD               = $001b;          // <user name>\PrintHood
    CSIDL_LOCAL_APPDATA           = $001c;          // <user name>\Local Settings\Applicaiton Data (non roaming)
    CSIDL_ALTSTARTUP              = $001d;          // non localized startup
    CSIDL_COMMON_ALTSTARTUP       = $001e;          // non localized common startup
    CSIDL_COMMON_FAVORITES        = $001f;
    CSIDL_INTERNET_CACHE          = $0020;
    CSIDL_COOKIES                 = $0021;
    CSIDL_HISTORY                 = $0022;
    CSIDL_COMMON_APPDATA          = $0023;          // All Users\Application Data
    CSIDL_WINDOWS                 = $0024;          // GetWindowsDirectory()
    CSIDL_SYSTEM                  = $0025;          // GetSystemDirectory()
    CSIDL_PROGRAM_FILES           = $0026;          // C:\Program Files
    CSIDL_MYPICTURES              = $0027;          // C:\Program Files\My Pictures
    CSIDL_PROFILE                 = $0028;          // USERPROFILE
    CSIDL_SYSTEMX86               = $0029;          // x86 system directory on RISC
    CSIDL_PROGRAM_FILESX86        = $002a;          // x86 C:\Program Files on RISC
    CSIDL_PROGRAM_FILES_COMMON    = $002b;          // C:\Program Files\Common
    CSIDL_PROGRAM_FILES_COMMONX86 = $002c;          // x86 Program Files\Common on RISC
    CSIDL_COMMON_TEMPLATES        = $002d;          // All Users\Templates
    CSIDL_COMMON_DOCUMENTS        = $002e;          // All Users\Documents
    CSIDL_COMMON_ADMINTOOLS       = $002f;          // All Users\Start Menu\Programs\Administrative Tools
    CSIDL_ADMINTOOLS              = $0030;          // <user name>\Start Menu\Programs\Administrative Tools
    CSIDL_CONNECTIONS             = $0031;          // Network and Dial-up Connections
    CSIDL_COMMON_MUSIC            = $0035;          // All Users\My Music
    CSIDL_COMMON_PICTURES         = $0036;          // All Users\My Pictures
    CSIDL_COMMON_VIDEO            = $0037;          // All Users\My Video
    CSIDL_RESOURCES               = $0038;          // Resource Direcotry
    CSIDL_RESOURCES_LOCALIZED     = $0039;          // Localized Resource Direcotry
    CSIDL_COMMON_OEM_LINKS        = $003a;          // Links to All Users OEM specific apps
    CSIDL_CDBURN_AREA             = $003b;          // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    // NOT USED                     $003c
    CSIDL_COMPUTERSNEARME         = $003d;          // Computers Near Me (computered from Workgroup membership)

    CSIDL_FLAG_CREATE         = $8000;
    CSIDL_FLAG_DONT_VERIFY    = $4000;
    CSIDL_FLAG_DONT_UNEXPAND  = $2000;
    CSIDL_FLAG_NO_ALIAS       = $1000;
    CSIDL_FLAG_PER_USER_INIT  = $0800;
    CSIDL_FLAG_MASK           = $FF00;


  const
    shell32 = 'shell32.dll';

  function SHGetFolderPath(hWnd: HWND; CSIDL: Integer; hToken: THandle; dwFlags: DWORD; pszPath: LPWSTR): HResult; stdcall;
    external shell32 name 'SHGetFolderPathW';


implementation

end.
