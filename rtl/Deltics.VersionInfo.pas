{
  * X11 (MIT) LICENSE *

  Copyright © 2008 Jolyon Smith

  Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.


  * GPL and Other Licenses *

  The FSF deem this license to be compatible with version 3 of the GPL.
   Compatability with other licenses should be verified by reference to those
   other license terms.


  * Contact Details *

  Original author : Jolyon Smith
  skype           : deltics
  e-mail          : <EXTLINK mailto: jsmith@deltics.co.nz>jsmith@deltics.co.nz</EXTLINK>
  website         : <EXTLINK http://www.deltics.co.nz>www.deltics.co.nz</EXTLINK>


  * Acknowledgements *

  With thanks to Peter Johnson for the insight into the translation records
   often overlooked by version info implementations:

   http://www.delphidabbler.com/articles?article=20
}

{$i deltics.rtl.inc}

{$ifdef deltics_versioninfo}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.VersionInfo;


interface

  uses
  { vcl: }
    Classes,
    Windows;


  const
    INFODW_FileOS         = 0;
    INFODW_FileType       = 1;
    INFODW_FileSubType    = 2;
    INFODW_FileFlagsMask  = 3;
    INFODW_FileFlags      = 4;

    INFOSTR_FIRST             = 0;
    INFOSTR_Comments          = INFOSTR_FIRST + 0;
    INFOSTR_CompanyName       = INFOSTR_FIRST + 1;
    INFOSTR_FileDescription   = INFOSTR_FIRST + 2;
    INFOSTR_FileVersion       = INFOSTR_FIRST + 3;
    INFOSTR_InternalName      = INFOSTR_FIRST + 4;
    INFOSTR_LegalCopyright    = INFOSTR_FIRST + 5;
    INFOSTR_LegalTrademarks   = INFOSTR_FIRST + 6;
    INFOSTR_OriginalFileName  = INFOSTR_FIRST + 7;
    INFOSTR_PrivateBuild      = INFOSTR_FIRST + 8;
    INFOSTR_ProductName       = INFOSTR_FIRST + 9;
    INFOSTR_ProductVersion    = INFOSTR_FIRST + 10;
    INFOSTR_SpecialBuild      = INFOSTR_FIRST + 11;
    INFOSTR_LAST              = INFOSTR_FIRST + 11;

  type
    TVersionItem = (viComments          = INFOSTR_Comments,
                    viCompanyName       = INFOSTR_CompanyName,
                    viFileDescription   = INFOSTR_FileDescription,
                    viFileVersion       = INFOSTR_FileVersion,
                    viInternalName      = INFOSTR_InternalName,
                    viLegalCopyright    = INFOSTR_LegalCopyright,
                    viLegalTrademarks   = INFOSTR_LegalTrademarks,
                    viOriginalFileName  = INFOSTR_OriginalFileName,
                    viPrivateBuild      = INFOSTR_PrivateBuild,
                    viProductName       = INFOSTR_ProductName,
                    viProductVersion    = INFOSTR_ProductVersion,
                    viSpecialBuild      = INFOSTR_SpecialBuild
                   );

    TVersionNo = packed record
      Major    : WORD;
      Minor    : WORD;
      Revision : WORD;
      Build    : WORD;
    end;


    TTranslationRecord = packed record
      LanguageCode : WORD;
      CharSet      : WORD;
    end;

    TTranslationRecords = array of TTranslationRecord;
    PTranslationRecords = ^TTranslationRecords;


    TVersionInfo = class
    private
      fFileName: String;
      fHasInfo: Boolean;
      fTranslationCount: Integer;
      fTranslation: Integer;
      fVersionInfo: Pointer;
      fFixedInfo: PVSFixedFileInfo;       // Pointer to fixed info area of version info buffer
      fTranslations: PTranslationRecords; // Pointer to translation records in version info buffer
      function get_Language: String;
      function get_LanguageCode: WORD;
      function get_CharSet: String;
      function get_CharSetCode: WORD;
      function get_Translation: TTranslationRecord;
      function get_TranslationIndex: Integer;
      function get_FileVersionNo: TVersionNo;
      function get_HasTranslation: Boolean;
      function get_InfoDWORD(const aIndex: Integer): DWORD;
      function get_InfoSTRING(const aIndex: Integer): String;
      function get_ItemName(const aItem: TVersionItem): String;
      function get_ItemValue(const aItem: TVersionItem): String;
      function get_NamedInfo(const aName: String): String;
      function get_ProductVersionNo: TVersionNo;
      procedure set_FileName(const aValue: String);
      procedure set_TranslationIndex(const aValue: Integer);

      procedure Clear;
      procedure LoadVersionInfo;
    public
      constructor Create;
      destructor Destroy; override;
      property HasInfo: Boolean read fHasInfo;
      property HasTranslation: Boolean read get_HasTranslation;

      property FileOS: DWORD        index INFODW_FileOS         read get_InfoDWORD;
      property FileType: DWORD      index INFODW_FileType       read get_InfoDWORD;
      property FileSubType: DWORD   index INFODW_FileSubType    read get_InfoDWORD;
      property FileFlagsMask: DWORD index INFODW_FileFlagsMask  read get_InfoDWORD;
      property FileFlags: DWORD     index INFODW_FileFlags      read get_InfoDWORD;

      property FileVersionNo: TVersionNo read get_FileVersionNo;
      property ProductVersionNo: TVersionNo read get_ProductVersionNo;

      property Comments: String         index INFOSTR_Comments         read get_InfoSTRING;
      property CompanyName: String      index INFOSTR_CompanyName      read get_InfoSTRING;
      property FileDescription: String  index INFOSTR_FileDescription  read get_InfoSTRING;
      property FileVersion: String      index INFOSTR_FileVersion      read get_InfoSTRING;
      property InternalName: String     index INFOSTR_InternalName     read get_InfoSTRING;
      property LegalCopyright: String   index INFOSTR_LegalCopyright   read get_InfoSTRING;
      property LegalTrademarks: String  index INFOSTR_LegalTrademarks  read get_InfoSTRING;
      property OriginalFileName: String index INFOSTR_OriginalFileName read get_InfoSTRING;
      property PrivateBuild: String     index INFOSTR_PrivateBuild     read get_InfoSTRING;
      property ProductName: String      index INFOSTR_ProductName      read get_InfoSTRING;
      property ProductVersion: String   index INFOSTR_ProductVersion   read get_InfoSTRING;
      property SpecialBuild: String     index INFOSTR_SpecialBuild     read get_InfoSTRING;
      property CustomValue[const aName: String]: String read get_NamedInfo;
      property ItemName[const aItem: TVersionItem]: String read get_ItemName;
      property ItemValue[const aItem: TVersionItem]: String read get_ItemValue;

      property Language: String read get_Language;
      property CharSet: String read get_CharSet;
      property LanguageCode: WORD read get_LanguageCode;
      property CharSetCode: WORD read get_CharSetCode;
      property Translation: TTranslationRecord read get_Translation;
      property TranslationCount: Integer read fTranslationCount;
      property TranslationIndex: Integer read get_TranslationIndex write set_TranslationIndex;

      property FileName: String read fFileName write set_FileName;
    end;


  function VersionToStr(const aVersionNo: TVersionNo): String;



implementation

  uses
  { vcl: }
    SysUtils;


  const
    INFO_Names : array[TVersionItem] of String = ('Comments',
                                                  'CompanyName',
                                                  'FileDescription',
                                                  'FileVersion',
                                                  'InternalName',
                                                  'LegalCopyright',
                                                  'LegalTrademarks',
                                                  'OriginalFileName',
                                                  'PrivateBuild',
                                                  'ProductName',
                                                  'ProductVersion',
                                                  'SpecialBuild');
    INVALID_TRANSLATION = -1;



  function VersionToStr(const aVersionNo: TVersionNo): String;
  begin
    result := Format('%d.%d.%d.%d',[aVersionNo.Major,
                                    aVersionNo.Minor,
                                    aVersionNo.Revision,
                                    aVersionNo.Build]);
  end;



  constructor TVersionInfo.Create;
  begin
    inherited Create;
    FileName := '';
  end;


  destructor TVersionInfo.Destroy;
  begin
    Clear;
    inherited Destroy;
  end;


  function TVersionInfo.get_CharSet: String;
  const
    CHARSET: array[0..11] of record
                               Code: WORD;
                               Name: String;
                             end = ( ( Code:    0; Name: '7-bit ASCII'                        ),
                                     ( Code:  932; Name: 'Windows, Japan (Shift - JIS X-0208)'),
                                     ( Code:  949; Name: 'Windows, Korea (Shift - KSC 5601)'  ),
                                     ( Code:  950; Name: 'Windows, Taiwan (GB5)'              ),
                                     ( Code: 1200; Name: 'Unicode'                            ),
                                     ( Code: 1250; Name: 'Windows, Latin-2 (Eastern European)'),
                                     ( Code: 1251; Name: 'Windows, Cyrillic'                  ),
                                     ( Code: 1252; Name: 'Windows, Multilingual'              ),
                                     ( Code: 1253; Name: 'Windows, Greek'                     ),
                                     ( Code: 1254; Name: 'Windows, Turkish'                   ),
                                     ( Code: 1255; Name: 'Windows, Hebrew'                    ),
                                     ( Code: 1256; Name: 'Windows, Arabic'                    )
                                   );
  var
    i: Integer;
    wCharSet: WORD;
  begin
    result := '';

    if NOT HasInfo then
      EXIT;

    wCharSet := CharSetCode;

    for i := Low(CHARSET) to High(CHARSET) do
    begin
      if (wCharSet = CHARSET[i].Code) then
      begin
        result := CHARSET[i].Name;
        EXIT;
      end;
    end;

    result := '(unknown)';
  end;


  function TVersionInfo.get_CharSetCode: WORD;
  begin
    if HasInfo and (TranslationIndex >= 0) then
      result := Translation.CharSet
    else
      result := 0;
  end;


  function TVersionInfo.get_Language: String;
  var
    buf: array[0..255] of char;
  begin
    result := '';

    if HasInfo and (VerLanguageName(get_LanguageCode, buf, 255) > 0) then
      result := buf;
  end;


  function TVersionInfo.get_LanguageCode: WORD;
  begin
    if HasTranslation then
      result := Translation.LanguageCode
    else
      result := 0;
  end;


  function TVersionInfo.get_FileVersionNo: TVersionNo;
  begin
    result.Major    := HiWord(fFixedInfo.dwFileVersionMS);
    result.Minor    := LoWord(fFixedInfo.dwFileVersionMS);
    result.Revision := HiWord(fFixedInfo.dwFileVersionLS);
    result.Build    := LoWord(fFixedInfo.dwFileVersionLS);
  end;


  function TVersionInfo.get_InfoDWORD(const aIndex: Integer): DWORD;
  begin
    case aIndex of
      INFODW_FileOS        : result := fFixedInfo.dwFileOS;
      INFODW_FileType      : result := fFixedInfo.dwFileType;
      INFODW_FileSubType   : result := fFixedInfo.dwFileSubType;
      INFODW_FileFlagsMask : result := fFixedInfo.dwFileFlagsMask;
      INFODW_FileFlags     : result := fFixedInfo.dwFileFlags;
    else
      result := 0;
    end;
  end;


  function TVersionInfo.get_InfoSTRING(const aIndex: Integer): String;
  begin
    result := ItemValue[TVersionItem(aIndex)];
  end;


  function TVersionInfo.get_ItemName(const aItem: TVersionItem): String;
  begin
    result := INFO_Names[aItem];
  end;


  function TVersionInfo.get_ItemValue(const aItem: TVersionItem): String;
  begin
    result := CustomValue[INFO_Names[aItem]];
  end;


  function TVersionInfo.get_NamedInfo(const aName: String): String;
  var
    sQuery: String;
    ptr: Pointer;
    notUsed: Cardinal;
  begin
    result := '';

    if NOT HasTranslation then
      EXIT;

    sQuery  := Format('\StringFileInfo\%4.4x%4.4x\%s', [Translation.LanguageCode,
                                                        Translation.CharSet,
                                                        aName]);

    if VerQueryValue(fVersionInfo, PChar(sQuery), ptr, notUsed) then
      result := PChar(ptr);
  end;


  function TVersionInfo.get_ProductVersionNo: TVersionNo;
  begin
    result.Major    := HiWord(fFixedInfo.dwProductVersionMS);
    result.Minor    := LoWord(fFixedInfo.dwProductVersionMS);
    result.Revision := HiWord(fFixedInfo.dwProductVersionLS);
    result.Build    := LoWord(fFixedInfo.dwProductVersionLS);
  end;


  function TVersionInfo.get_Translation: TTranslationRecord;
  begin
    result := TTranslationRecords(fTranslations)[fTranslation];
  end;


  function TVersionInfo.get_TranslationIndex: Integer;
  begin
    if HasInfo then
      result := fTranslation
    else
      result := INVALID_TRANSLATION;
  end;


  procedure TVersionInfo.set_FileName(const aValue: String);
  begin
    if (aValue = '') then
      fFileName := ParamStr(0)
    else
      fFileName := aValue;

    LoadVersionInfo;
  end;


  procedure TVersionInfo.Clear;
  begin
    if Assigned(fVersionInfo) then
      FreeMem(fVersionInfo);

    fVersionInfo  := NIL;
    fFixedInfo    := NIL;
    fTranslations := NIL;

    fHasInfo          := FALSE;
    fTranslationCount := 0;
    fTranslation      := INVALID_TRANSLATION;
  end;


  procedure TVersionInfo.LoadVersionInfo;
  var
    dwSize: DWORD;
    dwDummy: DWORD;
    ptr: Pointer;
  begin
    Clear;

    // Determine required buffer size (0 if no version info)
    dwSize := GetFileVersionInfoSize(PChar(fFileName), dwDummy);
    if (dwSize = 0) then
      EXIT;

    // Allocate a buffer for and read version info from the required file
    GetMem(fVersionInfo, dwSize);

    fHasInfo := GetFileVersionInfo(PChar(fFileName), dwDummy, dwSize, fVersionInfo);

    if fHasInfo then
    begin
      // Got version info - set the fixed info pointer

      VerQueryValue(fVersionInfo, '\', ptr, dwSize);
      fFixedInfo := PVSFixedFileInfo(ptr);

      // Get pointer to translation records and total size from which we
      //  can determine the number of translations available
      VerQueryValue(fVersionInfo, '\VarFileInfo\Translation', ptr, dwSize);

      if (dwSize > 0) then
      begin
        // One or more translations so set the translation base pointer,
        //  translations count and initial translation index
        fTranslations     := PTranslationRecords(ptr);
        fTranslationCount := dwSize div SizeOf(TTranslationRecord);
        fTranslation      := 0;
      end;
    end
    else
      Clear;
  end;


  procedure TVersionInfo.set_TranslationIndex(const aValue: Integer);
  begin
    if (aValue >= 0) and (aValue < TranslationCount) then
      fTranslation := aValue
    else
    begin
      fTranslation := INVALID_TRANSLATION;
      raise Exception.Create('No such translation');
    end;
  end;



  function TVersionInfo.get_HasTranslation: Boolean;
  begin
    result := HasInfo and (fTranslation <> INVALID_TRANSLATION);
  end;






end.
