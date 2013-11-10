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
}

{$i deltics.bonjour.inc}

{$ifdef deltics_bonjour_service}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Bonjour.Service;


interface

  uses
  { VCL: }
    Classes,
  { bonjour: }
    Deltics.Bonjour,
    Deltics.Bonjour.API,
    Deltics.Bonjour.TXT;


  type
    TService = class(TBonjourComponent)
    private
      fAutoRename: Boolean;
      fHandle: TDNSServiceRef;
      fPreAnnouncedServiceName: UnicodeString;
      fDomain: UnicodeString;
      fInterfaceID: Integer;
      fPort: Integer;
      fServiceName: UnicodeString;
      fServiceType: UnicodeString;
      fTXT: TTXTRecord;
      function get_ID: Integer;
      function get_TXT: TTXT;
    protected
      function get_Handle: TDNSServiceRef; override;
      procedure Activate; override;
      procedure Deactivate; override;
      procedure OnTXTChange(aSender: TObject);
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      property Domain: UnicodeString read fDomain write fDomain;
      property ID: Integer read get_ID;
      property TXT: TTXT read get_TXT;
    published
      property AutoRename: Boolean read fAutoRename write fAutoRename;
      property InterfaceID: Integer read fInterfaceID write fInterfaceID;
      property Port: Integer read fPort write fPort;
      property ServiceName: UnicodeString read fServiceName write fServiceName;
      property ServiceType: UnicodeString read fServiceType write fServiceType;
    end;



implementation

  uses
  { vcl: }
    SysUtils,
    Windows,
  { deltics: }
    Deltics.Strings,
  { bonjour: }
    Deltics.Bonjour.Thread;



  type
    TTXTRecordHelper = class(TTXTRecord);



  procedure DNSServiceRegisterReply(const aHandle: TDNSServiceRef;
                                    const aFlags: TDNSServiceFlags;
                                    const aErrorCode: TDNSServiceErrorType;
                                    const aName: PUTF8Char;
                                    const aRegType: PUTF8Char;
                                    const aDomain: PUTF8Char;
                                    const aContext: Pointer); stdcall;
  var
    theService: TService absolute aContext;
  begin
    Deltics.Bonjour.CheckResult(aErrorCode);

    theService.fServiceName := WIDE.FromUTF8(aName);
    theService.fDomain      := WIDE.FromUTF8(aDomain);
  end;





{ TService --------------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TService.Create(aOwner: TComponent);
  begin
    inherited;

    fTXT := TTXTRecord.Create;
    TTXTRecordHelper(fTXT).OnChange := OnTXTChange;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TService.Destroy;
  begin
    FreeAndNIL(fTXT);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TService.get_ID: Integer;
  begin
    result := Integer(self);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TService.get_TXT: TTXT;
  begin
    result := fTXT;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TService.get_Handle: TDNSServiceRef;
  begin
    result := fHandle;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TService.OnTXTChange(aSender: TObject);
  var
    txt: TTXTRecord absolute aSender;
  begin
    DNSServiceUpdateRecord(fHandle, NIL, 0, txt.Len, txt.Data, 0);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TService.Activate;
  var
    flags: Cardinal;
    pName: PUTF8Char;
    sName: UTF8String;
    sType: UTF8String;
  begin
    fPreAnnouncedServiceName := ServiceName;

    inherited;

    if AutoRename then
      flags := 0
    else
      flags := kDNSServiceFlagsNoAutoRename;  { - do not auto-rename }

    sType := UTF8.FromWide(fServiceType);

    if (fServiceName <> '') then
    begin
      sName := UTF8.FromWide(fServiceName);
      pName := PUTF8Char(sName);
    end
    else
      pName := NIL;

    CheckResult(DNSServiceRegister(fHandle,
                                   flags,
                                   0   { interfaceID - register on all interfaces },
                                   pName,
                                   PUTF8Char(sType),
                                   NIL { domain - register in all available },
                                   NIL { hostname - use default },
                                   ReverseBytes(Port),  {Port # is in network byte order (big endian, Windows is little endian) }
                                   fTXT.Len   { txtLen },
                                   fTXT.Data  { txtRecord },
                                   DNSServiceRegisterReply,
                                   self));

    TBonjourThread.AddService(fHandle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TService.Deactivate;
  begin
    inherited;

    TBonjourThread.RemoveService(fHandle);
    fHandle := NIL;

    ServiceName              := fPreAnnouncedServiceName;
    fPreAnnouncedServiceName := '';
  end;




end.
