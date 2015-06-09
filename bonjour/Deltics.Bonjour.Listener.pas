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

{$ifdef deltics_bonjour_listener}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Bonjour.Listener;


interface

  uses
    Classes,
    Contnrs,
    SyncObjs,
    Windows,
  { deltics: }
    Deltics.MultiCast,
    Deltics.Strings,
  { bonjour: }
    Deltics.Bonjour,
    Deltics.Bonjour.API,
    Deltics.Bonjour.Service,
    Deltics.Bonjour.TXT;


  type
    TListener = class;
    TServiceInfo = class;
    TServiceMetaQuery = class;


    TServiceDiscoveryEvent = procedure(const Sender: TListener; const aService: TServiceInfo) of object;
    TServiceResolvedEvent = procedure(const Sender: TServiceInfo) of object;


    TListener = class(TBonjourComponent)
    private
      fHandle: TDNSServiceRef;
      fDomain: UnicodeString;
      fServices: TThreadList;
      fServiceType: UnicodeString;
      fSynchronizeEvents: Boolean;
      fOnServiceFound: TServiceDiscoveryEvent;
      fOnServiceLost: TServiceDiscoveryEvent;
      fOnServiceResolved: TServiceDiscoveryEvent;
      function get_Count: Integer;
      function get_Items(const aIndex: Integer): TServiceInfo;
      procedure set_Domain(const aValue: UnicodeString);
      procedure set_ServiceType(const aValue: UnicodeString);
    protected
      function get_Handle: TDNSServiceRef; override;
      procedure Activate; override;
      procedure Deactivate; override;
      procedure DoServiceFound(const aService: TServiceInfo);
      procedure DoServiceLost(const aService: TServiceInfo);
      procedure DoServiceResolved(const aService: TServiceInfo);
    public
      class function ServiceMetaQuery: TServiceMetaQuery;
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      property Domain: UnicodeString read fDomain write set_Domain;
      property ServiceType: UnicodeString read fServiceType write set_ServiceType;
      property ServiceCount: Integer read get_Count;
      property Services[const aIndex: Integer]: TServiceInfo read get_Items; default;
      property SynchronizeEvents: Boolean read fSynchronizeEvents write fSynchronizeEvents;
      property OnServiceFound: TServiceDiscoveryEvent read fOnServiceFound write fOnServiceFound;
      property OnServiceLost: TServiceDiscoveryEvent read fOnServiceLost write fOnServiceLost;
      property OnServiceResolved: TServiceDiscoveryEvent read fOnServiceResolved write fOnServiceResolved;
    end;


    TServiceType = (
                    stA         = 1,      // Host address.
                    stNS        = 2,      // Authoritative server.
                    stMD        = 3,      // Mail destination.
                    stMF        = 4,      // Mail forwarder.
                    stCNAME     = 5,      // Canonical name.
                    stSOA       = 6,      // Start of authority zone.
                    stMB        = 7,      // Mailbox domain name.
                    stMG        = 8,      // Mail group member.
                    stMR        = 9,      // Mail rename name.
                    stNULL      = 10,     // Null resource record.
                    stWKS       = 11,     // Well known service.
                    stPTR       = 12,     // Domain name pointer.
                    stHINFO     = 13,     // Host information.
                    stMINFO     = 14,     // Mailbox information.
                    stMX        = 15,     // Mail routing information.
                    stTXT       = 16,     // One or more text strings.
                    stRP        = 17,     // Responsible person.
                    stAFSDB     = 18,     // AFS cell database.
                    stX25       = 19,     // X_25 calling address.
                    stISDN      = 20,     // ISDN calling address.
                    stRT        = 21,     // Router.
                    stNSAP      = 22,     // NSAP address.
                    stNSAP_PTR  = 23,     // Reverse NSAP lookup (deprecated).
                    stSIG       = 24,     // Security signature.
                    stKEY       = 25,     // Security key.
                    stPX        = 26,     // X.400 mail mapping.
                    stGPOS      = 27,     // Geographical position (withdrawn).
                    stAAAA      = 28,     // Ip6 Address.
                    stLOC       = 29,     // Location Information.
                    stNXT       = 30,     // Next domain (security).
                    stEID       = 31,     // Endpoint identifier.
                    stNIMLOC    = 32,     // Nimrod Locator.
                    stSRV       = 33,     // Server Selection.
                    stATMA      = 34,     // ATM Address
                    stNAPTR     = 35,     // Naming Authority PoinTeR
                    stKX        = 36,     // Key Exchange
                    stCERT      = 37,     // Certification record
                    stA6        = 38,     // IPv6 address (deprecates AAAA)
                    stDNAME     = 39,     // Non-terminal DNAME (for IPv6)
                    stSINK      = 40,     // Kitchen sink (experimentatl)
                    stOPT       = 41,     // EDNS0 option (meta-RR)
                    stTKEY      = 249,    // Transaction key
                    stTSIG      = 250,    // Transaction signature.
                    stIXFR      = 251,    // Incremental zone transfer.
                    stAXFR      = 252,    // Transfer zone of authority.
                    stMAILB     = 253,    // Transfer mailbox records.
                    stMAILA     = 254,    // Transfer mail agent records.
                    stANY       = 255     // Wildcard match.
                   );


    TServiceInfo = class
    private
      fDomain: UnicodeString;
      fFullName: UnicodeString;
      fHandle: TDNSServiceRef;
      fHostName: UnicodeString;
      fInterfaceID: Integer;
      fName: UnicodeString;
      fOwner: TListener;
      fPort: Word;
      fResolved: TEvent;
      fResolving: Boolean;
      fServiceType: UnicodeString;
      fTXT: TTXTInfo;
      fOnResolved: TServiceResolvedEvent;
      function get_HostName: UnicodeString;
      procedure set_TXT(const aValue: TTXTInfo);
    protected
      constructor Create(const aOwner: TListener;
                         const aName: UnicodeString;
                         const aDomain: UnicodeString;
                         const aServiceType: UnicodeString;
                         const aInterfaceID: Integer);
      procedure DoResolved(const aService: TServiceInfo);
    public
      destructor Destroy; override;
      procedure Query(const aServiceType: TServiceType);
      procedure Resolve; overload;
      function Resolve(const aTimeOut: Cardinal): Boolean; overload;
      property Domain: UnicodeString read fDomain;
      property FullName: UnicodeString read fFullName;
      property HostName: UnicodeString read get_HostName;
      property InterfaceID: Integer read fInterfaceID;
      property Name: UnicodeString read fName;
      property Owner: TListener read fOwner;
      property Port: Word read fPort;
      property Resolving: Boolean read fResolving;
      property ServiceType: UnicodeString read fServiceType;
      property TXT: TTXTInfo read fTXT write set_TXT;
      property OnResolved: TServiceResolvedEvent read fOnResolved write fOnResolved;
    end;


    TServiceMetaQuery = class
    private
      CSS: TCriticalSection;
      fDomains: array of UnicodeString;
      fServiceTypes: array of UnicodeString;
      fOn_Update: TMultiCastNotify;
      constructor Create;
      function get_Count: Integer;
      function get_Domain(const aIndex: Integer): UnicodeString;
      function get_ServiceType(const aIndex: Integer): UnicodeString;
      procedure AddServiceType(const aListener: TListener; const aService: TServiceInfo);
    public
      destructor Destroy; override;
      property Count: Integer read get_Count;
      property Domains[const aIndex: Integer]: UnicodeString read get_Domain;
      property ServiceTypes[const aIndex: Integer]: UnicodeString read get_ServiceType;
      property On_Update: TMultiCastNotify read fOn_Update;
    end;



    TListenerServiceMessage = record
      MessageID: Cardinal;
      Listener: TListener;
      Service: TServiceInfo;
      Result: LongInt;
    end;




implementation

  uses
  { vcl: }
    SysUtils,
    Types,
    WinSock,
  { bonjour: }
    Deltics.Bonjour.Thread;





  type
    TTXTHelper = class(TTXT);





  procedure Listener_ServiceFound(const aListener: TListener;
                                  const aName: UnicodeString;
                                  const aDomain: UnicodeString;
                                  const aServiceType: UnicodeString;
                                  const aInterfaceID: Integer);
  var
    service: TServiceInfo;
    msg: TListenerServiceMessage;
  begin
    service := TServiceInfo.Create(aListener, aName, aDomain, aServiceType, aInterfaceID);

    if aListener.SynchronizeEvents then
    begin
      msg.MessageID := BJM_ServiceFound;
      msg.Listener  := aListener;
      msg.Service   := service;

      TBonjourThread.NotifyVCL(msg);
    end
    else
      aListener.DoServiceFound(service);
  end;


  procedure Listener_ServiceLost(const aListener: TListener;
                                 const aName: UnicodeString;
                                 const aDomain: UnicodeString;
                                 const aServiceType: UnicodeString;
                                 const aInterfaceID: Integer);
  var
    i: Integer;
    service: TServiceInfo;
    msg: TListenerServiceMessage;
  begin
    for i := 0 to Pred(aListener.ServiceCount) do
    begin
      service := aListener.Services[i];

      if SameText(service.Name, aName)
       and SameText(service.Domain, aDomain)
       and SameText(service.ServiceType, aServiceType)
       and (service.InterfaceID = aInterfaceID) then
      begin
        if aListener.SynchronizeEvents then
        begin
          msg.MessageID := BJM_ServiceLost;
          msg.Listener  := aListener;
          msg.Service   := service;

          TBonjourThread.NotifyVCL(msg);
        end
        else
          aListener.DoServiceLost(service);

        EXIT;
      end;
    end;
  end;


  procedure Service_Resolved(const aService: TServiceInfo);
  var
    msg: TListenerServiceMessage;
  begin
    if aService.Owner.SynchronizeEvents then
    begin
      msg.MessageID := BJM_ServiceResolved;
      msg.Listener  := NIL;
      msg.Service   := aService;

      TBonjourThread.NotifyVCL(msg);
    end
    else
      aService.Owner.DoServiceResolved(aService);
  end;


  procedure Service_QueryResult(const aService: TServiceInfo);
  var
    msg: TListenerServiceMessage;
  begin
//    msg.MessageID := BJM_ServiceQueryResult;
    msg.Listener  := NIL;
    msg.Service   := aService;

    TBonjourThread.NotifyVCL(msg);
  end;



  procedure DNSServiceBrowseReply(const aServiceRef: TDNSServiceRef;
                                  const aFlags: TDNSServiceFlags;
                                  const aInterfaceIndex: uint32_t;
                                  const aErrorCode: TDNSServiceErrorType;
                                  const aServiceName: PUTF8Char;
                                  const aRegType: PUTF8Char;
                                  const aReplyDomain: PUTF8Char;
                                  const aContext: Pointer); stdcall;
  var
    theListener: TListener absolute aContext;
    sServiceName: UnicodeString;
    sRegType: UnicodeString;
    sReplyDomain: UnicodeString;
  begin
    Deltics.Bonjour.CheckResult(aErrorCode);

    sServiceName  := WIDE.FromUTF8(aServiceName);
    sRegType      := WIDE.FromUTF8(aRegType);
    sReplyDomain  := WIDE.FromUTF8(aReplyDomain);

    if ((kDNSServiceFlagsAdd and aFlags) <> 0) then
      Listener_ServiceFound(theListener, sServiceName, sReplyDomain, sRegType, aInterfaceIndex)
    else
      Listener_ServiceLost(theListener, sServiceName, sReplyDomain, sRegType, aInterfaceIndex);
  end;



  procedure DNSServiceResolveReply(const aHandle: TDNSServiceRef;
                                   const aFlags: TDNSServiceFlags;
                                   const aInterfaceIndex: uint32_t;
                                   const aErrorCode: TDNSServiceErrorType;
                                   const aFullname: PUTF8Char;
                                   const aHostTarget: PUTF8Char;
                                   const aPort: uint16_t;
                                   const aTxtLen: uint16_t;
                                   const aTxtRecord: PTXTRecordData;
                                   const aContext: Pointer); stdcall;
  var
    theService: TServiceInfo absolute aContext;
    sFullName: UnicodeString;
    sHostName: UnicodeString;
    txt: TTXTInfo;
  begin
    Deltics.Bonjour.CheckResult(aErrorCode);

    sFullName := WIDE.FromUTF8(aFullName);
    sHostName := WIDE.FromUTF8(aHostTarget);

    txt := TTXTInfo.Create(aTxtRecord, aTxtLen);
    theService.TXT := txt;
    theService.fFullName    := sFullName;
    theService.fHostName    := sHostName;
    theService.fPort        := ntohs(aPort);
    theService.fInterfaceID := aInterfaceIndex;

    TBonjourThread.RemoveService(aHandle);

    Service_Resolved(theService);
  end;




  procedure DNSServiceQueryRecordReply(const aHandle: TDNSServiceRef;
                                       const aFlags: TDNSServiceFlags;
                                       const aInterfaceIndex: uint32_t;
                                       const aErrorCode: TDNSServiceErrorType;
                                       const aFullname: PUTF8Char;
                                       const aRRType: uint16_t;
                                       const aRRClass: uint16_t;
                                       const aRDLen: uint16_t;
                                       const aRData: Pointer;
                                       const TTL: uint32_t;
                                       const aContext: Pointer
                                       ); stdcall;
  var
    theService: TServiceInfo absolute aContext;
  begin
    Deltics.Bonjour.CheckResult(aErrorCode);
//    TTXTHelper(theService.TXT).Load(aRData, aRDLen);

    TBonjourThread.RemoveService(aHandle);
//    theService.fResolver.Terminate(aHandle);
  end;




{ TListener -------------------------------------------------------------------------------------- }

  var
    MetaQuery: TServiceMetaQuery = NIL;
    MetaQueryListener: TListener = NIL;


{ TServiceMetaQuery }

  constructor TServiceMetaQuery.Create;
  begin
    inherited Create;

    CSS := TCriticalSection.Create;

    fOn_Update := TMultiCastNotify.Create(self);
  end;


  destructor TServiceMetaQuery.Destroy;
  begin
    fOn_Update.Disable;

    MetaQueryListener.Active         := FALSE;
    MetaQueryListener.OnServiceFound := NIL;

    CSS.Enter;

    FreeAndNIL(MetaQueryListener);

    MetaQuery := NIL;

    CSS.Free;

    fOn_Update.Free;

    inherited;
  end;


  procedure TServiceMetaQuery.AddServiceType(const aListener: TListener; const aService: TServiceInfo);
  var
    i: Integer;
  begin
    CSS.Enter;
    try
      i := Length(fDomains);

      SetLength(fDomains,       i + 1);
      SetLength(fServiceTypes,  i + 1);

      fDomains[i]       := aService.Domain;
      fServiceTypes[i]  := aService.Name + '.' + aService.ServiceType;

      if (fDomains[i] = '.')
       and (Pos(UnicodeString('.local.'), aService.ServiceType) = (Length(aService.ServiceType) - 6)) then
      begin
        fDomains[i] := 'local.';
        SetLength(fServiceTypes[i], Length(fServiceTypes[i]) - 6);
      end;

    finally
      CSS.Leave;
    end;

    fOn_Update.DoEvent;
  end;


  function TServiceMetaQuery.get_Count: Integer;
  begin
    CSS.Enter;
    try
      result := Length(fDomains);

    finally
      CSS.Leave;
    end;
  end;


  function TServiceMetaQuery.get_Domain(const aIndex: Integer): UnicodeString;
  begin
    CSS.Enter;
    try
      result := fDomains[aIndex];

    finally
      CSS.Leave;
    end;
  end;


  function TServiceMetaQuery.get_ServiceType(const aIndex: Integer): UnicodeString;
  begin
    CSS.Enter;
    try
      result := fServiceTypes[aIndex];

    finally
      CSS.Leave;
    end;
  end;



  class function TListener.ServiceMetaQuery: TServiceMetaQuery;
  begin
    if NOT Assigned(MetaQuery) then
    begin
      MetaQuery := TServiceMetaQuery.Create;

      MetaQueryListener := TListener.Create(NIL);
      MetaQueryListener.Domain          := 'local.';
      MetaQueryListener.ServiceType     := '_services._dns-sd._udp';
      MetaQueryListener.OnServiceFound  := MetaQuery.AddServiceType;

      MetaQueryListener.Active  := TRUE;
    end;

    result := MetaQuery;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TListener.Create(aOwner: TComponent);
  begin
    inherited;

    fServices           := TThreadList.Create;
    fSynchronizeEvents  := NOT IsConsole;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TListener.Destroy;
  var
    i: Integer;
    list: TList;
  begin
    inherited Destroy;

    list := fServices.LockList;
    for i := 0 to Pred(list.Count) do
      TServiceInfo(list[i]).Free;

    FreeAndNIL(fServices);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TListener.get_Count: Integer;
  begin
    result := fServices.LockList.Count;
    fServices.UnlockList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TListener.get_Handle: TDNSServiceRef;
  begin
    result := fHandle;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TListener.get_Items(const aIndex: Integer): TServiceInfo;
  begin
    result := TServiceInfo(fServices.LockList[aIndex]);
    fServices.UnlockList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.set_Domain(const aValue: UnicodeString);
  begin
    EnsureNotActive;
    fDomain := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.set_ServiceType(const aValue: UnicodeString);
  begin
    EnsureNotActive;
    fServiceType := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.Activate;
  begin
    CheckResult(DNSServiceBrowse(fHandle,
                                 0 { flags - not used },
                                 kDNSServiceInterfaceIndexAny,
                                 PUTF8Char(UTF8.FromWide(ServiceType)),
                                 PUTF8Char(UTF8.FromWide(Domain)),
                                 DNSServiceBrowseReply,
                                 self));

    TBonjourThread.AddService(fHandle);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.Deactivate;
  begin
    TBonjourThread.RemoveService(fHandle);
    fHandle := NIL;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.DoServiceFound(const aService: TServiceInfo);
  begin
    fServices.LockList.Add(aService);
    fServices.UnlockList;

    if Assigned(OnServiceFound) then
      OnServiceFound(self, aService);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.DoServiceLost(const aService: TServiceInfo);
  begin
    if Assigned(OnServiceLost) then
      OnServiceLost(self, aService);

    fServices.LockList.Remove(aService);
    try
      aService.Free;

    finally
      fServices.UnlockList;
    end;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TListener.DoServiceResolved(const aService: TServiceInfo);
  begin
    if Assigned(OnServiceResolved) then
      OnServiceResolved(self, aService);
  end;








{ TServiceInfo ----------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TServiceInfo.Create(const aOwner: TListener;
                                  const aName: UnicodeString;
                                  const aDomain: UnicodeString;
                                  const aServiceType: UnicodeString;
                                  const aInterfaceID: Integer);
  begin
    inherited Create;

    fOwner        := aOwner;
    fName         := aName;
    fDomain       := aDomain;
    fServiceType  := aServiceType;
    fInterfaceID  := aInterfaceID;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TServiceInfo.Destroy;
  begin
    FreeAndNIL(fTXT);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TServiceInfo.get_HostName: UnicodeString;
  const
    LOCAL = '.local.';
  begin
    result := fHostName;

    if SameText(Copy(result, Length(result) - (Length(LOCAL) - 1), Length(LOCAL)), LOCAL) then
      SetLength(result, Length(result) - Length(LOCAL));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TServiceInfo.set_TXT(const aValue: TTXTInfo);
  begin
    FreeAndNIL(fTXT);
    fTXT := aValue;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TServiceInfo.Resolve;
  {
    Resolves a service asynchronously.

    The caller should have assigned a handler to the OnServiceResolved event
     before calling this method.
  }
  var
    hResolve: TDNSServiceRef;
  begin
    if Resolving then
      EXIT;

    CheckResult(DNSServiceResolve(hResolve,
                                  0 { flags - not used },
                                  InterfaceID,
                                  PUTF8Char(UTF8.FromWide(Name)),
                                  PUTF8Char(UTF8.FromWide(ServiceType)),
                                  PUTF8Char(UTF8.FromWide(Domain)),
                                  DNSServiceResolveReply,
                                  self));

    fResolving := TRUE;

    TBonjourThread.AddService(hResolve);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  function TServiceInfo.Resolve(const aTimeOut: Cardinal): Boolean;
  {
    Attempts to resolve a service synchronously.

    If the service is already being resolved the function returns FALSE.
  }
  begin
    if NOT Resolving then
    begin
      fResolved := TEvent.Create(NIL, TRUE, FALSE, Name + '.resolved');
      result := TBonjourThread.WaitFor(Resolve, Name + '.resolved', aTimeOut);
    end
    else
      result := FALSE;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TServiceInfo.Query(const aServiceType: TServiceType);
  begin
    CheckResult(DNSServiceQueryRecord(fHandle,
                                      0,
                                      InterfaceID,
                                      PUTF8Char(UTF8.FromWide(FullName)),
                                      Ord(aServiceType),
                                      kDNSServiceClass_IN,
                                      DNSServiceQueryRecordReply,
                                      self));

    TBonjourThread.AddService(fHandle);

    // TODO: Synchronous version ?
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TServiceInfo.DoResolved;
  begin
    if Assigned(fResolved) then
    begin
      fResolved.SetEvent;
      FreeAndNIL(fResolved);
    end;

    fResolving := FALSE;

    if Assigned(fOnResolved) then
      fOnResolved(self);

    Owner.DoServiceResolved(self);
  end;




initialization

finalization
  if Assigned(MetaQuery) then
    MetaQuery.Free

end.

