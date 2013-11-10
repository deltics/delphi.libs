
  unit Bonjour.Tests;

interface

  uses
    Deltics.SmokeTest,
    Deltics.Bonjour,
    Deltics.Bonjour.API,
    Deltics.Bonjour.Domains,
    Deltics.Bonjour.Listener,
    Deltics.Bonjour.Service,
    Deltics.Bonjour.TXT;


  type
    TAPITests = class(TTestCase)
    private
      fTestAnnouncer: TService;
    published
      procedure IsBonjourInstalled;
      procedure GetVersion;
      procedure EnumDomains;
      procedure CreateTXTRecord;
      procedure CreateTestAnnouncer;
      procedure Browse_iTunesSharedLibraries;
      procedure Register_iTunesSharedLibrary;
      procedure Browse_TestServices;
      procedure KillTestAnnouncer;
    end;



implementation

  uses
    Classes,
    Forms,
    SysUtils,
    Windows;


{ TAPITests }

  procedure TAPITests.IsBonjourInstalled;
  begin
    Test('Bonjour installed').Expect(BonjourInstalled).IsTRUE.IsShowStopper;
  end;


  procedure TAPITests.GetVersion;
  var
    ver: Cardinal;
  begin
    ver := BonjourVersion;
    Test('Bonjour Version').Expect(ver).GreaterThan(0);
    Inspect('Bonjour Version').Value(ver);

// TODO: Low-level API tests need to be written as far as possible, similar to this alternative version test:
(*
  var
    size: Cardinal;
  begin
    size := sizeof(ver);
    CheckResult(DNSServiceGetProperty(PUTF8Char(StringToUTF8(kDNSServiceProperty_DaemonVersion)), ver, size));
  end;
*)
  end;


  procedure TAPITests.EnumDomains;
  var
    i: Integer;
    domains: TDomainBrowser;
  begin
    domains := TDomainBrowser.Create(dtBrowsing);
    try
      domains.Refresh(20000);

      for i := 0 to Pred(domains.Count) do
        Inspect('domain name')[i].Value(domains.Items[i].Name);

    finally
      domains.Free;
    end;
  end;


  procedure TAPITests.CreateTXTRecord;
  var
    txt: TTXTRecord;
  begin
    txt := TTXTRecord.Create;
    try
      txt.Values['key'] := '';

      Test('After adding ''key'' with no value')['Len'].Expect(txt.Len).Equals(4);
      Test('After adding ''key'' with no value')['Count'].Expect(txt.Count).Equals(1);

      txt.Delete('key');

      Test('After deleting ''key''')['Len'].Expect(txt.Len).Equals(0);
      Test('After deleting ''key''')['Count'].Expect(txt.Count).Equals(0);

    finally
      txt.Free;
    end;
  end;


  procedure TAPITests.CreateTestAnnouncer;
  begin
    fTestAnnouncer := TService.Create(NIL);
    fTestAnnouncer.AutoRename   := FALSE;
    fTestAnnouncer.ServiceName  := 'Test Service (smoketest)';
    fTestAnnouncer.ServiceType  := '_http._tcp';
    fTestAnnouncer.Port         := 80;

    fTestAnnouncer.TXT.Values['SomeKey'] := 'Unlocks some door ©2010';

    Note('Announcing: %s', [fTestAnnouncer.ServiceName]);

    fTestAnnouncer.Active := TRUE;
  end;


  procedure TAPITests.Browse_iTunesSharedLibraries;
  var
    i: Integer;
    listener: TListener;
    service: {Deltics.Bonjour.Listener.}TServiceInfo;
    sKey: String;
  begin
    listener := TListener.Create(NIL);
    try
      listener.Domain      := 'local.';
      listener.ServiceType := '_daap._tcp';
      listener.Active      := TRUE;

      Sleep(500);

      if listener.ServiceCount = 0 then
        Abort('No services found');

      service := listener[0];

      Note('Found service: ' + service.Name);
      Note('Resolving');

      Test('Resolving service').Expect(service.Resolve(500)).IsTRUE.IsRequired;

      Inspect('Service')['name'].Value(service.Name);
      Inspect('Service')['host'].Value(service.HostName);
      Inspect('Service')['port'].Value(service.Port);

      for i := 0 to Pred(service.TXT.Count) do
      begin
        sKey := service.TXT.Keys[i];
        Inspect('TXT')[sKey].Value(service.TXT.Values[sKey]);
      end;

    finally
      listener.Free;
    end;
  end;


  procedure TAPITests.Register_iTunesSharedLibrary;
  var
    server: TService;
  begin
    server := TService.Create(NIL);
    try
      server.ServiceName  := 'Smoketest™';
      server.ServiceType  := '_daap._tcp';
      server.Port         := 3689;
      server.Active       := TRUE;

      Note('Announcing iTunes Shared Library %s', [server.ServiceName]);

      Sleep(5000);

    finally
      server.Free;
    end;
  end;


  procedure TAPITests.Browse_TestServices;
  var
    i: Integer;
    listener: TListener;
    service: TServiceInfo;
    sKey: String;
  begin
    listener := TListener.Create(NIL);
    try
      listener.Domain      := 'local.';
      listener.ServiceType := '_http._tcp';
      listener.Active      := TRUE;

      Sleep(2000);

      if listener.ServiceCount = 0 then
        Abort('No services found!');

      service := listener[0];

      Note('Found service ''%s''', [service.Name]);
      Test('Resolving service').Expect(service.Resolve(500)).IsTRUE.IsRequired;

      Inspect('Service')['name'].Value(service.Name);
      Inspect('Service')['host'].Value(service.HostName);
      Inspect('Service')['port'].Value(service.Port);

      Test('Service has TXT records').Expect(service.TXT).IsAssigned.IsRequired;

      for i := 0 to Pred(service.TXT.Count) do
      begin
        sKey := service.TXT.Keys[i];
        Inspect('TXT')[sKey].Value(service.TXT.Values[sKey]);
      end;

      Test('TXT.SomeKey').Expect(service.TXT.Values['SomeKey']).Equals('Unlocks some door ©2010');

    finally
      listener.Free;
    end;
  end;


  procedure TAPITests.KillTestAnnouncer;
  begin
    Note('Unannouncing: %s', [fTestAnnouncer.ServiceName]);

    fTestAnnouncer.Free;
  end;





initialization
  Smoketest.Add([TAPITests]);
end.
