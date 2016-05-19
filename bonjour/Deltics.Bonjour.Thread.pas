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

{$ifdef deltics_bonjour_thread}
  {$debuginfo ON}
{$else}
  {$debuginfo OFF}
{$endif}

  unit Deltics.Bonjour.Thread;


interface

  uses
  { vcl: }
    Classes,
    Messages,
    SyncObjs,
    Windows,
    WinSock,
  { deltics: }
    Deltics.Threads,
    Deltics.Bonjour.API;


  const
    BJM_FIRST = WM_USER;

    BJM_DomainFound         = BJM_FIRST + 0;
    BJM_DomainLost          = BJM_FIRST + 1;
    BJM_DomainEnumFinished  = BJM_FIRST + 2;

    BJM_ServiceFound        = BJM_FIRST + 10;
    BJM_ServiceLost         = BJM_FIRST + 11;
    BJM_ServiceResolved     = BJM_FIRST + 12;

    BJM_LAST = BJM_ServiceResolved;


  type
    TBonjourThread = class(TMotile)
    private
      fServices: TThreadList;
    protected
      procedure Execute; override;
    public
      constructor Create; reintroduce;
      destructor Destroy; override;
      class procedure AddService(const aService: TDNSServiceRef);
      class procedure RemoveService(const aService: TDNSServiceRef);
      class function IsServicing(const aService: TDNSServiceRef): Boolean;
      class procedure NotifyVCL(const aMessage);
      class procedure ProcessVCLMessages;
    end;


implementation

  uses
  { vcl: }
    SysUtils,
    Types,
  { deltics: }
    Deltics.Finalizer,
    Deltics.MessageHandler,
    Deltics.Bonjour.Domains,
    Deltics.Bonjour.Listener;

  type
    TDomainBrowserHelper = class(TDomainBrowser);
    TListenerHelper = class(TListener);
    TServiceInfoHelper = class(TServiceInfo);




  { ---------------------------------------------------------------------------------------------- }
  type
    TVCLMediator = class(TMessageHandler)
    {
      The TVCLMediator class creates a hidden window which receives messages from
       the Bonjour thread and invokes corresponding event handlers, thus synchronizing
       the invocation of those event handlers with the VCL main thread which is
       pumping the message queue.
    }
    protected
      procedure OnDomainEnumFinished(var aMessage: TDomainBrowserMessage); message BJM_DomainEnumFinished;
      procedure OnDomainFound(var aMessage: TDomainBrowserMessage); message BJM_DomainFound;
      procedure OnDomainLost(var aMessage: TDomainBrowserMessage); message BJM_DomainLost;
      procedure OnServiceFound(var aMessage: TListenerServiceMessage); message BJM_ServiceFound;
      procedure OnServiceLost(var aMessage: TListenerServiceMessage); message BJM_ServiceLost;
      procedure OnServiceResolved(var aMessage: TListenerServiceMessage); message BJM_ServiceResolved;
    end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnDomainFound(var aMessage: TDomainBrowserMessage);
  begin
    TDomainBrowserHelper(aMessage.Browser).DoOnDomainAdded(aMessage.Domain);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnDomainLost(var aMessage: TDomainBrowserMessage);
  begin
    TDomainBrowserHelper(aMessage.Browser).DoOnDomainRemoved(aMessage.Domain);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnDomainEnumFinished(var aMessage: TDomainBrowserMessage);
  begin
    TDomainBrowserHelper(aMessage.Browser).DoOnEnumFinished;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnServiceFound(var aMessage: TListenerServiceMessage);
  begin
    TListenerHelper(aMessage.Listener).DoServiceFound(aMessage.Service);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnServiceLost(var aMessage: TListenerServiceMessage);
  begin
    TListenerHelper(aMessage.Listener).DoServiceLost(aMessage.Service);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TVCLMediator.OnServiceResolved(var aMessage: TListenerServiceMessage);
  begin
    TServiceInfoHelper(aMessage.Service).DoResolved(aMessage.Service);
  end;





{ Unit Variables --------------------------------------------------------------------------------- }

  var
    Thread: TBonjourThread = NIL;
    VCl: TVCLMediator = NIL;




{ TBonjourThread --------------------------------------------------------------------------------- }

  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TBonjourThread.AddService(const aService: TDNSServiceRef);
  begin
    Thread.fServices.Add(Pointer(aService));
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TBonjourThread.RemoveService(const aService: TDNSServiceRef);
  begin
    Thread.fServices.Remove(Pointer(aService));
    DNSServiceRefDeallocate(aService);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class function TBonjourThread.IsServicing(const aService: TDNSServiceRef): Boolean;
  begin
    result := Thread.fServices.LockList.IndexOf(Pointer(aService)) <> -1;
    Thread.fServices.UnlockList;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TBonjourThread.NotifyVCL(const aMessage);
  var
    msg: TMessage absolute aMessage;
  begin
    VCL.PostMessage(msg.Msg, msg.wParam, msg.lParam);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  class procedure TBonjourThread.ProcessVCLMessages;
  begin
    if NOT InVCLThread then
      EXIT;

    VCL.ProcessMessages(BJM_FIRST, BJM_LAST);
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  constructor TBonjourThread.Create;
  begin
    inherited Create(weRunContinuously, tpLower);

    Name := 'Bonjour/ZeroConf';

    fServices := TThreadList.Create;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  destructor TBonjourThread.Destroy;
  var
    i: Integer;
    list: TList;
  begin
    list := fServices.LockList;
    try
      for i := 0 to Pred(list.Count) do
        DNSServiceRefDeallocate(TDNSServiceRef(list[i]));

    finally
      fServices.UnlockList;
    end;

    FreeAndNIL(fServices);

    inherited;
  end;


  { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
  procedure TBonjourThread.Execute;

    type
      TServiceSocket = record
        Service: TDNSServiceRef;
        Socket: Integer;
      end;

  var
    i: Integer;
    refs: TList;
    services: array of TServiceSocket;
    read: TFDSet;
    timeout: TTimeVal;
  begin
    try
      refs := fServices.LockList;
      try
        if refs.Count = 0 then
          EXIT;

        FD_ZERO(read);

        SetLength(services, refs.Count);
        for i := 0 to Pred(refs.Count) do
        begin
          services[i].Service := refs[i];
          services[i].Socket  := DNSServiceRefSockFD(services[i].Service);
          FD_SET(services[i].Socket, read);
        end;

        timeout.tv_sec  := 0;
        timeout.tv_usec := 0;

        if (select(refs.Count, @read, NIL, NIL, @timeout) > 0)
         and (read.fd_count > 0) then
        begin
          for i := 0 to Pred(refs.Count) do
            if FD_ISSET(services[i].Socket, read) then
              DNSServiceProcessResult(services[i].Service);
        end;

      finally
        fServices.UnlockList;

        Sleep(200);
      end;

    except
      on e: Exception do
        OutputDebugString(PChar('Bonjour thread exception: ' + e.Message));
    end;
  end;






{ Finalizer -------------------------------------------------------------------------------------- }

  procedure DoFinalization;
  begin
    Thread.Terminate;
    Thread.WaitFor;

    VCL.Free;
  end;





initialization
  if NOT BonjourInstalled then
    EXIT;

  VCL     := TVCLMediator.Create;
  Thread  := TBonjourThread.Create;

  RegisterFinalization(DoFinalization, 'Deltics.Bonjour.Thread');

end.
