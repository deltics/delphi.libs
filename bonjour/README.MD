### Service Meta Query

Service Meta Queries allow an application to identify all currently discoverable Bonjour Service Types 
on a network.

Service Meta Queries identify Service *Types*, rather than actual services.  For example, if there
are three computers all sharing an iTunes library on a network then a service meta query will identify the
presence of the **local._daap._tcp._daap** service type, but not the individual computers on which those
libraries may be found.

Also be aware that a Service Meta Query will identify currently discoverable service types but does *not*
provide a mechanism to advertise when service types are removed from the network.

To identify specific services you must establish a listener for the specifically supported service types 
and resolve the services themselves.  A specific service listener *will* be notified when specific services
are removed from the network.

For more information see the Apple [documentation](https://developer.apple.com/library/content/qa/qa1337/_index.html)


#### Example of Service Meta Query

In this Bonjour implementation, an application registers a handler to receive Service Meta Query notifications.  
The notification provides the **domain** and **service type** of each discoverable service type on the
network by calling back to registered handlers with these passed as parameters to the handler.

For example, to populate a listbox with a simple list of all service types and domains for known service types:

    TMyForm = class(TForm)
      ..
      lbServiceTypes: TListbox;
      ..
      procedure FormCreate(Sender: TObject);
      procedure OnServiceMeta(const aDomain, aServiceType: String);
      ..
    end;

    implementation
    
      uses
        Deltics.Bonjour.Listener;


      procedure TMyForm.FormCreate(Sender: TObject);
      begin
        TListener.RegisterServiceMetaQueryHandler(OnServiceMeta);
      end;


      procedure TMyForm.FormDestroy(Sender: TObject);
      begin
        TListener.UnregisterServiceMetaQueryHandler(OnServiceMeta);
      end;


      procedure TMyForm.OnServiceMeta(const aDomain, aServiceType: String);
      var
        descriptor: String;
      begin
        descriptor := aServiceType + aDomain;
        if lbServiceTypes.Items.IndexOf(descriptor) = -1 then
          lbServiceTypes.Items.Add(descriptor);
      end;

    end.
    
A Service Meta Query listener is established for the first handler that is registered.  If additional handlers
are registered then notifications are *re-issued* for the currently known service types for all registered
handlers.

As illustrated above, handlers should anticipate and allow for duplicate notifications.

While any one or more handlers are registered, further notifications are then produced if/when any new service types 
appear on the network.

Once all handlers are unregistered then the Service Meta Query listener is terminated.
