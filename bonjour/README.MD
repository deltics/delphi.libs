# Deltics.Bonjour

Provides an encapsulation of Apple Bonjour Service Discovery framework for Windows including service advertisement, service listeners and Service Meta Query.


### Service Meta Query

A Service Meta Query is a specialised form of service listener.

While Bonjour listeners typically provide notifications for the appearance (and removal) of specific services on a network, a Service Meta Query allows an application to identify all (currently) advertised Service *Types*.

For example, if there are three computers all sharing an iTunes library on a network then a service meta query will identify the presence of the **local._daap._tcp._daap** service type, but not the individual computers on which those libraries may be found.

Also be aware that a Service Meta Query listener does *not* provide a mechanism to advertise when service types are removed from the network.

To identify specific services you must establish a listener for the specific service types.  A specific service listener *will* be notified when individual services are removed from the network.

For more information see the Apple [documentation](https://developer.apple.com/library/content/qa/qa1337/_index.html)


#### Implementing a Service Meta Query

Developers could implement their own Service Meta Query using a standard TListener, according to the documented requirements of a Service Meta Query listener (see the Apple documentation above).  Alternatively, the **Deltics.Bonjour** implementation provides a simplified mechanism, with an internal Meta Query listener which is activated simply by registering a handler to receive service meta query notifications.

Handlers are registered and unregistered via static methods on the **TListener** class.

The internal Service Meta Query provides the **domain** and **service type** of each discoverable service type on the network by calling back to registered handlers with these passed as parameters to the handler.

  * The internal Service Meta Query listener is established for the first handler that is registered and remains active, producing notifications for any new service types that appear, while at least one handler is registered.
  
  * If additional handlers are registered then notifications are *re-issued* for the currently known service types for *all* registered handlers.  This means that handlers should anticipate and allow for duplicate service type notifications.

  * Once all handlers are unregistered then the Service Meta Query listener is terminated.


#### Service Meta Query Example

The example below uses a registered meta query handler to populate a listbox with a simple list of all currently discoverable service types and domains:

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
    
