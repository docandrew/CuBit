package body Intel_GPU_Buffer_Requests.Images is
   package H renames Intel_GPU_Buffer_Handles;
   package L renames Intel_GPU_Image_Lease;
   procedure Prepare
     (Object : in out Service; Sender, Stamp : Unsigned_64;
      Key : L.Identity; Image : Intel_GPU_Image_Layout.Descriptor;
      Lease : in out L.Lease; Accepted : out Boolean) is
      Session : constant Unsigned_64 := Session_Of (Sender, Stamp);
      Source : H.Retained_Reference;
      Pinned, Returned : Boolean;
      use type L.State;
   begin
      Accepted := False;
      if Object.Failed or else not Owner_Ready or else Session = 0 or else
        Key.Session /= Session or else Key.Allocation = 0 or else
        Key.Allocation > Unsigned_64 (H.Handle'Last) or else
        L.Current (Lease) /= L.Empty
      then return; end if;
      if not Authorize (Key, Image) or else
        not Producer_Drained (Session, Key.Allocation) or else
        not Authorize (Key, Image)
      then return; end if;
      -- A callback may observe closure/replacement. Never pin using a stale
      -- previously authenticated identity after that boundary.
      if Object.Failed or else not Owner_Ready or else
        Session_Of (Sender, Stamp) /= Session
      then return; end if;
      H.Retain_Backing (Object.Handles, Session, H.Handle (Key.Allocation), Source, Pinned);
      if not Pinned then return; end if;
      L.Prepare (Lease, Object.Handles, Source, Key, Image, True, True, Accepted);
      -- This temporary pin never exposed a mapping or GPU consumer.
      H.Return_Reference (Object.Handles, Source, True, Returned);
      if not Returned then
         H.Quarantine (Object.Handles);
         Object.Failed := True;
         Accepted := False;
      end if;
   end Prepare;
   function Backing
     (Object : Service; Lease : L.Lease; Key : L.Identity)
      return Intel_GPU_Buffer_Reply.Backing is
     (if Object.Failed or else not Owner_Ready then (Ready => False)
      else L.Backing (Lease, Object.Handles, Key));
   procedure Retire
     (Object : in out Service; Lease : in out L.Lease;
      Key : L.Identity; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Consumers_Drained (Key) then return; end if;
      L.Retire (Lease, Object.Handles, Key, True, True, True, Accepted);
   end Retire;
end Intel_GPU_Buffer_Requests.Images;
