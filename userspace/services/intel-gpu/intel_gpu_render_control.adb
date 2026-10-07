package body Intel_GPU_Render_Control with SPARK_Mode is
   package Sessions renames Intel_GPU_Render_Sessions;
   function Storage_Index (Object : Controller; Tag : Unsigned_64)
                          return Sessions.Slot_Index is
     (Sessions.Storage_Index (Object.Sessions, Tag));
   function Issued_Tag (Object : Controller; Index : Sessions.Slot_Index)
                        return Unsigned_64 is
     (Sessions.Issued_Tag (Object.Sessions, Index));
   function Stored_Recipient_Slot
     (Object : Controller; Tag : Unsigned_64) return Unsigned_64 is
      Index : constant Sessions.Slot_Index := Storage_Index (Object, Tag);
   begin
      if Index = 0 or else Object.Recipient_Slots (Index) not in 40 .. 55 then
         return 0;
      end if;
      return Object.Recipient_Slots (Index);
   end Stored_Recipient_Slot;
   function Is_Broker
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Boolean is
     (Object.Broker /= 0 and then Sender = Object.Broker and then
      Stamped_Tag = Object.Broker_Tag);
   function Session_Status
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64; Ready : Boolean;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Words is
   begin
      if Resolve (Object, Sender, Stamped_Tag) = 0 then
         return [Denied, Version, 0, 0];
      elsif Request_Label /= Status_Label or else Length /= 4 or else
        Flags /= 0 or else Reserved /= 0 or else Request /= [Version, 0, 0, 0]
      then
         return [Bad_Request, Version, 0, 0];
      else
         return [(if Ready then OK else Unavailable), Version, 0, 0];
      end if;
   end Session_Status;
   procedure Bind
     (Object : in out Controller; Broker, Broker_Tag : Unsigned_64) is
   begin
      if Object.Bound then return; end if;
      -- Even invalid bootstrap input consumes the one binding attempt.
      Object.Bound := True;
      if Broker = 0 or else Broker_Tag = 0 or else
        Broker_Tag in Sessions.Tag_Base + 1 .. Sessions.Tag_Last
      then return; end if;
      Object.Broker := Broker;
      Object.Broker_Tag := Broker_Tag;
   end Bind;
   procedure Handle
     (Object : in out Controller; Sender, Stamped_Tag : Unsigned_64;
      Ready : Boolean; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words;
      Recipient_Ready : Boolean := False) is
      Recipient : constant Unsigned_64 := Request (1);
      PID : constant Unsigned_64 := Recipient mod 2 ** 32;
      Tag : Unsigned_64 := Request (2);
      Index : Sessions.Slot_Index;
      Accepted : Boolean;
   begin
      Response := [Denied, Version, 0, 0];
      if not Is_Broker (Object, Sender, Stamped_Tag) then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Label or Length /= 4 or Flags /= 0 or Reserved /= 0 or
        Request (0) /= Version or PID = 0 or Recipient / 2 ** 32 = 0 or
        Request (3) > Abort_Session then return; end if;
      if Request (3) = Reserve then
         if Tag /= 0 then return; end if;
         Response (0) := Unavailable;
         if not Ready or else Object.Next_Recipient_Slot = 56 then return; end if;
         Sessions.Reserve (Object.Sessions, PID, Tag);
         if Tag = 0 then return; end if;
         Index := Sessions.Storage_Index (Object.Sessions, Tag);
         if Index = 0 then return; end if;
         Object.Recipients (Index) := Recipient;
         Object.Recipient_Slots (Index) := Object.Next_Recipient_Slot;
         Object.Next_Recipient_Slot := Object.Next_Recipient_Slot + 1;
      else
         if Tag <= Sessions.Tag_Base or Tag > Sessions.Tag_Last
         then return; end if;
         Response (0) := Bad_State;
         Index := Sessions.Storage_Index (Object.Sessions, Tag);
         if Index = 0 or else Object.Recipients (Index) /= Recipient
         then return; end if;
         if Request (3) = Activate then
            Response (0) := Unavailable;
            if not Ready or else not Recipient_Ready then return; end if;
            Sessions.Finalize (Object.Sessions, PID, Tag, True, Accepted);
            Response (0) := Bad_State;
            if not Accepted then return; end if;
         else
            Sessions.Close (Object.Sessions, PID, Tag);
         end if;
      end if;
      Response := [OK, Version, Tag,
        (if Request (3) = Reserve then Stored_Recipient_Slot (Object, Tag) else 0)];
   end Handle;
   function Activation_Identity
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Unsigned_64 is
      Tag : constant Unsigned_64 := Request (2);
      Index : constant Sessions.Slot_Index := Sessions.Storage_Index (Object.Sessions, Tag);
   begin
      if Object.Broker = 0 or else Sender /= Object.Broker or else
        Stamped_Tag /= Object.Broker_Tag or else Request_Label /= Label or else
        Length /= 4 or else Flags /= 0 or else Reserved /= 0 or else
        Request (0) /= Version or else Request (3) /= Activate or else
        Index = 0
      then return 0; end if;
      if Request (1) = Object.Recipients (Index)
      then return Request (1); end if;
      return 0;
   end Activation_Identity;
   function Resolve
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
     (Sessions.Resolve (Object.Sessions, Sender, Stamped_Tag));
   function Resolve_Retired
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
     (Sessions.Resolve_Retired (Object.Sessions, Sender, Stamped_Tag));
   procedure Close_Own
     (Object : in out Controller; Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words; Response : out Words) is
      Tag : constant Unsigned_64 := Resolve (Object, Sender, Stamped_Tag);
   begin
      Response := [Denied, Version, 0, 0];
      if Tag = 0 then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Close_Own_Label or Length /= 4 or Flags /= 0 or
        Reserved /= 0 or Request /= [Version, 0, 0, 0] then return; end if;
      Sessions.Close (Object.Sessions, Sender, Tag);
      Response := [OK, Version, Tag, 0];
   end Close_Own;
   function Recipient_Identity
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
      Tag : constant Unsigned_64 := Resolve (Object, Sender, Stamped_Tag);
      Index : constant Sessions.Slot_Index := Sessions.Storage_Index (Object.Sessions, Tag);
   begin
      if Tag = 0 or else Index = 0 then return 0; end if;
      return Object.Recipients (Index);
   end Recipient_Identity;
   procedure Reject_Delivery
     (Object : in out Controller; Identity, Tag : Unsigned_64) is
      Index : constant Sessions.Slot_Index := Sessions.Storage_Index (Object.Sessions, Tag);
   begin
      if Index = 0 or else Identity = 0 then return; end if;
      if Object.Recipients (Index) = Identity then
         Sessions.Close (Object.Sessions, Identity mod 2 ** 32, Tag);
      end if;
   end Reject_Delivery;
   procedure Quarantine (Object : in out Controller) is
   begin
      Sessions.Quarantine (Object.Sessions);
   end Quarantine;
end Intel_GPU_Render_Control;
