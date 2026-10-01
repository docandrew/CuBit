package body Intel_GPU_Render_Control with SPARK_Mode is
   package Sessions renames Intel_GPU_Render_Sessions;
   procedure Bind
     (Object : in out Controller; Broker, Broker_Tag : Unsigned_64) is
   begin
      if Object.Bound then return; end if;
      -- Even invalid bootstrap input consumes the one binding attempt.
      Object.Bound := True;
      if Broker = 0 or else Broker_Tag = 0 or else
        Broker_Tag in Sessions.Tag_Base + 1 .. Sessions.Tag_Base + Sessions.Capacity
      then return; end if;
      Object.Broker := Broker;
      Object.Broker_Tag := Broker_Tag;
   end Bind;
   procedure Handle
     (Object : in out Controller; Sender, Stamped_Tag : Unsigned_64;
      Ready : Boolean; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Response : out Words) is
      Recipient : constant Unsigned_64 := Request (1);
      PID : constant Unsigned_64 := Recipient mod 2 ** 32;
      Tag : Unsigned_64 := Request (2);
      Accepted : Boolean;
   begin
      Response := [Denied, Version, 0, 0];
      if Object.Broker = 0 or Sender /= Object.Broker or
        Stamped_Tag /= Object.Broker_Tag then return; end if;
      Response (0) := Bad_Request;
      if Request_Label /= Label or Length /= 4 or Flags /= 0 or Reserved /= 0 or
        Request (0) /= Version or PID = 0 or Recipient / 2 ** 32 = 0 or
        Request (3) > Abort_Session then return; end if;
      if Request (3) = Reserve then
         if Tag /= 0 then return; end if;
         Response (0) := Unavailable;
         if not Ready then return; end if;
         Sessions.Reserve (Object.Sessions, PID, Tag);
         if Tag = 0 then return; end if;
         Object.Recipients (Positive (Tag - Sessions.Tag_Base)) := Recipient;
      else
         if Tag <= Sessions.Tag_Base or Tag > Sessions.Tag_Base + Sessions.Capacity
         then return; end if;
         Response (0) := Bad_State;
         if Object.Recipients (Positive (Tag - Sessions.Tag_Base)) /= Recipient
         then return; end if;
         if Request (3) = Activate then
            Response (0) := Unavailable;
            if not Ready then return; end if;
            Sessions.Finalize (Object.Sessions, PID, Tag, True, Accepted);
            Response (0) := Bad_State;
            if not Accepted then return; end if;
         else
            Sessions.Close (Object.Sessions, PID, Tag);
         end if;
      end if;
      Response := [OK, Version, Tag, 0];
   end Handle;
   function Resolve
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
     (Sessions.Resolve (Object.Sessions, Sender, Stamped_Tag));
   function Recipient_Identity
     (Object : Controller; Sender, Stamped_Tag : Unsigned_64) return Unsigned_64 is
      Tag : constant Unsigned_64 := Resolve (Object, Sender, Stamped_Tag);
   begin
      if Tag = 0 then return 0; end if;
      return Object.Recipients (Positive (Tag - Sessions.Tag_Base));
   end Recipient_Identity;
   procedure Reject_Delivery
     (Object : in out Controller; Identity, Tag : Unsigned_64) is
   begin
      if Tag <= Sessions.Tag_Base or else Tag > Sessions.Tag_Base + Sessions.Capacity
        or else Identity = 0 then return; end if;
      if Object.Recipients (Positive (Tag - Sessions.Tag_Base)) = Identity then
         Sessions.Close (Object.Sessions, Identity mod 2 ** 32, Tag);
      end if;
   end Reject_Delivery;
   procedure Quarantine (Object : in out Controller) is
   begin
      Sessions.Quarantine (Object.Sessions);
   end Quarantine;
end Intel_GPU_Render_Control;
