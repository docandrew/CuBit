pragma Ada_2022;
with AML_Identity.Issuer;
with AML_Decode;
package body ACPI_Field_NVS_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   use type AML_Decode.Byte;
   use type A.Counter, A.Endpoint_ID, AML_Identity.Identity;
   use type L.Register_Geometry, Client.Phase, Client.Outcome, P.Wire.Delivery;
   Provider_Epoch : constant A.Counter := 1;
   procedure Exchange_Tracked
     (Channel : in out Channel_State; Destination : Peer; Deadline : P.Wire.Time_Instant;
      Item : P.Packet; Reply : out P.Packet; Delivered : out P.Wire.Delivery) is
   begin
      Exchange (Channel.Inner, Destination, Deadline, Item, Reply, Delivered);
      Channel.Delivered := Delivered;
   end Exchange_Tracked;
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Pending (IO : Context) return Boolean is (IO.Working or else IO.Captured.Present);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Live (IO : Context) return Boolean is
     (IO.Stage = Ready and then not Pending (IO) and then Client.Observe (IO.Lower).Stage = Client.Ready);
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : A.Endpoint_ID;
      Deadline : P.Wire.Time_Instant; Accepted : out Boolean) is
      Result : Client.Outcome;
   begin
      Accepted := False;
      if IO.Stage /= Fresh or else IO.Working then return; end if;
      IO.Stage := Provisioning; IO.Working := True;
      if Symbol = A.No_Endpoint then IO.Stage := Failed; IO.Working := False; return; end if;
      Client.Load (IO.Lower, IO.Channel, Destination, Deadline, Result);
      if Result /= Client.Loaded then IO.Stage := Failed; IO.Working := False; return; end if;
      AML_Identity.Issuer.Issue (IO.Issuer, Accepted);
      if not Accepted then IO.Stage := Failed; IO.Working := False; return; end if;
      IO.Symbol := Symbol; IO.Stage := Ready; IO.Working := False;
   end Initialize;
   procedure Install
     (IO : in out Context; Geometry : L.Register_Geometry; Target : out A.Target; Accepted : out Boolean) is
      Bytes : constant A.Counter := A.Counter (P.T.Octets (Geometry.Width));
      Record_Item : Client.Description;
      Available : Boolean;
      Match, Offset : Client.Word := 0;
   begin
      Target := A.No_Target; Accepted := False;
      if not Live (IO) or else Geometry.Region.Space /= L.Memory_Space
        or else Geometry.Region.Length = 0
        or else Geometry.Region.Length - 1 > A.Counter'Last - Geometry.Region.Base
        or else Geometry.Byte_Offset >= Geometry.Region.Length
        or else Bytes > Geometry.Region.Length - Geometry.Byte_Offset
        or else (Geometry.Region.Base + Geometry.Byte_Offset) mod Bytes /= 0 then return; end if;
      for I in 1 .. Client.Observe (IO.Lower).Count loop
         Client.Describe (IO.Lower, I, Record_Item, Available);
         if not Available then return; end if;
         if Geometry.Region.Base >= Record_Item.Base
           and then Geometry.Region.Base - Record_Item.Base < Record_Item.Length
           and then Geometry.Region.Length <= Record_Item.Length - (Geometry.Region.Base - Record_Item.Base)
         then
            if Match /= 0 then return; end if;
            Match := Record_Item.ID; Offset := Geometry.Region.Base - Record_Item.Base + Geometry.Byte_Offset;
         end if;
      end loop;
      if Match = 0 then return; end if;
      for I in 1 .. IO.Count loop
         if IO.Items (I).Geometry = Geometry then
            Target := (IO.Issuer, IO.Symbol, Provider_Epoch, A.Register_ID (I)); Accepted := True; return;
         end if;
      end loop;
      if IO.Count = Maximum_Targets then return; end if;
      IO.Count := IO.Count + 1; IO.Items (IO.Count) := (Geometry, Match, Offset);
      Target := (IO.Issuer, IO.Symbol, Provider_Epoch, A.Register_ID (IO.Count)); Accepted := True;
   end Install;
   function Matches (IO : Context; Target : A.Target) return Boolean is
     (Target.Provider = IO.Issuer and then IO.Issuer /= AML_Identity.No_Identity
      and then Target.Endpoint = IO.Symbol and then Target.Epoch = Provider_Epoch
      and then Target.Register in 1 .. A.Register_ID (IO.Count));
   procedure Describe
     (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := Live (IO) and then Matches (IO, Target);
      if Accepted then Geometry := IO.Items (Target_Index (Target.Register)).Geometry; end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := not For_Write and then Live (IO) and then Matches (IO, Target)
     and then IO.Issued < A.Counter'Last; end Authorize;
   procedure Describe_Scoped_Read
     (IO : Context; Target : A.Target;
      ID : out ACPI_Locked_NVS_Protocol.Record_ID;
      Datum : out ACPI_Locked_NVS_Protocol.Geometry; Accepted : out Boolean) is
      package D renames ACPI_Locked_NVS_Protocol;
   begin
      ID := D.No_Record; Datum := D.Empty_Geometry;
      Authorize (IO, Target, False, Accepted);
      if not Accepted then return; end if;
      declare Row : Association renames IO.Items (Target_Index (Target.Register)); begin
         ID := D.Record_ID (Row.Record_ID); Datum := (Row.Offset, Row.Geometry.Width);
         Accepted := D.Valid (Datum);
         if not Accepted then ID := D.No_Record; Datum := D.Empty_Geometry; end if;
      end;
   end Describe_Scoped_Read;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted : Boolean;
      Slot : Target_Index;
      Result : Client.Outcome;
      Value : Client.Word;
      Deadline : P.Wire.Time_Instant;
   begin
      Reply := (others => <>); Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted or else Item.Value /= 0 then return; end if;
      Slot := Target_Index (Item.Destination.Register);
      IO.Working := True; IO.Issued := IO.Issued + 1;
      IO.Channel.Delivered := P.Wire.Not_Dispatched;
      Read_Deadline (Deadline);
      Client.Read (IO.Lower, IO.Channel, IO.Items (Slot).Record_ID, IO.Items (Slot).Offset,
        IO.Items (Slot).Geometry.Width, Deadline, Value, Result);
      IO.Working := False;
      if Result = Client.Read_Complete then
         Reply := (A.Completed, IO.Issuer, Value, A.No_Operation);
      elsif Result in Client.Completion_Unknown | Client.Bad_Reply then
         IO.Captured := (True, IO.Issuer, Item, A.Operation_ID (IO.Issued)); IO.Stage := Failed;
         Reply := (A.Indeterminate, IO.Issuer, 0, IO.Captured.Operation);
      elsif IO.Channel.Delivered = P.Wire.Reply_Received then
         IO.Fault := (True, IO.Issuer, Item, A.No_Operation); IO.Stage := Failed;
         Reply := (A.Failed_Completed, IO.Issuer, 0, A.No_Operation);
      elsif Client.Observe (IO.Lower).Stage /= Client.Ready then IO.Stage := Failed;
      end if;
   end Dispatch;
   procedure Reconciled (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Reconciled;
end ACPI_Field_NVS_Provider;
