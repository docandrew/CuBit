pragma Ada_2022;
with System;
with System.Storage_Elements;
with CuAlloc_Native;
with CuBit.Monotonic;
with CuBit.Messages;
with CuBit.Handles.IPC;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with ACPI_Service;
with ACPI_Endpoint;
package body ACPI_Provider_Transport is
   use Interfaces;
   use ACPI_Requests;
   use ACPI_Snapshot_Provider;
   use type ACPI_Service.Table_Kind;
   package Grants renames CuBit.Memory_Grants;
   Page_Bytes : constant Unsigned_64 := Grants.PAGE_SIZE;
   Import_Label : constant Unsigned_32 := 8;
   Initialized, Attempted, Poisoned, Has_Grant : Boolean := False;
   Mutation_Stopped, Start_Sent, Finish_Sent : Boolean := False;
   type Interest_State is (Not_Requested, Awaiting, Consumed, Armed, Failed);
   Interest : Interest_State := Not_Requested;
   Interest_Error : Outcome := OK;
   Notifications_Ready : Boolean := False;
   Consumer : Notification_Policy.State;
   use type Notification_Policy.Result;
   function Last_Interest_Error return Outcome is (Interest_Error);
   function Upload_Allowed return Boolean is (Interest in Not_Requested | Armed);
   function Notification_Session return Notification_Policy.Word is
     (Notification_Policy.Session(Consumer));
   function Has_Notification return Boolean is
     (Notification_Policy.Has_Observation(Consumer));
   function Copy_Notification return Notification_Policy.Observation is
     (Notification_Policy.Copy_Observation(Consumer));
   Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   Windows : Deadlines;
   Count : Natural range 0 .. Max_Catalog_Tables := 0;
   Next_Table : Positive range 1 .. Max_Catalog_Tables + 1 := 1;
   Catalog : Table_Slices(1 .. Max_Catalog_Tables) :=
     [others => ((1,ACPI_Service.Description,1),0)];
   -- Static process-lifetime catalog and one aligned allocator-owned buffer.
   -- The allocator allocation is never freed, even on initialization failure.
   Saved : Firmware_Tables.Bytes(1 .. Max_Catalog_Bytes) := [others => 0];
   Buffer_Word : Unsigned_64 := 0;
   Buffer_Size : Natural := 0;
   Reference : Grants.Grant_Reference;
   Revoke_Result : Boolean := False;
   function Quarantined return Boolean is (Poisoned);
   function Revoke_Accepted return Boolean is (Revoke_Result);
   function Grant_Pending return Boolean is (Poisoned or else Has_Grant);
   function Round_Pages (Length : Positive) return Unsigned_64 is
     ((Unsigned_64(Length) + Page_Bytes - 1) / Page_Bytes * Page_Bytes);
   function Address return System.Address is
     (System.Storage_Elements.To_Address(System.Storage_Elements.Integer_Address(Buffer_Word)));
   function Deadline (Span : Window; Value : out Unsigned_64) return Boolean is
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      Value := 0;
      if Now >= Unsigned_64'Last - Span then return False; end if;
      Value := Now + Span; return True;
   end Deadline;
   procedure Initialize (Target : CuBit.Handles.Endpoint; Items : Table_Slices;
      Data : Firmware_Tables.Bytes; Limits : Deadlines; Accepted : out Boolean;
      Startup : Startup_Notifications := Disabled) is
      Largest : Positive := 1;
   begin
      Accepted := False;
      if Attempted then return; end if;
      Attempted := True;
      if not CuBit.Handles.Valid(Target) or else Items'Length=0 or else
        Items'Length>Max_Catalog_Tables or else Data'Length>Max_Catalog_Bytes
      then return; end if;
      for I in Items'Range loop
         if Items(I).Item.Length < Firmware_Tables.Table_Header_Size or else
           Items(I).Item.Length > Data'Length or else
           Items(I).Offset > Data'Length - Items(I).Item.Length
         then return; end if;
         for J in Items'First .. I-1 loop
            if Items(J).Item.ID=Items(I).Item.ID then return; end if;
         end loop;
         Largest := Positive'Max(Largest,Items(I).Item.Length);
      end loop;
      Buffer_Size := Natural(Round_Pages(Largest));
      Buffer_Word := CuAlloc_Native.Allocate_Zeroed(Unsigned_64(Buffer_Size),Page_Bytes);
      if Buffer_Word=0 or else Buffer_Word mod Page_Bytes/=0 or else
        CuAlloc_Native.Usable_Size(Buffer_Word)<Unsigned_64(Buffer_Size)
      then return; end if;
      Saved(1..Data'Length):=Data;
      for I in Items'Range loop Catalog(I-Items'First+1):=Items(I); end loop;
      Count:=Items'Length; Endpoint:=Target; Windows:=Limits; Initialized:=True;
      Interest := (if Startup=Required then Awaiting else Not_Requested);
      Accepted:=True;
   end Initialize;
   procedure Call (Request : Packet; At_Time : Unsigned_64;
      Reply : out Packet; Known : out Boolean) is
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Known:=False; Reply:=(others=><>);
      if Request.Length/=4 or else Request.Flags/=0 or else Request.Reserved/=0 then return; end if;
      M.tag := (label=>Request.Label,length=>Request.Length,
        protocol=>0,ipc=><>,reserved=>Request.Reserved);
      M.words:=CuBit.Messages.MessageWords(Request.Data);
      M.tag:=CuBit.Handles.IPC.Call(Endpoint,M,At_Time);
      if M.tag.label>=CuBit.Messages.Kernel_Reply_First then return; end if;
      Reply:=(M.tag.label,M.tag.length,CuBit.Messages.Flag_Byte(M.tag),M.tag.reserved,Words(M.words));
      Known:=True;
   end Call;
   function Envelope (Reply : Packet) return Boolean is
     (Reply.Length=4 and then Reply.Flags=0 and then Reply.Reserved=0 and then
      (for all V of Reply.Data => V<=Max_Revision));
   procedure Configure_Interest (Result : out Interest_Result) is
      At_Time : Unsigned_64;
      Reply : Packet;
      Known : Boolean;
      Version : Revision_Number;
      function Valid_OK return Boolean is
      begin
         if not Envelope(Reply) then Result:=Interest_Invalid_Response; return False; end if;
         if Reply.Label=ACPI_Endpoint.Reply_Error then
            if Reply.Data(0) not in 1..Outcome'Pos(Outcome'Last) or else
               Reply.Data(2)/=0 or else Reply.Data(3)/=0
            then Result:=Interest_Invalid_Response;
            else Interest_Error:=Outcome'Val(Reply.Data(0)); Result:=Interest_Remote_Rejected;
            end if;
            return False;
         end if;
         if Reply.Label/=ACPI_Endpoint.Reply_OK then
            Result:=Interest_Invalid_Response; return False;
         end if;
         return True;
      end Valid_OK;
   begin
      Result:=Interest_Not_Eligible;
      if not Initialized or else Interest/=Awaiting or else Start_Sent or else
        Mutation_Stopped or else Grant_Pending then return; end if;
      Interest:=Consumed; -- before any query/call, no retry of this attempt
      if not Deadline(Windows.Query_And_Start,At_Time) then
         Result:=Interest_Deadline_Unavailable;
      else
         Call((Label=>Read_Metrics,others=><>),At_Time,Reply,Known);
         if not Known then Result:=Interest_Unknown;
         elsif Valid_OK then
            if Reply.Data(1..3)/=[0,0,0] or else Reply.Data(0)=Max_Revision then
               Result:=Interest_Invalid_Response;
            else
               Version:=Reply.Data(0);
               if not Deadline(Windows.Query_And_Start,At_Time) then
                  Result:=Interest_Deadline_Unavailable;
               else
                  Call((Label=>Configure_Notifications,Data=>[Version,1,0,0],others=><>),At_Time,Reply,Known);
                  if not Known then Result:=Interest_Unknown;
                  elsif Valid_OK then
                     if Reply.Data=[Version+1,1,0,
                       Unsigned_64(Access_Outcome_Code'Enum_Rep(Access_Available_Code))]
                     then Interest:=Armed; Result:=Interest_Armed; return;
                     else Result:=Interest_Invalid_Response;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      Interest:=Failed; Mutation_Stopped:=True;
   end Configure_Interest;
   procedure Start_Notifications (Accepted : out Boolean) is
      Result : Notification_Policy.Result;
   begin
      Accepted:=False;
      if not Notifications_Ready or else Mutation_Stopped or else Grant_Pending then return; end if;
      Notification_Policy.Start_Session(Consumer,Result);
      Accepted:=Result=Notification_Policy.Applied;
   end Start_Notifications;
   procedure Advance_Notifications (Result : out Notification_Policy.Result) is
      Request,Reply : Packet;
      Token : Notification_Policy.Token;
      At_Time : Unsigned_64;
      Known : Boolean;
   begin
      Result:=Notification_Policy.Idle;
      if not Notifications_Ready or else Mutation_Stopped or else Grant_Pending then return; end if;
      Notification_Policy.Next_Request(Consumer,Request,Token,Result);
      if Result/=Notification_Policy.Ready then return; end if;
      -- Only packets created by the private policy, never arbitrary caller input.
      if Request.Label not in Read_Notification_State | Claim_Notification |
        Read_Notification | Acknowledge_Notification
      then raise Program_Error; end if;
      if not Deadline(Windows.Query_And_Start,At_Time) then
         Notification_Policy.Transport_Unknown(Consumer,Token,Result); return;
      end if;
      Call(Request,At_Time,Reply,Known);
      if Known then Notification_Policy.Accept_Reply(Consumer,Token,Reply,Result);
      else Notification_Policy.Transport_Unknown(Consumer,Token,Result);
      end if;
      -- Unknown receipt outcomes retain the policy's exact receipt retry state.
      -- They never clear or reopen upload Mutation_Stopped/Start_Sent/Finish_Sent.
   end Advance_Notifications;
   procedure Confirm_Notification_Delivery
     (Session : Notification_Policy.Word; ID : Notification_Policy.Receipt;
      Result : out Notification_Policy.Result) is
   begin
      Result:=Notification_Policy.Invalid_Delivery;
      if not Notifications_Ready or else Mutation_Stopped then return; end if;
      Notification_Policy.Confirm_Delivery(Consumer,Session,ID,Result);
   end Confirm_Notification_Delivery;
   procedure Exchange (Request : Packet; Reply : out Packet; Known : out Boolean) is
      At_Time : Unsigned_64;
   begin
      Known:=False; Reply:=(others=><>);
      if not Initialized then return; end if;
      if Request.Label/=Read_Metrics then
         if Mutation_Stopped or else Grant_Pending or else not Upload_Allowed then return; end if;
         if Request.Label=Start_Snapshot then
            if Start_Sent then return; end if;
         elsif Request.Label=Finish_Snapshot then
            if Finish_Sent or else Next_Table/=Count+1 then return; end if;
         else return;
         end if;
      end if;
      if not Deadline((if Request.Label=Finish_Snapshot then Windows.Finish
                       else Windows.Query_And_Start),At_Time) then
         if Request.Label/=Read_Metrics then Mutation_Stopped:=True; end if;
         return;
      end if;
      if Request.Label=Start_Snapshot then Start_Sent:=True; end if;
      if Request.Label=Finish_Snapshot then Finish_Sent:=True; end if;
      Call(Request,At_Time,Reply,Known);
      if not Known and then Request.Label/=Read_Metrics then Mutation_Stopped:=True; end if;
      if Request.Label=Finish_Snapshot and then Known and then Interest=Armed and then
        Envelope(Reply) and then Reply.Label=ACPI_Endpoint.Reply_OK and then
        Request.Data(0)<Max_Revision and then Reply.Data=[Request.Data(0)+1,0,0,0]
      then Notifications_Ready:=True; end if;
   end Exchange;
   procedure Release_Provider_Grant (Released : out Boolean) is
   begin
      Released:=False;
      if Poisoned then return; end if;
      if not Has_Grant then Released:=True; return; end if;
      Grants.Revoke(Reference,Revoke_Result);
      -- Accepted revoke is not retirement. The query is authoritative even
      -- after a refused repeated revoke of an already retired reference.
      if Grants.Retirement_Confirmed(Reference) then
         Has_Grant:=False; Released:=True;
      end if;
   end Release_Provider_Grant;
   procedure Import_Table (Index : Positive; Token : ACPI_Snapshot_Provider.Revision;
      Item : Table_Descriptor; Reply : out Packet; Known : out Boolean;
      Cleanup : out Cleanup_State) is
      At_Time : Unsigned_64;
      Created, Released : Boolean;
      Actual : Grants.Grant_Reference;
   begin
      Known:=False; Reply:=(others=><>);
      Cleanup := (if Grant_Pending then Uncertain else Clear);
      if not Initialized or else Mutation_Stopped or else not Upload_Allowed or else Grant_Pending or else
        not Start_Sent or else Finish_Sent or else Index/=Next_Table or else Index>Count
      then return; end if;
      if Item.ID/=Catalog(Index).Item.ID or else Item.Kind/=Catalog(Index).Item.Kind or else
        Item.Length/=Catalog(Index).Item.Length then Mutation_Stopped:=True; return; end if;
      if not Deadline(Windows.Import_Table,At_Time) then Mutation_Stopped:=True; return; end if;
      declare
         Buffer : Firmware_Tables.Bytes(1..Buffer_Size) with Import,Address=>Address;
      begin
         Buffer := [others=>0];
         Buffer(1..Item.Length):=Saved(Catalog(Index).Offset+1..Catalog(Index).Offset+Item.Length);
      end;
      Grants.Create_Via_Capability(Endpoint,Address,Natural(Round_Pages(Item.Length)/Page_Bytes),
        False,Actual,Created,False);
      if not Created then
         -- False may hide successful raw creation and failed generation query.
         -- Never inspect/use Actual's default value; backing remains forever.
         Poisoned:=True; Mutation_Stopped:=True; Cleanup:=Uncertain; return;
      end if;
      Reference:=Actual; Has_Grant:=True; Next_Table:=Next_Table+1;
      Call((Label=>Import_Label,Data=>[Token,CuBit.Grant_References.Encode(Reference),
        Unsigned_64(Item.ID),Unsigned_64(ACPI_Service.Table_Kind'Pos(Item.Kind))*CuBit.Grant_References.Wire_Field_Base+Unsigned_64(Item.Length)],others=><>),
        At_Time,Reply,Known);
      if not Known then Mutation_Stopped:=True; end if;
      Release_Provider_Grant(Released);
      Cleanup:=(if Released then Clear else Pending);
   end Import_Table;
end ACPI_Provider_Transport;
