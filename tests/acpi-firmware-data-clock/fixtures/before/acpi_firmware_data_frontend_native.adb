pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with ACPI_Register_Roles;
with ACPI_Native_Startup_Timing;
package body ACPI_Firmware_Data_Frontend_Native is
   package H renames CuBit.Handles;
   package Calls renames H.IPC;
   package M renames CuBit.Messages;
   package F renames P.F;
   use type H.Endpoint, H.ABI.Kind, Calls.Carried_Kind, IPC.Time_Instant;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8;
   Maximum_Diagnostic_Records : constant Positive := 8;
   subtype Diagnostic_Index is Positive range 1 .. Maximum_Diagnostic_Records;
   type Diagnostic_Record is record
      Label : Interfaces.Unsigned_32 := 0;
      Now, Deadline : IPC.Time_Instant := 0;
      Dispatched, Kernel_Error, Carried : Boolean := False;
      Reply_Label : Interfaces.Unsigned_32 := 0;
      Reply_Length : Interfaces.Unsigned_8 := 0;
      Raw_Status, Raw_Value : Interfaces.Unsigned_64 := 0;
   end record;
   Trace : array (Diagnostic_Index) of Diagnostic_Record;
   Next_Record : Diagnostic_Index := Diagnostic_Index'First;
   Recorded : Natural range 0 .. Maximum_Diagnostic_Records := 0;
   procedure Remember (Item : Diagnostic_Record) is
   begin
      Trace (Next_Record) := Item;
      if Next_Record = Diagnostic_Index'Last then Next_Record := Diagnostic_Index'First;
      else Next_Record := Next_Record+1; end if;
      if Recorded < Maximum_Diagnostic_Records then Recorded := Recorded+1; end if;
   end Remember;
   function Hex (Value : Interfaces.Unsigned_64) return String is
      Hex_Characters : constant String := "0123456789ABCDEF";
      Text : String (1 .. 16); Remaining : Interfaces.Unsigned_64 := Value;
   begin
      for I in reverse Text'Range loop Text (I) := Hex_Characters (Natural (Remaining mod 16)+1); Remaining := Remaining/16; end loop;
      return "0x" & Text;
   end Hex;
   function Bit (Value : Boolean) return String is (if Value then "1" else "0");
   function Diagnostic_Line (Ordinal : Positive) return String is
      Index : Diagnostic_Index;
   begin
      if Ordinal > Recorded then return ""; end if;
      Index := Diagnostic_Index (((Natural (Next_Record)-1+Maximum_Diagnostic_Records-Recorded+Ordinal-1)
         mod Maximum_Diagnostic_Records)+1);
      declare Item : constant Diagnostic_Record := Trace (Index); begin
         return "label=" & Hex (Interfaces.Unsigned_64 (Item.Label)) &
           " now-ms=" & Item.Now'Image & " until-ms=" & Item.Deadline'Image &
           " left-ms=" & IPC.Time_Instant'Image (if Item.Deadline > Item.Now then Item.Deadline-Item.Now else 0) &
           " dispatched=" & Bit (Item.Dispatched) & " kernel-error=" & Bit (Item.Kernel_Error) &
           " reply=" & Hex (Interfaces.Unsigned_64 (Item.Reply_Label)) & " words=" & Item.Reply_Length'Image &
           " raw-status=" & Hex (Item.Raw_Status) & " raw-value=" & Hex (Item.Raw_Value) & " carried=" & Bit (Item.Carried);
      end;
   end Diagnostic_Line;
   procedure Initialize (Connection : in out Channel; Target : H.Endpoint; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Connection.Consumed then return; end if;
      Connection.Consumed := True;
      if Target = H.No_Endpoint then return; end if;
      declare D : constant Calls.Description := Calls.Describe (Target); begin
         if not D.Current or else D.Kind /= H.ABI.Endpoint or else not D.Held.Read
           or else D.Authority_Tag /= Interfaces.Unsigned_64 (ACPI_Register_Roles.Frontend_Tag)
         then return; end if;
      end;
      Connection.Endpoint := Target; Connection.Ready := True; Accepted := True;
   end Initialize;
   procedure Clock (Now : out IPC.Time_Instant; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Now := 0; Available := Sample.Available;
      if Available then Now := IPC.Time_Instant (Sample.Microseconds / F.Microseconds_Per_Millisecond); end if;
   end Clock;
   procedure Exchange (Connection : in out Channel; Deadline : IPC.Time_Instant;
      Request : IPC.Packet; Reply : out IPC.Packet;
      Received : out Calls.Carried; Delivered : out IPC.Delivery) is
      Message : M.Message := M.NULL_MESSAGE;
      Now : IPC.Time_Instant;
      Available : Boolean;
      Timing : ACPI_Native_Startup_Timing.Token;
      Fact : Diagnostic_Record;
   begin
      Reply := (Length => 0,others => <>); Received := (Kind => Calls.Absent);
      Delivered := IPC.Not_Dispatched;
      if not Connection.Ready or else Connection.Working or else Deadline = 0
        or else Deadline = IPC.No_Finite_Deadline or else not F.Decode (Request,False).Valid then return; end if;
      Clock (Now,Available);
      Fact.Label := Request.Label; Fact.Now := Now; Fact.Deadline := Deadline;
      if not Available or else Now >= Deadline then Remember (Fact); return; end if;
      Message.tag := (label => Request.Label,length => Request.Length,protocol => 0,ipc => <>,reserved => 0);
      Message.words := M.MessageWords (Request.Data);
      Connection.Working := True; Delivered := IPC.Completion_Unknown;
      ACPI_Native_Startup_Timing.Enter
        (ACPI_Native_Startup_Timing.P.Firmware_Data_RPC, Timing);
      Message.tag := Calls.Call_Accepting (Connection.Endpoint,Message,Interfaces.Unsigned_64 (Deadline),Received);
      ACPI_Native_Startup_Timing.Leave (Timing);
      Connection.Working := False;
      Fact.Dispatched := True; Fact.Kernel_Error := Message.tag.label >= M.Kernel_Reply_First;
      Fact.Reply_Label := Message.tag.label; Fact.Reply_Length := Message.tag.length;
      Fact.Carried := Received.Kind = Calls.Present;
      -- Raw wire words are diagnostics, not a validated read completion.
      if not Fact.Kernel_Error and then Message.tag.length = 4 then
         Fact.Raw_Status := Message.words (2); Fact.Raw_Value := Message.words (3);
      end if;
      Remember (Fact);
      -- Preserve any received handle even when the kernel reports an error.
      if Message.tag.label >= M.Kernel_Reply_First then return; end if;
      Reply := (Message.tag.label,Message.tag.length,M.Flag_Byte (Message.tag),
         Message.tag.reserved,IPC.Words (Message.words));
      Delivered := IPC.Reply_Received;
   end Exchange;
   function Present (Item : Calls.Carried) return Boolean is (Item.Kind = Calls.Present);
   procedure Dispose (Item : in out Calls.Carried; Closed : out Boolean) is
   begin
      Closed := True;
      if Present (Item) then
         declare Result : constant Calls.Close_Result := Close_Carried (Item); begin Closed := Result.Closed; end;
      end if;
      if Closed then Item := (Kind => Calls.Absent); end if;
   end Dispose;
end ACPI_Firmware_Data_Frontend_Native;
