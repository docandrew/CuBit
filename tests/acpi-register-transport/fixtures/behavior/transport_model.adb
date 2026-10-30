pragma Ada_2022;
with ACPI_Region_IO;
with ACPI_FADT.Transactions;
package body Transport_Model is
   package P renames ACPI_Region_Protocol;
   package R renames ACPI_Region_Policy;
   use type CuBit.Handles.Endpoint;
   type Hardware is record
      Value : Unsigned_64 := 0;
      Read_Count, Write_Count : Natural := 0;
   end record;
   procedure Transact
     (H : in out Hardware; Space : R.Address_Space; Address : Unsigned_64;
      Width : R.Access_Width; For_Write : Boolean; Input : Unsigned_64;
      Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Width);
      use type R.Address_Space;
   begin
      if Space /= R.Memory_Space or else Address /= 16#8000# then
         raise Program_Error with "model register geometry";
      end if;
      if For_Write then H.Value := Input; H.Write_Count := H.Write_Count + 1;
      else H.Read_Count := H.Read_Count + 1; end if;
      Output := H.Value; Completed := Behavior /= Unknown_Hardware;
   end Transact;
   package Backend is new ACPI_Region_IO (Hardware, Transact);
   Data : Hardware;
   function Cell return Unsigned_64 is (Data.Value);
   function Reads return Natural is (Data.Read_Count);
   function Writes return Natural is (Data.Write_Count);
   Stamp : constant Unsigned_64 := 16#AABB_CCDD_1020_3040#;
   procedure Reset is
      Fresh : R.State;
      Accepted : Boolean;
   begin
      Region := Fresh;
      R.Install (Region,
        (Tag => Stamp, Base => 16#8000#, Length => 8, Space => R.Memory_Space,
         Readable => True, Writable => True, Widths => [others => True],
         Register_Authority => (Resource_ID => Unsigned_64 (Register_ID),
           Readable => True, Writable => True, Delegable => False,
           Write_Mask => Unsigned_64'Last), Register_Offset => 0,
         Register_Width => ACPI_FADT.Transactions.Qword_Access), Accepted);
      if not Accepted then raise Program_Error with "model install"; end if;
      Behavior := Normal; Now := 10; Next_Time := 10; Advance_Clock := False;
      Clock_Reads := 0; Calls := 0; Data := (others => <>);
      Last_Deadline := 0; Ticket := 0;
      Last_Message := CuBit.Messages.NULL_MESSAGE;
   end Reset;
   function Call
     (Target : CuBit.Handles.Endpoint; Msg : in out CuBit.Messages.Message;
      Deadline : Unsigned_64) return CuBit.Messages.MessageTag is
      Result : P.Packet;
      Tag : CuBit.Messages.MessageTag;
   begin
      if Target /= CuBit.Handles.Test_Endpoint or else Deadline = Unsigned_64'Last
        or else Deadline <= Now or else Msg.authorityTag /= 0
        or else CuBit.Messages.Flag_Byte (Msg.tag) /= 0
      then raise Program_Error with "model IPC envelope"; end if;
      Calls := Calls + 1; Last_Message := Msg; Last_Deadline := Deadline;
      Backend.Dispatch (Region, Data, Stamp,
        (Label => Msg.tag.label, Length => Msg.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Msg.tag), Reserved => Msg.tag.reserved,
         Data => P.Words (Msg.words)), Result, Ticket);
      Tag := (label => Result.Label, length => Result.Length,
        protocol => 0, ipc => <>, reserved => Result.Reserved);
      Msg.words := CuBit.Messages.MessageWords (Result.Data);
      case Behavior is
         when Normal | Unknown_Hardware => null;
         when Timeout_After_Effect => Tag.label := CuBit.Messages.REPLY_TIMEOUT;
         when Kernel_Error_After_Effect => Tag.label := CuBit.Messages.Kernel_Reply_First;
         when Wrong_Label => Tag.label := P.Read_Operation;
         when Wrong_Length => Tag.length := 3;
         when Protocol_Flag => Tag.protocol := 1;
         when Accepts_Flag => Tag.ipc.Accepts_Capability := True;
         when Transferred_Flag => Tag.ipc.Capability_Transferred := True;
         when Refused_Flag => Tag.ipc.Capability_Refused := True;
         when Offered_Flag => Tag.ipc.Capability_Offered := True;
         when Reserved_Word => Tag.reserved := 1;
         when Oversize_Low => Msg.words (1) := P.Half_Mask + 1;
         when Oversize_High => Msg.words (2) := P.Half_Mask + 1;
         when Extra_Tail => Msg.words (3) := 1;
         when Nonzero_Write_Value => Msg.words (1) := 1;
      end case;
      -- The syscall returns the authoritative tag separately. Keep Msg.tag
      -- as the old request tag so this fixture exercises that distinction.
      return Tag;
   end Call;
   procedure Finish (Accepted : out Boolean) is
   begin
      R.Finish_Access (Region, Ticket, Accepted);
   end Finish;
end Transport_Model;
