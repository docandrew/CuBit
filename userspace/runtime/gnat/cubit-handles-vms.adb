pragma Ada_2022;
with System.Storage_Elements;
with Capability_ABI;
with CuBit.Handles.Kernel;
with CuBit.Messages;

package body CuBit.Handles.VMs is
   package K renames CuBit.Handles.Kernel;

   function Valid (Item : VM) return Boolean is (Capability_ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : VCPU) return Boolean is (Capability_ABI.Is_Handle_Word (Item.Word));

   function Address_Word (A : System.Address) return Unsigned_64 is
     (Unsigned_64 (System.Storage_Elements.To_Integer (A)));

   procedure Raw (Word : Unsigned_64; Label : Unsigned_64; W0, W1, W2, W3 : Unsigned_64;
                  Msg : out CuBit.Messages.Message; Readable : out Boolean);
   function Ask (Word, Label, W0, W1, W2, W3 : Unsigned_64 := 0) return Reply;

   --  One Call on Word: the reply message, or Readable False.
   procedure Raw (Word : Unsigned_64; Label : Unsigned_64; W0, W1, W2, W3 : Unsigned_64;
                  Msg : out CuBit.Messages.Message; Readable : out Boolean) is
      Result : Unsigned_64;
   begin
      Msg := CuBit.Messages.NULL_MESSAGE;
      Msg.tag.label := Unsigned_32 (Label);
      Msg.tag.length := 4;
      Msg.words := [W0, W1, W2, W3];
      Result := K.Call (K.Call_Via_Endpoint, Word, Address_Word (Msg'Address),
                        CuBit.Messages.Wait_Forever);
      Readable := Result /= 0 and then Result /= Unsigned_64'Last and then Msg.tag.length = 4;
   end Raw;

   function To_Outcome (W : Unsigned_64) return ABI.Outcome is
     (case W is
        when 0 => ABI.Ok, when 2 => ABI.Not_Supported, when 3 => ABI.Busy,
        when 4 => ABI.No_Memory, when 5 => ABI.Ended, when 6 => ABI.Bad_Range,
        when 7 => ABI.Ceiling, when others => ABI.Refused);

   function Ask (Word, Label, W0, W1, W2, W3 : Unsigned_64 := 0) return Reply is
      Msg : CuBit.Messages.Message;
      Readable : Boolean;
   begin
      Raw (Word, Label, W0, W1, W2, W3, Msg, Readable);
      if not Readable then
         return (others => <>);
      end if;
      return (Outcome => To_Outcome (Msg.words (0)),
              W1 => Msg.words (1), W2 => Msg.words (2), W3 => Msg.words (3));
   end Ask;

   function Create_VM (Authority : VM_Create; Machine : out VM) return Reply is
      R : constant Reply := Ask (K.Word (Authority), ABI.Create_VM_Label);
   begin
      Machine := (if ABI."=" (R.Outcome, ABI.Ok) then (Word => R.W1) else No_VM);
      return R;
   end Create_VM;

   function Add_RAM (Machine : VM; First, Address, Bytes, Rights : Unsigned_64) return Reply is
     (Ask (Machine.Word, ABI.Add_RAM_Label, First, Address, Bytes, Rights));

   function Create_VCPU (Machine : VM; CPU : out VCPU) return Reply is
      R : constant Reply := Ask (Machine.Word, ABI.Create_VCPU_Label);
   begin
      CPU := (if ABI."=" (R.Outcome, ABI.Ok) then (Word => R.W1) else No_VCPU);
      return R;
   end Create_VCPU;

   function Destroy (Machine : VM) return Reply is (Ask (Machine.Word, ABI.Destroy_Label));
   function Describe (Machine : VM) return Reply is (Ask (Machine.Word, ABI.Describe_Label));

   function Run
     (CPU : VCPU; Deadline : Unsigned_64; Flags : Unsigned_64 := 0;
      RAX, RDX : Unsigned_64 := 0) return Exit_Report
   is
      Msg : CuBit.Messages.Message;
      Readable : Boolean;
      Status : ABI.Run_Status;
   begin
      Raw (CPU.Word, ABI.Run_Label, Deadline, Flags, RAX, RDX, Msg, Readable);
      if not Readable then
         return (others => <>);
      end if;
      Status := (case Msg.tag.label is
                   when 1 => ABI.Exited, when 2 => ABI.Deadline, when 3 => ABI.Kicked,
                   when 4 => ABI.VM_Ended, when others => ABI.Run_Refused);
      return (Status => Status, Kind => Msg.words (0), Code => Msg.words (1),
              Info_1 => Msg.words (2), Info_2 => Msg.words (3));
   end Run;

   function Set_Word (S : ABI.State_Set) return Unsigned_64 is
     (Unsigned_64 (ABI.State_Set'Enum_Rep (S)));

   function Get_State (CPU : VCPU; Set : ABI.State_Set; Buffer : System.Address;
                       Bytes : Unsigned_64) return Reply is
     (Ask (CPU.Word, ABI.Get_State_Label, Set_Word (Set), Address_Word (Buffer), Bytes));
   function Set_State (CPU : VCPU; Set : ABI.State_Set; Buffer : System.Address;
                       Bytes : Unsigned_64) return Reply is
     (Ask (CPU.Word, ABI.Set_State_Label, Set_Word (Set), Address_Word (Buffer), Bytes));

   function Inject (CPU : VCPU; Kind : ABI.Event_Kind; Vector : Unsigned_64;
                    Error : Unsigned_64 := 0; Has_Error : Boolean := False) return Reply is
     (Ask (CPU.Word, ABI.Inject_Label, Unsigned_64 (ABI.Event_Kind'Enum_Rep (Kind)),
           Vector, Error, (if Has_Error then ABI.Event_Error_Valid else 0)));

   function Kick (CPU : VCPU) return Reply is (Ask (CPU.Word, ABI.Kick_Label));
   function Statistics (CPU : VCPU) return Reply is (Ask (CPU.Word, ABI.Statistics_Label));
   function Speculation (CPU : VCPU) return Reply is (Ask (CPU.Word, ABI.Speculation_Label));
end CuBit.Handles.VMs;
