with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Monotonic;
with Intel_GPU_Display_Mapping;
with Intel_GPU_Display_Enable;
with Intel_GPU_Pipe_IRQ;
package body Intel_GPU_Native_Pipe is
   use Intel_GPU_Display_Topology;
   Well : constant Request_Well := Pipe_Well (Item);
   IRQ_Base : constant Unsigned_32 := 16#44400# + 16 * Pipe'Pos (Item);
   Attempted, Faulted, Retained, Access_Ready, Delivery_Ready : Boolean := False;
   VGA_Checked, VGA_Disabled : Boolean := False;
   IRQ_Attempted : Boolean := False;
   function Held return Boolean is (Retained);
   function Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Faulted or else not Access_Ready or else
        Offset not in 16#45404# | 16#42000# | 16#41000#
      then Faulted := True; return Unsigned_32'Last; end if;
      declare
         Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#6000_0000# + Integer_Address (Offset));
      begin return Value; end;
   end Read;
   procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      Prior : Unsigned_32;
   begin
      Success := False;
      if Faulted or else not Access_Ready or else Offset /= 16#45404# then
         Faulted := True; return;
      end if;
      Prior := Read (Offset);
      if Prior = Unsigned_32'Last or else Value /= (Prior or Request_Mask (Well)) then
         Faulted := True; return;
      end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Intel_GPU_Display_Mapping.Virtual_Base) + 16#404#);
      begin Register_Value := Value; end;
      Success := True;
   end Write;
   procedure IRQ_Read (Offset : Unsigned_32; Value : out Unsigned_32; Success : out Boolean) is
   begin
      Value := Unsigned_32'Last; Success := False;
      if Faulted or else not Access_Ready or else
        (Offset /= IRQ_Base + 4 and Offset /= IRQ_Base + 8 and Offset /= IRQ_Base + 12)
      then Faulted := True; return; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#6000_0000# + Integer_Address (Offset));
      begin Value := Register_Value; end;
      -- All-ones is legal for IMR. The sequence verifies IER and drained IIR
      -- separately; MMIO faults themselves remain process faults.
      Success := True;
   end IRQ_Read;
   procedure IRQ_Write (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      Success := False;
      if Faulted or else not Access_Ready or else not Delivery_Ready or else
        not (((Offset = IRQ_Base + 4 or Offset = IRQ_Base + 8) and Value = Unsigned_32'Last) or
             (Offset = IRQ_Base + 12 and Value = 0))
      then Faulted := True; return; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#6140_2000# + Integer_Address (Offset - 16#44000#));
      begin Register_Value := Value; end;
      Success := True;
   end IRQ_Write;
   package IRQ is new Intel_GPU_Pipe_IRQ (Item, IRQ_Read, IRQ_Write);
   IRQ_Status : IRQ.Result := IRQ.Rejected;
   function Hex (Value : Unsigned_32) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
      Bits : Unsigned_32 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Bits and 15) + 1);
         Bits := Shift_Right (Bits, 4);
      end loop;
      return Text;
   end Hex;
   function IRQ_Diagnostic return String is
   begin
      return (case IRQ_Status is
        when IRQ.Rejected => "rejected",
        when IRQ.Write_Failed => "write-failed",
        when IRQ.Read_Failed => "read-failed",
        when IRQ.Verify_Failed => "verify-failed",
        when IRQ.Pending_Events => "pending-events",
        when IRQ.Complete => "complete") &
        " reg=" & Hex (IRQ.Last_Read_Offset) &
        " got=" & Hex (IRQ.Last_Read_Value) &
        " want=" & Hex (IRQ.Expected_Value);
   end IRQ_Diagnostic;
   procedure Post_Enable (Success : out Boolean) is
      VGA : constant Unsigned_32 := Read (16#41000#);
      use type IRQ.Result;
   begin
      Success := False;
      -- Preserve an inherited VGA consumer rather than disabling it blindly.
      VGA_Checked := True;
      VGA_Disabled := VGA /= Unsigned_32'Last and then (VGA and 16#8000_0000#) /= 0;
      if not VGA_Disabled then return; end if;
      IRQ_Attempted := True;
      IRQ.Quiesce (Access_Ready, True, Delivery_Ready, IRQ_Status);
      Success := IRQ_Status = IRQ.Complete and not Faulted;
   end Post_Enable;
   procedure Never_Disable (Success : out Boolean) is
   begin Success := False; end Never_Disable;
   function Now_Us return Unsigned_64 is
      Stamp : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin return (if Stamp.Available then Stamp.Microseconds else Unsigned_64'Last); end Now_Us;
   procedure Pause is
   begin System.Machine_Code.Asm ("pause", Volatile => True); end Pause;
   package Enable is new Intel_GPU_Display_Enable
     (Well, Read, Write, Now_Us, Pause, Post_Enable, Never_Disable);
   function Acquire
     (Owner, IRQ_Page_Ready, Upstream_Blocked : Boolean;
      Parent_References : Unsigned_64) return String
   is
      Added : Boolean;
      Status : Enable.Result;
      use type Enable.Result;
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if not Owner or else not IRQ_Page_Ready or else
        not Upstream_Blocked or else not Intel_GPU_Display_Mapping.Ready or else
        not Valid (Parent_References) or else
        (Parent_References and Ancestors (Well)) /= Ancestors (Well)
      then return "prerequisites-unavailable"; end if;
      Access_Ready := True; Delivery_Ready := True;
      Enable.Execute (True, 100_000, Added, Status);
      Retained := Status = Enable.Ready and not Faulted;
      if Retained then return (if Added then "held-added" else "held-inherited"); end if;
      return "enable-failed phase=" & Enable.Result'Image (Status) &
        " mmio-rejected=" & Boolean'Image (Faulted) &
        (if VGA_Checked then " VGA-disabled=" & Boolean'Image (VGA_Disabled) else "") &
        (if IRQ_Attempted then " IRQ=" & IRQ_Diagnostic else "") &
        " (references retained)";
   end Acquire;
end Intel_GPU_Native_Pipe;
