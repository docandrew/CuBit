with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Monotonic;
with Intel_GPU_Display_Mapping;
with Intel_GPU_Parent_Writes;
with Intel_GPU_Display_Enable;
with Intel_GPU_Display_Topology;
package body Intel_GPU_Native_Parent is
   use Intel_GPU_Display_Topology;
   Attempted, Faulted, Retained : Boolean := False;
   function Held return Boolean is (Retained);
   function Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Faulted or else not Intel_GPU_Display_Mapping.Ready or else
        Offset not in 16#45404# | 16#42000# | 16#46430#
      then Faulted := True; return Unsigned_32'Last; end if;
      declare
         Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#6000_0000# + Integer_Address (Offset));
      begin return Value; end;
   end Read;
   procedure Write (Offset, Value : Unsigned_32; Success : out Boolean) is
      Prior : Unsigned_32;
      Target : Integer_Address;
   begin
      Success := False;
      if Faulted or else not Intel_GPU_Display_Mapping.Ready or else
        Offset not in 16#45404# | 16#46430#
      then Faulted := True; return; end if;
      Prior := Read (Offset);
      -- Only add the selected parent's request or PW1 workaround. Preserve all
      -- other bits and fail on a changing baseline; no disable path exported.
      if not Intel_GPU_Parent_Writes.Allowed (Item, Offset, Prior, Value)
      then
         Faulted := True; return;
      end if;
      Target := Integer_Address (Intel_GPU_Display_Mapping.Virtual_Base) +
        (if Offset = 16#45404# then 16#404# else 16#1430#);
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Target);
      begin Register_Value := Value; end;
      Success := True;
   end Write;
   function Now_Us return Unsigned_64 is
      Stamp : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Stamp.Available then Stamp.Microseconds else Unsigned_64'Last);
   end Now_Us;
   procedure Pause is
   begin System.Machine_Code.Asm ("pause", Volatile => True); end Pause;
   procedure Parent_Post_Enable (Success : out Boolean) is
   begin
      -- Xe-LPD parent descriptors have irq_pipe_mask=0. Never use this
      -- callback for pipe wells. No legacy VGA access is performed here.
      Success := not Faulted and then Item in PW1 | PW2;
   end Parent_Post_Enable;
   procedure Never_Disable (Success : out Boolean) is
   begin Success := False; end Never_Disable;
   package Enable is new Intel_GPU_Display_Enable
     (Item, Read, Write, Now_Us, Pause,
      Parent_Post_Enable, Never_Disable);
   function Acquire (Owner, Ancestor_Held : Boolean) return String is
      Added : Boolean;
      Status : Enable.Result;
      use type Enable.Result;
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if Item not in PW1 | PW2 or else
        (Item = PW2 and then not Ancestor_Held)
      then return "unsupported-well-or-parent-unavailable"; end if;
      if not Owner or else not Intel_GPU_Display_Mapping.Ready then
         return "owner-or-mapping-unavailable";
      end if;
      Enable.Execute (True, 100_000, Added, Status);
      Retained := Status = Enable.Ready and not Faulted;
      return (case Status is
         when Enable.Ready => (if Added then "held-added" else "held-inherited"),
         when Enable.Rejected => "rejected",
         when Enable.Invalid_MMIO => "invalid-MMIO",
         when Enable.Invalid_Clock => "invalid-clock",
         when Enable.Deadline_Expired => "deadline-expired",
         when Enable.Poll_Exhausted => "poll-exhausted",
         when Enable.Write_Failed => "write-failed",
         when Enable.Request_Changed => "request-changed",
         when Enable.Post_Enable_Failed => "post-enable-failed",
         when Enable.Released | Enable.Pre_Disable_Failed => "unexpected-release");
   end Acquire;
end Intel_GPU_Native_Parent;
