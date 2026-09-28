with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Forcewake;
with Intel_GPU_ADLN_Reset;
with Intel_GPU_Reset_Pages;
package body Intel_GPU_Native_Reset is
   use Interfaces;
   Description : Inventory;
   Saved_Fuse : Unsigned_32 := Unsigned_32'Last;
   Attempted, Access_Fault : Boolean := False;
   Succeeded : Boolean := False;
   function Last_Succeeded return Boolean is (Succeeded);
   function Read (Offset : Unsigned_32) return Unsigned_32 is
      Allowed : Boolean := Offset = 16#941C# or Offset = 16#A2A0#;
   begin
      if Access_Fault then return Unsigned_32'Last; end if;
      for D in Domain loop Allowed := Allowed or Offset = Ack_Register (D); end loop;
      for E in Engine loop
         if Description.Engines (E) then
            Allowed := Allowed or Offset = Engine_Base (E) + 16#9C# or
              Offset = Engine_Base (E) + 16#D0# or Offset = Pending_Register (E);
         end if;
      end loop;
      if not Allowed then Access_Fault := True; return Unsigned_32'Last; end if;
      declare
         Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (16#6000_0000# + Integer_Address (Offset));
      begin return Value; end;
   end Read;
   procedure Write (Offset, Value : Unsigned_32) is
      Address_Value : Integer_Address := 0;
   begin
      -- Keep the failure latched, but still attempt cancellation on every
      -- admitted engine. No reset/stop/prepare or forcewake release is allowed
      -- after an access fault; a cleanup attempt does not clear quarantine.
      if Access_Fault and then not
        (Value = 16#10000# and then Engine_Write_Allowed (Description, Offset, Value))
      then return; end if;
      if (Offset = 16#941C# and Value = 1) or else
        Engine_Write_Allowed (Description, Offset, Value)
      then
         for P in Intel_GPU_Reset_Pages.Page_Index loop
            if Unsigned_64 (Offset) / 4096 = Intel_GPU_Reset_Pages.Offset (P) / 4096 then
               Address_Value := 16#6120_0000# + Integer_Address (P) * 4096 +
                 Integer_Address (Offset mod 4096);
            end if;
         end loop;
      else
         for D in Domain loop
            if Description.Domains (D) and Offset = Request_Register (D) and
              (Value = 16#10001# or Value = 16#10000#)
            then Address_Value := 16#6020_0000# + Integer_Address (Offset mod 4096); end if;
         end loop;
      end if;
      if Address_Value = 0 then Access_Fault := True; return; end if;
      declare
         Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Address_Value);
      begin Register_Value := Value; end;
   end Write;
   procedure Pause is
   begin System.Machine_Code.Asm ("pause", Volatile => True); end Pause;
   function Now return Unsigned_64 is
      Value : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Value.Available then Value.Microseconds else Unsigned_64'Last);
   end Now;
   function Milliseconds return Unsigned_64 is (syscall (SYSCALL_GETTIME));
   package Power is new Intel_GPU_ADLN_Forcewake (Read, Write, Pause, Milliseconds);
   procedure Hold (Success : out Boolean) is
   begin Power.Acquire (16#8086#, 16#46D2#, Saved_Fuse, Success); end Hold;
   package Reset is new Intel_GPU_ADLN_Reset (Read, Write, Hold, Now, Pause);
   function Execute (Fuse : Unsigned_32) return String is
      Status : Reset.Result;
      use type Reset.Result;
   begin
      Succeeded := False;
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      Description := Decode (16#8086#, 16#46D2#, Fuse);
      if not Description.Valid then return "invalid-inventory"; end if;
      if Now = Unsigned_64'Last then return "clock-unavailable"; end if;
      Saved_Fuse := Fuse;
      Reset.Execute (16#8086#, 16#46D2#, Fuse, Status);
      if Access_Fault then return "register-access-rejected"; end if;
      Succeeded := Status = Reset.Complete;
      return Reset.Result'Image (Status) & " engine=" & Natural'Image (Reset.Failure_Engine);
   end Execute;
end Intel_GPU_Native_Reset;
