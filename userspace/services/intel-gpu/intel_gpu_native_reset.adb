with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Forcewake;
with Intel_GPU_ADLN_Reset;
with Intel_GPU_Reset_Pages;
with Intel_GPU_Forcewake_Fallback;
with Intel_GPU_ADS_Observe;
with Intel_GPU_ADS_System_Info;
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
      -- New ADS reads are unavailable until the reset sequence completes.
      Allowed := Allowed or (Succeeded and then
        (Offset = Intel_GPU_ADLN_Steering.Slice_Register or else
         Offset = Intel_GPU_ADLN_Steering.DSS_Register or else
         Offset = Intel_GPU_ADLN_Steering.L3_Register or else
         Offset = Intel_GPU_ADLN_EU.EU_Disable_Register or else
         Offset = Intel_GPU_ADS_System_Info.Doorbell_Register));
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
   package ADS_Reader is new Intel_GPU_ADS_Observe (Read);
   ADS : ADS_Reader.Observation;
   function ADS_Observed return Boolean is
     (Succeeded and ADS.Valid and not Access_Fault);
   function ADS_Inventory return Inventory is
     (if ADS_Observed then Description else (others => <>));
   function ADS_Topology return Intel_GPU_ADLN_Steering.Topology is
     (if ADS_Observed then ADS.Topology else (others => <>));
   function ADS_Doorbell return Unsigned_32 is
     (if ADS_Observed then ADS.Doorbell else Unsigned_32'Last);
   function ADS_Execution_Units return Intel_GPU_ADLN_EU.Topology is
     (if ADS_Observed then ADS.Execution_Units else (others => <>));
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
               Address_Value := Integer_Address (Intel_GPU_Reset_Pages.Virtual_Base) + Integer_Address (P) * 4096 +
                 Integer_Address (Offset mod 4096);
            end if;
         end loop;
      else
         for D in Domain loop
            if Description.Domains (D) and Offset = Request_Register (D) and
              (Value = 16#10001# or Value = 16#10000# or
               Value = 16#8000_8000# or Value = 16#8000_0000#)
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
   package Fallback is new Intel_GPU_Forcewake_Fallback (Read, Write, Now, Pause);
   Fallback_Attempted : Boolean := False;
   Fallback_Failure_Saved : Boolean := False;
   Fallback_Status : Fallback.Result := Fallback.Ack_Unchanged;
   function Fallback_Detail return String is
     (if not Fallback_Attempted then "" else
        " fallback=" & (case Fallback_Status is
           when Fallback.Recovered => "recovered",
           when Fallback.Invalid_MMIO => "invalid-MMIO",
           when Fallback.Invalid_Clock => "invalid-clock",
           when Fallback.Timed_Out => "deadline-expired",
           when Fallback.Poll_Exhausted => "poll-budget-exhausted",
           when Fallback.Ack_Unchanged => "ack-unchanged"));
   procedure Recover_Domain (Item : Domain; Expected : Unsigned_32;
                             Recovered : in out Boolean) is
      use type Fallback.Result;
      Status : Fallback.Result;
   begin
      Recovered := False;
      if Access_Fault then return; end if;
      Fallback_Attempted := True;
      Fallback.Recover (Request_Register (Item), Ack_Register (Item), Expected,
                        100_000, Status);
      -- Cleanup of an earlier domain must not overwrite the first failed
      -- fallback's evidence with a later successful release recovery.
      if not Fallback_Failure_Saved then
         Fallback_Status := Status;
         Fallback_Failure_Saved := Status /= Fallback.Recovered;
      end if;
      Recovered := not Access_Fault and then Status = Fallback.Recovered;
   end Recover_Domain;
   package Power is new Intel_GPU_ADLN_Forcewake
     (Read, Write, Pause, Milliseconds, Recover_Domain => Recover_Domain);
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
      if Succeeded then
         declare
            use type Power.Ownership_State;
         begin
            ADS := ADS_Reader.Capture (Description, Power.State = Power.Held);
         end;
      end if;
      if Status = Reset.Forcewake_Failed then
         return "forcewake-failed " & Power.Failure_Detail & Fallback_Detail;
      end if;
      -- The native runtime uses Discard_Names; enumeration Image would emit
      -- an opaque ordinal instead of a useful hardware diagnostic.
      return (case Status is
         when Reset.Rejected => "rejected",
         when Reset.Forcewake_Failed => "forcewake-failed",
         when Reset.Stop_Failed => "stop-failed",
         when Reset.Prepare_Failed => "prepare-failed",
         when Reset.Reset_Failed => "reset-failed",
         when Reset.Cleanup_Failed => "cleanup-failed",
         when Reset.Complete => "complete") &
        " engine=" & Natural'Image (Reset.Failure_Engine) & Fallback_Detail;
   end Execute;
end Intel_GPU_Native_Reset;
