with Interfaces; use Interfaces;
with Intel_GPU_ADLN_MOCS;
with Intel_GPU_MOCS_Configure;
with Intel_GPU_Native_MOCS;
with Intel_GPU_Reset_Pages;
procedure MOCS_Configure_Tests is
   package Plan renames Intel_GPU_ADLN_MOCS;
   Table : array (Plan.Register_Index) of Unsigned_32 := [others => 0];
   Reads, Writes : Natural := 0;
   Mode : Natural := 0;
   Owned : Boolean := True;
   function Owner return Boolean is (Owned);
   function Read32 (Offset : Unsigned_32) return Unsigned_32 is
   begin
      Reads := Reads + 1;
      if Mode = 1 and Reads = 5 then return Unsigned_32'Last; end if;
      if Mode = 3 and Reads = 5 then Owned := False; end if;
      for I in Plan.Register_Index loop
         if Plan.Offset (I) = Offset then
            return (if Mode = 5 and I >= 64 then Table (I) or 16#80008000# else Table (I));
         end if;
      end loop;
      raise Program_Error;
   end Read32;
   procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      Writes := Writes + 1;
      for I in Plan.Register_Index loop
         if Plan.Offset (I) = Offset then
            pragma Assert (Value = Plan.Value (I));
            Table (I) := (if Mode = 4 then 0 else Value);
            Success := not (Mode = 2 and Writes = 3); return;
         end if;
      end loop;
      raise Program_Error;
   end Write32;
   package Config is new Intel_GPU_MOCS_Configure (Owner, Read32, Write32);
   use type Config.Result;
   function Never_Owned return Boolean is (False);
   package Native is new Intel_GPU_Native_MOCS (Never_Owned);
   OK : Boolean;
begin
   for Scenario in 0 .. 5 loop
      declare
         Attempt : Config.Attempt; Status : Config.Result;
         Saved_Reads, Saved_Writes : Natural;
      begin
         Mode := Scenario; Table := [others => 0]; Owned := True;
         Reads := 0; Writes := 0;
         Config.Configure (Attempt, Status);
         pragma Assert (Status = (case Scenario is
           when 0 | 5 => Config.Ready, when 1 => Config.Read_Failed,
           when 2 => Config.Write_Failed, when 3 => Config.Ownership_Lost,
           when others => Config.Readback_Failed));
         if Scenario = 0 or Scenario = 5 then
            pragma Assert (Writes = 96 and Reads = 192);
            for I in Plan.Register_Index loop pragma Assert (Table (I) = Plan.Value (I)); end loop;
         end if;
         Saved_Reads := Reads; Saved_Writes := Writes;
         Config.Configure (Attempt, Status);
         pragma Assert (Status = Config.Rejected and Reads = Saved_Reads and Writes = Saved_Writes);
      end;
   end loop;
   for I in Plan.Register_Index loop
      pragma Assert (Native.Address_For (Plan.Offset (I)) =
        Intel_GPU_Reset_Pages.Virtual_Base +
          (if I < 64 then 9 * 4096 else 10 * 4096) +
          Unsigned_64 (Plan.Offset (I) mod 4096));
      Native.Write32 (Plan.Offset (I), Plan.Value (I), OK);
      pragma Assert (not OK); -- no MMIO may occur without owner evidence
   end loop;
   pragma Assert (Native.Address_For (16#B000#) = 0 and
                  Native.Address_For (16#B0A0#) = 0 and
                  Native.Address_For (16#4001#) = 0 and
                  Native.Address_For (16#4800#) = 0);
   Native.Write32 (16#4000#, Unsigned_32'Last, OK);
   pragma Assert (not OK);
end MOCS_Configure_Tests;
