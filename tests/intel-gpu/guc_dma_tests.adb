with Interfaces; use Interfaces;
with Intel_GPU_GuC_DMA;
with Ada.Text_IO;
procedure GuC_DMA_Tests is
   type Scenario is (Good, Busy, Bad_Read, No_Clock, Bad_Poll,
                     Lost_Clock, Backward, Stalled, Deadline, Bad_Cleanup);
   procedure Run (Mode : Scenario; Source : Unsigned_64 := 16#200000#;
                  Bytes : Unsigned_64 := 335104;
                  Capacity : Unsigned_64 := 1024 * 1024;
                  Valid : Boolean := True) is
      Reads, Writes, Clocks, Pauses : Natural := 0;
      function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         pragma Assert (Offset = 16#C314#);
         Reads := Reads + 1;
         if Reads = 1 then
            return (if Mode = Busy then 1 elsif Mode = Bad_Read then
                    Unsigned_32'Last else 0);
         elsif Writes = 7 then
            return (if Mode = Bad_Cleanup then 16#10# else 0);
         else
            return (if Mode = Bad_Poll then Unsigned_32'Last
                    elsif Mode in Stalled | Deadline then 17 else 16);
         end if;
      end Read32;
      procedure Write32 (Offset, Value : Unsigned_32) is
         Offsets : constant array (1 .. 7) of Unsigned_32 :=
           [16#C300#, 16#C304#, 16#C308#, 16#C30C#, 16#C310#, 16#C314#, 16#C314#];
         Values : constant array (1 .. 7) of Unsigned_32 :=
           [Unsigned_32 (Source), 0, 16#2000#, 16#70000#,
            Unsigned_32 (Bytes), 16#00110011#, 16#00100000#];
      begin
         Writes := Writes + 1;
         pragma Assert (Writes <= 7);
         pragma Assert (Offset = Offsets (Writes) and Value = Values (Writes));
      end Write32;
      function Now return Unsigned_64 is
      begin
         Clocks := Clocks + 1;
         if Mode = No_Clock or (Mode = Lost_Clock and Clocks > 1) then
            return Unsigned_64'Last;
         elsif Clocks = 1 then return 100;
         elsif Mode = Backward then return 99;
         elsif Mode = Deadline then return 100100;
         else return 100;
         end if;
      end Now;
      procedure Pause is
      begin Pauses := Pauses + 1; end Pause;
      package DMA is new Intel_GPU_GuC_DMA (Read32, Write32, Now, Pause);
      use type DMA.Result;
      use type DMA.Phase;
      Object : DMA.Attempt;
      Status : DMA.Result;
      Saved : Natural;
      Saved_Phase : DMA.Phase;
   begin
      DMA.Execute (Object, Source, Bytes, Capacity, 3, Status);
      pragma Assert (Status =
        (if not Valid then DMA.Rejected else
          (case Mode is
             when Good => DMA.Complete,
             when Busy => DMA.Busy,
             when Bad_Read | Bad_Poll => DMA.Invalid_MMIO,
             when No_Clock | Lost_Clock | Backward => DMA.Invalid_Clock,
             when Stalled | Deadline => DMA.Timed_Out,
             when Bad_Cleanup => DMA.Cleanup_Failed)));
      if not Valid then pragma Assert (Reads = 0 and Clocks = 0); end if;
      pragma Assert (Writes = (if not Valid or Mode in Busy | Bad_Read | No_Clock then 0 else 7));
      pragma Assert (Pauses = (if Valid and Mode = Stalled then 2 else 0));
      Saved_Phase := DMA.Current (Object);
      pragma Assert (Saved_Phase =
        (if Writes = 0 then DMA.Consumed elsif Mode = Good then DMA.Transferred
         else DMA.Quarantined));
      Saved := Writes + Reads + Clocks + Pauses;
      DMA.Execute (Object, Source, Bytes, Capacity, 3, Status);
      pragma Assert (Status = DMA.Rejected and DMA.Current (Object) = Saved_Phase);
      pragma Assert (Saved = Writes + Reads + Clocks + Pauses);
   end Run;
begin
   for Mode in Scenario loop Run (Mode); end loop;
   Run (Good, Source => 1, Valid => False);
   Run (Good, Source => 2 ** 32, Valid => False);
   Run (Good, Source => 2 ** 32 - 4096, Valid => False);
   Run (Good, Bytes => 0, Valid => False);
   Run (Good, Bytes => 128, Valid => False);
   Run (Good, Bytes => 133, Valid => False);
   Run (Good, Capacity => 0, Valid => False);
   Run (Good, Capacity => 24 * 1024, Valid => False);
   Run (Good, Capacity => 8 * 1024 * 1024 + 1, Valid => False);
   Run (Good, Bytes => Unsigned_64'Last, Valid => False);
   Run (Good, Source => 2 ** 32 - 4096, Bytes => 4096);
   Run (Good, Bytes => 4096, Capacity => 28 * 1024);
   Ada.Text_IO.Put_Line ("PASS: GuC DMA ordering, bounds, timeout, quarantine and no retry (22 cases)");
end GuC_DMA_Tests;
