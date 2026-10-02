with Interfaces; use Interfaces;
with Intel_GPU_Firmware;
with Intel_GPU_GuC_Upload;
with Intel_GPU_GuC_Status;
with Intel_GPU_GuC_Parameters;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
procedure GuC_Upload_Tests is
   use type Intel_GPU_GuC_Status.State;
   Blob : array (Natural range 0 .. 335359) of Unsigned_8 := [others => 0];
   Actual_Blob : Boolean := False;
   procedure Check_Parameters is
      package P renames Intel_GPU_GuC_Parameters;
      use type P.Startup_Words;
      Config : P.Configuration :=
        (Pin_Bias => 4096, ADS => P.Address (P.Runtime_GGTT_Limit - 4096),
         ADS_Backing_Bytes => 4096,
         Log => (Base => P.Address (16#300000#), Backing_Bytes => 32 * 1024 * 1024,
                 Notify_Half_Full => True,
                 Capture_Megabyte_Units => True, Log_Megabyte_Units => True,
                 Crash_Count => 3, Capture_Count => 3, Debug_Pages_Count => 15),
         Device => 16#46D2#, Revision => 0, Scheduler_Enabled => False, SLPC_Enabled => False,
         PXP_Enabled => False, Logging_Enabled => False, Verbosity => 3,
         Workarounds => 16#00444000#);
      Block_Value : P.Parameter_Block;
      Data : P.Startup_Words;
   begin
      pragma Assert (not P.Valid (P.Address (0)));
      pragma Assert (not P.Valid (P.Address (1)));
      pragma Assert (not P.Valid (P.Address (2 ** 32)));
      pragma Assert (not P.Valid (P.Address (P.Runtime_GGTT_Limit)));
      pragma Assert (not P.Valid (P.Address (P.Runtime_GGTT_Limit + 4096)));
      pragma Assert (not P.Valid (P.Address (16#FFFFF000#)));
      pragma Assert (not P.Valid (P.Address (Unsigned_64'Last)));
      for Revision in Unsigned_8 loop
         Config.Revision := Revision;
         Block_Value := P.Encode_ADLN (Config);
         pragma Assert (P.Valid (Block_Value));
         Data := P.Words (Block_Value);
         pragma Assert (Data (0) = 16#00300FFF# and Data (1) = 16#00444000#
                        and Data (2) = 16#4000# and Data (3) = 16#40#
                        and Data (4) = 16#001FDBFE#
                        and Data (5) = 16#46D20000# + Unsigned_32 (Revision));
         pragma Assert ((for all I in 6 .. 13 => Data (I) = 0));
      end loop;
      -- Exhaust the PCI device field independently of the native platform
      -- admission: this codec must neither substitute46D2 nor encode other
      -- families. Every accepted device/revision pair preserves both fields.
      for Device in Unsigned_16 loop
         Config.Device := Device;
         Block_Value := P.Encode_ADLN (Config);
         pragma Assert (P.Valid (Block_Value) = (Device in 16#46D0# .. 16#46D4#));
         if P.Valid (Block_Value) then
            for Revision in Unsigned_8 loop
               Config.Revision := Revision;
               Data := P.Words (P.Encode_ADLN (Config));
               pragma Assert (Data (5) = Unsigned_32 (Device) * 65536 +
                                Unsigned_32 (Revision));
            end loop;
         else
            pragma Assert (P.Words (Block_Value) = P.Startup_Words'[others => 0]);
         end if;
      end loop;
      Config.Device := 16#46D2#;
      Config.Scheduler_Enabled := True;
      Config.SLPC_Enabled := True;
      Config.PXP_Enabled := True;
      Config.Logging_Enabled := True;
      Data := P.Words (P.Encode_ADLN (Config));
      pragma Assert (Data (2) = 6 and Data (3) = 3);
      for B in 0 .. 31 loop
         Config.Workarounds := Shift_Left (1, B);
         Block_Value := P.Encode_ADLN (Config);
         pragma Assert (P.Valid (Block_Value) = (B in 14 | 18 | 22));
         if not P.Valid (Block_Value) then
            pragma Assert (P.Words (Block_Value) = P.Startup_Words'[others => 0]);
         end if;
      end loop;
      Config.Workarounds := 0;
      Config.ADS := P.Address (0);
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.ADS := P.Address (4096);
      Config.Log.Base := P.Address (0);
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Log.Base := P.Address (16#300000#);
      for Crash in P.Small_Count loop
         for Debug in P.Debug_Count loop
            for Capture in P.Small_Count loop
               for Large_Log in Boolean loop
                  for Large_Capture in Boolean loop
                     Config.Log.Crash_Count := Crash;
                     Config.Log.Debug_Pages_Count := Debug;
                     Config.Log.Capture_Count := Capture;
                     Config.Log.Log_Megabyte_Units := Large_Log;
                     Config.Log.Capture_Megabyte_Units := Large_Capture;
                     Config.Log.Backing_Bytes :=
                       4096 + Unsigned_64 (Crash + Debug + 2) *
                         (if Large_Log then 1048576 else 4096) +
                       Unsigned_64 (Capture + 1) *
                         (if Large_Capture then 1048576 else 4096);
                     pragma Assert (P.Required_Log_Bytes (Config.Log) = Config.Log.Backing_Bytes);
                     pragma Assert (P.Valid (P.Encode_ADLN (Config)));
                     Config.Log.Backing_Bytes := Config.Log.Backing_Bytes - 4096;
                     pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Config.Log.Backing_Bytes := P.Required_Log_Bytes (Config.Log);
      Config.Log.Base := P.Address (P.Runtime_GGTT_Limit - Config.Log.Backing_Bytes);
      pragma Assert (P.Valid (P.Encode_ADLN (Config)));
      Config.Log.Base := P.Address (P.Runtime_GGTT_Limit - Config.Log.Backing_Bytes + 4096);
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Log.Base := P.Address (16#300000#);
      Config.Log.Backing_Bytes := Unsigned_64'Last;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Log.Backing_Bytes := P.Required_Log_Bytes (Config.Log);
      Config.ADS := P.Address (P.Runtime_GGTT_Limit - 4096);
      Config.ADS_Backing_Bytes := 8192;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.ADS_Backing_Bytes := Unsigned_64'Last;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.ADS_Backing_Bytes := 0;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.ADS_Backing_Bytes := 4097;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.ADS_Backing_Bytes := 4096;
      Config.Pin_Bias := 0;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := 4097;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := P.Runtime_GGTT_Limit;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := 16#300000#;
      pragma Assert (P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := Config.Pin_Bias + 4096;
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := 8192;
      Config.ADS := P.Address (4096);
      pragma Assert (not P.Valid (P.Encode_ADLN (Config)));
      Config.Pin_Bias := 4096;
      Config.Log.Crash_Count := 0;
      Config.Log.Debug_Pages_Count := 0;
      Config.Log.Capture_Count := 0;
      Config.Log.Log_Megabyte_Units := False;
      Config.Log.Capture_Megabyte_Units := False;
      Config.Log.Backing_Bytes := 16384;
      -- Independent page-set oracle for ADS/log overlap and adjacency.
      for ADS_Page in 1 .. 8 loop
         for Log_Page in 1 .. 8 loop
            for ADS_Pages in 1 .. 8 loop
               declare
                  Intersects : Boolean := False;
               begin
                  Config.ADS := P.Address (Unsigned_64 (ADS_Page) * 4096);
                  Config.ADS_Backing_Bytes := Unsigned_64 (ADS_Pages) * 4096;
                  Config.Log.Base := P.Address (Unsigned_64 (Log_Page) * 4096);
                  for Page in 1 .. 16 loop
                     Intersects := Intersects or
                       (Page in ADS_Page .. ADS_Page + ADS_Pages - 1 and
                        Page in Log_Page .. Log_Page + 3);
                  end loop;
                  Block_Value := P.Encode_ADLN (Config);
                  pragma Assert (P.Valid (Block_Value) = not Intersects);
                  if Intersects then
                     pragma Assert (P.Words (Block_Value) = P.Startup_Words'[others => 0]);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      -- Native bring-up shape:16MiB ADS plus the minimal16KiB log,
      -- scheduler enabled, SLPC/PXP disabled, no log notification interrupt.
      Config :=
        (Pin_Bias => 16#200000#, ADS => P.Address (16#200000#),
         ADS_Backing_Bytes => 16#1000000#,
         Log => (Base => P.Address (16#1200000#), Backing_Bytes => 16384,
                 Notify_Half_Full => False, Capture_Megabyte_Units => False,
                 Log_Megabyte_Units => False, Crash_Count => 0,
                 Capture_Count => 0, Debug_Pages_Count => 0),
         Device => 16#46D2#, Revision => 7,
         Scheduler_Enabled => True, SLPC_Enabled => False, PXP_Enabled => False,
         Logging_Enabled => True, Verbosity => 1, Workarounds => 16#444000#);
      Block_Value := P.Encode_ADLN (Config);
      pragma Assert (P.Valid (Block_Value));
      pragma Assert (P.Words (Block_Value) = P.Startup_Words'
        [0 => 16#01200001#, 1 => 16#00444000#, 2 => 0, 3 => 1,
         4 => 16#400#, 5 => 16#46D20007#, others => 0]);
   end Check_Parameters;
   procedure Run (Fail_Write : Natural; Bad_Source : Boolean := False;
                  Bad_Preparation : Natural := 0;
                  Boot_Mode : Natural := 0;
                  DMA_Mode : Natural := 0;
                  Bad_Parameters : Boolean := False) is
      Header : Intel_GPU_Firmware.CSS_Header := [others => 0];
      -- Distinct transport fixtures, NOT a bootable hardware configuration.
      function Parameter_Value (Index : Natural) return Unsigned_32 is
        (case Index is
           when 0 => 16#00300003#, when 1 => 16#00444000#, when 2 => 16#4000#,
           when 3 => 16#40#, when 4 => 16#800#, when 5 => 16#46D20000#,
           when others => 0);
      Size, Base : Unsigned_32 := 0;
      Writes, Reads, Bytes, Clocks : Natural := 0;
      Boot_Reads : Natural := 0;
      DMA_Reads : Natural := 0;
      procedure Put (Offset : Natural; Value : Unsigned_32) is
      begin
         for B in 0 .. 3 loop
            Header (Offset + B) := Unsigned_8 (Shift_Right (Value, B * 8) and 255);
         end loop;
      end Put;
      function Read32 (Offset : Unsigned_32) return Unsigned_32 is
      begin
         Reads := Reads + 1;
         case Offset is
            when 16#C000# =>
               pragma Assert (Writes = 90);
               Boot_Reads := Boot_Reads + 1;
               return (case Boot_Mode is
                 when 1 => 16#40000000#,
                 when 2 => 16#80007100#,
                 when 3 => 0,
                 when 4 => Unsigned_32'Last,
                 when others => 16#8000F000#);
            when 16#C050# => return Size;
            when 16#C340# => return Base;
            when 16#C064# => return (if Bad_Preparation = 1 then 0
                                     elsif Bad_Preparation = 3 then Unsigned_32'Last
                                     else 16#8607#);
            when 16#13816C# => return (if Bad_Preparation = 2 then 0
                                       elsif Bad_Preparation = 4 then Unsigned_32'Last
                                       else 1);
            when 16#C314# =>
               DMA_Reads := DMA_Reads + 1;
               -- The boot-status fixture still advertises READY. None of
               -- these failed transfers may even consult that stale status.
               if DMA_Mode = 1 and DMA_Reads = 1 then return 1; end if;
               if DMA_Mode = 2 and DMA_Reads = 1 then
                  return Unsigned_32'Last;
               end if;
               if DMA_Mode = 3 and DMA_Reads in 2 .. 4 then return 1; end if;
               if DMA_Mode = 4 and DMA_Reads = 2 then
                  return Unsigned_32'Last;
               end if;
               if DMA_Mode = 5 and DMA_Reads = 3 then return 16#10#; end if;
               if DMA_Mode = 6 and DMA_Reads = 3 then return 1; end if;
               return 0;
            when others => raise Program_Error;
         end case;
      end Read32;
      procedure Write32 (Offset, Value : Unsigned_32; Success : out Boolean) is
      begin
         Writes := Writes + 1;
         pragma Assert (Fail_Write = 0 or Writes <= Fail_Write);
         if Writes = 1 then
            pragma Assert (Offset = 16#C180# and Value = 0);
         elsif Writes <= 15 then
            pragma Assert (Offset = 16#C180# + Unsigned_32 (Writes - 1) * 4
                           and Value = Parameter_Value (Writes - 2));
         elsif Writes = 16 then
            pragma Assert (Offset = 16#C050#); Size := Value or 1;
         elsif Writes = 17 then
            pragma Assert (Offset = 16#C340#); Base := Value or 1;
         elsif Writes = 18 then
            pragma Assert (Offset = 16#C064# and Value = 16#8607#);
         elsif Writes = 19 then
            pragma Assert (Offset = 16#13816C# and Value = 1);
         elsif Writes <= 83 then
            pragma Assert (Bytes = 256 and Offset = 16#C200# + Unsigned_32 (Writes - 20) * 4);
            if Actual_Blob then
               declare
                  I : constant Natural := 335104 + (Writes - 20) * 4;
                  Expected : constant Unsigned_32 := Unsigned_32 (Blob (I)) or
                    Shift_Left (Unsigned_32 (Blob (I + 1)), 8) or
                    Shift_Left (Unsigned_32 (Blob (I + 2)), 16) or
                    Shift_Left (Unsigned_32 (Blob (I + 3)), 24);
               begin pragma Assert (Value = Expected); end;
            end if;
         else
            pragma Assert (Bytes = 256 and Offset in 16#C300# .. 16#C314#);
         end if;
         Success := Writes /= Fail_Write;
      end Write32;
      procedure Read_Byte (Offset : Unsigned_64; Value : out Unsigned_8;
                           Success : out Boolean) is
      begin
         pragma Assert (Writes = 19 and Offset = 335104 + Unsigned_64 (Bytes));
         Bytes := Bytes + 1;
         Value := (if Actual_Blob then Blob (Natural (Offset)) else 0);
         Success := True;
      end Read_Byte;
      function Now return Unsigned_64 is
      begin
         Clocks := Clocks + 1;
         if DMA_Mode = 7 and Clocks = 2 then return Unsigned_64'Last; end if;
         if DMA_Mode = 8 and Clocks = 3 then return 99; end if;
         return 100;
      end Now;
      procedure Pause is null;
      package U is new Intel_GPU_GuC_Upload (Read32, Write32, Read_Byte, Now, Pause);
      use type U.Result;
      package P renames Intel_GPU_GuC_Parameters;
      Invalid_Parameters : P.Parameter_Block;
      pragma Warnings (Off, "*Invalid_Parameters*could be declared constant*");
      Parameters : constant P.Parameter_Block := P.Encode_ADLN
        ((Pin_Bias => 4096, ADS => P.Address (16#400000#),
          ADS_Backing_Bytes => 4096,
          Log => (Base => P.Address (16#300000#), Backing_Bytes => 16_384,
                  Notify_Half_Full => True,
                  Capture_Megabyte_Units => False, Log_Megabyte_Units => False,
                  Crash_Count => 0, Capture_Count => 0, Debug_Pages_Count => 0),
          Device => 16#46D2#, Revision => 0, Scheduler_Enabled => False, SLPC_Enabled => False,
          PXP_Enabled => False, Logging_Enabled => False, Verbosity => 0,
          Workarounds => 16#00444000#));
      Status : U.Result;
      Saved : Natural;
      function Expected_Transfer return String is
      begin
         if Bad_Source or Bad_Parameters or Bad_Preparation /= 0 or
            Fail_Write in 1 .. 83 then return "not-attempted"; end if;
         if Fail_Write > 83 then return "write-failed; backing-retained"; end if;
         return (case DMA_Mode is
            when 1 => "busy", when 2 | 4 => "invalid-mmio",
            when 3 => "timed-out; backing-retained",
            when 5 | 6 => "cleanup-failed; backing-retained",
            when 7 | 8 => "invalid-clock",
            when others => "complete");
      end Expected_Transfer;
   begin
      Put (0, 6); Put (4, 161); Put (8, 16#10000#);
      Put (16, 16#8086#); Put (24, 16#147C1#);
      Put (28, 64); Put (32, 64); Put (36, 1);
      Put (64, 16#463104#); Put (120, 16#801000#);
      if Actual_Blob then
         for I in Header'Range loop Header (I) := Blob (I); end loop;
      end if;
      U.Execute (Header, (if Bad_Parameters then Invalid_Parameters else Parameters),
                 335360, (if Bad_Source then 1 else 16#200000#),
                 1024 * 1024, 16#4000#, 16#80000#, 3, Status);
      pragma Assert (Status =
        (if Bad_Source or Bad_Parameters then U.Rejected
         elsif Bad_Preparation /= 0 then U.Preparation_Failed
         elsif DMA_Mode /= 0 then U.Transfer_Failed
         elsif Fail_Write = 0 then
           (if Boot_Mode = 0 then U.Firmware_Ready else U.Startup_Failed)
         elsif Fail_Write <= 15 then U.Parameters_Failed
         elsif Fail_Write <= 17 then U.WOPCM_Failed
         elsif Fail_Write <= 19 then U.Preparation_Failed
         elsif Fail_Write <= 83 then U.Signature_Failed else U.Transfer_Failed));
      pragma Assert (Writes = (if Bad_Source or Bad_Parameters then 0
                               elsif Bad_Preparation in 1 | 3 then 18
                               elsif Bad_Preparation in 2 | 4 then 19
                               elsif DMA_Mode in 1 | 2 | 7 then 83
                               elsif Fail_Write = 0 then 90 else Fail_Write));
      Saved := Writes + Reads + Bytes + Clocks;
      pragma Assert (U.Last_Transfer_Detail = Expected_Transfer);
      pragma Assert (Saved = Writes + Reads + Bytes + Clocks);
      pragma Assert (Boot_Reads =
        (if Bad_Source or Bad_Parameters or Bad_Preparation /= 0 or
            Fail_Write /= 0 or DMA_Mode /= 0 then 0
         elsif Boot_Mode = 3 then 3 else 1));
      if Boot_Reads > 0 then
         pragma Assert (U.Last_Startup_State =
           Intel_GPU_GuC_Status.Decode (U.Last_Startup_Raw));
      else pragma Assert (U.Last_Startup_Raw = Unsigned_32'Last); end if;
      if Bad_Source or Bad_Parameters then pragma Assert (Saved = 0); end if;
      U.Execute (Header, Parameters, 335360, 16#200000#, 1024 * 1024, 16#4000#, 16#80000#, 3, Status);
      pragma Assert (Status = U.Rejected and Saved = Writes + Reads + Bytes + Clocks);
      pragma Assert (U.Last_Transfer_Detail = Expected_Transfer);
      pragma Assert (Saved = Writes + Reads + Bytes + Clocks);
   end Run;
begin
   Check_Parameters;
   Ada.Text_IO.Put_Line ("PASS: GuC parameter encoding, revisions, flags, address bounds and reserved words");
   Run (0, True);
   Run (0, Bad_Parameters => True);
   for Failure in 0 .. 90 loop Run (Failure); end loop;
   for Failure in 1 .. 4 loop Run (0, Bad_Preparation => Failure); end loop;
   for Failure in 1 .. 4 loop Run (0, Boot_Mode => Failure); end loop;
   for Failure in 1 .. 8 loop Run (0, DMA_Mode => Failure); end loop;
   Ada.Text_IO.Put_Line ("PASS: composed DMA busy/invalid/timeout/cleanup/clock failures block stale READY and retry (8 cases)");
   Ada.Text_IO.Put_Line ("PASS: composed GuC parameters/upload/startup, all90 write failures and no retry (101 cases)");
   if Ada.Command_Line.Argument_Count = 1 then
      declare
         package IO renames Ada.Streams.Stream_IO;
         use type IO.Count;
         use type Ada.Streams.Stream_Element_Offset;
         File : IO.File_Type;
         Data : Ada.Streams.Stream_Element_Array (1 .. Blob'Length);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
         pragma Assert (IO.Size (File) = Blob'Length);
         IO.Read (File, Data, Last);
         pragma Assert (Last = Data'Last);
         IO.Close (File);
         for I in Blob'Range loop
            Blob (I) := Unsigned_8 (Data (Ada.Streams.Stream_Element_Offset (I + 1)));
         end loop;
      end;
      Actual_Blob := True;
      Run (0);
      Ada.Text_IO.Put_Line ("PASS: packaged GuC blob header, exact64 RSA register words and composed transfer");
   end if;
end GuC_Upload_Tests;
