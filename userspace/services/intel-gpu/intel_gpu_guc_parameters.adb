package body Intel_GPU_GuC_Parameters with SPARK_Mode is
   function Address (GPU_Byte_Offset : Unsigned_64) return GPU_Page_Address is
   begin
      if GPU_Byte_Offset = 0 or else GPU_Byte_Offset >= Runtime_GGTT_Limit or else
        GPU_Byte_Offset mod 4096 /= 0 then return (False, 0); end if;
      return (True, Unsigned_32 (GPU_Byte_Offset / 4096));
   end Address;
   function Valid (Value : GPU_Page_Address) return Boolean is (Value.Present);
   function Valid (Value : Parameter_Block) return Boolean is (Value.Present);
   function Words (Value : Parameter_Block) return Startup_Words is (Value.Data);
   function Bit (Enabled : Boolean; Position : Natural) return Unsigned_32 is
     (if Enabled then Shift_Left (1, Position) else 0);
   function Required_Log_Bytes (Config : Log_Configuration) return Unsigned_64 is
      Log_Unit : constant Unsigned_64 :=
        (if Config.Log_Megabyte_Units then 1024 * 1024 else 4096);
      Capture_Unit : constant Unsigned_64 :=
        (if Config.Capture_Megabyte_Units then 1024 * 1024 else 4096);
   begin
      -- The encoded counts are one less than the number of units. Even zero
      -- counts therefore require three data sections plus the state page.
      return 4096 + (Unsigned_64 (Config.Crash_Count) + 1) * Log_Unit +
        (Unsigned_64 (Config.Debug_Pages_Count) + 1) * Log_Unit +
        (Unsigned_64 (Config.Capture_Count) + 1) * Capture_Unit;
   end Required_Log_Bytes;
   function Encode_ADLN (Config : Configuration) return Parameter_Block is
      Result : Parameter_Block;
      -- GuC ABI flags applicable to the ADL-N/IP12.0 bring-up path:
      -- PRE_PARSER, POLLCS, ENABLE_TSC_CHECK_ON_RC6. Selection is external.
      Allowed_Workarounds : constant Unsigned_32 := 16#0044_4000#;
      ADS_Start : constant Unsigned_64 := Unsigned_64 (Config.ADS.Page) * 4096;
      Log_Start : constant Unsigned_64 := Unsigned_64 (Config.Log.Base.Page) * 4096;
   begin
      if Config.Device not in 16#46D0# .. 16#46D4# or else
        not Valid (Config.ADS) or else not Valid (Config.Log.Base) or else
        (Config.Workarounds and not Allowed_Workarounds) /= 0
      then return Result; end if;
      if Config.Pin_Bias = 0 or else Config.Pin_Bias mod 4096 /= 0 or else
        Config.Pin_Bias >= Runtime_GGTT_Limit or else
        ADS_Start < Config.Pin_Bias or else Log_Start < Config.Pin_Bias or else
        Config.ADS_Backing_Bytes = 0 or else Config.ADS_Backing_Bytes mod 4096 /= 0 or else
        Config.ADS_Backing_Bytes > Runtime_GGTT_Limit - ADS_Start or else
        Config.Log.Backing_Bytes < Required_Log_Bytes (Config.Log) or else
        Config.Log.Backing_Bytes mod 4096 /= 0 or else
        Config.Log.Backing_Bytes > Runtime_GGTT_Limit - Log_Start
      then return Result; end if;
      if ADS_Start < Log_Start + Config.Log.Backing_Bytes and then
        Log_Start < ADS_Start + Config.ADS_Backing_Bytes
      then return Result; end if;
      Result.Data (0) := Shift_Left (Config.Log.Base.Page, 12) or 1 or
        Bit (Config.Log.Notify_Half_Full, 1) or
        Bit (Config.Log.Capture_Megabyte_Units, 2) or
        Bit (Config.Log.Log_Megabyte_Units, 3) or
        Shift_Left (Unsigned_32 (Config.Log.Crash_Count), 4) or
        Shift_Left (Unsigned_32 (Config.Log.Debug_Pages_Count), 6) or
        Shift_Left (Unsigned_32 (Config.Log.Capture_Count), 10);
      Result.Data (1) := Config.Workarounds;
      Result.Data (2) := Bit (Config.PXP_Enabled, 1) or
        Bit (Config.SLPC_Enabled, 2) or Bit (not Config.Scheduler_Enabled, 14);
      Result.Data (3) := (if Config.Logging_Enabled then Unsigned_32 (Config.Verbosity)
                         else 16#40#);
      Result.Data (4) := Shift_Left (Config.ADS.Page, 1);
      Result.Data (5) := Shift_Left (Unsigned_32 (Config.Device), 16) or
        Unsigned_32 (Config.Revision);
      Result.Present := True;
      return Result;
   end Encode_ADLN;
end Intel_GPU_GuC_Parameters;
