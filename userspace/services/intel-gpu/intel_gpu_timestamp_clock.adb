package body Intel_GPU_Timestamp_Clock with SPARK_Mode is
   function Timestamp_Hz (Mode : CTC_Mode; Config : RPM_CONFIG0;
                          Divider : Timestamp_Override)
     return Interfaces.Unsigned_32
   is
      use Interfaces;
   begin
      if Mode.Divide_Logic = 0 then
         return Crystal_Timestamp_Hz (Config);
      end if;
      return (Unsigned_32 (Divider.Divider) + 1) * 1_000_000 +
        1_000_000 / (Unsigned_32 (Divider.Denominator) + 1);
   end Timestamp_Hz;

   function Same_Clock
     (Mode_A, Mode_B : CTC_Mode; Config_A, Config_B : RPM_CONFIG0;
      Divider_A, Divider_B : Timestamp_Override) return Boolean is
     (Mode_A.Divide_Logic = Mode_B.Divide_Logic and then
      (if Mode_A.Divide_Logic = 0 then
         Config_A.CTC_Shift = Config_B.CTC_Shift and then
         Config_A.Crystal_Selector = Config_B.Crystal_Selector
       else Divider_A.Divider = Divider_B.Divider and then
         Divider_A.Denominator = Divider_B.Denominator));

   function Crystal_Timestamp_Hz (Config : RPM_CONFIG0)
     return Interfaces.Unsigned_32
   is
      use Interfaces;
      Crystal : Unsigned_32;
   begin
      case Config.Crystal_Selector is
         when 0 => Crystal := 24_000_000;
         when 1 => Crystal := 19_200_000;
         when 2 => Crystal := 38_400_000;
         when 3 => Crystal := 25_000_000;
         when others => return 0;
      end case;
      return Shift_Right (Crystal, 3 - Natural (Config.CTC_Shift));
   end Crystal_Timestamp_Hz;
end Intel_GPU_Timestamp_Clock;
