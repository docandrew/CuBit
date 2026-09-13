package body Mixer_Control with SPARK_Mode is
   function Master_Allowed
     (Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Data : Words;
      Caller, Control_Authority : Unsigned_64) return Boolean is
   begin
      if Control_Authority = 0 or else Caller /= Control_Authority or else
        Flags /= 0 or else Reserved /= 0 or else Data (2) /= 0 or else Data (3) /= 0
      then return False; end if;
      case Label is
         when 16#0507# =>
            return Length = 0 and then Data (0) = 0 and then Data (1) = 0;
         when 16#0508# =>
            return Length = 2 and then Data (0) <= 100 and then Data (1) <= 1;
         when others => return False;
      end case;
   end Master_Allowed;

   function Allowed
     (Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Data : Words; Caller : Unsigned_64;
      Owners : Owner_Table) return Boolean is
   begin
      if Caller = 0 or else Caller = Unsigned_64'Last or else
        Flags /= 0 or else Reserved /= 0 or else Data (2) /= 0 or else Data (3) /= 0
      then
         return False;
      end if;
      if Label = 16#0500# then
         --  Only the format actually implemented by the mixing engine.
         return Length = 2 and then Data (0) = 48_000 and then Data (1) = 2;
      end if;
      if Owners'Length = 0 or else Data (0) < Unsigned_64 (Owners'First) or else
        Data (0) > Unsigned_64 (Owners'Last)
      then
         return False;
      end if;
      if Owners (Natural (Data (0))) /= Caller then
         return False;
      end if;
      case Label is
         when 16#0501# | 16#0503# => -- Close / get volume
            return Length = 1 and then Data (1) = 0;
         when 16#0502# => -- Volume, 0 .. 2 in unsigned 16.16
            return Length = 2 and then Data (1) <= 16#2_0000#;
         when 16#0504# => -- Pan, 0 .. 1 in unsigned 16.16
            return Length = 2 and then Data (1) <= 16#1_0000#;
         when others =>
            return False;
      end case;
   end Allowed;
end Mixer_Control;
