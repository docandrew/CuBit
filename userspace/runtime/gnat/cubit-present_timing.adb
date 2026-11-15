pragma Ada_2022;
package body CuBit.Present_Timing with SPARK_Mode is
   Source_Mask : constant Unsigned_64 := 16#FF#;
   Period_Mask : constant Unsigned_64 := Maximum_Period_Us;
   Lag_Mask : constant Unsigned_64 := Maximum_Lag_Us;
   Low_Mask : constant Unsigned_64 := Low_Modulus - 1;
   --  Reply word 3.
   Reply_Period_Shift : constant := 8;
   Reply_Lag_Shift : constant := 32;
   --  Frame result high parts.
   Source_Bits : constant := 2;
   Result_Source_Mask : constant Unsigned_64 := 2 ** Source_Bits - 1;
   Result_Period_Shift : constant := Enumeration_Bits + Source_Bits;
   Result_Lag_Shift : constant := Enumeration_Bits;
   Result_Time_Shift : constant := Low_Bits;

   procedure Decode_Source (Rep : Unsigned_64; Item : out Latch_Source; Known : out Boolean);
   procedure Decode_Source (Rep : Unsigned_64; Item : out Latch_Source; Known : out Boolean) is
   begin
      Known := True;
      case Rep is
         when 0 => Item := None;
         when 1 => Item := Hardware_Latch;
         when 2 => Item := Host_Fence;
         when others =>
            Item := None;
            Known := False;
      end case;
   end Decode_Source;

   function Encode_Reply (Item : Timing) return Reply_Words is
     [Item.Armed_Us, Item.Latched_Us,
      Unsigned_64 (Latch_Source'Enum_Rep (Item.Source)) or
      Shift_Left (Item.Period, Reply_Period_Shift) or
      Shift_Left (Item.Observed_Us - Item.Latched_Us, Reply_Lag_Shift)];

   function Decode_Reply (Words : Reply_Words) return Timing is
      Source : Latch_Source;
      Period : constant Unsigned_64 := Shift_Right (Words (3), Reply_Period_Shift) and Period_Mask;
      Lag : constant Unsigned_64 := Shift_Right (Words (3), Reply_Lag_Shift);
      Known : Boolean;
      Result : Timing;
   begin
      Decode_Source (Words (3) and Source_Mask, Source, Known);
      if not Known or else Source = None or else Lag > Maximum_Lag_Us or else
        (Shift_Right (Words (3), Reply_Period_Shift + Period_Bits) and
         (2 ** (Reply_Lag_Shift - Reply_Period_Shift - Period_Bits) - 1)) /= 0 or else
        Words (1) > Words (2) or else Words (2) > Unsigned_64'Last - Lag or else
        (Source = Host_Fence and then Period /= 0)
      then
         return No_Timing;
      end if;
      Result := (Source, Words (1), Words (2), Words (2) + Lag, Period);
      return Result;
   end Decode_Reply;

   function To_Compact (Item : Timing) return Compact is
     (if Item.Source = None then No_Compact
      else (Item.Source, Item.Period, Item.Observed_Us - Item.Latched_Us,
            Low (Item.Armed_Us), Low (Item.Latched_Us)));

   function Expand (Item : Compact; Now : Unsigned_64) return Timing is
      Latched : Unsigned_64;
   begin
      if Item.Source = None then
         return No_Timing;
      end if;
      Latched := Full (Item.Latched_Low, Now);
      if Latched > Unsigned_64'Last - Item.Lag then
         return No_Timing;
      end if;
      return (Item.Source, Full (Item.Armed_Low, Latched), Latched, Latched + Item.Lag,
              Item.Period);
   end Expand;

   function Pack (Item : Compact) return Packed_Pair is
     ((Outcome_High =>
         Shift_Left (Unsigned_64 (Latch_Source'Enum_Rep (Item.Source)), Enumeration_Bits) or
         Shift_Left (Item.Period, Result_Period_Shift) or
         Shift_Left (Item.Armed_Low, Result_Time_Shift),
       Buffer_High =>
         Shift_Left (Item.Lag, Result_Lag_Shift) or
         Shift_Left (Item.Latched_Low, Result_Time_Shift)));

   function Unpack (Item : Packed_Pair) return Compact is
      Source : Latch_Source;
      Known : Boolean;
      Result : Compact;
   begin
      Decode_Source (Shift_Right (Item.Outcome_High, Enumeration_Bits) and Result_Source_Mask,
                     Source, Known);
      if not Known or else Source = None then
         return No_Compact;
      end if;
      Result :=
        (Source => Source,
         Period => Shift_Right (Item.Outcome_High, Result_Period_Shift) and Period_Mask,
         Lag => Shift_Right (Item.Buffer_High, Result_Lag_Shift) and Lag_Mask,
         Armed_Low => Shift_Right (Item.Outcome_High, Result_Time_Shift) and Low_Mask,
         Latched_Low => Shift_Right (Item.Buffer_High, Result_Time_Shift) and Low_Mask);
      if Source = Host_Fence and then Result.Period /= 0 then
         return No_Compact;
      end if;
      return Result;
   end Unpack;
end CuBit.Present_Timing;
