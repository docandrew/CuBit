--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Key_Repeat with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Repeat_Delay
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Repeat_Delay_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Repeat_Delay_Value'First), Interfaces.Integer_64 (Repeat_Delay_Value'Last), Value);
      Item := Repeat_Delay_Value (Value);
   end Read_Repeat_Delay;

   procedure Read_Repeat_Rate
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Repeat_Rate_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Repeat_Rate_Value'First), Interfaces.Integer_64 (Repeat_Rate_Value'Last), Value);
      Item := Repeat_Rate_Value (Value);
   end Read_Repeat_Rate;

   procedure Read_Key_Repeat
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Key_Repeat_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Key_Repeat");
      R.Expect (Source, Position, OK, " enabled => ");
      R.Read_Boolean (Source, Position, OK, Item.enabled);
      R.Expect (Source, Position, OK, " delay_ms => ");
      Read_Repeat_Delay (Source, Position, OK, Item.delay_ms);
      R.Expect (Source, Position, OK, " rate_hz => ");
      Read_Repeat_Rate (Source, Position, OK, Item.rate_hz);
      R.Expect (Source, Position, OK, ")");
   end Read_Key_Repeat;

   procedure Decode (Source : String; Item : out Key_Repeat_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Key_Repeat (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Key_Repeat;

