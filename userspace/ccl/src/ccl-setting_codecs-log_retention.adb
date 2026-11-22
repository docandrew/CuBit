--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Log_Retention with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Retained_Per_Source
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Retained_Per_Source_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Retained_Per_Source_Value'First), Interfaces.Integer_64 (Retained_Per_Source_Value'Last), Value);
      Item := Retained_Per_Source_Value (Value);
   end Read_Retained_Per_Source;

   procedure Read_Retained_Boot
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Retained_Boot_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Retained_Boot_Value'First), Interfaces.Integer_64 (Retained_Boot_Value'Last), Value);
      Item := Retained_Boot_Value (Value);
   end Read_Retained_Boot;

   procedure Read_Retained_Total
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Retained_Total_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Retained_Total_Value'First), Interfaces.Integer_64 (Retained_Total_Value'Last), Value);
      Item := Retained_Total_Value (Value);
   end Read_Retained_Total;

   procedure Read_Log_Retention
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Log_Retention_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Log_Retention");
      R.Expect (Source, Position, OK, " per_source => ");
      Read_Retained_Per_Source (Source, Position, OK, Item.per_source);
      R.Expect (Source, Position, OK, " boot => ");
      Read_Retained_Boot (Source, Position, OK, Item.boot);
      R.Expect (Source, Position, OK, " total => ");
      Read_Retained_Total (Source, Position, OK, Item.total);
      R.Expect (Source, Position, OK, ")");
   end Read_Log_Retention;

   procedure Decode (Source : String; Item : out Log_Retention_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Log_Retention (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Log_Retention;

