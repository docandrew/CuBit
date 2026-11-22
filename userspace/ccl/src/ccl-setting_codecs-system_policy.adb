--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.System_Policy with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Debugging
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Debugging_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Debugging_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Debugging.Disabled") then
         R.Expect_Word (Source, Position, OK, "Debugging.Disabled");
         Item := Disabled;
      elsif R.Next_Is_Word (Source, Position, "Debugging.Production") then
         R.Expect_Word (Source, Position, OK, "Debugging.Production");
         Item := Production;
      elsif R.Next_Is_Word (Source, Position, "Debugging.Development") then
         R.Expect_Word (Source, Position, OK, "Debugging.Development");
         Item := Development;
      else
         OK := False;
      end if;
   end Read_Debugging;

   procedure Read_System_Policy
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out System_Policy_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(System_Policy");
      R.Expect (Source, Position, OK, " debugging => ");
      Read_Debugging (Source, Position, OK, Item.debugging);
      R.Expect (Source, Position, OK, ")");
   end Read_System_Policy;

   procedure Decode (Source : String; Item : out System_Policy_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_System_Policy (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.System_Policy;

