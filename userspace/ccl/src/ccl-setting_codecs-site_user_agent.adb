--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.Site_User_Agent with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Agent_Presentation
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Agent_Presentation_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Agent_Presentation_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Agent_Presentation.Penny") then
         R.Expect_Word (Source, Position, OK, "Agent_Presentation.Penny");
         Item := Penny;
      elsif R.Next_Is_Word (Source, Position, "Agent_Presentation.Linux") then
         R.Expect_Word (Source, Position, OK, "Agent_Presentation.Linux");
         Item := Linux;
      elsif R.Next_Is_Word (Source, Position, "Agent_Presentation.Windows") then
         R.Expect_Word (Source, Position, OK, "Agent_Presentation.Windows");
         Item := Windows;
      elsif R.Next_Is_Word (Source, Position, "Agent_Presentation.Mac") then
         R.Expect_Word (Source, Position, OK, "Agent_Presentation.Mac");
         Item := Mac;
      else
         OK := False;
      end if;
   end Read_Agent_Presentation;

   procedure Read_Site_User_Agent
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Site_User_Agent_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Site_User_Agent");
      R.Expect (Source, Position, OK, " presentation => ");
      Read_Agent_Presentation (Source, Position, OK, Item.presentation);
      R.Expect (Source, Position, OK, ")");
   end Read_Site_User_Agent;

   procedure Decode (Source : String; Item : out Site_User_Agent_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Site_User_Agent (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Site_User_Agent;

