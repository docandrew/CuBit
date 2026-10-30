------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's settings, typed (CAP-003, docs/declaration-checking.md
--  section 1).
--
--  @description
--  A program reads and writes Config only through its generated
--  Startup.Settings package: one instance of CuBit.Settings.Readable,
--  Writable or Family for each setting its manifest's Config scopes cover
--  in the setting catalog (userspace/ccl/interfaces/settings/), with that
--  setting's key and codec. A key the manifest does not declare has no
--  accessor, so using one does not compile. Only a manifest that declares
--  config-all also gets an Inspector (CuBit.Settings.Inspection), which
--  takes keys as text.
--
--  Every read says what happened: Present (with the value), Absent (no
--  value: the program's own default applies), Denied (Config refused:
--  a scope the declaration lacks; logged once by config.svc), Invalid (the
--  stored value is not one of the setting's type) or Unavailable (no
--  Config). Only Absent means "use the default".
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Config_Protocol;

package CuBit.Settings is

   type Outcome is (Present, Absent, Denied, Invalid, Unavailable);

   --  A write or delete: Done, or why not.
   type Change_Outcome is (Done, Not_Present, Refused, Too_Long, No_Config);

   --  A value as Config stores it.
   Maximum_Text : constant := CuBit.Config_Protocol.Maximum_Value;
   subtype Text_Length is Natural range 0 .. Maximum_Text;
   type Text is record
      Length : Text_Length := 0;
      Data   : String (1 .. Maximum_Text) := [others => ' '];
   end record;
   function Image (Item : Text) return String is (Item.Data (1 .. Item.Length));
   function To_Text (Item : String) return Text
   with Pre => Item'Length <= Maximum_Text;

end CuBit.Settings;
