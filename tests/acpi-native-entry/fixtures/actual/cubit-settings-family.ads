------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A family of settings a program may read (CAP-003): every key Prefix &
--  Member (desktop.launch.<entry>, desktop.display.mode.<monitor>), all of
--  one type. Instantiated by the generated Startup.Settings package.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Services;

generic
   type Value is private;
   --  The family's prefix (it ends with '.').
   Prefix : String;
   with function Service return CuBit.Handles.Services.Config.Endpoint;
   with procedure Decode (Source : String; Item : out Value; Success : out Boolean);
package CuBit.Settings.Family is

   type Reading (Kind : Outcome := Absent) is record
      case Kind is
         when Present => Item : Value;
         when others  => null;
      end case;
   end record;

   --  The member's setting (its key is Prefix & Member).
   function Read (Member : String) return Reading;

   --  The family's prefix, for messages.
   function Setting_Prefix return String is (Prefix);

   --  Visit each member Config holds, in Config's order. Result: Present
   --  when listed (there may be none), else why not.
   generic
      with procedure Visit (Member : String);
   procedure For_Each (Result : out Outcome);

end CuBit.Settings.Family;
