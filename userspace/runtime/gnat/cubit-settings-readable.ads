------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One setting a program may read (CAP-003): instantiated by its generated
--  Startup.Settings package with the setting's catalog key and codec.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Services;

generic
   type Value is private;
   --  The setting's key (userspace/ccl/interfaces/settings/).
   Key : String;
   --  The program's Config endpoint (its generated Startup function).
   with function Service return CuBit.Handles.Services.Config.Endpoint;
   --  The codec's: Success False for a stored text that is not a Value.
   with procedure Decode (Source : String; Item : out Value; Success : out Boolean);
package CuBit.Settings.Readable is

   type Reading (Kind : Outcome := Absent) is record
      case Kind is
         when Present => Item : Value;
         when others  => null;
      end case;
   end record;

   function Read return Reading;

   --  The setting's key, for messages.
   function Setting_Key return String is (Key);

end CuBit.Settings.Readable;
