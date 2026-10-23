------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One setting a program may read and write (CAP-003): its manifest's
--  Config scope covering it grants write. Instantiated by the generated
--  Startup.Settings package.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Services;
with CuBit.Settings.Readable;

generic
   type Value is private;
   Key : String;
   with function Service return CuBit.Handles.Services.Config.Endpoint;
   with procedure Decode (Source : String; Item : out Value; Success : out Boolean);
   --  The codec's: the stored text of Item.
   with function Encode (Item : Value) return String;
package CuBit.Settings.Writable is

   package Values is new CuBit.Settings.Readable (Value, Key, Service, Decode);
   subtype Reading is Values.Reading;
   function Read return Reading renames Values.Read;
   function Setting_Key return String is (Key);

   procedure Write (Item : Value; Result : out Change_Outcome);
   procedure Delete (Result : out Change_Outcome);

end CuBit.Settings.Writable;
