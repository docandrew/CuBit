------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A family of settings a program may read and write (CAP-003).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Services;
with CuBit.Settings.Family;

generic
   type Value is private;
   Prefix : String;
   with function Service return CuBit.Handles.Services.Config.Endpoint;
   with procedure Decode (Source : String; Item : out Value; Success : out Boolean);
   with function Encode (Item : Value) return String;
package CuBit.Settings.Writable_Family is

   package Members is new CuBit.Settings.Family (Value, Prefix, Service, Decode);
   subtype Reading is Members.Reading;
   function Read (Member : String) return Reading renames Members.Read;
   function Setting_Prefix return String is (Prefix);

   procedure Write (Member : String; Item : Value; Result : out Change_Outcome);
   procedure Delete (Member : String; Result : out Change_Outcome);

end CuBit.Settings.Writable_Family;
