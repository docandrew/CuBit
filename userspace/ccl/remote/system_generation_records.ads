------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A system generation's record (Remote_Generations): its CCL type and its
--  text. Pure, so the hosted test (tests/service-switch) checks the same
--  text against the same type the device writes and reads.
--
--  @description
--    (System_Generation number => 2 previous => 1
--       pins => [(Service_Pin service => "acpi.svc" executable => "<sha256 hex>")])
--  number: the generation (1 ..); previous: the generation that was
--  current when it was made (0: the boot image's); pins: the programs it
--  runs instead of the image's, by their bytes' SHA-256 (the storehouse's
--  executables/<sha256>), one per startup-plan service.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package System_Generation_Records with Pure is

   Schema : constant String :=
     "(type Service_Pin (record (service String) (executable String))) " &
     "(type System_Generation (record (number Integer) (previous Integer) (pins (List Service_Pin))))";
   Root_Name : constant String := "System_Generation";
   --  Fields, in declaration order.
   Number_Field : constant := 1;
   Previous_Field : constant := 2;
   Pins_Field : constant := 3;
   Pin_Service_Field : constant := 1;
   Pin_Executable_Field : constant := 2;

   --  Named ceilings: pins in one generation; a service's name (a
   --  startup-plan executable, CCL.Configurations).
   Maximum_Pins : constant := 16;
   Maximum_Service_Name_Bytes : constant := 64;

   Digest_Bytes : constant := 32;
   subtype Digest_Index is Positive range 1 .. Digest_Bytes;
   type Digest is array (Digest_Index) of Unsigned_8;
   Hex_Length : constant := 2 * Digest_Bytes;

   subtype Generation_Number is Unsigned_32;
   Image_Generation : constant Generation_Number := 0;

   type Pin is record
      Service : String (1 .. Maximum_Service_Name_Bytes) := [others => ' '];
      Length  : Natural range 0 .. Maximum_Service_Name_Bytes := 0;
      Program : Digest := [others => 0];
   end record;
   subtype Pin_Count is Natural range 0 .. Maximum_Pins;
   type Pin_Array is array (1 .. Maximum_Pins) of Pin;
   type Generation_Value is record
      Number   : Generation_Number := Image_Generation;
      Previous : Generation_Number := Image_Generation;
      Count    : Pin_Count := 0;
      Pins     : Pin_Array;
   end record;

   function Hex (Item : Digest) return String
     with Post => Hex'Result'Length = Hex_Length;
   procedure From_Hex (Text : String; Item : out Digest; Valid : out Boolean);

   --  The record's text: one System_Generation value, no line break.
   function Record_Text (G : Generation_Value) return String;

   --  Where Service's pin is in G (0: it has none).
   function Pin_Of (G : Generation_Value; Service : String) return Natural;
end System_Generation_Records;
