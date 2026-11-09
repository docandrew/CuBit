------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Reading a system generation's record and the current pointer back: the
--  record is checked by the CCL type checker against Schema
--  (CCL.Typed_Values), never by a hand-written reader. One reader for
--  every user: ccl-remote's activations (Remote_Generations), procmgr at
--  boot (Boot_Generations) and the installer's read-back check.
------------------------------------------------------------------------------
pragma Ada_2022;

package System_Generation_Records.Reading is

   --  Named ceiling on the generation numbers kept (Remote_Generations
   --  makes at most this many; a pointer past it does not read).
   Maximum_Generations : constant := 256;
   subtype Kept_Generation is Generation_Number range Image_Generation .. Maximum_Generations;

   --  A pin's service: a startup-plan entry's name (name characters).
   function Valid_Service_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Service_Name_Bytes
      and then (for all C of Name =>
                  C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '+'));

   --  The current pointer's text (its trailing line breaks ignored): the
   --  decimal number of a generation, 0 .. Maximum_Generations, with no
   --  sign, space or leading zero (0 alone names the image's).
   procedure Parse_Current (Text : String; N : out Kept_Generation; Valid : out Boolean);

   --  Generation Expected's record text (trailing line breaks ignored). Valid:
   --  it checks against System_Generation, its number is Expected, its
   --  previous is below it, it has at most Maximum_Pins pins, each service
   --  is a valid name pinned once and each executable is a lowercase SHA-256.
   procedure Parse
     (Text : String; Expected : Generation_Number; G : out Generation_Value; Valid : out Boolean)
     with Pre => Expected /= Image_Generation;

   --  The pointer's text for N, as written ("3" and a line break).
   function Current_Text (N : Kept_Generation) return String;

end System_Generation_Records.Reading;
