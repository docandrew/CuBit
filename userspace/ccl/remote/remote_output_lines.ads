------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Whole lines from a stream of text chunks: a build's outlets as
--  ccl-remote keeps them (Remote_Jobs), one line per line written.
--
--  @description
--  An outlet ring delivers what the program wrote in whatever pieces the
--  reader finds; a program writing its standard error byte by byte gives
--  one-byte pieces. Feed keeps the partial line back until its line feed
--  arrives, and hands Emit each complete line (without the line feed);
--  Finish hands over a last line that never got one, when the program
--  ended. A line longer than Maximum_Line_Bytes (a named ceiling) is
--  handed over in pieces of that length, each one a line.
------------------------------------------------------------------------------
pragma Ada_2022;

package Remote_Output_Lines with SPARK_Mode is

   Maximum_Line_Bytes : constant := 4_096;
   subtype Line_Length is Natural range 0 .. Maximum_Line_Bytes;

   type Assembler is private;

   --  Bytes of the partial line held back.
   function Pending (A : Assembler) return Line_Length;

   --  Nothing held back: a new run of the program.
   procedure Reset (A : out Assembler)
   with Post => Pending (A) = 0;

   generic
      with procedure Emit (Line : String);
   procedure Feed (A : in out Assembler; Chunk : String);

   generic
      with procedure Emit (Line : String);
   procedure Finish (A : in out Assembler)
   with Post => Pending (A) = 0;

private
   type Assembler is record
      Data : String (1 .. Maximum_Line_Bytes) := [others => ' '];
      Length : Line_Length := 0;
   end record;

   function Pending (A : Assembler) return Line_Length is (A.Length);
end Remote_Output_Lines;
