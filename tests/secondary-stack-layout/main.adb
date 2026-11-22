------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Hosted checks of CuBit.Secondary_Stack_Layout: the thread area's layout
--  and the secondary stack's named sizes (run.sh; --prove for gnatprove).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Secondary_Stack_Layout; use CuBit.Secondary_Stack_Layout;

procedure Main is
   --  The area and its ceiling.
   pragma Compile_Time_Error (Maximum_Object_Bytes /= Chunk_Bytes + Maximum_Header_Bytes,
                              "the stack object is its static chunk and a bounded header");
   pragma Compile_Time_Error (Ceiling_Bytes mod Chunk_Bytes /= 0
                                or else Ceiling_Bytes < 2 * Chunk_Bytes,
                              "the ceiling is whole chunks, more than the static one");
   pragma Compile_Time_Error (Object_Alignment mod 16 /= 0,
                              "the stack object is aligned for its data");

   Checks, Failed : Natural := 0;
   KiB : constant := 1024;
   MiB : constant := 1024 * KiB;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failed := Failed + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   --  The stack object of the user runtime for a 64 KiB static chunk: its
   --  header (about 80 bytes) and the chunk.
   Typical_Object : constant Object_Bytes := Chunk_Bytes + 80;
begin
   --  A thread area: an 8-byte TLS segment aligned to 8.
   Check (TLS_Block_Bytes (8, 8) = 8, "TLS block: 8 bytes");
   Check (Thread_Pointer_Offset (8, 8) = 64, "thread pointer after the TLS block, 64-aligned");
   Check (Area_Prefix (8, 8) = 128, "prefix: TLS block and control block");
   Check (Object_Offset (Area_Prefix (8, 8)) = 128, "stack object after the prefix");
   Check (Area_Bytes (Area_Prefix (8, 8), Typical_Object) = 68 * KiB,
          "an area: prefix, 64 KiB chunk and header, page-rounded");
   Check (TLS_Block_Bytes (0, 1) = 0 and then Area_Prefix (0, 1) = Control_Block_Bytes,
          "no TLS: the control block alone");
   Check (TLS_Block_Bytes (24, 16) = 32 and then Thread_Pointer_Offset (24, 16) = 64,
          "TLS block rounded to its alignment, as the linker lays it out");
   Check (TLS_Block_Bytes (100, 64) = 128 and then Thread_Pointer_Offset (100, 64) = 128,
          "64-aligned TLS: thread pointer aligned");
   Check (Valid_TLS_Alignment (1) and then Valid_TLS_Alignment (8)
            and then Valid_TLS_Alignment (64),
          "power-of-two alignments up to the control block accepted");
   Check (not Valid_TLS_Alignment (0) and then not Valid_TLS_Alignment (24)
            and then not Valid_TLS_Alignment (128),
          "zero, non-power-of-two and over-large alignments refused");
   Check (Area_Prefix (Maximum_TLS_Bytes, Maximum_TLS_Alignment) <= Maximum_Prefix_Bytes,
          "the largest TLS segment fits the prefix");
   Check (Object_Offset (100) = 128 and then Object_Offset (128) = 128 and then Object_Offset (0) = 0,
          "the stack object starts at the next 64-byte boundary");

   --  Every prefix and object size: the area holds both, in whole pages,
   --  within the largest area.
   for Prefix in Prefix_Bytes range 0 .. 2 * KiB loop
      for Object in Object_Bytes range Chunk_Bytes - 1 .. Chunk_Bytes + 130 loop
         declare
            Bytes : constant Region_Bytes := Area_Bytes (Prefix, Object);
         begin
            Check (Bytes mod Page_Bytes = 0 and then Bytes >= Object_Offset (Prefix) + Object
                     and then Bytes - (Object_Offset (Prefix) + Object) < Page_Bytes,
                   "an area is the prefix and the object, page-rounded");
         end;
      end loop;
   end loop;
   Check (Area_Bytes (Maximum_Prefix_Bytes, Maximum_Object_Bytes) <= Maximum_Area_Bytes,
          "the largest area is bounded");
   Check (Area_Bytes (Maximum_Prefix_Bytes, Maximum_Object_Bytes) <= 2 * MiB,
          "the largest area is small");

   Put_Line ("secondary-stack-layout:" & Checks'Image & " checks," & Failed'Image & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
