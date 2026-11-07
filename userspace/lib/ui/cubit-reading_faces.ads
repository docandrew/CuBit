------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The reading faces a program offers its reader (APP-005), and the one
--  chosen: what CuBit.UI's Reading_Face text is set in.
--
--  A program offers each face once, by name, with bytes that stay valid for
--  the process; Offer registers it in the next free reading slot
--  (CuBit.Fonts.Reading_1 ..). Today the faces are ccl-build `embedded`
--  blobs (the Bible app's Literata, Source Serif 4, EB Garamond); the
--  system font store (UI-034) is meant to become the other caller of Offer,
--  so a reader lists and switches faces without knowing where they live.
--  Choosing is instant: every slot keeps its own cells.
------------------------------------------------------------------------------
with System;
with CuBit.Fonts;

package CuBit.Reading_Faces is

   MAXIMUM_FACES : constant := 3;   --  CuBit.Fonts' reading slots
   pragma Compile_Time_Error
     (MAXIMUM_FACES /= CuBit.Fonts.Reading_Face'Pos (CuBit.Fonts.Reading_Face'Last) -
                       CuBit.Fonts.Reading_Face'Pos (CuBit.Fonts.Reading_Face'First) + 1,
      "one offered face per reading slot");
   MAXIMUM_NAME : constant := 32;
   subtype Face_Count is Natural range 0 .. MAXIMUM_FACES;
   subtype Face_Index is Positive range 1 .. MAXIMUM_FACES;

   type Offer_Status is
     (Offered,       --  registered in a slot; Index names it
      No_Slot,       --  every slot holds a face (a Capacity refusal)
      Bad_Name,      --  empty, or longer than MAXIMUM_NAME
      Refused,       --  the engine refused the bytes (CuBit.Fonts.Refused)
      Not_A_Font);   --  not a face the engine draws

   procedure Offer (Name : String; Bytes : System.Address; Length : Natural; Index : out Face_Index;
                    Status : out Offer_Status);

   function Count return Face_Count;
   function Name (Index : Face_Index) return String
     with Pre => Index <= Count;
   --  The face named Name, or 0.
   function Find (Name : String) return Face_Count;

   --  The reader's choice (the first offered face until one is chosen).
   procedure Choose (Index : Face_Index)
     with Pre => Index <= Count;
   function Chosen return Face_Count;
   --  The chosen face's engine face (Reading_1 when none is offered).
   function Chosen_Face return CuBit.Fonts.Reading_Face;

end CuBit.Reading_Faces;
