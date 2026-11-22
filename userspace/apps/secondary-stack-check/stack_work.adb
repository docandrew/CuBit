------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Exceptions;
with System.Storage_Elements; use System.Storage_Elements;
pragma Warnings (Off, "internal GNAT unit");
with System.Secondary_Stack;
pragma Warnings (On, "internal GNAT unit");
with CuBit.Thread_Areas;

package body Stack_Work is

   package SS renames System.Secondary_Stack;
   use type SS.Mark_Id;
   use type System.Address;

   Word_Bytes : constant := 8;
   type Frame_Words is array (1 .. Frame_Bytes / Word_Bytes) of Unsigned_64;

   --  Distinct per seed and position; never all one value.
   Byte_Prime : constant := 251;
   Seed_Step  : constant := 131;
   Index_Step : constant := 7;
   Word_Mix   : constant := 16#9E37_79B9_7F4A_7C15#;

   --  A request no thread's stack may ever hold (the ceiling is 256 MiB).
   Past_Ceiling_Bytes : constant := Natural'Last;
   --  A small result after a refusal.
   After_Refusal_Bytes : constant := 4 * KiB;

   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));

   function Byte_At (Seed : Unsigned_64; Index : Positive) return Unsigned_8 is
     (Unsigned_8 ((Seed * Seed_Step + Unsigned_64 (Index) * Index_Step) mod Byte_Prime));

   function Word_At (Seed : Unsigned_64; Index : Positive) return Unsigned_64 is
     ((Seed + Unsigned_64 (Index)) * Word_Mix);

   function Pattern (Seed : Unsigned_64; Length : Natural) return Byte_Array is
   begin
      return Result : Byte_Array (1 .. Length) do
         for I in Result'Range loop
            Result (I) := Byte_At (Seed, I);
         end loop;
      end return;
   end Pattern;

   --  Two results, joined into a third on the secondary stack.
   function Join (Left, Right : Byte_Array) return Byte_Array with No_Inline;
   function Join (Left, Right : Byte_Array) return Byte_Array is
   begin
      return Left & Right;
   end Join;

   function Matches (Item : Byte_Array; Seed : Unsigned_64) return Boolean is
     (for all I in Item'Range => Item (I) = Byte_At (Seed, I - Item'First + 1));

   function Is_Capacity (Message, Reason : String) return Boolean is
      Head : constant String := SS.Exhaustion_Prefix & Reason & " ";
   begin
      return Message'Length > Head'Length
        and then Message (Message'First .. Message'First + Head'Length - 1) = Head;
   end Is_Capacity;

   --  SS_Info's lines, one of which is "  Number of Chunks        : N".
   Chunks_Label : constant String := "  Number of Chunks";

   function Chunk_Count return Natural is
      Count : Natural := 0;
      procedure Take (Line : String);
      procedure Take (Line : String) is
         Seen_Digit : Boolean := False;
      begin
         if Line'Length > Chunks_Label'Length
           and then Line (Line'First .. Line'First + Chunks_Label'Length - 1) = Chunks_Label
         then
            for C of Line (Line'First + Chunks_Label'Length .. Line'Last) loop
               if C in '0' .. '9' then
                  Count := Count * 10 + (Character'Pos (C) - Character'Pos ('0'));
                  Seen_Digit := True;
               elsif Seen_Digit then
                  exit;
               end if;
            end loop;
         end if;
      end Take;
      procedure Info is new SS.SS_Info (Take);
   begin
      Info;
      return Count;
   end Chunk_Count;

   --  At the deepest frame: results on the secondary stack, checked, and
   --  where they and the thread's stack object are.
   function Secondary (Seed : Unsigned_64; Found : in out Outcome) return Boolean
   with No_Inline;
   function Secondary (Seed : Unsigned_64; Found : in out Outcome) return Boolean is
      First : Byte_Array renames Pattern (Seed, Result_Bytes);
      Both : Byte_Array renames
        Join (Pattern (Seed + 1, Result_Bytes), Pattern (Seed + 2, Result_Bytes));
   begin
      Found.Result_Address := Value_Of (Both'Address);
      Found.Stack := Value_Of (CuBit.Thread_Areas.Current_Stack.all'Address);
      return First'Length = Result_Bytes
        and then Both'Length = 2 * Result_Bytes
        and then Matches (First, Seed)
        and then Matches (Both (Both'First .. Both'First + Result_Bytes - 1), Seed + 1)
        and then Matches (Both (Both'First + Result_Bytes .. Both'Last), Seed + 2);
   end Secondary;

   --  One primary-stack frame of Frame_Bytes, filled, then checked after
   --  the deeper frames and the secondary-stack work have run. Volatile so
   --  the frame really is written and read.
   function Deep (Level : Natural; Seed : Unsigned_64; Found : in out Outcome) return Boolean
   with No_Inline;
   function Deep (Level : Natural; Seed : Unsigned_64; Found : in out Outcome) return Boolean is
      Frame : Frame_Words with Volatile;
      Frame_Seed : constant Unsigned_64 := Seed + Unsigned_64 (Level) * Frame_Bytes;
      Intact : Boolean;
   begin
      for I in Frame'Range loop
         Frame (I) := Word_At (Frame_Seed, I);
      end loop;
      Intact := (if Level = 0 then Secondary (Seed, Found) else Deep (Level - 1, Seed, Found));
      for I in Frame'Range loop
         if Frame (I) /= Word_At (Frame_Seed, I) then
            Intact := False;
         end if;
      end loop;
      return Intact;
   end Deep;

   function Run (Seed : Unsigned_64) return Outcome is
      Found : Outcome;
   begin
      Found.Passed := Deep (Frames - 1, Seed, Found);
      return Found;
   end Run;

   --  Growth_Items allocations of Growth_Item_Bytes held at once, each
   --  taking a chunk of its own, every one filled and checked; then the
   --  release, and the same again, which must reuse the same chunks.
   procedure Grow_Across_Chunks (Seed : Unsigned_64; Found : in out Outcome) is
      type Addresses is array (1 .. Growth_Items) of System.Address;
      First_Pass, Second_Pass : Addresses;
      Words : constant := Growth_Item_Bytes / Word_Bytes;
      type Item_Words is array (1 .. Words) of Unsigned_64;

      procedure Grow (Pass : out Addresses; Pass_Seed : Unsigned_64; Intact : out Boolean) is
      begin
         Intact := True;
         for K in Pass'Range loop
            SS.SS_Allocate (Pass (K), Growth_Item_Bytes);
            declare
               Item : Item_Words with Import, Address => Pass (K);
            begin
               for W in Item'Range loop
                  Item (W) := Word_At (Pass_Seed + Unsigned_64 (K) * Words, W);
               end loop;
            end;
         end loop;
         for K in Pass'Range loop
            declare
               Item : constant Item_Words with Import, Address => Pass (K);
            begin
               for W in Item'Range loop
                  if Item (W) /= Word_At (Pass_Seed + Unsigned_64 (K) * Words, W) then
                     Intact := False;
                  end if;
               end loop;
            end;
         end loop;
      end Grow;

      Mark : constant SS.Mark_Id := SS.SS_Mark;
      Before : constant Natural := Chunk_Count;
      Intact, Again : Boolean;
      Grown_Chunks : Natural;
   begin
      Grow (First_Pass, Seed, Intact);
      Grown_Chunks := Chunk_Count;
      SS.SS_Release (Mark);
      Found.Chunks := Grown_Chunks;
      Found.Grown := Intact
        and then Grown_Chunks >= Before + Growth_Items - 1
        and then SS.SS_Mark = Mark
        and then SS.SS_Get_Max >= Long_Long_Integer (Growth_Items) * Growth_Item_Bytes;

      Grow (Second_Pass, Seed + 1, Again);
      Found.Reused := Again
        and then Chunk_Count = Grown_Chunks
        and then (for all K in Addresses'Range => Second_Pass (K) = First_Pass (K));
      SS.SS_Release (Mark);
   end Grow_Across_Chunks;

   --  A result past the ceiling: Storage_Error with the CAPACITY message,
   --  raised before the stack changed; then a small result works.
   procedure Refuse_Past_Ceiling (Seed : Unsigned_64; Found : in out Outcome) is
      Mark : constant SS.Mark_Id := SS.SS_Mark;
      Raised : Boolean := False;
      Message : String (1 .. 200) := [others => ' '];
      Message_Length : Natural := 0;
   begin
      begin
         declare
            Huge : Byte_Array renames Pattern (Seed, Past_Ceiling_Bytes);
         begin
            --  Never reached: the request is refused.
            Found.Refused := Huge'Length = 0;
         end;
      exception
         when E : Storage_Error =>
            Raised := True;
            declare
               Text : constant String := Ada.Exceptions.Exception_Message (E);
            begin
               Message_Length := Natural'Min (Text'Length, Message'Length);
               Message (1 .. Message_Length) := Text (Text'First .. Text'First + Message_Length - 1);
            end;
      end;
      Found.Refused := Raised
        and then Is_Capacity (Message (1 .. Message_Length), "CEILING_REACHED")
        and then SS.SS_Mark = Mark;
      declare
         Small : Byte_Array renames Pattern (Seed + 3, After_Refusal_Bytes);
      begin
         Found.Continued := Small'Length = After_Refusal_Bytes and then Matches (Small, Seed + 3);
      end;
   end Refuse_Past_Ceiling;

   function Full_Run (Seed : Unsigned_64) return Outcome is
      Found : Outcome := Run (Seed);
   begin
      --  The refusal first: on its way it frees the chunks Run left for
      --  reuse (too small for it), so the growth starts from the static
      --  chunk alone.
      Refuse_Past_Ceiling (Seed, Found);
      Grow_Across_Chunks (Seed, Found);
      return Found;
   end Full_Run;

end Stack_Work;
