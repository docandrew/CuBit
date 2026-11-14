with Ada.Finalization;

--  Text of any length held in process memory (the heap, CuAlloc natively),
--  copied when the holder is assigned and freed with it, so it behaves as a
--  value. A result's literal is one: as long as the value it prints, never
--  cut to a fixed buffer. The visible part is SPARK; the heap is behind the
--  private part.
package CCL.Held_Texts with SPARK_Mode is
   type Text is private;
   function Empty return Text
     with Post => Length (Empty'Result) = 0;
   function To_Text (Item : String) return Text
     with Post => Length (To_Text'Result) = Item'Length;
   function Length (Item : Text) return Natural;
   --  The whole text, 1-based.
   function Image (Item : Text) return String
     with Post => Image'Result'First = 1 and then Image'Result'Length = Length (Item);
   --  Characters First .. Last ("" when they are not all in the text).
   function Slice (Item : Text; First, Last : Natural) return String;
   --  Character Index (NUL outside the text).
   function Element (Item : Text; Index : Positive) return Character;
   function "=" (Left, Right : Text) return Boolean;
private
   pragma SPARK_Mode (Off);
   type String_Access is access String;
   type Text is new Ada.Finalization.Controlled with record
      Data : String_Access;
   end record;
   overriding procedure Adjust (Item : in out Text);
   overriding procedure Finalize (Item : in out Text);
end CCL.Held_Texts;
