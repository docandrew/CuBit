with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body Text_Test_Support is
   Passed, Failed : Natural := 0;

   procedure Section (Title : String) is
   begin
      Put_Line ("== " & Title);
   end Section;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed := Passed + 1;
         Put_Line ("  PASS " & What);
      else
         Failed := Failed + 1;
         Put_Line ("  FAIL " & What);
      end if;
   end Check;

   procedure Note (What : String) is
   begin
      Put_Line ("  note " & What);
   end Note;

   function Failures return Natural is (Failed);

   procedure Summary is
   begin
      Put_Line (Natural'Image (Passed) & " passed," & Natural'Image (Failed) & " failed");
      if Failed > 0 then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Summary;

   function Make (Seed : Unsigned_64) return Generator is
     (State => (if Seed = 0 then 16#9E37_79B9_7F4A_7C15# else Seed * 16#2545_F491_4F6C_DD1D# + 1));

   function Random (G : in out Generator; Bound : Natural) return Natural is
   begin
      G.State := G.State xor Shift_Right (G.State, 12);
      G.State := G.State xor Shift_Left (G.State, 25);
      G.State := G.State xor Shift_Right (G.State, 27);
      if Bound = 0 then
         return 0;
      end if;
      return Natural ((G.State * 16#2545_F491_4F6C_DD1D#) / 2 ** 32 mod Unsigned_64 (Bound));
   end Random;
end Text_Test_Support;
