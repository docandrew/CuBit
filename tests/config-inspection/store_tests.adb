with Ada.Text_IO;
with Config_Store; use Config_Store;
procedure Store_Tests is
   Model : State;
   Result : Update_Result;
   Value : Value_Text;
   Found, Removed : Boolean;
   Full_Key : constant String (1 .. Maximum_Key) := [others => 'k'];
   Full_Value : constant String (1 .. Maximum_Value) := [others => 'v'];
   High_Key : constant String (Positive'Last .. Positive'Last) := "x";
   Offset_Value : constant String (75 .. 77) := "abc";
begin
   Read (Model, "absent", Value, Found);
   pragma Assert (not Found and Value.Length = 0);
   Put (Model, High_Key, Offset_Value, Result);
   pragma Assert (Result = Stored);
   Read (Model, "x", Value, Found);
   pragma Assert (Found and Value.Length = 3 and Value.Data (1 .. 3) = "abc");
   Put (Model, "x", "", Result);
   Read (Model, "x", Value, Found);
   pragma Assert (Result = Stored and Found and Value.Length = 0);
   Put (Model, "", "bad", Result);
   pragma Assert (Result = Invalid_Request);
   Put (Model, Full_Key & "!", "bad", Result);
   pragma Assert (Result = Invalid_Request);
   Put (Model, "x", Full_Value & "!", Result);
   pragma Assert (Result = Invalid_Request);
   Read (Model, "x", Value, Found);
   pragma Assert (Found and Value.Length = 0);
   Remove (Model, "x", Removed);
   pragma Assert (Removed);
   Remove (Model, "x", Removed);
   pragma Assert (not Removed);
   for I in 1 .. Maximum_Entries loop
      Put (Model, I'Image, Full_Value, Result);
      pragma Assert (Result = Stored);
   end loop;
   Put (Model, "overflow", "bad", Result);
   pragma Assert (Result = Capacity_Exceeded);
   --  Replacement must work at capacity and must not leave stale value bytes.
   Put (Model, " 1", "a", Result);
   pragma Assert (Result = Stored);
   Read (Model, " 1", Value, Found);
   pragma Assert (Found and Value.Length = 1 and Value.Data (1) = 'a');
   pragma Assert (for all I in 2 .. Maximum_Value => Value.Data (I) = ASCII.NUL);
   Remove (Model, " 1", Removed);
   pragma Assert (Removed);
   Put (Model, Full_Key, Full_Value, Result);
   pragma Assert (Result = Stored);
   Read (Model, Full_Key, Value, Found);
   pragma Assert (Found and Value.Length = Maximum_Value and Value.Data = Full_Value);
   for I in Slot loop
      pragma Assert (Key_At (Model, I).Length > 0);
   end loop;
   Ada.Text_IO.Put_Line ("PASS Config owned store: bounds, replacement, capacity, reuse and non-1 strings");
end Store_Tests;
