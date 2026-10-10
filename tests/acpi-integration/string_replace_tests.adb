with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Objects; use AML_Objects;
procedure String_Replace_Tests is
   S : State := Empty;
   Prior : State;
   ID, Other, Link, Dummy : Object_ID;
   Alloc : Allocation_Status;
   Result : String_Update_Status;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Reject (Target : Object_ID; Expected : String_Update_Status) is
   begin
      Prior := S;
      Replace_String (S, Target, [1,2], Result);
      Check (Result = Expected and then S = Prior);
   end Reject;
begin
   Reject (0, Invalid_String_ID);
   Reject (1, Invalid_String_ID);
   New_Bytes (S, String_Object, [97,98,99], ID, Alloc);
   Check (Alloc = Allocated);
   New_Bytes (S, Buffer_Object, [41,42], Other, Alloc);
   Check (Alloc = Allocated);
   New_Package (S, 2, Link, Alloc);
   Check (Alloc = Allocated);
   Set_Element (S, Link, 0, ID); Set_Element (S, Link, 1, Other);
   Reject (Other, Not_A_String); Reject (Link, Not_A_String);
   for Size in 0 .. 12 loop
      declare Data : constant Bytes (Positive'Last - Size .. Positive'Last - 1) := [others => 123];
         Before : constant Usage := Usage_Of (S);
      begin
         Replace_String (S, ID, Data, Result);
         Check (Result = String_Updated and then Valid (S));
         Check (Kind (S, ID) = String_Object and then Byte_Data (S, ID) = Data);
         Check (Element (S, Link, 0) = ID and then Element (S, Link, 1) = Other);
         Check (Byte_Data (S, Other) = [41,42]);
         Check (Usage_Of (S) = Usage'(Before.Objects, Before.Bytes + Size, Before.Elements));
      end;
   end loop;
   -- Exact byte quota and unchanged failure; full arena still accepts empty.
   New_Bytes (S, Buffer_Object, [1 .. Max_Bytes - Byte_Count (S) - 2 => 0], Dummy, Alloc);
   Check (Alloc = Allocated);
   Replace_String (S, ID, [7,8], Result);
   Check (Result = String_Updated and then Byte_Count (S) = Max_Bytes);
   Reject (ID, String_Byte_Limit);
   Replace_String (S, ID, [1 .. 0 => 0], Result);
   Check (Result = String_Updated and then Length (S, ID) = 0);
   Check (Byte_Count (S) = Max_Bytes and then Element (S, Link, 0) = ID);
   Ada.Text_IO.Put_Line ("STRING-REPLACE PASS" & Checks'Image);
end String_Replace_Tests;
