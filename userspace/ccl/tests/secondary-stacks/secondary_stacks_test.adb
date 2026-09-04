with Ada.Text_IO; use Ada.Text_IO;
with CCL.Secondary_Stacks;

procedure Secondary_Stacks_Test is
   package Regions is new CCL.Secondary_Stacks
     (Capacity => 32, Max_Values => 4);
   use Regions;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   Region   : Stack;
   Boundary : Stack_Mark;
   Original : String_Value;
   Empty    : String_Value;
   Temporary : String_Value;
   Reused   : String_Value;
   Result   : Operation_Result;
   Element  : Character;
   Slid     : String (10 .. 14);
   Wrong    : String (1 .. 4);
begin
   Initialize (Region);
   Allocate_String (Region, "hello", Original, Result);
   Check
     (Result = Operation_Ok and then Is_Valid (Region, Original) and then
      First_Index (Original) = 1 and then Last_Index (Original) = 5 and then
      Length (Original) = 5,
      "unconstrained value acquires definite bounds");

   Allocate_String (Region, "", Empty, Result);
   Boundary := Mark (Region);
   Check
     (Result = Operation_Ok and then Is_Valid (Region, Empty) and then
      First_Index (Empty) = 1 and then Last_Index (Empty) = 0 and then
      Length (Empty) = 0,
      "null string has Ada-like one through zero bounds");

   Copy_To (Region, Original, Slid, Result);
   Check
     (Result = Operation_Ok and then Slid = "hello",
      "equal-length assignment slides into target bounds");
   Copy_To (Region, Original, Wrong, Result);
   Check
     (Result = Length_Mismatch,
      "constrained assignment rejects a different length");

   Allocate_String
     (Region, "secret", Temporary, Result, First => 7, Sensitive => True);
   Check
     (Result = Operation_Ok and then First_Index (Temporary) = 7 and then
      Last_Index (Temporary) = 12,
      "value preserves its constructed bounds");
   Read (Region, Temporary, 8, Element, Result);
   Check
     (Result = Operation_Ok and then Element = 'e',
      "one-based indexed read uses value bounds");

   Release (Region, Boundary, Result);
   Check
     (Result = Operation_Ok and then
      Is_Valid (Region, Original) and then
      not Is_Valid (Region, Temporary),
      "release preserves older values and invalidates temporaries");

   Allocate_String (Region, "again!", Reused, Result, First => 7);
   Check
     (Result = Operation_Ok and then Is_Valid (Region, Reused) and then
      not Is_Valid (Region, Temporary),
      "generation rejects a stale descriptor after slot reuse");

   Clear (Region);
   Check
     (Used_Bytes (Region) = 0 and then Live_Values (Region) = 0 and then
      not Is_Valid (Region, Original) and then
      not Is_Valid (Region, Reused),
      "execution teardown invalidates all values");

   if Failures /= 0 then
      raise Program_Error;
   end if;
end Secondary_Stacks_Test;
