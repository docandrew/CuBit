--  Hosted tests of CCL.Secondary_Arrays through a small Integer instance.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Integer_Array_Types; use Integer_Array_Types;
with Integer_Arrays; use Integer_Arrays;

procedure Main is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then Put_Line ("PASS " & Name);
      else Put_Line ("FAIL " & Name); Failures := Failures + 1; end if;
   end Check;
   S : Stack;
   A, B, C, D : Array_Value;
   R : Operation_Result;
   E : Integer_64;
   M : Stack_Mark;
   Out3 : Integer_Array (5 .. 7);
begin
   Initialize (S);
   Allocate (S, [10, 20, 30], A, R);
   Check (R = Operation_Ok and Length (A) = 3 and First_Index (A) = 1 and
          Last_Index (A) = 3, "allocate with bounds 1 .. 3");
   Read (S, A, 2, E, R);
   Check (R = Operation_Ok and E = 20, "read an element");
   Read (S, A, 4, E, R);
   Check (R = Invalid_Bounds, "reject an index past the bounds");
   Copy_To (S, A, Out3, R);
   Check (R = Operation_Ok and Out3 = [10, 20, 30], "slide into other bounds");
   M := Mark (S);
   Allocate (S, [1, 2, 3, 4, 5], B, R);
   Check (R = Operation_Ok and Used_Bytes (S) = 8, "second value");
   Allocate (S, [1 .. 9 => 7], C, R);
   Check (R = Storage_Full, "region capacity is enforced");
   Release (S, M, R);
   Check (R = Operation_Ok and not Is_Valid (S, B) and Is_Valid (S, A),
          "release invalidates newer values only");
   Allocate (S, [9, 9], D, R);
   Check (R = Operation_Ok and not Is_Valid (S, B),
          "a stale descriptor stays invalid after reuse");
   Allocate (S, [], C, R);
   Check (R = Operation_Ok and Length (C) = 0 and Last_Index (C) = 0,
          "empty array");
   Clear (S);
   Reserve (S, 4, A, R);
   Check (R = Operation_Ok and Length (A) = 4 and Used_Bytes (S) = 4, "reserve");
   Write (S, A, 1, 5, R);
   Write (S, A, 2, 6, R);
   Check (R = Operation_Ok, "write in place");
   Write (S, A, 5, 7, R);
   Check (R = Invalid_Bounds, "write past the bounds");
   Shrink (S, A, 2, R);
   Check (R = Operation_Ok and Length (A) = 2 and Used_Bytes (S) = 2,
          "the newest value shrinks in place");
   Read (S, A, 2, E, R);
   Check (R = Operation_Ok and E = 6, "shrinking keeps the prefix");
   Reserve (S, 3, B, R);
   Write (S, B, 1, 8, R);
   Allocate (S, [1], C, R);
   Shrink (S, B, 1, R);
   Check (R = Operation_Ok and Length (B) = 1 and Used_Bytes (S) = 7,
          "an older value is copied when shrunk");
   Read (S, B, 1, E, R);
   Check (R = Operation_Ok and E = 8, "the copy keeps the prefix");
   Shrink (S, B, 2, R);
   Check (R = Invalid_Bounds, "shrink cannot grow");
   Clear (S);
   Check (Used_Bytes (S) = 0 and not Is_Valid (S, A), "clear");
   if Failures = 0 then Put_Line ("ccl-secondary-arrays: all tests passed");
   else Put_Line ("ccl-secondary-arrays:" & Failures'Image & " failures"); end if;
end Main;
