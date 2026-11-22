with Ada.Text_IO;
with Compositor_Input_Delivery;
with Compositor_Input_Delivery_Pool;
with Compositor_Input_Batch_Wire;
with CuBit.Grant_References;
procedure Input_Delivery_Pool_Tests is
   package W renames Compositor_Input_Batch_Wire;
   package GR renames CuBit.Grant_References;
   use type W.Word;
   use type GR.Reference;
   Acquire_Count, Write_Count, Return_Count : Natural := 0;
   Last_Return : GR.Reference;
   Allowed : array (1 .. 4) of Boolean := [others => False];
   procedure Acquire
     (Owner : W.Identity; Grant : GR.Reference;
      Mapping : out W.Word; Acquired : out Boolean) is
   begin
      pragma Assert (Owner in 1 .. 2 and Grant.slot in 1 .. 4);
      Acquire_Count := Acquire_Count + 1; Mapping := 4096; Acquired := True;
   end Acquire;
   procedure Write
     (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean) is
      pragma Unreferenced (Payload);
   begin
      pragma Assert (Mapping = 4096);
      Write_Count := Write_Count + 1; Written := True;
   end Write;
   procedure Return_Loan (Grant : GR.Reference; Confirmed : out Boolean) is
   begin
      Return_Count := Return_Count + 1; Last_Return := Grant;
      Confirmed := Allowed (Integer (Grant.slot));
   end Return_Loan;
   package D is new Compositor_Input_Delivery (Acquire, Write, Return_Loan);
   package P is new Compositor_Input_Delivery_Pool (D);
   use type D.Outcome;
   use type P.State;
   subtype Pool is P.State (3);
   subtype Slots is P.Slot range 1 .. 3;
   S : Pool;
   Result : D.Outcome;
   Payload : W.Snapshot_Words := [others => 0];
begin
   P.Publish (S, 1, 10, (1, 1), Payload, Result);
   pragma Assert (Result = D.Quarantined);
   P.Publish (S, 1, 11, (2, 1), Payload, Result);
   pragma Assert (Result = D.Quarantined);
   P.Publish (S, 2, 10, (3, 1), Payload, Result);
   pragma Assert (Result = D.Quarantined and Acquire_Count = 3 and Write_Count = 3);
   for I in Slots loop
      pragma Assert (P.Pending (S, I) and P.Reference (S, I) = (W.Word (I), 1));
   end loop;
   declare Before : constant Pool := S; begin
      -- A retry cannot overwrite a loan even with a different grant identity.
      for Attempt in 1 .. 16 loop
         P.Publish (S, 1, 10, (4, 2), Payload, Result);
         pragma Assert (Result = D.Busy and S = Before);
      end loop;
      -- A new surface is also refused when the service pool is full.
      P.Publish (S, 2, 12, (4, 2), Payload, Result);
      pragma Assert (Result = D.Busy and S = Before and Acquire_Count = 3);
   end;
   -- Simulate all original channels gone: no channel state is needed for
   -- cleanup, and the permanently failing first loan cannot starve slot two.
   Allowed (2) := True;
   for I in Slots loop
      Return_Count := 0; P.Poll (S);
      pragma Assert (Return_Count = 1 and Last_Return = (W.Word (I), 1));
      pragma Assert (P.Cursor (S) = P.Next (S, I));
   end loop;
   pragma Assert (P.Pending (S, 1) and not P.Pending (S, 2) and P.Pending (S, 3));
   P.Publish (S, 1, 11, (4, 2), Payload, Result);
   pragma Assert (Result = D.Quarantined and P.Reference (S, 2) = (4, 2));
   pragma Assert (P.Reference (S, 1) = (1, 1) and P.Reference (S, 3) = (3, 1));
   Allowed := [others => True];
   for I in Slots loop P.Poll (S); end loop;
   for I in Slots loop pragma Assert (not P.Pending (S, I)); end loop;
   Return_Count := 0;
   for I in Slots loop P.Poll (S); end loop;
   pragma Assert (Return_Count = 0);
   -- Growth with the surface table: a full pool grows, its pending loans
   -- keep their slots and the cursor, and the new slots take new surfaces.
   Allowed := [others => False];
   P.Publish (S, 1, 20, (1, 3), Payload, Result);
   P.Publish (S, 1, 21, (2, 3), Payload, Result);
   P.Publish (S, 2, 20, (3, 3), Payload, Result);
   P.Publish (S, 2, 22, (4, 3), Payload, Result);
   pragma Assert (Result = D.Busy);
   declare
      Larger : P.State (6);
   begin
      P.Grow (S, Larger);
      pragma Assert (P.Cursor (Larger) = P.Cursor (S));
      for I in Slots loop
         pragma Assert (P.Pending (Larger, I) and P.Reference (Larger, I) = P.Reference (S, I));
      end loop;
      P.Publish (Larger, 2, 22, (4, 3), Payload, Result);
      pragma Assert (Result = D.Quarantined and P.Pending (Larger, 4) and not P.Pending (Larger, 5));
      P.Publish (Larger, 1, 20, (4, 4), Payload, Result);
      pragma Assert (Result = D.Busy);
   end;
   Ada.Text_IO.Put_Line ("INPUT DELIVERY POOL: PASS capacity, retry isolation, cleanup fairness, exact generation, reuse and growth");
end Input_Delivery_Pool_Tests;
