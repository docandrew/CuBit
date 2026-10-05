with Ada.Text_IO;
with Compositor_Input_Delivery;
with Compositor_Input_Batch_Wire;
with CuBit.Grant_References;
procedure Input_Delivery_Tests is
   package W renames Compositor_Input_Batch_Wire;
   package GR renames CuBit.Grant_References;
   use type W.Word;
   use type W.Snapshot_Words;
   use type GR.Reference;
   Acquire_OK, Write_OK, Return_OK, Null_Mapping : Boolean := False;
   Calls : Natural := 0;
   Expected_Grant : GR.Reference := (17, 23);
   Payload : constant W.Snapshot_Words := [others => W.Word'Last];
   procedure Acquire
     (Owner : W.Identity; Grant : GR.Reference;
      Mapping : out W.Word; Acquired : out Boolean) is
   begin
      pragma Assert (Calls = 0 and Owner = 42 and Grant = Expected_Grant);
      Calls := 1;
      Mapping := (if Null_Mapping then 0 else 4096);
      Acquired := Acquire_OK;
   end Acquire;
   procedure Write
     (Mapping : W.Word; Payload : W.Snapshot_Words; Written : out Boolean) is
   begin
      pragma Assert (Calls = 1 and Mapping = 4096);
      pragma Assert (Payload = Input_Delivery_Tests.Payload);
      Calls := 12; Written := Write_OK;
   end Write;
   procedure Return_Loan (Grant : GR.Reference; Confirmed : out Boolean) is
   begin
      pragma Assert (Grant = Expected_Grant);
      pragma Assert (Calls in 0 | 1 | 12);
      Calls := Calls * 10 + 3; Confirmed := Return_OK;
   end Return_Loan;
   package D is new Compositor_Input_Delivery (Acquire, Write, Return_Loan);
   use type D.Outcome;
   use type D.State;
   Cases : Natural := 0;
begin
   for A in Boolean loop
      for P in Boolean loop
         for R in Boolean loop
            for N in Boolean loop
               declare
                  S : D.State;
                  Result : D.Outcome;
               begin
                  Acquire_OK := A; Write_OK := P; Return_OK := R; Null_Mapping := N;
                  Calls := 0;
                  D.Deliver (S, 42, Expected_Grant, Payload, Result);
                  if not A then
                     pragma Assert (Result = D.Acquisition_Failed and Calls = 1);
                     pragma Assert (not D.Pending (S));
                  else
                     pragma Assert (Calls = (if N then 13 else 123));
                     pragma Assert (D.Pending (S) = not R);
                     pragma Assert (D.Reference (S) = Expected_Grant);
                     pragma Assert (Result =
                       (if not R then D.Quarantined
                        elsif P and not N then D.Published else D.Publication_Failed));
                     if not R then
                        declare Before : constant D.State := S; begin
                           Calls := 0;
                           D.Deliver (S, 77, (99, 100), Payload, Result);
                           pragma Assert (Result = D.Busy and Calls = 0 and S = Before);
                           -- Failed cleanup keeps the exact old identity.
                           D.Retire (S);
                           pragma Assert (Calls = 3 and S = Before);
                        end;
                     end if;
                  end if;
                  Return_OK := True; Calls := 0;
                  D.Retire (S);
                  pragma Assert (not D.Pending (S));
                  pragma Assert (Calls = (if A and not R then 3 else 0));
                  Calls := 0; D.Retire (S);
                  pragma Assert (Calls = 0);
                  -- The state can be reused only after the old return succeeds.
                  Acquire_OK := True; Write_OK := True; Null_Mapping := False;
                  D.Deliver (S, 42, Expected_Grant, Payload, Result);
                  pragma Assert (Result = D.Published and Calls = 123 and not D.Pending (S));
                  Cases := Cases + 1;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("INPUT DELIVERY: PASS" & Cases'Image &
     " acquisition/write/return/null-mapping fault combinations, quarantine and reuse");
end Input_Delivery_Tests;
