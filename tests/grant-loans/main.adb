with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Memory_Grants; use Memory_Grants;
with Loan_Proof;
procedure Main is
   package L renames Loan_Proof.Production;
   use type L.Parent_Phase, L.Loan_Phase, L.Reservation_Result;
   use type L.State, L.Loan_Reference, L.Terms, L.Forwarding_Policy;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "loan check" & Checks'Image;
      end if;
   end Check;
   Item : L.State;
   Loan : L.Loan_Reference;
   Result : L.Reservation_Result;
   Applied : Boolean;
   procedure Check_Stale (S : in out L.State; Ref : L.Loan_Reference) is
      Saved : constant L.State := S;
      Ok : Boolean;
   begin
      L.Publish (S, Ref, Ok);
      Check (not Ok and S = Saved);
      L.Acquire (S, Ref, Ok);
      Check (not Ok and S = Saved);
      L.Return_Reader (S, Ref, Ok);
      Check (not Ok and S = Saved);
      L.Revoke (S, Ref, Ok);
      Check (not Ok and S = Saved);
      L.Finish_Retirement (S, Ref, Ok);
      Check (not Ok and S = Saved);
   end Check_Stale;

   procedure Attenuation is
      Page_Values : constant array (Positive range <>) of Page_Count :=
        [1, 2, 8, 32, Maximum_Page_Count];
      Offsets : constant array (Positive range <>) of Page_Offset :=
        [0, 1, 7, 8, 31, 32, Maximum_Page_Count - 1];
   begin
      for Parent_Access in Permission loop
         for Child_Access in Permission loop
            for Forwarding in L.Forwarding_Policy loop
               for Pages of Page_Values loop
                  for Offset of Offsets loop
                     for Child_Pages of Page_Values loop
                        declare
                           S : L.State;
                           Requested : constant L.Terms :=
                             (Offset, Child_Pages, Child_Access);
                           Ref : L.Loan_Reference;
                           R : L.Reservation_Result;
                           Ok : Boolean;
                           Allowed : constant Boolean :=
                             Forwarding = L.Forward_Once and
                             Natural (Offset) + Natural (Child_Pages) <=
                               Natural (Pages) and
                             not (Parent_Access = Borrowed_Read_Only and
                                  Child_Access = Borrowed_Read_Write);
                        begin
                           L.Configure (S, (16, 7), Pages, Parent_Access,
                             Forwarding, Ok);
                           Check (Ok);
                           Check (L.Admits (S, Requested) = Allowed);
                           L.Reserve (S, Requested, Ref, R);
                           Check ((R = L.Reserved) = Allowed);
                           if Allowed then
                              Check (L.Describe (S, Ref) = Requested);
                              L.Revoke (S, Ref, Ok);
                              Check (Ok);
                              L.Finish_Retirement (S, Ref, Ok);
                              Check (Ok);
                           else
                              Check (Ref = L.No_Loan and L.Empty (S));
                           end if;
                           L.Close (S);
                           L.Release_Parent (S, Ok);
                           Check (Ok);
                        end;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end Attenuation;

   procedure Capacity_And_Lifetimes is
      S, Foreign_Parent, New_Generation : L.State;
      Refs : array (L.Loan_Index) of L.Loan_Reference;
      Old, New_Ref : L.Loan_Reference;
      R : L.Reservation_Result;
      Ok : Boolean;
   begin
      Check_Stale (S, L.No_Loan);
      L.Configure (S, (20, 5), 16, Borrowed_Read_Write, L.Forward_Once, Ok);
      Check (Ok);
      for I in Refs'Range loop
         L.Reserve (S, (I - 1, 1, Borrowed_Read_Only), Refs (I), R);
         Check (R = L.Reserved);
         L.Publish (S, Refs (I), Ok);
         Check (Ok);
      end loop;
      declare
         Saved : constant L.State := S;
      begin
         L.Reserve (S, (0, 1, Borrowed_Read_Only), New_Ref, R);
         Check (R = L.Full and S = Saved and New_Ref = L.No_Loan);
         L.Configure (S, (21, 6), 1, Borrowed_Read_Only, L.No_Forwarding, Ok);
         Check (not Ok and S = Saved);
      end;
      Old := Refs (1);
      for I in 1 .. Maximum_Acquisition_Count loop
         L.Acquire (S, Old, Ok);
         Check (Ok and L.Readers (S, Old) = I);
      end loop;
      L.Acquire (S, Old, Ok);
      Check (not Ok);
      L.Revoke (S, Old, Ok);
      Check (Ok);
      L.Acquire (S, Old, Ok);
      Check (not Ok);
      for I in reverse 1 .. Maximum_Acquisition_Count loop
         L.Finish_Retirement (S, Old, Ok);
         Check (not Ok);
         L.Return_Reader (S, Old, Ok);
         Check (Ok and L.Readers (S, Old) = I - 1);
      end loop;
      L.Return_Reader (S, Old, Ok);
      Check (not Ok);
      L.Finish_Retirement (S, Old, Ok);
      Check (Ok);
      L.Reserve (S, (0, 1, Borrowed_Read_Write), New_Ref, R);
      Check (R = L.Reserved and New_Ref /= Old);
      Check_Stale (S, Old);
      Refs (1) := New_Ref;
      L.Configure (Foreign_Parent, (21, 5), 16, Borrowed_Read_Write,
                   L.Forward_Once, Ok);
      Check (Ok);
      L.Configure (New_Generation, (20, 6), 16, Borrowed_Read_Write,
                   L.Forward_Once, Ok);
      Check (Ok);
      -- Identical slot/sequence in other scopes must not authenticate Old.
      L.Reserve (Foreign_Parent, (0, 1, Borrowed_Read_Only), New_Ref, R);
      Check (R = L.Reserved);
      L.Reserve (New_Generation, (0, 1, Borrowed_Read_Only), New_Ref, R);
      Check (R = L.Reserved);
      Check_Stale (Foreign_Parent, Old);
      Check_Stale (New_Generation, Old);
      L.Close (S);
      for I in Refs'Range loop
         L.Publish (S, Refs (I), Ok);
         Check (not Ok);
         L.Acquire (S, Refs (I), Ok);
         Check (not Ok);
         L.Release_Parent (S, Ok);
         Check (not Ok);
         L.Finish_Retirement (S, Refs (I), Ok);
         Check (Ok);
      end loop;
      L.Release_Parent (S, Ok);
      Check (Ok);
      L.Configure (S, (20, 5), 16, Borrowed_Read_Write, L.Forward_Once, Ok);
      Check (not Ok);
      Check_Stale (S, Old);
   end Capacity_And_Lifetimes;

   procedure Exhaustion is
      package B renames Loan_Proof.Bounded;
      use type B.Reservation_Result, B.Loan_Reference, B.State;
      S : B.State;
      Ref, First : B.Loan_Reference;
      R : B.Reservation_Result;
      Ok : Boolean;
   begin
      B.Configure (S, (1, 1), 1, Borrowed_Read_Write, B.Forward_Once, Ok);
      Check (Ok);
      for I in 1 .. 3 loop
         B.Reserve (S, (0, 1, Borrowed_Read_Only), Ref, R);
         Check (R = B.Reserved and B.Sequence (S) = Unsigned_64 (I));
         if I = 1 then First := Ref; end if;
         B.Revoke (S, Ref, Ok);
         Check (Ok);
         B.Finish_Retirement (S, Ref, Ok);
         Check (Ok);
      end loop;
      declare
         Saved : constant B.State := S;
      begin
         B.Reserve (S, (0, 1, Borrowed_Read_Only), Ref, R);
         Check (R = B.Exhausted and Ref = B.No_Loan and S = Saved);
      end;
      B.Publish (S, First, Ok);
      Check (not Ok);
      B.Close (S);
      B.Release_Parent (S, Ok);
      Check (Ok);
   end Exhaustion;

   -- Explore every seven-event sequence for a loan. Independent expected
   -- state uses reader count + booleans, not the implementation phase enum.
   procedure Event_Orders is
      type Action is (Publish, Acquire, Return_Reader, Revoke, Unmap,
                      Close_Parent, Release_Parent);
      procedure Walk
        (S : L.State; Ref : L.Loan_Reference; Depth : Natural;
         Published, Revoked, Gone, Closing, Released : Boolean;
         Count : Natural)
      is
      begin
         if Depth = 0 then return; end if;
         for A in Action loop
            declare
               Next : L.State := S;
               P : Boolean := Published;
               V : Boolean := Revoked;
               G : Boolean := Gone;
               C : Boolean := Closing;
               R : Boolean := Released;
               N : Natural := Count;
               Expected, Ok : Boolean;
            begin
               case A is
                  when Publish =>
                     Expected := not (P or V or G);
                     L.Publish (Next, Ref, Ok);
                     P := P or Expected;
                  when Acquire =>
                     Expected := P and not (V or G);
                     L.Acquire (Next, Ref, Ok);
                     if Expected then N := N + 1; end if;
                  when Return_Reader =>
                     Expected := N > 0 and not G;
                     L.Return_Reader (Next, Ref, Ok);
                     if Expected then N := N - 1; end if;
                  when Revoke =>
                     Expected := not (V or G);
                     L.Revoke (Next, Ref, Ok);
                     V := V or Expected;
                  when Unmap =>
                     Expected := V and not G and N = 0;
                     L.Finish_Retirement (Next, Ref, Ok);
                     G := G or Expected;
                  when Close_Parent =>
                     Expected := True;
                     Ok := True;
                     L.Close (Next);
                     C := True;
                     V := True;
                  when Release_Parent =>
                     Expected := C and G and not R;
                     L.Release_Parent (Next, Ok);
                     R := R or Expected;
               end case;
               Check (Ok = Expected);
               Check (L.Holds_Parent (Next) = not R);
               Check (L.Readers (Next, Ref) = N);
               Check (L.Live (Next, Ref) = not G);
               if not Ok then Check (Next = S); end if;
               Walk (Next, Ref, Depth - 1, P, V, G, C, R, N);
            end;
         end loop;
      end Walk;
      S : L.State;
      Ref : L.Loan_Reference;
      R : L.Reservation_Result;
      Ok : Boolean;
   begin
      L.Configure (S, (3, 8), 1, Borrowed_Read_Write, L.Forward_Once, Ok);
      Check (Ok);
      L.Reserve (S, (0, 1, Borrowed_Read_Only), Ref, R);
      Check (R = L.Reserved);
      Walk (S, Ref, 7, False, False, False, False, False, 0);
   end Event_Orders;
begin
   L.Configure (Item, (1, 1), 8, Borrowed_Read_Write, L.Forward_Once, Applied);
   Check (Applied);
   L.Reserve (Item, (2, 4, Borrowed_Read_Only), Loan, Result);
   Check (Result = L.Reserved);
   L.Publish (Item, Loan, Applied);
   Check (Applied);
   L.Acquire (Item, Loan, Applied);
   Check (Applied);
   L.Close (Item);
   Check (L.Phase_Of (Item, Loan) = L.Draining);
   L.Release_Parent (Item, Applied);
   Check (not Applied and L.Holds_Parent (Item));
   L.Return_Reader (Item, Loan, Applied);
   Check (Applied and L.Phase_Of (Item, Loan) = L.Unmapping);
   L.Release_Parent (Item, Applied);
   Check (not Applied and L.Holds_Parent (Item));
   L.Finish_Retirement (Item, Loan, Applied);
   Check (Applied);
   L.Release_Parent (Item, Applied);
   Check (Applied and L.Phase (Item) = L.Retired);
   L.Release_Parent (Item, Applied);
   Check (not Applied);
   Attenuation;
   Capacity_And_Lifetimes;
   Exhaustion;
   Event_Orders;
   Put_Line ("PASS grant loans:" & Checks'Image & " checks");
end Main;
