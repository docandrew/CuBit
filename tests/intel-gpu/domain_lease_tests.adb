with Intel_GPU_Domain_Lease;
procedure Domain_Lease_Tests is
   type Domain is (GT, Render, Media);
   type Seen is array (Domain) of Natural;
   Acquired, Released : Seen := [others => 0];
   type History is array (1 .. 3) of Domain;
   Release_Order : History := [others => GT];
   Release_Count : Natural := 0;
   Fail_Acquire, Fail_Release : Natural := 0;
   function Number (Item : Domain) return Positive is (Domain'Pos (Item) + 1);
   procedure Acquire_One (Item : Domain; Success : out Boolean) is
   begin Acquired (Item) := Acquired (Item) + 1; Success := Number (Item) /= Fail_Acquire; end;
   procedure Release_One (Item : Domain; Success : out Boolean) is
   begin
      Release_Count := Release_Count + 1;
      Release_Order (Release_Count) := Item;
      Released (Item) := Released (Item) + 1;
      Success := Number (Item) /= Fail_Release;
   end;
   package Domains is new Intel_GPU_Domain_Lease (Domain, Acquire_One, Release_One);
   use Domains;
   OK : Boolean;
begin
   pragma Assert (Number (GT) = 1 and Number (Render) = 2 and Number (Media) = 3);
   -- Exhaust every subset, every acquire failure, every release failure.
   for Mask in 0 .. 7 loop
      for A in 0 .. 3 loop
         for R in 0 .. 3 loop
            declare
               Object : Lease;
               Required : Selection;
               Failed : Boolean := False;
            begin
               Acquired := [others => 0]; Released := [others => 0];
               Release_Count := 0;
               Fail_Acquire := A; Fail_Release := R;
               for D in Domain loop Required (D) := (Mask / 2 ** Domain'Pos (D)) mod 2 = 1; end loop;
               Acquire (Object, Required, OK);
               for D in Domain loop
                  if Required (D) and not Failed then
                     pragma Assert (Acquired (D) = 1);
                     Failed := Number (D) = A;
                  else pragma Assert (Acquired (D) = 0); end if;
               end loop;
               pragma Assert (OK = (Mask /= 0 and not Failed));
               if OK then
                  declare
                     Before : constant Seen := Acquired;
                  begin
                     Acquire (Object, Required, OK);
                     pragma Assert (not OK and Acquired = Before and State (Object) = Held);
                  end;
                  Release (Object, OK);
                  pragma Assert (OK = (R = 0 or else not Required (Domain'Val (R - 1))));
                  for D in Domain loop
                     pragma Assert (Released (D) = (if Required (D) then 1 else 0));
                     pragma Assert (Uncertain (Object) (D) = (Required (D) and Number (D) = R));
                  end loop;
                  pragma Assert (State (Object) = (if OK then Idle else Faulted));
               elsif Failed then
                  pragma Assert (State (Object) = Faulted);
                  pragma Assert (Uncertain (Object) (Domain'Val (A - 1)));
                  for D in Domain loop
                     pragma Assert (Released (D) =
                       (if Required (D) and Number (D) < A then 1 else 0));
                     pragma Assert (Uncertain (Object) (D) =
                       (Required (D) and (Number (D) = A or
                         (Number (D) < A and Number (D) = R))));
                  end loop;
               else pragma Assert (State (Object) = Idle); end if;
               for I in 2 .. Release_Count loop
                  pragma Assert (Release_Order (I - 1) > Release_Order (I));
               end loop;
               declare
                  Before : constant Seen := Released;
               begin
                  Release (Object, OK);
                  pragma Assert (not OK and Released = Before);
               end;
               if State (Object) = Faulted then
                  declare
                     Before : constant Seen := Acquired;
                  begin
                     Acquire (Object, Required, OK);
                     pragma Assert (not OK and Acquired = Before);
                  end;
               end if;
            end;
         end loop;
      end loop;
   end loop;
end Domain_Lease_Tests;
