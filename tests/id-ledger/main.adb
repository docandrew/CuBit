--  Hosted checks for Id_Ledger against an independent reference model.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Test_Ledger; use Test_Ledger;

procedure Main is
   L : Ledger;

   --  Reference model, written independently of the package.
   Model_Used : array (Valid_Id) of Boolean := [others => False];
   Model_Gen  : array (Valid_Id) of Unsigned_32 := [others => 0];
   Model_Retired : array (Valid_Id) of Boolean := [others => False];
   Limit : constant := 40;   --  test_ledger.ads Generation_Limit

   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Natural is
   begin
      Seed := Seed * 6364136223846793005 + 1442695040888963407;
      return Natural (Shift_Right (Seed, 33) mod 2**30);
   end Next;

   function Model_Page_Empty_Except (I : Valid_Id) return Boolean is
   begin
      for J in Valid_Id loop
         if J /= I and then J / 8 = I / 8 and then Model_Used (J) then
            return False;
         end if;
      end loop;
      return True;
   end Model_Page_Empty_Except;

   Id_Out : Id;
   First, Last, Success : Boolean;
   Allocations, Releases, Specifics, Full : Natural := 0;
   Invalidations, Retirements : Natural := 0;
begin
   Initialize (L);
   for Step in 1 .. 200_000 loop
      case Next mod 4 is
         when 0 =>  --  Allocate lowest free non-reserved ID
            declare
               Expected : Id := 0;
            begin
               for J in 5 .. Valid_Id'Last loop
                  if not Model_Used (J) and then not Model_Retired (J) then
                     Expected := J; exit;
                  end if;
               end loop;
               declare
                  Expect_First : constant Boolean :=
                    (if Expected /= 0 then Model_Page_Empty_Except (Expected) else False);
               begin
                  Allocate (L, Id_Out, First);
                  pragma Assert (Id_Out = Expected);
                  if Id_Out /= 0 then
                     pragma Assert (First = Expect_First);
                     Model_Used (Id_Out) := True;
                     Allocations := Allocations + 1;
                  else
                     Full := Full + 1;
                  end if;
               end;
            end;
         when 1 =>  --  Release a random used ID
            declare
               I : constant Valid_Id := Next mod Valid_Id'Last + 1;
            begin
               if Model_Used (I) then
                  declare
                     Old_Gen : constant Unsigned_32 := Model_Gen (I);
                     Expect_Last : constant Boolean := Model_Page_Empty_Except (I);
                  begin
                     Release (L, I, Last);
                     pragma Assert (Last = Expect_Last);
                     Model_Used (I) := False;
                     if Model_Gen (I) < Limit then
                        Model_Gen (I) := Model_Gen (I) + 1;
                        pragma Assert (not Current (L, I, Old_Gen));
                     else
                        Model_Retired (I) := True;
                        Retirements := Retirements + 1;
                     end if;
                     Releases := Releases + 1;
                  end;
               end if;
            end;
         when 2 =>  --  Invalidate a reserved ID in place (teardown with grants)
            declare
               I : constant Valid_Id := Next mod Valid_Id'Last + 1;
               Saturated : Boolean;
            begin
               if Model_Used (I) then
                  declare
                     Old_Gen : constant Unsigned_32 := Model_Gen (I);
                  begin
                     Invalidate (L, I, Saturated);
                     pragma Assert (Saturated = (Old_Gen >= Limit));
                     if not Saturated then
                        Model_Gen (I) := Old_Gen + 1;
                        pragma Assert (not Current (L, I, Old_Gen));
                     end if;
                     Invalidations := Invalidations + 1;
                     --  Teardown: sometimes release right away, without a
                     --  second advance.
                     if Next mod 2 = 0 then
                        declare
                           Expect_Last : constant Boolean := Model_Page_Empty_Except (I);
                        begin
                           Release (L, I, Last, Advance => False);
                           pragma Assert (Last = Expect_Last);
                           Model_Used (I) := False;
                           if Model_Gen (I) >= Limit then
                              Model_Retired (I) := True;
                              Retirements := Retirements + 1;
                           end if;
                           pragma Assert (not Current (L, I, Old_Gen));
                           Releases := Releases + 1;
                        end;
                     end if;
                  end;
               end if;
            end;
         when others =>  --  Specific (including reserved) IDs
            declare
               I : constant Valid_Id := Next mod Valid_Id'Last + 1;
            begin
               Allocate_Specific (L, I, Success, First);
               pragma Assert (Success = (not Model_Used (I) and not Model_Retired (I)));
               if Success then
                  Model_Used (I) := True;
                  Specifics := Specifics + 1;
               end if;
            end;
      end case;
      for J in Valid_Id loop
         pragma Assert (Used (L, J) = Model_Used (J));
         pragma Assert (Generation_Of (L, J) = Model_Gen (J));
         pragma Assert (Retired (L, J) = Model_Retired (J));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS: id ledger, 200000 random operations:" & Allocations'Image &
      " allocations," & Releases'Image & " releases," & Specifics'Image &
      " specific," & Invalidations'Image & " invalidations," &
      Retirements'Image & " retirements at the generation limit," &
      Full'Image & " full-table refusals");
end Main;
