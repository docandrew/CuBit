pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Local_CMOS_Provider;
with ACPI_CMOS_Transactions;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_FADT.Transactions;

procedure CMOS_Provider_Tests is
   package T renames ACPI_CMOS_Transactions;
   package P renames T.P;
   package I renames T.I;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   use type A.Counter, A.Completion, A.Operation_ID, A.Target, A.Request;
   use type I.Byte, P.Device_Kind, P.Port_Pair, T.Completion;
   Profile : P.Profile := (P.Intel_Compatible, True);
   NMI : Boolean := False;
   Known, Standard, Extended, Allowed : Boolean := True;
   Century : I.Byte := 50;
   Calls, Checks : Natural := 0;
   Next_Result : T.Completion := T.Completed;
   type RAM is array (I.Byte) of I.Byte;
   Bytes : RAM := [others => 0];
   procedure Check (Good : Boolean) is
   begin
      if not Good then raise Program_Error with "CMOS provider regression"; end if;
      Checks := Checks + 1;
   end Check;
   procedure Configuration
     (Item : out P.Profile; RTC_Century : out I.Byte;
      Known_NMI, NMI_Disabled, Standard_Granted, Extended_Granted : out Boolean) is
   begin
      Item := Profile; RTC_Century := Century; Known_NMI := Known;
      NMI_Disabled := NMI; Standard_Granted := Standard; Extended_Granted := Extended;
   end Configuration;
   function Access_Allowed (For_Write : Boolean) return Boolean is
      pragma Unreferenced (For_Write);
   begin return Allowed; end Access_Allowed;
   procedure Transfer (Plan : T.Transfer; Value : out I.Byte; Outcome : out T.Completion);
   package Driver is new ACPI_Local_CMOS_Provider (Configuration, Access_Allowed, Transfer);
   type Context_Access is access all Driver.Context;
   Active : Context_Access := null;
   Current_Request : A.Request;
   Inside : Boolean := False;
   procedure Transfer (Plan : T.Transfer; Value : out I.Byte; Outcome : out T.Completion) is
      Cell : constant I.Byte := T.Logical_Cell (Plan);
      Reentry : A.Response;
   begin
      Check (not Inside and Active /= null);
      Inside := True;
      Calls := Calls + 1;
      Check ((((Plan.Selector and 16#80#) /= 0) = NMI));
      Check (Plan.Pair /= P.Extended_Pair or (Profile.Device = P.Intel_Compatible and Profile.Extended_Enabled));
      -- The physical adapter is called while the provider's reentry latch is
      -- held. A recursive request must not reach a second select/data pair.
      Driver.Dispatch (Active.all, Current_Request, Reentry);
      Check (Reentry.Status = A.Denied);
      Outcome := Next_Result;
      if Outcome /= T.Refused then
         if Plan.For_Write then Bytes (Cell) := Plan.Value; end if;
         Value := Bytes (Cell);
      else Value := 0;
      end if;
      Inside := False;
   end Transfer;
   function Geometry (Cell : I.Byte) return L.Register_Geometry is
     ((Region => (Space => 5, Base => A.Counter (Cell), Length => 1, PCI => (Known => False)),
       Byte_Offset => 0, Width => ACPI_FADT.Transactions.Byte_Access));
   Stale : A.Target := A.No_Target;
begin
   for Kind in P.Device_Kind loop
      for Bank in Boolean loop
         for Bit in Boolean loop
            declare
               IO : aliased Driver.Context;
               Target : A.Target;
               Good : Boolean;
               Reply : A.Response;
               Before : Natural;
               Maximum : constant Natural :=
                 (if Kind = P.PC_AT_64 then 63 elsif Bank then 255 else 127);
            begin
               Profile := (Kind, Bank); NMI := Bit; Calls := 0;
               for Cell in I.Byte loop Bytes (Cell) := Cell xor 16#5A#; end loop;
               Active := IO'Unchecked_Access;
               Driver.Initialize (IO, 17, Good);
               Check (Good = (Kind in P.PC_AT_64 | P.Intel_Compatible));
               if Good then
                  if Stale /= A.No_Target then
                     Driver.Dispatch (IO, (Stale, False, 0), Reply);
                     Check (Reply.Status = A.Denied and Calls = 0);
                  end if;
                  -- Native resolution occurs before Begin_Field. Selecting
                  -- Status D needs no active scope and causes no transfer.
                  Allowed := False; Before := Calls;
                  Driver.Select_Target (IO, Geometry (13), False, Target, Good);
                  Check (Good and Calls = Before);
                  Driver.Authorize (IO, Target, False, Good);
                  Check (not Good and Calls = Before);
                  Driver.Dispatch (IO, (Target, False, 0), Reply);
                  Check (Reply.Status = A.Denied and Calls = Before);
                  Allowed := True;
                  Driver.Authorize (IO, Target, False, Good);
                  Check (Good and Calls = Before);
                  Current_Request := (Target, False, 0);
                  Driver.Dispatch (IO, Current_Request, Reply);
                  Check (Reply.Status = A.Completed and Calls = Before + 1);
                  for Cell in I.Byte loop
                     Driver.Select_Target (IO, Geometry (Cell), False, Target, Good);
                     Check (Good = (Natural (Cell) <= Maximum));
                     if Good then
                        Current_Request := (Target, False, 0);
                        Driver.Dispatch (IO, Current_Request, Reply);
                        Check (Reply.Status = A.Completed and Reply.Value = A.Counter (Bytes (Cell)));
                        Before := Calls;
                        Driver.Dispatch (IO, (Target, False, 1), Reply);
                        Check (Reply.Status = A.Denied and Calls = Before);
                        Current_Request := (Target, True, 16#A5#);
                        Driver.Dispatch (IO, Current_Request, Reply);
                        if Cell <= 13 or Cell = Century then
                           Check (Reply.Status = A.Denied and Calls = Before);
                        else
                           Check (Reply.Status = A.Completed and Bytes (Cell) = 16#A5#);
                        end if;
                     end if;
                  end loop;
                  Check (Driver.Register_Count (IO) = Maximum + 1);
                  Driver.Select_Target (IO, Geometry (14), False, Target, Good);
                  Check (Good); Stale := Target;
                  Before := Calls; Allowed := False;
                  Driver.Dispatch (IO, (Target, False, 0), Reply);
                  Check (Reply.Status = A.Denied and Calls = Before);
                  Allowed := True; Next_Result := T.Refused;
                  Current_Request := (Target, False, 0);
                  Driver.Dispatch (IO, Current_Request, Reply);
                  Check (Reply.Status = A.Failed_Completed and Driver.Completed_Fault (IO).Present);
                  Check (not Driver.Pending (IO));
                  Next_Result := T.Unknown;
                  Current_Request := (Target, True, 16#AA#);
                  Driver.Dispatch (IO, Current_Request, Reply);
                  Check (Reply.Status = A.Indeterminate and Reply.Operation /= A.No_Operation);
                  Check (Driver.Pending (IO) and Driver.Uncertain (IO).Present);
                  Check (Driver.Uncertain (IO).Item = Current_Request);
                  Before := Calls; Next_Result := T.Completed;
                  for Attempt in 1 .. 3 loop
                     Driver.Dispatch (IO, Current_Request, Reply);
                     Check (Reply.Status = A.Denied and Calls = Before);
                     Driver.Close (IO);
                     Check (Driver.Pending (IO));
                  end loop;
               end if;
               Active := null;
            end;
         end loop;
      end loop;
   end loop;
   -- Startup receipt failures are rejected before any transfer or target.
   for Reason in 1 .. 4 loop
      declare
         IO : aliased Driver.Context;
         Good : Boolean;
         Target : A.Target;
      begin
         Known := Reason /= 1; Standard := Reason /= 2; Extended := Reason /= 3;
         Profile := (if Reason = 4 then (P.PC_AT_64, False) else (P.Intel_Compatible, True));
         Century := (if Reason = 4 then 128 else 50);
         Calls := 0; Active := IO'Unchecked_Access;
         Driver.Initialize (IO, 17, Good);
         Check (not Good and not Driver.Is_Ready (IO));
         Driver.Select_Target (IO, Geometry (14), False, Target, Good);
         Check (not Good and Target = A.No_Target and Calls = 0);
         Driver.Close (IO); Active := null;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("CMOS provider checks " & Checks'Image & " PASS");
end CMOS_Provider_Tests;
