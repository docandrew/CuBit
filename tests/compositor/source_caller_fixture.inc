with Ada.Text_IO;
with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with Compositor_Source_Loans;
procedure Source_Caller_Tests is
   subtype ProcessID is Unsigned_64;
   package DP is
      type Status_Code is (Success, Denied, Bad_Object, Bad_State, Invalid_Request,
                           Unsupported, Resources_Exhausted);
   end DP;
   use type DP.Status_Code;
   package Desktop_Compositor is
      type Source_Release is (Source_Retired, Source_Busy, Source_Unsafe);
      Outcome : Source_Release := Source_Busy;
      Calls : Natural := 0;
      procedure Forget_Source (Pixels : System.Address; Result : out Source_Release);
   end Desktop_Compositor;
   package body Desktop_Compositor is
      procedure Forget_Source (Pixels : System.Address; Result : out Source_Release) is
      begin
         pragma Assert (Pixels /= System.Null_Address);
         Calls := Calls + 1; Result := Outcome;
      end Forget_Source;
   end Desktop_Compositor;
   package MG is
      subtype Grant_Reference is Natural;
      type Required_Access is (Read_Access);
      Acquire_Ok, Return_Ok : Boolean := True;
      Acquisitions, Returns : Natural := 0;
      procedure Acquire (Grant : Grant_Reference; Owner : ProcessID;
         Offset, Bytes : Unsigned_64; Access_Mode : Required_Access;
         Address : out System.Address; Acquired : out Boolean);
      procedure Return_Acquisition (Grant : Grant_Reference; Confirmed : out Boolean);
   end MG;
   package body MG is
      procedure Acquire (Grant : Grant_Reference; Owner : ProcessID;
         Offset, Bytes : Unsigned_64; Access_Mode : Required_Access;
         Address : out System.Address; Acquired : out Boolean) is
      begin
         pragma Assert (Owner = 77 and Offset = 0 and Bytes = 4096 and Access_Mode = Read_Access);
         Acquisitions := Acquisitions + 1;
         Acquired := Acquire_Ok;
         Address := (if Acquired then To_Address (Integer_Address (Grant * 4096)) else System.Null_Address);
      end Acquire;
      procedure Return_Acquisition (Grant : Grant_Reference; Confirmed : out Boolean) is
         use type Desktop_Compositor.Source_Release;
      begin
         pragma Assert (Grant > 0 and Desktop_Compositor.Outcome = Desktop_Compositor.Source_Retired);
         Returns := Returns + 1; Confirmed := Return_Ok;
      end Return_Acquisition;
   end MG;
   package Source_Loans is new Compositor_Source_Loans (24);
   use type Source_Loans.Phase;
   use type Source_Loans.Ticket;
   type Source_Loan is record
      Grant : MG.Grant_Reference := 0;
      Address : System.Address := System.Null_Address;
   end record;
   type Source_Loan_Table is array (Source_Loans.Slot) of Source_Loan;
   sourceLoanPolicy : Source_Loans.State;
   sourceLoans : Source_Loan_Table;
   Restart : exception;
   LF : constant Character := ASCII.LF;
   procedure debugPrint (Text : String) is begin null; end debugPrint;
   procedure exitCompositor (Code : Natural) is begin raise Restart; end exitCompositor;
@DESKTOP_ROUTINES@
   Tickets : array (Source_Loans.Slot) of Source_Loans.Ticket;
   New_Ticket, Failed : Source_Loans.Ticket;
   Address : System.Address;
   Result : DP.Status_Code;
   Saved : Source_Loan_Table;
   Old_Calls : Natural;
begin
   MG.Acquire_Ok := False;
   acquireSourceLoan (1, 77, 4096, Address, Failed, Result);
   pragma Assert (Result = DP.Denied and Failed = Source_Loans.No_Ticket);
   MG.Acquire_Ok := True;
   for I in Source_Loans.Slot loop
      acquireSourceLoan (I, 77, 4096, Address, Tickets (I), Result);
      pragma Assert (Result = DP.Success and Address = To_Address (Integer_Address (I * 4096)));
      retireSourceLoan (Tickets (I));
   end loop;
   Saved := sourceLoans;
   -- No remaining surface reference is needed by the pump. Simulate a caller
   -- dropping every surface while rendering is still busy for 1000 polls.
   for Poll in 1 .. 1000 loop
      pumpSourceRetirements;
      pragma Assert (sourceRetirementPending and sourceLoans = Saved and MG.Returns = 0);
   end loop;
   Old_Calls := MG.Acquisitions;
   acquireSourceLoan (25, 77, 4096, Address, Failed, Result);
   pragma Assert (Result = DP.Resources_Exhausted and MG.Acquisitions = Old_Calls);
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Retired;
   pumpSourceRetirements;
   pragma Assert (not sourceRetirementPending and MG.Returns = 24);
   for I in Source_Loans.Slot loop
      pragma Assert (sourceLoans (I).Address = System.Null_Address);
   end loop;
   acquireSourceLoan (25, 77, 4096, Address, New_Ticket, Result);
   pragma Assert (Result = DP.Success);
   retireSourceLoan (Tickets (1));
   pragma Assert (MG.Returns = 24 and Source_Loans.Current (sourceLoanPolicy, New_Ticket) = Source_Loans.Attached);
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Unsafe;
   begin retireSourceLoan (New_Ticket); raise Program_Error;
   exception when Restart => null; end;
   pragma Assert (MG.Returns = 24 and sourceLoans (Source_Loans.Index (New_Ticket)).Grant = 25);
   -- The failed return must also preserve the actual mapping and quarantine.
   acquireSourceLoan (26, 77, 4096, Address, New_Ticket, Result);
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Retired;
   MG.Return_Ok := False;
   begin retireSourceLoan (New_Ticket); raise Program_Error;
   exception when Restart => null; end;
   pragma Assert (Source_Loans.Current (sourceLoanPolicy, New_Ticket) = Source_Loans.Quarantined);
   pragma Assert (sourceLoans (Source_Loans.Index (New_Ticket)).Grant = 26);
   Ada.Text_IO.Put_Line ("SOURCE-CALLER: PASS exact Desktop helpers, 24000 busy polls, retained mappings, exhaustion before syscall, stale tickets, ordered return and quarantine");
end Source_Caller_Tests;
