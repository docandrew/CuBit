with Ada.Text_IO;
with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;
with Compositor_Source_Loans;
with Compositor_Surface_State;
procedure Source_Surface_Tests is
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
   package Surface_Policy is new Compositor_Surface_State;
   use type Surface_Policy.Phase;
   package Publication is
      subtype Identity is Unsigned_64 range 1 .. 2 ** 31 - 1;
      type Receipt (Status : DP.Status_Code := DP.Invalid_Request) is record
         case Status is
            when DP.Success => Epoch, Ticket : Identity;
            when others => null;
         end case;
      end record;
   end Publication;
   type Publication_Buffer is record
      Acquired : Boolean := False;
      Loan : Source_Loans.Ticket := Source_Loans.No_Ticket;
      Grant : MG.Grant_Reference := 0;
      Address : System.Address := System.Null_Address;
      Retired : Publication.Receipt;
   end record;
   type Publication_Buffers is array (Surface_Policy.Slot) of Publication_Buffer;
   type Surface is record
      publicationMode : Boolean := False;
      publicationBuffers : Publication_Buffers;
      publicationPolicy : Surface_Policy.State;
      bufferAttached : Boolean := False;
      bufferLoan : Source_Loans.Ticket := Source_Loans.No_Ticket;
      bufferAddr : System.Address := System.Null_Address;
      bufferW, bufferH, bufferPitch, bufferFormat : Natural := 0;
      bufferLogicalW, bufferLogicalH : Natural := 0;
   end record;
@DESKTOP_ROUTINES@
   S : Surface;
   Accepted : Boolean;
   Loan, Other : Source_Loans.Ticket;
   Address : System.Address;
   Result : DP.Status_Code;
   Returns_Before : Natural;
   procedure Attach_Publication (I : Surface_Policy.Slot; Grant : Positive) is
   begin
      Surface_Policy.Stage (S.publicationPolicy, I, S.publicationPolicy.Requested, Accepted);
      pragma Assert (Accepted);
      acquireSourceLoan (Grant, 77, 4096, Address, Loan, Result);
      pragma Assert (Result = DP.Success);
      S.publicationBuffers (I) := (True, Loan, Grant, Address, (Status => DP.Bad_State));
      Surface_Policy.Present (S.publicationPolicy, I, S.publicationPolicy.Requested,
                             S.publicationPolicy.Buffers (I).Ticket, Accepted);
      pragma Assert (Accepted);
   end Attach_Publication;
begin
   -- Replacing a visible publication retires the old source but cannot report
   -- success while its renderer is still reading. The new source stays live.
   S.publicationMode := True;
   Surface_Policy.Configure (S.publicationPolicy, Accepted);
   pragma Assert (Accepted);
   Attach_Publication (1, 1);
   Other := Loan;
   Attach_Publication (2, 2);
   for Poll in 1 .. 1000 loop
      retirePublicationBuffer (S, 1);
      pragma Assert (S.publicationBuffers (1).Acquired and
                     S.publicationBuffers (1).Retired.Status /= DP.Success and MG.Returns = 0);
      pragma Assert (Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Attached);
   end loop;
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Retired;
   pumpSourceRetirements;
   pragma Assert (MG.Returns = 1);
   retirePublicationBuffer (S, 1);
   pragma Assert (S.publicationBuffers (1).Retired.Status = DP.Success and
                  S.publicationBuffers (1).Retired.Epoch = 1 and
                  S.publicationBuffers (1).Retired.Ticket = 1 and
                  S.publicationPolicy.Buffers (1).Status = Surface_Policy.Empty);
   retirePublicationBuffer (S, 1);
   pragma Assert (MG.Returns = 1);
   -- Closing clears aliases immediately; resetting/reusing the whole Surface
   -- must not erase the global loan or make it available for acquisition.
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Busy;
   S.bufferAttached := True; S.bufferAddr := S.publicationBuffers (2).Address;
   releaseSurfaceBuffer (S);
   pragma Assert (not S.bufferAttached and S.bufferAddr = System.Null_Address and
                  Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Renderer_Pending);
   S := (others => <>);
   for Poll in 1 .. 1000 loop
      pumpSourceRetirements;
      pragma Assert (MG.Returns = 1 and sourceLoans (Source_Loans.Index (Loan)).Grant = 2);
   end loop;
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Retired;
   pumpSourceRetirements;
   pragma Assert (MG.Returns = 2 and not sourceRetirementPending);
   -- Legacy replacement acquires first, retires old, and reuses the surface
   -- record while the old renderer reference remains pending.
   acquireSourceLoan (3, 77, 4096, Address, Other, Result);
   S.bufferAttached := True; S.bufferLoan := Other; S.bufferAddr := Address;
   acquireSourceLoan (4, 77, 4096, Address, Loan, Result);
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Busy;
   releaseSurfaceBuffer (S);
   S.bufferAttached := True; S.bufferLoan := Loan; S.bufferAddr := Address;
   pragma Assert (Source_Loans.Current (sourceLoanPolicy, Other) = Source_Loans.Renderer_Pending and
                  Source_Loans.Current (sourceLoanPolicy, Loan) = Source_Loans.Attached);
   releaseSurfaceBuffer (S); S := (others => <>);
   for Poll in 1 .. 1000 loop pumpSourceRetirements; end loop;
   pragma Assert (MG.Returns = 2 and sourceRetirementPending);
   Desktop_Compositor.Outcome := Desktop_Compositor.Source_Retired;
   pumpSourceRetirements;
   pragma Assert (MG.Returns = 4 and not sourceRetirementPending);
   Returns_Before := MG.Returns;
   releaseSurfaceBuffer (S);
   pragma Assert (MG.Returns = Returns_Before);
   Ada.Text_IO.Put_Line ("SOURCE-SURFACE: PASS exact Desktop release/receipt routines, publication replacement, pending receipts, close/reset, legacy replacement, idempotence");
end Source_Surface_Tests;
