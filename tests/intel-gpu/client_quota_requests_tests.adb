with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Contexts;
with Intel_GPU_Buffer_Requests.Closed_Tables;
with Intel_GPU_Buffer_Reply;
with System.Storage_Elements; use System.Storage_Elements;
procedure Client_Quota_Requests_Tests is
   Ready : Boolean := True;
   function Owner return Boolean is (Ready);
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 then Stamp else 0);
   package P is new Intel_GPU_Buffer_Requests (Session_Of, Owner);
   package C is new P.Contexts;
   package T is new P.Closed_Tables;
   Pool : P.Service;
   Parent, Tables, App, Other : P.Ticket;
   OK, Consumed : Boolean;
   Reply : P.Words;
   Handle : Unsigned_64;
   use type P.Allocation_Outcome;
   use type P.Words;
begin
   P.Configure_Client_Budgets (Pool, 16384, OK); pragma Assert (OK);
   P.Configure_Client_Budgets (Pool, 32768, OK); pragma Assert (not OK);
   C.Reserve (Pool, 100, Parent, Pages => 2); pragma Assert (Parent /= 0);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   P.Query_Accounting (Pool, 42, 100, P.Accounting_Label, 4, 0, 0, [1,0,0,0], Reply);
   pragma Assert (Reply = [P.OK, 1, 16384, 8192]); -- pending private charge visible
   for Fault in 1 .. 7 loop
      P.Query_Accounting (Pool, (if Fault = 1 then 43 else 42),
        (if Fault = 2 then 0 else 100),
        (if Fault = 3 then 0 else P.Accounting_Label),
        (if Fault = 4 then 3 else 4), (if Fault = 5 then 1 else 0),
        (if Fault = 6 then 1 else 0),
        (if Fault = 7 then [1,101,0,0] else [1,0,0,0]), Reply);
      pragma Assert (Reply = [(if Fault <= 2 then P.Denied else P.Bad_Request),1,0,0]);
      pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   end loop;
   Ready := False;
   P.Query_Accounting (Pool, 42, 100, P.Accounting_Label, 4, 0, 0, [1,0,0,0], Reply);
   pragma Assert (Reply = [P.Unavailable,1,0,0]); Ready := True;
   P.Finish_Private (Pool, Parent, Consumed); pragma Assert (Consumed);
   P.Reserve_Private (Pool, 100, Tables, True, Pages => 2);
   pragma Assert (Tables /= 0 and P.Client_Usage (Pool, 100).Charged = 16384);
   P.Finish_Private (Pool, Tables, Consumed); pragma Assert (Consumed);
   P.Handle (Pool, 42, 100, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Reply, App);
   pragma Assert (App = 0 and P.Last_Allocation (Pool) = P.Client_Quota_Unavailable);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 16384);
   P.Acknowledge_Private_Retirement (Pool, 101, Tables, True, OK); pragma Assert (not OK);
   P.Acknowledge_Private_Retirement (Pool, 100, Tables, False, OK); pragma Assert (not OK);
   P.Acknowledge_Private_Retirement (Pool, 100, Tables, True, OK); pragma Assert (OK);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   P.Acknowledge_Private_Retirement (Pool, 100, Tables, True, OK); pragma Assert (not OK);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   P.Handle (Pool, 42, 100, P.Label, 4, 0, 0, [1, P.Create, 8192, 0], Reply, App);
   pragma Assert (App /= 0 and P.Client_Usage (Pool, 100).Charged = 16384);
   P.Complete (Pool, App, Intel_GPU_Buffer_Reply.From_Linear
     (16#200000#, Intel_GPU_Buffer_Reply.Layout.CPU_Base, 8192, 16#200000#), Reply, Consumed);
   pragma Assert (Consumed and Reply (0) = P.OK); Handle := Reply (2);
   P.Handle (Pool, 42, 100, P.Label, 4, 0, 0, [1, P.Close, Handle, 0], Reply, Other);
   pragma Assert (Reply (0) = P.OK and P.Client_Usage (Pool, 100).Charged = 16384);
   P.Acknowledge_Retirement (Pool, 100, App, True, OK); pragma Assert (OK);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   P.Acknowledge_Retirement (Pool, 100, App, True, OK); pragma Assert (not OK);
   P.Retire_Session (Pool, 100);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 8192);
   C.Acknowledge (Pool, 100, Parent, True, OK); pragma Assert (OK);
   pragma Assert (P.Client_Usage (Pool, 100).Charged = 0);
   C.Reserve (Pool, 100, Other, Pages => 1); pragma Assert (Other = 0);
   P.Reserve_Private (Pool, 101, Other, True, Pages => 4); pragma Assert (Other /= 0);
   P.Finish_Private (Pool, Other, Consumed); pragma Assert (Consumed);
   P.Retire_Session (Pool, 101);
   T.Acknowledge (Pool, 100, Other, True, OK); pragma Assert (not OK);
   T.Acknowledge (Pool, 101, Other, True, OK); pragma Assert (OK);
   pragma Assert (P.Client_Usage (Pool, 101).Charged = 0);
   P.Retire_Session (Pool, 102); -- closed even before first allocation
   P.Reserve_Private (Pool, 102, Other, Pages => 1); pragma Assert (Other = 0);
   P.Handle (Pool, 42, 103, P.Label, 4, 0, 0, [1, P.Create, 16384, 0], Reply, App);
   pragma Assert (App /= 0);
   P.Complete (Pool, App, (Ready => False), Reply, Consumed); pragma Assert (Consumed);
   P.Handle (Pool, 42, 103, P.Label, 4, 0, 0, [1, P.Create, 4096, 0], Reply, Other);
   pragma Assert (Other = 0 and P.Client_Usage (Pool, 103).Charged = 16384);
   P.Quarantine (Pool);
   P.Query_Accounting (Pool, 42, 103, P.Accounting_Label, 4, 0, 0, [1,0,0,0], Reply);
   pragma Assert (Reply = [P.Unavailable,1,0,0]);
   pragma Assert (P.Client_Usage (Pool, 103).Charged = 16384);
   declare
      Growing : P.Service;
      type RAM is array (Natural range 0 .. 4095) of Unsigned_8;
      Metadata : RAM := [others => 0] with Alignment => 4096;
      Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
      ID : P.Ticket;
   begin
      P.Configure_Client_Budgets (Growing, 4096, OK); pragma Assert (OK);
      for Session in Unsigned_64 range 1 .. 64 loop
         P.Reserve_Private (Growing, Session, ID, True, Pages => 1);
         if Session = 17 then
            pragma Assert (ID = 0 and not P.Client_Usage (Growing, Session).Known);
            P.Extend_Client_Accounts (Growing, Base, 4096, OK); pragma Assert (OK);
            P.Reserve_Private (Growing, Session, ID, True, Pages => 1);
         end if;
         pragma Assert (ID /= 0 and P.Client_Usage (Growing, Session).Charged = 4096);
         P.Extend_Client_Accounts (Growing, Base, 4096, OK); pragma Assert (not OK);
         P.Finish_Private (Growing, ID, Consumed); pragma Assert (Consumed);
         P.Retire_Session (Growing, Session);
         T.Acknowledge (Growing, Session, ID, True, OK); pragma Assert (OK);
         for Prior in 1 .. Session loop
            pragma Assert (P.Client_Usage (Growing, Prior).Closed);
            pragma Assert (P.Client_Usage (Growing, Prior).Charged = 0);
         end loop;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Client quota integration PASS: public/private/context combined admission, close retention, exact refunds, cross-session reuse and uncertain failure");
end Client_Quota_Requests_Tests;
