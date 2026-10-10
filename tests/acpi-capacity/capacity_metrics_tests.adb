with Ada.Text_IO;
with ACPI_Requests; use ACPI_Requests;
procedure Capacity_Metrics_Tests is
   type State_Access is access State;
   S : constant State_Access := new State (1, 128, 128, 0);
   R : Response;
   P : constant Packet := (Label => Read_Metrics, Data => [5,0,0,0], others => <>);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
begin
   Handle (S.all, Observer, P, R);
   Check (R.Status = OK and then R.Data = [0,0,65536,65536], "page5 default capacity and free");
   Handle (S.all, No_Authority, P, R);
   Check (R.Status = Denied and then R.Data = [0,0,0,0], "page5 unauthenticated denied");
   Handle (S.all, Observer, (P with delta Data => [5,1,0,0]), R);
   Check (R.Status = Malformed, "page5 malformed rejected");
   Ada.Text_IO.Put_Line ("CAPACITY-METRICS: PASS" & Checks'Image);
end Capacity_Metrics_Tests;
