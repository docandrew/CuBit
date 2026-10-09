with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Page_Allocation;
with Process_Memory_Budget; use Process_Memory_Budget;

-- Actual acquisition and budget cores, mocked physical hooks. Tests the
-- required adapter protocol, NOT native locks, page tables or GPU retirement.
procedure Page_Charge_Handoff_Test is
   Negative_Bound_State : constant Boolean :=
     Ada.Command_Line.Argument_Count = 1 and then
     Ada.Command_Line.Argument (1) = "--negative-bound-state";
   use type Page_Allocation.Result;
   type Failure is (None, At_Allocate, At_Track, At_Claim, At_Bind, At_Map);
   Mode : Failure;
   Budget : Ledger;
   Bound, Handed_Off, Tracked : Boolean;
   Frees, Refunds : Natural;

   procedure Allocate (Frame : out Unsigned_64) is
   begin
      Frame := (if Mode = At_Allocate then 0 else 4096);
   end Allocate;
   procedure Release_Frame (Frame : Unsigned_64) is
      OK : Boolean;
   begin
      pragma Assert (Frame = 4096);
      Frees := Frees + 1;
      if Bound then
         Bound := False;
         Release (Budget, Ordinary, 1, OK);
         pragma Assert (OK);
         Refunds := Refunds + 1;
      end if;
   end Release_Frame;
   procedure Track (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096);
      Accepted := Mode /= At_Track;
      Tracked := Accepted;
   end Track;
   procedure Forget is
   begin
      pragma Assert (Tracked);
      Tracked := False;
   end Forget;
   procedure Claim (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096);
      Accepted := Mode not in At_Claim | At_Bind;
      if Accepted then
         Bound := True;
         -- Sticky handoff: even if the callback already refunded during
         -- failed Map, the outer rollback must not refund a second time.
         Handed_Off := True;
      end if;
   end Claim;
   procedure Map (Frame : Unsigned_64; Accepted : out Boolean) is
   begin
      pragma Assert (Frame = 4096 and Bound);
      Accepted := Mode /= At_Map;
   end Map;
   procedure Acquire is new Page_Allocation.Acquire
     (Unsigned_64, Allocate, Release_Frame, Track, Forget, Claim, Map);
   Frame : Unsigned_64;
   Outcome : Page_Allocation.Result;
   OK : Boolean;
begin
   Adopt (Budget, 1, OK);
   pragma Assert (OK);
   for Fault in Failure loop
      Mode := Fault;
      Bound := False;
      Handed_Off := False;
      Tracked := False;
      Frees := 0;
      Refunds := 0;
      Reserve (Budget, Ordinary, 1, OK);
      pragma Assert (OK);
      Acquire (Frame, Outcome);
      if Outcome /= Page_Allocation.Page_Added and then
        (if Negative_Bound_State then not Bound else not Handed_Off)
      then
         if Fault = At_Map and Negative_Bound_State then
            Ada.Text_IO.Put_Line ("MUTATION: attempting duplicate map-failure refund");
         end if;
         Release (Budget, Ordinary, 1, OK);
         pragma Assert (OK);
         Refunds := Refunds + 1;
      end if;
      if Fault = None then
         pragma Assert (Outcome = Page_Allocation.Page_Added and Bound);
         pragma Assert (Used (Budget) = 1 and Refunds = 0);
         -- Owner death/requested free alone must leave the charge held.
         Reserve (Budget, DMA_Backing, 1, OK);
         pragma Assert (not OK and Used (Budget) = 1);
         Forget;
         Release_Frame (Frame); -- Simulated final pin retirement.
      else
         pragma Assert (Outcome /= Page_Allocation.Page_Added);
      end if;
      pragma Assert (Used (Budget) = 0 and Refunds = 1 and not Tracked);
      pragma Assert (Frees = (if Fault = At_Allocate then 0 else 1));
      if Fault = At_Map then
         pragma Assert (Handed_Off and not Bound);
      end if;
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS page-charge handoff: allocation/track/claim/bind/map rollback, exactly one refund (mock physical hooks)");
end Page_Charge_Handoff_Test;
