--  The interrupter's Event Handler Busy race (xHCI 1.2, 4.17.2, 5.5.2.3.3),
--  explored exhaustively against a model controller.
--
--  The controller writes an event TRB, then (separately) sets IP and EHB
--  and sends the MSI, but only while EHB is clear. Software clears EHB by
--  writing ERDP with EHB set. A driver that polls between the event write
--  and the EHB set consumes the event first; if it then services the
--  interrupt without writing ERDP (nothing left to consume), EHB stays set,
--  no further interrupt is ever raised, and the sleeping driver never
--  wakes: "the mouse works for a while, then stops".
--
--  XHCI_Rings.Must_Write_Dequeue is the production policy. The old policy
--  (write only after consuming) must reach the stuck state; the production
--  policy must never reach it, over every interleaving of up to
--  Max_Events events.
with Ada.Text_IO;
with XHCI_Rings;

procedure EHB_Tests is
   Max_Events : constant := 3;
   Awake_Polls : constant := 2;
   type Policy is access function (Consumed : Natural; Interrupted : Boolean) return Boolean;

   --  The old driver: ERDP only after consuming (Interrupted is ignored).
   function Old_Policy (Consumed : Natural; Interrupted : Boolean) return Boolean is
      pragma Unreferenced (Interrupted);
   begin
      return Consumed > 0;
   end Old_Policy;

   type State is record
      Posted     : Natural := 0;     --  events the controller wrote
      Consumed   : Natural := 0;     --  events the driver consumed
      Signal_Due : Boolean := False; --  interrupter has something to signal
      EHB        : Boolean := False;
      IRQ        : Boolean := False; --  an MSI the driver has not serviced
      Polls_Left : Natural := 0;     --  driver awake for this many polls
   end record;

   Stuck_Found : Boolean;
   States : Natural;

   procedure Write_ERDP (S : in out State) is
   begin
      S.EHB := False;
      --  5.5.2.3.3: a dequeue pointer behind the enqueue pointer re-arms.
      if S.Consumed < S.Posted then
         S.Signal_Due := True;
      end if;
   end Write_ERDP;

   procedure Explore (S : State; Write_Policy : Policy) is
      N : State;
      Took : Natural;
   begin
      States := States + 1;
      --  Stuck: asleep, no interrupt coming, and EHB blocks every future one.
      if S.Polls_Left = 0 and then not S.IRQ and then S.EHB then
         Stuck_Found := True;
         return;
      end if;
      --  Controller: post an event.
      if S.Posted < Max_Events then
         N := S;
         N.Posted := N.Posted + 1;
         N.Signal_Due := True;
         Explore (N, Write_Policy);
      end if;
      --  Controller: assert the interrupt (IP, EHB, MSI; IP self-clears).
      if S.Signal_Due and then not S.EHB then
         N := S;
         N.Signal_Due := False;
         N.EHB := True;
         N.IRQ := True;
         Explore (N, Write_Policy);
      end if;
      --  Driver: service the interrupt doorbell.
      if S.IRQ then
         N := S;
         N.IRQ := False;
         Took := N.Posted - N.Consumed;
         N.Consumed := N.Posted;
         if Write_Policy (Took, True) then
            Write_ERDP (N);
         end if;
         N.Polls_Left := Awake_Polls;
         Explore (N, Write_Policy);
      end if;
      --  Driver: an awake loop pass polls the ring without an interrupt.
      if S.Polls_Left > 0 then
         N := S;
         N.Polls_Left := N.Polls_Left - 1;
         Took := N.Posted - N.Consumed;
         N.Consumed := N.Posted;
         if Write_Policy (Took, False) then
            Write_ERDP (N);
         end if;
         Explore (N, Write_Policy);
      end if;
   end Explore;

   Initial : constant State := (Polls_Left => 0, others => <>);
begin
   Stuck_Found := False;
   States := 0;
   Explore (Initial, Old_Policy'Access);
   pragma Assert (Stuck_Found, "the model must reproduce the old driver's stall");
   Ada.Text_IO.Put_Line ("ehb-tests: old policy stalls (reproduced) in" & Natural'Image (States) & " states");

   Stuck_Found := False;
   States := 0;
   Explore (Initial, XHCI_Rings.Must_Write_Dequeue'Access);
   pragma Assert (not Stuck_Found, "production ERDP policy can lose the interrupter");
   Ada.Text_IO.Put_Line ("ehb-tests: PASS production policy never stalls;" &
                         Natural'Image (States) & " states");
end EHB_Tests;
