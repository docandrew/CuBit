with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Lease_Request;
procedure Routing_Tests is
   package LR renames Compositor_Lease_Request;
   use type LR.Phase;
   subtype Output_Index is Natural range 0 .. 1;
   outputLeaseRequests : array (Output_Index) of LR.State;
   type Tag is record label : Unsigned_32 := 16#906#; length, flags : Unsigned_8 := 0; reserved : Unsigned_16 := 0; end record;
   type Words is array (0 .. 3) of Unsigned_64;
   type Message is record tag : Routing_Tests.Tag; words : Routing_Tests.Words := (others => 0); end record;
   type Completion_Entry is record token : Unsigned_64 := 0; valid : Boolean := True; status : Natural := 0; msg : Message; end record;
   COMPLETION_OK : constant Natural := 0;
   OP_DISPLAY_RELEASE : constant Unsigned_32 := 16#906#;
   completion : Completion_Entry;
   matched : Boolean;
   Rejected : exception;
   LF : constant Character := ASCII.LF;
   procedure debugPrint (Text : String) is begin null; end;
   procedure quarantinePresentations is begin raise Rejected; end;
   procedure Route is
   begin
      matched := False;
@DESKTOP_ROUTINES@
   end Route;
   procedure Initialize is
      Accepted : Boolean;
   begin
      for O in Output_Index loop
         declare Empty : LR.State; begin outputLeaseRequests (O) := Empty; end;
         LR.Prepare (outputLeaseRequests (O), 101 + Unsigned_64 (O), Accepted);
         pragma Assert (Accepted);
         LR.Submitted (outputLeaseRequests (O), True);
      end loop;
   end Initialize;
begin
   for O in Output_Index loop
      for Fault in 0 .. 10 loop
         Initialize;
         completion := (101 + Unsigned_64 (O), True, COMPLETION_OK, ((OP_DISPLAY_RELEASE, 1, 0, 0), (others => 0)));
         case Fault is
            when 1 => completion.valid := False;
            when 2 => completion.status := 1;
            when 3 => completion.msg.tag.label := 0;
            when 4 => completion.msg.tag.length := 0;
            when 5 => completion.msg.tag.flags := 1;
            when 6 => completion.msg.tag.reserved := 1;
            when 7 .. 10 => completion.msg.words (Fault - 7) := 1;
            when others => null;
         end case;
         begin
            Route;
            pragma Assert (Fault = 0 and matched and LR.Status (outputLeaseRequests (O)) = LR.Released);
         exception when Rejected =>
            pragma Assert (Fault /= 0 and matched and LR.Status (outputLeaseRequests (O)) = LR.Quarantined);
         end;
         pragma Assert (LR.Status (outputLeaseRequests (1 - O)) = LR.Pending);
      end loop;
   end loop;
   Initialize;
   for T in Unsigned_64 range 0 .. 100 loop
      completion.token := T; Route;
      pragma Assert (not matched and (for all S of outputLeaseRequests => LR.Status (S) = LR.Pending));
   end loop;
   completion := (101, True, 0, ((OP_DISPLAY_RELEASE, 1, 0, 0), (others => 0)));
   Route;
   begin Route; pragma Assert (False); exception when Rejected => null; end;
   pragma Assert (LR.Status (outputLeaseRequests (0)) = LR.Quarantined and LR.Status (outputLeaseRequests (1)) = LR.Pending);
   Ada.Text_IO.Put_Line ("LEASE-ROUTING: PASS exact collector branch, two outputs, all envelope fields, unrelated tokens and duplicate reply");
end Routing_Tests;
