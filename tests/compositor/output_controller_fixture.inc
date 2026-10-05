with Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Command_Line;
with Compositor_Lease_Request;
with System;
with Compositor_Output_Retirement;
procedure Output_Controller_Tests is
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   package LR renames Compositor_Lease_Request;
   use type LR.Phase;
   subtype Output_Index is Natural range 0 .. 1;
   package BP is subtype Live_Slot is Positive range 1 .. 3; end BP;
   package MG is
      subtype Grant_Reference is Natural range 0 .. 6;
      procedure Revoke (Grant : Grant_Reference; Accepted : out Boolean);
      function Retirement_Confirmed (Grant : Grant_Reference) return Boolean;
   end MG;
   Revoke_Count : array (MG.Grant_Reference) of Natural := (others => 0);
   Grant_Confirmed : array (MG.Grant_Reference) of Boolean := (others => False);
   Require_Grant : array (MG.Grant_Reference) of Boolean := (others => True);
   package body MG is
      procedure Revoke (Grant : Grant_Reference; Accepted : out Boolean) is
      begin
         pragma Assert (Grant /= 0 and Revoke_Count (Grant) = 0);
         Revoke_Count (Grant) := Revoke_Count (Grant) + 1; Accepted := True;
      end Revoke;
      function Retirement_Confirmed (Grant : Grant_Reference) return Boolean is (Grant_Confirmed (Grant));
   end MG;
   package PS is
      subtype Ticket is Natural range 0 .. 8;
      No_Ticket : constant Ticket := 0;
      function Charged (S : Natural) return Natural is (S);
   end PS;
   pixelStorage : Natural := 8;
   Allocation_Alive : array (PS.Ticket) of Boolean := (others => True);
   procedure releasePixelStorage (Allocation : in out PS.Ticket) is
   begin
      if Allocation = PS.No_Ticket then return; end if;
      pragma Assert (Allocation_Alive (Allocation));
      if Allocation <= 6 then pragma Assert (not Require_Grant (Allocation) or else Grant_Confirmed (Allocation));
      else pragma Assert ((for all G in 1 .. 6 => Grant_Confirmed (G))); end if;
      Allocation_Alive (Allocation) := False;
      pixelStorage := pixelStorage - 1; Allocation := PS.No_Ticket;
   end releasePixelStorage;
   package Desktop_Compositor is
      type Target_Release is (Targets_Retired, Targets_Busy, Targets_Unsafe);
      Result : Target_Release := Targets_Busy;
      procedure Forget_Targets (Value : out Target_Release);
   end Desktop_Compositor;
   package body Desktop_Compositor is
      procedure Forget_Targets (Value : out Target_Release) is begin Value := Result; end;
   end Desktop_Compositor;
   type Message_Tag is record label, length, flags, reserved : Natural := 0; end record;
   type Words is array (0 .. 0) of Natural;
   type Message is record tag : Message_Tag; words : Output_Controller_Tests.Words; end record;
   OP_DISPLAY_RELEASE : constant Natural := 1;
   Lease_Calls : array (Output_Index) of Natural := (others => 0);
   Attempts : Natural := 0;
   Allow_Submit : Boolean := False;
   CAP_SLOT_DISPLAY : constant Natural := 1;
   package CR is
      procedure Allocate (Sequence : in out Unsigned_64; Token : out Unsigned_64);
   end CR;
   package body CR is
      procedure Allocate (Sequence : in out Unsigned_64; Token : out Unsigned_64) is
      begin Sequence := Sequence + 1; Token := Sequence; end;
   end CR;
   package DSP is
      type Lease_Operation is (Release_Display);
      function Encode_Lease_Request (Operation : Lease_Operation) return Message is
        ((OP_DISPLAY_RELEASE, 4, 0, 0), (0 => 0));
      function With_Output (Value : Message; Output : Output_Index) return Message;
   end DSP;
   package body DSP is
      function With_Output (Value : Message; Output : Output_Index) return Message is
         Result : Message := Value;
      begin Result.tag.reserved := Output; return Result; end;
   end DSP;
   package CuBit is
      package Desktop_Messages is
         function From_Wire (Value : Message) return Message is (Value);
      end Desktop_Messages;
   end CuBit;
   function capSubmit (Slot : Natural; Value : Message; Token : Unsigned_64) return Boolean is
   begin
      pragma Assert (Slot = CAP_SLOT_DISPLAY and Value.tag.label = OP_DISPLAY_RELEASE and Token > 100);
      Attempts := Attempts + 1;
      if Allow_Submit then
         Lease_Calls (Value.tag.reserved) := Lease_Calls (Value.tag.reserved) + 1;
         pragma Assert (Lease_Calls (Value.tag.reserved) = 1);
      end if;
      return Allow_Submit;
   end capSubmit;
   function callDisplay (Operation : Natural; Output : Output_Index) return Message is
      use type Desktop_Compositor.Target_Release;
   begin
      pragma Assert (Operation = OP_DISPLAY_RELEASE and
        Desktop_Compositor.Result = Desktop_Compositor.Targets_Retired and Lease_Calls (Output) = 0);
      pragma Assert (False, "synchronous lease release forbidden");
      return ((OP_DISPLAY_RELEASE, 1, 0, 0), (0 => 0));
   end callDisplay;
   type Target_Storage is record
      Allocation : PS.Ticket := PS.No_Ticket;
      Granted : Boolean := False;
      Grant : MG.Grant_Reference := 0;
   end record;
   type Target_Array is array (BP.Live_Slot) of Target_Storage;
   package CP is
      type State is (Closed, Available, In_Flight);
      function Writable (S : State) return Boolean is (S = Available);
   end CP;
   type Output_Presentation is record
      Transfer : CP.State := CP.In_Flight;
      Leased, Enabled : Boolean := False;
      Targets : Target_Array;
      Completion_Token : Natural := 0;
   end record;
   presentations : array (Output_Index) of Output_Presentation;
   package Output_Retirement is new Compositor_Output_Retirement (3);
   use type Output_Retirement.Phase, Output_Retirement.Grant_Phase;
   outputRetirement : array (Output_Index) of Output_Retirement.State;
   outputLeaseRequests : array (Output_Index) of LR.State;
   outputDrainRequested, outputReopenPending, shutdownRequested : Boolean := False;
   backBufferReady : Boolean := True;
   sceneAllocation : PS.Ticket := 7;
   dragAllocation : PS.Ticket := 8;
   backBufferAddr, privateSceneAddr, dragBaseBufferAddr : System.Address := System.Null_Address;
   dragBaseReady, directOutput, drawingBackBuffer, cursorSaveValid : Boolean := True;
   sceneCapacityBytes : Natural := 10;
   requestSequence : Unsigned_64 := 100;
   retiredThrough : Unsigned_64 := 10;
   framePending, cursorPresentPending : Boolean := True;
   type Rect is record x,y,w,h : Natural := 0; end record;
   frameDamage : Rect;
   New_Window : Boolean := False;
   function anySurfaceUsed return Boolean is (New_Window);
   LF : constant Character := ASCII.LF;
   Restart : exception;
   procedure quarantinePresentations is begin raise Restart; end;
   procedure debugPrint (Text : String) is begin null; end;
   procedure exitCompositor (Code : Integer) is begin raise Restart; end;
@DESKTOP_ROUTINES@
begin
   if Mode in 4 .. 19 then
      backBufferReady := False; sceneAllocation := 0; dragAllocation := 0;
      pixelStorage := 3; New_Window := True; shutdownRequested := Mode mod 2 = 0;
      Require_Grant := (others => False);
      presentations (0).Completion_Token := 90;
      presentations (0).Transfer := CP.Closed;
      presentations (0).Leased := (Mode - 4) / 8 = 1;
      for B in BP.Live_Slot loop
         presentations (0).Targets (B) := (B, ((Mode - 4) / (2 ** (B - 1))) mod 2 = 1, B);
         Require_Grant (B) := presentations (0).Targets (B).Granted;
      end loop;
      releaseDisplayBuffer;
      for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
      pragma Assert (Attempts = 0 and pixelStorage = 3);
      Desktop_Compositor.Result := Desktop_Compositor.Targets_Retired;
      for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
      if presentations (0).Leased then
         pragma Assert (Attempts = 1000 and pixelStorage = 3 and Lease_Calls = (0,0));
         Allow_Submit := True; pumpOutputRetirements;
         pragma Assert (Attempts = 1001 and Lease_Calls = (1,0));
         for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
         pragma Assert (Attempts = 1001 and pixelStorage = 3 and (for all N of Revoke_Count => N = 0));
         LR.Complete (outputLeaseRequests (0), LR.Token (outputLeaseRequests (0)), True);
         pumpOutputRetirements;
      else pragma Assert (Attempts = 0); end if;
      for G in 1 .. 3 loop pragma Assert (Revoke_Count (G) = (if Require_Grant (G) then 1 else 0)); end loop;
      if (for some G in 1 .. 3 => Require_Grant (G)) then
         pragma Assert (pixelStorage = 3 and presentations (0).Completion_Token = 90);
      end if;
      Grant_Confirmed := (others => True); pumpOutputRetirements;
      pragma Assert (pixelStorage = 0 and not outputDrainRequested and outputReopenPending = not shutdownRequested);
      for Poll in 1 .. 1000 loop releaseDisplayBuffer; end loop;
      pragma Assert (pixelStorage = 0);
      Ada.Text_IO.Put_Line ("ASYNC-LEASE-CALLER: PASS partial/shutdown mode" & Mode'Image);
      return;
   end if;
   for O in Output_Index loop
      presentations (O).Leased := True; presentations (O).Enabled := True;
      presentations (O).Completion_Token := 90 + O;
      for B in BP.Live_Slot loop presentations (O).Targets (B) := (O * 3 + B, True, O * 3 + B); end loop;
   end loop;
   releaseDisplayBuffer;
   for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
   pragma Assert (Attempts = 0 and pixelStorage = 8);
   Desktop_Compositor.Result := Desktop_Compositor.Targets_Retired;
   for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
   pragma Assert (Attempts = 0 and pixelStorage = 8);
   for O in Output_Index loop presentations (O).Transfer := CP.Available; end loop;
   for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
   pragma Assert (Attempts = 2000 and Lease_Calls = (0,0) and pixelStorage = 8);
   pragma Assert ((for all N of Revoke_Count => N = 0));
   Allow_Submit := True; pumpOutputRetirements;
   pragma Assert (Attempts = 2002 and Lease_Calls = (1,1));
   for Poll in 1 .. 10000 loop pumpOutputRetirements; end loop;
   pragma Assert (Attempts = 2002 and pixelStorage = 8 and outputDrainRequested);
   if Mode > 0 then
      LR.Complete (outputLeaseRequests (0), LR.Token (outputLeaseRequests (0)) + (if Mode = 2 then 1 else 0), Mode /= 1);
      if Mode = 3 then LR.Complete (outputLeaseRequests (0), LR.Token (outputLeaseRequests (0)), True); end if;
      begin pumpOutputRetirements; pragma Assert (False);
      exception when Restart =>
         pragma Assert (pixelStorage = 8 and retiredThrough = 10 and (for all N of Revoke_Count => N = 0));
      end;
   else
      New_Window := True;
      LR.Complete (outputLeaseRequests (0), LR.Token (outputLeaseRequests (0)), True);
      pumpOutputRetirements;
      for G in 1 .. 3 loop pragma Assert (Revoke_Count (G) = 1); Grant_Confirmed (G) := True; end loop;
      pumpOutputRetirements;
      pragma Assert (pixelStorage = 5 and presentations (0).Completion_Token = 90 and retiredThrough = 10);
      for Poll in 1 .. 1000 loop pumpOutputRetirements; end loop;
      pragma Assert (Attempts = 2002 and pixelStorage = 5 and (for all G in 4 .. 6 => Revoke_Count (G) = 0));
      LR.Complete (outputLeaseRequests (1), LR.Token (outputLeaseRequests (1)), True);
      pumpOutputRetirements;
      for G in 4 .. 6 loop pragma Assert (Revoke_Count (G) = 1); Grant_Confirmed (G) := True; end loop;
      pumpOutputRetirements;
      pragma Assert (pixelStorage = 0 and retiredThrough = requestSequence and outputReopenPending and not outputDrainRequested);
   end if;
   Ada.Text_IO.Put_Line ("ASYNC-LEASE-CALLER: PASS exact cleanup mode" & Mode'Image);
end Output_Controller_Tests;
