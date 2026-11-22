------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One root port's attach/detach lifecycle: debounce, reset, reset
--  recovery, enumeration, release.
--
--  @description
--  Driven by Port Status Change events (xHCI 1.2, 4.19.2) and deadlines;
--  never waits. The driver reads PORTSC, acknowledges its change bits
--  (XHCI_Registers.Acknowledge_Changes) and hands the reading it acted on
--  to Step; it performs the returned action and comes back at
--  Next_Deadline.
--
--  Timing is the specification's minimum, never an arbitrary sleep:
--    * connection stable for Debounce_Stable_Ms (USB 2.0, 7.1.7.3, TATTDB;
--      xHCI 4.3 step 1 leaves debouncing to software); any connect change
--      restarts the interval; Debounce_Limit_Ms bounds a bouncing port;
--    * USB2 ports are reset by software (PR, 4.3.1 and 4.19.1.1); USB3
--      ports reach Enabled by link training alone (4.19.1.2), so a port
--      found enabled after debouncing skips the reset; Reset_Limit_Ms
--      bounds how long the controller may take to finish one;
--    * Reset_Recovery_Ms after the reset before the first request
--      (USB 2.0, 7.1.7.5, TRSTRCY).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package XHCI_Port_Machine with SPARK_Mode => On, Pure is

   subtype Milliseconds is Unsigned_64;
   No_Deadline : constant Milliseconds := Milliseconds'Last;

   Debounce_Stable_Ms : constant Milliseconds := 100;
   Debounce_Limit_Ms  : constant Milliseconds := 2_000;
   --  Linux hub.c HUB_RESET_TIMEOUT (one reset attempt).
   Reset_Limit_Ms     : constant Milliseconds := 800;
   Reset_Recovery_Ms  : constant Milliseconds := 10;

   function Plus (Now, Interval : Milliseconds) return Milliseconds is
     (if Now <= Milliseconds'Last - Interval then Now + Interval
      else Milliseconds'Last)
   with Post => Plus'Result >= Now;

   function Since (Now, Earlier : Milliseconds) return Milliseconds is
     (if Now >= Earlier then Now - Earlier else 0);

   type Phase is
     (Empty,        --  nothing connected
      Debouncing,   --  connected, not yet stable for Debounce_Stable_Ms
      Resetting,    --  port reset requested or underway
      Recovering,   --  enabled; reset recovery interval running
      Ready,        --  waiting for the (single) enumeration engine
      Enumerating,  --  the engine owns this port
      Attached,     --  enumerated (or holds slots of an unsupported device)
      Failed,       --  gave up; waits for a disconnect or reconnect
      Releasing);   --  the driver is disabling this port's slots

   type Port_State is record
      Current      : Phase := Empty;
      Connected_At : Milliseconds := 0;
      Stable_From  : Milliseconds := 0;
      Deadline     : Milliseconds := No_Deadline;
   end record;

   --  What one PORTSC reading says (XHCI_Registers names the bits).
   type Observation is record
      Connected       : Boolean := False;   --  CCS
      Enabled         : Boolean := False;   --  PED
      Resetting       : Boolean := False;   --  PR or WPR
      Super_Speed     : Boolean := False;
      Connect_Changed : Boolean := False;   --  CSC
      Enable_Changed  : Boolean := False;   --  PEC
   end record;
   function Observe (Status : Unsigned_32) return Observation;

   type Action is
     (No_Action,
      Start_Reset,     --  write PORTSC with PR (XHCI_Ports.Reset_Write)
      Release_Slots,   --  disable every slot behind this port, then Released
      Give_Up);        --  log the failure; the port waits for a disconnect

   procedure Step
     (S : in out Port_State; O : Observation; Now : Milliseconds; A : out Action)
   with Post =>
     --  A disconnected port is empty or being released.
     (if not O.Connected then S.Current in Empty | Releasing)
     --  A reset only after a stable debounce interval with no change.
     and then (if A = Start_Reset then
                 S'Old.Current = Debouncing and then S.Current = Resetting
                 and then not O.Connect_Changed
                 and then Since (Now, S'Old.Stable_From) >= Debounce_Stable_Ms)
     --  Enumeration waits for the recovery interval after an enabled port.
     and then (if S.Current = Recovering and then S'Old.Current /= Recovering then
                 O.Connected and then O.Enabled and then not O.Resetting
                 and then S.Deadline = Plus (Now, Reset_Recovery_Ms))
     and then (if S.Current = Ready and then S'Old.Current /= Ready then
                 S'Old.Current = Recovering and then Now >= S'Old.Deadline)
     --  Slots are released only from phases that can hold them.
     and then (if A = Release_Slots then
                 S.Current = Releasing
                 and then S'Old.Current in Enumerating | Attached | Failed)
     and then (if S'Old.Current in Enumerating | Attached
                 and then S.Current not in Enumerating | Attached
               then A = Release_Slots);

   --  The engine takes a Ready port.
   procedure Claim (S : in out Port_State)
   with Pre => S.Current = Ready, Post => S.Current = Enumerating;

   --  The engine finished: Attached, or Failed after it released any slot.
   procedure Finished (S : in out Port_State; Success : Boolean)
   with Pre => S.Current = Enumerating,
     Post => S.Current = (if Success then Attached else Failed);

   --  Software replug (XHCI_Endpoint_Recovery's last rung): release the
   --  attached device's slots; Released then debounces, resets and
   --  re-enumerates it like a new connection.
   procedure Recover (S : in out Port_State)
   with Pre => S.Current = Attached, Post => S.Current = Releasing;

   --  The driver disabled the port's slots. A port still connected starts
   --  over (a new device, or the same one after its port was disabled).
   procedure Released (S : in out Port_State; Connected : Boolean; Now : Milliseconds)
   with Pre => S.Current = Releasing,
     Post => S.Current = (if Connected then Debouncing else Empty);

   --  The ports found at the end of boot enumeration.
   function Booted (Connected, Has_Slots : Boolean; Now : Milliseconds) return Port_State
   with Post => Booted'Result.Current =
     (if Has_Slots then Attached elsif Connected then Debouncing else Empty);

   function Next_Deadline (S : Port_State) return Milliseconds is
     (if S.Current in Debouncing | Resetting | Recovering then S.Deadline
      else No_Deadline);

   function Phase_Name (P : Phase) return String is
     (case P is
         when Empty => "empty", when Debouncing => "debounce",
         when Resetting => "reset", when Recovering => "recovery",
         when Ready => "ready", when Enumerating => "enumerate",
         when Attached => "attached", when Failed => "failed",
         when Releasing => "release");

end XHCI_Port_Machine;
