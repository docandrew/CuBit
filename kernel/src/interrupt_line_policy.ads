-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  One legacy interrupt line's owners and its mask-until-acknowledged
--  state machine (Interrupt_Line_ABI), as pure, proved transitions.
--
--  @description
--  A line has a bounded set of owners (shared PCI INTx needs more than one;
--  the bound is the kernel's, Owners_Full past it), one trigger mode and
--  polarity (every owner agrees: Mode_Conflict otherwise), and a delivery
--  count. Interrupt_Lines applies each transition's Hardware_Action to the
--  I/O APIC under its lock.
--
--  Level-triggered: Deliver masks the input and records every owner as
--  Waiting; the input stays masked (Held_Masked) until no owner is waiting.
--  An owner stops waiting by Acknowledge (after clearing its source) or by
--  leaving (Release). Unmasking re-evaluates the input: a source still
--  asserted is delivered again, so nothing is lost, and nothing is
--  delivered while an owner still has to look, so the line cannot storm.
--  A level line left with no owner is masked (nobody would clear it).
--
--  Edge-triggered: never masked by delivery; Acknowledge is accepted and
--  does nothing.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interrupt_Line_ABI; use Interrupt_Line_ABI;

package Interrupt_Line_Policy with
   SPARK_Mode => On,
   Pure
is
   --  The most owners one line has at once (shared PCI INTx).
   Maximum_Owners : constant := 8;
   subtype Owner_Slot is Natural range 0 .. Maximum_Owners - 1;

   --  An owner is a process identity word; zero is no owner.
   No_Owner : constant Unsigned_64 := 0;

   type Owner_Array is array (Owner_Slot) of Unsigned_64;
   type Flag_Array  is array (Owner_Slot) of Boolean;

   type Line_State is record
      Owners      : Owner_Array := [others => No_Owner];
      Waiting     : Flag_Array := [others => False];
      Trigger     : Trigger_Mode := Edge;
      Polarity    : Pin_Polarity := Active_High;
      --  Masked by a level delivery, until no owner is waiting.
      Held_Masked : Boolean := False;
      Delivered   : Delivery_Count := 0;
   end record;

   function Has_Owner (S : Line_State) return Boolean is
     (for some I in Owner_Slot => S.Owners (I) /= No_Owner);

   function Any_Waiting (S : Line_State) return Boolean is
     (for some I in Owner_Slot => S.Waiting (I));

   function Is_Owner (S : Line_State; Owner : Unsigned_64) return Boolean is
     (for some I in Owner_Slot => S.Owners (I) = Owner)
   with Pre => Owner /= No_Owner;

   function Is_Waiting (S : Line_State; Owner : Unsigned_64) return Boolean is
     (for some I in Owner_Slot => S.Owners (I) = Owner and then S.Waiting (I))
   with Pre => Owner /= No_Owner;

   --  Only owners wait; only a masked line has waiters; only a level line
   --  is masked; and an owned masked line is masked because someone waits.
   function Valid (S : Line_State) return Boolean is
     ((for all I in Owner_Slot =>
         (if S.Waiting (I) then S.Owners (I) /= No_Owner))
      and then (if Any_Waiting (S) then S.Held_Masked)
      and then (if S.Held_Masked then S.Trigger = Level)
      and then (if S.Held_Masked and then Has_Owner (S) then Any_Waiting (S)));

   --  What the I/O APIC must do after a transition.
   type Hardware_Action is
     (No_Change,          --  leave the redirection entry alone
      Mask_Input,         --  set its mask bit
      Unmask_Input,       --  clear its mask bit (the input is re-evaluated)
      Program_Unmasked,   --  write mode, polarity and destination, unmasked
      Program_Masked);    --  write them, masked

   type Route_Result is (Routed, Owners_Full, Mode_Conflict);

   function Same_Mode
     (S : Line_State; Trigger : Trigger_Mode; Polarity : Pin_Polarity)
      return Boolean is
     (S.Trigger = Trigger and then S.Polarity = Polarity);

   function Has_Free_Slot (S : Line_State) return Boolean is
     (for some I in Owner_Slot => S.Owners (I) = No_Owner);

   ---------------------------------------------------------------------------
   --  Route: subscribe Owner (idempotent) in the given mode. A line with
   --  owners keeps its mode: another mode is Mode_Conflict. The input is
   --  programmed masked exactly when an owner still has to acknowledge.
   ---------------------------------------------------------------------------
   procedure Route
     (S        : in out Line_State;
      Owner    : Unsigned_64;
      Trigger  : Trigger_Mode;
      Polarity : Pin_Polarity;
      Result   : out Route_Result;
      Action   : out Hardware_Action)
   with Pre  => Valid (S) and then Owner /= No_Owner,
        Post => Valid (S)
          and then S.Delivered = S'Old.Delivered
          and then (Result = Mode_Conflict) =
                     (Has_Owner (S'Old) and then not Same_Mode (S'Old, Trigger, Polarity))
          and then (Result = Owners_Full) =
                     ((not Has_Owner (S'Old) or else Same_Mode (S'Old, Trigger, Polarity))
                      and then not Is_Owner (S'Old, Owner)
                      and then not Has_Free_Slot (S'Old))
          and then (if Result = Routed then
                      Is_Owner (S, Owner)
                      and then Same_Mode (S, Trigger, Polarity)
                      and then S.Held_Masked = Any_Waiting (S)
                      and then S.Waiting = S'Old.Waiting
                      and then (if S.Held_Masked then Action = Program_Masked
                                else Action = Program_Unmasked)
                      and then (for all I in Owner_Slot =>
                                  (if S'Old.Owners (I) /= No_Owner
                                   then S.Owners (I) = S'Old.Owners (I)))
                    else S = S'Old and then Action = No_Change);

   ---------------------------------------------------------------------------
   --  Deliver: the kernel took an interrupt on the line. Dispatch says
   --  whether any owner is told. A level line is masked and every owner
   --  waits; an edge line is left alone.
   ---------------------------------------------------------------------------
   procedure Deliver
     (S        : in out Line_State;
      Action   : out Hardware_Action;
      Dispatch : out Boolean)
   with Pre  => Valid (S),
        Post => Valid (S)
          and then S.Owners = S'Old.Owners
          and then S.Trigger = S'Old.Trigger
          and then S.Polarity = S'Old.Polarity
          and then S.Delivered = Next_Count (S'Old.Delivered)
          and then Dispatch = Has_Owner (S)
          and then (if S.Trigger = Level then
                      Action = Mask_Input
                      and then S.Held_Masked
                      and then (for all I in Owner_Slot =>
                                  S.Waiting (I) = (S.Owners (I) /= No_Owner))
                    else Action = No_Change
                         and then not S.Held_Masked
                         and then S.Waiting = S'Old.Waiting);

   ---------------------------------------------------------------------------
   --  Acknowledge: Owner has cleared its source. Refused (nothing changes)
   --  when Owner does not own the line. The input is unmasked exactly when
   --  the line was held masked and no owner is waiting any more.
   ---------------------------------------------------------------------------
   procedure Acknowledge
     (S        : in out Line_State;
      Owner    : Unsigned_64;
      Accepted : out Boolean;
      Action   : out Hardware_Action)
   with Pre  => Valid (S) and then Owner /= No_Owner,
        Post => Valid (S)
          and then Accepted = Is_Owner (S'Old, Owner)
          and then S.Owners = S'Old.Owners
          and then S.Trigger = S'Old.Trigger
          and then S.Polarity = S'Old.Polarity
          and then S.Delivered = S'Old.Delivered
          and then (if not Accepted then S = S'Old and then Action = No_Change
                    else not Is_Waiting (S, Owner)
                         and then (for all I in Owner_Slot =>
                                     (if S'Old.Owners (I) /= Owner
                                      then S.Waiting (I) = S'Old.Waiting (I)))
                         and then (Action = Unmask_Input) =
                                    (S'Old.Held_Masked and then not Any_Waiting (S))
                         and then Action in No_Change | Unmask_Input
                         and then S.Held_Masked = Any_Waiting (S)
                         and then (if Action = Unmask_Input then not Any_Waiting (S)));

   ---------------------------------------------------------------------------
   --  Release: Owner leaves (it exited). Its slots and waits are cleared.
   --  A level line left with no owner is masked; one whose last waiter
   --  left is unmasked.
   ---------------------------------------------------------------------------
   procedure Release
     (S      : in out Line_State;
      Owner  : Unsigned_64;
      Action : out Hardware_Action)
   with Pre  => Valid (S) and then Owner /= No_Owner,
        Post => Valid (S)
          and then not Is_Owner (S, Owner)
          and then S.Delivered = S'Old.Delivered
          and then (for all I in Owner_Slot =>
                      (if S'Old.Owners (I) /= Owner then
                         S.Owners (I) = S'Old.Owners (I)
                         and then S.Waiting (I) = S'Old.Waiting (I)))
          and then (if not Is_Owner (S'Old, Owner) then S = S'Old and then Action = No_Change)
          and then (if Is_Owner (S'Old, Owner) and then not Has_Owner (S)
                       and then S.Trigger = Level
                    then Action = Mask_Input and then S.Held_Masked)
          and then (if Action = Unmask_Input then not Any_Waiting (S) and then Has_Owner (S))
          and then Action in No_Change | Mask_Input | Unmask_Input;

end Interrupt_Line_Policy;
