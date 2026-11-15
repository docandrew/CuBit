-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Legacy interrupt lines: the kernel/userspace ABI of ENABLE_IRQ, which
--  both routes a line (the device manager) and acknowledges it (its owner).
--
--  @description
--  One definition, used by the kernel (Syscall.Admin, Interrupt_Lines) and
--  by userspace (CuBit.Handles.Control, CuBit.Handles.Devices). A legacy
--  line is an input of the I/O APIC that serves global system interrupts
--  (GSIs) from 0: input I is delivered on IDT vector Vector_Base + I. Input
--  0 (the timer) is the kernel's; inputs 1 .. 15 are routable; a GSI of 16
--  or more has no vector here yet and is refused by whoever resolves it
--  (CuBit.ACPI_SCI_Route).
--
--  Level-triggered lines are masked at the I/O APIC when the kernel
--  delivers them and stay masked until every owner that was told
--  acknowledges after clearing its source. The acknowledgement is ENABLE_IRQ
--  again, in its owner form (Route_Acknowledge): "enable my line again". No
--  system call exists only for it. Edge-triggered lines are never masked by
--  delivery; acknowledging one is accepted and changes nothing.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Interrupt_Line_ABI with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  Lines and their vectors
   ---------------------------------------------------------------------------
   Vector_Base : constant := 32;
   Timer_Input : constant := 0;
   First_Input : constant := 1;
   Last_Input  : constant := 15;
   subtype Line_Input is Natural range First_Input .. Last_Input;

   First_Vector : constant := Vector_Base + First_Input;
   Last_Vector  : constant := Vector_Base + Last_Input;
   subtype Line_Vector is Natural range First_Vector .. Last_Vector;

   function Vector_Of (Input : Line_Input) return Line_Vector is
     (Vector_Base + Input);
   function Input_Of (Vector : Line_Vector) return Line_Input is
     (Vector - Vector_Base);

   type Trigger_Mode is (Edge, Level);
   type Pin_Polarity is (Active_High, Active_Low);

   ---------------------------------------------------------------------------
   --  ENABLE_IRQ's route word: the destination local APIC in the low byte,
   --  then the trigger mode and polarity of the redirection entry.
   --
   --  Route_Acknowledge alone (no other bit) selects the owner form instead:
   --  arg0 = the line's vector, arg4 = the caller's own interrupt handle
   --  (read right) naming exactly that one line, arg1 and arg3 zero (the
   --  caller acknowledges for itself and names no one else). It routes
   --  nothing and changes no trigger, polarity or destination.
   ---------------------------------------------------------------------------
   Route_CPU_Mask     : constant Unsigned_64 := 16#00FF#;
   Route_Level        : constant Unsigned_64 := 16#0100#;
   Route_Active_Low   : constant Unsigned_64 := 16#0200#;
   Route_Acknowledge  : constant Unsigned_64 := 16#0400#;
   Route_Defined_Bits : constant Unsigned_64 :=
     Route_CPU_Mask or Route_Level or Route_Active_Low;

   --  The owner form's argument words that must be zero.
   No_Owner_Word     : constant Unsigned_64 := 0;
   No_Authority_Word : constant Unsigned_64 := 0;

   function Is_Acknowledge_Route (Word : Unsigned_64) return Boolean is
     (Word = Route_Acknowledge);
   pragma Compile_Time_Error
     ((Route_Acknowledge and Route_Defined_Bits) /= 0,
      "the owner form must not be a route word");

   subtype Route_CPU is Unsigned_64 range 0 .. Route_CPU_Mask;

   function Route_Word
     (CPU : Route_CPU; Trigger : Trigger_Mode; Polarity : Pin_Polarity)
      return Unsigned_64
   is (CPU
       or (if Trigger = Level then Route_Level else 0)
       or (if Polarity = Active_Low then Route_Active_Low else 0));

   function Defined_Route (Word : Unsigned_64) return Boolean is
     ((Word and not Route_Defined_Bits) = 0);

   function CPU_Of (Word : Unsigned_64) return Route_CPU is
     (Word and Route_CPU_Mask);
   function Trigger_Of (Word : Unsigned_64) return Trigger_Mode is
     (if (Word and Route_Level) /= 0 then Level else Edge);
   function Polarity_Of (Word : Unsigned_64) return Pin_Polarity is
     (if (Word and Route_Active_Low) /= 0 then Active_Low else Active_High);

   ---------------------------------------------------------------------------
   --  ENABLE_IRQ's result word: why a line was or was not routed, or (the
   --  owner form) why an acknowledgement was refused.
   ---------------------------------------------------------------------------
   type Route_Outcome is
     (Routed,                --  the owner is subscribed and the input enabled
      Not_A_Line,            --  the vector is not a routable legacy line
      Undefined_Route,       --  the route word has undefined bits
      Invalid_Owner,         --  the owner identity names no live process
      No_Process_Authority,  --  no process handle with Grant for the owner
      No_Line_Authority,     --  no interrupt handle with Grant for the line
      Owners_Full,           --  the line's shared owner set is full
      Mode_Conflict,         --  owned already with another trigger/polarity
      No_Controller,         --  no I/O APIC serving GSI 0 for the line
      Input_Absent,          --  the I/O APIC has no such input
      Level_Needs_IO_APIC,   --  a level line needs the I/O APIC's mask
      Not_Line_Owner,        --  owner form: the caller is not routed the line
      --  Owner form: arg1/arg3 not zero, or arg0 is not the handle's line.
      Malformed_Acknowledge);
   for Route_Outcome use
     (Routed               => 0,
      Not_A_Line           => 1,
      Undefined_Route      => 2,
      Invalid_Owner        => 3,
      No_Process_Authority => 4,
      No_Line_Authority    => 5,
      Owners_Full          => 6,
      Mode_Conflict        => 7,
      No_Controller        => 8,
      Input_Absent         => 9,
      Level_Needs_IO_APIC  => 10,
      Not_Line_Owner       => 11,
      Malformed_Acknowledge => 12);

   function Outcome_Word (Outcome : Route_Outcome) return Unsigned_64 is
     (Unsigned_64 (Route_Outcome'Enum_Rep (Outcome)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word <= Outcome_Word (Route_Outcome'Last));

   function To_Outcome (Word : Unsigned_64) return Route_Outcome
   with Pre  => Is_Outcome_Word (Word),
        Post => Outcome_Word (To_Outcome'Result) = Word;

   ---------------------------------------------------------------------------
   --  The owner form's result word: Acknowledged_Flag with the line's
   --  delivery count (every interrupt the kernel took on it since boot,
   --  saturating), or a Route_Outcome word (a small number, never with the
   --  flag): No_Line_Authority when arg4 is not the caller's read handle
   --  naming exactly one line, Not_Line_Owner when the caller is not routed
   --  that line, Malformed_Acknowledge for the argument rules above.
   ---------------------------------------------------------------------------
   Acknowledged_Flag      : constant Unsigned_64 := 2 ** 63;
   Maximum_Delivery_Count : constant := 2 ** 63 - 1;
   subtype Delivery_Count is Unsigned_64 range 0 .. Maximum_Delivery_Count;

   function Next_Count (Count : Delivery_Count) return Delivery_Count is
     (if Count = Delivery_Count'Last then Count else Count + 1);

   function Acknowledged_Word (Count : Delivery_Count) return Unsigned_64 is
     (Acknowledged_Flag or Count);

   function Is_Acknowledged (Word : Unsigned_64) return Boolean is
     ((Word and Acknowledged_Flag) /= 0);

   function Count_Of (Word : Unsigned_64) return Delivery_Count is
     (Word and not Acknowledged_Flag)
   with Pre => Is_Acknowledged (Word);

end Interrupt_Line_ABI;
