------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform power object (KERN-004): its one operation, Enter_State,
--  as a Call on a CAP_PLATFORM_POWER handle. Shared by the kernel
--  (Platform_Power_Policy, Platform_Power) and the runtime
--  (CuBit.Handles.Power).
--
--  @description
--  A kernel object reached through the ordinary Call (ABI-002): the caller
--  passes the handle and a message exactly as for an endpoint call. The
--  kernel resolves the handle (type, generation, rights: Write) and, for
--  a platform power handle, runs the operation in ring 0 with no process
--  switch, then writes the reply into the caller's message.
--
--  Request (MessageTag.label = Operation'Enum_Rep, length = Request_Words):
--     words (State_Word)  Power_State'Enum_Rep
--     words (Type_A_Word) SLP_TYPa, 0 .. Sleep_Type'Last (S5 only, else 0)
--     words (Type_B_Word) SLP_TYPb, 0 .. Sleep_Type'Last (S5 only, else 0)
--     words (Unused_Word) 0
--  The sleep types are the values acpi.svc evaluated from \_S5; the
--  register targets are the kernel's own, from the FADT it parsed at boot.
--  The caller never names a register.
--
--  Reply (label = the request's, length = Reply_Words):
--     words (Outcome_Word) Refusal'Enum_Rep
--  S5 and Reset do not return when they take effect. Every reply is a
--  refusal; nothing has been written to a register unless the reply is
--  Did_Not_Take_Effect.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Platform_Power_ABI with Pure, SPARK_Mode is

   type Operation is (Enter_State);
   for Operation use (Enter_State => 1);

   --  The system states the object knows. Wire codes are fixed.
   type Power_State is (S3, S4, S5, Reset);
   for Power_State use (S3 => 3, S4 => 4, S5 => 5, Reset => 16);

   --  SLP_TYPx is a three-bit field (ACPI 6.5, 4.8.3.2.1 and 4.8.3.7).
   Sleep_Type_Bits : constant := 3;
   subtype Sleep_Type is Unsigned_64 range 0 .. 2 ** Sleep_Type_Bits - 1;

   --  Word positions in the request and the reply.
   State_Word    : constant := 0;
   Type_A_Word   : constant := 1;
   Type_B_Word   : constant := 2;
   Unused_Word   : constant := 3;
   Request_Words : constant := 3;
   Outcome_Word  : constant := 0;
   Reply_Words   : constant := 1;

   --  Why Enter_State came back. Codes start at 1 and are fixed.
   type Refusal is
     (Malformed,               --  wrong length, an unknown state, a nonzero unused word
      Unknown_Operation,       --  the label is no Operation
      Not_Permitted,           --  the handle lacks Write
      Sleep_Type_Out_Of_Range, --  SLP_TYPx wider than its field, or set for Reset
      Not_Yet,                 --  S3 and S4: not implemented (no resume path yet)
      No_Sleep_Registers,      --  the FADT names no usable PM1 or sleep control
      No_Reset_Register,       --  the FADT names no usable reset register
      Unsupported_Register,    --  a register outside system I/O space
      Did_Not_Take_Effect,     --  written; the machine is still running
      In_Progress);            --  another transition holds the CPUs
   for Refusal use
     (Malformed               => 1,
      Unknown_Operation       => 2,
      Not_Permitted           => 3,
      Sleep_Type_Out_Of_Range => 4,
      Not_Yet                 => 5,
      No_Sleep_Registers      => 6,
      No_Reset_Register       => 7,
      Unsupported_Register    => 8,
      Did_Not_Take_Effect     => 9,
      In_Progress             => 10);

   function Refusal_Word (Why : Refusal) return Unsigned_64 is
     (Unsigned_64 (Refusal'Enum_Rep (Why)));

   function Is_Refusal_Word (Word : Unsigned_64) return Boolean is
     (Word in Refusal_Word (Refusal'First) .. Refusal_Word (Refusal'Last));

end Platform_Power_ABI;
