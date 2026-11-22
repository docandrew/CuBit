------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The platform power object (KERN-004): Enter_State through Call on a
--  Platform_Power handle (Platform_Power_ABI).
--
--  @description
--  A kernel object: the Call runs in the kernel with no process switch.
--  The kernel writes only the registers it decoded from the FADT itself;
--  the caller supplies the state and, for S5, the sleep types it evaluated
--  from \_S5. S5 and Reset do not return when they take effect: before any
--  register write the kernel drains its console ring (every line queued
--  before the call reaches the console) and parks the other CPUs.
------------------------------------------------------------------------------
pragma Ada_2022;
with Platform_Power_ABI;

package CuBit.Handles.Power is

   package ABI renames Platform_Power_ABI;

   type Outcome (Answered : Boolean := False) is record
      case Answered is
         --  The kernel's typed refusal (S5 and Reset return only refused).
         when True  => Why : ABI.Refusal := ABI.Malformed;
         --  The handle names no platform power object, or the reply was
         --  unreadable (not a Platform_Power_ABI reply).
         when False => null;
      end case;
   end record;

   --  Enter State. Type_A and Type_B are SLP_TYPa and SLP_TYPb for S5 (0
   --  for Reset).
   function Enter_State
     (Object : Platform_Power; State : ABI.Power_State;
      Type_A, Type_B : ABI.Sleep_Type := 0) return Outcome;

end CuBit.Handles.Power;
