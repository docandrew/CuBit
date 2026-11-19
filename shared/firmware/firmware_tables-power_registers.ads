pragma Ada_2022;
with Interfaces; use Interfaces;

--  The FADT's power registers (KERN-004): the PM1a/PM1b control blocks
--  (X_PM1x_CNT_BLK when present and nonzero, else the legacy port and
--  PM1_CNT_LEN), the hardware-reduced sleep control register, and the reset
--  register with its value (ACPI 6.5, 5.2.9). Only what the kernel's
--  platform power object writes; decoded once from the admitted table,
--  never from a caller.
--
--  A register is Present only when its whole access lies in one address
--  space the FADT names and its width is the one the object writes: PM1
--  control 16 bits, sleep control and reset 8 bits, bit offset 0. A
--  system-I/O register lies inside the 64 KiB port space. Anything else is
--  left out (not Present), so the object refuses rather than guesses.
package Firmware_Tables.Power_Registers with SPARK_Mode, Pure is

   type Register_Space is (System_IO, System_Memory, Other_Space);

   Port_Space_Last : constant := 16#FFFF#;
   PM1_Control_Bytes : constant := 2;
   Byte_Register_Bytes : constant := 1;
   subtype Register_Bytes is Natural range 0 .. PM1_Control_Bytes;

   type Register is record
      Present : Boolean        := False;
      Space   : Register_Space := Other_Space;
      Address : Address_Value  := 0;
      Bytes   : Register_Bytes := 0;
   end record;

   No_Register : constant Register := (others => <>);

   --  A present register's access is nonempty, nonzero and, in I/O space,
   --  within the port space.
   function Well_Formed (R : Register) return Boolean is
     (if R.Present then
        R.Address /= 0 and then R.Bytes >= Byte_Register_Bytes
        and then (if R.Space = System_IO then
                    R.Address <= Port_Space_Last - Address_Value (R.Bytes - 1))
      else R = No_Register);

   type Sleep_Model is (No_Sleep_Registers, PM1A_Only, PM1A_And_B, Hardware_Reduced);

   type Description is record
      Model         : Sleep_Model := No_Sleep_Registers;
      PM1A_Control  : Register    := No_Register;
      PM1B_Control  : Register    := No_Register;
      Sleep_Control : Register    := No_Register;
      Reset         : Register    := No_Register;
      Reset_Value   : Byte        := 0;
      --  The FADT's SMI command port (SMI_CMD; 0 when the firmware has
      --  none). Some firmware resets through a software SMI: Reset is then
      --  this port and Reset_Value the SMI command (the IdeaPad Slim 3i:
      --  0xFB to 0xB2).
      SMI_Command   : Address_Value := 0;
   end record;

   No_Description : constant Description := (others => <>);

   function Consistent (D : Description) return Boolean is
     (Well_Formed (D.PM1A_Control) and then Well_Formed (D.PM1B_Control)
      and then Well_Formed (D.Sleep_Control) and then Well_Formed (D.Reset)
      and then (if D.PM1A_Control.Present then D.PM1A_Control.Bytes = PM1_Control_Bytes)
      and then (if D.PM1B_Control.Present then D.PM1B_Control.Bytes = PM1_Control_Bytes)
      and then (if D.Sleep_Control.Present then D.Sleep_Control.Bytes = Byte_Register_Bytes)
      and then (if D.Reset.Present then D.Reset.Bytes = Byte_Register_Bytes)
      and then D.SMI_Command <= Port_Space_Last
      and then (case D.Model is
                  when No_Sleep_Registers =>
                    not D.PM1A_Control.Present and not D.PM1B_Control.Present
                    and not D.Sleep_Control.Present,
                  when PM1A_Only =>
                    D.PM1A_Control.Present and not D.PM1B_Control.Present
                    and not D.Sleep_Control.Present,
                  when PM1A_And_B =>
                    D.PM1A_Control.Present and D.PM1B_Control.Present
                    and not D.Sleep_Control.Present,
                  when Hardware_Reduced =>
                    D.Sleep_Control.Present and not D.PM1A_Control.Present
                    and not D.PM1B_Control.Present));

   --  Data is one whole FADT ("FACP"). An unadmitted table (signature,
   --  length, checksum) gives No_Description.
   function Decode (Data : Bytes) return Description
     with Post => Consistent (Decode'Result);

end Firmware_Tables.Power_Registers;
