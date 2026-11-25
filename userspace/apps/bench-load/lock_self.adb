--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-lock-self (IDIOT-001, tests/headless/init-mailbox-self.ccl): an
--  ordinary program names itself as the target of the system calls that
--  lock the target's mailbox and check the caller's authority: RESUME,
--  INSTALL_LAUNCH_ARGUMENTS and ENABLE_IRQ. It holds no authority for any
--  of them, so each must be refused. A kernel that looked the caller's
--  authority up under the target's lock took its own mailbox lock twice
--  and halted ("Recursive CPU spinlock acquisition").
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Control;
with CuBit.Handles.Lines;
procedure Lock_Self is
   package H renames CuBit.Handles;
   package LA renames CuBit.Handles.Lines.LA;
   use type LA.Route_Outcome;
   --  This program holds no process or line authority: it names none.
   Self : constant CuBit.Messages.Process_ID := Own_Process;
   Arguments : aliased String (1 .. 16) := [others => 'x'];
   Passed : Boolean := True;

   procedure Expect (What : String; Refusal : Boolean) is
   begin
      debugPrint ("MAILBOX-SELF: " & What & (if Refusal then " refused PASS" else " FAIL") &
                  ASCII.LF);
      Passed := Passed and Refusal;
   end Expect;
begin
   debugPrint ("MAILBOX-SELF: START" & ASCII.LF);
   Expect ("RESUME of itself",
           not H.Control.Resume (Self, H.No_Process, Max_Frames => 0, CPU_Quota => 0));
   Expect ("INSTALL_LAUNCH_ARGUMENTS on itself",
           not H.Control.Install_Launch_Block
             (Self, H.No_Process, Arguments'Address, Unsigned_64 (Arguments'Length)));
   Expect ("ENABLE_IRQ to itself",
           H.Lines.Route (Vector => LA.First_Vector, Owner => Self, CPU => 0,
                          Trigger => LA.Edge, Polarity => LA.Active_High,
                          Authority => H.No_Process, Lines => H.No_Interrupt)
             = LA.No_Process_Authority);
   debugPrint ("MAILBOX-SELF: done " & (if Passed then "PASS" else "FAIL") & ASCII.LF);
end Lock_Self;
