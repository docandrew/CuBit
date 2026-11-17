------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's side of the frame ring (Display_Frame_Ring, IPC-009): presents
--  out, answers in, each answer matched to the present it answers. No
--  system call here: the caller opens the channel, wakes Display when
--  Submit says so, and arms before it sleeps; the same code runs in the
--  hosted test (tests/frame-rings).
--
--  @description
--  Submit records each present under a fresh ring tag with the caller's
--  Token (its own name for the frame) and the frame number the request
--  carries. Reap hands on an answer only when its tag is outstanding and,
--  when it decodes as a frame result, it names that same frame: anything
--  else (an unknown or repeated tag, an answer for another frame, a
--  malformed entry) is dropped and counted (Strays), and the present stays
--  outstanding. When the channel ends (Display went away), Fail_Next gives
--  each outstanding present once, so every present ends in an answer or a
--  typed failure.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use type System.Address;
with CuBit.Desktop_Protocol;
with Display_Frame_Ring;

package Display_Frame_Client is

   package FR renames Display_Frame_Ring;
   package DP renames CuBit.Desktop_Protocol;

   type Client is limited private;

   procedure Attach (C : in out Client; Own_Region, Display_Region : System.Address)
   with Pre => Own_Region /= System.Null_Address and then Display_Region /= System.Null_Address;
   --  The channel ended: nothing more is read or written (what was
   --  outstanding stays, for Fail_Next).
   procedure Detach (C : in out Client);
   function Attached (C : Client) return Boolean;

   function Outstanding (C : Client) return Natural;

   type Submit_Result is (Queued, Full, Not_Attached);
   --  Queue Item. Wake_Display: Display sleeps and asked to be rung (once
   --  per arming); the caller rings it.
   procedure Submit
     (C : in out Client; Item : FR.Present; Token, Frame : Unsigned_64;
      Result : out Submit_Result; Wake_Display : out Boolean);

   type Reaped is record
      Token       : Unsigned_64 := 0;
      Result      : DP.Wire_Message;
      Taken_Us    : Unsigned_64 := 0;
      Answered_Us : Unsigned_64 := 0;
   end record;
   --  The next answer that matches an outstanding present (never blocks).
   --  Display never waits for room in the answer ring: a client has at most
   --  as many presents outstanding as the ring has answer slots.
   procedure Reap (C : in out Client; Item : out Reaped; Got : out Boolean);

   --  After Detach: one outstanding present's Token, each once.
   procedure Fail_Next (C : in out Client; Token : out Unsigned_64; Got : out Boolean);

   --  Before sleeping: declare this side asleep for answers. False: one
   --  arrived meanwhile (Reap; do not sleep). Disarm after waking.
   function Arm (C : in out Client) return Boolean;
   procedure Disarm (C : Client);

   --  Answers dropped: unknown tag, another frame's, or malformed.
   function Strays (C : Client) return Unsigned_64;

private
   type Entry_State is record
      Used  : Boolean := False;
      Tag   : FR.RC.Token := 0;
      Token : Unsigned_64 := 0;
      Frame : Unsigned_64 := 0;
   end record;
   type Entry_Table is array (1 .. FR.Frames_In_Flight) of Entry_State;
   type Client is limited record
      Ring     : FR.RC.Client;
      Attached : Boolean := False;
      Next_Tag : FR.RC.Token := 0;
      Table    : Entry_Table;
      Strays   : Unsigned_64 := 0;
   end record;
end Display_Frame_Client;
