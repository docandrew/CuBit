------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Hosted tests of the launch handle's decisions (CAP-012, DBG-001):
--  userspace/services/procmgr/handle_admission.ads and its wire encoding,
--  CuBit.Launch_Handles. Each decision is compared, over every case, with an
--  oracle written out here independently.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Debug_ABI;
with CuBit.Child_Exits;
with CuBit.Launch_Handles;
with Handle_Admission;
with Session_Launch;

procedure Main is
   package LH renames CuBit.Launch_Handles;
   package HA renames Handle_Admission;
   use type LH.Outcome, LH.Rights, LH.Operation, LH.Right, LH.Debug_Path;
   use type HA.Route, HA.Issuance_State, HA.Target_Class, HA.Debugging_Gate;
   use type Debug_ABI.Debug_Kind;

   Cases, Failures : Natural := 0;

   procedure Check (Ok : Boolean; What : String) is
   begin
      Cases := Cases + 1;
      if not Ok then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Set_Of (Word : Unsigned_64) return LH.Rights is (LH.Rights_Of (Word));

   --  The oracle for a handle operation (docs/authority-model.md 7.5 item 5).
   function Expected (F : HA.Facts; Op : LH.Operation; Length : Unsigned_8) return LH.Outcome is
      Words : constant Unsigned_8 :=
        (if Op in LH.Observe_Op | LH.Revoke_Op then 0 else 1);
      Needs : constant Boolean := Op not in LH.Narrow_Op | LH.Revoke_Op;
      Right : constant LH.Right :=
        (case Op is
           when LH.Observe_Op => LH.Observe, when LH.Control_Op => LH.Stop,
           when LH.Attach_Op => LH.Attach, when LH.Introduce_Op => LH.Introduce,
           when others => LH.Debug);
   begin
      if F.State = HA.Unknown then
         return LH.Not_A_Handle;
      elsif F.State = HA.Revoked then
         return LH.Revoked;
      elsif Length /= Words then
         return LH.Malformed;
      elsif F.State = HA.Child_Gone and then Op /= LH.Observe_Op and then Op /= LH.Revoke_Op then
         return LH.Child_Ended;
      elsif Needs and then not F.Rights (Right) then
         return LH.Right_Missing;
      else
         return LH.Done;
      end if;
   end Expected;

   Tags : constant array (Positive range <>) of Unsigned_64 :=
     [0, 1, LH.Tag_Of (1), LH.Tag_Of (LH.Serial'Last), LH.Handle_Tag_Prefix,
      LH.Session_Debug_Tag, LH.Notice_Tag, Session_Launch.Session_Launch_Tag,
      Session_Launch.Remote_Launch_Tag, LH.Handle_Tag_Prefix or LH.Serial_Limit,
      Unsigned_64'Last];
   Labels : constant array (Positive range <>) of Unsigned_32 :=
     [0, 16#0106#, 16#0110#, 16#0115#, 16#0116#, 16#0117#, 16#0118#, 16#0119#,
      16#011A#, 16#011B#, 16#011C#, 16#011D#, 16#011E#, 16#011F#, 16#0120#, 16#F000#];
   type Word_List is array (Positive range <>) of Unsigned_64;
   type Label_List is array (Positive range <>) of Unsigned_32;
   Serials : constant Word_List := [1, 2, 16#1234_5678#, LH.Serial'Last];
   Not_Handles : constant Word_List :=
     [0, LH.Handle_Tag_Prefix, LH.Session_Debug_Tag, LH.Notice_Tag,
      Session_Launch.Session_Launch_Tag, Session_Launch.Remote_Launch_Tag,
      LH.Handle_Tag_Prefix or LH.Serial_Limit];
   Other_Labels : constant Label_List :=
     [LH.Session_Debug_Operation, LH.Debug_Records_Operation, LH.Exit_Notice_Label,
      16#0106#, 16#0110#];
begin
   --  Wire encoding.
   for W in Unsigned_64 range 0 .. LH.Rights_Word_Limit - 1 loop
      Check (LH.Word_Of (LH.Rights_Of (W)) = W, "rights word round trip");
   end loop;
   Check (not LH.Valid_Rights_Word (LH.Rights_Word_Limit), "rights word past its bits");
   for S of Serials loop
      Check (LH.Is_Handle_Tag (LH.Tag_Of (S)) and then LH.Serial_Of (LH.Tag_Of (S)) = S,
             "handle tag round trip");
   end loop;
   for T of Not_Handles loop
      Check (not LH.Is_Handle_Tag (T), "a non-handle tag is not a handle");
   end loop;
   for Op in LH.Operation loop
      Check (LH.Is_Operation_Label (LH.Label_Of (Op))
             and then LH.To_Operation (LH.Label_Of (Op)) = Op, "operation label round trip");
   end loop;
   for L of Other_Labels loop
      Check (not LH.Is_Operation_Label (L), "not a handle operation");
   end loop;
   for O in LH.Outcome loop
      Check (LH.Is_Outcome_Word (LH.Outcome_Word (O))
             and then LH.To_Outcome (LH.Outcome_Word (O)) = O, "outcome round trip");
   end loop;
   Check (not LH.Is_Outcome_Word (0), "0 is not an outcome");

   --  The exit notice is an exit only through procmgr's badge.
   Check (CuBit.Child_Exits.Is_Exit (LH.Exit_Notice_Label, 3, LH.Notice_Tag, 5, 1, 0),
          "procmgr's notice is an exit");
   Check (not CuBit.Child_Exits.Is_Exit (LH.Exit_Notice_Label, 3, 0, 5, 1, 0),
          "an untagged notice is not an exit");
   Check (not CuBit.Child_Exits.Is_Exit (LH.Exit_Notice_Label, 3, LH.Tag_Of (5), 5, 1, 0),
          "a notice through a handle's tag is not an exit");
   Check (CuBit.Child_Exits.Is_Exit (CuBit.Child_Exits.Event_Label, 3, 0, 5, 2, 0),
          "the kernel's event is an exit");
   Check (not CuBit.Child_Exits.Is_Exit (LH.Exit_Notice_Label, 3, LH.Notice_Tag, 5, 2, 7),
          "a Stopped notice with a code is malformed");

   --  Routing, every tag against every label.
   for T of Tags loop
      for L of Labels loop
         declare
            R : constant HA.Route := HA.Route_Of (T, L);
            Want : constant HA.Route :=
              (if LH.Is_Handle_Tag (T) then
                 (if L in 16#0116# .. 16#011C# then HA.Handle_Request else HA.Refused)
               elsif L in 16#0116# .. 16#011C# then HA.Refused
               elsif T = LH.Session_Debug_Tag then
                 (if L = 16#011D# then HA.Session_Debug_Request else HA.Refused)
               elsif L = 16#011D# then HA.Refused
               else HA.Ordinary);
         begin
            Check (R = Want, "route of tag" & T'Image & " label" & L'Image);
         end;
      end loop;
   end loop;

   --  Every handle operation, state, rights set and length.
   for State in HA.Issuance_State loop
      for W in Unsigned_64 range 0 .. LH.Rights_Word_Limit - 1 loop
         for Op in LH.Operation loop
            for Length in Unsigned_8 range 0 .. 4 loop
               declare
                  F : constant HA.Facts := (State => State, Rights => Set_Of (W));
               begin
                  Check (HA.Decide (F, Op, Length) = Expected (F, Op, Length),
                         "decide " & State'Image & W'Image & " " & Op'Image & Length'Image);
               end;
            end loop;
         end loop;
      end loop;
   end loop;

   --  Issuance and narrowing.
   Check (HA.Issued (Session_Launch.Session_Badge) = LH.All_Rights, "a session launch issues all");
   Check (HA.Issued (Session_Launch.No_Badge) = LH.No_Rights, "no badge, no rights");
   declare
      Remote : constant LH.Rights := HA.Issued (Session_Launch.Remote_Badge);
   begin
      Check (Remote (LH.Observe) and Remote (LH.Stop) and Remote (LH.Attach)
             and not Remote (LH.Introduce) and not Remote (LH.Debug), "a remote launch's rights");
   end;
   for H in Unsigned_64 range 0 .. LH.Rights_Word_Limit - 1 loop
      for A in Unsigned_64 range 0 .. LH.Rights_Word_Limit - 1 loop
         Check (LH.Word_Of (HA.Narrowed (Set_Of (H), Set_Of (A))) = (H and A), "narrowing");
      end loop;
   end loop;

   --  The image's debugging gate: only Development grants the session's
   --  debug authority.
   for Gate in HA.Debugging_Gate loop
      Check (HA.Debug_Authority_Granted (Gate) = (Gate = HA.Development), "debug authority by gate");
   end loop;

   --  Debug minting: every gate, path, class, kind and Control.
   for Gate in HA.Debugging_Gate loop
   for Path in LH.Debug_Path loop
      for Class in HA.Target_Class loop
         for Kind in Debug_ABI.Debug_Kind loop
            for Held in Boolean loop
               declare
                  F : constant HA.Debug_Facts :=
                    (Path => Path, Class => Class, Held_Control => Held, Kind => Kind);
                  Never : constant Boolean :=
                    Class in HA.Secrets_Service | HA.Trusted_Path | HA.Process_Manager
                           | HA.Device_Manager;
                  Allowed : constant Boolean :=
                    Kind = Debug_ABI.Observe
                    and then (if Path = LH.Launch_Handle_Path then Class = HA.Session_Member
                              else Class in HA.Session_Member | HA.System_Service);
                  Want : constant LH.Outcome :=
                    (if Gate = HA.Disabled then LH.Debugging_Disabled
                     elsif Gate = HA.Production then LH.Approval_Needed
                     elsif Never then LH.Never_Debuggable
                     elsif not Held then LH.Not_Held
                     elsif not Allowed then LH.Kind_Withheld
                     else LH.Done);
               begin
                  Check (HA.Debug_Decision (F, HA.User_Session_Debug, Gate) = Want,
                         "debug " & Gate'Image & " " & Path'Image & " " & Class'Image & " " & Kind'Image
                         & Held'Image);
                  Check (HA.Debug_Decision (F, (others => HA.No_Kinds), Gate) /= LH.Done,
                         "an empty envelope mints nothing");
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   end loop;

   Put_Line ("launch-handles:" & Cases'Image & " cases," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error with "launch-handles: failures";
   end if;
end Main;
