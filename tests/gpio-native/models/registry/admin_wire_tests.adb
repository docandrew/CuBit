pragma Ada_2022;
with Ada.Text_IO;
with CuBit.Process_IDs;
with GPIO_Admin_Wire;
with GPIO_Control_Wire;
with GPIO_Channel_Badges;
procedure Admin_Wire_Tests is
   package A renames GPIO_Admin_Wire;
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   package B renames GPIO_Channel_Badges;
   use type A.Decode_Status, A.Request_Kind, W.Completion_Status, W.Word, P.Process_ID;
   Checks : Natural := 0;
   Badge : constant W.Word := B.Canonical ((160, B.Level_Triggered, B.Active_Low));
   Owner : constant P.Process_ID := P.From_Word (16#F123_4567_0000_1234#);
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
   R : A.Decoding;
   Reply : A.Reply_Decoding;
   Labels : constant array (Positive range 1 .. 2) of W.Operation :=
      [A.OP_GPIO_ADMIT_SINK, A.OP_GPIO_RETIRE_SINK];
begin
   Check (A.Encode ((Kind => A.Admit_Sink))'Length = 0);
   R := A.Decode (A.OP_GPIO_ADMIT_SINK, 1, [], True); Check (R.Status = A.Decoded and then R.Value.Kind = A.Admit_Sink);
   R := A.Decode (A.OP_GPIO_ADMIT_SINK, 1, [], False); Check (R.Status = A.Missing_Offer);
   R := A.Decode (A.OP_GPIO_ADMIT_SINK, 1, [0], True); Check (R.Status = A.Wrong_Length);
   R := A.Decode (A.OP_GPIO_ADMIT_SINK, 2, [], True); Check (R.Status = A.Wrong_Protocol);
   R := A.Decode (W.OP_GPIO_EVENT, 1, [], True); Check (R.Status = A.Wrong_Operation);
   declare Words : constant W.Message_Words := A.Encode ((A.Retire_Sink, Badge, Owner)); begin
      Check (Words'Length = 2 and then Words (Words'First) = Badge and then Words (Words'First + 1) = P.To_Word (Owner));
      R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, Words, False);
      Check (R.Status = A.Decoded and then R.Value.Kind = A.Retire_Sink and then R.Value.Owner = Owner and then R.Value.Badge = Badge);
      R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, Words, True); Check (R.Status = A.Unexpected_Offer);
   end;
   R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, [Badge, 0], False); Check (R.Status = A.Invalid_Owner);
   R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, [0, P.To_Word (Owner)], False); Check (R.Status = A.Invalid_Badge);
   R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, [Badge], False); Check (R.Status = A.Wrong_Length);
   declare Shifted : constant W.Message_Words (Natural'Last - 1 .. Natural'Last) := [Badge, P.To_Word (Owner)]; begin
      R := A.Decode (A.OP_GPIO_RETIRE_SINK, 1, Shifted, False); Check (R.Status = A.Decoded and then R.Value.Owner = Owner);
   end;
   for Status in W.Completion_Status loop
      for Label of Labels loop
         Reply := A.Decode_Reply (Label, 1, A.Encode_Reply (Status));
         Check (Reply.Status = A.Decoded and then Reply.Value = Status);
      end loop;
   end loop;
   Reply := A.Decode_Reply (A.OP_GPIO_ADMIT_SINK, 1, [W.Word'Last]); Check (Reply.Status = A.Unknown_Status);
   Reply := A.Decode_Reply (A.OP_GPIO_RETIRE_SINK, 2, [0]); Check (Reply.Status = A.Wrong_Protocol);
   Reply := A.Decode_Reply (W.OP_GPIO_ARM, 1, [0]); Check (Reply.Status = A.Wrong_Operation);
   Reply := A.Decode_Reply (A.OP_GPIO_RETIRE_SINK, 1, []); Check (Reply.Status = A.Wrong_Length);
   Reply := A.Decode_Reply (A.OP_GPIO_RETIRE_SINK, 1, [0, 0]); Check (Reply.Status = A.Wrong_Length);
   Ada.Text_IO.Put_Line ("GPIO admin wire checks" & Checks'Image);
end Admin_Wire_Tests;
