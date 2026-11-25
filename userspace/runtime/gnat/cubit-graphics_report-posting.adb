------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Graphics_Report.Posting is

   --  One call of Label, acknowledged by devmgr within Post_Deadline_Ms.
   function Sent (Target : CuBit.Handles.Endpoint; Label : Unsigned_32; Length : Unsigned_8;
                  Values : MessageWords) return Boolean;

   function Sent (Target : CuBit.Handles.Endpoint; Label : Unsigned_32; Length : Unsigned_8;
                  Values : MessageWords) return Boolean
   is
      Msg : Message :=
        (tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0, words => Values);
      Tag : constant MessageTag := capCall (Target, Msg, Deadline_After (Post_Deadline_Ms));
   begin
      return Ending_Of (Tag) = Call_Answered and then Tag.label = Label;
   end Sent;

   procedure Post (Target : CuBit.Handles.Endpoint; Item : Report_Entry; Delivered : out Boolean) is
      Wire : constant Entry_Wire := Encode (Item);
   begin
      Delivered := CuBit.Handles.Valid (Target)
        and then Sent (Target, Post_Label, Post_Words, [Wire (0), Wire (1), Wire (2), Wire (3)])
        and then Sent (Target, Post_End_Label, Post_Words, [Wire (4), Wire (5), Wire (6), Wire (7)]);
   end Post;

   procedure Gone (Target : CuBit.Handles.Endpoint; Program : Subject_Id; Delivered : out Boolean) is
   begin
      Delivered := CuBit.Handles.Valid (Target)
        and then Sent (Target, Gone_Label, Gone_Words, [Program, 0, 0, 0]);
   end Gone;

end CuBit.Graphics_Report.Posting;
