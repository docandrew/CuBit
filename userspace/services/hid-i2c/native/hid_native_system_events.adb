pragma Ada_2022;
with CuBit.Channel_Bells;
with HID_System_Events;
package body HID_Native_System_Events is
   package CV renames CuBit.Control_Events;
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean) is
      Noted : Boolean;
   begin
      Stop := False; Handled := True;
      case HID_System_Events.Classify (Publisher, Item) is
         when HID_System_Events.Not_System => Handled := False;
         when HID_System_Events.Rejected | HID_System_Events.Ignored_Control => null;
         when HID_System_Events.Stop_After_Drain => Stop := True;
         when HID_System_Events.Channel_Wake =>
            CuBit.Channel_Bells.Note_Notice (Item.tag.label, Item.tag.length,
               [Item.words (0), Item.words (1), Item.words (2), Item.words (3)], Noted);
         when HID_System_Events.Grant_Notice =>
            Grant_Notice (CV.Decode (Item.tag.label, Item.tag.length,
               Item.words (0), Item.words (1), Item.words (2)));
      end case;
   end Handle;
end HID_Native_System_Events;
