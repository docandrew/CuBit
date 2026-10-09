with Control; with CuBit.Memory_Grants;
with CuBit.Metric_Records; with CuBit.Metric_Protocol;
package body CuBit.Messages is
 function capCall (Slot : CapabilitySlot; Msg : in out Message; Deadline : Unsigned_64) return MessageTag is
  package P renames CuBit.Metric_Protocol;
  package R renames CuBit.Metric_Records;
  Cursor : constant Unsigned_64 := Msg.words (0);
  Page : P.Raw_Page with Import, Address => CuBit.Memory_Grants.Captured;
  Encoded : constant R.Slot_Words := R.Encode ((R.Span, 1, 20, 30, 991));
 begin
  Control.Calls := Control.Calls + 1;
  if Msg.tag.label /= P.Operation'Enum_Rep (P.Query_Raw) or Msg.words (3) /= 4096 then raise Program_Error; end if;
  Page := (others => (others => 0));
  Page (0) (0) := Cursor; Page (0) (1) := 7;
  Page (0) (2) := P.Publisher_Tag (1); Page (0) (3) := 1;
  for I in R.Slot_Word_Index loop Page (0) (8 + I) := Encoded (I); end loop;
  Msg.tag := (P.Status'Enum_Rep (P.OK), 4, 0, 0);
  Msg.words := [1, Cursor + 1, 0, 0];
  case Control.Current is
   when Control.Success => null;
   when Control.Bad_Page => Page (0) (2) := P.Observer_Tag (1);
   when Control.Failed_Call => return (0, 0, 0, 0);
   when Control.Replaced_Endpoint => Control.Identity := Control.Identity + 2 ** 32;
   when Control.Denied => Msg.tag.label := P.Status'Enum_Rep (P.Denied);
   when Control.Bad_Length => Msg.tag.length := 3;
   when Control.Bad_Flags => Msg.tag.flags := 1;
   when Control.Bad_Reserved => Msg.tag.reserved := 1;
   when Control.Mismatched_Tag => Msg.tag.label := P.Status'Enum_Rep (P.Denied); return (P.Status'Enum_Rep (P.OK), 4, 0, 0);
   when Control.Too_Many_Rows => Msg.words (0) := 33;
   when Control.Reversed_Cursor => Msg.words (1) := 0;
   when Control.Gap_Overflow => Msg.words (2) := Unsigned_64'Last;
  end case;
  return Msg.tag;
 end capCall;
end CuBit.Messages;
