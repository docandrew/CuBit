pragma Ada_2022;
with CuBit.Capability_Grants;
with CuBit.Metric_Raw_Validation;
package body CuBit.Metric_Raw_Observer with SPARK_Mode => Off is
   use CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package C renames CuBit.Capability_Grants;
   function Disabled (Item : Observer) return Boolean is (Item.Off);
   function Incarnation (Item : Observer) return Process_ID is (Item.Identity);
   procedure Query (Item : in out Observer; Cursor : Unsigned_64;
      Page : out P.Raw_Page; Written : out P.Raw_Row_Count;
      Next, Gap, Dropped : out Unsigned_64; Result : out P.Status) is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Page := (others => (others => 0));
      Written := 0;
      Next := Cursor;
      Gap := 0;
      Dropped := 0;
      Result := P.Unavailable;
      if Item.Off then
         return;
      end if;
      if Cursor = 0 then
         Result := P.Invalid_Request;
         return;
      end if;
      if not CuBit.Process_IDs.Is_Process (Item.Identity) then
         Item.Identity := C.Incarnation (C.Capture (Item.Slot));
      end if;
      if not C.Endpoint_Matches (Item.Slot, Item.Identity) then
         Item.Off := True;
         return;
      end if;
      if not Item.Has_Grant then
         G.Create_Via_Capability (Item.Slot, Item.Page'Address, 1, True,
           Item.Grant, Item.Has_Grant);
         if not Item.Has_Grant then
            return;
         end if;
      end if;
      Msg.tag := (label => P.Operation'Enum_Rep (P.Query_Raw),
                  length => P.Message_Words, flags => 0, reserved => 0);
      Msg.words := [Cursor, Item.Grant.slot, Item.Grant.generation, 4096];
      Tag := capCall (Item.Slot, Msg, Wait_Forever);
      if not C.Endpoint_Matches (Item.Slot, Item.Identity) or else
        Tag.label /= P.Status'Enum_Rep (P.OK) or else
        Tag.length /= P.Message_Words or else Tag.flags /= 0 or else
        Tag.reserved /= 0 or else Msg.tag /= Tag
      then
         Item.Off := True;
         return;
      end if;
      if not CuBit.Metric_Raw_Validation.Valid (P.Raw_Page (Item.Page),
        Cursor, Msg.words (0), Msg.words (1), Msg.words (2))
      then
         Item.Off := True;
         Result := P.Invalid_Request;
         return;
      end if;
      Page := P.Raw_Page (Item.Page);
      Written := P.Raw_Row_Count (Msg.words (0));
      Next := Msg.words (1);
      Gap := Msg.words (2);
      Dropped := Msg.words (3);
      Result := P.OK;
   end Query;
   procedure Disconnect (Item : in out Observer; Done : out Boolean) is
      Requested : Boolean;
   begin
      Done := False;
      Item.Off := True;
      if Item.Has_Grant then
         G.Revoke (Item.Grant, Requested);
         if G.Retirement_Confirmed (Item.Grant) then
            Item.Has_Grant := False;
         elsif not Requested then
            return;
         end if;
      end if;
      Done := not Item.Has_Grant;
   end Disconnect;
end CuBit.Metric_Raw_Observer;
