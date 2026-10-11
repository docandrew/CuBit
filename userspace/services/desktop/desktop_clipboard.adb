with System;
with CuBit.Memory_Grants;
with CuBit.Desktop_Messages;
with CuBit.Desktop_Protocol;
with Clipboard_Gestures;
with Clipboard_Chords;
with Compositor_Clipboard_Protocol;
with Desktop_Logs;

package body Desktop_Clipboard with SPARK_Mode => Off is
   package CS renames Clipboard_Store;
   package G renames Clipboard_Gestures;
   package K renames Clipboard_Chords;
   use type K.Ticket;
   package P renames Compositor_Clipboard_Protocol;
   package DP renames CuBit.Desktop_Protocol;
   package MG renames CuBit.Memory_Grants;
   use type CS.Entry_Name, CS.Sensitivity, CS.Offer_Outcome,
     CS.Paste_Outcome, CS.Payload_Length, CS.Millisecond;
   use type P.Sensitivity;
   use type G.Gesture;
   use type DP.Status_Code;
   use type CuBit.Messages.Process_ID;
   use type System.Address;

   LF : constant Character := Character'Val (10);

   Tickets : G.State;
   --  Content passes through here between the client's window and the
   --  store, and is zeroed after every request: no secret lingers.
   Staging : CS.Payload_Bytes := [others => 0];

   subtype Window_Bytes is CS.Payload_Bytes;

   function Handles (Label : Unsigned_32) return Boolean is
     (Label = P.Offer_Label or else Label = P.Paste_Label);

   procedure Note_Key
     (Surface : Unsigned_64; Code : Unsigned_8; Modifiers : Unsigned_64;
      Now : Unsigned_64)
   is
      Kind : constant K.Ticket :=
        K.Ticket_Of (K.Key_Code (Code), K.Modifier_Bits (Modifiers));
   begin
      if Surface = 0 then
         return;
      end if;
      case Kind is
         when K.Copy_Ticket =>
            G.Issue (Tickets, G.Copy_Gesture, G.Surface_Name (Surface),
                     G.Millisecond (Now));
         when K.Paste_Ticket =>
            G.Issue (Tickets, G.Paste_Gesture, G.Surface_Name (Surface),
                     G.Millisecond (Now));
         when K.No_Ticket =>
            null;
      end case;
   end Note_Key;

   procedure Note_Activation (Surface : Unsigned_64; Now : Unsigned_64) is
   begin
      if Surface /= 0 then
         pragma Assert (K.Activation_Ticket = K.Copy_Ticket);
         G.Issue (Tickets, G.Copy_Gesture, G.Surface_Name (Surface),
                  G.Millisecond (Now));
      end if;
   end Note_Activation;

   procedure Revoke_Gestures is
   begin
      G.Revoke (Tickets);
   end Revoke_Gestures;

   function Kind_Text (Kind : CS.Sensitivity) return String is
     (if Kind = CS.Sensitive then "sensitive" else "ordinary");

   function To_Store (Kind : P.Sensitivity) return CS.Sensitivity is
     (if Kind = P.Sensitive then CS.Sensitive else CS.Ordinary);
   function To_Wire (Kind : CS.Sensitivity) return P.Sensitivity is
     (if Kind = CS.Sensitive then P.Sensitive else P.Ordinary);

   function Offer_Reply (Value : P.Offer_Receipt) return CuBit.Messages.Message is
     (CuBit.Desktop_Messages.From_Wire (P.Encode (Value)));
   function Paste_Reply (Value : P.Paste_Receipt) return CuBit.Messages.Message is
     (CuBit.Desktop_Messages.From_Wire (P.Encode (Value)));

   --  Failure replies with a status known only at run time.
   function Offer_Refused (Why : DP.Status_Code) return CuBit.Messages.Message is
      Value : P.Offer_Receipt (Why);
   begin
      return Offer_Reply (Value);
   end Offer_Refused;
   function Paste_Refused (Why : DP.Status_Code) return CuBit.Messages.Message is
      Value : P.Paste_Receipt (Why);
   begin
      return Paste_Reply (Value);
   end Paste_Refused;

   --  The request must name the focused surface and come from its owner,
   --  and the owner must hold a fresh chord of the right kind for it.
   function Authorized
     (From : CuBit.Messages.Process_ID; Surface : P.Surface_Name;
      Focus : Unsigned_64; Focus_Owner : CuBit.Messages.Process_ID;
      Kind : G.Gesture; Now : Unsigned_64; Why : out DP.Status_Code)
     return Boolean
   is
      Granted : Boolean;
   begin
      if From = CuBit.Messages.No_Process or else Focus = 0 or else
        Unsigned_64 (Surface) /= Focus or else From /= Focus_Owner
      then
         Why := DP.Denied;
         Desktop_Logs.Write ("desktop: clipboard " &
           (if Kind = G.Copy_Gesture then "offer" else "paste") &
           " denied reason=not-focused" & LF);
         return False;
      end if;
      G.Redeem (Tickets, Kind, G.Surface_Name (Surface), G.Millisecond (Now),
                Granted);
      if not Granted then
         Why := DP.Denied;
         Desktop_Logs.Write ("desktop: clipboard " &
           (if Kind = G.Copy_Gesture then "offer" else "paste") &
           " denied reason=no-gesture" & LF);
         return False;
      end if;
      Why := DP.Success;
      return True;
   end Authorized;

   function Handle_Offer
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Focus : Unsigned_64; Focus_Owner : CuBit.Messages.Process_ID;
      Now : Unsigned_64) return CuBit.Messages.Message
   is
      Decoded : constant P.Offer_Decoding :=
        P.Decode_Offer (CuBit.Desktop_Messages.To_Wire (Request));
      Why : DP.Status_Code;
      Mapped : System.Address;
      Acquired, Returned : Boolean;
      Name : CS.Entry_Name;
      Outcome : CS.Offer_Outcome;
      Shape : CS.Layout;
      Count : CS.Payload_Length;
   begin
      if not Decoded.Accepted then
         return Offer_Reply ((Status => DP.Invalid_Request));
      end if;
      if not Authorized (From, Decoded.Value.Surface, Focus, Focus_Owner,
                         G.Copy_Gesture, Now, Why)
      then
         return Offer_Refused (Why);
      end if;
      Shape := (Type_Length => CS.Type_Name_Length (Decoded.Value.Shape.Type_Length),
                Text_Length => CS.Payload_Length (Decoded.Value.Shape.Text_Length),
                CCL_Length  => CS.Payload_Length (Decoded.Value.Shape.CCL_Length));
      Count := Shape.Type_Length + Shape.Text_Length + Shape.CCL_Length;
      MG.Acquire (Decoded.Value.Window, From, 0, Unsigned_64 (Count),
                  MG.Read_Access, Mapped, Acquired);
      if not Acquired or else Mapped = System.Null_Address then
         Desktop_Logs.Write ("desktop: clipboard offer denied reason=window" & LF);
         return Offer_Reply ((Status => DP.Denied));
      end if;
      declare
         Window : Window_Bytes
           with Import, Volatile, Address => Mapped;
      begin
         --  Copy, then validate and use only the copy (data-plane rule 1):
         --  the client can still write its window.
         for I in 1 .. Count loop
            Staging (I) := Window (I);
         end loop;
      end;
      MG.Return_Acquisition (Decoded.Value.Window, Returned);
      if not Returned then
         Staging := [others => 0];
         Desktop_Logs.Write ("desktop: clipboard offer refused reason=return" & LF);
         return Offer_Reply ((Status => DP.Bad_State));
      end if;
      CS.Offer (History, CS.Source_Identity (CuBit.Messages.To_Word (From)),
                CS.Millisecond (Now), To_Store (Decoded.Value.Kind), Shape,
                Staging, Name, Outcome);
      Staging := [others => 0];
      case Outcome is
         when CS.Stored =>
            if Decoded.Value.Kind = P.Sensitive then
               --  Never the length: it is part of the secret.
               Desktop_Logs.Write ("desktop: clipboard offer entry=" &
                 Name'Image & " kind=sensitive" & LF);
            else
               Desktop_Logs.Write ("desktop: clipboard offer entry=" &
                 Name'Image & " kind=ordinary text=" &
                 Shape.Text_Length'Image & " typed=" &
                 (if Shape.CCL_Length > 0 then "yes" else "no") & LF);
            end if;
            return Offer_Reply ((DP.Success, P.Entry_Name (Name)));
         when CS.History_Full | CS.Names_Exhausted =>
            Desktop_Logs.Write ("desktop: clipboard offer refused reason=full" & LF);
            return Offer_Reply ((Status => DP.Resources_Exhausted));
      end case;
   end Handle_Offer;

   function Handle_Paste
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Focus : Unsigned_64; Focus_Owner : CuBit.Messages.Process_ID;
      Now : Unsigned_64) return CuBit.Messages.Message
   is
      Decoded : constant P.Paste_Decoding :=
        P.Decode_Paste (CuBit.Desktop_Messages.To_Wire (Request));
      Why : DP.Status_Code;
      Mapped : System.Address;
      Acquired, Returned : Boolean;
      Meta : CS.Metadata;
      Outcome : CS.Paste_Outcome;
      Count : CS.Payload_Length;
   begin
      if not Decoded.Accepted then
         return Paste_Reply ((Status => DP.Invalid_Request));
      end if;
      if not Authorized (From, Decoded.Value.Surface, Focus, Focus_Owner,
                         G.Paste_Gesture, Now, Why)
      then
         return Paste_Refused (Why);
      end if;
      --  Map the whole window first: a window too small for any entry is
      --  refused before an entry (maybe a secret) is consumed.
      MG.Acquire (Decoded.Value.Window, From, 0, P.Maximum_Payload,
                  MG.Write_Access, Mapped, Acquired);
      if not Acquired or else Mapped = System.Null_Address then
         Desktop_Logs.Write ("desktop: clipboard paste denied reason=window" & LF);
         return Paste_Reply ((Status => DP.Denied));
      end if;
      CS.Paste (History, CS.Millisecond (Now), To_Store (Decoded.Value.Target),
                [CS.Text_Form => True, CS.CCL_Form => Decoded.Value.Want_CCL],
                Meta, Staging, Outcome);
      if Outcome = CS.Delivered then
         Count := Meta.Shape.Type_Length + Meta.Shape.Text_Length +
           Meta.Shape.CCL_Length;
         declare
            Window : Window_Bytes
              with Import, Volatile, Address => Mapped;
         begin
            for I in 1 .. Count loop
               Window (I) := Staging (I);
            end loop;
         end;
      end if;
      Staging := [others => 0];
      MG.Return_Acquisition (Decoded.Value.Window, Returned);
      if not Returned then
         Desktop_Logs.Write ("desktop: clipboard paste refused reason=return" & LF);
         return Paste_Reply ((Status => DP.Bad_State));
      end if;
      case Outcome is
         when CS.Delivered =>
            Desktop_Logs.Write ("desktop: clipboard paste entry=" &
              Meta.Name'Image & " kind=" & Kind_Text (Meta.Kind) &
              (if Meta.Kind = CS.Sensitive or else
                  Decoded.Value.Target = P.Sensitive
               then " consumed" else "") &
              (if Meta.Shape.CCL_Length > 0 then " typed" else "") & LF);
            return Paste_Reply
              ((DP.Success, P.Entry_Name (Meta.Name),
                (Type_Length => P.Type_Name_Length (Meta.Shape.Type_Length),
                 Text_Length => P.Payload_Length (Meta.Shape.Text_Length),
                 CCL_Length  => P.Payload_Length (Meta.Shape.CCL_Length)),
                To_Wire (Meta.Kind)));
         when CS.Nothing_Available =>
            Desktop_Logs.Write ("desktop: clipboard paste empty" & LF);
            return Paste_Reply ((Status => DP.Bad_State));
         when CS.Format_Refused =>
            return Paste_Reply ((Status => DP.Unsupported));
      end case;
   end Handle_Paste;

   function Handle
     (From        : CuBit.Messages.Process_ID;
      Request     : CuBit.Messages.Message;
      Focus       : Unsigned_64;
      Focus_Owner : CuBit.Messages.Process_ID;
      Now         : Unsigned_64) return CuBit.Messages.Message is
   begin
      if Request.tag.label = P.Offer_Label then
         return Handle_Offer (From, Request, Focus, Focus_Owner, Now);
      else
         return Handle_Paste (From, Request, Focus, Focus_Owner, Now);
      end if;
   end Handle;

   procedure Paste_From_Manager
     (Slot : Clipboard_Store.Slot_Index; Focus : Unsigned_64;
      Now : Unsigned_64; Done : out Boolean) is
   begin
      CS.Promote (History, Slot, Done);
      if Done and then Focus /= 0 then
         G.Issue (Tickets, G.Paste_Gesture, G.Surface_Name (Focus),
                  G.Millisecond (Now));
      end if;
   end Paste_From_Manager;

   function Expire (Now : Unsigned_64) return Boolean is
      Before : constant CS.Millisecond :=
        CS.Next_Deadline (History, CS.Millisecond'Last);
   begin
      if CS.Millisecond (Now) < Before then
         return False;
      end if;
      CS.Expire (History, CS.Millisecond (Now));
      Desktop_Logs.Write ("desktop: clipboard secret expired" & LF);
      return True;
   end Expire;

   function Next_Deadline (Otherwise : Unsigned_64) return Unsigned_64 is
     (Unsigned_64 (CS.Next_Deadline (History, CS.Millisecond (Otherwise))));

   procedure Toggle_Pin (Slot : Clipboard_Store.Slot_Index) is
      Done : Boolean;
   begin
      CS.Set_Pinned (History, Slot, not CS.Info (History, Slot).Pinned, Done);
   end Toggle_Pin;

   procedure Delete (Slot : Clipboard_Store.Slot_Index) is
   begin
      CS.Delete (History, Slot);
   end Delete;

   procedure Clear is
   begin
      CS.Clear (History);
   end Clear;
end Desktop_Clipboard;
