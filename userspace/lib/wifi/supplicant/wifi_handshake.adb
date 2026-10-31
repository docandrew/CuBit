------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with SPARKNaCl; use SPARKNaCl;
with SPARKNaCl.AES;
with WiFi_Crypto.Legacy_WPA2;
with WiFi_Crypto.AES_Key_Wrap;
with Wifi_Key_Data;
with Wifi_RSN;

package body Wifi_Handshake with SPARK_Mode is
   --  Sanitize wipes secrets nothing reads afterwards, by design.
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "secrets are wiped");
   pragma Warnings (GNATprove, Off, "* is set by ""Sanitize"" but not used after the call",
                    Reason => "secrets are wiped");
   package W2 renames WiFi_Crypto.Legacy_WPA2;
   package KW renames WiFi_Crypto.AES_Key_Wrap.AES128;
   package KD renames Wifi_Key_Data;

   --  802.11-2020 12.7.1.3: the label of the pairwise key expansion.
   Label : constant String := "Pairwise key expansion";
   PTK_Bytes : constant := 3 * Key_Bytes;          --  KCK, KEK, TK (CCMP-128)
   Address_Bytes : constant := 6;
   Data_Bytes : constant := 2 * Address_Bytes + 2 * E.Nonce_Bytes;
   Semiblock : constant := 8;
   Minimum_Wrapped : constant := 3 * Semiblock;    --  two semiblocks of key data + IV

   function Less (A, B : Address) return Boolean is
     (for some I in Address'Range =>
        A (I) < B (I) and then (for all J in 0 .. I - 1 => A (J) = B (J)));

   function Less (A, B : E.Nonce) return Boolean is
     (for some I in E.Nonce'Range =>
        A (I) < B (I) and then (for all J in 0 .. I - 1 => A (J) = B (J)));

   procedure Derive_PTK (S : in out Session) is
      K : Byte_Seq (0 .. PMK_Bytes - 1);
      A : Byte_Seq (0 .. Label'Length - 1);
      B : Byte_Seq (0 .. Data_Bytes - 1) := (others => 0);
      PTK : Byte_Seq (0 .. PTK_Bytes - 1);
      Low_A  : constant Address := (if Less (S.AA, S.SPA) then S.AA else S.SPA);
      High_A : constant Address := (if Less (S.AA, S.SPA) then S.SPA else S.AA);
      Low_N  : constant E.Nonce := (if Less (S.A_Nonce, S.S_Nonce) then S.A_Nonce else S.S_Nonce);
      High_N : constant E.Nonce := (if Less (S.A_Nonce, S.S_Nonce) then S.S_Nonce else S.A_Nonce);
   begin
      for I in K'Range loop
         K (I) := Byte (S.PMK (Natural (I)));
      end loop;
      for I in A'Range loop
         A (I) := Byte (Character'Pos (Label (Label'First + Natural (I))));
      end loop;
      for I in 0 .. Address_Bytes - 1 loop
         B (N32 (I)) := Byte (Low_A (I));
         B (N32 (Address_Bytes + I)) := Byte (High_A (I));
      end loop;
      for I in E.Nonce'Range loop
         B (N32 (2 * Address_Bytes + I)) := Byte (Low_N (I));
         B (N32 (2 * Address_Bytes + E.Nonce_Bytes + I)) := Byte (High_N (I));
      end loop;
      W2.PRF (PTK, K, A, B);
      for I in 0 .. Key_Bytes - 1 loop
         S.KCK (I) := Unsigned_8 (PTK (N32 (I)));
         S.KEK (I) := Unsigned_8 (PTK (N32 (Key_Bytes + I)));
         S.TK (I) := Unsigned_8 (PTK (N32 (2 * Key_Bytes + I)));
      end loop;
      Sanitize (PTK);
      Sanitize (K);
   end Derive_PTK;

   --  HMAC-SHA1-128 under the KCK over Frame (0 .. Length - 1), MIC zero.
   function MIC_Of (KCK : Key_16; Frame : E.Octets; Length : Natural) return E.MIC_Value
   with Pre => Frame'First = 0 and then Length <= Frame'Length and then Length >= E.Key_Data_At
               and then Length <= E.Maximum_Frame
   is
      K : Byte_Seq (0 .. Key_Bytes - 1);
      M : Byte_Seq (0 .. N32 (Length) - 1);
      D : W2.HMAC_SHA1.Digest;
   begin
      for I in K'Range loop
         K (I) := Byte (KCK (Natural (I)));
      end loop;
      for I in M'Range loop
         M (I) := (if Natural (I) in E.MIC_At .. E.MIC_At + E.MIC_Bytes - 1 then 0
                   else Byte (Frame (Natural (I))));
      end loop;
      W2.HMAC_SHA1.Compute (D, M, K);
      return [for I in E.MIC_Value'Range => Unsigned_8 (D (N32 (I)))];
   end MIC_Of;

   function Same_MIC (A, B : E.MIC_Value) return Boolean is
      X, Y : Bytes_16;
   begin
      for I in X'Range loop
         X (I) := Byte (A (Natural (I)));
         Y (I) := Byte (B (Natural (I)));
      end loop;
      return Equal (X, Y);
   end Same_MIC;

   procedure Seal (S : Session; Reply : in out Reply_Frame; Length : Natural)
   with Pre => Length in E.Key_Data_At .. Reply_Bytes
   is
   begin
      E.Put_MIC (Reply, MIC_Of (S.KCK, Reply, Length));
   end Seal;

   Plain_Maximum : constant := E.Maximum_Frame;
   type Plain_Buffer is array (0 .. Plain_Maximum - 1) of Unsigned_8;

   --  Unwrap the key data with the KEK and decode it.
   procedure Open_Key_Data
     (S : Session; Frame : E.Octets; K : E.Key_Frame; Plain : out Plain_Buffer;
      Contents : out KD.Contents; OK : out Boolean)
   with Pre  => Frame'First = 0 and then E.Valid (K) and then K.Frame_Length <= Frame'Length,
        Post => (if OK then Contents.RSN_Element.First + Contents.RSN_Element.Length <= Plain_Maximum)
   is
      Wrapped_Bytes : constant Natural := K.Data_Length;
      Status : KD.Status;
      Valid : Boolean;
      use type KD.Status;
   begin
      Plain := [others => 0];
      Contents := (others => <>);
      OK := False;
      if Wrapped_Bytes < Minimum_Wrapped or else Wrapped_Bytes mod Semiblock /= 0 then
         return;
      end if;
      declare
         Key_Seq : Bytes_16;
         Wrapped : Byte_Seq (0 .. N32 (Wrapped_Bytes) - 1);
         Clear : Byte_Seq (0 .. N32 (Wrapped_Bytes - Semiblock) - 1);
      begin
         for I in Key_Seq'Range loop
            Key_Seq (I) := Byte (S.KEK (Natural (I)));
         end loop;
         for I in Wrapped'Range loop
            Wrapped (I) := Byte (Frame (K.Data_First + Natural (I)));
         end loop;
         declare
            RK : constant SPARKNaCl.AES.AES128_Round_Keys :=
              SPARKNaCl.AES.Key_Expansion (SPARKNaCl.AES.Construct (Key_Seq));
         begin
            KW.Unwrap (Clear, Valid, RK, Wrapped);
         end;
         Sanitize (Key_Seq);
         if not Valid then
            return;
         end if;
         for I in Clear'Range loop
            Plain (Natural (I)) := Unsigned_8 (Clear (I));
         end loop;
         Sanitize (Clear);
         declare
            Data : constant KD.Octets (0 .. Clear'Length - 1) :=
              [for I in 0 .. Clear'Length - 1 => Plain (I)];
         begin
            KD.Decode (Data, Contents, Status);
         end;
         OK := Status = KD.Decoded and then Contents.Has_GTK;
      end;
   end Open_Key_Data;

   procedure Start (S : out Session; PMK : PMK_Value; AA, SPA : Address;
                    Beacon_RSN : RSN_Element; Beacon_RSN_Bytes : RSN_Length;
                    S_Nonce : E.Nonce) is
   begin
      S := (State => Waiting_Message_1, PMK => PMK, AA => AA, SPA => SPA,
            Beacon => Beacon_RSN, Beacon_Bytes => Beacon_RSN_Bytes, S_Nonce => S_Nonce,
            others => <>);
   end Start;

   procedure Receive
     (S : in out Session; Frame : E.Octets; Reply : out Reply_Frame;
      Reply_Length : out Natural; Install : out Install_Request; Outcome : out Result)
   is
      K : E.Key_Frame;
      Status : E.Status;
      use type E.Status;
      use type E.Message_Kind;
      use type E.Nonce;
   begin
      Reply := [others => 0];
      Reply_Length := 0;
      Install := (others => <>);
      E.Decode (Frame, K, Status);
      if Status /= E.Decoded then
         Outcome := Malformed;
         return;
      elsif S.Seen and then K.Replay <= S.Replay then
         Outcome := Replay_Refused;
         return;
      end if;

      case K.Kind is
         when E.Pairwise_1 =>
            if S.State not in Waiting_Message_1 | Waiting_Message_3 then
               Outcome := Ignored;
               return;
            end if;
            S.A_Nonce := K.Key_Nonce;
            Derive_PTK (S);
            declare
               Own : constant E.Octets (0 .. Wifi_RSN.Own_Element_Bytes - 1) :=
                 [for I in 0 .. Wifi_RSN.Own_Element_Bytes - 1 => Wifi_RSN.Own_Element (I)];
            begin
               E.Build_Message_2 (K.Replay, S.S_Nonce, Own, Reply, Reply_Length);
            end;
            Seal (S, Reply, Reply_Length);
            S.State := Waiting_Message_3;
            S.Replay := K.Replay;
            S.Seen := True;
            Outcome := Replied;

         when E.Pairwise_3 | E.Group_1 =>
            if (K.Kind = E.Pairwise_3 and then S.State not in Waiting_Message_3 | Established)
              or else (K.Kind = E.Group_1 and then S.State /= Established)
            then
               Outcome := Ignored;
               return;
            elsif not Same_MIC (MIC_Of (S.KCK, Frame, K.Frame_Length), K.MIC) then
               Outcome := MIC_Failed;
               return;
            elsif K.Kind = E.Pairwise_3 and then K.Key_Nonce /= S.A_Nonce then
               Outcome := Nonce_Changed;
               return;
            end if;
            declare
               Plain : Plain_Buffer;
               Contents : KD.Contents;
               OK : Boolean;
            begin
               Open_Key_Data (S, Frame, K, Plain, Contents, OK);
               if not OK then
                  Outcome := Key_Data_Refused;
                  return;
               end if;
               if K.Kind = E.Pairwise_3 then
                  if Contents.RSN_Element.Length /= S.Beacon_Bytes
                    or else (for some I in 0 .. S.Beacon_Bytes - 1 =>
                               Plain (Contents.RSN_Element.First + I) /= S.Beacon (I))
                  then
                     Outcome := RSN_Mismatch;
                     return;
                  end if;
                  E.Build_Message_4 (K.Replay, Reply, Reply_Length);
               else
                  E.Build_Group_2 (K.Replay, Reply, Reply_Length);
               end if;
               Seal (S, Reply, Reply_Length);
               Install.GTK_Bytes := Contents.GTK_Bytes_Count;
               for I in 1 .. Contents.GTK_Bytes_Count loop
                  Install.GTK (I) := Contents.GTK (I);
               end loop;
               Install.GTK_Index := Contents.GTK_Key_ID;
               Install.GTK_RSC := K.RSC;
               if K.Kind = E.Group_1 then
                  Install.Kind := Group_Only;
               elsif not S.Handed_Out then
                  Install.Kind := Pairwise_And_Group;
                  Install.TK := S.TK;
                  S.Handed_Out := True;
               else
                  --  A retransmitted message 3: answered, nothing reinstalled.
                  Install := (others => <>);
               end if;
               S.State := Established;
               S.Replay := K.Replay;
               S.Seen := True;
               Outcome := Replied;
            end;

         when E.Other =>
            Outcome := Ignored;
      end case;
   end Receive;

   procedure Clear (S : out Session) is
   begin
      S := (others => <>);
   end Clear;

end Wifi_Handshake;
