------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with Desktop_Logs;

package body Desktop_Capture with SPARK_Mode => Off is
   package MG renames CuBit.Memory_Grants;
   package GR renames CuBit.Grant_References;
   package P renames Desktop_Capture_Policy;
   use type System.Address;
   use type P.Decision, P.Session, DC.Status;

   LF : constant Character := Character'Val (10);

   function memcpy (Dest, Src : System.Address; Len : Storage_Count) return System.Address
     with Import, Convention => C, External_Name => "memcpy";

   Policy : P.State := P.Initial;
   Peer_Text : String (1 .. DC.Peer_Text_Bytes) := [others => ' '];
   Peer_Last : DC.Peer_Length := 0;
   --  Captures answered since Desktop started (the reply's serial).
   Serial : Unsigned_64 := 0;

   function Handles (Label : Unsigned_32) return Boolean is
     (Label = DC.Announce_Label or else Label = DC.Capture_Label);

   function Session return P.Session is (Policy.Holder);
   function Peer return String is (Peer_Text (1 .. Peer_Last));
   function Allowed return Boolean is (Policy.Allowed);
   function Marked return Boolean is (Policy.Marked);
   function Mark_End return Unsigned_64 is (Policy.Mark_End);

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Value'Image;
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Process_Text (From : CuBit.Messages.Process_ID) return String is
     ("process " & Decimal (CuBit.Messages.To_Word (From)));

   function Answer (Result : DC.Status; W0, W1, W2 : Unsigned_64 := 0) return CuBit.Messages.Message is
     (tag => (label => DC.Status'Enum_Rep (Result), length => 3, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0, words => [0 => W0, 1 => W1, 2 => W2, 3 => 0]);

   procedure Set_Allowed (On : Boolean; Now : Unsigned_64) is
      Ignore : P.Decision;
      Was : constant Boolean := Policy.Allowed;
   begin
      P.Step (Policy, (Kind => P.Set_Allowed, Now => Now, Allowed => On), Ignore);
      if Policy.Allowed /= Was then
         Desktop_Logs.Write ("desktop: screen capture " &
           (if Policy.Allowed then "allowed" else "not allowed") &
           " for the remote session (by the person at the device)" & LF);
      end if;
   end Set_Allowed;

   procedure Tick (Now : Unsigned_64; Changed : out Boolean) is
      Ignore : P.Decision;
      Was : constant Boolean := Policy.Marked;
   begin
      P.Step (Policy, (Kind => P.Tick, Now => Now), Ignore);
      Changed := Was and then not Policy.Marked;
   end Tick;

   ---------------------------------------------------------------------------
   --  Announce
   ---------------------------------------------------------------------------
   procedure Announce
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Now : Unsigned_64;
      Reply : out CuBit.Messages.Message; Changed : out Boolean)
   is
      Holder : P.Session;
      Text : String (1 .. DC.Peer_Text_Bytes) := [others => ' '];
      Last : DC.Peer_Length := 0;
      Ignore : P.Decision;
      New_Pairing : Boolean;
   begin
      Changed := False;
      if Request.tag.length /= DC.Request_Words or else Request.words (0) > 2 then
         Reply := Answer (DC.Invalid_Request);
         return;
      end if;
      Holder := (case Request.words (0) is
                    when 0 => P.No_Session, when 1 => P.Listening, when others => P.Paired);
      --  The peer's address: printable bytes up to the first NUL.
      Scan :
      for Word in 1 .. 3 loop
         for B in 0 .. 7 loop
            declare
               Byte : constant Unsigned_64 := Shift_Right (Request.words (Word), B * 8) and 16#FF#;
            begin
               exit Scan when Byte = 0;
               if Byte not in Character'Pos (' ') .. Character'Pos ('~') then
                  Reply := Answer (DC.Invalid_Request);
                  return;
               end if;
               Last := Last + 1;
               Text (Last) := Character'Val (Byte);
            end;
         end loop;
      end loop Scan;
      New_Pairing := Holder = P.Paired and then
        (Policy.Holder /= P.Paired or else Text (1 .. Last) /= Peer_Text (1 .. Peer_Last));
      Changed := Holder /= Policy.Holder or else Text (1 .. Last) /= Peer_Text (1 .. Peer_Last);
      P.Step (Policy, (Kind => P.Announce, Now => Now, Holder => Holder, New_Pairing => New_Pairing), Ignore);
      Peer_Text := Text;
      Peer_Last := (if Holder = P.Paired then Last else 0);
      if Changed then
         Desktop_Logs.Write ("desktop: remote session (" & Process_Text (From) & ") " &
           (case Holder is
               when P.No_Session => "ended",
               when P.Listening => "listening, not paired",
               when P.Paired => "paired with " & Peer) &
           "; screen capture " & (if Policy.Allowed then "allowed" else "off") & LF);
      end if;
      Reply := Answer (DC.Captured, (if Policy.Allowed then 1 else 0));
   end Announce;

   ---------------------------------------------------------------------------
   --  Capture
   ---------------------------------------------------------------------------
   procedure Capture
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Frames : Frame_Array; Now : Unsigned_64;
      Reply : out CuBit.Messages.Message; Changed : out Boolean)
   is
      Decision : P.Decision;
      Output : Output_Number;
      Wanted : DC.Area;
      Where : DC.Resolution;
      Bytes : Unsigned_64;
      Grant : GR.Reference;
      Mapped : System.Address;
      Acquired, Returned : Boolean;

      procedure Refuse (Why : DC.Status) is
      begin
         Desktop_Logs.Write ("desktop: screen capture refused for " & Process_Text (From) & ": " &
           DC.Reason (Why) & LF);
         Reply := Answer (Why);
      end Refuse;
   begin
      Changed := False;
      if Request.tag.length /= DC.Request_Words or else not GR.Valid_Wire (Request.words (0)) then
         Refuse (DC.Invalid_Request);
         return;
      end if;
      if Request.words (2) > Unsigned_64 (Output_Number'Last) then
         Refuse (DC.Bad_Output);
         return;
      end if;
      Output := Output_Number (Request.words (2));
      Wanted := DC.Decode_Area (Request.words (3));
      if not Frames (Output).Present or else Frames (Output).Address = System.Null_Address then
         Refuse ((if Frames (Output).Width = 0 then DC.Bad_Output else DC.No_Frame));
         return;
      end if;
      Where := DC.Resolve (Wanted, Frames (Output).Width, Frames (Output).Height);
      if not Where.Fits then
         Refuse (DC.Bad_Area);
         return;
      end if;
      Bytes := DC.Bytes_Of (Where.Chosen);
      if Request.words (1) < Bytes then
         --  Not a capture (the policy is not asked): the holder learns the
         --  size it needs and asks again with a buffer that holds it.
         Reply := Answer (DC.Buffer_Too_Small, DC.Encode_Size (Where.Chosen.Width, Where.Chosen.Height), Bytes);
         return;
      end if;
      --  The policy last: a malformed request never counts as a capture.
      P.Step (Policy, (Kind => P.Request, Now => Now), Decision);
      case Decision is
         when P.Refuse_No_Session => Refuse (DC.No_Session); return;
         when P.Refuse_Off => Refuse (DC.Capture_Off); return;
         when P.Refuse_Rate => Refuse (DC.Rate_Limited); return;
         when P.None | P.Capture => null;
      end case;
      Grant := GR.Decode (Request.words (0));
      MG.Acquire (Grant, From, 0, Bytes, MG.Write_Access, Mapped, Acquired);
      if not Acquired or else Mapped = System.Null_Address then
         Refuse (DC.Buffer_Refused);
         Changed := True;   --  the policy counted it (the mark shows an attempt that took effect)
         return;
      end if;
      declare
         Row_Bytes : constant Storage_Count := Storage_Count (Where.Chosen.Width) * DC.Bytes_Per_Pixel;
         Ignore : System.Address;
      begin
         for Row in 0 .. Where.Chosen.Height - 1 loop
            Ignore := memcpy
              (Mapped + Storage_Offset (Row) * Row_Bytes,
               Frames (Output).Address +
                 Storage_Offset ((Where.Chosen.Y + Row) * Frames (Output).Pitch) +
                 Storage_Offset (Where.Chosen.X) * DC.Bytes_Per_Pixel,
               Row_Bytes);
         end loop;
      end;
      MG.Return_Acquisition (Grant, Returned);
      Serial := Serial + 1;
      Changed := True;
      Desktop_Logs.Write ("desktop: screen captured for " & Process_Text (From) & ": output" &
        Output'Image & " " & Decimal (Unsigned_64 (Where.Chosen.Width)) & "x" &
        Decimal (Unsigned_64 (Where.Chosen.Height)) &
        (if DC.Is_Whole (Wanted) then "" else " at " & Decimal (Unsigned_64 (Where.Chosen.X)) & "," &
                                                  Decimal (Unsigned_64 (Where.Chosen.Y))) &
        " (capture" & Serial'Image & ")" & (if Returned then "" else "; mapping return uncertain") & LF);
      Reply := Answer (DC.Captured, DC.Encode_Size (Where.Chosen.Width, Where.Chosen.Height), Bytes, Serial);
   end Capture;

   procedure Handle
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Frames : Frame_Array; Now : Unsigned_64;
      Reply : out CuBit.Messages.Message; Changed : out Boolean)
   is
   begin
      Changed := False;
      --  The badge first: a plain desktop endpoint learns nothing more.
      if Request.authorityTag /= DC.Authority_Tag then
         Desktop_Logs.Write ("desktop: display-capture request refused for " & Process_Text (From) &
           ": " & DC.Reason (DC.Not_Authorized) & LF);
         Reply := Answer (DC.Not_Authorized);
         return;
      end if;
      if Request.tag.label = DC.Announce_Label then
         Announce (From, Request, Now, Reply, Changed);
      else
         Capture (From, Request, Frames, Now, Reply, Changed);
      end if;
   end Handle;
end Desktop_Capture;
