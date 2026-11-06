------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The display-capture capability (REMOTE-001 "Remote screenshots"): a
--  read-only copy of one composed frame, per output, optionally a
--  rectangle of it, at the output's native pixels. Desktop composes the
--  frames, so Desktop answers; the capability is Desktop's endpoint badged
--  with Authority_Tag, minted by procmgr only for a manifest request
--  ("display-capture") on a startup entry that also says
--  (display-capture approve-declared). Never ambient: a plain "desktop"
--  endpoint (tag 0) is refused, and so is every other tag.
--
--  @description
--  The grant makes capture possible; the person at the device makes it
--  allowed: Desktop's status panel shows an "Allow screen capture" switch
--  for the remote session, off by default and off again whenever a new
--  host pairs. A capture while it is off is refused (Capture_Off), as is
--  one sooner than Desktop_Capture_Policy's interval (Rate_Limited).
--  Single frames only: there is no stream (screen sharing is a separate,
--  future capability).
--
--  Announce (the holder's remote session, so the panel can show it):
--    words (0) Session_State'Enum_Rep, (1 .. 3) the paired peer's address
--    as text, Peer_Text_Bytes bytes, NUL padded (empty when not paired).
--    Reply: label Status; words (0) 1 when capture is allowed now, else 0.
--  Capture (one frame into the caller's buffer):
--    words (0) a memory grant reference (CuBit.Grant_References.Encode) of
--    the caller's own writable buffer, (1) its length in bytes, (2) the
--    output number, (3) the area: x | y << 16 | width << 32 | height << 48
--    in the output's pixels (all zero: the whole output).
--    Desktop maps the buffer, copies the area of the newest frame it
--    completed for that output as BGRA 8888 rows of width * 4 bytes (no
--    padding), returns the mapping, and replies: label Status; words (0)
--    width | height << 32, (1) the bytes written, (2) the capture's serial
--    number (from 1, per Desktop run). Nothing of the frame is kept.
--    Buffer_Too_Small carries the same words (0) and (1): the size and the
--    bytes the area needs (no capture happened; the holder asks again).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Display_Capture with Pure, SPARK_Mode is
   --  The catalog role (userspace/ccl/catalogs/native-runtime-services.ccl,
   --  "display-capture"); procmgr answers it with Desktop's endpoint.
   Service_Role : constant Unsigned_64 := 39;
   --  "DSCP" and a version: kernel-stamped, so knowing it grants nothing.
   Authority_Tag : constant Unsigned_64 := 16#4453_4350_0000_0001#;

   Announce_Label : constant := 16#0880#;
   Capture_Label  : constant := 16#0881#;
   Request_Words  : constant := 4;

   --  How a request ended (the reply's label).
   type Status is
     (Captured,          --  done (Announce: noted)
      Not_Authorized,    --  not the display-capture badge
      Invalid_Request,   --  malformed words
      Capture_Off,       --  the person has not allowed screen capture
      Rate_Limited,      --  sooner than the policy's interval after the last
      No_Session,        --  capture before the holder announced a session
      No_Frame,          --  that output shows no completed frame yet
      Bad_Output,        --  no such output
      Bad_Area,          --  the area is empty or leaves the output
      Buffer_Refused,    --  the buffer grant did not map for writing
      Buffer_Too_Small); --  the buffer cannot hold the area
   for Status use
     (Captured => 16#F000#, Not_Authorized => 16#F002#, Invalid_Request => 16#F003#,
      Capture_Off => 16#F110#,
      Rate_Limited => 16#F111#, No_Session => 16#F112#, No_Frame => 16#F113#,
      Bad_Output => 16#F114#, Bad_Area => 16#F115#, Buffer_Refused => 16#F116#,
      Buffer_Too_Small => 16#F117#);

   type Status_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Status;
         when False => null;
      end case;
   end record;
   function Decode_Status (Label : Unsigned_32) return Status_Decoding is
     (case Label is
         when 16#F000# => (True, Captured),
         when 16#F002# => (True, Not_Authorized),
         when 16#F110# => (True, Capture_Off),
         when 16#F111# => (True, Rate_Limited),
         when 16#F112# => (True, No_Session),
         when 16#F113# => (True, No_Frame),
         when 16#F114# => (True, Bad_Output),
         when 16#F115# => (True, Bad_Area),
         when 16#F116# => (True, Buffer_Refused),
         when 16#F117# => (True, Buffer_Too_Small),
         when 16#F003# => (True, Invalid_Request),
         when others => (Valid => False))
     with Post => (if Decode_Status'Result.Valid
                   then Status'Enum_Rep (Decode_Status'Result.Value) = Label);

   --  What the person reads for each refusal (Desktop's log, ccl-remote's
   --  activity line, the host CLI).
   function Reason (Item : Status) return String is
     (case Item is
         when Captured => "captured",
         when Not_Authorized => "not the display-capture capability",
         when Capture_Off => "screen capture is off on the device (Deskbar, Remote access, " &
                             "Allow screen capture)",
         when Rate_Limited => "too soon after the last capture",
         when No_Session => "no remote session announced",
         when No_Frame => "that output shows no frame yet",
         when Bad_Output => "no such output",
         when Bad_Area => "the area is empty or leaves the output",
         when Buffer_Refused => "the capture buffer did not map",
         when Buffer_Too_Small => "the capture buffer is too small",
         when Invalid_Request => "a malformed request");

   ---------------------------------------------------------------------------
   --  The holder's remote session
   ---------------------------------------------------------------------------
   type Session_State is (No_Session, Listening, Paired);
   for Session_State use (No_Session => 0, Listening => 1, Paired => 2);
   Peer_Text_Bytes : constant := 24;
   subtype Peer_Length is Natural range 0 .. Peer_Text_Bytes;

   ---------------------------------------------------------------------------
   --  Areas
   ---------------------------------------------------------------------------
   Maximum_Side : constant := 65_535;
   subtype Coordinate is Natural range 0 .. Maximum_Side;
   type Area is record
      X, Y, Width, Height : Coordinate := 0;
   end record;
   Whole_Output : constant Area := (others => 0);
   function Is_Whole (Item : Area) return Boolean is (Item = Whole_Output);

   Field_Mask : constant Unsigned_64 := 16#FFFF#;
   function Encode_Area (Item : Area) return Unsigned_64 is
     (Unsigned_64 (Item.X) or Shift_Left (Unsigned_64 (Item.Y), 16) or
      Shift_Left (Unsigned_64 (Item.Width), 32) or Shift_Left (Unsigned_64 (Item.Height), 48));
   function Decode_Area (Word : Unsigned_64) return Area is
     ((X => Coordinate (Word and Field_Mask),
       Y => Coordinate (Shift_Right (Word, 16) and Field_Mask),
       Width => Coordinate (Shift_Right (Word, 32) and Field_Mask),
       Height => Coordinate (Shift_Right (Word, 48) and Field_Mask)));
   --  Each field is 16 bits, so every word is some area's encoding
   --  (tests/display-capture checks the round trip).

   --  The rectangle Item selects of a Width x Height output: the whole of
   --  it, or Item when it is nonempty and inside; Fits is False otherwise.
   type Resolution is record
      Fits : Boolean := False;
      Chosen : Area := Whole_Output;
   end record;
   function Resolve (Item : Area; Output_Width, Output_Height : Coordinate) return Resolution is
     (if Is_Whole (Item) then
        (if Output_Width > 0 and then Output_Height > 0
         then (True, (0, 0, Output_Width, Output_Height)) else (False, Whole_Output))
      elsif Item.Width > 0 and then Item.Height > 0 and then
        Item.X + Item.Width <= Output_Width and then Item.Y + Item.Height <= Output_Height
      then (True, Item)
      else (False, Whole_Output))
     with Post =>
       (if Resolve'Result.Fits then
          Resolve'Result.Chosen.Width > 0 and then Resolve'Result.Chosen.Height > 0 and then
          Resolve'Result.Chosen.X + Resolve'Result.Chosen.Width <= Output_Width and then
          Resolve'Result.Chosen.Y + Resolve'Result.Chosen.Height <= Output_Height);

   --  The bytes a capture of Item takes (BGRA, unpadded rows).
   Bytes_Per_Pixel : constant := 4;
   function Bytes_Of (Item : Area) return Unsigned_64 is
     (Unsigned_64 (Item.Width) * Unsigned_64 (Item.Height) * Bytes_Per_Pixel);

   --  The reply's size word.
   function Encode_Size (Width, Height : Coordinate) return Unsigned_64 is
     (Unsigned_64 (Width) + Unsigned_64 (Height) * 2 ** 32);
   function Width_Of (Word : Unsigned_64) return Unsigned_64 is (Word mod 2 ** 32);
   function Height_Of (Word : Unsigned_64) return Unsigned_64 is (Word / 2 ** 32);
end CuBit.Display_Capture;
