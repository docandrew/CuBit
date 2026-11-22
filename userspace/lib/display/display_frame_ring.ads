------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop's frames to Display and Display's answers (IPC-009, second
--  half): one CuBit.Request_Channels channel, the present ring from
--  Desktop and the completion ring from Display. The kernel only sets it up
--  and rings a side that sleeps.
--
--  @description
--  A present is the frame request Desktop sent before as a message (a pool
--  frame, or a plan frame naming the plane plan it shows; the output in the
--  header's reserved field), the frame's damage boxes (at most
--  CuBit.Display_Frame_Damage.Maximum_Boxes; a frame with more sends its
--  area alone, which stands for "everything that changed"), and Desktop's
--  stamp when it queued the frame. An answer is the frame's result as
--  before (published, superseded, rejected or failed, and whether the
--  buffer came back) and Display's stamps when it took the frame and when
--  it answered. Every present is answered exactly once: Display takes a
--  present only with a completion slot reserved (Request_Channels), so a
--  full ring is back pressure, never a drop.
--
--  Everything here is words: each side copies an entry out of the peer's
--  page once (Copy_Then_Validate) and decodes the copy; a header word whose
--  unused bits are set does not decode.
--
--  Opening: Desktop opens the channel through its endpoint to Display
--  (CuBit.Channels.Open with Element, producing). Display admits it
--  (Display_Frame_Rings) and still checks every frame against the display
--  lease, session and sequence it holds for the sender.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Contracts;
with CuBit.Desktop_Protocol;
with CuBit.Display_Frame_Damage;
with CuBit.Protocols;
with CuBit.Request_Channels;

package Display_Frame_Ring is

   package DP renames CuBit.Desktop_Protocol;
   package FD renames CuBit.Display_Frame_Damage;

   --  Header word: label (bits 0 .. 31), length (32 .. 39), protocol flags
   --  (40 .. 43), reserved (48 .. 63); bits 44 .. 47 are zero.
   function Header (Item : DP.Wire_Message) return Unsigned_64 is
     (Unsigned_64 (Item.Label) or Shift_Left (Unsigned_64 (Item.Length), 32) or
      Shift_Left (Unsigned_64 (Item.Flags), 40) or Shift_Left (Unsigned_64 (Item.Reserved), 48));
   function Valid_Header (Word : Unsigned_64) return Boolean is
     ((Shift_Right (Word, 44) and 16#F#) = 0);

   subtype Present_Index is Natural range 0 .. 16;
   type Present_Words is array (Present_Index) of Unsigned_64;
   Frame_Header_Word : constant := 0;
   Frame_First_Word  : constant := 1;   --  .. 4: the frame message's words
   Damage_First_Word : constant := 5;   --  .. 15: FD.Word_Array
   Submitted_Word    : constant := 16;
   pragma Compile_Time_Error
     (Damage_First_Word + FD.Record_Words /= Submitted_Word, "the damage record fills words 5 .. 15");

   subtype Answer_Index is Natural range 0 .. 6;
   type Answer_Words is array (Answer_Index) of Unsigned_64;
   Answer_Header_Word : constant := 0;
   Answer_First_Word  : constant := 1;  --  .. 4: the result message's words
   Taken_Word         : constant := 5;
   Answered_Word      : constant := 6;

   type Present is record
      Frame        : DP.Wire_Message;
      Damage       : FD.Word_Array := [others => 0];
      Submitted_Us : Unsigned_64 := 0;
   end record;

   type Answer is record
      Result      : DP.Wire_Message;
      Taken_Us    : Unsigned_64 := 0;
      Answered_Us : Unsigned_64 := 0;
   end record;

   function Encode (Item : Present) return Present_Words;
   type Present_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Present;
         when False => null;
      end case;
   end record;
   function Decode (Words : Present_Words) return Present_Decoding;

   function Encode (Item : Answer) return Answer_Words;
   type Answer_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Answer;
         when False => null;
      end case;
   end record;
   function Decode (Words : Answer_Words) return Answer_Decoding;

   --  Eight presents in flight at most (outputs times frames each, with
   --  room); as many answers, so every present has its slot.
   Request_Bits    : constant := 3;
   Completion_Bits : constant := 3;
   package RC is new CuBit.Request_Channels
     (Present_Words, Answer_Words, Request_Bits => Request_Bits, Completion_Bits => Completion_Bits);

   Schema : constant CuBit.Protocols.Schema_Id := 16#4652_414D_4552_0001#;
   Token_Bytes : constant := 8;
   Present_Bytes : constant := (Present_Index'Last + 1) * 8;
   Submission_Bytes : constant := Token_Bytes + Present_Bytes;

   Element : constant CuBit.Channel_Contracts.Contract :=
     (Element => (Identity => Schema, Version => 1,
                  Sizing => CuBit.Protocols.Fixed_Size, Wire_Size => Submission_Bytes),
      Kind    => CuBit.Channel_Contracts.Duplex,
      Policy  => CuBit.Channel_Contracts.Lossless,
      Pages   => RC.Client_Pages,
      Buffers => RC.Server_Pages,
      Rule    => CuBit.Channel_Contracts.Copy_Then_Validate);

   --  Frames outstanding on one channel at most (Display answers each).
   Frames_In_Flight : constant := 2 ** Request_Bits;
   pragma Compile_Time_Error
     (Frames_In_Flight > 2 ** Completion_Bits, "every present needs its answer's slot");

   --  Channels per client at Display: Desktop's, and one more while a
   --  restarted Desktop opens before the old channel's end is seen.
   Channels_Per_Client : constant := 2;

end Display_Frame_Ring;
