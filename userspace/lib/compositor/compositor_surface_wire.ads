------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The surface ring's elements (IPC-004, docs/desktop-protocol.md): what a
--  client and Desktop exchange through one window's request channel
--  (Compositor_Surface_Ring) once it is open, so that presenting a frame
--  and receiving input take no system call while both sides are busy.
--
--  @description
--  Requests (client -> Desktop), answered exactly once each:
--    Present           the attached buffer's Area changed; answered once
--                      Desktop has drawn it (the client's pixels are read).
--    Publish           stage and publish one frame buffer (Grant) at Epoch
--                      in one step; answered with the publication receipt.
--    Await_Retirement  answered when the publication Ticket has retired
--                      (the client may write that buffer again).
--    Receive_Input     answered when input after serial After is queued,
--                      with up to Input_Batch events in serial order.
--  Each side copies the other's element, then validates it here (rule 1 of
--  docs/data-plane.md); nothing a peer writes is trusted. No system call,
--  allocation or address: the same code runs in both processes and in
--  hosted tests.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Grant_References;

package Compositor_Surface_Wire with SPARK_Mode, Pure is

   package DP renames CuBit.Desktop_Protocol;
   package Pub renames CuBit.Desktop_Protocol.Publication;
   package GR renames CuBit.Grant_References;
   use type DP.Status_Code;
   use type DP.Input_Event_Kind;

   type Request_Kind is (Present, Publish, Await_Retirement, Receive_Input);
   for Request_Kind use
     (Present => 1, Publish => 2, Await_Retirement => 3, Receive_Input => 4);

   --  Events one Receive_Input answer carries at most.
   Input_Batch : constant := 8;
   subtype Event_Count is Natural range 0 .. Input_Batch;
   subtype Event_Index is Positive range 1 .. Input_Batch;

   ---------------------------------------------------------------------------
   --  The wire: what each side writes into its ring slot.
   ---------------------------------------------------------------------------
   type Request_Words is record
      Kind, Surface, A, B, C, D : Unsigned_64 := 0;
   end record;

   type Event_Words is record
      Kind, Serial, Payload0, Payload1 : Unsigned_64 := 0;
   end record;
   type Event_Block is array (Event_Index) of Event_Words;

   type Result_Words is record
      Kind, Status, A, B, Count : Unsigned_64 := 0;
      Events : Event_Block := [others => (others => 0)];
   end record;

   ---------------------------------------------------------------------------
   --  Requests.
   ---------------------------------------------------------------------------
   type Request (Kind : Request_Kind := Present) is record
      Surface : DP.Live_Surface_Name := 1;
      case Kind is
         when Present | Publish =>
            --  Damage, in the buffer's pixels; all zero: the whole buffer.
            Area : DP.Rectangle := (0, 0, 0, 0);
            --  Handled-input watermark (correlation metadata only).
            Input_After : Unsigned_64 := 0;
            case Kind is
               when Publish =>
                  Epoch : Pub.Identity := 1;
                  Grant : GR.Reference;
               when others => null;
            end case;
         when Await_Retirement =>
            Retiring_Epoch, Ticket : Pub.Identity := 1;
         when Receive_Input =>
            After : Unsigned_64 := 0;
      end case;
   end record;

   function Valid (Item : Request) return Boolean is
     (case Item.Kind is
         when Present | Publish => DP.Valid_Damage (Item.Area),
         when others => True);

   function Encode (Item : Request) return Request_Words
   with Pre => Valid (Item);

   type Request_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Request;
         when False => null;
      end case;
   end record;
   function Decode (Wire : Request_Words) return Request_Decoding
   with Post => (if Decode'Result.Valid then Valid (Decode'Result.Value));

   ---------------------------------------------------------------------------
   --  Answers.
   ---------------------------------------------------------------------------
   type Event_Array is array (Event_Index) of DP.Input_Envelope;

   type Result (Kind : Request_Kind := Present) is record
      Status : DP.Status_Code := DP.Invalid_Request;
      case Kind is
         when Present => null;
         when Publish | Await_Retirement =>
            --  Success: the receipt's identities (the staged and published
            --  ticket; the retired one).
            Epoch, Ticket : Pub.Identity := 1;
         when Receive_Input =>
            --  Success: Count events, serials strictly increasing.
            Count : Event_Count := 0;
            More : Boolean := False;
            Events : Event_Array := [others => (others => <>)];
      end case;
   end record;

   --  Each event is a valid envelope with a nonzero serial, the serials
   --  increase, and a successful answer carries at least one.
   function Valid_Events (Item : Result) return Boolean is
     (Item.Kind /= Receive_Input or else Item.Status /= DP.Success or else
        (Item.Count >= 1 and then
         (for all I in 1 .. Item.Count =>
            Item.Events (I).Kind /= DP.No_Input and then
            not Item.Events (I).More_Pending and then
            DP.Valid_Input_Envelope (Item.Events (I)) and then
            Item.Events (I).Serial /= 0 and then
            (I = 1 or else Item.Events (I - 1).Serial < Item.Events (I).Serial))));

   function Encode (Item : Result) return Result_Words
   with Pre => Valid_Events (Item);

   type Result_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Result;
         when False => null;
      end case;
   end record;
   --  Expected: the kind of the request this answer's tag belongs to.
   function Decode (Wire : Result_Words; Expected : Request_Kind) return Result_Decoding
   with Post => (if Decode'Result.Valid then
                   Decode'Result.Value.Kind = Expected and then Valid_Events (Decode'Result.Value));

   ---------------------------------------------------------------------------
   --  Damage packing: x, y, width, height in successive 16-bit fields.
   ---------------------------------------------------------------------------
   Field_Bits : constant := 16;
   function Pack (Area : DP.Rectangle) return Unsigned_64 is
     (Unsigned_64 (Area.X) or Shift_Left (Unsigned_64 (Area.Y), Field_Bits) or
      Shift_Left (Unsigned_64 (Area.Width), 2 * Field_Bits) or
      Shift_Left (Unsigned_64 (Area.Height), 3 * Field_Bits));
   function Unpack (Word : Unsigned_64) return DP.Rectangle is
     ((X      => DP.Pixel_Coordinate (Word and 16#FFFF#),
       Y      => DP.Pixel_Coordinate (Shift_Right (Word, Field_Bits) and 16#FFFF#),
       Width  => DP.Pixel_Extent (Shift_Right (Word, 2 * Field_Bits) and 16#FFFF#),
       Height => DP.Pixel_Extent (Shift_Right (Word, 3 * Field_Bits) and 16#FFFF#)));

end Compositor_Surface_Wire;
