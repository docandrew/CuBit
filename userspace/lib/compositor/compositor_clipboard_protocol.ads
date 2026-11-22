------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop clipboard wire codec (copy offer and paste), docs/clipboard.md
--
--  Desktop protocol extension, like Compositor_Input_Protocol: no IPC and no
--  authority here. The endpoint authenticates the sender; Desktop checks
--  focus and a single-use gesture ticket. Bulk bytes travel in a transfer
--  window: one page-aligned region the client owns and grants read-write to
--  Desktop once. Desktop maps it per request, copies, and returns it before
--  replying. Layout inside the window, from offset 0: type name, text form,
--  CCL form, contiguous, no header (lengths travel in the request/reply).
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Grant_References;

package Compositor_Clipboard_Protocol with SPARK_Mode, Pure is
   package DP renames CuBit.Desktop_Protocol;
   package GR renames CuBit.Grant_References;
   subtype Word is Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_64;
   use type DP.Status_Code;

   Offer_Label : constant := 16#0870#;
   Paste_Label : constant := 16#0871#;

   Page_Bytes : constant := 4_096;
   --  Type name, text and CCL source together (Clipboard_Store's limit).
   Maximum_Payload : constant := 65_536;
   Window_Pages : constant := Maximum_Payload / Page_Bytes;
   Maximum_Type_Name : constant := 256;

   type Payload_Length is range 0 .. Maximum_Payload;
   subtype Type_Name_Length is Payload_Length range 0 .. Maximum_Type_Name;

   type Sensitivity is (Ordinary, Sensitive);
   for Sensitivity use (Ordinary => 0, Sensitive => 1);

   --  Lengths of the window's regions in order.
   type Layout is record
      Type_Length : Type_Name_Length := 0;
      Text_Length : Payload_Length := 0;
      CCL_Length  : Payload_Length := 0;
   end record;
   function Fits (L : Layout) return Boolean is
     (Natural (L.Type_Length) + Natural (L.Text_Length) +
        Natural (L.CCL_Length) <= Maximum_Payload);
   function Well_Formed (L : Layout) return Boolean is
     (Fits (L) and then L.Text_Length > 0 and then
      (L.CCL_Length > 0) = (L.Type_Length > 0));
   function Total (L : Layout) return Payload_Length is
     (L.Type_Length + L.Text_Length + L.CCL_Length)
     with Pre => Fits (L);

   type Surface_Name is new Word range 1 .. Word'Last;
   type Entry_Name is new Word range 1 .. Word'Last;

   --  Copy: offer the window's first Total (Shape) bytes as a new entry.
   type Offer_Request is record
      Surface : Surface_Name := 1;
      Window  : GR.Reference;
      Shape   : Layout;
      Kind    : Sensitivity := Ordinary;
   end record;
   type Offer_Decoding (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Offer_Request;
         when False => null;
      end case;
   end record;
   function Encode (Value : Offer_Request) return DP.Wire_Message
     with Pre => Well_Formed (Value.Shape);
   function Decode_Offer (Wire : DP.Wire_Message) return Offer_Decoding
     with Post => (if Decode_Offer'Result.Accepted then
                     Well_Formed (Decode_Offer'Result.Value.Shape));

   type Offer_Receipt (Status : DP.Status_Code := DP.Invalid_Request) is record
      case Status is
         when DP.Success => Name : Entry_Name := 1;
         when others => null;
      end case;
   end record;
   function Encode (Value : Offer_Receipt) return DP.Wire_Message;
   function Decode_Offer_Receipt (Wire : DP.Wire_Message) return Offer_Receipt;

   --  Paste: Desktop writes the current entry into the window. Text is always
   --  accepted; Want_CCL also asks for the typed form. A sensitive target
   --  (a secret field) consumes whatever it receives: paste once.
   type Paste_Request is record
      Surface  : Surface_Name := 1;
      Window   : GR.Reference;
      Want_CCL : Boolean := False;
      Target   : Sensitivity := Ordinary;
   end record;
   type Paste_Decoding (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Paste_Request;
         when False => null;
      end case;
   end record;
   function Encode (Value : Paste_Request) return DP.Wire_Message;
   function Decode_Paste (Wire : DP.Wire_Message) return Paste_Decoding;

   type Paste_Receipt (Status : DP.Status_Code := DP.Invalid_Request) is record
      case Status is
         when DP.Success =>
            Name  : Entry_Name := 1;
            Shape : Layout;
            Kind  : Sensitivity := Ordinary;
         when others => null;
      end case;
   end record;
   function Valid (Value : Paste_Receipt) return Boolean is
     (Value.Status /= DP.Success or else Well_Formed (Value.Shape));
   function Encode (Value : Paste_Receipt) return DP.Wire_Message
     with Pre => Valid (Value);
   function Decode_Paste_Receipt (Wire : DP.Wire_Message) return Paste_Receipt
     with Post => Valid (Decode_Paste_Receipt'Result);

   --  Shape and sensitivity in one word: text bits 0..19, CCL 20..39, type
   --  name 40..49, sensitivity bit 56.
   function Pack (Shape : Layout; Kind : Sensitivity) return Word;
   type Descriptor (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Shape : Layout;
            Kind  : Sensitivity;
         when False => null;
      end case;
   end record;
   function Unpack (Value : Word) return Descriptor
     with Post => (if Unpack'Result.Valid then
                     Well_Formed (Unpack'Result.Shape));
   function Round_Trip (Shape : Layout; Kind : Sensitivity) return Boolean is
     (Unpack (Pack (Shape, Kind)).Valid and then
      Unpack (Pack (Shape, Kind)).Shape = Shape and then
      Unpack (Pack (Shape, Kind)).Kind = Kind)
     with Ghost;
end Compositor_Clipboard_Protocol;
