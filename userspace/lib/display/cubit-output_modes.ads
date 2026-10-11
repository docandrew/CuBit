pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Output_Discovery;

--  Output mode changes on already authorized service endpoints: a host or
--  monitor changes its preferred mode (QEMU window resize, hot plug), and a
--  client switches an output to a new mode.
--
--  Watch is the back-flow of a layered call graph: the upper service submits
--  it asynchronously and the provider answers once its mode revision differs
--  from Known (at once if it already does). The answer names the new
--  revision only; the catalog (CuBit.Output_Discovery) says what changed.
--  Set_Mode is an ordinary downward call. The output travels in the tag's
--  routing field (CuBit.Display_Protocol.With_Output), not in the payload.
package CuBit.Output_Modes with SPARK_Mode, Pure is
   package DP renames CuBit.Desktop_Protocol;
   package OD renames CuBit.Output_Discovery;
   use type DP.Wire_Message, DP.Status_Code;

   type Operation is (Watch, Set_Mode);
   function Code (Service : OD.Provider; Action : Operation) return Unsigned_32 is
     (case Service is
        when OD.Display_Broker =>
          (case Action is when Watch => 16#0914#, when Set_Mode => 16#0915#),
        when OD.GPU_Backend =>
          (case Action is when Watch => 16#0A0A#, when Set_Mode => 16#0A0B#));

   subtype Revision is OD.Catalog_Revision;

   type Mode is record
      Width, Height : OD.Extent := 1;
   end record;

   --  32-bit pixels, no row padding (virtio-gpu 2D resources).
   Bytes_Per_Pixel : constant := 4;
   --  At most 65_535 * 65_535 * 4 < 2 ** 34.
   type Byte_Count is range 0 .. 2 ** 34;
   function Bytes (Item : Mode) return Byte_Count is
     (Byte_Count (Item.Width) * Byte_Count (Item.Height) * Bytes_Per_Pixel);

   --  The mode a backend with Budget bytes per scanout buffer offers for a
   --  Requested mode: Requested itself when it fits and is no smaller than
   --  Minimum; otherwise each dimension raised to Minimum, and then both
   --  scaled down together (keeping the aspect within rounding) until it
   --  fits. Never below Minimum, never over Budget.
   Scale_Steps : constant := 1024;
   function Fit (Requested, Minimum : Mode; Budget : Byte_Count) return Mode
     with Pre => Bytes (Minimum) <= Budget,
       Post => Bytes (Fit'Result) <= Budget and then
         Fit'Result.Width >= Minimum.Width and then
         Fit'Result.Height >= Minimum.Height and then
         Fit'Result.Width <= OD.Extent'Max (Requested.Width, Minimum.Width) and then
         Fit'Result.Height <= OD.Extent'Max (Requested.Height, Minimum.Height) and then
         (if Requested.Width >= Minimum.Width and then
             Requested.Height >= Minimum.Height and then
             Bytes (Requested) <= Budget
          then Fit'Result = Requested);

   type Mode_Result is record
      Status  : DP.Status_Code := DP.Bad_State;
      Current : Mode;
   end record;

   type Revision_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Revision;
         when False => null;
      end case;
   end record;
   type Mode_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Mode;
         when False => null;
      end case;
   end record;
   type Result_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Mode_Result;
         when False => null;
      end case;
   end record;

   --  A watch request (Known: the revision the client has seen) and its
   --  answer (the provider's current revision) share one shape.
   function Encode_Watch (Service : OD.Provider; Known : Revision)
      return DP.Wire_Message is
     ((Label => Code (Service, Watch), Length => 4, Flags => 0, Reserved => 0,
       Words => [Known, 0, 0, 0]));
   function Decode_Watch (Service : OD.Provider; Wire : DP.Wire_Message)
      return Revision_Decoding is
     (if Wire.Label /= Code (Service, Watch) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else Wire.Words (0) = 0 or else
        Wire.Words (1) /= 0 or else Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then (Valid => False)
      else (True, Wire.Words (0)))
     with Post =>
       (if Decode_Watch'Result.Valid then
          Encode_Watch (Service, Decode_Watch'Result.Value) = Wire);

   function Encode_Set_Mode (Service : OD.Provider; Item : Mode)
      return DP.Wire_Message is
     ((Label => Code (Service, Set_Mode), Length => 4, Flags => 0, Reserved => 0,
       Words => [Unsigned_64 (Item.Width), Unsigned_64 (Item.Height), 0, 0]));
   --  Routing already removed (Without_Output) by the receiver.
   function Decode_Set_Mode (Service : OD.Provider; Wire : DP.Wire_Message)
      return Mode_Decoding
     with Post =>
       (if Decode_Set_Mode'Result.Valid then
          Encode_Set_Mode (Service, Decode_Set_Mode'Result.Value) = Wire);

   function Status_Of (Word : Unsigned_64) return DP.Status_Code
     with Pre => Word <= DP.Status_Code'Enum_Rep (DP.Status_Code'Last),
       Post => DP.Status_Code'Enum_Rep (Status_Of'Result) = Word;

   function Encode_Result (Service : OD.Provider; Item : Mode_Result)
      return DP.Wire_Message is
     ((Label => Code (Service, Set_Mode), Length => 4, Flags => 0, Reserved => 0,
       Words => [DP.Status_Code'Enum_Rep (Item.Status),
                 Unsigned_64 (Item.Current.Width), Unsigned_64 (Item.Current.Height), 0]));
   function Decode_Result (Service : OD.Provider; Wire : DP.Wire_Message)
      return Result_Decoding
     with Post =>
       (if Decode_Result'Result.Valid then
          Encode_Result (Service, Decode_Result'Result.Value) = Wire);

end CuBit.Output_Modes;
