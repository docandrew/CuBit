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
--  Set_Mode, List_Mode and Identify are ordinary downward calls. The output
--  travels in the tag's routing field (CuBit.Display_Protocol.With_Output),
--  not in the payload.
package CuBit.Output_Modes with SPARK_Mode, Pure is
   use type CuBit.Desktop_Protocol.Protocol_Flags;
   package DP renames CuBit.Desktop_Protocol;
   package OD renames CuBit.Output_Discovery;
   use type DP.Wire_Message, DP.Status_Code;

   type Operation is (Watch, Set_Mode, List_Mode, Identify);
   function Code (Service : OD.Provider; Action : Operation) return Unsigned_32 is
     (case Service is
        when OD.Display_Broker =>
          (case Action is
             when Watch => 16#0914#, when Set_Mode => 16#0915#, when List_Mode => 16#092B#,
             when Identify => 16#092C#),
        when OD.GPU_Backend =>
          (case Action is
             when Watch => 16#0A0A#, when Set_Mode => 16#0A0B#, when List_Mode => 16#0A0C#,
             when Identify => 16#0A0D#));

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

   --  A refresh rate as List_Mode reports it.
   subtype Millihertz is Unsigned_32 range 1 .. Unsigned_32'Last;

   --  What Set_Mode asks for (DISP-016): a size, and how to pick its rate.
   --  Best_Rate lets the provider choose (its preferred mode of that size,
   --  else the highest pixel clock; a virtual output fits the size to its
   --  buffers, as for a host window). Exact_Rate names one of the output's
   --  listed modes: that size at the refresh rate List_Mode reports for it.
   type Rate_Choice is (Best_Rate, Exact_Rate);
   for Rate_Choice use (Best_Rate => 0, Exact_Rate => 1);
   type Mode_Request (Rate : Rate_Choice := Best_Rate) is record
      Size : Mode;
      case Rate is
         when Exact_Rate => Refresh : Millihertz := Millihertz'First;
         when Best_Rate => null;
      end case;
   end record;

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
   type Request_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Mode_Request;
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

   --  Words: [width, height, rate choice, refresh mHz (Exact_Rate) or 0].
   function Encode_Set_Mode (Service : OD.Provider; Item : Mode_Request)
      return DP.Wire_Message is
     ((Label => Code (Service, Set_Mode), Length => 4, Flags => 0, Reserved => 0,
       Words => [Unsigned_64 (Item.Size.Width), Unsigned_64 (Item.Size.Height),
                 Rate_Choice'Enum_Rep (Item.Rate),
                 (case Item.Rate is
                    when Exact_Rate => Unsigned_64 (Item.Refresh),
                    when Best_Rate => 0)]));
   --  Routing already removed (Without_Output) by the receiver.
   function Decode_Set_Mode (Service : OD.Provider; Wire : DP.Wire_Message)
      return Request_Decoding
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


   ---------------------------------------------------------------------------
   --  List_Mode (DISP-005): the output's modes as its monitor states them,
   --  each with the driver's admission verdict, one per call (Index is
   --  1-based; the answer carries the count). Read-only.
   ---------------------------------------------------------------------------
   --  Why a mode can or cannot be driven (a driver's typed reason, shared
   --  so Settings can name it).
   --  Below_Minimum_Size and Above_Scanout_Buffers are a virtual output's
   --  (virtio-gpu: any size its buffers hold, from the desktop's minimum).
   type Admission is
     (Admitted, Sync_Unsupported, Port_Unsupported, Link_Unsupported,
      Above_Source_Clock, Above_Sink_Clock, Clock_Hole, Needs_Scrambling,
      Needs_Higher_CD_Clock, Below_Minimum_Size, Above_Scanout_Buffers);
   for Admission use
     (Admitted => 0, Sync_Unsupported => 1, Port_Unsupported => 2, Link_Unsupported => 3,
      Above_Source_Clock => 4, Above_Sink_Clock => 5, Clock_Hole => 6, Needs_Scrambling => 7,
      Needs_Higher_CD_Clock => 8, Below_Minimum_Size => 9, Above_Scanout_Buffers => 10);
   function Name (Item : Admission) return String is
     (case Item is
        when Admitted => "available",
        when Sync_Unsupported => "sync type not supported",
        when Port_Unsupported => "port not supported yet",
        when Link_Unsupported => "link type not supported yet",
        when Above_Source_Clock => "above the GPU's pixel clock",
        when Above_Sink_Clock => "above the monitor's link clock",
        when Clock_Hole => "pixel clock in the PHY's gap",
        when Needs_Scrambling => "monitor lacks HDMI 2.0 scrambling",
        when Needs_Higher_CD_Clock => "needs a faster display clock",
        when Below_Minimum_Size => "below the desktop's minimum size",
        when Above_Scanout_Buffers => "larger than the scanout buffers");

   type Mode_Count is new Unsigned_16;
   subtype Mode_Index is Mode_Count range 1 .. Mode_Count'Last;
   --  Millihertz and kHz fit 32 bits for any EDID mode. Current: the mode
   --  the output runs now (at most one per output; none when it runs a
   --  timing its monitor does not list).
   type Listed_Mode is record
      Size : Mode;
      Refresh_Millihertz : Unsigned_32 := 0;
      Pixel_kHz : Unsigned_32 := 0;
      Verdict : Admission := Admitted;
      Preferred, Current : Boolean := False;
   end record;
   --  Settings order (DISP-016): larger sizes first (width, then height),
   --  then higher refresh rates; a strict weak order, so a stable sort
   --  keeps the monitor's order among equal modes.
   function Before (Left, Right : Listed_Mode) return Boolean is
     (Left.Size.Width > Right.Size.Width or else
      (Left.Size.Width = Right.Size.Width and then
       (Left.Size.Height > Right.Size.Height or else
        (Left.Size.Height = Right.Size.Height and then
         Left.Refresh_Millihertz > Right.Refresh_Millihertz))));
   --  The Set_Mode request that names Item exactly.
   function Request_For (Item : Listed_Mode) return Mode_Request is
     (if Item.Refresh_Millihertz = 0 then (Best_Rate, Item.Size)
      else (Exact_Rate, Item.Size, Item.Refresh_Millihertz));
   --  Whether a provider's listed mode is the one Request names (an Exact
   --  request names one rate; a Best request names its size).
   function Names (Request : Mode_Request; Item : Listed_Mode) return Boolean is
     (Request.Size = Item.Size and then
      (case Request.Rate is
         when Exact_Rate => Item.Refresh_Millihertz = Request.Refresh,
         when Best_Rate => True));
   type Listing is record
      Index : Mode_Index := 1;
      Count : Mode_Count := 1;
      Item : Listed_Mode;
   end record;
   type Index_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Mode_Index;
         when False => null;
      end case;
   end record;
   type Listing_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Listing;
         when False => null;
      end case;
   end record;

   function Encode_List_Request (Service : OD.Provider; Index : Mode_Index)
      return DP.Wire_Message is
     ((Label => Code (Service, List_Mode), Length => 4, Flags => 0, Reserved => 0,
       Words => [Unsigned_64 (Index), 0, 0, 0]));
   --  Routing already removed by the receiver.
   function Decode_List_Request (Service : OD.Provider; Wire : DP.Wire_Message)
      return Index_Decoding
     with Post =>
       (if Decode_List_Request'Result.Valid then
          Encode_List_Request (Service, Decode_List_Request'Result.Value) = Wire);

   --  Words: [index | count << 16 | verdict << 32 | preferred << 40 |
   --  current << 41], [width | height << 16], [refresh mHz], [pixel kHz].
   function Encode_Listing (Service : OD.Provider; Item : Listing) return DP.Wire_Message
     with Pre => Item.Index <= Item.Count;
   function Decode_Listing (Service : OD.Provider; Wire : DP.Wire_Message)
      return Listing_Decoding
     with Post =>
       (if Decode_Listing'Result.Valid then
          Decode_Listing'Result.Value.Index <= Decode_Listing'Result.Value.Count);
   --  Canonical (Encode_Listing (Decode_Listing (W)) = W for every accepted
   --  W): tested against every single-bit change of every word.

   ---------------------------------------------------------------------------
   --  Identify (DISP-016): the output's monitor as its EDID names it, so a
   --  client can keep per-monitor settings (the chosen mode) keyed by it.
   --  An output without EDID answers a one-word status (Unsupported).
   ---------------------------------------------------------------------------
   --  The PNP manufacturer ID ('?' for a letter outside A .. Z) and the
   --  monitor name descriptor (printable ASCII, space padded).
   subtype Vendor_Letter is Character
     with Static_Predicate => Vendor_Letter in 'A' .. 'Z' | '?';
   type Vendor_ID is array (1 .. 3) of Vendor_Letter;
   Name_Length : constant := 13;
   subtype Name_Count is Natural range 0 .. Name_Length;
   subtype Name_Character is Character range ' ' .. '~';
   type Name_Text is array (1 .. Name_Length) of Name_Character;
   type Monitor_Identity is record
      Vendor : Vendor_ID := ['?', '?', '?'];
      Product : Unsigned_16 := 0;
      Serial : Unsigned_32 := 0;
      Name_Last : Name_Count := 0;
      --  Spaces past Name_Last (the canonical form the codec carries).
      Name : Name_Text := [others => ' '];
   end record;
   function Canonical (Item : Monitor_Identity) return Boolean is
     (for all I in Item.Name_Last + 1 .. Name_Length => Item.Name (I) = ' ');
   type Identity_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Monitor_Identity;
         when False => null;
      end case;
   end record;

   function Identify_Request (Service : OD.Provider) return DP.Wire_Message is
     ((Label => Code (Service, Identify), Length => 4, Flags => 0, Reserved => 0,
       Words => [0, 0, 0, 0]));
   --  Words: [serial | product << 32 | name length << 48],
   --  [vendor (three bytes) | name 1 .. 5 << 24], [name 6 .. 13], [0].
   function Encode_Identity (Service : OD.Provider; Item : Monitor_Identity)
      return DP.Wire_Message
     with Pre => Canonical (Item);
   function Decode_Identity (Service : OD.Provider; Wire : DP.Wire_Message)
      return Identity_Decoding
     with Post =>
       (if Decode_Identity'Result.Valid then Canonical (Decode_Identity'Result.Value));
   --  Canonical too: tested against every single-bit change of every word.
end CuBit.Output_Modes;
