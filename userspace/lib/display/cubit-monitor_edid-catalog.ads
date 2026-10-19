pragma Ada_2022;
with Interfaces;

--  Everything else a monitor's EDID says (VESA E-EDID 1.4, CTA-861-G):
--  who it is, its name and physical size, every detailed timing in the base
--  block and the CTA-861 extensions, the CTA short video codes CuBit knows,
--  and the HDMI sink's TMDS limits. Untrusted metadata like the parent: it
--  says what the monitor claims, not what a link can drive. Mode admission
--  (the display driver) decides that.
package CuBit.Monitor_EDID.Catalog with SPARK_Mode, Pure is
   use Interfaces;

   subtype Block is Base_Block;
   Block_Bytes : constant := 128;

   --  Extension blocks after the base block (its byte 126 says how many).
   subtype Extension_Count is Natural range 0 .. 255;
   type Block_Array is array (Positive range <>) of Block;

   --  Base block byte 126: the extension blocks that follow it.
   function Extensions_Declared (Base : Block) return Extension_Count is
     (Extension_Count (Base (126)));

   --  The checksum of any 128-byte block (base or extension).
   function Valid_Checksum (Data : Block) return Boolean;

   ---------------------------------------------------------------------------
   --  Identity and physical size.
   ---------------------------------------------------------------------------
   Name_Length : constant := 13;
   subtype Name_Count is Natural range 0 .. Name_Length;
   --  Printable ASCII only; anything else reads as '?'.
   type Name_Text is record
      Length : Name_Count := 0;
      Text : String (1 .. Name_Length) := [others => ' '];
   end record;

   --  The three-letter PNP manufacturer ID ('?' for a bad letter).
   subtype Vendor_Text is String (1 .. 3);

   type Identity is record
      Vendor : Vendor_Text := "???";
      Product : Unsigned_16 := 0;
      Serial : Unsigned_32 := 0;
      Name : Name_Text;
      --  Year of manufacture (zero when the block gives a model year only).
      Year : Natural range 0 .. 1990 + 255 := 0;
   end record;
   function Identify (Base : Block) return Identity;

   --  Millimetres; zero means the monitor does not say (a projector).
   type Physical_Size is record
      Width_MM, Height_MM : Dimension_MM := 0;
   end record;
   --  The preferred detailed timing's image size when it gives one (whole
   --  millimetres), else the base block's centimetres.
   function Size_Of (Base : Block) return Physical_Size;

   ---------------------------------------------------------------------------
   --  Modes.
   ---------------------------------------------------------------------------
   type Mode_Source is (Base_Detailed, Extension_Detailed, Video_Code);
   --  CTA-861 Video Identification Code (zero: not from a short descriptor).
   subtype Video_Identification is Natural range 0 .. 255;
   type Mode is record
      Timing : Monitor_EDID.Timing;
      Source : Mode_Source := Base_Detailed;
      Code : Video_Identification := 0;
      --  The base block's first detailed timing (the preferred mode).
      Preferred : Boolean := False;
      --  A short descriptor marked native by the monitor.
      Native : Boolean := False;
   end record;

   --  Each mode costs at least one byte of an extension's 124 data bytes
   --  (bytes 4 .. 127), and the base block has four descriptors: the most
   --  modes the blocks can encode, not a policy.
   Base_Descriptors : constant := 4;
   Extension_Mode_Bytes : constant := Block_Bytes - 4;
   function Capacity (Extensions : Extension_Count) return Positive is
     (Base_Descriptors + Extension_Mode_Bytes * Extensions);

   type Mode_Array is array (Positive range <>) of Mode;
   type Mode_List (Capacity : Positive) is record
      Count : Natural := 0;
      Items : Mode_Array (1 .. Capacity);
   end record
     with Dynamic_Predicate => Mode_List.Count <= Mode_List.Capacity;

   --  Every progressive detailed timing of the base block and of each valid
   --  CTA-861 extension, then the short video codes CuBit knows (CTA-861-G
   --  table 1), duplicates removed (the first source wins). Extensions with
   --  a bad checksum or another tag are skipped, never half-read.
   function Modes (Base : Block; Extensions : Block_Array) return Mode_List
     with Pre => Extensions'Length <= Extension_Count'Last and then
                 (Extensions'Length = 0 or else Extensions'First = 1),
          Post => Modes'Result.Capacity = Capacity (Extensions'Length);
   --  The same, into a list the caller allocated (heap-sized for a monitor
   --  that declares many extensions): List starts empty.
   procedure Collect (Base : Block; Extensions : Block_Array; List : in out Mode_List)
     with Pre => Extensions'Length <= Extension_Count'Last and then
                 List.Capacity >= Capacity (Extensions'Length);

   --  The timing of a CTA-861 code CuBit knows (progressive, square-pixel
   --  modes up to 4096 x 2160 at 60 Hz).
   type Known_Code (Known : Boolean := False) is record
      case Known is
         when True => Timing : Monitor_EDID.Timing;
         when False => null;
      end case;
   end record;
   function Code_Timing (Code : Video_Identification) return Known_Code;

   ---------------------------------------------------------------------------
   --  HDMI sink limits (CTA-861 vendor-specific data blocks).
   ---------------------------------------------------------------------------
   --  kHz; zero means not stated.
   subtype Clock_kHz is Natural range 0 .. 2_550_000;
   type Sink_Limits is record
      --  An HDMI vendor block is present: the sink is HDMI, not DVI.
      HDMI : Boolean := False;
      --  The largest TMDS character rate the sink states (HDMI 1.4 VSDB or
      --  HDMI Forum VSDB), zero when it states none.
      Max_TMDS : Clock_kHz := 0;
      --  HDMI Forum VSDB: the sink supports SCDC (scrambling above 340 MHz).
      SCDC : Boolean := False;
      --  Display range limits descriptor (base block), zero when absent.
      Max_Pixel_Clock : Clock_kHz := 0;
   end record;
   function Limits (Base : Block; Extensions : Block_Array) return Sink_Limits
     with Pre => Extensions'Length <= Extension_Count'Last;
end CuBit.Monitor_EDID.Catalog;
