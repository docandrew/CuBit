with Interfaces; use Interfaces;
with CCL.Device_Sections;

--  An image's driver index, devices.index in its bootstrap archive (HW-002,
--  docs/device-matching.md): every driver in the image by file name, with
--  the devices it drives (its .cubit.devices section). ccl-device-index
--  writes it from the drivers' own sections when the image is built and
--  refuses an ambiguous image; devmgr reads it at boot and binds each
--  present device to at most one driver. Decode treats the bytes as
--  untrusted and accepts exactly what Encode writes.
package CCL.Device_Index with Pure, SPARK_Mode => On is
   package DS renames CCL.Device_Sections;

   MAGIC : constant := 16#4944_4243#;   --  "CBDI" little-endian
   VERSION : constant := 1;
   HEADER_BYTES : constant := 8;
   NAME_BYTES : constant := 32;
   --  A record: the name, NUL-padded; the section length (u16) and two
   --  reserved bytes; the section, zero-padded to its maximum.
   RECORD_PREFIX_BYTES : constant := NAME_BYTES + 4;
   RECORD_BYTES : constant := RECORD_PREFIX_BYTES + DS.MAX_SECTION_BYTES;
   MAX_DRIVERS : constant := 32;
   MAX_INDEX_BYTES : constant := HEADER_BYTES + MAX_DRIVERS * RECORD_BYTES;

   subtype Name_Length is Natural range 0 .. NAME_BYTES;
   type Driver_Name is record
      Length : Name_Length := 0;
      Text   : String (1 .. NAME_BYTES) := [others => ASCII.NUL];
   end record;

   --  A file name in the image: 1 .. 32 of [a-z0-9._-], NUL-padded.
   function Valid_Name (N : Driver_Name) return Boolean is
     (N.Length >= 1
      and then (for all I in 1 .. NAME_BYTES =>
                  (if I <= N.Length then N.Text (I) in 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-'
                   else N.Text (I) = ASCII.NUL)));

   type Driver is record
      Name    : Driver_Name;
      Matches : DS.Match_Table;
   end record;

   subtype Driver_Count is Natural range 0 .. MAX_DRIVERS;
   subtype Driver_Number is Positive range 1 .. MAX_DRIVERS;
   type Driver_Array is array (Driver_Number) of Driver;
   type Index is record
      Count   : Driver_Count := 0;
      Drivers : Driver_Array := [others => (others => <>)];
   end record;

   function Valid_Index (I : Index) return Boolean is
     (for all D in 1 .. I.Count =>
        Valid_Name (I.Drivers (D).Name) and then DS.Valid_Table (I.Drivers (D).Matches));

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Index_Length is Natural range 0 .. MAX_INDEX_BYTES;
   subtype Index_Bytes is Byte_Array (1 .. MAX_INDEX_BYTES);

   procedure Encode (I : Index; Bytes : out Index_Bytes; Length : out Index_Length)
   with Pre  => Valid_Index (I),
        Post => Length = HEADER_BYTES + I.Count * RECORD_BYTES;

   type Decode_Status is (Decoded, Invalid_Length, Invalid_Header, Invalid_Record);
   --  Explicit text: native runtimes discard enumeration names.
   function Status_Name (S : Decode_Status) return String is
     (case S is
        when Decoded => "decoded", when Invalid_Length => "invalid length",
        when Invalid_Header => "invalid header", when Invalid_Record => "invalid record");

   procedure Decode (Data : Byte_Array; I : out Index; Status : out Decode_Status)
   with Pre  => Data'First = 1 and then Data'Length <= MAX_INDEX_BYTES,
        Post => (if Status = Decoded then Valid_Index (I));

   --  Which driver a PCI function binds (docs/device-matching.md
   --  "Precedence"): the one driver whose best entry ranks highest. Two
   --  drivers sharing the highest rank are Ambiguous and neither binds.
   type Binding_Kind is (Unbound, Bound, Ambiguous);
   type Binding is record
      Kind     : Binding_Kind := Unbound;
      --  Bound: the driver and its entry; Ambiguous: the first two drivers.
      Driver   : Driver_Count := 0;
      At_Entry : DS.Entry_Count := 0;
      Rival    : Driver_Count := 0;
   end record;

   procedure Bind (I : Index; F : DS.PCI_Function; Result : out Binding)
   with Pre  => Valid_Index (I),
        Post => (case Result.Kind is
                   when Unbound => Result.Driver = 0,
                   when Bound =>
                      Result.Driver in 1 .. I.Count
                      and then Result.At_Entry in 1 .. I.Drivers (Result.Driver).Matches.Count
                      and then DS.Rank_Of (I.Drivers (Result.Driver).Matches.Entries
                                             (Result.At_Entry), F) /= DS.No_Rank,
                   when Ambiguous =>
                      Result.Driver in 1 .. I.Count and then Result.Rival in 1 .. I.Count
                      and then Result.Driver /= Result.Rival);

   --  The first pair of distinct drivers with Ambiguous entries
   --  (DS.Ambiguous), or Found False when the index is unambiguous.
   procedure First_Ambiguity
     (I : Index; Found : out Boolean; Left, Right : out Driver_Count;
      Left_Entry, Right_Entry : out DS.Entry_Count)
   with Pre  => Valid_Index (I),
        Post => (if Found then Left in 1 .. I.Count and then Right in 1 .. I.Count
                               and then Left /= Right);

   --  The driver named Name, or 0.
   function Find (I : Index; Name : String) return Driver_Count
   with Pre  => I.Count <= MAX_DRIVERS,
        Post => Find'Result <= I.Count;
end CCL.Device_Index;
