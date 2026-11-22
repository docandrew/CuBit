pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_Locked_NVS_Protocol;
with ACPI_PCI_Config_Reads;
-- Version-two keyed PCI read protocol. Requests carry no address, mapping,
-- grant or caller scope. Describe returns the named physical window bounds
-- for alias geometry only. Scope comes from authenticated receive metadata.
package ACPI_PCI_Protocol with Pure, SPARK_Mode is
   use Interfaces;
   package Wire renames ACPI_Region_Protocol;
   package P renames ACPI_PCI_Config_Reads;
   use type Wire.Time_Instant;
   use type P.Outcome;
   subtype Packet is Wire.Packet;
   type Operation is (Header, Read, Describe);
   Header_Operation : constant Unsigned_32 := 16#2B0#;
   Read_Operation : constant Unsigned_32 := 16#2B1#;
   Describe_Operation : constant Unsigned_32 := 16#2B2#;
   Describe_Reply : constant Unsigned_32 := 16#F2B2#;
   Header_Reply : constant Unsigned_32 := 16#F2B0#;
   Read_Reply : constant Unsigned_32 := 16#F2B1#;
   Version : constant Unsigned_64 := 2;
   pragma Compile_Time_Error
     (Header_Operation <= ACPI_Locked_NVS_Protocol.First_Operation +
        ACPI_Locked_NVS_Protocol.Operation'Pos (ACPI_Locked_NVS_Protocol.Operation'Last)
      or else Read_Operation /= Header_Operation + 1
      or else Describe_Operation /= Read_Operation + 1
      or else Describe_Operation > 16#2BF#
      or else Describe_Reply /= Describe_Operation + 16#F000#
      or else Header_Reply /= Header_Operation + 16#F000#
      or else Read_Reply /= Read_Operation + 16#F000#
      or else P.T.Access_Width'Pos (P.T.Access_Width'Last) /= 3,
      "PCI labels/width encoding conflict with the version-one layout");
   -- Same machine-monotonic epoch as native microsecond clocks, not the
   -- millisecond IPC timeout. The capCall deadline remains independently due.
   type Microsecond_Deadline is new Unsigned_64;
   No_Finite_Deadline : constant Microsecond_Deadline := Microsecond_Deadline'Last;
   Microseconds_Per_Millisecond : constant := 1_000;
   -- Shared machine-monotonic epoch; ceiling schedules IPC only. Data access
   -- still checks its original microsecond deadline. Overflow stays nonfinite.
   function IPC_Deadline_For (Value : Microsecond_Deadline) return Wire.Time_Instant is
     (if Value = No_Finite_Deadline then Wire.No_Finite_Deadline
      else Wire.Time_Instant (Value / Microseconds_Per_Millisecond)
        + (if Value mod Microseconds_Per_Millisecond = 0 then 0 else 1));
   function Data_Deadline_For (Value : Wire.Time_Instant) return Microsecond_Deadline is
     (if Value = Wire.No_Finite_Deadline or else Unsigned_64 (Value) >
        (Unsigned_64'Last - 1) / Microseconds_Per_Millisecond then No_Finite_Deadline
      else Microsecond_Deadline (Unsigned_64 (Value) * Microseconds_Per_Millisecond));
   type Encoding_Outcome is (Encoded, Missing_Identity, Invalid_Deadline, Rejected_Geometry);
   type Encoded_Request (Status : Encoding_Outcome := Missing_Identity) is record
      case Status is
         when Encoded => Item : Packet;
         when Rejected_Geometry => Reason : P.Outcome;
         when others => null;
      end case;
   end record;
   -- A nonfinite setup deadline permits metadata lookup only. A backend that
   -- must acquire a missing bus requires a finite kernel-millisecond deadline.
   function Header_Request
     (Key : P.Bus_Key := (others => <>);
      Setup_Deadline : Wire.Time_Instant := Wire.No_Finite_Deadline) return Packet;
   function Describe_Request (Epoch, ID : Unsigned_64) return Packet
     with Pre => Epoch /= 0 and then ID /= 0;
   function Valid_Window_Metadata (First, Bytes : Unsigned_64) return Boolean is
     (Bytes = P.Bus_Bytes and then First mod P.Function_Bytes = 0
      and then Bytes - 1 <= Unsigned_64'Last - First);
   function Read_Request
     (Epoch, ID : Unsigned_64; Region : P.Region; Offset : Unsigned_64;
      Width : P.T.Access_Width; Deadline : Microsecond_Deadline;
      For_Write : Boolean := False; Key : P.Bus_Key := (others => <>)) return Encoded_Request;
   type Request is record
      Valid : Boolean := False;
      Kind : Operation := Header;
      Epoch, ID : Unsigned_64 := 0;
      Key : P.Bus_Key;
      Setup_Deadline : Wire.Time_Instant := Wire.No_Finite_Deadline;
      Region : P.Region;
      Offset : Unsigned_64 := 0;
      Width : P.T.Access_Width := P.T.Byte_Access;
      Deadline : Microsecond_Deadline := 0;
      For_Write : Boolean := False;
   end record;
   -- Framing/representation validity only. The backend independently checks
   -- region/width/write/deadline policy and current authority for each request.
   function Decode_Request (Item : Packet) return Request;
   function Owned_Label (Label : Unsigned_32) return Boolean is
     (Label in Header_Operation | Read_Operation | Describe_Operation);
   type Outcome is (Denied, Malformed, Stale, Unavailable, Ready, Done,
     Policy_Refused, Deadline_Expired, Clock_Unavailable, Clock_Reversed, Fault, Reentered, Not_Ready, Backing_Unavailable, Catalog_Changed);
   function Header_Result (Epoch, ID : Unsigned_64) return Packet
     with Pre => Epoch /= 0 and then ID /= 0;
   function Describe_Result (First, Bytes : Unsigned_64) return Packet
     with Pre => Valid_Window_Metadata (First, Bytes);
   function Read_Result (Value : Unsigned_64) return Packet;
   function Error_Reply (Kind : Operation; Status : Outcome;
      Reason : P.Outcome := P.Invalid_Window) return Packet
     with Pre => Status not in Ready | Done
       and then (if Status = Policy_Refused then Kind = Read and then Reason /= P.Read_Ready);
   type Reply is record
      Valid : Boolean := False;
      Status : Outcome := Malformed;
      Epoch, ID, Value : Unsigned_64 := 0;
      First, Bytes : Unsigned_64 := 0;
      Has_Reason : Boolean := False;
      Reason : P.Outcome := P.Invalid_Window;
   end record;
   function Decode_Reply
     (Item : Packet; Kind : Operation; Width : P.T.Access_Width := P.T.Byte_Access) return Reply;
private
   Function_Bits : constant := 8;
   Base_Shift : constant := Function_Bits;
   Length_Shift : constant := 20;
   Offset_Shift : constant := 33;
   Width_Shift : constant := 45;
   Write_Shift : constant := 47;
   Version_Shift : constant := 56;
   Function_Mask : constant Unsigned_64 := 16#FF#;
   Base_Mask : constant Unsigned_64 := 16#FFF#;
   Length_Mask : constant Unsigned_64 := 16#1FFF#;
   Reserved_Mask : constant Unsigned_64 := 16#00FF_0000_0000_0000#;
end ACPI_PCI_Protocol;
