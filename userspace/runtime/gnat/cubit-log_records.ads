pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Protocols;

--  Versioned diagnostic records, not trusted audit records or authority.
package CuBit.Log_Records with Pure, SPARK_Mode is
   Maximum_Text_Bytes : constant := 512;
   Header_Bytes : constant := 32;
   subtype Text_Count is Natural range 0 .. Maximum_Text_Bytes;
   subtype Wire_Count is Natural range 0 .. Header_Bytes + Maximum_Text_Bytes;
   type Wire_Buffer is array (Positive range 1 .. Wire_Count'Last)
     of Unsigned_8;
   type Severity is (Trace, Debug, Information, Warning, Error, Critical);
   type Timebase is (Unspecified, Monotonic_Milliseconds, Unix_Milliseconds);
   subtype Clock_Domain is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Timestamp (Clock : Timebase := Unspecified) is record
      case Clock is
         when Unspecified => null;
         when Monotonic_Milliseconds =>
            Domain : Clock_Domain;
            Ticks : Unsigned_64;
         when Unix_Milliseconds =>
            Since_Epoch : Unsigned_64;
      end case;
   end record;
   type Log_Record is private;
   Empty_Record : constant Log_Record;
   type Decoding_Error is
     (Too_Long, Invalid_Text, Truncated, Invalid_Header, Length_Mismatch,
      Invalid_Timestamp);
   type Decoded (Success : Boolean := False) is record
      case Success is
         when False => Reason : Decoding_Error := Invalid_Header;
         when True => Value : Log_Record;
      end case;
   end record;
   --  UTF-8 scalars; reject ASCII controls other than TAB, and reject DEL.
   --  Renderers must still escape text for HTML/terminal/markup contexts.
   function Valid_Text (Text : String) return Boolean;
   function Make
     (Text : String; Level : Severity := Information;
      Reported_Time : Timestamp := (Clock => Unspecified)) return Decoded;
   function Text (Item : Log_Record) return String;
   function Level (Item : Log_Record) return Severity;
   function Reported_Time (Item : Log_Record) return Timestamp;
   procedure Encode
     (Item : Log_Record; Bytes : out Wire_Buffer; Used : out Wire_Count);
   --  Copy stable input bytes before decoding shared untrusted storage.
   --  On failure the discriminant exposes no partially decoded record.
   function Decode (Bytes : Wire_Buffer; Used : Wire_Count) return Decoded;

   Contract : constant CuBit.Protocols.Schema_Contract :=
     (Identity => 16#4355_424C_4F47_0001#, Version => 1,
      Sizing => CuBit.Protocols.Bounded_Size,
      Wire_Size => Unsigned_32 (Wire_Count'Last));
private
   type Log_Record is record
      Content : String (1 .. Maximum_Text_Bytes) :=
        [others => Character'Val (0)];
      Length : Text_Count := 0;
      Importance : Severity := Information;
      Time : Timestamp := (Clock => Unspecified);
   end record;
   Empty_Record : constant Log_Record := (others => <>);
end CuBit.Log_Records;
