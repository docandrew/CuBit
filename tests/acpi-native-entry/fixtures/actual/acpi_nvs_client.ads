pragma Ada_2022;
with Interfaces;
with ACPI_NVS_Protocol;
generic
   type Transport is limited private;
   type Peer is private;
   No_Peer : Peer;
   with procedure Read_Clock
     (Now : out ACPI_NVS_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Channel : in out Transport; Destination : Peer;
      Deadline : ACPI_NVS_Protocol.Wire.Time_Instant;
      Item : ACPI_NVS_Protocol.Packet; Reply : out ACPI_NVS_Protocol.Packet;
      Delivered : out ACPI_NVS_Protocol.Wire.Delivery);
package ACPI_NVS_Client with SPARK_Mode is
   package P renames ACPI_NVS_Protocol;
   subtype Word is Interfaces.Unsigned_64;
   subtype Tick is P.Wire.Time_Instant;
   Maximum_Records : constant Positive := 16;
   subtype Record_Count is Natural range 0 .. Maximum_Records;
   type Phase is (Fresh, Loading, Ready, Failed, Uncertain);
   type Outcome is (Loaded, Read_Complete, Not_Ready, Already_Consumed,
     Invalid_Peer, Invalid_Deadline, Expired, Clock_Unavailable, Clock_Reversed,
     Not_Sent, Completion_Unknown, Bad_Reply, Backend_Refused, Metadata_Changed,
     Invalid_Metadata, Missing_Record, Out_Of_Range, Reentered);
   type Description is record
      ID, Base, Length : Word := 0;
   end record;
   type Context is limited private;
   type Statistics is record
      Stage : Phase;
      Count : Record_Count;
      Working : Boolean;
   end record;
   function Observe (State : Context) return Statistics;
   -- One peer for the lifetime of this context. Admit the entire bounded
   -- snapshot and reconfirm its epoch/count before publishing any metadata.
   -- All calls use the caller's same finite absolute millisecond deadline.
   procedure Load
     (State : in out Context; Channel : in out Transport; Destination : Peer;
      Deadline : Tick; Result : out Outcome);
   procedure Describe
     (State : Context; Ordinal : Natural; Item : out Description; Available : out Boolean);
   procedure Read
     (State : in out Context; Channel : in out Transport; ID, Offset : Word;
      Width : P.T.Access_Width; Deadline : Tick; Value : out Word; Result : out Outcome);
   -- Unknown completion/malformed replies cannot replay, refresh or retarget.
   -- The transport/clock callbacks return normally and never alias state.
private
   subtype Row_Index is Positive range 1 .. Maximum_Records;
   type Rows is array (Row_Index) of Description;
   type Context is limited record
      Stage : Phase := Fresh;
      Destination : Peer := No_Peer;
      Epoch : Word := 0;
      Count : Record_Count := 0;
      Items : Rows;
      Working, Seen_Clock : Boolean := False;
      Last_Clock : Tick := 0;
   end record;
end ACPI_NVS_Client;
