pragma Ada_2022;
with Interfaces; use Interfaces;

--  Pure SNTPv4 client logic (RFC 4330, RFC 5905 packet format): request
--  construction, reply validation and the time estimate from one exchange,
--  plus agreement across servers. No IO; the caller supplies monotonic
--  timestamps in milliseconds and the reply bytes.
package SNTP with SPARK_Mode is
   Packet_Length : constant := 48;
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Packet is Byte_Array (1 .. Packet_Length);

   --  Seconds from the NTP epoch (1900) to the Unix epoch (1970).
   Unix_Offset : constant := 2_208_988_800;
   --  The latest UTC the clock service accepts (end of 2099), in ms.
   Latest_UTC_MS : constant := 4_102_444_799_999;
   --  A reply whose server-side error bound exceeds this is not used.
   Maximum_Root_Distance_MS : constant := 1_000;
   --  Round trips longer than this are not used.
   Maximum_Round_Trip_MS : constant := 2_000;

   subtype UTC_Milliseconds is Unsigned_64 range 0 .. Latest_UTC_MS;

   --  Client request: LI 0, version 4, mode 3 (client), all other fields
   --  zero except the transmit timestamp, which carries an unpredictable
   --  nonce. The server must echo it as the origin timestamp.
   function Request (Nonce : Unsigned_64) return Packet;

   type Rejection is
     (Accepted, Wrong_Length, Wrong_Mode, Wrong_Version, Kiss_Of_Death,
      Bad_Stratum, Unsynchronized, Origin_Mismatch, Zero_Timestamp,
      Server_Time_Order, Local_Time_Order, Round_Trip_Too_Long,
      Root_Distance_Too_Large, Out_Of_Range);

   --  The runtime discards enumeration names, so 'Image gives positions.
   function Name (Value : Rejection) return String is
     (case Value is
         when Accepted => "accepted",
         when Wrong_Length => "wrong length",
         when Wrong_Mode => "wrong mode",
         when Wrong_Version => "wrong version",
         when Kiss_Of_Death => "kiss-o'-death",
         when Bad_Stratum => "bad stratum",
         when Unsynchronized => "server unsynchronized",
         when Origin_Mismatch => "origin mismatch",
         when Zero_Timestamp => "zero timestamp",
         when Server_Time_Order => "server timestamps out of order",
         when Local_Time_Order => "local timestamps out of order",
         when Round_Trip_Too_Long => "round trip too long",
         when Root_Distance_Too_Large => "root distance too large",
         when Out_Of_Range => "time out of range");

   --  One exchange's estimate: UTC as it was at the local monotonic instant
   --  Observed_MS, within +/- Uncertainty_MS.
   type Estimate is record
      UTC_MS : UTC_Milliseconds := 0;
      Observed_MS : Unsigned_64 := 0;
      Uncertainty_MS : Unsigned_32 := 0;
   end record;

   --  Sent_MS and Received_MS are local monotonic readings taken just
   --  before sending and just after receiving.
   procedure Evaluate
     (Reply : Byte_Array; Nonce : Unsigned_64; Sent_MS, Received_MS : Unsigned_64;
      Result : out Estimate; Status : out Rejection)
   with
     Post =>
       (if Status = Accepted then
          Reply'Length = Packet_Length and then
          Result.Observed_MS = Received_MS and then
          Received_MS >= Sent_MS and then
          Received_MS - Sent_MS <= Maximum_Round_Trip_MS and then
          Result.Uncertainty_MS <=
            Maximum_Round_Trip_MS + Maximum_Root_Distance_MS + 1);

   --  NTP 64-bit timestamp (32.32 seconds since 1900) to Unix milliseconds.
   --  Era 0 values below 2^31 seconds (before 1968) are read as era 1
   --  (after February 2036), the RFC 4330 convention valid until 2104.
   procedure To_Unix_MS
     (Timestamp : Unsigned_64; Result : out UTC_Milliseconds; Success : out Boolean);

   Maximum_Servers : constant := 4;
   subtype Server_Count is Natural range 0 .. Maximum_Servers;
   type Estimate_Array is array (1 .. Maximum_Servers) of Estimate;

   --  Agreement across Count estimates (Marzullo's algorithm). Each estimate
   --  is an interval [UTC - U, UTC + U] projected to the latest observation.
   --  Succeeds when a strict majority of at least two intervals share a
   --  point; the result is the midpoint of their intersection, with its
   --  half-width as uncertainty, and Agreeing is that majority's size.
   procedure Combine
     (Items : Estimate_Array; Count : Server_Count;
      Result : out Estimate; Agreeing : out Server_Count; Success : out Boolean)
   with
     Post =>
       (if Success then
          Agreeing >= 2 and then Agreeing <= Count and then 2 * Agreeing > Count);
end SNTP;
