------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A DNS query (RFC 1035 4.1) for Name's A record of class IN, with
--  recursion desired: the question DNS_Response.Parse accepts answers to.
--
--  Proved (tests/net-tcp): no run-time errors; the message is exactly the
--  12-byte header (ID, RD set, one question), Name as given, then QTYPE A
--  and QCLASS IN.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with DNS_Name;
with DNS_Response;

package DNS_Query with SPARK_Mode is

   subtype Bytes is DNS_Name.Bytes;
   Header_Size     : constant := DNS_Response.Header_Size;
   Question_Tail   : constant := DNS_Response.Question_Tail;
   Maximum_Message : constant := Header_Size + DNS_Name.Maximum_Wire + Question_Tail;
   Recursion_Desired : constant := 16#0100#;

   subtype Message is Bytes (0 .. Maximum_Message - 1);

   procedure Build
     (Id : Unsigned_16; Name : DNS_Name.Wire; Name_Length : DNS_Name.Wire_Length;
      M : out Message; Length : out Natural)
   with
     Pre  => Name_Length >= 1,
     Post => Length = Header_Size + Name_Length + Question_Tail and then
             DNS_Response.U16 (M, 0) = Id and then
             DNS_Response.U16 (M, 2) = Recursion_Desired and then
             DNS_Response.U16 (M, 4) = 1 and then
             DNS_Response.U16 (M, 6) = 0 and then
             (for all K in 0 .. Name_Length - 1 => M (Header_Size + K) = Name (K)) and then
             DNS_Response.U16 (M, Header_Size + Name_Length) = DNS_Response.Type_A and then
             DNS_Response.U16 (M, Header_Size + Name_Length + 2) = DNS_Response.Class_IN;

end DNS_Query;
