------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A DNS response (RFC 1035 4.1) to one question for an A record of class
--  IN: its ID, the question's name (DNS_Name.Read_Name: plain labels,
--  bounded), and the first answer that is an A record of class IN with
--  four bytes of data. Answers before it (CNAME chains) are skipped; a
--  compressed name is skipped without being followed. Anything that runs
--  past the message ends the walk with no address.
--
--  Proved (tests/net-tcp): no access outside the message and the walk ends,
--  for any bytes; an accepted message is a response (QR set) with one
--  question for A/IN, and R.Id and R.Rcode are its ID and response code; a returned address is the four
--  data bytes of an A/IN answer of length four, inside the message.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with DNS_Name;

package DNS_Response with SPARK_Mode is

   subtype Bytes is DNS_Name.Bytes;

   Header_Size     : constant := 12;
   Maximum_Message : constant := 65_535;
   Response_Flag   : constant := 16#8000#;   --  QR
   Type_A          : constant := 1;
   Class_IN        : constant := 1;
   IPv4_Length     : constant := 4;
   Question_Tail   : constant := 4;          --  QTYPE, QCLASS
   Answer_Fixed    : constant := 10;         --  TYPE, CLASS, TTL, RDLENGTH
   Maximum_Label   : constant := 63;
   Pointer_Bits    : constant := 16#C0#;
   Rcode_Bits      : constant := 16#000F#;

   --  RFC 1035 4.1.1 response codes.
   subtype Response_Code is Unsigned_8 range 0 .. 15;
   No_Error       : constant Response_Code := 0;
   Server_Failure : constant Response_Code := 2;
   Name_Error     : constant Response_Code := 3;   --  NXDOMAIN
   Refused        : constant Response_Code := 5;

   subtype Message_Length is Natural range 0 .. Maximum_Message;
   type IPv4 is array (0 .. 3) of Unsigned_8;

   type Response is record
      Id          : Unsigned_16 := 0;
      Rcode       : Response_Code := No_Error;
      Name        : DNS_Name.Wire := [others => 0];
      Name_Length : DNS_Name.Wire_Length := 0;
      Has_Address : Boolean := False;
      Address     : IPv4 := [others => 0];
      Address_At  : Message_Length := 0;
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   procedure Parse (Message : Bytes; R : out Response; OK : out Boolean) with
     Pre  => Message'First = 0 and then Message'Length <= Maximum_Message,
     Post => (if OK then
                Message'Length >= Header_Size and then
                (U16 (Message, 2) and Response_Flag) /= 0 and then
                U16 (Message, 4) = 1 and then
                R.Id = U16 (Message, 0) and then
                R.Rcode = Response_Code (U16 (Message, 2) and Rcode_Bits) and then
                R.Name_Length >= 1) and then
             (if OK and then R.Has_Address then
                R.Address_At >= Header_Size and then
                R.Address_At + IPv4_Length <= Message'Length and then
                U16 (Message, R.Address_At - Answer_Fixed) = Type_A and then
                U16 (Message, R.Address_At - Answer_Fixed + 2) = Class_IN and then
                U16 (Message, R.Address_At - 2) = IPv4_Length and then
                (for all K in IPv4'Range => R.Address (K) = Message (R.Address_At + K)));

end DNS_Response;
