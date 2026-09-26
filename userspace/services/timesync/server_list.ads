pragma Ada_2022;
with Interfaces; use Interfaces;
with SNTP;

--  Parses the time.servers setting: up to four space-separated entries, each
--  "host" or "host:port" (port 1..65535, default 123). Hosts are DNS names or
--  dotted IPv4 literals: letters, digits, '.', '-' only, 1..63 characters.
--  Netstack enforces the manifest's scope; this only rejects malformed text.
package Server_List with SPARK_Mode is
   Maximum_Host_Length : constant := 63;
   Default_Port : constant Unsigned_16 := 123;
   subtype Host_Length is Natural range 0 .. Maximum_Host_Length;

   type Server is record
      Host : String (1 .. Maximum_Host_Length) := [others => ' '];
      Length : Host_Length := 0;
      Port : Unsigned_16 := Default_Port;
   end record;
   type Server_Array is array (1 .. SNTP.Maximum_Servers) of Server;

   --  Fails on any malformed entry or more than Maximum_Servers entries,
   --  rather than silently using part of the list.
   procedure Parse
     (Text : String; Servers : out Server_Array;
      Count : out SNTP.Server_Count; Success : out Boolean)
   with
     Pre => Text'Last < Integer'Last,
     Post =>
       (if Success then Count >= 1 and then
          (for all I in 1 .. Count =>
             Servers (I).Length >= 1 and then Servers (I).Port >= 1));

   --  "@net:udp:HOST:PORT" for netstack's NET_OPEN.
   Maximum_Scheme_Length : constant := 9 + Maximum_Host_Length + 1 + 5;
   procedure Scheme
     (Item : Server; Text : out String; Length : out Natural)
   with
     Pre => Text'First = 1 and then Text'Length = Maximum_Scheme_Length,
     Post => Length <= Maximum_Scheme_Length;
end Server_List;
