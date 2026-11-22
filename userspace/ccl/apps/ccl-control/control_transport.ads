with Interfaces;

--  Native grant-backed TCP adapter. HTTP and CBOR parsing live elsewhere.
--  Deliberately one bounded development connection, not a socket API.
package Control_Transport is
   procedure Listen (Network_Process : out Interfaces.Unsigned_64; Success : out Boolean);
   procedure Accept_Connection
     (Deadline : Interfaces.Unsigned_64; Success : out Boolean);
   procedure Read_Some
     (Data : out String; Count : out Natural;
      Deadline : Interfaces.Unsigned_64; Success : out Boolean);
   procedure Write_All (Data : String; Success : out Boolean);
   procedure Close;
   procedure Close_Connection;
end Control_Transport;
