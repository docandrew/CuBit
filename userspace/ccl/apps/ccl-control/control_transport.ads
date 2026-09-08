with Interfaces;

--  Native grant-backed TCP adapter. The framed CCL protocol is elsewhere.
--  Deliberately one bounded development connection, not a socket API.
package Control_Transport is
   procedure Open (Network_Process : out Interfaces.Unsigned_64; Success : out Boolean);
   procedure Read_Exact (Data : out String; Success : out Boolean);
   procedure Write_All (Data : String; Success : out Boolean);
   procedure Close;
end Control_Transport;
