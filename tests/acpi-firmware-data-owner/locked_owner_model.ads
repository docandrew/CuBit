pragma Ada_2022;
with Ada.Command_Line;
with Interfaces;
with ACPI_Region_Protocol;
package Locked_Owner_Model is
   package W renames ACPI_Region_Protocol;
   type Peer is new Interfaces.Unsigned_64;
   type Channel_State is null record;
   Destination : constant Peer := 16#8000_0000_0000_0077#;
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Data_Mode : constant Natural := (if Ada.Command_Line.Argument_Count > 1
      then Natural'Value (Ada.Command_Line.Argument (2)) else 0);
   Data_Publishes, Data_Begins, Data_Ends, Data_Reads : Natural := 0;
   procedure Data_Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
      Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery);
   Now : Interfaces.Unsigned_64 := 1;
   Word : Interfaces.Unsigned_32 := 0;
   Checks, Loads, Changes, Acquires, Releases, Queries_Lock, Legacy_Reads : Natural := 0;
   Late_Load : Boolean := False;
   Drop_Termination_Replies : Boolean := Mode = 11;
   IPC_Now : W.Time_Instant := 500;
   procedure Check (Good : Boolean);
   procedure Clock (Value : out Interfaces.Unsigned_64; Available : out Boolean);
   procedure Lease_Clock (Value : out Interfaces.Unsigned_64; Available : out Boolean);
   procedure IPC_Clock (Value : out W.Time_Instant; Available : out Boolean);
   procedure Exchange (Channel : in out Channel_State; Target : Peer; Deadline : W.Time_Instant;
     Request : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery);
   procedure Configure;
end Locked_Owner_Model;
