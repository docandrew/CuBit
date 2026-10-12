pragma Ada_2022;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with CuBit.Child_Exits;

--  The processes devmgr started (drivers and services), by name, so their
--  exits can be logged with it: the kernel reports a child's exit to its
--  parent (CuBit.Process_Events), and an unread report also keeps the PID
--  from reuse. A full table forgets nothing already kept; later children
--  are counted and their exits still taken.
package Devmgr_Children is
   Capacity   : constant := 48;
   Name_Bytes : constant := 16;
   subtype Child_Name is String (1 .. Name_Bytes);

   procedure Remember (Child : Process_ID; Name : String);

   --  The next remembered child whose exit has arrived (polls the kernel's
   --  event lane), then forgotten. Found False when none.
   procedure Take_Next_Exit
     (Child : out Process_ID; Name : out Child_Name; Length : out Natural;
      Report : out CuBit.Child_Exits.Report; Found : out Boolean);

   --  Children started while the table was full (their names are lost).
   function Unnamed return Natural;
end Devmgr_Children;
