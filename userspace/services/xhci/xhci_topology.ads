with Interfaces; use Interfaces;

-- USB route/TT identity only; slots must separately be live and owned.
package XHCI_Topology with SPARK_Mode is
   type Speed is (Full_Speed, Low_Speed, High_Speed, Super_Speed);
   for Speed use (Full_Speed => 1, Low_Speed => 2,
                  High_Speed => 3, Super_Speed => 4);
   subtype Port_Number is Positive range 1 .. 255;
   subtype Slot_Number is Positive range 1 .. 255;
   type Path is private;
   type Attach_Result is (Attached, Depth_Exceeded, Unsupported_Speed);

   function Root (Port : Port_Number; Rate : Speed) return Path;
   function Root_Port (Item : Path) return Port_Number;
   function Device_Speed (Item : Path) return Speed;
   function Route_String (Item : Path) return Unsigned_32
     with Post => Route_String'Result <= 16#F_FFFF#;
   function TT_Context (Item : Path) return Unsigned_32
     with Post => TT_Context'Result <= 16#FFFF#;

   -- Parent is an enumerated hub. Parent_Slot is its controller slot ID.
   -- This initial child path supports USB2 hubs only. Parent TT identity is
   -- inherited through intervening full-speed hubs, not replaced by them.
   procedure Child
     (Parent : Path; Parent_Slot : Slot_Number; Port : Port_Number;
      Rate : Speed; Item : out Path; Result : out Attach_Result)
     with Post =>
       (if Result = Attached then Root_Port (Item) = Root_Port (Parent)
        and then Device_Speed (Item) = Rate);
private
   subtype Route_Depth is Natural range 0 .. 5;
   type Route_Ports is array (Positive range 1 .. 5) of Natural range 0 .. 15;
   type Path is record
      Root : Port_Number := 1;
      Rate : Speed := Full_Speed;
      Depth : Route_Depth := 0;
      Ports : Route_Ports := [others => 0];
      TT_Slot, TT_Port : Natural range 0 .. 255 := 0;
   end record;
end XHCI_Topology;
