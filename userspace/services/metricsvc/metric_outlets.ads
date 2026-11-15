pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Messages;
with Metric_Series;
with Metric_Sources;
private with Heap_Arrays;

--  metrics.svc's side of each declared com.cubit-os.stdmetric outlet
--  (CuBit.Metric_Outlet, MET-002): each ring a program opens is accepted
--  here and drained into the series store on every pass. A new series
--  appears the first time a program describes it: no registration. Labels
--  come from the opener (its kernel-stamped process and what its launcher
--  said of it, Metric_Sources), never from the ring; every element is
--  decoded as untrusted.
--
--  metrics.svc never asks to be woken by a publisher (it never arms the
--  rings): it drains every Drain_Interval_Ms while any outlet is open, so a
--  publisher's sample never costs a system call.
--
--  Accounted per outlet: elements malformed, samples for a key no Describe
--  named (or whose series the store refused), and the ring's own shed count
--  (which the producer's Loss elements also report per series).
package Metric_Outlets is
   Drain_Interval_Ms : constant := 100;
   --  Elements taken from one ring per pass.
   Batch_Elements : constant := 4_096;
   --  A ring whose opener the launcher has not named yet waits this long
   --  for the notice before its series are labelled without it.
   Identity_Wait_Ms : constant := 1_000;
   --  Outlets open at once, and keys per outlet (CuBit.Metric_Outlet's).
   Maximum_Outlets : constant := 4_096;

   type Table is limited private;

   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message);
   procedure Close
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Now_Ms : Unsigned_64);
   procedure Ended
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Event : CuBit.Control_Events.Event; Now_Ms : Unsigned_64);
   --  Take what every ring holds into Store. Pending: a ring still holds
   --  elements (a batch's worth was taken).
   procedure Drain
     (T : in out Table; Store : in out Metric_Series.Store; Sources : Metric_Sources.Table;
      Now_Ms : Unsigned_64; Pending : out Boolean);
   function Open_Count (T : Table) return Natural;

   type Totals is record
      Elements, Malformed, Unknown_Key, Ring_Shed, Refused_Outlets : Unsigned_64 := 0;
   end record;
   function Accounting (T : Table) return Totals;
private
   package Key_Blocks is new Heap_Arrays (Metric_Series.Series_Count);
   type Outlet_Record is record
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Authority : Unsigned_64 := 0;
      Link : CuBit.Channels.Channel;
      Keys : Key_Blocks.Block := Key_Blocks.Empty;
      Key_Count : Natural := 0;
      Opened_Ms : Unsigned_64 := 0;
      Shed_Seen : Unsigned_64 := 0;
   end record;
   package Outlet_Blocks is new Heap_Arrays (Outlet_Record);
   type Table is limited record
      Outlets : Outlet_Blocks.Block := Outlet_Blocks.Empty;
      Used : Natural := 0;
      Sums : Totals;
   end record;
end Metric_Outlets;
