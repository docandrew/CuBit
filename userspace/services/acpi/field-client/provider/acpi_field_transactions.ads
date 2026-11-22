pragma Ada_2022;
generic
   type IO_Context is limited private;
   -- These actuals act on the same retained composite provider context.
   -- Physical owns the existing whole-Field guard; Data owns only scoped
   -- firmware-data admission. Callbacks return normally and never reenter.
   with procedure Physical_Begin (IO : in out IO_Context;
      Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   with procedure Data_Begin (IO : in out IO_Context;
      Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   with procedure Data_End (IO : in out IO_Context; Accepted : in out Boolean);
   with procedure Physical_End (IO : in out IO_Context; Accepted : in out Boolean);
   with procedure Data_Cleanup (IO : in out IO_Context; Accepted : out Boolean);
   with procedure Physical_Cleanup (IO : in out IO_Context; Accepted : out Boolean);
-- Hook sequencing only. No target catalogue, new lock owner, RPC, deadline
-- refresh, arena alias or allocation. Original cleanup/uncertainty state
-- remains in IO. One retained Context per composite provider lifetime.
package ACPI_Field_Transactions with SPARK_Mode is
   type Depth_Count is new Natural;
   type Context is limited private;
   type Statistics is record
      Depth : Depth_Count;
      Working, Stopped, Data_Cleanup_Required, Physical_Cleanup_Required : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   function Pending (Owner : Context) return Boolean;
   procedure Begin_Field (Owner : in out Context; IO : in out IO_Context;
      Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   -- Every accepted Begin has exactly one End, even after failure. The data
   -- lane closes first; physical End still runs when data End refuses.
   procedure End_Field (Owner : in out Context; IO : in out IO_Context;
      Accepted : in out Boolean);
   -- Explicit stop, not a way to resume AML. At depth0 attempt both cleanup
   -- owners in order, even if data cleanup fails. Never pop a live parent to
   -- clean up a child whose Begin was refused. Call again only explicitly.
   procedure Cleanup (Owner : in out Context; IO : in out IO_Context; Accepted : out Boolean);
private
   type Context is limited record
      Depth : Depth_Count := 0;
      Working, Stopped, Data_Due, Physical_Due : Boolean := False;
   end record;
end ACPI_Field_Transactions;
