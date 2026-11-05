pragma Ada_2022;
with ACPI_Firmware_Data_Replies;
generic
   type Channel is limited private;
   type Carried is private;
   No_Carried : Carried;
   with function Present (Item : Carried) return Boolean;
   with procedure Clock (Now : out ACPI_Firmware_Data_Replies.IPC.Time_Instant;
      Available : out Boolean);
   with procedure Exchange (Connection : in out Channel;
      Deadline : ACPI_Firmware_Data_Replies.IPC.Time_Instant;
      Request : ACPI_Firmware_Data_Replies.IPC.Packet;
      Reply : out ACPI_Firmware_Data_Replies.IPC.Packet;
      Received : out Carried; Delivered : out ACPI_Firmware_Data_Replies.IPC.Delivery);
   -- Plain metadata token. A failed mutating close callback must not cause
   -- loss of the original retained token: the owner calls it on a copy.
   with procedure Dispose (Item : in out Carried; Closed : out Boolean);
package ACPI_Firmware_Data_Transport with SPARK_Mode is
   package P renames ACPI_Firmware_Data_Replies;
   package F renames P.F;
   package IPC renames P.IPC;
   type Outcome is (Completed, Not_Sent, Completion_Unknown, Malformed_Reply,
      Invalid_Request, Invalid_Deadline, Clock_Unavailable, Clock_Reversed,
      Expired, Late_Reply, Cleanup_Pending, Already_Attempted, Reentered);
   type Result is record
      Status : Outcome := Invalid_Request;
      Reply : P.Decoded;
   end record;
   type Context is limited private;
   type Attempt is limited private;
   type Statistics is record
      Working, Cleanup_Due, Clock_Broken : Boolean;
   end record;
   function Observe (Owner : Context) return Statistics;
   -- One finite call per Attempt. No automatic retry of any request and no
   -- reset. A new explicit Query may recover history for cleanup, never resume
   -- an expired/failed AML operation. Width is the original selected datum
   -- width; it bounds Done Read values even on historical Query replies.
   procedure Call (Owner : in out Context; Connection : in out Channel;
      Item : in out Attempt; Request : F.Request; Expected : P.Effect_Kind;
      Width : F.T.Access_Width; Deadline : IPC.Time_Instant; Reply : out Result);
   -- Cleanup only. No RPC, clock sample, Field effect or attempt reset.
   procedure Recover_Cleanup (Owner : in out Context; Closed : out Boolean);
private
   type Attempt is limited record
      Used : Boolean := False;
   end record;
   type Context is limited record
      Working, Cleanup_Due, Seen_Clock, Clock_Broken : Boolean := False;
      Last_Clock : IPC.Time_Instant := 0;
      Held : Carried := No_Carried;
   end record;
end ACPI_Firmware_Data_Transport;
