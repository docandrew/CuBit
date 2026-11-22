pragma Ada_2022;
with ACPI_Firmware_Data_Frames;
package ACPI_Firmware_Data_Replies with Pure, SPARK_Mode is
   package F renames ACPI_Firmware_Data_Frames;
   package IPC renames F.IPC;
   subtype Word is F.Word;
   use type Word;
   type Effect_Kind is (No_Effect, Read_Effect, Write_Effect);
   -- Wire statuses, independent of private owner enumeration positions.
   type Outcome is (Ready, Pending, Refused, Stopped, Capacity_Refused,
      Invalid_Request, Request_Changed, Not_Found, Busy, Denied, Closed,
      Clock_Unavailable, Clock_Reversed, Expired, Lease_Refused,
      Selection_Refused, Running, Done, Indeterminate, No_Evidence, Backend_Fault);
   Code_Bits : constant := 8;
   Code_Mask : constant Word := 2 ** Code_Bits - 1;
   Used_Status_Bits : constant := 2 * Code_Bits;
   pragma Compile_Time_Error
      (Outcome'Pos (Outcome'Last) >= 2 ** Code_Bits
       or else Effect_Kind'Pos (Effect_Kind'Last) >= 2 ** Code_Bits
       or else Used_Status_Bits >= Word'Size,
       "Firmware data reply codes exceed their wire fields");
   type Reply is record
      Kind : F.Operation := F.Publish;
      Correlation, Auxiliary : F.Identifier := F.No_ID;
      Status : Outcome := Invalid_Request;
      Effect : Effect_Kind := No_Effect;
      Value : Word := 0;
   end record;
   -- Four words: correlation, auxiliary, status/effect, value. Setup Ready
   -- carries an opaque server record/scope/descriptor ID. Only Done reads
   -- carry data; every write and non-success value is zero. Select correlates
   -- client descriptor ID as auxiliary; data operations correlate operation ID.
   function Valid (Item : Reply) return Boolean;
   function Encode (Item : Reply) return IPC.Packet with Pre => Valid (Item);
   type Decoded (Valid : Boolean := False) is record
      case Valid is
         when True => Item : Reply;
         when False => null;
      end case;
   end record;
   -- Expected_Effect is the original operation's kind, including for Query.
   -- It is not inferred from history, status or a zero-valued result. Setup
   -- requests use No_Effect. Unknown/malformed completion never permits retry.
   function Decode (Item : IPC.Packet; Request : F.Request;
      Expected_Effect : Effect_Kind; Has_Capability : Boolean) return Decoded;
end ACPI_Firmware_Data_Replies;
