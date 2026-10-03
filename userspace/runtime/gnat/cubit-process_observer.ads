pragma Ada_2022;
with Interfaces; use Interfaces;

--  Seeing what runs: procmgr's process-observer role (docs/ccl-console.md,
--  ":ps"). Like log-observer, it is procmgr's own endpoint under a distinct
--  kernel-stamped authority tag, so procmgr answers a listing only for a
--  holder of that tag. A listing joins the kernel's process table with what
--  only procmgr knows: each process's manifest identity, its launcher and
--  when it started.
package CuBit.Process_Observer with Pure, SPARK_Mode is
   Observer_Service_Role : constant Unsigned_64 := 26;
   --  The fixed capability slot a client's manifest binds the role to.
   Observer_Slot : constant Unsigned_64 := 29;
   --  "PROC" in the high half; the low half is a never-reused issuance.
   Observer_Tag_Base : constant Unsigned_64 := 16#5052_4F43_0000_0000#;
   function Observer_Tag (Issuance : Unsigned_32) return Unsigned_64 is
     (Observer_Tag_Base + Unsigned_64 (Issuance));
   --  Tags are kernel-stamped; knowing these numeric values grants nothing.
   function Is_Observer (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Observer_Tag_Base and then
      (Tag and 16#FFFF_FFFF#) /= 0);

   --  List: words (0) a memory grant reference (generation << 32 | slot) of
   --  one writable page the caller owns. procmgr fills it with records and
   --  replies (0) Status, (1) records written, (2) processes in total.
   List_Label : constant := 16#0107#;
   type Status is (OK, Denied, Invalid_Request, Unavailable);
   for Status use (OK => 16#F000#, Denied => 16#F002#, Invalid_Request => 16#F003#,
                   Unavailable => 16#F007#);

   --  One record per process, little-endian:
   --     0  u32 pid               4  u32 launcher pid (0: unknown)
   --     8  u8  state (the kernel's ProcessState position)
   --     9  u8  cpu              10  i16 priority
   --    12  u32 frames (4 KiB pages)
   --    16  u64 started (monotonic ms; 0: unknown)
   --    24  u8  name length      25..40  name (16 bytes)
   --    41  u8  identity length  42..105 identity (64 bytes)
   --   106..127 reserved (zero)
   Record_Bytes : constant := 128;
   Page_Bytes : constant := 4_096;
   Page_Records : constant := Page_Bytes / Record_Bytes;
   Name_Bytes : constant := 16;
   Identity_Bytes : constant := 64;
   Pid_Offset : constant := 0;
   Launcher_Offset : constant := 4;
   State_Offset : constant := 8;
   CPU_Offset : constant := 9;
   Priority_Offset : constant := 10;
   Frames_Offset : constant := 12;
   Started_Offset : constant := 16;
   Name_Length_Offset : constant := 24;
   Name_Offset : constant := 25;
   Identity_Length_Offset : constant := 41;
   Identity_Offset : constant := 42;
end CuBit.Process_Observer;
