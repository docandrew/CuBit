------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The steps of an install (SYS-001 installer v2, SYS-002, SYS-003), in the
--  only order Layout_Writer takes them. Each is a typed state an install
--  reports as its progress; a failed step ends the install there, and the
--  install is restartable from the start (every step rewrites what it
--  needs; nothing on the disk is trusted).
--
--    Checking            the request, the disk's identity, the payload
--    Partitioning        the ESP's and the root's first blocks zeroed (so
--                        neither is a filesystem until its last step), then
--                        the GPT, read back
--    Formatting_Root     the root's ext2 metadata (its group-0 superblock,
--                        which makes it a filesystem, is written last)
--    Filling_Storehouse  every file: the image closure, and every component
--                        once more by content, storehouse/executables/<sha256>
--    Writing_Generation  storehouse/profiles/system/1 (generation 1: the
--                        installed image), then .../current naming it
--    Verifying_Root      every block written, read back: metadata compared,
--                        each file's bytes hashed and compared with what was
--                        read from the source
--    Writing_ESP         the ESP's FATs, directories and boot files, without
--                        its boot sector (not a filesystem yet)
--    Verifying_ESP       the same, read back
--    Making_Bootable     after a device flush: FSInfo, the backup boot
--                        sector, then the boot sector last (read back)
--    Flushing            the device's cache made durable
--    Done                installed
--
--  SYS-002: a disk is bootable only once Making_Bootable has written its
--  last sector, and that step starts only after both verifications passed
--  and a flush: an install interrupted at any earlier step leaves a disk
--  the firmware cannot boot (its ESP has no boot sector).
------------------------------------------------------------------------------
pragma Ada_2022;

package Install_Steps with SPARK_Mode, Pure is

   type Step is
     (Checking, Partitioning, Formatting_Root, Filling_Storehouse, Writing_Generation,
      Verifying_Root, Writing_ESP, Verifying_ESP, Making_Bootable, Flushing, Done);
   for Step use
     (Checking => 0, Partitioning => 1, Formatting_Root => 2, Filling_Storehouse => 3,
      Writing_Generation => 4, Verifying_Root => 5, Writing_ESP => 6, Verifying_ESP => 7,
      Making_Bootable => 8, Flushing => 9, Done => 10);

   --  The disk may boot only after this step completed.
   Bootable_After : constant Step := Making_Bootable;

   function Rank (Item : Step) return Natural is (Step'Pos (Item));

   --  Steps are taken in declaration order only.
   function Follows (Later, Earlier : Step) return Boolean is (Rank (Later) > Rank (Earlier));

   --  Whether a disk an install stopped during (or after) Reached can boot.
   function May_Boot (Reached : Step; Completed : Boolean) return Boolean is
     (Reached = Done or else (Reached = Flushing) or else (Reached = Making_Bootable and then Completed));

   --  Stable names for the log and the CCL Install_Step enumeration.
   function Name (Item : Step) return String is
     (case Item is
        when Checking => "checking", when Partitioning => "partitioning",
        when Formatting_Root => "formatting the root", when Filling_Storehouse => "filling the storehouse",
        when Writing_Generation => "writing generation 1", when Verifying_Root => "verifying the root",
        when Writing_ESP => "writing the ESP", when Verifying_ESP => "verifying the ESP",
        when Making_Bootable => "making the disk bootable", when Flushing => "flushing",
        when Done => "done");

end Install_Steps;
