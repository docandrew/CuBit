------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Path names to CuBit names (docs/self-hosting.md, item 2): what a program
--  writes ("src/a.c", "/tls/x", "@usb:0/y") to the name the filesystem
--  service is sent ("@nvme:0/src/a.c").
--
--  @description
--  A CuBit name is a volume ("@" and its name, no '/') then "/component"s;
--  a volume's root is the volume and one '/'. A path starting with '@'
--  names its volume, one starting with '/' is on System_Volume, and any
--  other starts from a base (the working directory, or an open directory
--  for the *at calls), itself resolved the same way. "." and empty
--  components are dropped; ".." removes the component before it but never
--  the volume (POSIX: "/.." is "/"). That is exact: there are no symbolic
--  links.
--
--  Proved (gnatprove, tests/path-names): a resolved name starts with '@',
--  keeps the selected volume, fits Maximum_Name_Bytes, and has no ".."
--  component, whatever the input. The filesystem service refuses ".."
--  as well; this is the first of the two checks, not the only one.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Path_Names with Pure, SPARK_Mode is

   --  The longest name the filesystem service takes
   --  (CuBit.Filesystems.MAXIMUM_PATH_BYTES; tests/path-names checks).
   Maximum_Name_Bytes : constant := 4096;
   --  The longest path accepted as input (Linux's PATH_MAX); ".." may make
   --  a long path resolve to a short name.
   Maximum_Path_Bytes : constant := 4096;

   System_Volume : constant String := "@nvme:0";  --  "@system" later
   Volume_Mark   : constant Character := '@';
   Separator     : constant Character := '/';
   Dot           : constant Character := '.';

   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   subtype Name is String (Name_Index);

   type Resolution is
     (Resolved,
      Empty_Path,       --  "" names nothing (ENOENT)
      Too_Long,         --  the name would not fit (ENAMETOOLONG)
      Invalid_Base);    --  a relative path's base is not a CuBit name

   --  The ".." component check: no separator in Item (Root + 1 .. Last) is
   --  followed by exactly "..".
   function No_Parent_Component
     (Item : String; Root, Last : Natural) return Boolean is
     (for all I in Root + 1 .. Last =>
        (if Item (I) = Separator and then I <= Last - 2 then
           not (Item (I + 1) = Dot and then Item (I + 2) = Dot
                and then (I + 2 = Last or else Item (I + 3) = Separator))))
   with Ghost,
        Pre => Item'First = 1 and then Item'Last <= Maximum_Name_Bytes
               and then Last <= Item'Last and then Root <= Last;

   --  Base is used only when Path is relative. Result (1 .. Length) is the
   --  name when Status = Resolved; Length < Capacity leaves room for a C
   --  string's NUL.
   procedure Resolve
     (Base, Path : String; Capacity : Name_Length;
      Result : out Name; Length : out Name_Length; Status : out Resolution)
   with Pre => Base'First = 1 and then Base'Length <= Maximum_Path_Bytes
               and then Path'First = 1 and then Path'Length <= Maximum_Path_Bytes,
        Post => (if Status = Resolved then
                   Length in 2 .. Capacity - 1
                   and then Result (1) = Volume_Mark
                   and then (for some Root in 1 .. Length - 1 =>
                               Result (Root + 1) = Separator
                               and then (for all K in 1 .. Root =>
                                           Result (K) /= Separator)
                               and then No_Parent_Component
                                          (Result, Root, Length))
                 else Length = 0);

   --  The name as getcwd shows it: on System_Volume, a POSIX path ("/src",
   --  "/" for its root); elsewhere the CuBit name itself.
   procedure Display
     (Item : String; Result : out Name; Length : out Name_Length)
   with Pre => Item'First = 1 and then Item'Length <= Maximum_Name_Bytes,
        Post => Length in 1 .. Maximum_Name_Bytes;

end CuBit.Path_Names;
