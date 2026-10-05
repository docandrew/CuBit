------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's files through the filesystem service (docs/servo-port.md,
--  docs/filesystem-data-plane.md, docs/c-removal.md).
--
--  A client of filesystem.svc's typed protocol (CuBit.Filesystems) over the
--  capability in the fixed filesystem slot. The service checks every name
--  against the program's filesystem scopes; this library adds no authority
--  and no policy of its own.
--
--  Names are resolved by CuBit.Path_Names, relative ones from the working
--  directory kept here. Open, positioned read and write, flush, close and
--  the namespace operations go through the request queue lent to the
--  service once (CuBit.Filesystem_Queues), on the proved client of
--  CuBit.Submission_Queues; without a queue, messages through one bounce
--  buffer. Pages are cached while the service delegates reads to a handle
--  (CuBit.Libc_File_Cache), writes are buffered under a write delegation
--  (CuBit.Libc_Dirty_Map), and closed handles are parked for a reopen
--  (CuBit.Libc_Park_Table).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package CuBit.Libc_Files is

   subtype int is Interfaces.C.int;
   subtype long is Interfaces.C.long;
   subtype size_t is Interfaces.C.size_t;

   function Resolve (Path, Result : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_resolve";
   function Change_Directory (Path : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_chdir";
   procedure Working_Directory_Start (Name : System.Address)
   with Export, Convention => C, External_Name => "__cubit_cwd_start";
   function Working_Directory_Name (Result, Chosen : System.Address) return size_t
   with Export, Convention => C, External_Name => "__cubit_cwd_name";
   function Get_Working_Directory (Buffer : System.Address; Size : size_t) return long
   with Export, Convention => C, External_Name => "__cubit_getcwd";

   function Open (Path : System.Address; Directory : int; Options : Unsigned_64;
                  Handle, Size : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_file_open";
   procedure Close (Handle : Unsigned_64; Directory : int)
   with Export, Convention => C, External_Name => "__cubit_file_close";
   function Read_At (Handle : Unsigned_64; Buffer : System.Address; Count : size_t;
                     Offset : Unsigned_64) return long
   with Export, Convention => C, External_Name => "__cubit_file_read_at";
   function Write_At (Handle : Unsigned_64; Buffer : System.Address; Count : size_t;
                      Offset : Unsigned_64) return long
   with Export, Convention => C, External_Name => "__cubit_file_write_at";
   function Flush (Handle : Unsigned_64) return long
   with Export, Convention => C, External_Name => "__cubit_file_flush";
   function Describe (Handle : Unsigned_64; Inspection : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_file_describe";
   function Resize (Handle : Unsigned_64; Size : Unsigned_64) return long
   with Export, Convention => C, External_Name => "__cubit_file_resize";
   function Path_Access (Path, Directory, May_Write, Inspection, Described :
                           System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_path_access";
   function Path_Remove (Path : System.Address; Kind : int) return long
   with Export, Convention => C, External_Name => "__cubit_path_remove";
   function Path_Mkdir (Path : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_path_mkdir";
   function Path_Rename (From, To : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_path_rename";
   function Directory_Read_Page (Handle : Unsigned_64; Page : System.Address) return long
   with Export, Convention => C, External_Name => "__cubit_dir_read_page";

end CuBit.Libc_Files;
