------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with CuBit.Debug.Bytes;
with CuBit.Debug.Cfi;
with CuBit.Debug.Demangle;
with CuBit.Debug.Rust_Names;
with CuBit.Debug.Dwarf;
with CuBit.Debug.Elf;
with CuBit.Debug.Expressions;
with CuBit.Debug_Protocol;

package body CuBit.Debug is
   package BY renames CuBit.Debug.Bytes;
   package CF renames CuBit.Debug.Cfi;
   package DW renames CuBit.Debug.Dwarf;
   package EL renames CuBit.Debug.Elf;
   package EX renames CuBit.Debug.Expressions;
   package P renames CuBit.Debug_Protocol;
   package DT renames CuBit.Debug_Target;
   use type DT.Status;
   use type DT.Connection_Access;
   use type DW.Image_Access;
   use type CF.Table_Access;
   use type DW.Load_Result;
   use type DW.Value_Class;
   use type DT.Buffer_Access;
   use type EX.Result_Kind;

   Attach_Mode_Text : constant String := "cooperative attach (preview)";
   Kernel_Attach_Text : constant String := "kernel attach (Debug_Observe)";
   --  Where a program's image starts (userspace/c/link.ld: text at
   --  0x400000, the ELF and program headers loaded on the page before it).
   Image_Base : constant := 16#40_0000#;
   --  The build-id note is looked for this far past Image_Base when the
   --  image loads no ELF header (LLD).
   Build_Id_Scan_Pages : constant := 64;
   Scan_Page_Bytes : constant := 4096;
   --  A thread on a CPU has no saved registers: ask again this often.
   Register_Retries : constant := 32;
   --  How far above its stack pointer a thread's frames are believed.
   Stack_Walk_Ceiling : constant := 8 * 1024 * 1024;
   Truncation_Mark  : constant Character := '~';
   Ada_Language_Codes : constant array (1 .. 2) of Unsigned_64 := [16#0D#, 16#2E#];
   --  Ada 95 and Ada 2005/2012: arrays without a lower bound start at 1.
   Ada_Default_Lower : constant := 1;
   Other_Default_Lower : constant := 0;
   Address_Bytes    : constant := 8;
   Value_Bytes_Ceiling : constant := 8;   --  a scalar's widest encoding read
   Change_Slots     : constant := 4096;
   Change_Bytes_Ceiling : constant := 256;
   Member_Ceiling   : constant := 64;
   Type_Hops_Ceiling : constant := 16;
   Seqlock_Retries  : constant := 8;
   Source_Cache_Ceiling : constant := 8;
   Source_Line_Ceiling : constant := 65_536;
   Display_Elements : constant := 8;      --  array elements shown inline
   FNV_Offset       : constant := 16#CBF2_9CE4_8422_2325#;
   FNV_Prime        : constant := 16#0000_0100_0000_01B3#;
   Hex_Digits       : constant String := "0123456789abcdef";
   Nibble_Bits      : constant := 4;
   Runtime_Prefixes : constant array (1 .. 6) of access constant String :=
     [new String'("system__"), new String'("ada__"), new String'("interfaces__"),
      new String'("gnat__"), new String'("cubit__"), new String'("_")];
   Stub_Prefix      : constant String := "cubit__debug_stub__";
   Rust_Stub_Prefix : constant String := "cubit_debug_stub::";
   --  DW_LANG_Rust.
   Rust_Language_Code : constant := 16#1C#;
   TAG_Template_Type_Param : constant := 16#2F#;
   AT_Artificial : constant := 16#34#;
   --  Vec, slice and string elements a graph follows from one field.
   Graph_Element_Ceiling : constant := 16;
   --  Rust lengths beyond this are not believed (a torn or bad read).
   Rust_Length_Ceiling : constant := 2 ** 31;
   --  Nesting followed to find a pointer inside a wrapper struct.
   Leaf_Depth_Ceiling : constant := 8;
   --  GNAT's main subprogram's link name: the program's own sources are
   --  the units in its directory (the binder's and generated units aside).
   Main_Link_Name   : constant String := "_ada_main";
   Generated_Directory : constant String := "/build/";

   ---------------------------------------------------------------------------
   --  Texts
   ---------------------------------------------------------------------------
   generic
      Capacity : Positive;
      type Text is private;
      with function Make (Bytes : String; Length : Natural) return Text;
   function Bounded (S : String) return Text;
   function Bounded (S : String) return Text is
      Buffer : String (1 .. Capacity) := [others => ' '];
   begin
      if S'Length <= Capacity then
         Buffer (1 .. S'Length) := S;
         return Make (Buffer, S'Length);
      end if;
      Buffer := S (S'First .. S'First + Capacity - 2) & Truncation_Mark;
      return Make (Buffer, Capacity);
   end Bounded;

   function Make_Name (Bytes : String; Length : Natural) return Name_Text is ((Bytes, Length));
   function Make_Display (Bytes : String; Length : Natural) return Display_Text is ((Bytes, Length));
   function Make_Path (Bytes : String; Length : Natural) return Path_Text is ((Bytes, Length));
   function Make_Field_Name (Bytes : String; Length : Natural) return Field_Name_Text is ((Bytes, Length));
   function Make_Field_Display (Bytes : String; Length : Natural) return Field_Display_Text is ((Bytes, Length));
   function Make_Line (Bytes : String; Length : Natural) return Source_Line_Text is ((Bytes, Length));
   function Name_Of_Text is new Bounded (Name_Capacity, Name_Text, Make_Name);
   function Display_Of_Text is new Bounded (Display_Capacity, Display_Text, Make_Display);
   function Path_Of_Text is new Bounded (Path_Capacity, Path_Text, Make_Path);
   function Field_Name_Of is new Bounded (Field_Name_Capacity, Field_Name_Text, Make_Field_Name);
   function Field_Display_Of is new Bounded (Field_Display_Capacity, Field_Display_Text, Make_Field_Display);
   function Line_Of is new Bounded (Source_Line_Capacity, Source_Line_Text, Make_Line);

   function To_Name (S : String) return Name_Text renames Name_Of_Text;
   function To_Display (S : String) return Display_Text renames Display_Of_Text;
   function To_Path (S : String) return Path_Text renames Path_Of_Text;

   function Image_Of (V : Integer_64) return String is
      Text : constant String := Integer_64'Image (V);
   begin
      return (if Text (Text'First) = ' ' then Text (Text'First + 1 .. Text'Last) else Text);
   end Image_Of;
   function Image_Of (V : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (V);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image_Of;
   function Hex_Of (V : Unsigned_64) return String is
      Buffer : String (1 .. 16);
      X : Unsigned_64 := V;
      First : Natural := Buffer'Last;
   begin
      for I in reverse Buffer'Range loop
         Buffer (I) := Hex_Digits (Natural (X and 16#F#) + 1);
         X := Shift_Right (X, Nibble_Bits);
         if Buffer (I) /= '0' then
            First := I;
         end if;
      end loop;
      return "0x" & Buffer (First .. Buffer'Last);
   end Hex_Of;

   --  GNAT's encoded names, as Ada writes them: demo_world__the_world is
   --  Demo_World.The_World. Encoding suffixes (___XVE ...) are dropped.
   function Ada_Name (Raw : String) return String is
      Result : String (1 .. Raw'Length);
      Length : Natural := 0;
      I : Natural := Raw'First;
      Upper : Boolean := True;
   begin
      while I <= Raw'Last loop
         if I + 2 <= Raw'Last and then Raw (I .. I + 2) = "___" then
            exit;
         elsif I + 1 <= Raw'Last and then Raw (I .. I + 1) = "__" then
            Length := Length + 1;
            Result (Length) := '.';
            Upper := True;
            I := I + 2;
         else
            Length := Length + 1;
            Result (Length) :=
              (if Upper and then Raw (I) in 'a' .. 'z'
               then Character'Val (Character'Pos (Raw (I)) - 32) else Raw (I));
            Upper := Raw (I) = '_';
            I := I + 1;
         end if;
      end loop;
      declare
         Text : constant String := Result (1 .. Length);
      begin
         if Length >= 5 and then Text (1 .. 5) = "Cubit" and then (Length = 5 or else Text (6) = '.') then
            return "CuBit" & Text (6 .. Length);
         end if;
         return Text;
      end;
   end Ada_Name;

   --  The last component (Demo_World.High is High).
   function Simple_Name (Raw : String) return String is
      Full : constant String := Ada_Name (Raw);
      Start : Natural := Full'First;
   begin
      for I in Full'Range loop
         if Full (I) = '.' then
            Start := I + 1;
         end if;
      end loop;
      return Full (Start .. Full'Last);
   end Simple_Name;

   function Starts_With (S, Prefix : String) return Boolean is
     (S'Length >= Prefix'Length and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   ---------------------------------------------------------------------------
   --  Sessions
   ---------------------------------------------------------------------------
   type Frame_Registers is array (Frame_Index) of CF.Register_Set;
   type Frame_Addresses is array (Frame_Index) of Target_Address;
   type Thread_Frames is record
      Epoch : Stop_Epoch := 0;
      Count : Frame_Count := 0;
      Regs  : Frame_Registers;
      CFAs  : Frame_Addresses := [others => 0];
   end record;
   type Thread_Frame_Array is array (Thread_Index) of Thread_Frames;

   type Change_Entry is record
      Address : Target_Address := 0;
      Of_Type : Type_Key := No_Type;
      Hash    : Unsigned_64 := 0;
      Used    : Boolean := False;
   end record;
   type Change_Array is array (0 .. Change_Slots - 1) of Change_Entry;

   type Line_Starts is array (Natural range <>) of Natural;
   type Line_Starts_Access is access Line_Starts;
   type Source_Entry is record
      Name  : Path_Text;
      Data  : DT.Buffer_Access;
      Starts : Line_Starts_Access;
      Lines : Natural := 0;
   end record;
   type Source_Cache is array (1 .. Source_Cache_Ceiling) of Source_Entry;

   type State_Record is record
      Conn    : DT.Connection_Access;
      Img     : DW.Image_Access;
      Unwind  : CF.Table_Access;
      Info    : Program_Info;
      Epoch   : Stop_Epoch := 0;
      Stopped : Boolean := False;
      Ended   : Boolean := False;
      Frames  : Thread_Frame_Array;
      Changes : Change_Array;
      Sources : Source_Cache;
      Source_Next : Positive range 1 .. Source_Cache_Ceiling := 1;
      Own_Directory : Path_Text;   --  where the main unit's source is
      --  Through the kernel: the threads as the last Threads or Frames saw
      --  them (St.Frames is indexed the same way).
      Kernel_List  : DT.Kernel_Thread_Array;
      Kernel_Count : DT.Kernel_Thread_Count := 0;
   end record;
   type State_Access is access State_Record;

   type Slot_Entry is record
      State : State_Access;
      Generation : Unsigned_32 := 0;
   end record;
   Slots : array (1 .. Session_Ceiling) of Slot_Entry;

   function Lookup (S : Session) return State_Access is
     (if S.Slot in Slots'Range and then Slots (S.Slot).Generation = S.Generation
      then Slots (S.Slot).State else null);

   function Is_Ada (St : State_Record; Unit_Index : Natural) return Boolean is
     (for some L of Ada_Language_Codes => St.Img.Units (Unit_Index).Language = L);
   function Is_Rust (St : State_Record; Unit_Index : Natural) return Boolean is
     (Unit_Index < St.Img.Unit_Count and then St.Img.Units (Unit_Index).Language = Rust_Language_Code);

   --  A type's or component's name as its language writes it.
   function Type_Display_Name (St : State_Record; Unit_Index : Natural; Raw : String) return String is
     (if Is_Rust (St, Unit_Index) and then Raw'Last < CuBit.Debug.Rust_Names.Raw_Ceiling
      then CuBit.Debug.Rust_Names.Simplified (Raw)
      elsif Is_Rust (St, Unit_Index) then Raw else Ada_Name (Raw));
   function Field_Display_Name (St : State_Record; Unit_Index : Natural; Raw : String) return String is
     (if Is_Rust (St, Unit_Index)
      then (if Raw'Length > 2 and then Raw (Raw'First .. Raw'First + 1) = "__"
            then Raw (Raw'First + 2 .. Raw'Last) else Raw)
      else Simple_Name (Raw));

   ---------------------------------------------------------------------------
   --  Target memory
   ---------------------------------------------------------------------------
   procedure Read_Memory
     (St : State_Record; Address : Target_Address; Into : out DT.Byte_Buffer; Got : out Natural) is
   begin
      DT.Read (St.Conn.all, Unsigned_64 (Address), Into, Got);
   end Read_Memory;

   procedure Read_Word
     (St : State_Record; Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Good : out Boolean)
   is
      Buffer : DT.Byte_Buffer (0 .. Value_Bytes_Ceiling - 1);
      Got : Natural;
   begin
      Value := 0;
      Good := False;
      if Size > Value_Bytes_Ceiling then
         return;
      end if;
      Read_Memory (St, Target_Address (Address), Buffer (0 .. Size - 1), Got);
      if Got /= Size then
         return;
      end if;
      for I in reverse 0 .. Size - 1 loop
         Value := Shift_Left (Value, 8) or Unsigned_64 (Buffer (I));
      end loop;
      Good := True;
   end Read_Word;

   --  The target's state page, consistent for thread T (its sequence even
   --  and unchanged across the copy).
   procedure Snapshot (St : State_Record; Page : out DT.State_Page) is
   begin
      DT.Read_State (St.Conn.all, Page);
   end Snapshot;

   function Page_U64 (Page : DT.State_Page; At_Byte : Natural) return Unsigned_64 is
      V : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. 7 loop
         V := Shift_Left (V, 8) or Unsigned_64 (Page (At_Byte + I));
      end loop;
      return V;
   end Page_U64;
   function Page_U32 (Page : DT.State_Page; At_Byte : Natural) return Unsigned_32 is
      V : Unsigned_32 := 0;
   begin
      for I in reverse 0 .. 3 loop
         V := Shift_Left (V, 8) or Unsigned_32 (Page (At_Byte + I));
      end loop;
      return V;
   end Page_U32;
   function Page_Text (Page : DT.State_Page; At_Byte, Width : Natural) return String is
      Last : Natural := 0;
      Result : String (1 .. Width);
   begin
      for I in 1 .. Width loop
         Result (I) := Character'Val (Page (At_Byte + I - 1));
         if Result (I) not in ' ' .. '~' then
            Result (I) := ' ';
         end if;
         if Result (I) /= ' ' then
            Last := I;
         end if;
      end loop;
      return Result (1 .. Last);
   end Page_Text;

   function Thread_Total (Page : DT.State_Page) return Natural is
     (Natural'Min (Natural (Page_U32 (Page, P.Version_At) / 2 ** 16), P.Thread_Ceiling));

   ---------------------------------------------------------------------------
   --  Attach
   ---------------------------------------------------------------------------
   function Outcome_Of (S : DT.Status) return Outcome is
     (case S is
         when DT.Ok => Ok, when DT.No_Target => No_Target, when DT.Refused => Refused,
         when DT.Timed_Out => Timed_Out, when DT.Not_Found => No_Debug_Info,
         when DT.Malformed => Malformed);

   procedure Free_State is new Ada.Unchecked_Deallocation (State_Record, State_Access);

   procedure Release (St : in out State_Access) is
   begin
      if St = null then
         return;
      end if;
      DW.Free (St.Img);
      CF.Free (St.Unwind);
      Free_State (St);
   end Release;

   --  A Rust program's own main: crate::main, one path level deep.
   function Rust_Main (Link : String) return Boolean is
      Demangled : CuBit.Debug.Demangle.Output;
      Separators : Natural := 0;
   begin
      if Link'Length = 0 or else Link'Last = Positive'Last then
         return False;
      end if;
      CuBit.Debug.Demangle.Rust (Link, Demangled);
      if not Demangled.Valid then
         return False;
      end if;
      for I in 1 .. Demangled.Length - 1 loop
         if Demangled.Text (I .. I + 1) = "::" then
            Separators := Separators + 1;
         end if;
      end loop;
      return Separators = 1 and then Demangled.Length > 6
        and then Demangled.Text (Demangled.Length - 5 .. Demangled.Length) = "::main";
   end Rust_Main;

   --  A global's display name: Ada's, or a Rust static's demangled path.
   function Global_Name (St : State_Record; D : DW.Die; Raw : String) return String is
   begin
      if Is_Rust (St, D.Unit_Index) then
         declare
            A : DW.Attribute;
            Found : Boolean;
            Demangled : CuBit.Debug.Demangle.Output;
         begin
            DW.Find_Inherited (St.Img.all, D, DW.AT_Linkage_Name, A, Found);
            if Found then
               declare
                  Link : constant String := DW.Text_Of (St.Img.all, A);
               begin
                  if Link'Length > 0 and then Link'Last < Positive'Last then
                     CuBit.Debug.Demangle.Rust (Link, Demangled);
                     if Demangled.Valid then
                        return Demangled.Text (1 .. Demangled.Length);
                     end if;
                  end if;
               end;
            end if;
            return Raw;
         end;
      end if;
      return Ada_Name (Raw);
   end Global_Name;

   ---------------------------------------------------------------------------
   --  Through the kernel
   ---------------------------------------------------------------------------
   function Through_Kernel (S : Session) return Boolean is
      St : constant State_Access := Lookup (S);
   begin
      return St /= null and then St.Conn /= null and then DT.Through_Kernel (St.Conn.all);
   end Through_Kernel;

   function Little (Bytes : DT.Byte_Buffer; At_Byte, Width : Natural) return Unsigned_64 is
      V : Unsigned_64 := 0;
   begin
      for B in reverse 0 .. Width - 1 loop
         V := Shift_Left (V, 8) or Unsigned_64 (Bytes (Bytes'First + At_Byte + B));
      end loop;
      return V;
   end Little;

   --  The target's GNU build-id, read from its memory: the PT_NOTE its
   --  loaded program headers name, or (no ELF header loaded) the note
   --  itself, looked for past Image_Base.
   procedure Find_Build_Id (St : State_Record; Id : out String; Found : out Boolean) is
      ELF_Header_Bytes : constant := 64;
      Phdr_Bytes : constant := 56;
      PT_Note : constant := 4;
      Note_Header_Bytes : constant := 12;
      GNU_Name_Bytes : constant := 4;
      NT_GNU_Build_Id : constant := 3;
      Note_Read_Ceiling : constant := 256;
      Phdr_Ceiling : constant := 32;
      Header : DT.Byte_Buffer (0 .. ELF_Header_Bytes - 1);
      Got : Natural;

      --  A GNU build-id note at At_Byte of Bytes.
      function Note_At (Bytes : DT.Byte_Buffer; At_Byte : Natural) return Boolean is
        (At_Byte + Note_Header_Bytes + GNU_Name_Bytes + Build_Id_Bytes <= Bytes'Length
         and then Little (Bytes, At_Byte, 4) = GNU_Name_Bytes
         and then Little (Bytes, At_Byte + 4, 4) = Build_Id_Bytes
         and then Little (Bytes, At_Byte + 8, 4) = NT_GNU_Build_Id
         and then Bytes (Bytes'First + At_Byte + 12) = Character'Pos ('G')
         and then Bytes (Bytes'First + At_Byte + 13) = Character'Pos ('N')
         and then Bytes (Bytes'First + At_Byte + 14) = Character'Pos ('U')
         and then Bytes (Bytes'First + At_Byte + 15) = 0);

      procedure Take (Bytes : DT.Byte_Buffer; At_Byte : Natural) is
      begin
         for I in 0 .. Build_Id_Bytes - 1 loop
            declare
               Byte : constant Unsigned_8 :=
                 Bytes (Bytes'First + At_Byte + Note_Header_Bytes + GNU_Name_Bytes + I);
            begin
               Id (Id'First + 2 * I) := Hex_Digits (Natural (Byte / 16) + 1);
               Id (Id'First + 2 * I + 1) := Hex_Digits (Natural (Byte mod 16) + 1);
            end;
         end loop;
         Found := True;
      end Take;
   begin
      Id := [others => '0'];
      Found := False;
      Read_Memory (St, Image_Base, Header, Got);
      if Got = Header'Length and then Header (0) = 16#7F# and then Header (1) = Character'Pos ('E')
        and then Header (2) = Character'Pos ('L') and then Header (3) = Character'Pos ('F')
      then
         declare
            Phoff : constant Unsigned_64 := Little (Header, 32, 8);
            Phnum : constant Natural := Natural'Min (Natural (Little (Header, 56, 2)), Phdr_Ceiling);
            Phdr : DT.Byte_Buffer (0 .. Phdr_Bytes - 1);
         begin
            for K in 0 .. Phnum - 1 loop
               Read_Memory (St, Target_Address (Image_Base + Phoff + Unsigned_64 (K * Phdr_Bytes)),
                            Phdr, Got);
               exit when Got /= Phdr'Length;
               if Little (Phdr, 0, 4) = PT_Note then
                  declare
                     Notes : DT.Byte_Buffer
                       (0 .. Natural'Min (Natural (Little (Phdr, 40, 8)), Note_Read_Ceiling) - 1);
                     At_Byte : Natural := 0;
                  begin
                     Read_Memory (St, Target_Address (Little (Phdr, 16, 8)), Notes, Got);
                     while At_Byte + Note_Header_Bytes <= Got loop
                        if Note_At (Notes (0 .. Got - 1), At_Byte) then
                           Take (Notes, At_Byte);
                           return;
                        end if;
                        --  The next note: its header, name and desc, each 4-aligned.
                        At_Byte := At_Byte + Note_Header_Bytes
                          + Natural ((Little (Notes, At_Byte, 4) + 3) / 4 * 4)
                          + Natural ((Little (Notes, At_Byte + 4, 4) + 3) / 4 * 4);
                     end loop;
                  end;
               end if;
            end loop;
         end;
      end if;
      --  No loaded header (LLD) or no note there: look for the note itself.
      for Pg in 0 .. Build_Id_Scan_Pages - 1 loop
         declare
            Bytes : DT.Byte_Buffer (0 .. Scan_Page_Bytes - 1);
         begin
            Read_Memory (St, Target_Address (Image_Base + Unsigned_64 (Pg * Scan_Page_Bytes)), Bytes, Got);
            exit when Got = 0;
            for At_Byte in 0 .. (Got - 1) / 4 loop
               if Note_At (Bytes (0 .. Got - 1), 4 * At_Byte) then
                  Take (Bytes, 4 * At_Byte);
                  return;
               end if;
            end loop;
         end;
      end loop;
   end Find_Build_Id;

   --  The kernel's thread list now (St.Kernel_List).
   procedure Kernel_Snapshot (St : in out State_Record; R : out Outcome) is
      Live : Natural;
      Result : DT.Status;
   begin
      DT.Kernel_Threads (St.Conn.all, St.Kernel_List, St.Kernel_Count, Live, Result);
      R := (if Result /= DT.Ok then (if Result = DT.No_Target then Exited else Refused)
            elsif Live > St.Kernel_Count then More else Ok);
      if Result = DT.No_Target then
         St.Ended := True;
      end if;
   end Kernel_Snapshot;

   --  Thread's place in St.Kernel_List (its Id is its kernel slot + 1); 0: none.
   function Kernel_Index (St : State_Record; Thread : Thread_Id) return Natural is
   begin
      for K in 1 .. St.Kernel_Count loop
         if Thread_Id (St.Kernel_List (K).Slot) + 1 = Thread then
            return K;
         end if;
      end loop;
      return 0;
   end Kernel_Index;

   function Kernel_Thread_Name (T : DT.Kernel_Thread) return String is
     ("thread" & Unsigned_16'Image (T.Slot));

   procedure Attach
     (Target     : CuBit.Debug_Target.Process_Handle;
      Rights     : Debug_Rights;
      Until_Time : Deadline_Ms;
      S          : out Session;
      R          : out Outcome)
   is
      pragma Unreferenced (Rights);
      St : State_Access := new State_Record;
      Result : DT.Status;
      Page : DT.State_Page;
      Data : DT.Buffer_Access;
      Path : String (1 .. Path_Capacity);
      Path_Length : Natural;
      Loaded : DW.Load_Result;
      Free_Slot : Natural := 0;
   begin
      S := No_Session;
      for I in Slots'Range loop
         if Slots (I).State = null then
            Free_Slot := I;
            exit;
         end if;
      end loop;
      if Free_Slot = 0 then
         Release (St);
         R := Refused;
         return;
      end if;
      DT.Connect (Target, Until_Time, St.Conn, Result);
      if Result /= DT.Ok then
         Release (St);
         R := Outcome_Of (Result);
         return;
      end if;
      if DT.Through_Kernel (St.Conn.all) then
         --  No stub: the build-id from the target's own memory.
         declare
            Found : Boolean;
         begin
            Find_Build_Id (St.all, St.Info.Build_Id, Found);
            if not Found then
               DT.Disconnect (St.Conn, Until_Time);
               Release (St);
               R := No_Debug_Info;
               return;
            end if;
         end;
         St.Info.Process := DT.Target_Identity (St.Conn.all);
         St.Info.Program := To_Name ("process " & Image_Of (St.Info.Process));
         St.Info.Attach_Mode := To_Name (Kernel_Attach_Text);
      else
         Snapshot (St.all, Page);
         if Page_U32 (Page, P.Magic_At) /= P.Magic
           or else Page_U32 (Page, P.Version_At) mod 2 ** 16 /= Unsigned_32 (P.Version)
           or else Page_U32 (Page, P.Build_Id_Length_At) /= P.Build_Id_Ceiling
         then
            DT.Disconnect (St.Conn, Until_Time);
            Release (St);
            R := Malformed;
            return;
         end if;
         for I in 0 .. Build_Id_Bytes - 1 loop
            declare
               Byte : constant Unsigned_8 := Page (P.Build_Id_At + I);
            begin
               St.Info.Build_Id (1 + 2 * I) := Hex_Digits (Natural (Byte / 16) + 1);
               St.Info.Build_Id (2 + 2 * I) := Hex_Digits (Natural (Byte mod 16) + 1);
            end;
         end loop;
         St.Info.Program := To_Name (Page_Text (Page, P.Name_At, P.Name_Bytes));
         St.Info.Process := Page_U64 (Page, P.Process_At);
         St.Info.Attach_Mode := To_Name (Attach_Mode_Text);
      end if;

      DT.Load_Debug_File (St.Conn.all, St.Info.Build_Id, Until_Time, Data, Path, Path_Length, Result);
      St.Info.Debug_File := To_Path (Path (1 .. Path_Length));
      if Result /= DT.Ok then
         DT.Disconnect (St.Conn, Until_Time);
         Release (St);
         R := (if Result = DT.Timed_Out then Timed_Out else No_Debug_Info);
         return;
      end if;
      DW.Load (Data, St.Img, Loaded);
      if Loaded /= DW.Loaded then
         DT.Disconnect (St.Conn, Until_Time);
         Release (St);
         R := (if Loaded = DW.Malformed_Info then Malformed else No_Debug_Info);
         return;
      end if;
      --  The build-id the file names must be the target's.
      declare
         File_Id : String (1 .. 2 * Build_Id_Bytes);
         File_Id_Length : Natural;
      begin
         EL.Read_Build_Id (St.Img.Data.all, St.Img.Sections, File_Id, File_Id_Length);
         if File_Id_Length /= File_Id'Length or else File_Id /= St.Info.Build_Id then
            DT.Disconnect (St.Conn, Until_Time);
            Release (St);
            R := No_Debug_Info;
            return;
         end if;
      end;
      --  Unwind tables: the file's .eh_frame, or the target's own copy of it
      --  when the debug file keeps only its header (NOBITS).
      declare
         EH : constant EL.Section := St.Img.Sections (EL.Eh_Frame);
         Bytes : DT.Buffer_Access;
         Got : Natural;
      begin
         if EH.Present and then EH.Size > 0 and then EH.Size < Unsigned_64 (Natural'Last) then
            Bytes := new DT.Byte_Buffer (0 .. Natural (EH.Size) - 1);
            if EH.In_File then
               Bytes.all := St.Img.Data (Natural (EH.File_Offset) .. Natural (EH.File_Offset + EH.Size) - 1);
            else
               DT.Read (St.Conn.all, EH.Address, Bytes.all, Got);
            end if;
            CF.Build (Bytes, EH.Address, St.Unwind);
         end if;
      end;
      --  The program's own directory, from its main subprogram's unit.
      for F in 0 .. St.Img.Function_Count - 1 loop
         declare
            D : DW.Die;
            Good : Boolean;
            I : DW.Attribute_Count;
         begin
            DW.Read_Die (St.Img.all, St.Img.Functions (F).Die_Offset, D, Good);
            I := (if Good then DW.Find (D, DW.AT_Linkage_Name) else 0);
            if I /= 0 and then
              (DW.Text_Of (St.Img.all, D.Attrs (I)) = Main_Link_Name
               or else (Is_Rust (St.all, D.Unit_Index) and then DW.Name_Of (St.Img.all, D) = "main"
                        and then Rust_Main (DW.Text_Of (St.Img.all, D.Attrs (I)))))
            then
               declare
                  Name : constant String := DW.Unit_Name (St.Img.all, D.Unit_Index);
                  Last : Natural := 0;
               begin
                  for C in Name'Range loop
                     if Name (C) = '/' then
                        Last := C;
                     end if;
                  end loop;
                  if Last > 0 then
                     St.Own_Directory := To_Path (Name (Name'First .. Last));
                     --  Through the kernel, the program is named by its
                     --  source directory (userspace/apps/<name>/).
                     if DT.Through_Kernel (St.Conn.all) then
                        declare
                           First : Positive := Name'First;
                        begin
                           for C in Name'First .. Last - 1 loop
                              if Name (C) = '/' then
                                 First := C + 1;
                              end if;
                           end loop;
                           if First < Last then
                              St.Info.Program := To_Name (Name (First .. Last - 1));
                           end if;
                        end;
                     end if;
                  end if;
               end;
               exit;
            end if;
         end;
      end loop;
      Slots (Free_Slot).Generation := Slots (Free_Slot).Generation + 1;
      Slots (Free_Slot).State := St;
      S := (Slot => Free_Slot, Generation => Slots (Free_Slot).Generation);
      R := Ok;
   end Attach;

   procedure Free_Starts is new Ada.Unchecked_Deallocation (Line_Starts, Line_Starts_Access);
   procedure Free_Buffer is new Ada.Unchecked_Deallocation (DT.Byte_Buffer, DT.Buffer_Access);

   procedure Detach (S : in out Session) is
      St : State_Access := Lookup (S);
      --  The target revokes its grants at once; this bounds a stuck one.
      Detach_Answer_Ms : constant := 250;
   begin
      if St /= null then
         if St.Conn /= null then
            DT.Disconnect (St.Conn, DT.Deadline_After (Detach_Answer_Ms));
         end if;
         for E of St.Sources loop
            if E.Data /= null then
               Free_Buffer (E.Data);
               Free_Starts (E.Starts);
            end if;
         end loop;
         Slots (S.Slot).State := null;
         Release (St);
      end if;
      S := No_Session;
   end Detach;

   function State (S : Session) return Target_State is
      St : constant State_Access := Lookup (S);
   begin
      if St = null then
         return Detached;
      elsif St.Ended then
         return Exited;
      elsif St.Stopped then
         return Stopped_Cooperatively;
      end if;
      return Running;
   end State;

   procedure Describe (S : Session; Info : out Program_Info; R : out Outcome) is
      St : constant State_Access := Lookup (S);
      Page : DT.State_Page;
      Total : Unsigned_64 := 0;
   begin
      Info := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      if not DT.Through_Kernel (St.Conn.all) then
         Snapshot (St.all, Page);
         for T in 1 .. Thread_Total (Page) loop
            Total := Total + Page_U64 (Page, P.Threads_At + (T - 1) * P.Thread_Bytes + P.T_Checkpoints_At);
         end loop;
      end if;
      Info := St.Info;
      Info.Checkpoints := Total;
      R := Ok;
   end Describe;

   procedure Pause (S : Session; Until_Time : Deadline_Ms; R : out Outcome) is
      St : constant State_Access := Lookup (S);
      Epoch : Unsigned_64;
      Result : DT.Status;
   begin
      if St = null then
         R := Not_Attached;
         return;
      end if;
      DT.Pause (St.Conn.all, Until_Time, Epoch, Result);
      if Result = DT.Ok then
         St.Epoch := Stop_Epoch (Epoch);
         St.Stopped := True;
      end if;
      R := Outcome_Of (Result);
   end Pause;

   procedure Resume (S : Session; Until_Time : Deadline_Ms; R : out Outcome) is
      St : constant State_Access := Lookup (S);
      Result : DT.Status;
   begin
      if St = null then
         R := Not_Attached;
         return;
      end if;
      DT.Resume (St.Conn.all, Until_Time, Result);
      if Result = DT.Ok then
         St.Stopped := False;
      end if;
      R := Outcome_Of (Result);
   end Resume;

   ---------------------------------------------------------------------------
   --  Threads and frames
   ---------------------------------------------------------------------------
   procedure Threads (S : Session; List : out Thread_List; R : out Outcome) is
      St : constant State_Access := Lookup (S);
      Page : DT.State_Page;
   begin
      List := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      if DT.Through_Kernel (St.Conn.all) then
         Kernel_Snapshot (St.all, R);
         if R not in Ok | More then
            return;
         end if;
         for K in 1 .. St.Kernel_Count loop
            if List.Count = Thread_Ceiling then
               R := More;
               exit;
            end if;
            declare
               T : DT.Kernel_Thread renames St.Kernel_List (K);
            begin
               List.Count := List.Count + 1;
               List.Items (List.Count) :=
                 (Id => Thread_Id (T.Slot) + 1, Name => To_Name (Kernel_Thread_Name (T)),
                  State => Running, PC => Target_Address (T.Sampled_RIP),
                  Stack_Low => 0, Stack_High => 0, Wait => T.Wait, Peer => T.Peer,
                  Since => T.Since, Deadline => T.Deadline, CPU => T.CPU);
            end;
         end loop;
         return;
      end if;
      Snapshot (St.all, Page);
      for T in 1 .. Thread_Total (Page) loop
         declare
            Base : constant Natural := P.Threads_At + (T - 1) * P.Thread_Bytes;
            Raw : constant Unsigned_32 := Page_U32 (Page, Base + P.T_State_At);
         begin
            List.Count := List.Count + 1;
            List.Items (List.Count) :=
              (Id => Thread_Id (Page_U32 (Page, Base + P.T_Id_At)),
               Name => To_Name (Page_Text (Page, Base + P.T_Name_At, P.T_Name_Bytes)),
               State => (if Raw = P.Thread_Parked then Parked
                         elsif Raw = P.Thread_Ended then Ended else Running),
               PC => Target_Address (Page_U64 (Page, Base + P.T_RIP_At)),
               Stack_Low => Target_Address (Page_U64 (Page, Base + P.T_Stack_Low_At)),
               Stack_High => Target_Address (Page_U64 (Page, Base + P.T_Stack_High_At)),
               others => <>);
         end;
      end loop;
      R := Ok;
   end Threads;

   --  The function at PC as its language writes it (Rust: demangled from
   --  its link name), and whether it is a debug stub's own.
   function Function_Name
     (St : State_Record; PC : Unsigned_64; Display : out Name_Text; Stub : out Boolean) return Boolean
   is
      F : constant Integer := DW.Function_At (St.Img.all, PC);
      D : DW.Die;
      Good : Boolean;
   begin
      Display := (others => <>);
      Stub := False;
      if F < 0 then
         return False;
      end if;
      DW.Read_Die (St.Img.all, St.Img.Functions (F).Die_Offset, D, Good);
      if not Good then
         return False;
      end if;
      if St.Img.Units (D.Unit_Index).Language = Rust_Language_Code then
         declare
            A : DW.Attribute;
            Found : Boolean;
            Demangled : CuBit.Debug.Demangle.Output;
         begin
            DW.Find_Inherited (St.Img.all, D, DW.AT_Linkage_Name, A, Found);
            if Found then
               declare
                  Link : constant String := DW.Text_Of (St.Img.all, A);
               begin
                  if Link'Length > 0 and then Link'Last < Positive'Last then
                     CuBit.Debug.Demangle.Rust (Link, Demangled);
                  end if;
               end;
            end if;
            if Demangled.Valid then
               Display := To_Name (Demangled.Text (1 .. Demangled.Length));
            else
               Display := To_Name (DW.Name_Of (St.Img.all, D));
            end if;
            Stub := Starts_With (Image (Display), Rust_Stub_Prefix);
         end;
      else
         declare
            Raw : constant String := DW.Name_Of (St.Img.all, D);
         begin
            Display := To_Name (Ada_Name (Raw));
            Stub := Starts_With (Raw, Stub_Prefix);
         end;
      end if;
      return True;
   end Function_Name;

   --  What CuBit.Debug_Stub.Checkpoint records.
   Captured : constant array (1 .. 8) of CF.Register :=
     [CF.Return_Address, CF.RSP, CF.RBP, CF.RBX, CF.R12, CF.R13, CF.R14, CF.R15];

   procedure Frames
     (S : Session; Thread : Thread_Id; List : out Frame_List; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      Page : DT.State_Page;
      Index : Natural := 0;
      Base : Natural;
      Regs, Caller : CF.Register_Set;
      CFA : Unsigned_64;
      Stepped : Boolean;
      Low, High : Unsigned_64;
      Seq_Before : Unsigned_64;
   begin
      List := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      if DT.Through_Kernel (St.Conn.all) then
         --  The registers the kernel saved (all of them, at its entry), a
         --  new epoch, and memory as it is now.
         declare
            Snap : Outcome;
            Saved : DT.General_Registers;
            Got : DT.Register_Status := DT.Failed;
            use type DT.Register_Status;
            --  Debug_ABI's General order to the DWARF columns.
            Order : constant array (0 .. 16) of CF.Register :=
              [0 => CF.RAX, 1 => CF.RBX, 2 => CF.RCX, 3 => CF.RDX, 4 => CF.RSI, 5 => CF.RDI,
               6 => CF.RBP, 7 => CF.RSP, 8 => 8, 9 => 9, 10 => 10, 11 => 11,
               12 => CF.R12, 13 => CF.R13, 14 => CF.R14, 15 => CF.R15, 16 => CF.Return_Address];
         begin
            Kernel_Snapshot (St.all, Snap);
            if Snap not in Ok | More then
               R := Snap;
               return;
            end if;
            Index := Kernel_Index (St.all, Thread);
            if Index = 0 or else Index > Thread_Ceiling then
               R := Not_Found;
               return;
            end if;
            for Attempt in 1 .. Register_Retries loop
               DT.Kernel_Registers (St.Conn.all, St.Kernel_List (Index), Saved, Got);
               exit when Got /= DT.On_CPU;
            end loop;
            if Got = DT.On_CPU then
               R := Not_Stopped;
               return;
            elsif Got /= DT.Saved then
               R := Not_Found;
               return;
            end if;
            DT.Refresh (St.Conn.all);
            St.Epoch := St.Epoch + 1;
            for K in Order'Range loop
               Regs.Value (Order (K)) := Saved (K);
               Regs.Known (Order (K)) := True;
            end loop;
            Low := Regs.Value (CF.RSP);
            High := Low + Stack_Walk_Ceiling;
         end;
      else
      for Attempt in 1 .. Seqlock_Retries loop
         Snapshot (St.all, Page);
         Index := 0;
         for T in 1 .. Thread_Total (Page) loop
            if Thread_Id (Page_U32 (Page, P.Threads_At + (T - 1) * P.Thread_Bytes + P.T_Id_At)) = Thread then
               Index := T;
            end if;
         end loop;
         exit when Index = 0;
         Base := P.Threads_At + (Index - 1) * P.Thread_Bytes;
         Seq_Before := Page_U64 (Page, Base + P.T_Sequence_At);
         exit when Seq_Before mod 2 = 0;
      end loop;
      if Index = 0 then
         R := Not_Found;
         return;
      end if;
      if Page_U32 (Page, Base + P.T_State_At) /= P.Thread_Parked then
         R := Not_Stopped;
         return;
      end if;
      Low := Page_U64 (Page, Base + P.T_Stack_Low_At);
      High := Page_U64 (Page, Base + P.T_Stack_High_At);
      Regs.Value (CF.Return_Address) := Page_U64 (Page, Base + P.T_RIP_At);
      Regs.Value (CF.RSP) := Page_U64 (Page, Base + P.T_RSP_At);
      Regs.Value (CF.RBP) := Page_U64 (Page, Base + P.T_RBP_At);
      Regs.Value (CF.RBX) := Page_U64 (Page, Base + P.T_RBX_At);
      Regs.Value (CF.R12) := Page_U64 (Page, Base + P.T_R12_At);
      Regs.Value (CF.R13) := Page_U64 (Page, Base + P.T_R13_At);
      Regs.Value (CF.R14) := Page_U64 (Page, Base + P.T_R14_At);
      Regs.Value (CF.R15) := Page_U64 (Page, Base + P.T_R15_At);
      for Reg of Captured loop
         Regs.Known (Reg) := True;
      end loop;
      end if;

      declare
         TF : Thread_Frames renames St.Frames (Thread_Index (Natural'Max (1, Natural'Min (Index, Thread_Ceiling))));
         procedure Read (Address : Unsigned_64; Value : out Unsigned_64; Good : out Boolean) is
         begin
            Read_Word (St.all, Address, Address_Bytes, Value, Good);
         end Read;
      begin
         TF := (Epoch => St.Epoch, Count => 0, others => <>);
         loop
            exit when List.Count = Frame_Ceiling;
            declare
               PC : constant Unsigned_64 := Regs.Value (CF.Return_Address);
               Innermost : constant Boolean := List.Count = 0;
               Lookup_PC : constant Unsigned_64 := (if Innermost then PC else PC - 1);
               F : Frame;
               Raw : Name_Text;
               Stub : Boolean;
               Row : DW.Row;
               Found : Boolean;
            begin
               F.Thread := Thread;
               F.Index := List.Count;
               F.Epoch := St.Epoch;
               F.PC := Target_Address (PC);
               F.Has_Debug_Info := Function_Name (St.all, Lookup_PC, Raw, Stub);
               F.Function_Name := (if F.Has_Debug_Info then Raw else To_Name (Hex_Of (PC)));
               F.In_Stub := F.Has_Debug_Info and then Stub;
               DW.Line_At (St.Img.all, Lookup_PC, Row, Found);
               if Found then
                  F.File := To_Path (DW.File_Name (St.Img.all, Row.File));
                  F.Line := Row.Line;
                  F.Column := Row.Column;
               end if;
               Stepped := St.Unwind /= null;
               if Stepped then
                  CF.Step (St.Unwind.all, Regs, Innermost, Read'Access, Caller, CFA, Stepped);
               end if;
               F.CFA := (if Stepped then Target_Address (CFA) else 0);
               List.Count := List.Count + 1;
               List.Items (F.Index) := F;
               TF.Regs (F.Index) := Regs;
               TF.CFAs (F.Index) := F.CFA;
               TF.Count := List.Count;
               exit when not Stepped;
               --  The caller must be further up the same stack.
               exit when Caller.Value (CF.RSP) <= Regs.Value (CF.RSP)
                 or else Caller.Value (CF.RSP) > High or else Caller.Value (CF.RSP) < Low;
               Regs := Caller;
            end;
         end loop;
      end;
      R := Ok;
   end Frames;

   ---------------------------------------------------------------------------
   --  Types
   ---------------------------------------------------------------------------

   --  Through typedefs and qualifiers to the type that has the layout. Name:
   --  the first name met on the way (the subtype's or typedef's).
   procedure Strip
     (St : State_Record; Key : Type_Key; Core : out DW.Die; Name : out Name_Text; Good : out Boolean)
   is
      Cur : BY.Offset := BY.Offset (Key);
      Next : BY.Offset;
   begin
      Name := (others => <>);
      Good := False;
      Core := (others => <>);
      for Hop in 1 .. Type_Hops_Ceiling loop
         DW.Read_Die (St.Img.all, Cur, Core, Good);
         if not Good then
            return;
         end if;
         if Name.Length = 0 then
            declare
               N : constant String := DW.Name_Of (St.Img.all, Core);
            begin
               if N'Length > 0 then
                  Name := To_Name (Type_Display_Name (St, Core.Unit_Index, N));
               end if;
            end;
         end if;
         if Core.Tag in DW.TAG_Typedef | DW.TAG_Const_Type | DW.TAG_Volatile_Type
                      | DW.TAG_Restrict_Type | DW.TAG_Atomic_Type
         then
            if not DW.Reference_Of (Core, DW.AT_Type, Next) then
               Good := False;
               return;
            end if;
            Cur := Next;
         else
            return;
         end if;
      end loop;
      Good := False;
   end Strip;

   function Byte_Size (D : DW.Die) return Unsigned_64 is
      I : constant DW.Attribute_Count := DW.Find (D, DW.AT_Byte_Size);
   begin
      if I /= 0 and then D.Attrs (I).Class in DW.Constant_Value | DW.Signed_Value then
         return D.Attrs (I).Value;
      end if;
      return 0;
   end Byte_Size;

   function Type_Of (D : DW.Die) return Type_Key is
      T : BY.Offset;
   begin
      return (if DW.Reference_Of (D, DW.AT_Type, T) then Type_Key (T) else No_Type);
   end Type_Of;

   function Size_Of_Type (St : State_Record; Key : Type_Key) return Unsigned_64;

   --  A bound attribute's value: a constant, or a variable's value.
   function Bound_Of (St : State_Record; D : DW.Die; Name : Unsigned_64; Value : out Integer_64) return Boolean is
      I : constant DW.Attribute_Count := DW.Find (D, Name);
   begin
      Value := 0;
      if I = 0 then
         return False;
      end if;
      return DW.Constant_Of (D.Attrs (I), Value);
      pragma Unreferenced (St);
   end Bound_Of;

   type Array_Shape is record
      Element : Type_Key := No_Type;
      Element_Size : Unsigned_64 := 0;
      Low, High : Integer_64 := 0;
      Known : Boolean := False;
      Dimensions : Natural := 0;
   end record;

   function Shape_Of (St : State_Record; D : DW.Die; Ref : Value_Ref) return Array_Shape is
      Shape : Array_Shape;
      Cur : BY.Offset := D.Next;
      Child : DW.Die;
      Good : Boolean;
      Lo, Hi, Count : Integer_64;
   begin
      Shape.Element := Type_Of (D);
      Shape.Element_Size := Size_Of_Type (St, Shape.Element);
      if not D.Children then
         return Shape;
      end if;
      loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = DW.TAG_Subrange_Type or else Child.Tag = DW.TAG_Enumeration_Type then
            Shape.Dimensions := Shape.Dimensions + 1;
            if Shape.Dimensions = 1 then
               if not Bound_Of (St, Child, DW.AT_Lower_Bound, Lo) then
                  Lo := (if Is_Ada (St, D.Unit_Index) then Ada_Default_Lower else Other_Default_Lower);
               end if;
               if Bound_Of (St, Child, DW.AT_Upper_Bound, Hi) then
                  Shape.Known := True;
               elsif Bound_Of (St, Child, DW.AT_Count, Count) then
                  Hi := Lo + Count - 1;
                  Shape.Known := True;
               end if;
               Shape.Low := Lo;
               Shape.High := Hi;
            end if;
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
      if Ref.Has_Bounds then
         Shape.Low := Ref.Low;
         Shape.High := Ref.High;
         Shape.Known := True;
      end if;
      return Shape;
   end Shape_Of;

   function Element_Count (Shape : Array_Shape) return Natural is
     (if not Shape.Known or else Shape.High < Shape.Low then 0
      elsif Shape.High - Shape.Low >= Integer_64 (Natural'Last) then Natural'Last
      else Natural (Shape.High - Shape.Low + 1));

   function Size_Of_Type (St : State_Record; Key : Type_Key) return Unsigned_64 is
      Core : DW.Die;
      Name : Name_Text;
      Good : Boolean;
      Size : Unsigned_64;
   begin
      if Key = No_Type then
         return 0;
      end if;
      Strip (St, Key, Core, Name, Good);
      if not Good then
         return 0;
      end if;
      Size := Byte_Size (Core);
      if Size /= 0 then
         return Size;
      end if;
      case Core.Tag is
         when DW.TAG_Pointer_Type | DW.TAG_Reference_Type | DW.TAG_Rvalue_Reference =>
            return Address_Bytes;
         when DW.TAG_Subrange_Type =>
            return Size_Of_Type (St, Type_Of (Core));
         when DW.TAG_Array_Type =>
            declare
               Shape : constant Array_Shape := Shape_Of (St, Core, Null_Ref);
            begin
               return Unsigned_64 (Element_Count (Shape)) * Shape.Element_Size;
            end;
         when others =>
            return 0;
      end case;
   end Size_Of_Type;

   ---------------------------------------------------------------------------
   --  Reading a value's bytes
   ---------------------------------------------------------------------------
   procedure Value_Word
     (St : State_Record; Ref : Value_Ref; Offset_In : Unsigned_64; Size : Positive;
      Value : out Unsigned_64; Good : out Boolean) is
   begin
      Value := 0;
      Good := False;
      if Size > Value_Bytes_Ceiling then
         return;
      end if;
      case Ref.Location is
         when In_Memory =>
            Read_Word (St, Unsigned_64 (Ref.Address) + Offset_In, Size, Value, Good);
         when Register_Value =>
            if Offset_In + Unsigned_64 (Size) <= Value_Bytes_Ceiling then
               Value := Shift_Right (Ref.Bits, Natural (Offset_In) * 8);
               if Size < Value_Bytes_Ceiling then
                  Value := Value and (Shift_Left (1, Size * 8) - 1);
               end if;
               Good := True;
            end if;
         when others =>
            null;
      end case;
   end Value_Word;

   function Hash_Of (St : State_Record; Ref : Value_Ref; Size : Unsigned_64) return Unsigned_64 is
      H : Unsigned_64 := FNV_Offset;
      Buffer : DT.Byte_Buffer (0 .. Change_Bytes_Ceiling - 1);
      Count : constant Natural := Natural (Unsigned_64'Min (Size, Change_Bytes_Ceiling));
      Got : Natural := 0;
   begin
      if Ref.Location = In_Memory then
         Read_Memory (St, Ref.Address, Buffer (0 .. Count - 1), Got);
         for I in 0 .. Got - 1 loop
            H := (H xor Unsigned_64 (Buffer (I))) * FNV_Prime;
         end loop;
         return H;
      end if;
      return (H xor Ref.Bits) * FNV_Prime;
   end Hash_Of;

   function Note_Change (St : in out State_Record; Ref : Value_Ref; Size : Unsigned_64) return Boolean is
      --  A value in memory is known by its address and type; one held in a
      --  register (a local at -O2) by its name and type.
      function Name_Key return Unsigned_64 is
         H : Unsigned_64 := FNV_Offset;
      begin
         for C of Image (Ref.Name) loop
            H := (H xor Unsigned_64 (Character'Pos (C))) * FNV_Prime;
         end loop;
         return H;
      end Name_Key;
      Identity : constant Unsigned_64 :=
        (if Ref.Location = In_Memory then Unsigned_64 (Ref.Address) else Name_Key);
      Key : constant Unsigned_64 := Identity xor Shift_Left (Unsigned_64 (Ref.Of_Type), 20);
      Slot : constant Natural := Natural ((Key * FNV_Prime) mod Change_Slots);
      H : constant Unsigned_64 := Hash_Of (St, Ref, Size);
      E : Change_Entry renames St.Changes (Slot);
      Changed : Boolean := False;
   begin
      if Ref.Location not in In_Memory | Register_Value then
         return False;
      end if;
      if E.Used and then E.Address = Target_Address (Identity) and then E.Of_Type = Ref.Of_Type then
         Changed := E.Hash /= H;
      end if;
      E := (Address => Target_Address (Identity), Of_Type => Ref.Of_Type, Hash => H, Used => True);
      return Changed;
   end Note_Change;

   ---------------------------------------------------------------------------
   --  Members of a record (with the active variant's)
   ---------------------------------------------------------------------------
   type Member is record
      Die_Offset : BY.Offset := 0;
      Offset_In  : Unsigned_64 := 0;
      Of_Type    : Type_Key := No_Type;
      Is_Discriminant : Boolean := False;
   end record;
   subtype Member_Count is Natural range 0 .. Member_Ceiling;
   type Member_Array is array (1 .. Member_Ceiling) of Member;
   type Member_List is record
      Items : Member_Array;
      Count : Member_Count := 0;
      Has_Variant : Boolean := False;
      Variant : Name_Text;
   end record;

   function Member_Offset (D : DW.Die) return Unsigned_64 is
      I : constant DW.Attribute_Count := DW.Find (D, DW.AT_Data_Member_Location);
   begin
      if I /= 0 and then D.Attrs (I).Class in DW.Constant_Value | DW.Signed_Value then
         return D.Attrs (I).Value;
      end if;
      return 0;
   end Member_Offset;

   procedure Enum_Literal (St : State_Record; Enum_Type : DW.Die; Value : Integer_64; Text : out Name_Text);

   --  Whether a DW_AT_discr_list (labels and ranges, as unsigned LEB128)
   --  holds Value.
   function Discr_List_Holds (St : State_Record; A : DW.Attribute; Value : Unsigned_64) return Boolean is
      DSC_Label : constant := 0;
      DSC_Range : constant := 1;
      C : BY.Cursor := BY.Make (A.At_Byte, A.At_Byte + A.Size);
      Kind, Low, High : Unsigned_64;
   begin
      while C.Ok and then C.At_Byte < C.Limit loop
         BY.U8 (St.Img.Data.all, C, Kind);
         BY.ULEB (St.Img.Data.all, C, Low);
         if Kind = DSC_Range then
            BY.ULEB (St.Img.Data.all, C, High);
            if C.Ok and then Value >= Low and then Value <= High then
               return True;
            end if;
         elsif Kind = DSC_Label then
            if C.Ok and then Value = Low then
               return True;
            end if;
         else
            return False;
         end if;
      end loop;
      return False;
   end Discr_List_Holds;

   procedure Members_Of (St : State_Record; Struct : DW.Die; Ref : Value_Ref; List : out Member_List) is
      procedure Walk (First : BY.Offset; Depth : Natural) is
         Cur : BY.Offset := First;
         Child : DW.Die;
         Good : Boolean;
      begin
         loop
            DW.Read_Die (St.Img.all, Cur, Child, Good);
            exit when not Good or else Child.Tag = 0;
            if Child.Tag = DW.TAG_Member and then List.Count < Member_Ceiling then
               List.Count := List.Count + 1;
               List.Items (List.Count) :=
                 (Die_Offset => Child.Offset, Offset_In => Member_Offset (Child),
                  Of_Type => Type_Of (Child), Is_Discriminant => False);
            elsif Child.Tag = DW.TAG_Variant_Part and then Depth < 4 then
               List.Has_Variant := True;
               declare
                  Discr_At : BY.Offset;
                  Discr : DW.Die;
                  Discr_Value : Unsigned_64 := 0;
                  Have_Discr : Boolean := False;
                  Vcur : BY.Offset := Child.Next;
                  V : DW.Die;
                  Chosen : BY.Offset := 0;
                  Default_Variant : BY.Offset := 0;
               begin
                  if DW.Reference_Of (Child, DW.AT_Discr, Discr_At) then
                     DW.Read_Die (St.Img.all, Discr_At, Discr, Good);
                     if Good then
                        declare
                           Size : constant Unsigned_64 := Size_Of_Type (St, Type_Of (Discr));
                        begin
                           if Size in 1 .. Value_Bytes_Ceiling then
                              Value_Word (St, Ref, Member_Offset (Discr), Positive (Size), Discr_Value, Have_Discr);
                           end if;
                        end;
                        for I in 1 .. List.Count loop
                           if List.Items (I).Die_Offset = Discr_At then
                              List.Items (I).Is_Discriminant := True;
                           end if;
                        end loop;
                        --  The variant's name: the discriminant's literal.
                        declare
                           Core : DW.Die;
                           Name : Name_Text;
                           Stripped : Boolean;
                        begin
                           Strip (St, Type_Of (Discr), Core, Name, Stripped);
                           if Stripped and then Have_Discr and then Core.Tag = DW.TAG_Enumeration_Type then
                              Enum_Literal (St, Core, BY.To_Signed (Discr_Value), List.Variant);
                           elsif Have_Discr then
                              List.Variant := To_Name (Image_Of (Discr_Value));
                           end if;
                        end;
                     end if;
                  end if;
                  if Child.Children then
                     loop
                        DW.Read_Die (St.Img.all, Vcur, V, Good);
                        exit when not Good or else V.Tag = 0;
                        if V.Tag = DW.TAG_Variant then
                           declare
                              I : constant DW.Attribute_Count := DW.Find (V, DW.AT_Discr_Value);
                              L : constant DW.Attribute_Count := DW.Find (V, DW.AT_Discr_List);
                              Want : Integer_64;
                           begin
                              if I = 0 and then L = 0 then
                                 Default_Variant := V.Next;   --  "when others"
                              elsif I /= 0 and then Have_Discr and then DW.Constant_Of (V.Attrs (I), Want)
                                and then (Unsigned_64'(BY.To_Unsigned (Want)) = Discr_Value
                                          or else V.Attrs (I).Value = Discr_Value)
                              then
                                 Chosen := V.Next;
                              elsif L /= 0 and then Have_Discr and then V.Attrs (L).Class = DW.Block_Value
                                and then Discr_List_Holds (St, V.Attrs (L), Discr_Value)
                              then
                                 Chosen := V.Next;   --  a niche's value or range
                              end if;
                           end;
                        end if;
                        Vcur := DW.Sibling_Of (St.Img.all, V);
                     end loop;
                  end if;
                  if Chosen = 0 and then Have_Discr then
                     Chosen := Default_Variant;
                  end if;
                  if Chosen /= 0 then
                     Walk (Chosen, Depth + 1);
                  end if;
               end;
            end if;
            Cur := DW.Sibling_Of (St.Img.all, Child);
         end loop;
      end Walk;
   begin
      List := (others => <>);
      if Struct.Children then
         Walk (Struct.Next, 0);
      end if;
   end Members_Of;

   procedure Enum_Literal (St : State_Record; Enum_Type : DW.Die; Value : Integer_64; Text : out Name_Text) is
      Cur : BY.Offset := Enum_Type.Next;
      Child : DW.Die;
      Good : Boolean;
      V : Integer_64;
   begin
      Text := To_Name (Image_Of (Value));
      if not Enum_Type.Children then
         return;
      end if;
      loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = DW.TAG_Enumerator then
            declare
               I : constant DW.Attribute_Count := DW.Find (Child, DW.AT_Const_Value);
            begin
               if I /= 0 and then DW.Constant_Of (Child.Attrs (I), V) and then
                 (V = Value or else
                  (Child.Attrs (I).Class = DW.Constant_Value and then Child.Attrs (I).Value = BY.To_Unsigned (Value)))
               then
                  Text := To_Name (Simple_Name (DW.Name_Of (St.Img.all, Child)));
                  return;
               end if;
            end;
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
   end Enum_Literal;

   --  GNAT's fat pointer to an unconstrained array: {P_ARRAY, P_BOUNDS}.
   function Is_Fat_Pointer (St : State_Record; D : DW.Die; Array_Member, Bounds_Member : out Member) return Boolean is
      List : Member_List;
      Found_Array, Found_Bounds : Boolean := False;
      M : DW.Die;
      Good : Boolean;
   begin
      Array_Member := (others => <>);
      Bounds_Member := (others => <>);
      if D.Tag /= DW.TAG_Structure_Type then
         return False;
      end if;
      Members_Of (St, D, Null_Ref, List);
      if List.Count /= 2 then
         return False;
      end if;
      for I in 1 .. List.Count loop
         DW.Read_Die (St.Img.all, List.Items (I).Die_Offset, M, Good);
         if Good then
            declare
               N : constant String := DW.Name_Of (St.Img.all, M);
            begin
               if N = "P_ARRAY" or else N = "p_array" then
                  Array_Member := List.Items (I);
                  Found_Array := True;
               elsif N = "P_BOUNDS" or else N = "p_bounds" then
                  Bounds_Member := List.Items (I);
                  Found_Bounds := True;
               end if;
            end;
         end if;
      end loop;
      return Found_Array and then Found_Bounds;
   end Is_Fat_Pointer;

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   --  Rust layouts (rustc's DWARF): forms recognised by the type's name and
   --  members, read through the members rustc emits, never by guessed
   --  offsets.
   ---------------------------------------------------------------------------
   type Rust_Form is
     (Not_Special, Str_Form, Slice_Form, Vec_Form, String_Form, Rc_Form, Arc_Form,
      Map_Form, Wrapper_Form);

   function Direct_Members (St : State_Record; Struct : DW.Die) return Natural is
      Cur : BY.Offset := Struct.Next;
      Child : DW.Die;
      Good : Boolean;
      Count : Natural := 0;
   begin
      if not Struct.Children then
         return 0;
      end if;
      for Step in 1 .. Member_Ceiling loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = DW.TAG_Member then
            Count := Count + 1;
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
      return Count;
   end Direct_Members;

   --  Struct's member named Name.
   procedure Member_Named (St : State_Record; Struct : DW.Die; Name : String; M : out Member; Found : out Boolean) is
      Cur : BY.Offset := Struct.Next;
      Child : DW.Die;
      Good : Boolean;
   begin
      M := (others => <>);
      Found := False;
      if not Struct.Children then
         return;
      end if;
      for Step in 1 .. Member_Ceiling loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = DW.TAG_Member and then DW.Name_Of (St.Img.all, Child) = Name then
            M := (Die_Offset => Child.Offset, Offset_In => Member_Offset (Child),
                  Of_Type => Type_Of (Child), Is_Discriminant => False);
            Found := True;
            return;
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
   end Member_Named;

   --  The generic's type parameter named Name (Vec<T>'s T).
   function Template_Parameter (St : State_Record; Struct : DW.Die; Name : String) return Type_Key is
      Cur : BY.Offset := Struct.Next;
      Child : DW.Die;
      Good : Boolean;
   begin
      if not Struct.Children then
         return No_Type;
      end if;
      for Step in 1 .. Member_Ceiling loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = TAG_Template_Type_Param and then DW.Name_Of (St.Img.all, Child) = Name then
            return Type_Of (Child);
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
      return No_Type;
   end Template_Parameter;

   --  The first pointer inside a type (through its first members, depth
   --  first): where it is, and what it points at. RawVec's
   --  inner.ptr.pointer.pointer, NonNull's pointer.
   procedure Pointer_Leaf
     (St : State_Record; Key : Type_Key; Depth : Natural;
      Offset_In : out Unsigned_64; Target_Type : out Type_Key; Found : out Boolean)
   is
      Core : DW.Die;
      Name : Name_Text;
      Good : Boolean;
      Cur : BY.Offset;
      Child : DW.Die;
   begin
      Offset_In := 0;
      Target_Type := No_Type;
      Found := False;
      if Depth > Leaf_Depth_Ceiling then
         return;
      end if;
      Strip (St, Key, Core, Name, Good);
      if not Good then
         return;
      end if;
      if Core.Tag in DW.TAG_Pointer_Type | DW.TAG_Reference_Type | DW.TAG_Rvalue_Reference then
         Target_Type := Type_Of (Core);
         Found := True;
         return;
      end if;
      if Core.Tag /= DW.TAG_Structure_Type or else not Core.Children then
         return;
      end if;
      Cur := Core.Next;
      for Step in 1 .. Member_Ceiling loop
         DW.Read_Die (St.Img.all, Cur, Child, Good);
         exit when not Good or else Child.Tag = 0;
         if Child.Tag = DW.TAG_Member then
            declare
               Inner_Offset : Unsigned_64;
            begin
               Pointer_Leaf (St, Type_Of (Child), Depth + 1, Inner_Offset, Target_Type, Found);
               if Found then
                  Offset_In := Member_Offset (Child) + Inner_Offset;
                  return;
               end if;
            end;
         end if;
         Cur := DW.Sibling_Of (St.Img.all, Child);
      end loop;
   end Pointer_Leaf;

   function Rust_Form_Of (St : State_Record; Core : DW.Die) return Rust_Form is
   begin
      if Core.Tag /= DW.TAG_Structure_Type or else not Is_Rust (St, Core.Unit_Index) then
         return Not_Special;
      end if;
      declare
         N : constant String := DW.Name_Of (St.Img.all, Core);
      begin
         if N = "&str" or else N = "&mut str" then
            return Str_Form;
         elsif Starts_With (N, "&[") or else Starts_With (N, "&mut [") then
            return Slice_Form;
         elsif Starts_With (N, "Vec<") then
            return Vec_Form;
         elsif N = "String" then
            return String_Form;
         elsif Starts_With (N, "Rc<") then
            return Rc_Form;
         elsif Starts_With (N, "Arc<") then
            return Arc_Form;
         elsif Starts_With (N, "HashMap<") then
            return Map_Form;
         elsif (Starts_With (N, "UnsafeCell<") or else Starts_With (N, "Cell<")
                or else Starts_With (N, "NonNull<") or else Starts_With (N, "Unique<")
                or else Starts_With (N, "ManuallyDrop<") or else Starts_With (N, "Atomic")
                or else Starts_With (N, "Align"))
           and then Direct_Members (St, Core) = 1
         then
            return Wrapper_Form;
         end if;
      end;
      return Not_Special;
   end Rust_Form_Of;

   --  A word of the value at Offset_In.
   procedure Member_Word
     (St : State_Record; Ref : Value_Ref; Offset_In : Unsigned_64; Value : out Unsigned_64; Good : out Boolean) is
   begin
      Value_Word (St, Ref, Offset_In, Address_Bytes, Value, Good);
   end Member_Word;

   --  A slice-like value's elements: where they are, how many, of what.
   procedure Slice_Of
     (St : State_Record; Ref : Value_Ref; Core : DW.Die; Form : Rust_Form;
      Data : out Target_Address; Length : out Natural; Element : out Type_Key; Good : out Boolean)
   is
      M, L : Member;
      Found, Found_L, Read_Ok : Boolean;
      Word : Unsigned_64;
   begin
      Data := 0;
      Length := 0;
      Element := No_Type;
      Good := False;
      case Form is
         when Str_Form | Slice_Form =>
            Member_Named (St, Core, "data_ptr", M, Found);
            Member_Named (St, Core, "length", L, Found_L);
            if not (Found and Found_L) then
               return;
            end if;
            declare
               PCore : DW.Die;
               PName : Name_Text;
               Stripped : Boolean;
            begin
               Strip (St, M.Of_Type, PCore, PName, Stripped);
               Element := (if Stripped then Type_Of (PCore) else No_Type);
            end;
            Member_Word (St, Ref, M.Offset_In, Word, Read_Ok);
            Data := Target_Address (Word);
            Member_Word (St, Ref, L.Offset_In, Word, Good);
            Good := Good and then Read_Ok and then Word <= Rust_Length_Ceiling;
            Length := (if Good then Natural (Word) else 0);
         when Vec_Form =>
            Element := Template_Parameter (St, Core, "T");
            Member_Named (St, Core, "buf", M, Found);
            Member_Named (St, Core, "len", L, Found_L);
            if not (Found and Found_L) then
               return;
            end if;
            declare
               Leaf : Unsigned_64;
               Ignore : Type_Key;
               Leaf_Found : Boolean;
            begin
               Pointer_Leaf (St, M.Of_Type, 0, Leaf, Ignore, Leaf_Found);
               if not Leaf_Found then
                  return;
               end if;
               Member_Word (St, Ref, M.Offset_In + Leaf, Word, Read_Ok);
               Data := Target_Address (Word);
            end;
            Member_Word (St, Ref, L.Offset_In, Word, Good);
            Good := Good and then Read_Ok and then Word <= Rust_Length_Ceiling;
            Length := (if Good then Natural (Word) else 0);
         when String_Form =>
            Member_Named (St, Core, "vec", M, Found);
            if not Found then
               return;
            end if;
            declare
               VCore : DW.Die;
               VName : Name_Text;
               Stripped : Boolean;
               Inner : Value_Ref := Ref;
            begin
               Strip (St, M.Of_Type, VCore, VName, Stripped);
               if not Stripped then
                  return;
               end if;
               Inner.Address := Ref.Address + Target_Address (M.Offset_In);
               Inner.Of_Type := M.Of_Type;
               Slice_Of (St, Inner, VCore, Vec_Form, Data, Length, Element, Good);
            end;
         when others =>
            null;
      end case;
   end Slice_Of;

   --  Rc<T> and Arc<T>: the shared value, and its counts.
   procedure Shared_Of
     (St : State_Record; Ref : Value_Ref; Core : DW.Die; Form : Rust_Form;
      Target : out Value_Ref; Strong, Weak : out Unsigned_64; Good : out Boolean)
   is
      M, Value_Member, Strong_Member, Weak_Member : Member;
      Found, F1, F2, F3 : Boolean;
      Leaf, Inner_Address : Unsigned_64;
      Inner_Type : Type_Key;
      ICore : DW.Die;
      IName : Name_Text;
   begin
      Target := Null_Ref;
      Strong := 0;
      Weak := 0;
      Good := False;
      Member_Named (St, Core, "ptr", M, Found);
      if not Found then
         return;
      end if;
      Pointer_Leaf (St, M.Of_Type, 0, Leaf, Inner_Type, Found);
      if not Found then
         return;
      end if;
      Member_Word (St, Ref, M.Offset_In + Leaf, Inner_Address, Found);
      Strip (St, Inner_Type, ICore, IName, F1);
      if not (Found and F1) or else Inner_Address = 0 then
         return;
      end if;
      Member_Named (St, ICore, (if Form = Rc_Form then "value" else "data"), Value_Member, F1);
      Member_Named (St, ICore, "strong", Strong_Member, F2);
      Member_Named (St, ICore, "weak", Weak_Member, F3);
      if not F1 then
         return;
      end if;
      if F2 then
         Read_Word (St, Inner_Address + Strong_Member.Offset_In, Address_Bytes, Strong, F2);
      end if;
      if F3 then
         Read_Word (St, Inner_Address + Weak_Member.Offset_In, Address_Bytes, Weak, F3);
      end if;
      Target := (Name => To_Name ("*" & Image (Ref.Name)), Location => In_Memory,
                 Address => Target_Address (Inner_Address + Value_Member.Offset_In),
                 Of_Type => Value_Member.Of_Type, others => <>);
      Good := True;
   end Shared_Of;

   --  hashbrown's table under a HashMap (base.table: RawTable<(K, V)>,
   --  its .table: RawTableInner): the control bytes, how many buckets, how
   --  many items, and the bucket type. Bucket I lies just below the control
   --  bytes: at Ctrl - (I + 1) * size, and is full when its control byte's
   --  top bit is clear.
   Map_Bucket_Ceiling : constant := 4096;
   Control_Empty_Bit  : constant := 16#80#;
   procedure Map_Of
     (St : State_Record; Ref : Value_Ref; Core : DW.Die;
      Ctrl : out Unsigned_64; Buckets, Items : out Natural; Element : out Type_Key; Good : out Boolean)
   is
      Base_M, Table_M, Inner_M, Mask_M, Ctrl_M, Items_M : Member;
      F1, F2, F3, F4, F5, F6, S1, S2, S3 : Boolean;
      Base_Core, Table_Core, Inner_Core : DW.Die;
      N : Name_Text;
      Mask, Count, Leaf : Unsigned_64;
      Ignore : Type_Key;
      Offset : Unsigned_64;
   begin
      Ctrl := 0;
      Buckets := 0;
      Items := 0;
      Element := No_Type;
      Good := False;
      Member_Named (St, Core, "base", Base_M, F1);
      if not F1 then
         return;
      end if;
      Strip (St, Base_M.Of_Type, Base_Core, N, S1);
      Member_Named (St, Base_Core, "table", Table_M, F2);
      if not (S1 and F2) then
         return;
      end if;
      Strip (St, Table_M.Of_Type, Table_Core, N, S2);
      Element := Template_Parameter (St, Table_Core, "T");
      Member_Named (St, Table_Core, "table", Inner_M, F3);
      if not (S2 and F3) or else Element = No_Type then
         return;
      end if;
      Strip (St, Inner_M.Of_Type, Inner_Core, N, S3);
      Member_Named (St, Inner_Core, "bucket_mask", Mask_M, F4);
      Member_Named (St, Inner_Core, "ctrl", Ctrl_M, F5);
      Member_Named (St, Inner_Core, "items", Items_M, F6);
      if not (S3 and F4 and F5 and F6) then
         return;
      end if;
      Offset := Base_M.Offset_In + Table_M.Offset_In + Inner_M.Offset_In;
      Pointer_Leaf (St, Ctrl_M.Of_Type, 0, Leaf, Ignore, F5);
      Member_Word (St, Ref, Offset + Mask_M.Offset_In, Mask, F4);
      Member_Word (St, Ref, Offset + Ctrl_M.Offset_In + Leaf, Ctrl, F5);
      Member_Word (St, Ref, Offset + Items_M.Offset_In, Count, F6);
      if not (F4 and F5 and F6) or else Mask >= Map_Bucket_Ceiling or else Count > Mask + 1 then
         return;
      end if;
      Buckets := Natural (Mask + 1);
      Items := Natural (Count);
      Good := True;
   end Map_Of;

   --  The I-th full bucket of a map (0-based), as an element reference.
   procedure Map_Entry
     (St : State_Record; Ctrl : Unsigned_64; Buckets : Natural; Element : Type_Key; Index : Natural;
      Child : out Value_Ref)
   is
      Controls : DT.Byte_Buffer (0 .. Map_Bucket_Ceiling - 1);
      Got : Natural;
      Seen : Natural := 0;
      Size : constant Unsigned_64 := Size_Of_Type (St, Element);
   begin
      Child := Null_Ref;
      if Buckets = 0 or else Buckets > Map_Bucket_Ceiling or else Size = 0 then
         return;
      end if;
      Read_Memory (St, Target_Address (Ctrl), Controls (0 .. Buckets - 1), Got);
      for B in 0 .. Got - 1 loop
         if (Controls (B) and Control_Empty_Bit) = 0 then
            if Seen = Index then
               Child := (Name => To_Name ("[" & Image_Of (Unsigned_64 (Index)) & "]"), Location => In_Memory,
                         Address => Target_Address (Ctrl - (Unsigned_64 (B) + 1) * Size),
                         Of_Type => Element, others => <>);
               return;
            end if;
            Seen := Seen + 1;
         end if;
      end loop;
   end Map_Entry;

   --  A wrapper's one member (UnsafeCell's value, Atomic's v, NonNull's pointer).
   function Wrapped (St : State_Record; Ref : Value_Ref; Core : DW.Die) return Value_Ref is
      List : Member_List;
      R : Value_Ref := Ref;
   begin
      Members_Of (St, Core, Ref, List);
      if List.Count /= 1 then
         return Ref;
      end if;
      R.Of_Type := List.Items (1).Of_Type;
      if Ref.Location = In_Memory then
         R.Address := Ref.Address + Target_Address (List.Items (1).Offset_In);
      elsif Ref.Location = Register_Value and then List.Items (1).Offset_In < Value_Bytes_Ceiling then
         R.Bits := Shift_Right (Ref.Bits, Natural (List.Items (1).Offset_In) * 8);
      end if;
      return R;
   end Wrapped;

   --  A Rust enum's active variant: its name and the struct of its fields
   --  (Members_Of keeps the variant's one member, typed by that struct).
   procedure Rust_Variant
     (St : State_Record; Core : DW.Die; Ref : Value_Ref;
      Name : out Name_Text; Fields : out DW.Die; Offset_In : out Unsigned_64; Good : out Boolean)
   is
      List : Member_List;
      M : DW.Die;
      FName : Name_Text;
   begin
      Name := (others => <>);
      Fields := (others => <>);
      Offset_In := 0;
      Good := False;
      Members_Of (St, Core, Ref, List);
      for I in 1 .. List.Count loop
         DW.Read_Die (St.Img.all, List.Items (I).Die_Offset, M, Good);
         if Good and then DW.Find (M, AT_Artificial) = 0 then
            Name := To_Name (DW.Name_Of (St.Img.all, M));
            Offset_In := List.Items (I).Offset_In;
            Strip (St, List.Items (I).Of_Type, Fields, FName, Good);
            return;
         end if;
      end loop;
      Good := False;
   end Rust_Variant;

   function Kind_Of (St : State_Record; Key : Type_Key) return Value_Kind is
      Core : DW.Die;
      Name : Name_Text;
      Good : Boolean;
      A, Bm : Member;
   begin
      Strip (St, Key, Core, Name, Good);
      if not Good then
         return Unknown;
      end if;
      case Core.Tag is
         when DW.TAG_Base_Type | DW.TAG_Subrange_Type => return Scalar;
         when DW.TAG_Enumeration_Type => return Enum;
         when DW.TAG_Pointer_Type | DW.TAG_Reference_Type | DW.TAG_Rvalue_Reference => return Pointer;
         when DW.TAG_Array_Type => return Array_Value;
         when DW.TAG_Structure_Type | DW.TAG_Union_Type =>
            case Rust_Form_Of (St, Core) is
               when Str_Form | Slice_Form | Vec_Form | String_Form | Map_Form => return Slice;
               when Rc_Form | Arc_Form => return Pointer;
               when Wrapper_Form =>
                  declare
                     List : Member_List;
                  begin
                     Members_Of (St, Core, Null_Ref, List);
                     return (if List.Count = 1 then Kind_Of (St, List.Items (1).Of_Type) else Record_Value);
                  end;
               when Not_Special => null;
            end case;
            if Is_Fat_Pointer (St, Core, A, Bm) then
               return Pointer;
            end if;
            declare
               List : Member_List;
            begin
               Members_Of (St, Core, Null_Ref, List);
               return (if List.Has_Variant
                       then (if Is_Rust (St, Core.Unit_Index) then Rust_Enum else Variant_Record)
                       else Record_Value);
            end;
         when others => return Unknown;
      end case;
   end Kind_Of;

   function Member_Ref (St : State_Record; Parent : Value_Ref; M : Member; Name : String) return Value_Ref is
      R : Value_Ref := (Name => To_Name (Name), Of_Type => M.Of_Type, Epoch => Parent.Epoch, others => <>);
   begin
      pragma Unreferenced (St);
      case Parent.Location is
         when In_Memory =>
            R.Location := In_Memory;
            R.Address := Parent.Address + Target_Address (M.Offset_In);
         when Register_Value =>
            if M.Offset_In < Value_Bytes_Ceiling then
               R.Location := Register_Value;
               R.Bits := Shift_Right (Parent.Bits, Natural (M.Offset_In) * 8);
            else
               R.Location := Unavailable;
            end if;
         when others =>
            R.Location := Parent.Location;
      end case;
      return R;
   end Member_Ref;

   --  The object a pointer value points at (Has False when null or unreadable).
   procedure Pointee
     (St : State_Record; Ref : Value_Ref; Core : DW.Die; Target : out Value_Ref; Has : out Boolean)
   is
      Address : Unsigned_64;
      Good : Boolean;
      A, Bm : Member;
   begin
      Target := Null_Ref;
      Has := False;
      case Rust_Form_Of (St, Core) is
         when Rc_Form | Arc_Form =>
            declare
               Strong, Weak : Unsigned_64;
            begin
               Shared_Of (St, Ref, Core, Rust_Form_Of (St, Core), Target, Strong, Weak, Has);
               Has := Has and then Target.Of_Type /= No_Type;
            end;
            return;
         when Wrapper_Form =>
            declare
               Inner : constant Value_Ref := Wrapped (St, Ref, Core);
               ICore : DW.Die;
               IName : Name_Text;
               Stripped : Boolean;
            begin
               Strip (St, Inner.Of_Type, ICore, IName, Stripped);
               if Stripped and then Inner.Of_Type /= Ref.Of_Type then
                  Pointee (St, Inner, ICore, Target, Has);
               end if;
            end;
            return;
         when others =>
            null;
      end case;
      if Is_Fat_Pointer (St, Core, A, Bm) then
         declare
            Array_Ptr, Bounds_Ptr, Lo, Hi : Unsigned_64;
            G1, G2, G3, G4 : Boolean;
            Ptr_Core : DW.Die;
            Ptr_Name : Name_Text;
            Stripped : Boolean;
            Bound_Size : Unsigned_64 := 4;
         begin
            Value_Word (St, Ref, A.Offset_In, Address_Bytes, Array_Ptr, G1);
            Value_Word (St, Ref, Bm.Offset_In, Address_Bytes, Bounds_Ptr, G2);
            if not (G1 and G2) or else Array_Ptr = 0 or else Bounds_Ptr = 0 then
               return;
            end if;
            --  The bounds record {LB0, UB0}: its component size.
            declare
               BCore : DW.Die;
               BName : Name_Text;
               BGood : Boolean;
               Target_Type : BY.Offset;
            begin
               Strip (St, Bm.Of_Type, BCore, BName, BGood);
               if BGood and then DW.Reference_Of (BCore, DW.AT_Type, Target_Type) then
                  Strip (St, Type_Key (Target_Type), BCore, BName, BGood);
                  if BGood then
                     Bound_Size := Unsigned_64'Max (1, Byte_Size (BCore) / 2);
                  end if;
               end if;
            end;
            Read_Word (St, Bounds_Ptr, Positive (Bound_Size), Lo, G3);
            Read_Word (St, Bounds_Ptr + Bound_Size, Positive (Bound_Size), Hi, G4);
            Strip (St, A.Of_Type, Ptr_Core, Ptr_Name, Stripped);
            if not (G3 and G4 and Stripped) then
               return;
            end if;
            Target := (Name => To_Name (Image (Ref.Name) & ".all"), Location => In_Memory,
                       Address => Target_Address (Array_Ptr), Of_Type => Type_Of (Ptr_Core),
                       Has_Bounds => True,
                       Low => BY.Sign_Extend (Lo, Natural (Bound_Size) * 8),
                       High => BY.Sign_Extend (Hi, Natural (Bound_Size) * 8),
                       Epoch => 0, Bits => 0);
            Has := True;
            return;
         end;
      end if;
      Value_Word (St, Ref, 0, Address_Bytes, Address, Good);
      if not Good or else Address = 0 then
         return;
      end if;
      Target := (Name => To_Name (if Is_Rust (St, Core.Unit_Index) then "*" & Image (Ref.Name)
                                  else Image (Ref.Name) & ".all"),
                 Location => In_Memory,
                 Address => Target_Address (Address), Of_Type => Type_Of (Core), others => <>);
      Has := Target.Of_Type /= No_Type;
   end Pointee;

   function Scalar_Text (St : State_Record; Core : DW.Die; Ref : Value_Ref;
                         Size : Unsigned_64; Raw : out Unsigned_64; Signed : out Integer_64;
                         Good : out Boolean) return String
   is
      Encoding : Unsigned_64 := DW.ATE_Signed;
      Base : DW.Die := Core;
      Name : Name_Text;
      Stripped : Boolean;
   begin
      Raw := 0;
      Signed := 0;
      if Core.Tag = DW.TAG_Subrange_Type then
         Strip (St, Type_Of (Core), Base, Name, Stripped);
         if not Stripped then
            Base := Core;
         end if;
      end if;
      declare
         I : constant DW.Attribute_Count := DW.Find (Base, DW.AT_Encoding);
      begin
         if I /= 0 then
            Encoding := Base.Attrs (I).Value;
         end if;
      end;
      if Size not in 1 .. Value_Bytes_Ceiling then
         Good := False;
         return "<" & Image_Of (Size) & " bytes>";
      end if;
      Value_Word (St, Ref, 0, Positive (Size), Raw, Good);
      if not Good then
         return "<unreadable>";
      end if;
      Signed := BY.Sign_Extend (Raw, Natural (Size) * 8);
      case Encoding is
         when DW.ATE_Boolean =>
            return (if Raw /= 0 then "True" else "False");
         when DW.ATE_Signed_Char | DW.ATE_Unsigned_Char | DW.ATE_UTF =>
            if Raw in 32 .. 126 then
               return "'" & Character'Val (Raw) & "'";
            end if;
            return "Character'Val (" & Image_Of (Raw) & ")";
         when DW.ATE_Unsigned | DW.ATE_Address =>
            Signed := BY.To_Signed (Raw);
            return Image_Of (Raw);
         when DW.ATE_Float =>
            return "<float " & Hex_Of (Raw) & ">";
         when others =>
            return Image_Of (Signed);
      end case;
   end Scalar_Text;

   procedure Make_Node (St : in out State_Record; Ref : Value_Ref; Node : out Value_Node; Depth : Natural);

   --  A short display of a value, for an enclosing record's or array's text.
   function Short_Text (St : in out State_Record; Ref : Value_Ref; Depth : Natural) return String is
      N : Value_Node;
   begin
      Make_Node (St, Ref, N, Depth + 1);
      return Image (N.Display);
   end Short_Text;

   Nesting_Ceiling : constant := 3;

   --  A display under construction, cut at the display capacity.
   type Text_Buffer is record
      Text : String (1 .. Display_Capacity + 1) := [others => ' '];
      Length : Natural := 0;
   end record;
   procedure Put (B : in out Text_Buffer; S : String) is
      Take : constant Natural := Natural'Min (S'Length, B.Text'Length - B.Length);
   begin
      B.Text (B.Length + 1 .. B.Length + Take) := S (S'First .. S'First + Take - 1);
      B.Length := B.Length + Take;
   end Put;

   --  Bytes as a quoted Rust string (control bytes as "."), at most Shown.
   function Quoted (St : State_Record; Data : Target_Address; Length : Natural) return String is
      Shown : constant Natural := Natural'Min (Length, Display_Capacity - 2);
      Buffer : DT.Byte_Buffer (0 .. Display_Capacity - 1);
      Got : Natural := 0;
      Result : String (1 .. Shown + 2);
   begin
      if Shown > 0 then
         Read_Memory (St, Data, Buffer (0 .. Shown - 1), Got);
      end if;
      Result (1) := '"';
      for I in 0 .. Shown - 1 loop
         Result (I + 2) := (if I < Got and then Buffer (I) in 32 .. 126 then Character'Val (Buffer (I)) else '.');
      end loop;
      Result (Shown + 2) := '"';
      return Result & (if Length > Shown then "~" else "");
   end Quoted;

   --  Element I of a slice-like value.
   function Element_Ref (St : State_Record; Data : Target_Address; Element : Type_Key; I : Natural) return Value_Ref is
     ((Name => To_Name ("[" & Image_Of (Unsigned_64 (I)) & "]"), Location => In_Memory,
       Address => Data + Target_Address (Unsigned_64 (I) * Size_Of_Type (St, Element)),
       Of_Type => Element, others => <>));

   --  A Rust struct's or variant's fields (Struct's members, the value at Ref).
   function Field_Refs_Total (St : State_Record; Struct : DW.Die; Ref : Value_Ref) return Natural is
      List : Member_List;
   begin
      Members_Of (St, Struct, Ref, List);
      return List.Count;
   end Field_Refs_Total;

   procedure Field_Ref
     (St : State_Record; Struct : DW.Die; Ref : Value_Ref; Index : Natural; Child : out Value_Ref)
   is
      List : Member_List;
      M : DW.Die;
      Good : Boolean;
   begin
      Child := Null_Ref;
      Members_Of (St, Struct, Ref, List);
      if Index < List.Count then
         DW.Read_Die (St.Img.all, List.Items (Index + 1).Die_Offset, M, Good);
         Child := Member_Ref (St, Ref, List.Items (Index + 1),
                              (if Good then Field_Display_Name (St, M.Unit_Index, DW.Name_Of (St.Img.all, M))
                               else "?"));
      end if;
   end Field_Ref;


   --  Fields as Rust writes them: { a: 1, b: 2 } or, for a tuple, (1, 2).
   procedure Put_Fields (St : in out State_Record; B : in out Text_Buffer; Struct : DW.Die;
                         Ref : Value_Ref; Depth : Natural; Total : Natural) is
      Child : Value_Ref;
      Tuple : Boolean := True;
   begin
      for I in 0 .. Total - 1 loop
         Field_Ref (St, Struct, Ref, I, Child);
         Tuple := Tuple and then Child.Name.Length > 0 and then Image (Child.Name) (1) in '0' .. '9';
      end loop;
      Put (B, (if Tuple then "(" else "{ "));
      for I in 0 .. Total - 1 loop
         Field_Ref (St, Struct, Ref, I, Child);
         if I > 0 then
            Put (B, ", ");
         end if;
         if not Tuple then
            Put (B, Image (Child.Name) & ": ");
         end if;
         Put (B, Short_Text (St, Child, Depth));
         exit when B.Length > Display_Capacity;
      end loop;
      Put (B, (if Tuple then ")" else " }"));
   end Put_Fields;

   procedure Rust_Node
     (St : in out State_Record; Ref : Value_Ref; Core : DW.Die; Type_Name : Name_Text;
      Node : in out Value_Node; Depth : Natural)
   is
      Form : constant Rust_Form := Rust_Form_Of (St, Core);
      B : Text_Buffer;
      Data : Target_Address;
      Length : Natural;
      Element : Type_Key;
      Good : Boolean;
   begin
      case Form is
         when Wrapper_Form =>
            declare
               Inner : Value_Ref := Wrapped (St, Ref, Core);
               Changed : constant Boolean := Node.Changed;
            begin
               Inner.Name := Ref.Name;
               Make_Node (St, Inner, Node, Depth + 1);
               Node.Self := Ref;
               Node.Type_Name := Type_Name;
               Node.Address := Ref.Address;
               Node.Changed := Changed;
            end;
            return;
         when Str_Form | String_Form =>
            Slice_Of (St, Ref, Core, Form, Data, Length, Element, Good);
            Node.Display := To_Display (if Good then Quoted (St, Data, Length) else "<unreadable>");
            Node.Kind := Slice;
         when Slice_Form | Vec_Form =>
            Slice_Of (St, Ref, Core, Form, Data, Length, Element, Good);
            Node.Kind := Slice;
            if not Good then
               Node.Display := To_Display ("<unreadable>");
               return;
            end if;
            Node.Child_Count := Length;
            Put (B, "len " & Image_Of (Unsigned_64 (Length)) & ": [");
            if Depth < Nesting_Ceiling then
               for I in 0 .. Natural'Min (Length, Display_Elements) - 1 loop
                  if I > 0 then
                     Put (B, ", ");
                  end if;
                  Put (B, Short_Text (St, Element_Ref (St, Data, Element, I), Depth));
                  exit when B.Length > Display_Capacity;
               end loop;
               if Length > Display_Elements then
                  Put (B, ", ...");
               end if;
            elsif Length > 0 then
               Put (B, "...");
            end if;
            Put (B, "]");
            Node.Display := To_Display (B.Text (1 .. B.Length));
         when Map_Form =>
            declare
               Ctrl : Unsigned_64;
               Buckets, Items : Natural;
               Child : Value_Ref;
            begin
               Map_Of (St, Ref, Core, Ctrl, Buckets, Items, Element, Good);
               Node.Kind := Slice;
               if not Good then
                  Node.Display := To_Display ("<unreadable>");
                  return;
               end if;
               Node.Child_Count := Items;
               Put (B, "len " & Image_Of (Unsigned_64 (Items)) & ": {");
               for I in 0 .. Natural'Min (Items, Display_Elements) - 1 loop
                  exit when Depth >= Nesting_Ceiling;
                  Map_Entry (St, Ctrl, Buckets, Element, I, Child);
                  if I > 0 then
                     Put (B, ", ");
                  end if;
                  Put (B, Short_Text (St, Child, Depth));
                  exit when B.Length > Display_Capacity;
               end loop;
               if Items > Display_Elements then
                  Put (B, ", ...");
               end if;
               Put (B, "}");
               Node.Display := To_Display (B.Text (1 .. B.Length));
            end;
         when Rc_Form | Arc_Form =>
            declare
               Target : Value_Ref;
               Strong, Weak : Unsigned_64;
               TCore : DW.Die;
               TName : Name_Text;
               Stripped : Boolean;
            begin
               Shared_Of (St, Ref, Core, Form, Target, Strong, Weak, Good);
               Node.Kind := Pointer;
               Node.Has_Target := Good;
               Node.Target := Target;
               if not Good then
                  Node.Display := To_Display ("<unreadable>");
                  return;
               end if;
               Node.Child_Count := 1;
               Strip (St, Target.Of_Type, TCore, TName, Stripped);
               Node.Display := To_Display
                 (Hex_Of (Unsigned_64 (Target.Address)) & " -> " & Image (TName)
                  & " (strong " & Image_Of (Strong) & ", weak " & Image_Of (Weak) & ")");
            end;
         when Not_Special =>
            if Node.Kind = Rust_Enum then
               declare
                  Variant_Name : Name_Text;
                  Fields : DW.Die;
                  Offset : Unsigned_64;
                  Payload : Value_Ref := Ref;
                  Total : Natural;
               begin
                  Rust_Variant (St, Core, Ref, Variant_Name, Fields, Offset, Good);
                  if not Good then
                     Node.Display := To_Display ("<unknown variant>");
                     return;
                  end if;
                  Node.Variant := Variant_Name;
                  Payload.Address := Ref.Address + Target_Address (Offset);
                  Payload.Of_Type := Type_Key (Fields.Offset);
                  Total := Field_Refs_Total (St, Fields, Payload);
                  Node.Child_Count := Total;
                  Put (B, Image (Variant_Name));
                  if Total > 0 and then Depth < Nesting_Ceiling then
                     declare
                        Child : Value_Ref;
                     begin
                        Field_Ref (St, Fields, Payload, 0, Child);
                        if Image (Child.Name) (1) not in '0' .. '9' then
                           Put (B, " ");
                        end if;
                     end;
                     Put_Fields (St, B, Fields, Payload, Depth, Total);
                  elsif Total > 0 then
                     Put (B, "(..)");
                  end if;
                  Node.Display := To_Display (B.Text (1 .. B.Length));
               end;
            else
               declare
                  Total : constant Natural := Field_Refs_Total (St, Core, Ref);
               begin
                  Node.Child_Count := Total;
                  if Depth >= Nesting_Ceiling then
                     Put (B, "{ .. }");
                  else
                     Put_Fields (St, B, Core, Ref, Depth, Total);
                  end if;
                  Node.Display := To_Display (B.Text (1 .. B.Length));
               end;
            end if;
      end case;
   end Rust_Node;

   --  A Rust value's children: elements, the shared value, a variant's
   --  fields, or a struct's (Handled False: not a Rust form).
   procedure Rust_Child
     (St : State_Record; Ref : Value_Ref; Core : DW.Die; Index : Natural;
      Child : out Value_Ref; Total : out Natural; Handled : out Boolean)
   is
      Form : constant Rust_Form := Rust_Form_Of (St, Core);
      Data : Target_Address;
      Length : Natural;
      Element : Type_Key;
      Good : Boolean;
   begin
      Child := Null_Ref;
      Total := 0;
      Handled := True;
      case Form is
         when Str_Form | String_Form =>
            null;
         when Map_Form =>
            declare
               Ctrl : Unsigned_64;
               Buckets, Items : Natural;
            begin
               Map_Of (St, Ref, Core, Ctrl, Buckets, Items, Element, Good);
               if Good then
                  Total := Items;
                  if Index < Items then
                     Map_Entry (St, Ctrl, Buckets, Element, Index, Child);
                  end if;
               end if;
            end;
         when Slice_Form | Vec_Form =>
            Slice_Of (St, Ref, Core, Form, Data, Length, Element, Good);
            if Good then
               Total := Length;
               if Index < Length then
                  Child := Element_Ref (St, Data, Element, Index);
               end if;
            end if;
         when Rc_Form | Arc_Form =>
            declare
               Strong, Weak : Unsigned_64;
            begin
               Shared_Of (St, Ref, Core, Form, Child, Strong, Weak, Good);
               Total := (if Good then 1 else 0);
               if Index >= Total then
                  Child := Null_Ref;
               end if;
            end;
         when Wrapper_Form =>
            Handled := False;
         when Not_Special =>
            if Kind_Of (St, Ref.Of_Type) = Rust_Enum then
               declare
                  Variant_Name : Name_Text;
                  Fields : DW.Die;
                  Offset : Unsigned_64;
                  Payload : Value_Ref := Ref;
               begin
                  Rust_Variant (St, Core, Ref, Variant_Name, Fields, Offset, Good);
                  if Good then
                     Payload.Address := Ref.Address + Target_Address (Offset);
                     Payload.Of_Type := Type_Key (Fields.Offset);
                     Total := Field_Refs_Total (St, Fields, Payload);
                     if Index < Total then
                        Field_Ref (St, Fields, Payload, Index, Child);
                     end if;
                  end if;
               end;
            else
               Handled := False;
            end if;
      end case;
   end Rust_Child;

   procedure Make_Node (St : in out State_Record; Ref : Value_Ref; Node : out Value_Node; Depth : Natural) is
      Core : DW.Die;
      Type_Name : Name_Text;
      Good : Boolean;
   begin
      Node := (Self => Ref, Name => Ref.Name, others => <>);
      Node.Address := Ref.Address;
      case Ref.Location is
         when Optimized_Out =>
            Node.Display := To_Display ("<optimized out>");
         when Unavailable =>
            Node.Display := To_Display ("<not available here>");
         when others =>
            null;
      end case;
      if Ref.Of_Type = No_Type then
         if Node.Display.Length = 0 then
            Node.Display := To_Display ("<no type>");
         end if;
         return;
      end if;
      Strip (St, Ref.Of_Type, Core, Type_Name, Good);
      if not Good then
         Node.Display := To_Display ("<unknown type>");
         return;
      end if;
      Node.Type_Name := Type_Name;
      Node.Kind := Kind_Of (St, Ref.Of_Type);
      Node.Size := Size_Of_Type (St, Ref.Of_Type);
      if Ref.Location in Optimized_Out | Unavailable then
         return;
      end if;
      if Depth = 0 then
         Node.Changed := Note_Change (St, Ref, Node.Size);
      end if;
      if Core.Tag = DW.TAG_Structure_Type and then Is_Rust (St, Core.Unit_Index) then
         Rust_Node (St, Ref, Core, Type_Name, Node, Depth);
         return;
      end if;
      case Node.Kind is
         when Scalar | Enum =>
            declare
               Raw : Unsigned_64;
               Signed : Integer_64;
               Text : constant String := Scalar_Text (St, Core, Ref, Node.Size, Raw, Signed, Good);
               Lo, Hi : Integer_64;
               Range_Die : DW.Die;
               Range_Name : Name_Text;
               Stripped : Boolean;
            begin
               if Node.Kind = Enum and then Good then
                  declare
                     Lit : Name_Text;
                  begin
                     Enum_Literal (St, Core, Signed, Lit);
                     Node.Display := To_Display (Image (Lit));
                  end;
               else
                  Node.Display := To_Display (Text);
               end if;
               --  A ranged subtype (also through a const qualifier).
               Strip (St, Ref.Of_Type, Range_Die, Range_Name, Stripped);
               if Stripped and then Range_Die.Tag = DW.TAG_Subrange_Type then
                  if not Bound_Of (St, Range_Die, DW.AT_Lower_Bound, Lo) then
                     Lo := (if Is_Ada (St, Range_Die.Unit_Index) then Ada_Default_Lower else Other_Default_Lower);
                  end if;
                  if Bound_Of (St, Range_Die, DW.AT_Upper_Bound, Hi) then
                     Node.Has_Range := True;
                     Node.Range_Low := Lo;
                     Node.Range_High := Hi;
                     Node.Out_Of_Range := Good and then (Signed < Lo or else Signed > Hi);
                  end if;
               end if;
            end;
         when Pointer =>
            declare
               Target : Value_Ref;
               Has : Boolean;
               Address : Unsigned_64;
            begin
               Pointee (St, Ref, Core, Target, Has);
               Node.Has_Target := Has;
               Node.Target := Target;
               if Has then
                  Node.Child_Count := 1;
                  declare
                     Target_Core : DW.Die;
                     Target_Name : Name_Text;
                     Stripped : Boolean;
                  begin
                     Strip (St, Target.Of_Type, Target_Core, Target_Name, Stripped);
                     if Node.Type_Name.Length = 0 and then Stripped and then Target_Name.Length > 0 then
                        --  An anonymous access type (an in out parameter's reference).
                        Node.Type_Name := To_Name ("access " & Image (Target_Name));
                     end if;
                     Node.Display := To_Display
                       (Hex_Of (Unsigned_64 (Target.Address))
                        & (if Stripped and then Target_Name.Length > 0 then " -> " & Image (Target_Name) else "")
                        & (if Target.Has_Bounds
                           then " (" & Image_Of (Target.Low) & " .. " & Image_Of (Target.High) & ")" else ""));
                  end;
               else
                  Value_Word (St, Ref, 0, Address_Bytes, Address, Good);
                  Node.Display := To_Display
                    (if Good and then Address = 0 then "null"
                     elsif Good then Hex_Of (Address) else "<unreadable>");
               end if;
            end;
         when Record_Value | Variant_Record =>
            declare
               List : Member_List;
               Text : String (1 .. Display_Capacity + 1) := [others => ' '];
               Length : Natural := 0;
               M : DW.Die;
               procedure Put (S : String) is
                  Take : constant Natural := Natural'Min (S'Length, Text'Length - Length);
               begin
                  Text (Length + 1 .. Length + Take) := S (S'First .. S'First + Take - 1);
                  Length := Length + Take;
               end Put;
            begin
               Members_Of (St, Core, Ref, List);
               Node.Child_Count := List.Count;
               Node.Variant := List.Variant;
               if Depth >= Nesting_Ceiling then
                  Node.Display := To_Display ("(...)");
                  return;
               end if;
               Put ("(");
               for I in 1 .. List.Count loop
                  DW.Read_Die (St.Img.all, List.Items (I).Die_Offset, M, Good);
                  declare
                     Field_Name : constant String :=
                       (if Good then Field_Display_Name (St, M.Unit_Index, DW.Name_Of (St.Img.all, M)) else "?");
                  begin
                     if I > 1 then
                        Put (", ");
                     end if;
                     Put (Field_Name & " => " &
                          Short_Text (St, Member_Ref (St, Ref, List.Items (I), Field_Name), Depth));
                  end;
                  exit when Length > Display_Capacity;
               end loop;
               Put (")");
               Node.Display := To_Display (Text (1 .. Length));
            end;
         when Array_Value =>
            declare
               Shape : constant Array_Shape := Shape_Of (St, Core, Ref);
               Count : constant Natural := Element_Count (Shape);
               Elem_Core : DW.Die;
               Elem_Name : Name_Text;
               Stripped : Boolean;
               Is_Text : Boolean := False;
               Text : String (1 .. Display_Capacity + 1) := [others => ' '];
               Length : Natural := 0;
               procedure Put (S : String) is
                  Take : constant Natural := Natural'Min (S'Length, Text'Length - Length);
               begin
                  Text (Length + 1 .. Length + Take) := S (S'First .. S'First + Take - 1);
                  Length := Length + Take;
               end Put;
            begin
               Node.Child_Count := Count;
               Node.Size := Unsigned_64 (Count) * Shape.Element_Size;
               Strip (St, Shape.Element, Elem_Core, Elem_Name, Stripped);
               if Shape.Known and then Is_Rust (St, Core.Unit_Index) then
                  Node.Type_Name := To_Name
                    ("[" & (if Stripped then Image (Elem_Name) else "?") & "; " & Image_Of (Unsigned_64 (Count)) & "]");
               elsif Shape.Known then
                  Node.Type_Name := To_Name
                    ((if Type_Name.Length > 0 then Image (Type_Name) & " " else "")
                     & "(" & Image_Of (Shape.Low) & " .. " & Image_Of (Shape.High) & ")");
               end if;
               if Stripped and then Elem_Core.Tag = DW.TAG_Base_Type and then Shape.Element_Size = 1 then
                  declare
                     I : constant DW.Attribute_Count := DW.Find (Elem_Core, DW.AT_Encoding);
                  begin
                     Is_Text := I /= 0 and then Elem_Core.Attrs (I).Value in
                       DW.ATE_Unsigned_Char | DW.ATE_Signed_Char | DW.ATE_UTF;
                  end;
               end if;
               if Is_Text and then Ref.Location = In_Memory then
                  declare
                     Shown : constant Natural := Natural'Min (Count, Display_Capacity - 2);
                     Buffer : DT.Byte_Buffer (0 .. Display_Capacity - 1);
                     Got : Natural;
                  begin
                     Read_Memory (St, Ref.Address, Buffer (0 .. Shown - 1), Got);
                     Put ("""");
                     for I in 0 .. Got - 1 loop
                        Put ((if Buffer (I) in 32 .. 126 then [Character'Val (Buffer (I))] else "."));
                     end loop;
                     Put ("""");
                  end;
               elsif Depth >= Nesting_Ceiling then
                  Put ("(...)");
               else
                  Put (if Is_Rust (St, Core.Unit_Index) then "[" else "(");
                  for I in 0 .. Natural'Min (Count, Display_Elements) - 1 loop
                     if I > 0 then
                        Put (", ");
                     end if;
                     Put (Short_Text (St, (Name => To_Name ("(" & Image_Of (Shape.Low + Integer_64 (I)) & ")"),
                                          Location => Ref.Location,
                                          Address => Ref.Address + Target_Address (Unsigned_64 (I) * Shape.Element_Size),
                                          Of_Type => Shape.Element, Epoch => Ref.Epoch, others => <>), Depth));
                  end loop;
                  if Count > Display_Elements then
                     Put (", ...");
                  end if;
                  Put (if Is_Rust (St, Core.Unit_Index) then "]" else ")");
               end if;
               Node.Display := To_Display (Text (1 .. Length));
            end;
         when others =>
            Node.Display := To_Display ("<" & Image_Of (Node.Size) & " bytes>");
      end case;
   end Make_Node;

   procedure Value
     (S : Session; Ref : Value_Ref; Node : out Value_Node; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
   begin
      Node := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      Make_Node (St.all, Ref, Node, 0);
      R := Ok;
   end Value;

   --  Child Index (0-based) of Ref, and how many there are.
   procedure Child_Of
     (St : State_Record; Ref : Value_Ref; Index : Natural; Child : out Value_Ref; Total : out Natural)
   is
      Core : DW.Die;
      Name : Name_Text;
      Good : Boolean;
   begin
      Child := Null_Ref;
      Total := 0;
      if Ref.Of_Type = No_Type or else Ref.Location in Optimized_Out | Unavailable then
         return;
      end if;
      Strip (St, Ref.Of_Type, Core, Name, Good);
      if not Good then
         return;
      end if;
      if Core.Tag = DW.TAG_Structure_Type and then Is_Rust (St, Core.Unit_Index) then
         declare
            Handled : Boolean;
         begin
            Rust_Child (St, Ref, Core, Index, Child, Total, Handled);
            if Handled then
               return;
            end if;
            if Rust_Form_Of (St, Core) = Wrapper_Form then
               Child_Of (St, Wrapped (St, Ref, Core), Index, Child, Total);
               return;
            end if;
         end;
      end if;
      case Kind_Of (St, Ref.Of_Type) is
         when Pointer =>
            declare
               Has : Boolean;
            begin
               Pointee (St, Ref, Core, Child, Has);
               Total := (if Has then 1 else 0);
               if Index >= Total then
                  Child := Null_Ref;
               end if;
            end;
         when Record_Value | Variant_Record =>
            declare
               List : Member_List;
               M : DW.Die;
            begin
               Members_Of (St, Core, Ref, List);
               Total := List.Count;
               if Index < List.Count then
                  DW.Read_Die (St.Img.all, List.Items (Index + 1).Die_Offset, M, Good);
                  Child := Member_Ref (St, Ref, List.Items (Index + 1),
                                       (if Good then Field_Display_Name (St, M.Unit_Index, DW.Name_Of (St.Img.all, M))
                                        else "?"));
               end if;
            end;
         when Array_Value =>
            declare
               Shape : constant Array_Shape := Shape_Of (St, Core, Ref);
            begin
               Total := Element_Count (Shape);
               if Index < Total and then Ref.Location = In_Memory then
                  Child := (Name => To_Name ("(" & Image_Of (Shape.Low + Integer_64 (Index)) & ")"),
                            Location => In_Memory,
                            Address => Ref.Address + Target_Address (Unsigned_64 (Index) * Shape.Element_Size),
                            Of_Type => Shape.Element, Epoch => Ref.Epoch, others => <>);
               end if;
            end;
         when others =>
            null;
      end case;
   end Child_Of;

   procedure Children
     (S : Session; Ref : Value_Ref; First : Natural; Count : Child_Count;
      List : out Node_List; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      Child : Value_Ref;
      Total : Natural := 0;
   begin
      List := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      for I in 0 .. Count - 1 loop
         Child_Of (St.all, Ref, First + I, Child, Total);
         exit when First + I >= Total;
         List.Count := List.Count + 1;
         Make_Node (St.all, Child, List.Items (List.Count), 0);
      end loop;
      if Total = 0 then
         Child_Of (St.all, Ref, 0, Child, Total);
      end if;
      R := (if First + List.Count < Total then More else Ok);
   end Children;

   ---------------------------------------------------------------------------
   --  Locals and globals
   ---------------------------------------------------------------------------
   procedure Read_For_Expression
     (St : State_Record; Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Good : out Boolean) is
   begin
      Read_Word (St, Address, Size, Value, Good);
   end Read_For_Expression;

   TAG_GNU_Call_Site : constant := 16#4109#;
   OP_Reg0 : constant := 16#50#;
   --  Entry values chain through callers (recursion passes them on): at
   --  most this many frames up.
   Entry_Depth_Ceiling : constant := 16;
   Call_Site_Walk_Ceiling : constant := 4096;

   --  Frame Index's evaluation context: its registers, its CFA, and its
   --  function's frame base. Fn: that function's DIE.
   procedure Frame_Context
     (St : State_Record; TF : Thread_Frames; Index : Frame_Index;
      Ctx : out EX.Context; Fn : out DW.Die; Lookup_PC : out Unsigned_64; Good : out Boolean)
   is
      F : Integer;
      A : DW.Attribute;
      Found : Boolean;
      At_Byte, Size : BY.Offset;
      Result : EX.Result;
      procedure Read (Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Ok_Read : out Boolean) is
      begin
         Read_For_Expression (St, Address, Size, Value, Ok_Read);
      end Read;
   begin
      Ctx := (others => <>);
      Fn := (others => <>);
      Good := False;
      Lookup_PC := 0;
      if Index >= TF.Count then
         return;
      end if;
      Ctx.Regs := TF.Regs (Index);
      Ctx.CFA := Unsigned_64 (TF.CFAs (Index));
      Ctx.Has_CFA := TF.CFAs (Index) /= 0;
      Lookup_PC := TF.Regs (Index).Value (CF.Return_Address);
      if Index > 0 then
         Lookup_PC := Lookup_PC - 1;
      end if;
      F := DW.Function_At (St.Img.all, Lookup_PC);
      if F < 0 then
         return;
      end if;
      DW.Read_Die (St.Img.all, St.Img.Functions (F).Die_Offset, Fn, Good);
      if not Good then
         return;
      end if;
      DW.Find_Inherited (St.Img.all, Fn, DW.AT_Frame_Base, A, Found);
      if Found then
         DW.Location_At (St.Img.all, Fn.Unit_Index, A, Lookup_PC, At_Byte, Size, Found);
         if Found then
            EX.Evaluate (St.Img.Data.all, At_Byte, Size, Ctx, Read'Access, False, Result);
            if Result.Kind = EX.Memory_Location then
               Ctx.Frame_Base := Result.Address;
               Ctx.Has_Frame_Base := True;
            elsif Result.Kind = EX.Register_Location and then Result.Has_Value then
               --  DW_OP_regN (rustc: the stack pointer): the base is the
               --  register's value.
               Ctx.Frame_Base := Result.Value;
               Ctx.Has_Frame_Base := True;
            end if;
         end if;
      end if;
   end Frame_Context;

   --  The call site in Fn's subtree whose return address is Return_PC.
   function Call_Site_Of (St : State_Record; Fn : DW.Die; Return_PC : Unsigned_64; Site : out DW.Die) return Boolean is
      Cur : BY.Offset := Fn.Next;
      Limit : constant BY.Offset := DW.Sibling_Of (St.Img.all, Fn);
      D : DW.Die;
      Good : Boolean;
   begin
      Site := (others => <>);
      if not Fn.Children then
         return False;
      end if;
      for Step in 1 .. Call_Site_Walk_Ceiling loop
         exit when Cur >= Limit;
         DW.Read_Die (St.Img.all, Cur, D, Good);
         exit when not Good;
         Cur := D.Next;   --  every DIE in order, children included
         if D.Tag = DW.TAG_Call_Site or else D.Tag = TAG_GNU_Call_Site then
            declare
               I : constant DW.Attribute_Count := DW.Find (D, DW.AT_Call_Return_PC);
            begin
               if I /= 0 and then DW.Address_Of (St.Img.all, D.Unit_Index, D.Attrs (I)) = Return_PC then
                  Site := D;
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Call_Site_Of;

   procedure Evaluate_In
     (St : State_Record; TF : Thread_Frames; Index : Frame_Index; Ctx : EX.Context;
      At_Byte, Size : BY.Offset; As_Value : Boolean; Result : out EX.Result; Depth : Natural)
   is
      procedure Read (Address : Unsigned_64; Size : Positive; Value : out Unsigned_64; Ok_Read : out Boolean) is
      begin
         Read_For_Expression (St, Address, Size, Value, Ok_Read);
      end Read;

      --  The register's value on entry: the caller's call-site parameter
      --  for it (DW_AT_call_value), evaluated in the caller's frame.
      procedure Resolve (Register : CF.Register; Value : out Unsigned_64; Ok_Out : out Boolean) is
         Caller_Ctx : EX.Context;
         Caller_Fn, Site, Param : DW.Die;
         Caller_PC : Unsigned_64;
         Good : Boolean;
         Cur : BY.Offset;
      begin
         Value := 0;
         Ok_Out := False;
         if Depth >= Entry_Depth_Ceiling or else Index + 1 >= TF.Count then
            return;
         end if;
         Frame_Context (St, TF, Index + 1, Caller_Ctx, Caller_Fn, Caller_PC, Good);
         if not Good or else not Call_Site_Of (St, Caller_Fn, TF.Regs (Index + 1).Value (CF.Return_Address), Site)
           or else not Site.Children
         then
            return;
         end if;
         Cur := Site.Next;
         loop
            DW.Read_Die (St.Img.all, Cur, Param, Good);
            exit when not Good or else Param.Tag = 0;
            declare
               L : constant DW.Attribute_Count := DW.Find (Param, DW.AT_Location);
               V : constant DW.Attribute_Count := DW.Find (Param, DW.AT_Call_Value);
            begin
               if L /= 0 and then V /= 0 and then Param.Attrs (L).Class = DW.Block_Value
                 and then Param.Attrs (L).Size = 1
                 and then Unsigned_64 (St.Img.Data (Natural (Param.Attrs (L).At_Byte)))
                   = OP_Reg0 + Unsigned_64 (Register)
                 and then Param.Attrs (V).Class = DW.Block_Value
               then
                  declare
                     Inner : EX.Result;
                  begin
                     Evaluate_In (St, TF, Index + 1, Caller_Ctx, Param.Attrs (V).At_Byte, Param.Attrs (V).Size,
                                  True, Inner, Depth + 1);
                     if Inner.Kind in EX.Implicit_Value | EX.Register_Location and then Inner.Has_Value then
                        Value := Inner.Value;
                        Ok_Out := True;
                     end if;
                     return;
                  end;
               end if;
            end;
            Cur := DW.Sibling_Of (St.Img.all, Param);
         end loop;
      end Resolve;
   begin
      EX.Evaluate (St.Img.Data.all, At_Byte, Size, Ctx, Read'Access, As_Value, Result, Resolve'Access);
   end Evaluate_In;

   procedure Locals
     (S : Session; F : Frame; List : out Ref_List; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      D : DW.Die;
      Good : Boolean;
      Ctx : EX.Context;
      Lookup_PC : Unsigned_64;
      TF_Index : Natural := 0;
      Truncated : Boolean := False;
   begin
      List := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      --  The thread's cached frames of this stop.
      if DT.Through_Kernel (St.Conn.all) then
         TF_Index := Kernel_Index (St.all, F.Thread);
      else
         declare
            Page : DT.State_Page;
         begin
            Snapshot (St.all, Page);
            for T in 1 .. Thread_Total (Page) loop
               if Thread_Id (Page_U32 (Page, P.Threads_At + (T - 1) * P.Thread_Bytes + P.T_Id_At)) = F.Thread then
                  TF_Index := T;
               end if;
            end loop;
         end;
      end if;
      if TF_Index > Thread_Ceiling then
         TF_Index := 0;
      end if;
      if TF_Index = 0 then
         R := Not_Found;
         return;
      end if;
      declare
         TF : Thread_Frames renames St.Frames (TF_Index);
      begin
         if TF.Epoch /= F.Epoch or else F.Index >= TF.Count or else TF.Epoch /= St.Epoch then
            R := Stale;
            return;
         end if;
         Frame_Context (St.all, TF, F.Index, Ctx, D, Lookup_PC, Good);
         if not Good then
            R := No_Debug_Info;
            return;
         end if;
         declare
            Unit_Index : constant Natural := D.Unit_Index;

            procedure Add (V : DW.Die) is
               A : DW.Attribute;
               Found : Boolean;
               At_Byte, Size : BY.Offset;
               Result : EX.Result;
               Ref : Value_Ref;
            begin
               if List.Count = Ref_List_Ceiling then
                  Truncated := True;
                  return;
               end if;
               Ref.Name := To_Name (Field_Display_Name (St.all, V.Unit_Index, DW.Name_Of (St.Img.all, V)));
               Ref.Epoch := F.Epoch;
               DW.Find_Inherited (St.Img.all, V, DW.AT_Type, A, Found);
               if Found and then A.Class = DW.Reference_Value then
                  Ref.Of_Type := Type_Key (A.Value);
               end if;
               DW.Find_Inherited (St.Img.all, V, DW.AT_Location, A, Found);
               if not Found then
                  DW.Find_Inherited (St.Img.all, V, DW.AT_Const_Value, A, Found);
                  if Found and then A.Class in DW.Constant_Value | DW.Signed_Value then
                     Ref.Location := Register_Value;
                     Ref.Bits := A.Value;
                  else
                     Ref.Location := Optimized_Out;
                  end if;
               else
                  DW.Location_At (St.Img.all, Unit_Index, A, Lookup_PC, At_Byte, Size, Found);
                  if not Found then
                     Ref.Location := Optimized_Out;
                  else
                     Evaluate_In (St.all, TF, F.Index, Ctx, At_Byte, Size, False, Result, 0);
                     case Result.Kind is
                        when EX.Memory_Location =>
                           Ref.Location := In_Memory;
                           Ref.Address := Target_Address (Result.Address);
                        when EX.Register_Location =>
                           if Result.Has_Value then
                              Ref.Location := Register_Value;
                              Ref.Bits := Result.Value;
                           else
                              Ref.Location := Optimized_Out;
                           end if;
                        when EX.Implicit_Value =>
                           Ref.Location := Register_Value;
                           Ref.Bits := Result.Value;
                        when EX.Optimized_Out =>
                           Ref.Location := Optimized_Out;
                        when EX.Entry_Value | EX.Unsupported | EX.Failed =>
                           Ref.Location := Unavailable;
                     end case;
                  end if;
               end if;
               if Ref.Name.Length > 0 then
                  List.Count := List.Count + 1;
                  List.Items (List.Count) := Ref;
               end if;
            end Add;

            procedure Walk (First : BY.Offset; Depth : Natural) is
               Cur : BY.Offset := First;
               Child : DW.Die;
               Ok_Die : Boolean;
            begin
               loop
                  DW.Read_Die (St.Img.all, Cur, Child, Ok_Die);
                  exit when not Ok_Die or else Child.Tag = 0;
                  if Child.Tag = DW.TAG_Formal_Parameter or else Child.Tag = DW.TAG_Variable then
                     Add (Child);
                  elsif Child.Tag = DW.TAG_Lexical_Block and then Child.Children and then Depth < DW.Depth_Ceiling
                    and then ((DW.Find (Child, DW.AT_Low_PC) = 0 and then DW.Find (Child, DW.AT_Ranges) = 0)
                              or else DW.Contains (St.Img.all, Child, Lookup_PC))
                  then
                     Walk (Child.Next, Depth + 1);
                  end if;
                  Cur := DW.Sibling_Of (St.Img.all, Child);
               end loop;
            end Walk;
         begin
            if D.Children then
               Walk (D.Next, 0);
            end if;
         end;
      end;
      R := (if Truncated then More else Ok);
   end Locals;

   procedure Globals
     (S : Session; Unit : String; List : out Ref_List; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      D : DW.Die;
      Good : Boolean;
      Prefix : String (1 .. Unit'Length + 2);
      Truncated : Boolean := False;
   begin
      List := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      for I in Unit'Range loop
         Prefix (I - Unit'First + 1) :=
           (if Unit (I) in 'A' .. 'Z' then Character'Val (Character'Pos (Unit (I)) + 32)
            elsif Unit (I) = '.' then '_' else Unit (I));
      end loop;
      Prefix (Prefix'Last - 1 .. Prefix'Last) := "__";
      for G in 0 .. St.Img.Global_Count - 1 loop
         DW.Read_Die (St.Img.all, St.Img.Globals (G).Die_Offset, D, Good);
         if Good then
            declare
               Raw : constant String := DW.Name_Of (St.Img.all, D);
               Wanted : Boolean;
            begin
               if Unit'Length > 0 then
                  Wanted := Starts_With (Raw, Prefix);
               else
                  --  The program's own units: in the main unit's directory,
                  --  not generated, and not the runtime's.
                  declare
                     Source : constant String := DW.Unit_Name (St.Img.all, D.Unit_Index);
                     Own : constant String := Image (St.Own_Directory);
                  begin
                     Wanted := Raw'Length > 0
                       and then (for all Pre of Runtime_Prefixes => not Starts_With (Raw, Pre.all))
                       --  Rust's compiler-made statics (vtables) are not the program's.
                       and then Raw (Raw'First) /= '<'
                       and then not Starts_With (Raw, "ada_main__")
                       and then (Own'Length = 0 or else
                                 (Starts_With (Source, Own)
                                  and then not Starts_With (Source (Source'First + Own'Length - 1 .. Source'Last),
                                                            Generated_Directory)));
                  end;
               end if;
               if Wanted then
                  if List.Count = Ref_List_Ceiling then
                     Truncated := True;
                     exit;
                  end if;
                  List.Count := List.Count + 1;
                  List.Items (List.Count) :=
                    (Name => To_Name (Global_Name (St.all, D, Raw)), Location => In_Memory,
                     Address => Target_Address (St.Img.Globals (G).Address),
                     Of_Type => Type_Of (D), others => <>);
               end if;
            end;
         end if;
      end loop;
      R := (if Truncated then More elsif List.Count = 0 and then Unit'Length > 0 then Not_Found else Ok);
   end Globals;

   ---------------------------------------------------------------------------
   --  Memory and source
   ---------------------------------------------------------------------------
   procedure Memory
     (S : Session; Address : Target_Address; Length : Memory_Length;
      Bytes : out Byte_Array; Got : out Memory_Length; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      Buffer : DT.Byte_Buffer (0 .. Memory_Read_Ceiling - 1);
      Count : Natural := 0;
   begin
      Bytes := [others => 0];
      Got := 0;
      if St = null then
         R := Not_Attached;
         return;
      end if;
      if Length > 0 then
         Read_Memory (St.all, Address, Buffer (0 .. Length - 1), Count);
         for I in 0 .. Count - 1 loop
            Bytes (Bytes'First + I) := Buffer (I);
         end loop;
      end if;
      Got := Count;
      R := (if Count = Length then Ok else Unreadable);
   end Memory;

   procedure Source
     (S : Session; File : String; First_Line : Line_Number;
      Page : out Source_Page; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      Index : Natural := 0;
      Data : DT.Buffer_Access;
      Result : DT.Status;
      Load_Wait_Ms : constant := 2_000;
   begin
      Page := (others => <>);
      if St = null then
         R := Not_Attached;
         return;
      end if;
      for I in St.Sources'Range loop
         if St.Sources (I).Data /= null and then Image (St.Sources (I).Name) = File then
            Index := I;
         end if;
      end loop;
      if Index = 0 then
         DT.Load_Source (St.Conn.all, St.Info.Build_Id, File,
                         DT.Deadline_After (Load_Wait_Ms), Data, Result);
         if Result /= DT.Ok then
            R := (if Result = DT.Timed_Out then Timed_Out else Not_Found);
            return;
         end if;
         Index := St.Source_Next;
         St.Source_Next := (if Index = Source_Cache_Ceiling then 1 else Index + 1);
         declare
            E : Source_Entry renames St.Sources (Index);
            Lines : Natural := 1;
         begin
            if E.Data /= null then
               Free_Buffer (E.Data);
               Free_Starts (E.Starts);
            end if;
            for B of Data.all loop
               if B = Character'Pos (ASCII.LF) and then Lines < Source_Line_Ceiling then
                  Lines := Lines + 1;
               end if;
            end loop;
            E.Name := To_Path (File);
            E.Data := Data;
            E.Starts := new Line_Starts (1 .. Lines);
            E.Starts (1) := 0;
            E.Lines := 1;
            for I in Data'Range loop
               if Data (I) = Character'Pos (ASCII.LF) and then E.Lines < Lines then
                  E.Lines := E.Lines + 1;
                  E.Starts (E.Lines) := I + 1;
               end if;
            end loop;
            if E.Lines > 1 and then E.Starts (E.Lines) > Data'Last then
               E.Lines := E.Lines - 1;   --  the file ends in a newline
            end if;
         end;
      end if;
      declare
         E : Source_Entry renames St.Sources (Index);
      begin
         Page.Total := E.Lines;
         Page.First := Natural'Max (1, First_Line);
         for L in Page.First .. E.Lines loop
            exit when Page.Count = Source_Page_Ceiling;
            declare
               Start : constant Natural := E.Starts (L);
               Stop : Natural := (if L < E.Lines then E.Starts (L + 1) - 1 else E.Data'Last + 1);
               Text : String (1 .. Natural'Max (0, Stop - Start));
            begin
               if Stop > Start and then E.Data (Stop - 1) = Character'Pos (ASCII.LF) then
                  Stop := Stop - 1;
               end if;
               for I in Start .. Stop - 1 loop
                  Text (I - Start + 1) :=
                    (if E.Data (I) = Character'Pos (ASCII.HT) then ' '
                     elsif E.Data (I) in 32 .. 126 then Character'Val (E.Data (I)) else ' ');
               end loop;
               Page.Count := Page.Count + 1;
               Page.Lines (Page.Count) := Line_Of (Text (1 .. Stop - Start));
            end;
         end loop;
      end;
      R := (if Page.First + Page.Count - 1 < Page.Total then More else Ok);
   end Source;

   ---------------------------------------------------------------------------
   --  Graph
   ---------------------------------------------------------------------------
   procedure Graph
     (S : Session; Roots : Ref_List; Depth : Graph_Depth; Max_Nodes : Node_Count;
      Result : in out Graph_Result; R : out Outcome)
   is
      St : constant State_Access := Lookup (S);
      type Pending is record
         Ref : Value_Ref;
         Level : Natural := 0;
         From : Natural := 0;        --  node index, 0: a root
         From_Field : Natural := 0;
         Label : Field_Name_Text;
      end record;
      Queue_Ceiling : constant := Graph_Node_Ceiling + Graph_Edge_Ceiling;
      Queue : array (1 .. Queue_Ceiling) of Pending;
      Head, Tail : Natural := 0;
      Parents : array (Node_Index) of Natural := [others => 0];
      Limit : constant Natural := Natural'Min (Max_Nodes, Graph_Node_Ceiling);

      procedure Enqueue (Item : Pending) is
      begin
         if Tail = Queue_Ceiling then
            Result.Truncated := True;
         else
            Tail := Tail + 1;
            Queue (Tail) := Item;
         end if;
      end Enqueue;

      function Find_Node (Id : Node_Id) return Natural is
      begin
         for I in 1 .. Result.Node_Total loop
            if Result.Nodes (I).Id = Id then
               return I;
            end if;
         end loop;
         return 0;
      end Find_Node;

      function Is_Ancestor (Candidate, Of_Node : Natural) return Boolean is
         Cur : Natural := Of_Node;
      begin
         for Hop in 1 .. Graph_Node_Ceiling loop
            exit when Cur = 0;
            if Cur = Candidate then
               return True;
            end if;
            Cur := Parents (Cur);
         end loop;
         return False;
      end Is_Ancestor;

      procedure Add_Edge (From, To, Field : Natural; Label : Field_Name_Text; Kind : Edge_Kind) is
      begin
         if From = 0 then
            return;
         end if;
         if Result.Edge_Total = Graph_Edge_Ceiling then
            Result.Truncated := True;
            return;
         end if;
         Result.Edge_Total := Result.Edge_Total + 1;
         Result.Edges (Result.Edge_Total) :=
           (From => From, To => To, From_Field => Field, Label => Label, Kind => Kind);
      end Add_Edge;
   begin
      Result.Node_Total := 0;
      Result.Edge_Total := 0;
      Result.Truncated := False;
      if St = null then
         R := Not_Attached;
         return;
      end if;
      for I in 1 .. Roots.Count loop
         Enqueue ((Ref => Roots.Items (I), Level => 0, From => 0, From_Field => 0,
                   Label => Field_Name_Of (Image (Roots.Items (I).Name))));
      end loop;
      while Head < Tail loop
         Head := Head + 1;
         declare
            Item : constant Pending := Queue (Head);
            Object : Value_Ref := Item.Ref;
            Node : Value_Node;
            Id : Node_Id;
            Existing : Natural;
            Core : DW.Die;
            Name : Name_Text;
            Good : Boolean;
         begin
            --  A Rust enum whose payload is one pointer (Option<Box<T>>,
            --  Some(&T)): the edge goes through it; None ends it.
            if Kind_Of (St.all, Object.Of_Type) = Rust_Enum then
               declare
                  Payload : Value_Ref;
                  Total : Natural;
               begin
                  Child_Of (St.all, Object, 0, Payload, Total);
                  if Total = 0 then
                     goto Continue;
                  elsif Total = 1 and then Kind_Of (St.all, Payload.Of_Type) = Pointer then
                     Payload.Name := Object.Name;
                     Object := Payload;
                  end if;
               end;
            end if;
            --  A pointer root or field: the node is what it points at.
            if Kind_Of (St.all, Object.Of_Type) = Pointer then
               Make_Node (St.all, Object, Node, 0);
               if not Node.Has_Target then
                  goto Continue;
               end if;
               Object := Node.Target;
               if Item.From = 0 then
                  Object.Name := Item.Ref.Name;
               end if;
            end if;
            Strip (St.all, Object.Of_Type, Core, Name, Good);
            Id := (Address => Object.Address, Of_Type => (if Good then Type_Key (Core.Offset) else Object.Of_Type));
            Existing := Find_Node (Id);
            if Existing /= 0 then
               Result.Nodes (Existing).Incoming := Result.Nodes (Existing).Incoming + 1;
               Add_Edge (Item.From, Existing, Item.From_Field, Item.Label,
                         (if Is_Ancestor (Existing, Item.From) then Back_Edge else Shared_Edge));
               goto Continue;
            end if;
            if Result.Node_Total >= Limit then
               Result.Truncated := True;
               goto Continue;
            end if;
            Result.Node_Total := Result.Node_Total + 1;
            declare
               This : constant Node_Index := Result.Node_Total;
               G : Graph_Node renames Result.Nodes (This);
               Child : Value_Ref;
               Total : Natural;
               Field_Node : Value_Node;
            begin
               Parents (This) := Item.From;
               Make_Node (St.all, Object, G.Value, 0);
               G.Id := Id;
               G.Depth := Item.Level;
               G.Is_Root := Item.From = 0;
               G.Incoming := (if Item.From = 0 then 0 else 1);
               G.Fields_Shown := 0;
               G.Field_Total := 0;
               Add_Edge (Item.From, This, Item.From_Field, Item.Label, Pointer_Edge);
               Child_Of (St.all, Object, 0, Child, Total);
               G.Field_Total := Total;
               declare
                  Object_Is_Slice : constant Boolean := G.Value.Kind = Slice;
                  procedure Follow (Target : Value_Ref; Field : Natural; Label : String) is
                  begin
                     if Item.Level < Depth then
                        Enqueue ((Ref => Target, Level => Item.Level + 1, From => This,
                                  From_Field => Field, Label => Field_Name_Of (Label)));
                     else
                        Result.Truncated := True;
                     end if;
                  end Follow;
               begin
                  for F in 0 .. Total - 1 loop
                     --  A Vec's or slice's node: its elements become nodes
                     --  (up to a ceiling), its first ones shown as fields.
                     exit when F >= (if Object_Is_Slice then Graph_Element_Ceiling else Graph_Field_Ceiling);
                     Child_Of (St.all, Object, F, Child, Total);
                     Make_Node (St.all, Child, Field_Node, 0);
                     declare
                        Points : constant Boolean := Field_Node.Kind = Pointer and then Field_Node.Has_Target;
                        Payload : Value_Ref;
                        Payload_Total : Natural := 0;
                        Through_Enum : Boolean := False;
                        Elements : Boolean := False;
                     begin
                        if Field_Node.Kind = Rust_Enum then
                           Child_Of (St.all, Child, 0, Payload, Payload_Total);
                           Through_Enum := Payload_Total = 1 and then Kind_Of (St.all, Payload.Of_Type) = Pointer;
                        elsif Field_Node.Kind = Slice and then Field_Node.Child_Count > 0 then
                           --  A Vec field of records or pointers: its elements.
                           declare
                              First_Element : Value_Ref;
                              Ignore : Natural;
                           begin
                              Child_Of (St.all, Child, 0, First_Element, Ignore);
                              Elements := Kind_Of (St.all, First_Element.Of_Type)
                                in Pointer | Record_Value | Rust_Enum | Variant_Record;
                           end;
                        end if;
                        if G.Fields_Shown < Graph_Field_Ceiling then
                           G.Fields_Shown := G.Fields_Shown + 1;
                           G.Fields (G.Fields_Shown) :=
                             (Name => Field_Name_Of (Image (Child.Name)),
                              Display => Field_Display_Of (Image (Field_Node.Display)),
                              Kind => Field_Node.Kind, Changed => Field_Node.Changed,
                              Is_Edge => Points or else Through_Enum or else Elements
                                         or else (Object_Is_Slice and then Field_Node.Kind
                                                    in Record_Value | Rust_Enum | Variant_Record),
                              Is_Null => (Field_Node.Kind = Pointer and then not Field_Node.Has_Target)
                                         or else (Field_Node.Kind = Rust_Enum and then Payload_Total = 0));
                        end if;
                        if Points or else Through_Enum
                          or else (Object_Is_Slice and then Field_Node.Kind in Record_Value | Variant_Record)
                        then
                           Follow (Child, G.Fields_Shown, Image (Child.Name));
                        elsif Elements then
                           for E in 0 .. Natural'Min (Field_Node.Child_Count, Graph_Element_Ceiling) - 1 loop
                              declare
                                 Element : Value_Ref;
                                 Ignore : Natural;
                              begin
                                 Child_Of (St.all, Child, E, Element, Ignore);
                                 Follow (Element, G.Fields_Shown, Image (Child.Name) & Image (Element.Name));
                              end;
                           end loop;
                        end if;
                     end;
                  end loop;
               end;
            end;
            <<Continue>>
         end;
      end loop;
      R := (if Result.Truncated then More else Ok);
   end Graph;

end CuBit.Debug;
