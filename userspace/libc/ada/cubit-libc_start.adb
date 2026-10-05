------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System; use type System.Address;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Launch_Arguments; use CuBit.Launch_Arguments;
with CuBit.Libc_ABI; use CuBit.Libc_ABI;
with CuBit.Libc_Descriptors;
with CuBit.Libc_Start_Layout;

package body CuBit.Libc_Start is

   --  The main thread's stack size without a PT_GNU_STACK header.
   Default_Stack_Bytes : constant := 2 ** 20;
   --  musl's stack protector and hash seeds: AT_RANDOM's 16 bytes.
   Random_Bytes : constant := 16;
   Random_Words : constant := Random_Bytes / 8;
   RDRAND_Attempts : constant := 16;
   --  Auxiliary vector: five pairs and AT_NULL.
   Auxiliary_Words : constant := 12;

   Program_Name : aliased constant String := "cubit-program" & Character'Val (0);
   type Random_Array is array (1 .. Random_Words) of Unsigned_64;
   Random : aliased Random_Array := [others => 0];

   type Main_Access is access function
     (Count : Interfaces.C.int; Arguments, Environment : System.Address)
      return Interfaces.C.int
   with Convention => C;
   function Main
     (Count : Interfaces.C.int; Arguments, Environment : System.Address)
      return Interfaces.C.int
   with Import, Convention => C, External_Name => "main";

   function Libc_Start_Main
     (Program : Main_Access; Count : Interfaces.C.int; Arguments : System.Address;
      Init, Fini, Loader_Fini : System.Address) return Interfaces.C.int
   with Import, Convention => C, External_Name => "__libc_start_main";

   --  libc (file.c): adopt the launcher's working directory.
   procedure Working_Directory_Start (Name : System.Address)
   with Import, Convention => C, External_Name => "__cubit_cwd_start";

   --  The program's own ELF header, which the linker places in the image.
   ELF_Header : constant Storage_Element
   with Import, Convention => C, External_Name => "__ehdr_start";

   function Word_At (Where : System.Address) return Unsigned_64;
   function Word_At (Where : System.Address) return Unsigned_64 is
      Value : constant Unsigned_64 with Import, Address => Where;
   begin
      return Value;
   end Word_At;

   function Half_At (Where : System.Address) return Unsigned_16;
   function Half_At (Where : System.Address) return Unsigned_16 is
      Value : constant Unsigned_16 with Import, Address => Where;
   begin
      return Value;
   end Half_At;

   function Address_Value (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));

   --  16 random bytes: RDRAND, else the time-stamp counter.
   procedure Fill_Random;
   procedure Fill_Random is
      Value : Unsigned_64;
      Carry : Unsigned_8;
   begin
      for W in Random'Range loop
         Carry := 0;
         for Attempt in 1 .. RDRAND_Attempts loop
            Asm ("rdrand %0" & ASCII.LF & "setc %1",
                 Outputs => [Unsigned_64'Asm_Output ("=r", Value),
                             Unsigned_8'Asm_Output ("=qm", Carry)],
                 Volatile => True);
            exit when Carry /= 0;
         end loop;
         if Carry = 0 then
            declare
               Low, High : Unsigned_32;
            begin
               Asm ("rdtsc",
                    Outputs => [Unsigned_32'Asm_Output ("=a", Low),
                                Unsigned_32'Asm_Output ("=d", High)],
                    Volatile => True);
               Value := Shift_Left (Unsigned_64 (High), 32) or Unsigned_64 (Low);
            end;
         end if;
         Random (W) := Value;
      end loop;
   end Fill_Random;

   --  The PT_GNU_STACK size, or the default.
   function Program_Stack_Size return Unsigned_64;
   function Program_Stack_Size return Unsigned_64 is
      Header : constant System.Address := ELF_Header'Address;
      Headers : constant System.Address :=
        Header + Storage_Offset (Word_At (Header + Ehdr_Phoff_Offset));
      Entry_Bytes : constant Storage_Offset :=
        Storage_Offset (Half_At (Header + Ehdr_Phentsize_Offset));
      Count : constant Natural := Natural (Half_At (Header + Ehdr_Phnum_Offset));
   begin
      for K in 0 .. Count - 1 loop
         declare
            Program_Header : constant System.Address :=
              Headers + Storage_Offset (K) * Entry_Bytes;
            Kind : constant Unsigned_32 with Import,
              Address => Program_Header + Phdr_Type_Offset;
            Size : constant Unsigned_64 := Word_At (Program_Header + Phdr_Memsz_Offset);
         begin
            if Kind = PT_GNU_STACK and then Size /= 0 then
               return Size;
            end if;
         end;
      end loop;
      return Default_Stack_Bytes;
   end Program_Stack_Size;

   procedure Start (Initial_Stack, Launch_Length : Unsigned_64) is
      Mapped : constant Block (1 .. Maximum_Block_Bytes)
      with Import, Address => To_Address (Block_Address);
      Length : constant Present_Length :=
        (if Launch_Length in Header_Bytes .. Maximum_Block_Bytes
           and then Validate (Mapped (1 .. Natural (Launch_Length))) = Valid
         then Natural (Launch_Length) else Header_Bytes);
      --  The strings end here; the program's description fills the rest.
      Strings_End : constant Natural :=
        (if Length > Header_Bytes then Strings_Last (Mapped (1 .. Length)) else Header_Bytes);
      Firsts : CuBit.Libc_Start_Layout.Starts;
      Found : String_Count := 0;
      Arguments, Environment, Directories : Natural := 0;
   begin
      Stack_Top := Initial_Stack;
      Stack_Size := Program_Stack_Size;
      Fill_Random;
      if Length > Strings_End then
         CuBit.Libc_Descriptors.Adopt_Ports
           (Mapped (Strings_End + 1)'Address, Length - Strings_End);
      else
         CuBit.Libc_Descriptors.Adopt_Ports (System.Null_Address, 0);
      end if;
      if Strings_End > Header_Bytes then
         declare
            Item : Block renames Mapped (1 .. Length);
         begin
            CuBit.Libc_Start_Layout.Locate_Strings (Item, Firsts, Found);
            if Found = Strings_Declared (Item) then
               Arguments := Arguments_Declared (Item);
               Environment := Environment_Declared (Item);
               Directories := Directory_Declared (Item);
            end if;
         end;
      end if;
      declare
         --  The strings, copied so the program may write to them.
         String_Bytes : constant Natural := Strings_End - Header_Bytes;
         Strings : Block (1 .. String_Bytes + 1) := [others => 0];
         Argument_Count : constant Natural := Natural'Max (Arguments, 1);
         --  argv and its null, envp and its null, the auxiliary vector.
         Vector : array (1 .. Argument_Count + 1 + Environment + 1 + Auxiliary_Words)
           of Unsigned_64 := [others => 0];
         I : Natural := 0;
         Header : constant System.Address := ELF_Header'Address;
         Ignore : Interfaces.C.int;

         procedure Put (Value : Unsigned_64);
         procedure Put (Value : Unsigned_64) is
         begin
            I := I + 1;
            Vector (I) := Value;
         end Put;

         function Copy_Of (K : Positive) return Unsigned_64 is
           (Address_Value (Strings (Firsts (K) - Header_Bytes)'Address));
      begin
         if String_Bytes > 0 then
            Strings (1 .. String_Bytes) := Mapped (Header_Bytes + 1 .. Strings_End);
         end if;
         if Arguments = 0 then
            Put (Address_Value (Program_Name'Address));        --  argv[0]
         end if;
         for K in 1 .. Arguments loop
            Put (Copy_Of (K));
         end loop;
         Put (0);                                              --  end of argv
         for K in Arguments + 1 .. Arguments + Environment loop
            Put (Copy_Of (K));
         end loop;
         Put (0);                                              --  end of envp
         Put (AT_PHDR);
         Put (Address_Value (Header) + Word_At (Header + Ehdr_Phoff_Offset));
         Put (AT_PHNUM);
         Put (Unsigned_64 (Half_At (Header + Ehdr_Phnum_Offset)));
         Put (AT_PHENT);
         Put (Unsigned_64 (Half_At (Header + Ehdr_Phentsize_Offset)));
         Put (AT_PAGESZ);
         Put (CuBit.Kernel_ABI.Page_Bytes);
         Put (AT_RANDOM);
         Put (Address_Value (Random'Address));
         Put (AT_NULL);
         Put (0);
         if Directories > 0 then
            Working_Directory_Start
              (Strings (Firsts (Arguments + Environment + 1) - Header_Bytes)'Address);
         end if;
         Ignore := Libc_Start_Main
           (Main'Access, Interfaces.C.int (Argument_Count), Vector'Address,
            System.Null_Address, System.Null_Address, System.Null_Address);
      end;
      --  __libc_start_main does not return.
      loop
         null;
      end loop;
   end Start;

end CuBit.Libc_Start;
