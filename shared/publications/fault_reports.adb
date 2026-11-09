------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Fault_Reports with SPARK_Mode is

   Vector_Limit : constant := 2 ** 8;

   Error_Code_Limit : constant := 2 ** 32;

   function Exception_Report
     (Vector, Error_Code, Address, RIP, RSP : Unsigned_64; L : Layout) return Report
   is
      Kind   : constant Fault_Kind := Kind_Of_Vector (Vector);
      Code   : constant Page_Fault_Code := Decode_Page_Fault (Error_Code);
      Paging : constant Boolean := Kind = Page_Fault;
      At_Address : constant Anchored := Anchor_Of (Address, L);
      At_RIP : constant Anchored := Anchor_Of (RIP, L);
      At_RSP : constant Anchored := Anchor_Of (RSP, L);
   begin
      return
        (Kind         => Fault_Kind'Enum_Rep (Kind),
         Operation    => (if Paging then Access_Kind'Enum_Rep (Code.Operation) else 0),
         Cause        => (if Paging then Page_Cause'Enum_Rep (Code.Cause) else 0),
         Mode         => (if Paging then Privilege'Enum_Rep (Code.Mode)
                          else Privilege'Enum_Rep (User_Mode)),
         Where        => (if Paging then Address_Class'Enum_Rep (Classify (Address, Code, L)) else 0),
         Stack        => Stack_Position'Enum_Rep (Stack_Position_Of (RSP, L)),
         Vector       => (if Vector < Vector_Limit then Unsigned_8 (Vector) else Unsigned_8'Last),
         Address_From => (if Paging then Anchor'Enum_Rep (At_Address.From) else 0),
         Error_Code   => (if Error_Code < Error_Code_Limit then Unsigned_32 (Error_Code)
                          else Unsigned_32'Last),
         RIP_From     => Anchor'Enum_Rep (At_RIP.From),
         RSP_From     => Anchor'Enum_Rep (At_RSP.From),
         Spare        => 0,
         Address      => (if Paging then At_Address.Offset else 0),
         RIP          => At_RIP.Offset,
         RSP          => At_RSP.Offset,
         Stopper      => 0,
         Spare_2      => 0,
         Spare_3      => 0);
   end Exception_Report;

   function Out_Of_Memory_Report
     (Address, RIP, RSP : Unsigned_64; L : Layout) return Report
   is
      At_Address : constant Anchored := Anchor_Of (Address, L);
      At_RIP : constant Anchored := Anchor_Of (RIP, L);
      At_RSP : constant Anchored := Anchor_Of (RSP, L);
   begin
      return
        (Kind         => Fault_Kind'Enum_Rep (Out_Of_Memory),
         Operation    => 0,
         Cause        => Page_Cause'Enum_Rep (Not_Present),
         Mode         => Privilege'Enum_Rep (User_Mode),
         Where        => Address_Class'Enum_Rep
                           (Classify (Address, (Operation => Read, Cause => Not_Present,
                                                Mode => User_Mode), L)),
         Stack        => Stack_Position'Enum_Rep (Stack_Position_Of (RSP, L)),
         Vector       => Vector_Page_Fault,
         Address_From => Anchor'Enum_Rep (At_Address.From),
         Error_Code   => 0,
         RIP_From     => Anchor'Enum_Rep (At_RIP.From),
         RSP_From     => Anchor'Enum_Rep (At_RSP.From),
         Spare        => 0,
         Address      => At_Address.Offset,
         RIP          => At_RIP.Offset,
         RSP          => At_RSP.Offset,
         Stopper      => 0,
         Spare_2      => 0,
         Spare_3      => 0);
   end Out_Of_Memory_Report;

   function Stopped_Report (Stopper : Unsigned_64) return Report is
     ((Kind => Fault_Kind'Enum_Rep (Stopped_By_Process), Stopper => Stopper, others => <>));

   function Anchor_Value (Code : Unsigned_8) return Anchor is
   begin
      for A in Anchor loop
         if Anchor'Enum_Rep (A) = Code then
            return A;
         end if;
      end loop;
      return No_Anchor;
   end Anchor_Value;

   function Anchor_Name (From : Anchor) return String is
     (case From is
         when No_Anchor    => "",
         when Image        => "image+",
         when Stack_Top    => "stack top-",
         when Stack_Bottom => "stack bottom-",
         when Heap         => "heap+",
         when Zero         => "",
         when Unanchored   => "");

   function Kind_Of (R : Report) return Fault_Kind is
   begin
      for K in Fault_Kind loop
         if Fault_Kind'Enum_Rep (K) = R.Kind then
            return K;
         end if;
      end loop;
      return Other_Exception;
   end Kind_Of;

   function Operation_Of (R : Report) return Access_Kind is
   begin
      for A in Access_Kind loop
         if Access_Kind'Enum_Rep (A) = R.Operation then
            return A;
         end if;
      end loop;
      return No_Access;
   end Operation_Of;

   function Where_Of (R : Report) return Address_Class is
   begin
      for C in Address_Class loop
         if Address_Class'Enum_Rep (C) = R.Where then
            return C;
         end if;
      end loop;
      return Not_Applicable;
   end Where_Of;

   function Stack_Of (R : Report) return Stack_Position is
   begin
      for S in Stack_Position loop
         if Stack_Position'Enum_Rep (S) = R.Stack then
            return S;
         end if;
      end loop;
      return Unknown;
   end Stack_Of;

   function Kind_Name (Kind : Fault_Kind) return String is
     (case Kind is
         when No_Fault            => "no fault",
         when Divide_Error        => "divide error (#DE)",
         when Debug_Trap          => "debug trap (#DB)",
         when Breakpoint          => "breakpoint (#BP)",
         when Overflow            => "overflow (#OF)",
         when Bound_Range         => "bound range exceeded (#BR)",
         when Invalid_Opcode      => "invalid opcode (#UD)",
         when Segment_Not_Present => "segment not present (#NP)",
         when Stack_Segment       => "stack-segment fault (#SS)",
         when General_Protection  => "general protection fault (#GP)",
         when Page_Fault          => "page fault",
         when X87_Floating_Point  => "x87 floating-point error (#MF)",
         when Alignment_Check     => "alignment check (#AC)",
         when SIMD_Floating_Point => "SIMD floating-point exception (#XM)",
         when Virtualization      => "virtualization exception (#VE)",
         when Control_Protection  => "control protection (#CP)",
         when Other_Exception     => "CPU exception",
         when Out_Of_Memory       => "out of memory",
         when Stopped_By_Process  => "stopped");

   function Operation_Name (Operation : Access_Kind) return String is
     (case Operation is
         when No_Access => "access",
         when Read      => "read from",
         when Write     => "write to",
         when Execute   => "instruction fetch from");

   function Access_Name (Operation : Access_Kind) return String is
     (case Operation is
         when No_Access => "access",
         when Read      => "read",
         when Write     => "write",
         when Execute   => "instruction fetch");

   function Where_Name (Where : Address_Class) return String is
     (case Where is
         when Not_Applicable      => "",
         when Null_Page           => "null page",
         when Stack_Overflow      => "stack overflow",
         when Unmapped            => "unmapped",
         when Read_Only           => "read-only",
         when No_Execute          => "no-execute",
         when Reserved_Entry_Page => "reserved bit in page table",
         when Kernel_Space        => "kernel space",
         when In_Stack            => "stack",
         when In_Heap             => "heap",
         when In_Image            => "program image");

   function Stack_Name (Stack : Stack_Position) return String is
     (case Stack is
         when Unknown              => "unknown",
         when In_Stack_Reservation => "in stack",
         when In_Stack_Guard       => "below the stack (overflowed)",
         when Outside_Stack        => "outside the stack");
end Fault_Reports;
