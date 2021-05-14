-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- x86-64 instruction wrappers
-------------------------------------------------------------------------------

--with Interfaces; use Interfaces;
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

--with TextIO;

package body x86 with
    SPARK_Mode => off           -- disabled for Asm()
is

    function "+" (Left : IOPort; Right : Unsigned_8) return IOPort is
    begin
        return Left + IOPort(Right);
    end;

    ---------------------------------------------------------------------------
    -- Output a single byte to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out8(port : in IOPort; data : in Unsigned_8) is
    begin
        Asm ("out %1, %0",                               -- output the byte
            Inputs =>  (Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                         Unsigned_8'Asm_Input ("a", data)),
            Volatile => True);
    end out8;
    pragma Inline (out8);

    ---------------------------------------------------------------------------
    -- Output a single 16-bit word to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out16(port : in IOPort; data : in Unsigned_16) is
    begin
        Asm ("out %1, %0", 
            Inputs =>  (Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                        Unsigned_16'Asm_Input ("a", data)),
            Volatile => True);
    end out16;
    pragma Inline (out16);

    ---------------------------------------------------------------------------
    -- Output a 32-bit quad to x86 I/O port
    ---------------------------------------------------------------------------
    procedure out32(port : in IOPort; data : in Unsigned_32) is
    begin
        Asm ("out %1, %0", 
            Inputs =>  (Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                        Unsigned_32'Asm_Input ("a", data)),
            Volatile => True
            );
    end out32;
    pragma Inline (out32);

    ---------------------------------------------------------------------------
    -- Output multiple words
    ---------------------------------------------------------------------------
    procedure outs16(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32)
    is
        dummyAddr : System.Address := addr; -- needed for an Asm_Output
        dummyCount : Unsigned_32 := count;  -- needed for an Asm_Output
    begin
        Asm("cld; repne; outsw;",
            Outputs => (System.Address'Asm_Output("=D", dummyAddr),
                        Unsigned_32'Asm_Output("=c", dummyCount)),
            Inputs =>  (Unsigned_16'Asm_Input("d", port),
                        System.Address'Asm_Input("0", dummyAddr),
                        Unsigned_32'Asm_Input("1", dummyCount)),
            Clobber =>  "memory, cc",
            Volatile => True);
    end outs16;
    pragma Inline (outs16);

    ---------------------------------------------------------------------------
    -- Output multiple dwords
    ---------------------------------------------------------------------------
    procedure outs32(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32)
    is
        dummyAddr : System.Address := addr; -- needed for an Asm_Output
        dummyCount : Unsigned_32 := count;  -- needed for an Asm_Output
    begin
        Asm("cld; repne; outsl;",
            Outputs => (System.Address'Asm_Output("=D", dummyAddr),
                        Unsigned_32'Asm_Output("=c", dummyCount)),
            Inputs =>  (Unsigned_16'Asm_Input("d", port),
                        System.Address'Asm_Input("0", dummyAddr),
                        Unsigned_32'Asm_Input("1", dummyCount)),
            Clobber =>  "memory, cc",
            Volatile => True);
    end outs32;
    pragma Inline (outs32);

    ---------------------------------------------------------------------------
    -- Input a single byte from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in8(port : in IOPort; val : out Unsigned_8) is
        ret : Unsigned_8 := 0;
    begin
        Asm ( "in %1, %0",
                Outputs => Unsigned_8'Asm_Output ("=a", ret),
                Inputs =>  Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                Volatile => True
             );
        val := ret;
    end in8;
    pragma Inline (in8);

    ---------------------------------------------------------------------------
    -- Input a short from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in16(port : in IOPort; val : out Unsigned_16) is
        ret : Unsigned_16 := 0;
    begin
        Asm ( "in %1, %0",
                Outputs => Unsigned_16'Asm_Output ("=a", ret),
                Inputs =>  Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                Volatile => True
             );
        val := ret;
    end in16;
    pragma Inline (in16);

    ---------------------------------------------------------------------------
    -- Input a single byte from x86 I/O port
    ---------------------------------------------------------------------------
    procedure in32(port : in IOPort; val : out Unsigned_32) is
        ret : Unsigned_32 := 0;
    begin
        --Asm (   "mov %0, %%dx"      & LF & HT &                 -- mov port to DX
        --        "in %%dx, %%al",                                -- mov data to EAX
        Asm ( "in %1, %0",
                Outputs => Unsigned_32'Asm_Output ("=a", ret),
                Inputs =>  Unsigned_16'Asm_Input ("d", Unsigned_16(port)),
                Volatile => True
             );
        val := ret;
    end in32;
    pragma Inline (in32);

    ---------------------------------------------------------------------------
    -- Input multiple words
    ---------------------------------------------------------------------------
    procedure ins16(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32)
    is
        dummyAddr : System.Address := addr; -- needed for an Asm_Output
        dummyCount : Unsigned_32 := count;  -- needed for an Asm_Output
    begin
        Asm("cld; repne; insw;",
            Outputs => (System.Address'Asm_Output("=D", dummyAddr),
                        Unsigned_32'Asm_Output("=c", dummyCount)),
            Inputs =>  (Unsigned_16'Asm_Input("d", port),
                        System.Address'Asm_Input("0", dummyAddr),
                        Unsigned_32'Asm_Input("1", dummyCount)),
            Clobber =>  "memory, cc",
            Volatile => True);
    end ins16;
    pragma Inline (ins16);

    ---------------------------------------------------------------------------
    -- Input multiple dwords
    ---------------------------------------------------------------------------
    procedure ins32(port : in IOPort; addr : in System.Address;
                    count : in Unsigned_32)
    is
        dummyAddr : System.Address := addr; -- needed for an Asm_Output
        dummyCount : Unsigned_32 := count;  -- needed for an Asm_Output
    begin
        Asm("cld; repne; insl;",
            Outputs => (System.Address'Asm_Output("=D", dummyAddr),
                        Unsigned_32'Asm_Output("=c", dummyCount)),
            Inputs =>  (Unsigned_16'Asm_Input("d", port),
                        System.Address'Asm_Input("0", dummyAddr),
                        Unsigned_32'Asm_Input("1", dummyCount)),
            Clobber =>  "memory, cc",
            Volatile => True);
    end ins32;
    pragma Inline (ins32);

    ---------------------------------------------------------------------------
    -- Load Interrupt Descriptor Table
    ---------------------------------------------------------------------------
    procedure lidt(idtpPtr : System.Address) is
        function raw is new Ada.Unchecked_Conversion(System.Address, Unsigned_64);
    begin
        Asm("lidt (%0)",
            Inputs => Unsigned_64'Asm_Input("r", raw(idtpPtr)),
            Volatile => True);
    end lidt;

    ---------------------------------------------------------------------------
    -- Load General Descriptor Table
    ---------------------------------------------------------------------------
    procedure lgdt(gdtPtr : System.Address) is
        function raw is new Ada.Unchecked_Conversion(System.Address, Unsigned_64);
    begin
        Asm("lgdt (%0)",
            Inputs => Unsigned_64'Asm_Input("r", raw(gdtPtr)),
            Volatile => True);
    end lgdt;

    ---------------------------------------------------------------------------
    -- Load Task Register
    ---------------------------------------------------------------------------
    procedure ltr(selector : Unsigned_16) is
    begin
        Asm("ltr %0",
            Inputs => Unsigned_16'Asm_Input("r", selector),
            Volatile => True);
    end ltr;

    ---------------------------------------------------------------------------
    -- Get rflags
    ---------------------------------------------------------------------------
    function getFlags return RFlags is
        function toRecord is new Ada.Unchecked_Conversion(Unsigned_64, RFlags);
        ret : Unsigned_64;
    begin
        Asm("pushf; pop %0",
            Outputs => Unsigned_64'Asm_Output("=r", ret),
            Volatile => True);

        return toRecord(ret);
    end getFlags;

    ---------------------------------------------------------------------------
    -- get CR0 register
    ---------------------------------------------------------------------------
    function getCR0 return Unsigned_64 is
        ret : Unsigned_64;
    begin
        Asm("mov %%cr0, %0",
            Outputs => Unsigned_64'Asm_Output("=r", ret),
            Volatile => True);

        return ret;
    end getCR0;

    ---------------------------------------------------------------------------
    -- set CR0 register
    ---------------------------------------------------------------------------
    procedure setCR0 (cr0 : Unsigned_64) is
    begin
        Asm("mov %0, %%cr0",
            Inputs => Unsigned_64'Asm_Input("r", cr0),
            Volatile => True);
    end setCR0;

    ---------------------------------------------------------------------------
    -- get CR2 register
    ---------------------------------------------------------------------------
    function getCR2 return Unsigned_64 is
        ret : Unsigned_64;
    begin
        Asm("mov %%cr2, %0",
            Outputs => Unsigned_64'Asm_Output("=r", ret),
            Volatile => True);

        return ret;
    end getCR2;

    ---------------------------------------------------------------------------
    -- get CR3 register
    ---------------------------------------------------------------------------
    function getCR3 return Integer_Address is
        ret : Integer_Address;
    begin
        Asm("mov %%cr3, %0",
            Outputs => Integer_Address'Asm_Output("=r", ret),
            Volatile => True);

        return ret;
    end getCR3;

    ---------------------------------------------------------------------------
    -- get CR4 register
    ---------------------------------------------------------------------------
    function getCR4 return Unsigned_64 is
        ret : Unsigned_64;
    begin
        Asm("mov %%cr4, %0",
            Outputs => Unsigned_64'Asm_Output("=r", ret),
            Volatile => True);

        return ret;
    end getCR4;

    ---------------------------------------------------------------------------
    -- set CR4 register
    ---------------------------------------------------------------------------
    procedure setCR4 (cr4 : Unsigned_64) is
    begin
        Asm("mov %0, %%cr4",
            Inputs => Unsigned_64'Asm_Input("r", cr4),
            Volatile => True);
    end setCR4;

    ---------------------------------------------------------------------------
    -- get base pointer
    ---------------------------------------------------------------------------
    function getRBP return Unsigned_64 is
        rbp : Unsigned_64;
    begin
        Asm("mov %%rbp, %0",
            Outputs => Unsigned_64'Asm_Output("=r", rbp),
            Volatile => True);

        return rbp;
    end getRBP;

    ---------------------------------------------------------------------------
    -- swapgs
    ---------------------------------------------------------------------------
    procedure swapgs is
    begin
        Asm("swapgs", Volatile => True);
    end swapgs;
    Pragma Inline (swapgs);

    ---------------------------------------------------------------------------
    -- Enable interrupts
    ---------------------------------------------------------------------------
    procedure sti is
    begin
        --textmode.println("sti");
        Asm("sti", Volatile => True);

        interruptsEnabled := True;      -- ghost assignment
    end sti;

    ---------------------------------------------------------------------------
    -- Disable interrupts
    ---------------------------------------------------------------------------
    procedure cli is
    begin
        --textmode.println("cli");
        Asm("cli", Volatile => True);

        interruptsEnabled := False;     -- ghost assignment
    end cli;

    ---------------------------------------------------------------------------
    -- XCHG inline function
    -- atomically try to set the "var" param to "newval" and return the old
    -- value of "var".
    ---------------------------------------------------------------------------
    procedure xchg( var     : in out Unsigned_32;
                    newval  : in Unsigned_32;
                    oldval  : out Unsigned_32)
        with SPARK_Mode => Off      --inline ASM
    is
    begin
        Asm("lock; xchgl %0, %1",
            Outputs => (Unsigned_32'Asm_Output("+m", var),      -- +m read-write-modify
                        Unsigned_32'Asm_Output("=a", oldval)),  -- need to use eax
            Inputs => Unsigned_32'Asm_Input("1", newval),       -- operand 1 is val
            Volatile => True,
            Clobber => "cc");
    end xchg;

    ---------------------------------------------------------------------------
    -- Version of XCHG strictly for LockBool, to allow SPARK proving
    ---------------------------------------------------------------------------
    procedure lock_xchg(var     : in out locks.LockBool; 
                        newval  : in locks.LockBool;
                        oldval  : out locks.LockBool)
        with SPARK_Mode => Off      --inline ASM
    is
    begin
        Asm("lock; xchgl %0, %1",
            Outputs => (Unsigned_32'Asm_Output("+m", var),      -- +m read-write-modify
                        Unsigned_32'Asm_Output("=a", oldval)),  -- need to use eax
            Inputs => Unsigned_32'Asm_Input("1", newval),       -- operand 1 is val
            Volatile => True,
            Clobber => "cc");
    end lock_xchg;

    ---------------------------------------------------------------------------
    -- Kernel panic
    -- TODO: move this to or call this from a separate error-handling packages
    ---------------------------------------------------------------------------
    procedure panic is
        dummy : exception;
    begin
        panicked := True;
        Asm("int $127", Volatile => True);
        raise dummy;
    end panic;

    ---------------------------------------------------------------------------
    -- Halt the processor.
    ---------------------------------------------------------------------------
    procedure halt is
    begin
        --while(True) loop
            -- not really "Volatile", but don't want it optimized away
            Asm("hlt", Volatile => True);
        --end loop;
    end halt;

    ---------------------------------------------------------------------------
    -- Read from a model-specific register (MSR)
    ---------------------------------------------------------------------------
    function rdmsr(msraddr : in MSR) return Unsigned_64
    is
        low : Unsigned_32;
        high : Unsigned_32;
    begin
        Asm("rdmsr",
            Inputs  =>  Unsigned_32'Asm_Input("c", msraddr),
            Outputs => (Unsigned_32'Asm_Output("=a", low),
                        Unsigned_32'Asm_Output("=d", high)),
            Volatile => True);
        return (Shift_Left(Unsigned_64(high), 32) or Unsigned_64(low));
    end rdmsr;

    ---------------------------------------------------------------------------
    -- Write to a model-specific register (MSR)
    ---------------------------------------------------------------------------
    procedure wrmsr(msraddr : in MSR; val : in Unsigned_64)
    is
        low : constant Unsigned_32 := Unsigned_32(val and 16#FFFF_FFFF#);
        high : constant Unsigned_32 := Unsigned_32(Shift_Right(val, 32));
    begin
        Asm("wrmsr",
            Inputs => (Unsigned_32'Asm_Input("c", msraddr),
                       Unsigned_32'Asm_Input("a", low),
                       Unsigned_32'Asm_Input("d", high)),
            Volatile => True);
    end wrmsr;

    ---------------------------------------------------------------------------
    -- Read Time-Stamp Counter
    ---------------------------------------------------------------------------
    function rdtsc return Unsigned_64
    is
        high    : Unsigned_32;
        low     : Unsigned_32;
    begin
        Asm("rdtsc",
            Outputs => (Unsigned_32'Asm_Output("=a", low),
                        Unsigned_32'Asm_Output("=d", high)),
            Volatile => True);

        return (Shift_Left(Unsigned_64(high), 32) or Unsigned_64(low));
    end rdtsc;

    ---------------------------------------------------------------------------
    -- Read Time-Stamp Counter & Processor ID atomically
    ---------------------------------------------------------------------------
    function rdtscp(chip : out Unsigned_32; core : out Unsigned_32)
        return Unsigned_64
    is
        high    : Unsigned_32;
        low     : Unsigned_32;
        ecx     : Unsigned_32;
    begin
        Asm("rdtscp",
            Outputs => (Unsigned_32'Asm_Output("=a", low),
                        Unsigned_32'Asm_Output("=d", high),
                        Unsigned_32'Asm_Output("=c", ecx)),
            Volatile => True);

            chip := Shift_Right(16#00FF_F000# and ecx, 12);
            core := 16#0000_0FFF# and ecx;
            
        return (Shift_Left(Unsigned_64(high), 32) or Unsigned_64(low));
    end rdtscp;

    ---------------------------------------------------------------------------
    -- rep movsb
    ---------------------------------------------------------------------------
    procedure rep_movsb (dst : in System.Address;
                         src : in System.Address;
                         len : in System.Storage_Elements.Storage_Count) is
        dstl : System.Address := dst;
        srcl : System.Address := src;
        lenl : Storage_Count  := len;
    begin
        Asm("rep movsb",
            Outputs => (
                System.Address'Asm_Output("=D", dstl),
                System.Address'Asm_Output("=S", srcl),
                Storage_Count'Asm_Output("=c", lenl)
            ),
            Inputs => (
                System.Address'Asm_Input("0", dstl),
                System.Address'Asm_Input("1", srcl),
                Storage_Count'Asm_Input("2", lenl)
            ),
            Clobber  => "memory",
            Volatile => True);
    end rep_movsb;

    ---------------------------------------------------------------------------
    -- FXSAVE Save floating point state
    ---------------------------------------------------------------------------
    procedure fxsave (saveArea : System.Address) is
    begin
        Asm("fxsave %0",
            Inputs => System.Address'Asm_Input("m", saveArea),
            Volatile => True);
    end fxsave;

    ---------------------------------------------------------------------------
    -- FXRSTOR Restore floating point state
    ---------------------------------------------------------------------------
    procedure fxrstor (saveArea : System.Address) is
    begin
        Asm("fxrstor %0",
            Inputs => System.Address'Asm_Input("m", saveArea),
            Volatile => True);
    end fxrstor;
end x86;