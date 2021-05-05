-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary CuBitOS utility functions
-------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;
with x86;

package body Util with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setBit overloads
    ---------------------------------------------------------------------------
    procedure setBit(var : in out Unsigned_8; bit : in Natural) 
        with SPARK_Mode => On
    is
    begin
        var := var or (Shift_Left(1, bit));
    end setBit;


    procedure setBit(var : in out Unsigned_16; bit : in Natural) 
        with SPARK_Mode => On
    is
    begin
        var := var or (Shift_Left(1, bit));
    end setBit;


    procedure setBit(var : in out Unsigned_32; bit : in Natural) 
        with SPARK_Mode => On
    is
    begin
        var := var or (Shift_Left(1, bit));
    end setBit;


    procedure setBit(var : in out Unsigned_64; bit : in Natural) 
        with SPARK_Mode => On
    is
    begin
        var := var or (Shift_Left(1, bit));
    end setBit;


    ---------------------------------------------------------------------------
    -- clearBit overloads
    ---------------------------------------------------------------------------
    procedure clearBit(var : in out Unsigned_8; bit : in Natural) 
        with SPARK_Mode => On
    is
        mask : constant Unsigned_8 := not Shift_Left(1, bit);
    begin
        var := var and mask;
    end clearBit;


    procedure clearBit(var : in out Unsigned_16; bit : in Natural) 
        with SPARK_Mode => On
    is
        mask : constant Unsigned_16 := not Shift_Left(1, bit);
    begin
        var := var and mask;
    end clearBit;


    procedure clearBit(var : in out Unsigned_32; bit : in Natural) 
        with SPARK_Mode => On
    is
        mask : constant Unsigned_32 := not Shift_Left(1, bit);
    begin
        var := var and mask;
    end clearBit;


    procedure clearBit(var : in out Unsigned_64; bit : in Natural) 
        with SPARK_Mode => On
    is
        mask : constant Unsigned_64 := not Shift_Left(1, bit);
    begin
        var := var and mask;
    end clearBit;


    ---------------------------------------------------------------------------
    -- isBitSet overloads
    ---------------------------------------------------------------------------
    function isBitSet(var : in Unsigned_8; bit : in Natural) return Boolean
        with SPARK_Mode => On
    is
        mask : constant Unsigned_8 := Shift_Left(1, bit);
    begin
        return not ((var and mask) = 0);
    end isBitSet;


    function isBitSet(var : in Unsigned_16; bit : in Natural) return Boolean
        with SPARK_Mode => On
    is
        mask : constant Unsigned_16 := Shift_Left(1, bit);
    begin
        return not ((var and mask) = 0);
    end isBitSet;


    function isBitSet(var : in Unsigned_32; bit : in Natural) return Boolean
        with SPARK_Mode => On
    is
        mask : constant Unsigned_32 := Shift_Left(1, bit);
    begin
        return not ((var and mask) = 0);
    end isBitSet;


    function isBitSet(var : in Unsigned_64; bit : in Natural) return Boolean
        with SPARK_Mode => On
    is
        mask : constant Unsigned_64 := Shift_Left(1, bit);
    begin
        return not ((var and mask) = 0);
    end isBitSet;


    ---------------------------------------------------------------------------
    -- getByte overloads
    ---------------------------------------------------------------------------
    function getByte(var : in Unsigned_16; n : in Natural) return Unsigned_8
        with SPARK_Mode => On
    is
        mask : constant Unsigned_16 := Shift_Left(16#FF#, n*8);
    begin
        return Unsigned_8(Shift_Right(var and mask, n*8));
    end getByte;


    function getByte(var : in Unsigned_32; n : in Natural) return Unsigned_8
        with SPARK_Mode => On
    is
        mask : constant Unsigned_32 := Shift_Left(16#FF#, n*8);
    begin
        return Unsigned_8(Shift_Right(var and mask, n*8));
    end getByte;


    function getByte(var : in Unsigned_64; n : in Natural) return Unsigned_8
        with SPARK_Mode => On
    is
        mask : constant Unsigned_64 := Shift_Left(16#FF#, n*8);
    begin
        return Unsigned_8(Shift_Right(var and mask, n*8));
    end getByte;


    ---------------------------------------------------------------------------
    -- min - get the minimum of two values
    ---------------------------------------------------------------------------
    function min (a : in T; b : in T) return T with
        SPARK_Mode => On
    is
    begin
        return (if (a < b) then a else b);
    end min;


    ---------------------------------------------------------------------------
    -- max - get the maximum of two values
    ---------------------------------------------------------------------------
    function max (a : in T; b : in T) return T with
        SPARK_Mode => On
    is
    begin
        return (if (a > b) then a else b);
    end max;
    

    ---------------------------------------------------------------------------
    -- getFirstSetBit - return the lowest set bit
    ---------------------------------------------------------------------------
    function getFirstSetBit(var : in Unsigned_64) return Natural
        with SPARK_Mode => Off   -- inline asm
    is
        ret : Unsigned_64;
    begin
        Asm("bsfq %1,%0",
            Outputs => Unsigned_64'Asm_Output("=r", ret),
            Inputs => Unsigned_64'Asm_Input("r", var));

        return Natural(ret);
    end getFirstSetBit;


    -- Set all elements of memory to a particular value.
    procedure memset(addr   : System.Address; 
                     val    : System.Storage_Elements.Storage_Element;
                     len    : System.Storage_Elements.Storage_Count)
        with SPARK_Mode => Off 
    is
        mem : Storage_Array(1..len)
            with Import, Address => addr;
    begin
        for element of mem loop
            element := val;
        end loop;
    end memset;


    -- See if all elements are the same between two arrays
    function memcmp(s1  : System.Address;
                    s2  : System.Address;
                    len : Natural) return Integer
        with SPARK_Mode => Off
    is
        mem1 : Storage_Array(1..Storage_Offset(len))
            with Import, Address => s1;
        mem2 : Storage_Array(1..Storage_Offset(len))
            with Import, Address => s2;
        diff : Integer;
    begin
        if len = 0 then
            return 0;
        end if;

        for i in 1..len loop
            
            diff := Integer(mem1(Storage_Offset(i)))
                    - Integer(mem2(Storage_Offset(i)));

            if diff /= 0 then
                return diff;
            end if;
        end loop;

        return 0;
    end memcmp;

    ---------------------------------------------------------------------------
    -- memcpy
    -- @TODO we can optimize this, check for dest, src alignment and
    -- len alignment and copy by words.
    ---------------------------------------------------------------------------
    function memcpy(dest    : System.Address;
                    src     : System.Address;
                    len     : System.Storage_Elements.Storage_Count) return System.Address
        with SPARK_Mode => Off
    is
        -- memd : Storage_Array(1..Storage_Offset(len))
        --     with Import, Address => dest;
        -- mems : Storage_Array(1..Storage_Offset(len))
        --     with Import, Address => src;
    begin
        -- if len > 0 then
            -- for i in 1..len loop
            --     memd(Storage_Offset(i)) := mems(Storage_Offset(i));
            -- end loop;
        -- end if;
        x86.rep_movsb (dest, src, len);

        return dest;
    end memcpy;

    ---------------------------------------------------------------------------
    -- memmove
    ---------------------------------------------------------------------------
    function memmove (dest : System.Address;
                      src  : System.Address;
                      len  : System.Storage_Elements.Storage_Count) return System.Address
        with SPARK_Mode => Off
    is
        use type System.Address;

        memd : Storage_Array(1..len)
            with Import, Address => dest;
        
        mems : Storage_Array(1..len)
            with Import, Address => src;
    begin
        -- These ranges might overlap, so if destination is above the source,
        -- copy back to front. Otherwise, copy front-to-back.

        if dest < src then
            return memcpy (dest, src, len);
        else
            for i in reverse 1..len loop
                memd(i) := mems(i);
            end loop;
        end if;

        return dest;
    end memmove;

    ---------------------------------------------------------------------------
    -- memCopy
    ---------------------------------------------------------------------------
    procedure memCopy (dest  : in System.Address;
                       src   : in System.Address;
                       len   : in System.Storage_Elements.Storage_Count) with SPARK_Mode => Off
    is
        dummy : System.Address;
    begin
        dummy := memcpy(dest, src, len);
    end memCopy;

    ---------------------------------------------------------------------------
    -- next power of 2
    -- Find the rightmost 1 bit using lzcnt. First, subtract 1 to account
    -- for n that's already a power of 2. Shift 1 by 63 - number of leading
    -- zeroes to get the next power of 2.
    ---------------------------------------------------------------------------
    function nextPow2 (n : in Unsigned_64) return Unsigned_64 with
        SPARK_Mode => Off
    is
        leadingZeroes : Unsigned_64;
        m : Unsigned_64 := n - 1;
    begin
        Asm("lzcntq %1, %0",
            Outputs => Unsigned_64'Asm_Output("=r", leadingZeroes),
            Inputs => Unsigned_64'Asm_Input("rm", m),
            Clobber => "cc");
        
        return Shift_Left(Unsigned_64(1), Natural(64 - leadingZeroes));
    end nextPow2;

    ---------------------------------------------------------------------------
    -- roundToNearest
    ---------------------------------------------------------------------------
    function roundToNearest (num : T; multiple : T) return T with
        SPARK_Mode => On
    is
        remainder : T;
    begin
        if multiple = 0 then
            return num;
        end if;

        remainder := num rem multiple;

        if remainder = 0 then
            return num;
        end if;

        return num + multiple - remainder;
    end roundToNearest;

end Util;
