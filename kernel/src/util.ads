-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS utility functions
--
-- TODO: there's probably a smarter way of using generics here for these
--  functions.
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

package Util with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Symbol is a useless type, used to prevent us from forgetting to use
    -- 'Address when referring to one.
    ---------------------------------------------------------------------------
    type Symbol is (USELESS) with Size => System.Word_Size;
    
    function addrToNum is new Ada.Unchecked_Conversion (System.Address, Unsigned_64);
    function numToAddr is new Ada.Unchecked_Conversion (Unsigned_64, System.Address);
    function elemToNum is new Ada.Unchecked_Conversion (Storage_Element, Unsigned_8);

    -----------------------------------------------------------------------
    -- setBit - set a single bit in the argument var
    -----------------------------------------------------------------------
    procedure setBit (var : in out Unsigned_8; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure setBit (var : in out Unsigned_16; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure setBit (var : in out Unsigned_32; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure setBit (var : in out Unsigned_64; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    -----------------------------------------------------------------------
    -- clearBit - clear a single bit in the argument var
    -----------------------------------------------------------------------
    procedure clearBit (var : in out Unsigned_8; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure clearBit (var : in out Unsigned_16; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure clearBit (var : in out Unsigned_32; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    procedure clearBit (var : in out Unsigned_64; bit : in Natural) 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    -----------------------------------------------------------------------
    -- isBitSet - see if an individual bit is set or not.
    -- TODO: can probably speed this up a bit with pre-built masks and 
    --  use of the TEST instruction.
    -----------------------------------------------------------------------
    function isBitSet (var : in Unsigned_8; bit : in Natural) return Boolean 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    function isBitSet (var : in Unsigned_16; bit : in Natural) return Boolean 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;
    
    function isBitSet (var : in Unsigned_32; bit : in Natural) return Boolean 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;
    
    function isBitSet (var : in Unsigned_64; bit : in Natural) return Boolean 
    with
        SPARK_Mode => On,
        Pre => bit < var'Size;

    -----------------------------------------------------------------------
    -- getByte - return a byte at position N, zero-indexed.
    -----------------------------------------------------------------------
    function getByte (var : in Unsigned_16; n : in Natural) return Unsigned_8
    with
        SPARK_Mode => On,
        Pre => n < (var'Size / 8);

    function getByte (var : in Unsigned_32; n : in Natural) return Unsigned_8
    with
        SPARK_Mode => On,
        Pre => n < (var'Size / 8);

    function getByte (var : in Unsigned_64; n : in Natural) return Unsigned_8
    with
        SPARK_Mode => On,
        Pre => n < (var'Size / 8);

    ---------------------------------------------------------------------------
    -- getFirstSetBit - return the lowest set bit
    ---------------------------------------------------------------------------
    function getFirstSetBit (var : in Unsigned_64) return Natural
    with
        Pre => var /= 0,
        Post => getFirstSetBit'Result < 63,
        Inline => True;

    ---------------------------------------------------------------------------
    -- min - get the minimum of two values
    ---------------------------------------------------------------------------
    generic
        type T is (<>);
    function min (a : in T; b : in T) return T
    with
        Inline => True;

    ---------------------------------------------------------------------------
    -- max - get the maximum of two values
    ---------------------------------------------------------------------------
    generic
        type T is (<>);
    function max (a : in T; b : in T) return T
    with
        Inline => True;

    ---------------------------------------------------------------------------
    -- memset - replacement for C's memset as required by GNAT (for static
    -- array initialization.)
    -- from comp.lang.ada h/t Shark8
    -- https://groups.google.com/forum/#!topic/comp.lang.ada/v7HUbFJqKYI
    ---------------------------------------------------------------------------
    procedure memset (addr   : System.Address; 
                      val    : System.Storage_Elements.Storage_Element;
                      len    : System.Storage_Elements.Storage_Count) with
        SPARK_Mode => Off,
        Export => True, 
        Convention => C, 
        External_Name => "memset";

    ---------------------------------------------------------------------------
    -- memcmp - replacement for C's memcmp as required by GNAT (for variant
    -- records, probably other operations as well)
    ---------------------------------------------------------------------------
    function memcmp (s1  : System.Address;
                     s2  : System.Address;
                     len : Natural) return Integer with
        SPARK_Mode => Off,
        Export => True,
        Convention => C,
        External_Name => "memcmp";

    ---------------------------------------------------------------------------
    -- memcpy - replacement for C's memcpy as required by GNAT (for exceptions)
    -- Copy len bytes from src to dest
    ---------------------------------------------------------------------------
    function memcpy (dest    : System.Address;
                     src     : System.Address;
                     len     : System.Storage_Elements.Storage_Count) return System.Address with
        SPARK_Mode => Off,
        Export => True,
        Convention => C,
        External_Name => "memcpy";

    ---------------------------------------------------------------------------
    -- memmove - replacement for C's memmove as required by GNAT for some
    -- array slice assignments
    -- Copy len bytes from src to dest, taking care to ensure that if the
    -- array ranges overlap, the dest will not be overwritten by itself.
    ---------------------------------------------------------------------------
    function memmove (dest : System.Address;
                      src  : System.Address;
                      len  : System.Storage_Elements.Storage_Count) return System.Address with
        SPARK_Mode => Off,
        Export     => True,
        Convention => C,
        External_Name => "memmove";

    ---------------------------------------------------------------------------
    -- memCopy - non-returning version of memcpy for use in SPARK mode.
    ---------------------------------------------------------------------------
    procedure memCopy (dest  : in System.Address;
                       src   : in System.Address;
                       len   : in System.Storage_Elements.Storage_Count) with Inline => True;

    ---------------------------------------------------------------------------
    -- nextPow2 - return the next highest power of 2
    ---------------------------------------------------------------------------
    function nextPow2 (n : in Unsigned_64) return Unsigned_64;

    ---------------------------------------------------------------------------
    -- roundToNearest - round to the nearest multiple 
    ---------------------------------------------------------------------------
    generic
       type T is mod <>;
    function roundToNearest (num : T; multiple : T) return T;

end Util;
