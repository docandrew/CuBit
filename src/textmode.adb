-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- EGA Textmode Console Routines
--
-- TODO: use generics to reduce some of the redundancy here
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with config;
with serial;
with strings; use strings;

package body textmode with 
    Refined_State => (ScreenState => screen),
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- The actual screen we are blitting chars to.
    ---------------------------------------------------------------------------
    screen : TextBuffer with
        Address => System.Storage_Elements.To_Address (SCREEN_ADDR),
        Async_Readers => True,
        Effective_Writes => True;

    -- get the value at a specific coordinate
    --function get (x : in Col; y : in Row) return TextBufferChar is
    --begin
    --    return screen(CursorType(y) * COLS + CursorType(x));
    --end get;

    -- print character to EGA buffer
    procedure put (x : in Col; y : in Row; fg,bg : in Color; ch : in Character) with
        Refined_Global => (Output => screen)
    is
        pragma Annotate (GNATProve, Intentional, "might not be written", 
                         "SPARK does not recognize writes to individual elements as affecting the whole.");
    begin
        screen (CursorType(y) * COLS + CursorType(x)) := (ch,fg,bg);
    end put;

    -- Print string to EGA buffer. This will only print up to the end of the 
    -- buffer. If scrolling is desired, use the print() procedures.
--    procedure put (x : in Col; y : in Row; fg,bg : in Color; str : in String) is
--        c : Natural := 0;   -- str index
--    begin
--        -- blit to end of string or end of buffer, whichever comes first.
--        for i in str'range loop
--            if x + c <= Col'last then
--                put (x + Col(c), y, fg, bg, str(i));
--                c := c + 1;
--            else
--                return;
--            end if;
--        end loop;
--    end put;

    -- clear EGA text buffer
    procedure clear (bg : Color) with
        Refined_Global => (Output => (screen, cursor))
    is
    begin
        for x in Col'range loop
            for y in Row'range loop
                put (x,y,bg,bg,' ');
            end loop;
        end loop;

        setCursor(0,0);
    end clear;

    procedure printLF with
        Global => (In_Out => (screen, cursor))
    is
    begin
        if getRow = Row'last then
            scrollUp;
        else
            setCursor(0, getRow + 1);
        end if;
    end printLF;

    -- Print a single char at the current cursor and update cursor.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary. Treat LF and CR as the same for text purposes.
    procedure print (ch : in Character; fg,bg : in Color) is
    begin
        case ch is
            when LF | CR =>
                printLF;
            --TODO: figure out a sensible way of dealing with tabs
            when HT =>
                null;
            --    if (cursor mod 4 = 0) then
            --        cursor := cursor + 4;
            --    else
            --        cursor := cursor mod 4;
            --    end if;
            --
            --    if getRow >= Row'last then
            --        scrollUp;
            --    end if;
            when NUL =>
                null; --do nothing
            when others =>
                put(getCol, getRow, fg, bg, ch);
                
                if getCol = Col'last then
                    printLF;
                else
                    cursor := cursor + 1;
                end if;
        end case;

        -- Mirror to serial port, if enabled.
        if(config.serialMirror and then ch /= NUL) then
            serial.send(config.serialMirrorPort, ch);
        end if;
    end print;

    procedure print (ch : in Character) is
    begin
        print (ch, LT_GRAY, BLACK);
    end print;

    procedure print (str : in String; fg,bg : in Color) is
    begin
        for i in str'range loop
            print (str(i), fg, bg);
        end loop;
    end print;

    procedure print (str : in String) is
    begin
        print (str, LT_GRAY, BLACK);
    end print;

    procedure println (str : in String; fg,bg : in Color) is
    begin
        print (str,fg,bg);
        println;
    end println;

    procedure println (str : in String) is
    begin
        println(str, LT_GRAY, BLACK);
    end println;

    procedure println is
    begin
        print (LF);
    end println;

    ---------------------------------------------------------------------------
    -- Print Unsigned_32 as an integer value
    ---------------------------------------------------------------------------
    procedure printd(n : in Unsigned_32; fg,bg : in Color) with
        SPARK_Mode => On
    is
        MAX_DIGITS : constant := 10;
        myDigits : array (1..MAX_DIGITS) of Character := (others => NUL);
        i : Unsigned_32 := n;
        c : Natural := 0;
    begin
        if i = 0 then
            print('0', fg, bg);
            return;
        end if;

        while (i > 0 and c < 10) loop
            pragma Loop_Invariant (c >= 0);

            myDigits(MAX_DIGITS - c) := Character'Val((i mod 10) + 48);
            i := i / 10;
            c := c + 1;
        end loop;

        for j in myDigits'Range loop
            print(myDigits(j), fg, bg);
        end loop;
    end printd;

    procedure printd(n : in Unsigned_32) is
    begin
        printd(n, LT_GRAY, BLACK);
    end printd;

    procedure printdln(n : in Unsigned_32; fg,bg : in Color) is
    begin
        printd(n,fg,bg);
        println;
    end printdln;

    procedure printdln(n : in Unsigned_32) is
    begin
        printd(n);
        println;
    end printdln;

    ---------------------------------------------------------------------------
    -- Print Unsigned_64 as an integer value
    ---------------------------------------------------------------------------
    procedure printd(n : in Unsigned_64; fg,bg : in Color) with
        SPARK_Mode => On
    is
        MAX_DIGITS : constant := 20;
        myDigits : array (1..MAX_DIGITS) of Character := (others => NUL);
        i : Unsigned_64 := n;
        c : Natural := 0;
    begin
        if i = 0 then
            print('0', fg, bg);
            return;
        end if;

        while (i > 0 and c < 10) loop
            pragma Loop_Invariant (c >= 0);

            myDigits(MAX_DIGITS - c) := Character'Val((i mod 10) + 48);
            i := i / 10;
            c := c + 1;
        end loop;

        for j in myDigits'Range loop
            print(myDigits(j), fg, bg);
        end loop;
    end printd;

    procedure printd(n : in Unsigned_64) is
    begin
        printd(n, LT_GRAY, BLACK);
    end printd;

    procedure printdln(n : in Unsigned_64; fg,bg : in Color) is
    begin
        printd(n,fg,bg);
        println;
    end printdln;

    procedure printdln(n : in Unsigned_64) is
    begin
        printd(n);
        println;
    end printdln;

    ---------------------------------------------------------------------------
    -- Print integer
    ---------------------------------------------------------------------------
    procedure print(i : in Integer; fg,bg : in Color) with 
        SPARK_Mode => On 
    is
        MAX_DIGITS : constant := 10;
        myDigits : array (1..MAX_DIGITS) of Character := (others => NUL);
        c : Integer := 0;
        i1 : Long_Integer := Long_Integer(i);  -- to prevent overflow with abs
    begin
        if i < 0 then
            print('-', fg, bg);
            i1 := abs i1;
        end if;

        if i1 = 0 then
            print('0', fg, bg);
            return;
        end if;
        
        while (i1 > 0 and c < 10) loop
            pragma Loop_Invariant (c >= 0);
            --pragma Loop_Invariant (c < 10);

            -- '0' = 48 ASCII
            myDigits(MAX_DIGITS - c) := Character'Val((i1 mod 10) + 48);
            i1 := i1 / 10;
            c := c + 1;
        end loop;

        -- works because our print method ignores NUL chars
        for j in myDigits'Range loop
            print(myDigits(j), fg, bg);
        end loop;
    end print;

    procedure print(i : in Integer) is
    begin
        print(i, LT_GRAY, BLACK);
    end;

    procedure println(i : in Integer; fg,bg : in Color) is
    begin
        print(i,fg,bg);
        println;
    end println;

    procedure println(i : in Integer) is
    begin
        print(i);
        println;
    end println;

    procedure print(u8 : Unsigned_8) is
    begin
        print(strings.toHexString(u8));
    end print;

    procedure println(u8 : Unsigned_8) is
    begin
        print(u8);
        println;
    end println;

    procedure print(u16 : Unsigned_16) is
    begin
        print(strings.toHexString(u16));
    end print;

    procedure println(u16 : Unsigned_16) is
    begin
        print(u16);
        println;
    end println;

    procedure print(u32 : Unsigned_32) is
    begin
        print(strings.toHexString(u32));
    end print;

    procedure println(u32 : Unsigned_32) is
    begin
        print(u32);
        println;
    end println;

    procedure print(u64 : Unsigned_64) is
    begin
        print(strings.toHexString(u64));
    end print;

    procedure println(u64 : Unsigned_64) is
    begin
        print(u64);
        println;
    end println;

    procedure print(b : in Boolean; fg,bg : in Color) is
    begin
        if b then
            print("True",fg,bg);
        else
            print("False",fg,bg);
        end if;
    end print;

    procedure print(b : in Boolean) is
    begin
        print(b, LT_GRAY, BLACK);
    end print;

    procedure println(b : in Boolean; fg,bg : in Color) is
    begin
        print(b,fg,bg);
        println;
    end println;

    procedure println(b : in Boolean) is
    begin
        print(b);
        println;
    end println;

    -- addresses
    procedure print(addr : System.Address) is
    begin
        print(To_Integer(addr));
    end print;

    procedure println(addr : System.Address) is
    begin
        print(addr);
        println;
    end println;

    -- Integer Addresses
    procedure print(addr : Integer_Address) is
    begin
        print(toHexString(Unsigned_64(addr)));
    end print;

    procedure println(addr : Integer_Address) is
    begin
        print(addr);
        println;
    end println;

    -- C-style strings
    procedure printz(addr : System.Address) is
        use System.Storage_Elements;
        nextAddr : System.Address := addr;
    begin
        loop
            getchar: 
            declare
                c : Character with
                    Import, Address => nextAddr;
            begin
                exit when c = NUL;
                --println(nextAddr);
                print(c);
                nextAddr := To_Address(To_Integer(nextAddr) + 1);
            end getchar;
        end loop;
    end printz;

    -- Set cursor to a specific x,y coordinate
    procedure setCursor (x : Col; y : Row) is
    begin
        cursor := (CursorType(y) * COLS + CursorType(x));
    end setCursor;

    -- Get the row of our cursor
    function getRow return Row is
    begin
        return Row(cursor / 80);
    end getRow;

    -- Get column of our cursor
    function getCol return Col is
    begin
        return Col(cursor mod 80);
    end getCol;

    -- Scroll all text up one line. Set cursor to bottom left.
    procedure scrollUp 
    is
    begin
        for y in 0 .. (Row'last - 1) loop
            for x in Col'range loop
                screen (CursorType(y) * COLS + CursorType(x)) := 
                    screen(CursorType(y + Row(1)) * COLS + CursorType(x));
            end loop;
        end loop;

        -- clear line at end
        for x in Col'range loop
            put (x, Row'last, BLACK, BLACK, ' ');
        end loop;
        
        -- move cursor to bottom left of screen
        setCursor(0, Row'last);
    end scrollUp;

end textmode;