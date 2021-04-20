-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2021 Jon Andrew
--
-- Text I/O
--
-- To use this package with a particular video driver, make sure the driver can
-- supply the number of rows and columns of mono-spaced text that the video
-- driver can support, and methods for drawing a single character at a
-- particular row and column.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Strings; use Strings;

package body TextIO is

    type OutputType is (NONE, SERIAL_ONLY, VIDEO_ONLY, SERIAL_VIDEO);
    output : OutputType := NONE;

    -- CURSOR_END : constant := rows * cols - 1;

    -- type Col is new Natural range 0..cols - 1;
    -- type Row is new Natural range 0..rows - 1;

    -- type CursorType is new Natural range 0 .. CURSOR_END;

    -- cursor : CursorType;
    rows        : Natural := 0;
    cols        : Natural := 0;
    lastRow     : Natural := 0;
    lastCol     : Natural := 0;
    cursor      : Natural := 0;
    CURSOR_END  : Natural := 0;

    put         : PutCharProc  := null;
    scrollUp    : ScrollUpProc := null;
    clearScreen : ClearProc    := null;

    ---------------------------------------------------------------------------
    -- setVideoDriver
    ---------------------------------------------------------------------------
    procedure setVideo (driver : TextIO.TextIOInterface) is
    begin
        if output = NONE then
            output := VIDEO_ONLY;
        elsif output = SERIAL_ONLY then
            output := SERIAL_VIDEO;
        end if;

        -- Set procedures for each of the Text IO operations.
        TextIO.put         := driver.put;
        TextIO.scrollUp    := driver.scroll;
        TextIO.clearScreen := driver.clear;

        -- Set cursor limit
        TextIO.rows        := driver.rows;
        TextIO.cols        := driver.cols;
        TextIO.lastRow     := driver.rows - 1;
        TextIO.lastCol     := driver.cols - 1;
        CURSOR_END         := driver.rows * driver.cols - 1;
    end setVideo;

    ---------------------------------------------------------------------------
    -- enableSerial
    ---------------------------------------------------------------------------
    procedure enableSerial is
    begin
        if output = NONE then
            output := SERIAL_ONLY;
        elsif output = VIDEO_ONLY then
            output := SERIAL_VIDEO;
        end if;
    end enableSerial;

    ---------------------------------------------------------------------------
    -- disableVideo
    ---------------------------------------------------------------------------
    procedure disableVideo is
    begin
        if output = SERIAL_VIDEO then
            output := SERIAL_ONLY;
        elsif output = VIDEO_ONLY then
            output := NONE;
        end if;
    end disableVideo;

    ---------------------------------------------------------------------------
    -- clear
    ---------------------------------------------------------------------------
    procedure clear (bg : TextIO.Color) is
    begin
        clearScreen (bg);
    end clear;

    ---------------------------------------------------------------------------
    -- Set cursor to a specific column and row.
    ---------------------------------------------------------------------------
    procedure setCursor (x : Natural; y : Natural) is
    begin
        cursor := (y * cols + x);
    end setCursor;

    ---------------------------------------------------------------------------
    -- Get the row of our cursor
    ---------------------------------------------------------------------------
    function getRow return Natural is
    begin
        return cursor / cols;
    end getRow;

    ---------------------------------------------------------------------------
    -- Get column of our cursor
    ---------------------------------------------------------------------------
    function getCol return Natural is
    begin
        return cursor mod cols;
    end getCol;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    procedure printLF is
    begin
        if getRow = lastRow then
            scrollUp.all;
            setCursor (0, lastRow);
        else
            setCursor (0, getRow + 1);
        end if;
    end printLF;

    -- Print a single char at the current cursor and update cursor.
    --  This will wrap cursor around to the next row and scroll up
    --  if necessary. Treat LF and CR as the same for text purposes.
    procedure print (ch : in Character; fg,bg : in TextIO.Color) is
        use ASCII;
    begin
        if output = VIDEO_ONLY or output = SERIAL_VIDEO then
            case ch is
                when LF | CR =>
                    printLF;

                when HT =>
                    null;

                when NUL =>
                    null;

                when others =>
                    put (getCol, getRow, fg, bg, ch);
                    
                    if getCol = lastCol then
                        printLF;
                    else
                        cursor := cursor + 1;
                    end if;
            end case;
        end if;

        if output = SERIAL_ONLY or output = SERIAL_VIDEO then
            -- Mirror to serial port, if enabled.
            if ch /= NUL and Character'Pos(ch) < 127 then
                Serial.send (Config.serialMirrorPort, ch);
            end if;
        end if;
    end print;

    procedure print (ch : in Character) is
    begin
        print (ch, LT_GRAY, BLACK);
    end print;

    procedure print (str : in String; fg,bg : in TextIO.Color) is
    begin
        for i in str'range loop
            print (str(i), fg, bg);
        end loop;
    end print;

    procedure print (str : in String) is
    begin
        print (str, LT_GRAY, BLACK);
    end print;

    procedure println (str : in String; fg,bg : in TextIO.Color) is
    begin
        print (str,fg,bg);
        println;
    end println;

    procedure println (str : in String) is
    begin
        println(str, LT_GRAY, BLACK);
    end println;

    procedure println is
        use ASCII;
    begin
        print (LF);
    end println;

    ---------------------------------------------------------------------------
    -- Print Unsigned_32 as an integer value
    ---------------------------------------------------------------------------
    procedure printd (n : in Unsigned_32; fg,bg : in TextIO.Color) with
        SPARK_Mode => On
    is
        use ASCII;

        MAX_DIGITS : constant := 10;
        myDigits : array (1..MAX_DIGITS) of Character := (others => NUL);
        i : Unsigned_32 := n;
        c : Natural := 0;
    begin
        if i = 0 then
            print ('0', fg, bg);
            return;
        end if;

        while (i > 0 and c < 10) loop
            pragma Loop_Invariant (c >= 0);

            myDigits(MAX_DIGITS - c) := Character'Val((i mod 10) + 48);
            i := i / 10;
            c := c + 1;
        end loop;

        for j in myDigits'Range loop
            print (myDigits(j), fg, bg);
        end loop;
    end printd;

    procedure printd (n : in Unsigned_32) is
    begin
        printd (n, LT_GRAY, BLACK);
    end printd;

    procedure printdln (n : in Unsigned_32; fg,bg : in TextIO.Color) is
    begin
        printd (n,fg,bg);
        println;
    end printdln;

    procedure printdln (n : in Unsigned_32) is
    begin
        printd (n);
        println;
    end printdln;

    ---------------------------------------------------------------------------
    -- Print Unsigned_64 as an integer value
    ---------------------------------------------------------------------------
    procedure printd (n : in Unsigned_64; fg,bg : in TextIO.Color) with
        SPARK_Mode => On
    is
        use ASCII;

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
            print (myDigits(j), fg, bg);
        end loop;
    end printd;

    procedure printd (n : in Unsigned_64) is
    begin
        printd (n, LT_GRAY, BLACK);
    end printd;

    procedure printdln (n : in Unsigned_64; fg,bg : in TextIO.Color) is
    begin
        printd (n,fg,bg);
        println;
    end printdln;

    procedure printdln (n : in Unsigned_64) is
    begin
        printd (n);
        println;
    end printdln;

    ---------------------------------------------------------------------------
    -- Print integer
    ---------------------------------------------------------------------------
    procedure print (i : in Integer; fg,bg : in TextIO.Color) with 
        SPARK_Mode => On 
    is
        use ASCII;

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
            print (myDigits(j), fg, bg);
        end loop;
    end print;

    procedure print (i : in Integer) is
    begin
        print (i, LT_GRAY, BLACK);
    end;

    procedure println (i : in Integer; fg,bg : in TextIO.Color) is
    begin
        print (i,fg,bg);
        println;
    end println;

    procedure println (i : in Integer) is
    begin
        print (i);
        println;
    end println;

    procedure print (u8 : Unsigned_8) is
    begin
        print (Strings.toHexString (u8));
    end print;

    procedure println (u8 : Unsigned_8) is
    begin
        print (u8);
        println;
    end println;

    procedure print(u16 : Unsigned_16) is
    begin
        print (Strings.toHexString(u16));
    end print;

    procedure println (u16 : Unsigned_16) is
    begin
        print (u16);
        println;
    end println;

    procedure print (u32 : Unsigned_32) is
    begin
        print (Strings.toHexString(u32));
    end print;

    procedure println (u32 : Unsigned_32) is
    begin
        print (u32);
        println;
    end println;

    procedure print (u64 : Unsigned_64) is
    begin
        print (Strings.toHexString(u64));
    end print;

    procedure println (u64 : Unsigned_64) is
    begin
        print (u64);
        println;
    end println;

    procedure print (b : in Boolean; fg,bg : in TextIO.Color) is
    begin
        if b then
            print ("True",fg,bg);
        else
            print ("False",fg,bg);
        end if;
    end print;

    procedure print (b : in Boolean) is
    begin
        print (b, LT_GRAY, BLACK);
    end print;

    procedure println (b : in Boolean; fg,bg : in TextIO.Color) is
    begin
        print (b,fg,bg);
        println;
    end println;

    procedure println (b : in Boolean) is
    begin
        print (b);
        println;
    end println;

    -- addresses
    procedure print (addr : System.Address) is
    begin
        print (To_Integer(addr));
    end print;

    procedure println (addr : System.Address) is
    begin
        print (addr);
        println;
    end println;

    -- Integer Addresses
    procedure print (addr : Integer_Address) is
    begin
        print (toHexString (Unsigned_64(addr)));
    end print;

    procedure println (addr : Integer_Address) is
    begin
        print (addr);
        println;
    end println;

    -- C-style strings
    procedure printz (addr : System.Address) is
        use ASCII;
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
                print (c);
                nextAddr := To_Address (To_Integer (nextAddr) + 1);
            end getchar;
        end loop;
    end printz;

end TextIO;
