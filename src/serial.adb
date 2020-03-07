-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- Serial Ports
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

-- x86-specific instruction wrappers
package body serial with 
    SPARK_Mode => on
is
    -- Given a serial port base address, get the RBR (Receiver Buffer Register) address
    function RBR(port : in SerialPort) return IOPort is 
    begin
        return IOPort(port);
    end RBR;

    -- Given a serial port base address, get the THR (Transmitter Holding Register) address
    function THR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port);
    end THR;

    -- Given a serial port base address, get the DLL (Divisor Latch LSB) address
    function DLL(port : in SerialPort) return IOPort is
    begin
        return IOPort(port);
    end DLL;

    -- Given a serial port base address, get the DLM (Divisor Latch MSB) address
    function DLM(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 1);
    end DLM;

    -- Given a serial port base address, get the IER (Interrupt Enable Register) address
    function IER(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 1);
    end IER;

    -- Given a serial port base address, get the IIR (Interrupt Identification Register) address
    function IIR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 2);
    end IIR;

    -- Given a serial port base address, get the FCR (FIFO Control Register) address
    function FCR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 2);
    end FCR;

    -- Given a serial port base address, get the LCR (Line Control Register) address
    function LCR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 3);
    end LCR;

    -- Given a serial port base address, get the MCR (Modem Control Register) address
    function MCR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 4);
    end MCR;

    -- Given a serial port base address, get the LSR (Line Status Register) address
    function LSR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 5);
    end LSR;

    -- Given a serial port base address, get the MSR (Modem Status Register) address
    function MSR(port : in SerialPort) return IOPort is
    begin
        return IOPort(port + 6);
    end MSR;

    -- Initialize a COM port with default settings of 115200 8N1 with FIFO enabled,
    -- cleared, and 14-byte FIFO interrupt trigger level set.
    procedure init(port : in SerialPort) is
        baud    : constant BaudRate := RATE115200;
        line    : constant LineControl :=  (dataBits => EIGHT, 
                                            stopBit => False, 
                                            parity => NONE,
                                            others => False);

        fifo    : constant FIFOControl :=   (enable => True, 
                                            rxClear => True, 
                                            txClear => True, 
                                            fifoTrigger => bytes_14,
                                            others => False);

        modem   : constant ModemControl :=  (terminalReady => True, 
                                            RTS => True, 
                                            aux2 => True,
                                            others => False);
    begin
        init(port, baud, line, fifo, modem); 
    end init;

    -- Initialize this serial port with the baud rate and parameters
    procedure init( port : in SerialPort; 
                    rate : in BaudRate; 
                    protocol : in LineControl; 
                    fifo : in FIFOControl; 
                    modem : in ModemControl)
    is
        disableInterrupts   : constant Unsigned_8 := 16#00#;
        enableDLAB          : constant Unsigned_8 := 16#80#;
        divisorMSB          : constant Unsigned_8 := 16#00#;
        enableInterrupts    : constant Unsigned_8 := 16#01#;

        function protToByte is new Ada.Unchecked_Conversion(LineControl, Unsigned_8);
        function fifoToByte is new Ada.Unchecked_Conversion(FIFOControl, Unsigned_8);
        function modmToByte is new Ada.Unchecked_Conversion(ModemControl, Unsigned_8);
    begin
        out8(IER(port), disableInterrupts);
        out8(LCR(port), enableDLAB);
        out8(DLL(port), Unsigned_8(rate));
        out8(DLM(port), divisorMSB);
        out8(LCR(port), protToByte(protocol));
        out8(FCR(port), fifoToByte(fifo));
        out8(MCR(port), modmToByte(modem));
        out8(IER(port), enableInterrupts);
    end init;

    -- Send a single character via this serial port.
    procedure send(port : in SerialPort; c : in Character) is
    begin
        out8(THR(port), Character'Pos(c));
    end send;

    -- See if this serial port has data available.
    function hasData(port : in SerialPort) return Boolean is
        lsr : LineStatus;
        inByte : Unsigned_8;
        lsrPort : constant IOPort := serial.LSR(port);
        function byteToLine is new Ada.Unchecked_Conversion(Unsigned_8, LineStatus);
    begin
        --lsrPort := serial.LSR(port);
        in8(lsrPort, inByte);
        lsr := byteToLine(inByte);
        return lsr.dataAvailable;
    end hasData;

    -- Receive a single character via this serial port.
    function recv(port : in SerialPort) return Character is
        rec : Character;
        inByte : Unsigned_8;
        rbrPort : constant IOPort := RBR(port);
    begin
        in8(rbrPort, inByte);
        rec := Character'Val(inByte);
        return rec;
    end recv;

end serial;