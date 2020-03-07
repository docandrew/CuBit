-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2019 Jon Andrew
--
-- Serial Ports
-------------------------------------------------------------------------------
with x86; use x86;
with Interfaces; use Interfaces;

-- 16550A UART Register Specification and Functions
-- Some info gathered from: https://www.lammertbies.nl/comm/info/serial-uart.html
package serial with 
    SPARK_Mode => On
is
    -- We use BaudRate as the LSB of the divisor for the UART clock
    -- The MSB is only used for baud rates of 50 & 300, so we aren't
    --  going to worry about those here.
    type BaudRate is new Unsigned_8;

    RATE115200  : constant BaudRate := 1;
    RATE57600   : constant BaudRate := 2;
    RATE38400   : constant BaudRate := 3;
    RATE19200   : constant BaudRate := 6;
    RATE9600    : constant BaudRate := 12;
    RATE4800    : constant BaudRate := 24;
    RATE2400    : constant BaudRate := 48;
    RATE1200    : constant BaudRate := 96;
    
    -- Number of data bits sent per UART word
    type DataBitsType is (
        FIVE, 
        SIX, 
        SEVEN, 
        EIGHT)
    with Size => 8;

    for DataBitsType use (
        FIVE => 0, 
        SIX => 1, 
        SEVEN => 2, 
        EIGHT => 3);
    
    -- When (if at all) parity bits should be sent
    -- Note the presence of BAD__X enumerated values.
    -- These can't exist, but are necessary to get rid
    -- of "holes" in the enumerated type. GNAT tries to
    -- insert un-pragma-able runtime checks for these.
    type ParityType is (
        NONE,
        ODD,
        BAD_1,
        EVEN,
        BAD_2,
        MARK,
        BAD_3,
        SPACE);
    
    for ParityType use (
        NONE => 0,
        ODD => 1,
        BAD_1 => 2,
        EVEN => 3,
        BAD_2 => 4,
        MARK => 5,
        BAD_3 => 6,
        SPACE => 7);

    ---------------------------------------------------------------------------
    -- Line Control Register
    ---------------------------------------------------------------------------
    type LineControl is
        record
            dataBits : DataBitsType;
            stopBit : Boolean;
            parity : ParityType;
            breakSignal : Boolean;
            DLAB : Boolean;         -- if 1, divisor latch register available.
                                    -- if 0, receiver buffer register available.
        end record
        with Size => 8;
    
    for LineControl use
        record
            dataBits    at 0 range 0 .. 1;
            stopBit     at 0 range 2 .. 2;
            parity      at 0 range 3 .. 5;
            breakSignal at 0 range 6 .. 6;
            DLAB        at 0 range 7 .. 7;
        end record;
    
    ---------------------------------------------------------------------------
    -- Interrupt enable register
    ---------------------------------------------------------------------------
    type InterruptEnable is
        record
            dataAvailable : Boolean;
            txEmpty : Boolean;
            rxChange : Boolean;
            modemChange : Boolean;
            sleep : Boolean;
            lowPower : Boolean;
            res1 : Boolean;             -- reserved
            res2 : Boolean;             -- reserved
        end record with Size => 8;
    
    for InterruptEnable use
        record
            dataAvailable   at 0 range 0 .. 0;
            txEmpty         at 0 range 1 .. 1;
            rxChange        at 0 range 2 .. 2;
            modemChange     at 0 range 3 .. 3;
            sleep           at 0 range 4 .. 4;
            lowPower        at 0 range 5 .. 5;
            res1            at 0 range 6 .. 6;
            res2            at 0 range 7 .. 7;
        end record;

    ---------------------------------------------------------------------------
    -- Interrupt identification register (and change bits)
    ---------------------------------------------------------------------------
    type UARTChangeType is (
        modemStatus,
        txEmpty,
        dataAvailable,
        lineStatusChg,
        BAD_1,
        BAD_2,
        charTimeout);

    for UARTChangeType use (
        modemStatus => 0,
        txEmpty => 1,
        dataAvailable => 2,
        lineStatusChg => 3,
        BAD_1 => 4,
        BAD_2 => 5,
        charTimeout => 6);

    type FIFOInterruptType is (
        noFIFO,
        BAD_1,
        unusableFIFO,
        FIFOEnabled);
    
    for FIFOInterruptType use (
        noFIFO => 0,
        BAD_1 => 1,
        unusableFIFO => 2,
        FIFOEnabled => 3);

    type InterruptID is
        record
            intPending : Boolean;
            change : UARTChangeType;
            res1 : Boolean;                     -- reserved
            res_enable64 : Boolean;             -- reserved or enable 64-byte FIFO depending on model
            fifoInt : FIFOInterruptType;
        end record with Size => 8;
    
    for InterruptID use
        record
            intPending      at 0 range 0 .. 0;
            change          at 0 range 1 .. 3;
            res1            at 0 range 4 .. 4;
            res_enable64    at 0 range 5 .. 5;
            fifoInt         at 0 range 6 .. 7;
        end record;

    ---------------------------------------------------------------------------
    -- FIFO Control Register
    ---------------------------------------------------------------------------
    type FIFOTriggerLevel is (
        bytes_1,
        bytes_4,
        bytes_8,
        bytes_14);

    for FIFOTriggerLevel use (
        bytes_1 => 0,
        bytes_4 => 1,
        bytes_8 => 2,
        bytes_14 => 3);
    
    type FIFOControl is 
        record
            enable : Boolean            := True;            -- enable FIFOs
            rxClear : Boolean           := True;            -- clear receive FIFO
            txClear : Boolean           := True;            -- clear transmit FIFO
            dmaMode : Boolean           := False;           -- select DMA mode 0 or 1
            res1    : Boolean           := False;           -- reserved
            res_enable64 : Boolean      := False;           -- reserved or enable 64-byte FIFO
            fifoTrigger : FIFOTriggerLevel := bytes_14;
        end record with Size => 8;

    for FIFOControl use 
        record
            enable          at 0 range 0 .. 0;
            rxClear         at 0 range 1 .. 1;
            txClear         at 0 range 2 .. 2;
            dmaMode         at 0 range 3 .. 3;
            res1            at 0 range 4 .. 4;
            res_enable64    at 0 range 5 .. 5;
            fifoTrigger     at 0 range 6 .. 7;
        end record;
    
    ---------------------------------------------------------------------------
    -- Modem Control Register
    ---------------------------------------------------------------------------
    type ModemControl is
        record
            terminalReady : Boolean;
            RTS : Boolean;
            aux1 : Boolean;
            aux2 : Boolean;
            loopback : Boolean;
            autoflow : Boolean;
            res1 : Boolean;
            res2 : Boolean;
        end record with Size => 8;

    for ModemControl use
        record
            terminalReady   at 0 range 0 .. 0;
            RTS             at 0 range 1 .. 1;
            aux1            at 0 range 2 .. 2;
            aux2            at 0 range 3 .. 3;
            loopback        at 0 range 4 .. 4;
            autoflow        at 0 range 5 .. 5;
            res1            at 0 range 6 .. 6;
            res2            at 0 range 7 .. 7;
        end record;

    ---------------------------------------------------------------------------
    -- Line Status Register
    ---------------------------------------------------------------------------
    type LineStatus is
        record
            dataAvailable : Boolean;
            overrunError : Boolean;
            parityError : Boolean;
            framingError : Boolean;
            breakSignal : Boolean;
            THREmpty : Boolean;
            THREmptyIdle : Boolean;
            FIFOError : Boolean;
        end record with Size => 8;
    
    for LineStatus use
        record
            dataAvailable   at 0 range 0 .. 0;
            overrunError    at 0 range 1 .. 1;
            parityError     at 0 range 2 .. 2;
            framingError    at 0 range 3 .. 3;
            breakSignal     at 0 range 4 .. 4;
            THREmpty        at 0 range 5 .. 5;
            THREmptyIdle    at 0 range 6 .. 6;
            FIFOError       at 0 range 7 .. 7;
        end record;

    ---------------------------------------------------------------------------
    -- COM Port base registers
    ---------------------------------------------------------------------------
    type SerialPort is new IOPort;

    COM1 : constant SerialPort := 16#3F8#;
    COM2 : constant SerialPort := 16#2F8#;
    COM3 : constant SerialPort := 16#3E8#;
    COM4 : constant SerialPort := 16#2E8#;

    -- Offsets for each of the control/status registers
    --UART_DATA           : constant := 0;
    --UART_INT_ENABLE     : constant := 1;
    --UART_FIFO_CONTROL   : constant := 2;
    --UART_LINE_CONTROL   : constant := 3;
    --UART_MODEM_CONTROL  : constant := 4;
    --UART_LINE_STATUS    : constant := 5;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the RBR (Receiver Buffer Register) address
    ---------------------------------------------------------------------------
    function RBR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the THR (Transmitter Holding Register) address
    ---------------------------------------------------------------------------
    function THR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the DLL (Divisor Latch LSB) address
    ---------------------------------------------------------------------------
    function DLL(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the DLM (Divisor Latch MSB) address
    ---------------------------------------------------------------------------
    function DLM(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the IER (Interrupt Enable Register) address
    ---------------------------------------------------------------------------
    function IER(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the IIR (Interrupt Identification Register)
    -- address
    ---------------------------------------------------------------------------
    function IIR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the FCR (FIFO Control Register) address
    ---------------------------------------------------------------------------
    function FCR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the LCR (Line Control Register) address
    ---------------------------------------------------------------------------
    function LCR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the MCR (Modem Control Register) address
    ---------------------------------------------------------------------------
    function MCR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the LSR (Line Status Register) address
    ---------------------------------------------------------------------------
    function LSR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Given a serial port, get the MSR (Modem Status Register) address
    ---------------------------------------------------------------------------
    function MSR(port : SerialPort) return IOPort;

    ---------------------------------------------------------------------------
    -- Initialize a COM port with default settings of 115200 8N1 with FIFO
    -- enabled, cleared, and 14-byte FIFO interrupt trigger level set.
    ---------------------------------------------------------------------------
    procedure init(port : SerialPort);

    ---------------------------------------------------------------------------
    -- Initialize this serial port with the baud rate and parameters
    ---------------------------------------------------------------------------
    procedure init( port : SerialPort; 
                    rate : BaudRate; 
                    protocol : LineControl; 
                    fifo : FIFOControl; 
                    modem : ModemControl);

    ---------------------------------------------------------------------------
    -- Send a single character via this serial port.
    ---------------------------------------------------------------------------
    procedure send(port : SerialPort; c : Character);

    ---------------------------------------------------------------------------
    -- See if this serial port has data available.
    ---------------------------------------------------------------------------
    function hasData(port : SerialPort) return Boolean;

    ---------------------------------------------------------------------------
    -- Receive a single character via this serial port.
    ---------------------------------------------------------------------------
    function recv(port : SerialPort) return Character;

end serial;