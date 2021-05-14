-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS IPC
--
-------------------------------------------------------------------------------
package Process.IPC with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- send
    -- Put a message in a process' mailbox, change this process' state to
    -- WAITINGFORREPLY.
    -- @return the reply value.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- sendEvent
    -- Non-blocking version of send, intended for interrupts. Sends a message
    -- to the destination process but does not block whatever process was
    -- active when the interrupt occurred.
    ---------------------------------------------------------------------------
    procedure sendEvent (dest : ProcessID; msg : Unsigned_64)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receive
    -- Receive a message from one's mailbox. Block if no message available.
    ---------------------------------------------------------------------------
    function receive (from : out ProcessID) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receiveEvent
    -- Block until an event is received, or return the event if one is already
    -- waiting. If multiple events are sent to this
    -- process between receiveEvent calls, only the latest one will be
    -- delivered.
    ---------------------------------------------------------------------------
    function receiveEvent return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- reply
    -- Reply to a process who called send() and unblock it.
    ---------------------------------------------------------------------------
    function reply (replyTo : ProcessID; msg : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- receiveNB
    -- Non-blocking version of receive.
    -- Receive a message from one's mailbox, return Unsigned_64'Last if no
    -- message available.
    ---------------------------------------------------------------------------
    -- function receiveNB return Unsigned_64
    --     with SPARK_Mode => On;

end Process.IPC;
