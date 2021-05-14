-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS IPC
--
-- @TODO
-- We probably want per-mailbox locks here.
-------------------------------------------------------------------------------
with PerCPUData;
with Process.Queues;
with TextIO; use TextIO;

package body Process.IPC with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- receive
    -- Return the message if it exists, block otherwise.
    ---------------------------------------------------------------------------
    function receive (from : out ProcessID) return Unsigned_64 with SPARK_Mode => On is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;

        -- Since multiple threads can listen for the same process, the actual
        -- receive queue used will be that of this receiver.
        receiver : ProcessID;

        message  : Unsigned_64;
        sender   : ProcessID;

        ignore   : ProcessID;
    begin

        if proctab(mypid).isThread then
            receiver := getParent (mypid);
        else
            receiver := mypid;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        -- Add this thread to list of waiting receivers for the mailbox. 
        -- Their code in send() will remove and unblock us.
        -- Mark which mailbox we're waiting for with
        -- queuekey, so if we get killed, we can be removed from the appropriate list.
        -- (see Process.kill)
        proctab(mypid).queueKey := receiver;
        ignore := Queues.enqueue (mailtab(receiver).recvQueue, mypid);

        if not mailtab(receiver).hasMsg then
            -- No message, so block.
            proctab(mypid).state := RECEIVING;
            
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            
            yield;
        end if;

        -- Unblocked, receive message, empty mailbox
        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        message := mailtab(receiver).message;
        mailtab(receiver).hasMsg := False;

        -- Now we've received, we can remove the sender from our list and move it to WAITINGFORREPLY
        sender := Queues.dequeue (mailtab(receiver).sendQueue);
        proctab(sender).state := WAITINGFORREPLY;
        from := sender;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        return message;
    end receive;

    ---------------------------------------------------------------------------
    -- receiveEvent
    ---------------------------------------------------------------------------
    function receiveEvent return Unsigned_64 with SPARK_Mode => On is
        mypid    : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
        event    : Unsigned_64;
    begin
        -- print ("Process.IPC: pid "); print (Integer(mypid)); println(" receiving event.");
        if proctab(mypid).isThread then
            receiver := proctab(mypid).ppid;
        else
            receiver := mypid;
        end if;

        Spinlocks.enterCriticalSection (mailtab(receiver).lock);

        if not mailtab(receiver).hasEvent then
            proctab(mypid).state := WAITINGFOREVENT;
            -- print ("Process.IPC: pid "); print (Integer(mypid)); println(" waiting for event.");
            Spinlocks.exitCriticalSection (mailtab(receiver).lock);
            
            yield;

            Spinlocks.enterCriticalSection (mailtab(receiver).lock);
        end if;

        event := mailtab(receiver).event;
        mailtab(receiver).hasEvent := False;

        Spinlocks.exitCriticalSection (mailtab(receiver).lock);

        return event;
    end receiveEvent;

    ---------------------------------------------------------------------------
    -- receiveNB
    ---------------------------------------------------------------------------
    -- function receiveNB return Unsigned_64 with SPARK_Mode => On is
    --     pid     : constant ProcessID := PerCPUData.getCurrentPID;
    --     message : Unsigned_64;
    -- begin
    --     Spinlocks.enterCriticalSection (lock);

    --     if proctab(pid).mail.hasMsg then
    --         message := proctab(pid).mail.message;
    --         proctab(pid).mail.hasMsg := False;
    --     else
    --         message := Unsigned_64'Last;
    --     end if;

    --     Spinlocks.exitCriticalSection (lock);

    --     return message;
    -- end receiveNB;

    ---------------------------------------------------------------------------
    -- send
    -- If recipient's mailbox is full, put us in the SENDING state and
    -- block. If not full, dequeue from their recvQueue and make them READY.
    ---------------------------------------------------------------------------
    function send (dest : ProcessID; msg : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On
    is
        pid      : constant ProcessID := PerCPUData.getCurrentPID;
        receiver : ProcessID;
        reply    : Unsigned_64;
        ignore   : ProcessID;
    begin
        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        -- Add us to queue of senders
        proctab(pid).queueKey := dest;
        ignore := Queues.enqueue (mailtab(dest).sendQueue, pid);

        -- Block until we are explicitly unqueued and woken.
        proctab(pid).state := SENDING;

        -- Left off here - @TODO need to model the various scenarios with
        -- send first, then receive, vs receive then send happens later.
        Spinlocks.exitCriticalSection (mailtab(dest).lock);
        
        yield;

        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        -- Deliver message to process queue
        mailtab(dest).hasMsg  := True;
        mailtab(dest).message := msg;
        mailtab(dest).sender  := pid;

        -- Receiving thread will be the first in the receive queue. Remove from
        -- queue and unblock them.
        ready (Queues.dequeue (mailtab(dest).recvQueue));

        Spinlocks.exitCriticalSection (mailtab(dest).lock);

        -- Now we block, and wait for reply.
        proctab(pid).state := WAITINGFORREPLY;
        yield;

        reply := proctab(pid).reply;
        proctab(pid).reply := Unsigned_64'Last;

        return reply;
    end send;

    ---------------------------------------------------------------------------
    -- send
    -- Special non-blocking version of send for interrupts.
    --
    -- This may be called by an interrupt handler, so not appropriate to 
    -- re-enter the scheduler. Deadlock will result.
    ---------------------------------------------------------------------------
    procedure sendEvent (dest : ProcessID; msg : Unsigned_64)
        with SPARK_Mode => On is
    begin
        Spinlocks.enterCriticalSection (mailtab(dest).lock);

        mailtab(dest).hasEvent := True;
        mailtab(dest).event    := msg;

        if proctab(dest).state = WAITINGFOREVENT then
            inform (dest);
        end if;

        Spinlocks.exitCriticalSection (mailtab(dest).lock);
    end sendEvent;

    ---------------------------------------------------------------------------
    -- reply
    ---------------------------------------------------------------------------
    function reply (replyTo : ProcessID; msg : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        proctab(replyTo).reply := msg;

        -- Unblock the original sending thread
        if proctab(replyTo).state = WAITINGFORREPLY then
            inform (replyTo);
            -- no need to give up our time-slice here.
        else
            raise ProcessException with "Process.reply: Replied to sender but they weren't WAITINGFORREPLY";
        end if;

        return 1;
    end reply;

end Process.IPC;
