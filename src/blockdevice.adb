
package body BlockDevice
    with SPARK_Mode => On
is
    blockDrivers : BlockDriverList;

    procedure registerBlockDriver(major : in Devices.MajorNumber;
                                  bufSyncFunc : BufferSyncFunction) with
        SPARK_Mode => On
    is
    begin
        blockDrivers(major) := bufSyncFunc;
    end registerBlockDriver;


    procedure syncBuffer(buf : in out FileCache.BufferPtr) with
        SPARK_Mode => On
    is
    begin
        blockDrivers(buf.all.device.major).all(buf);
    end syncBuffer;

end BlockDevice;
