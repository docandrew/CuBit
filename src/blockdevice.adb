
package body BlockDevice
    with SPARK_Mode => On
is
    blockDrivers : BlockDeviceList;

    procedure registerBlockDriver(major : in Device.MajorNumber;
                                  bufSyncFunc : access procedure(buf : in out BufferCache.BufferPtr)) with
        SPARK_Mode => On
    is
    begin
        blockDrivers(major) := bufSyncFunc;
    end registerBlockDriver;


    procedure syncBuffer(buf : in out BufferCache.BufferPtr) with
        SPARK_Mode => On
    is
    begin
        blockDrivers(buf.all.device.major).all(buf);
    end syncBuffer;

end BlockDevice;
