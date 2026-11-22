const extended = 'Method(_BIX,0) { Return(Package(20){0,1,42000,40000,1,11000,2000,1000,12,100000,0xFFFFFFFF,10,500,5,1,10,"CUBIT","SER1","LION",""}) }';
const dynamic = 'Method(_BST,0) { Local0=Package(4){1,100,0,11000} Local0[2]=REMA Return(Local0) }';
function replaceOne(text, from, to) {
  if (!text.includes(from) || text.indexOf(from) !== text.lastIndexOf(from)) throw Error('Battery fixture anchor');
  return text.replace(from, to);
}
export function batteryFixture(text, mode) {
  if (mode === 'battery-absent') return replaceOne(text,
    'Name(_HID,EisaId("PNP0C0A")) Name(_STA,0x1F)', 'Name(_HID,EisaId("PNP0C0A")) Name(_STA,0x0F)');
  if (mode === 'battery-none') return replaceOne(text,
    'Name(_HID,EisaId("PNP0C0A"))', 'Name(_HID,"TEST0005")');
  if (mode === 'battery-legacy') return replaceOne(text, extended,
    'Method(_BIF,0) { Return(Package(13){0,43000,41000,1,12000,2100,1100,1,10,"LEGACY","SER2","LION",""}) }');
  if (mode === 'battery-unknown') {
    text = replaceOne(text, extended,
      'Method(_BIX,0) { Return(Package(20){0,1,0xFFFFFFFF,0xFFFFFFFF,1,0xFFFFFFFF,2000,1000,12,100000,0xFFFFFFFF,10,500,5,1,10,"CUBIT","SER1","LION",""}) }');
    return replaceOne(text, dynamic,
      'Method(_BST,0) { Return(Package(4){1,0xFFFFFFFF,0xFFFFFFFF,0xFFFFFFFF}) }');
  }
  const count = {'battery-multiple': 2, 'battery-full': 16, 'battery-overflow': 17}[mode];
  if (count) {
    let devices = '';
    for (let slot = 2; slot <= count; slot++) {
      const name = 'B' + String(slot).padStart(3, '0');
      devices += `Device(${name}) { Name(_HID,EisaId("PNP0C0A")) Name(_STA,0x1F)
        Method(_BIX,0) { Return(Package(20){0,0,42000,40000,1,11000,2000,1000,12,100000,0xFFFFFFFF,10,500,5,1,10,"CUBIT","SER${slot}","LION",""}) }
        Method(_BST,0) { Return(Package(4){1,100,${20000 + slot},11000}) } }
      `;
    }
    return replaceOne(text, 'Device(EC0)', devices + 'Device(EC0)');
  }
  return text;
}
