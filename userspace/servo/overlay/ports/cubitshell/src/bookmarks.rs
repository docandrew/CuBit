//! Bounded bookmark persistence. All writes stay under the Bookmarks scope.
use std::{fs, io::{self, Read, Write}, path::Path};
const MAX_BYTES: usize = 4 + 64 * (6 + 256 + 1024 + 1024);
fn invalid() -> io::Error { io::Error::new(io::ErrorKind::InvalidData, "invalid bookmark snapshot") }
fn checksum(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, b| (h ^ u64::from(*b)).wrapping_mul(0x100000001b3))
}
fn snapshot(dir: &Path, slot: usize) -> io::Result<(u64, Vec<u8>)> {
    let mut bytes = Vec::new();
    fs::File::open(dir.join(format!("browser-{slot}.dat")))?
        .take((MAX_BYTES + 25) as u64).read_to_end(&mut bytes)?;
    if bytes.len() < 28 || bytes.len() > MAX_BYTES + 24 || &bytes[..4] != b"CBS1" { return Err(invalid()); }
    let generation = u64::from_le_bytes(bytes[4..12].try_into().unwrap());
    let length = u32::from_le_bytes(bytes[12..16].try_into().unwrap()) as usize;
    let hash = u64::from_le_bytes(bytes[16..24].try_into().unwrap());
    if generation == 0 || length != bytes.len() - 24 || hash != checksum(&bytes[24..]) || &bytes[24..28] != b"CBM2" { return Err(invalid()); }
    Ok((generation, bytes[24..].to_vec()))
}
fn latest(dir: &Path) -> io::Result<(usize, u64, Vec<u8>)> {
    let a = snapshot(dir, 0); let b = snapshot(dir, 1);
    match (a, b) {
        (Ok(a), Ok(b)) => if a.0 >= b.0 { Ok((0, a.0, a.1)) } else { Ok((1, b.0, b.1)) },
        (Ok(a), Err(_)) => Ok((0, a.0, a.1)),
        (Err(_), Ok(b)) => Ok((1, b.0, b.1)),
        (Err(a), Err(b)) if a.kind() == io::ErrorKind::NotFound && b.kind() == io::ErrorKind::NotFound => Err(a),
        _ => Err(invalid()),
    }
}
fn load(dir: &Path) -> io::Result<Vec<u8>> { latest(dir).map(|(_, _, bytes)| bytes) }
fn save(dir: &Path, bytes: &[u8]) -> io::Result<()> {
    if bytes.len() > MAX_BYTES || !bytes.starts_with(b"CBM2") { return Err(invalid()); }
    let (slot, generation) = match latest(dir) {
        Ok((slot, generation, _)) => (1 - slot, generation.checked_add(1).ok_or_else(invalid)?),
        Err(e) if e.kind() == io::ErrorKind::NotFound => (0, 1),
        Err(e) => return Err(e),
    };
    fs::create_dir_all(dir)?;
    // CuBit rename does not replace existing targets. Alternate bounded,
    // checksummed snapshots; never truncate the last committed generation.
    let mut file = fs::File::create(dir.join(format!("browser-{slot}.dat")))?;
    file.write_all(b"CBS1")?;
    file.write_all(&generation.to_le_bytes())?;
    file.write_all(&(bytes.len() as u32).to_le_bytes())?;
    file.write_all(&checksum(bytes).to_le_bytes())?;
    file.write_all(bytes)?;
    file.sync_all()
}
// The Ada caller supplies valid borrowed buffers for these synchronous calls.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_bookmarks_load(data: *mut u8, capacity: u32) -> i32 {
    if data.is_null() || capacity as usize != MAX_BYTES { return -1; }
    match load(Path::new("/Bookmarks")) {
        Ok(bytes) => { unsafe { std::ptr::copy_nonoverlapping(bytes.as_ptr(), data, bytes.len()); } bytes.len() as i32 }
        Err(e) if e.kind() == io::ErrorKind::NotFound => 0,
        Err(_) => -1,
    }
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_bookmarks_save(data: *const u8, length: u32) -> u32 {
    if data.is_null() || length as usize > MAX_BYTES { return 0; }
    let bytes = unsafe { std::slice::from_raw_parts(data, length as usize) };
    u32::from(save(Path::new("/Bookmarks"), bytes).is_ok())
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn replacement_and_failed_save_preserve_committed_file() {
        let dir = std::env::temp_dir().join(format!("cubit-bookmarks-{}", std::process::id()));
        fs::create_dir(&dir).unwrap();
        assert_eq!(load(&dir).unwrap_err().kind(), io::ErrorKind::NotFound);
        save(&dir, b"CBM2first").unwrap();
        save(&dir, b"CBM2second").unwrap();
        assert_eq!(load(&dir).unwrap(), b"CBM2second");
        assert!(save(&dir, b"bad").is_err());
        fs::remove_file(dir.join("browser-0.dat")).unwrap();
        fs::create_dir(dir.join("browser-0.dat")).unwrap();
        assert!(save(&dir, b"CBM2third").is_err());
        assert_eq!(load(&dir).unwrap(), b"CBM2second");
        fs::remove_dir(dir.join("browser-0.dat")).unwrap();
        fs::write(dir.join("browser-0.dat"), b"CBS1torn").unwrap();
        assert_eq!(load(&dir).unwrap(), b"CBM2second");
        save(&dir, b"CBM2recovered").unwrap();
        assert_eq!(load(&dir).unwrap(), b"CBM2recovered");
        fs::write(dir.join("browser-0.dat"), vec![0; MAX_BYTES + 25]).unwrap();
        fs::write(dir.join("browser-1.dat"), b"corrupt").unwrap();
        assert!(load(&dir).is_err());
        assert!(save(&dir, b"CBM2refuse").is_err());
        fs::remove_dir_all(dir).unwrap();
    }
}

static ICONS: std::sync::Mutex<Vec<(String, [u32; 256])>> = std::sync::Mutex::new(Vec::new());
#[cfg(not(test))]
pub fn remember_icon(url: &str, image: &servo::Image) {
    use servo::PixelFormat;
    if url.len() > 1024 || image.width == 0 || image.height == 0 || image.width > 1024 || image.height > 1024 { return; }
    let channels = match image.format { PixelFormat::RGBA8 | PixelFormat::BGRA8 => 4, PixelFormat::RGB8 => 3, _ => return };
    let bytes = image.data();
    if bytes.len() < image.width as usize * image.height as usize * channels { return; }
    let mut pixels = [0u32; 256];
    for y in 0..16 { for x in 0..16 {
        let i = ((y * image.height as usize / 16) * image.width as usize + x * image.width as usize / 16) * channels;
        let (r, g, b) = if matches!(image.format, PixelFormat::BGRA8) { (bytes[i+2], bytes[i+1], bytes[i]) } else { (bytes[i], bytes[i+1], bytes[i+2]) };
        let a = if channels == 4 { bytes[i+3] } else { 255 };
        pixels[y*16+x] = (u32::from(a)<<24) | (u32::from(r)<<16) | (u32::from(g)<<8) | u32::from(b);
    }}
    let Ok(mut icons) = ICONS.lock() else { return; };
    if let Some(item) = icons.iter_mut().find(|item| item.0 == url) { item.1 = pixels; return; }
    if icons.len() == 128 { icons.remove(0); }
    icons.push((url.to_owned(), pixels));
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_bookmark_icon(url: *const u8, length: u32, out: *mut u32) -> u32 {
    if url.is_null() || out.is_null() || length > 1024 { return 0; }
    let Ok(url) = std::str::from_utf8(unsafe { std::slice::from_raw_parts(url, length as usize) }) else { return 0; };
    let Ok(icons) = ICONS.lock() else { return 0; };
    let Some((_, pixels)) = icons.iter().find(|item| item.0 == url) else { return 0; };
    unsafe { std::ptr::copy_nonoverlapping(pixels.as_ptr(), out, 256); }
    1
}
